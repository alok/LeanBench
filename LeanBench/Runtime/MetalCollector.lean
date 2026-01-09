import LeanBench.Runtime.Types
import LeanBench.Runtime.GenericCollector
import LeanBench.Runtime.PerfCollector
import LeanBench.Runtime.InstrumentsCollector

/-!
# Metal GPU Collector

Parses Metal GPU profiling data from Instruments Metal System Trace
or custom Metal performance counters.

Usage:
1. Record: `xcrun xctrace record --template 'Metal System Trace' --launch ./your_binary`
2. Export: `xcrun xctrace export --input Recording.trace --output trace.xml`
3. Import: pass `--metal trace.xml` to leanobserve

Alternatively, use the vendor-neutral JSON format directly.
-/

namespace LeanBench.Runtime

-- Re-use helpers from InstrumentsCollector (strContains from PerfCollector, strFindSubstr? from InstrumentsCollector)

open Lean (Json)

/-- Map Metal kernel name to Lean declaration.
    Metal kernels are often named based on extern annotations.
-/
def mapMetalKernelToLean (kernelName : String) : Option String :=
  -- Try common patterns
  if kernelName.startsWith "lean_" then
    demangleLeanSymbol kernelName
  else if strContains kernelName "_kernel" then
    -- Strip "_kernel" suffix and try demangling
    let base := kernelName.replace "_kernel" ""
    demangleLeanSymbol ("lean_" ++ base)
  else
    -- Return as-is for manual mapping
    none

/-- Parse Metal System Trace XML for GPU kernel executions. -/
def parseMetalSystemTrace (content : String) : Array GpuKernel := Id.run do
  let mut kernels : Array GpuKernel := #[]
  let lines := content.splitOn "\n"

  -- Track current kernel being parsed
  let mut currentName : Option String := none
  let mut currentDuration : Option Float := none
  let mut currentOccupancy : Option Float := none

  for line in lines do
    -- Look for compute shader / kernel entries
    if strContains line "<compute-kernel" || strContains line "<dispatch" then
      -- Start of new kernel entry
      currentName := none
      currentDuration := none
      currentOccupancy := none

    -- Parse kernel name
    if strContains line "function=\"" || strContains line "name=\"" then
      if let some startIdx := strFindSubstr? line "=\"" then
        let rest := (line.drop (startIdx + 2)).toString
        if let some endIdx := strFindSubstr? rest "\"" then
          currentName := some ((rest.take endIdx).toString)

    -- Parse duration (nanoseconds)
    if strContains line "duration=\"" then
      if let some startIdx := strFindSubstr? line "duration=\"" then
        let rest := (line.drop (startIdx + 10)).toString
        if let some endIdx := strFindSubstr? rest "\"" then
          let durStr := (rest.take endIdx).toString
          -- Try to parse as float
          currentDuration := String.toNat? durStr |>.map (·.toFloat)

    -- Parse occupancy
    if strContains line "occupancy=\"" then
      if let some startIdx := strFindSubstr? line "occupancy=\"" then
        let rest := (line.drop (startIdx + 11)).toString
        if let some endIdx := strFindSubstr? rest "\"" then
          let occStr := (rest.take endIdx).toString
          currentOccupancy := String.toNat? occStr |>.map (·.toFloat)

    -- End of kernel entry
    if strContains line "</compute-kernel" || strContains line "</dispatch" then
      if let some name := currentName then
        let leanDecl := mapMetalKernelToLean name
        kernels := kernels.push {
          name := name
          leanDecl := leanDecl
          file := leanDecl.bind (fun d => symbolToFile name (some d))
          durationNs := currentDuration
          occupancyPercent := currentOccupancy
        }
      currentName := none

  return kernels

/-- Parse Metal GPU memory events. -/
def parseMetalMemoryEvents (content : String) : Array MemoryEvent := Id.run do
  let mut events : Array MemoryEvent := #[]
  let lines := content.splitOn "\n"

  for line in lines do
    -- Look for allocation events
    if strContains line "<allocation" || strContains line "MTLBuffer" then
      if strContains line "size=\"" then
        if let some startIdx := strFindSubstr? line "size=\"" then
          let rest := (line.drop (startIdx + 6)).toString
          if let some endIdx := strFindSubstr? rest "\"" then
            let sizeStr := (rest.take endIdx).toString
            if let some size := String.toNat? sizeStr then
              events := events.push {
                type := "alloc"
                bytes := size
              }

    -- Look for deallocation events
    if strContains line "<deallocation" || strContains line "release" then
      if strContains line "size=\"" then
        if let some startIdx := strFindSubstr? line "size=\"" then
          let rest := (line.drop (startIdx + 6)).toString
          if let some endIdx := strFindSubstr? rest "\"" then
            let sizeStr := (rest.take endIdx).toString
            if let some size := String.toNat? sizeStr then
              events := events.push {
                type := "free"
                bytes := size
              }

  return events

/-- Aggregate GPU kernels by name. -/
def aggregateGpuKernelsByName (kernels : Array GpuKernel) : Array GpuKernel := Id.run do
  let mut byName : Std.HashMap String GpuKernel := {}
  let mut counts : Std.HashMap String Nat := {}

  for kernel in kernels do
    let count := counts.getD kernel.name 0
    counts := counts.insert kernel.name (count + 1)

    if let some existing := byName[kernel.name]? then
      let newDuration := match existing.durationNs, kernel.durationNs with
        | some d1, some d2 => some (d1 + d2)
        | some d, none => some d
        | none, some d => some d
        | none, none => none
      let avgOccupancy := match existing.occupancyPercent, kernel.occupancyPercent with
        | some o1, some o2 => some ((o1 + o2) / 2.0)
        | some o, none => some o
        | none, some o => some o
        | none, none => none
      byName := byName.insert kernel.name { existing with
        durationNs := newDuration
        occupancyPercent := avgOccupancy
      }
    else
      byName := byName.insert kernel.name kernel

  return byName.fold (fun acc _ v => acc.push v) #[]

/-- Convert Metal trace to RuntimeData. -/
def metalTraceToRuntimeData (content : String) : RuntimeData := Id.run do
  let rawKernels := parseMetalSystemTrace content
  let aggregatedKernels := aggregateGpuKernelsByName rawKernels
  let memEvents := parseMetalMemoryEvents content

  return {
    source := { tool := "instruments", platform := some "macos" }
    gpuInfo := some { vendor := some "apple" }
    gpuKernels := aggregatedKernels
    gpuMemoryEvents := memEvents
  }

/-- Load Metal trace and convert to MetricByFile and MetricByDecl. -/
def collectFromMetalWithDecls (path : System.FilePath) : IO (MetricByFile × MetricByDecl) := do
  let content ← IO.FS.readFile path
  let data := metalTraceToRuntimeData content
  let floatByFile := runtimeDataToMetricsByFile data
  let floatByDecl := runtimeDataToMetricsByDecl data
  let byFile := mergeMetricByFileF {} floatByFile
  let byDecl := mergeMetricByDeclF {} floatByDecl
  return (byFile, byDecl)

/-- Load Metal trace and convert to MetricByFile. -/
def collectFromMetal (path : System.FilePath) : IO MetricByFile := do
  let (byFile, _) ← collectFromMetalWithDecls path
  return byFile

/-- Parse vendor-neutral GPU JSON (for any GPU vendor). -/
def parseGenericGpuJson (json : Json) : Except String RuntimeData := do
  -- Delegate to main parser
  parseRuntimeJson json

end LeanBench.Runtime
