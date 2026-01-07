import LeanBench.Runtime.Types
import LeanBench.Runtime.GenericCollector
import LeanBench.Runtime.PerfCollector

namespace LeanBench.Runtime

/-- Trim whitespace from a string. -/
@[inline] def trimStr' (s : String) : String := s.trimAscii.toString

/-- Tracy zone record from CSV export. -/
structure TracyZone where
  name : String
  sourceFile : Option String := none
  sourceLine : Option Nat := none
  startNs : Float
  endNs : Float
  threadId : Option Nat := none
  isGpu : Bool := false
  deriving Inhabited

/-- Tracy frame record. -/
structure TracyFrame where
  frameId : Nat
  startNs : Float
  endNs : Float
  deriving Inhabited

/-- Parse a CSV line into fields, handling quoted values. -/
def parseCsvLine (line : String) : Array String := Id.run do
  let mut fields : Array String := #[]
  let mut current := ""
  let mut inQuotes := false

  for c in line.toList do
    if c == '"' then
      inQuotes := !inQuotes
    else if c == ',' && !inQuotes then
      fields := fields.push current
      current := ""
    else
      current := current.push c

  fields := fields.push current
  return fields

/-- Parse Tracy CSV header to get column indices. -/
def parseTracyCsvHeader (header : String) : Std.HashMap String Nat := Id.run do
  let fields := parseCsvLine header
  let mut indices : Std.HashMap String Nat := {}
  for i in [:fields.size] do
    let field := (trimStr' fields[i]!).toLower
    indices := indices.insert field i
  return indices

/-- Parse a Tracy zone from CSV fields. -/
def parseTracyZone (fields : Array String) (indices : Std.HashMap String Nat) : Option TracyZone := Id.run do
  -- Get name (required)
  let nameIdx := indices.getD "name" 0
  if nameIdx >= fields.size then return none
  let name := trimStr' fields[nameIdx]!

  if name.isEmpty then return none

  -- Get timing (required)
  let startIdx := indices.getD "start_ns" (indices.getD "start" 999)
  let endIdx := indices.getD "end_ns" (indices.getD "end" 999)

  if startIdx >= fields.size || endIdx >= fields.size then return none

  let startStr := trimStr' fields[startIdx]!
  let endStr := trimStr' fields[endIdx]!
  let startNs := startStr.toNat?.map (·.toFloat) |>.getD 0.0
  let endNs := endStr.toNat?.map (·.toFloat) |>.getD 0.0

  -- Skip invalid zones where end <= start (use negated > for comparison)
  if !(endNs > startNs) then return none

  -- Get optional fields
  let fileIdx := indices.getD "source_file" (indices.getD "file" 999)
  let sourceFile := if fileIdx < fields.size then
    let f := trimStr' fields[fileIdx]!
    if f.isEmpty then none else some f
  else none

  let lineIdx := indices.getD "source_line" (indices.getD "line" 999)
  let sourceLine := if lineIdx < fields.size then
    (trimStr' fields[lineIdx]!).toNat?
  else none

  let threadIdx := indices.getD "thread_id" (indices.getD "thread" 999)
  let threadId := if threadIdx < fields.size then
    (trimStr' fields[threadIdx]!).toNat?
  else none

  -- Check if GPU zone
  let isGpu := indices.contains "gpu_context" ||
    strContains name "GPU" ||
    strContains name "gpu" ||
    strContains name "Kernel" ||
    strContains name "kernel"

  return some {
    name := name
    sourceFile := sourceFile
    sourceLine := sourceLine
    startNs := startNs
    endNs := endNs
    threadId := threadId
    isGpu := isGpu
  }

/-- Parse Tracy CSV export into zones. -/
def parseTracyCsv (content : String) : Array TracyZone := Id.run do
  let lines := content.splitOn "\n" |>.filter (fun s => (trimStr' s).length > 0)
  if lines.length < 2 then return #[]

  let indices := parseTracyCsvHeader lines[0]!
  let mut zones : Array TracyZone := #[]

  for i in [1:lines.length] do
    let fields := parseCsvLine lines[i]!
    if let some zone := parseTracyZone fields indices then
      zones := zones.push zone

  return zones

/-- Aggregate Tracy zones into CPU samples. -/
def tracyZonesToCpuSamples (zones : Array TracyZone) : Array CpuSample := Id.run do
  -- Group by name and aggregate
  let mut byName : Std.HashMap String (Float × Nat × Option String × Option Nat) := {}

  for zone in zones do
    if zone.isGpu then continue  -- Skip GPU zones for CPU samples

    let duration := zone.endNs - zone.startNs
    let existing := byName.getD zone.name (0.0, 0, zone.sourceFile, zone.sourceLine)
    byName := byName.insert zone.name (existing.1 + duration, existing.2.1 + 1, existing.2.2.1, existing.2.2.2)

  let mut samples : Array CpuSample := #[]
  for (name, (totalTime, count, file, line)) in byName.toList do
    -- Try to map Tracy zone name to Lean declaration
    let leanDecl := if strContains name "lean_" then
      demangleLeanSymbol name
    else if strContains name "::" then
      -- C++ style: Module::function -> Module.function
      some (name.replace "::" ".")
    else
      none

    samples := samples.push {
      symbol := name
      leanDecl := leanDecl
      file := file.orElse (fun _ => leanDecl.bind (fun d => symbolToFile name (some d)))
      line := line
      samples := some count
      timeNs := some totalTime
    }

  return samples

/-- Aggregate Tracy GPU zones into GPU kernels. -/
def tracyZonesToGpuKernels (zones : Array TracyZone) : Array GpuKernel := Id.run do
  let mut byName : Std.HashMap String (Float × Nat × Option String × Option Nat) := {}

  for zone in zones do
    if !zone.isGpu then continue  -- Only GPU zones

    let duration := zone.endNs - zone.startNs
    let existing := byName.getD zone.name (0.0, 0, zone.sourceFile, zone.sourceLine)
    byName := byName.insert zone.name (existing.1 + duration, existing.2.1 + 1, existing.2.2.1, existing.2.2.2)

  let mut kernels : Array GpuKernel := #[]
  for (name, (totalTime, _count, file, line)) in byName.toList do
    let leanDecl := if strContains name "lean_" then
      demangleLeanSymbol name
    else
      none

    kernels := kernels.push {
      name := name
      leanDecl := leanDecl
      file := file.orElse (fun _ => leanDecl.bind (fun d => symbolToFile name (some d)))
      line := line
      durationNs := some totalTime
    }

  return kernels

/-- Extract frame timing from Tracy zones. -/
def tracyZonesToFrames (zones : Array TracyZone) : Array FrameData := Id.run do
  -- Look for zones named "Frame" or similar
  let frameZones := zones.filter fun z =>
    strContains z.name "Frame" || strContains z.name "frame"

  let mut frames : Array FrameData := #[]
  for i in [:frameZones.size] do
    let zone := frameZones[i]!
    frames := frames.push {
      frameId := i
      startNs := some zone.startNs
      durationNs := some (zone.endNs - zone.startNs)
    }

  return frames

/-- Convert Tracy data to RuntimeData. -/
def tracyToRuntimeData (content : String) : RuntimeData := Id.run do
  let zones := parseTracyCsv content
  let cpuSamples := tracyZonesToCpuSamples zones
  let gpuKernels := tracyZonesToGpuKernels zones
  let frames := tracyZonesToFrames zones

  return {
    source := { tool := "tracy" }
    cpuSamples := cpuSamples
    gpuKernels := gpuKernels
    frames := frames
  }

/-- Load Tracy CSV and convert to MetricByFile. -/
def collectFromTracy (path : System.FilePath) : IO MetricByFile := do
  let content ← IO.FS.readFile path
  let data := tracyToRuntimeData content
  let floatMetrics := runtimeDataToMetricsByFile data
  return mergeMetricByFileF {} floatMetrics

end LeanBench.Runtime
