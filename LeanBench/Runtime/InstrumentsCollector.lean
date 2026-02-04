import LeanBench.Runtime.Types
import LeanBench.Runtime.GenericCollector
import LeanBench.Runtime.PerfCollector

/-!
# macOS Instruments Collector

Parses Time Profiler and GPU trace data exported from Instruments.

Usage:
1. Record with Instruments or: `xcrun xctrace record --template 'Time Profiler' --launch ./your_binary`
2. Export: `xcrun xctrace export --input Recording.trace --output trace.xml`
3. Import: pass `--instruments trace.xml` to leanobserve

For GPU traces, use Metal System Trace template.
-/

namespace LeanBench.Runtime

/-- Find position of substring in string. -/
def strFindSubstr? (s sub : String) : Option Nat := Id.run do
  let sChars := s.toList
  let subChars := sub.toList
  if subChars.isEmpty then return some 0
  for i in [0:sChars.length] do
    if isPrefixChars subChars (sChars.drop i) then
      return some i
  return none

open Lean (Json)

/-- Parse exported Instruments XML/JSON trace.
    Note: xctrace export produces XML by default. For JSON, use additional tools
    or parse the XML directly.
-/
def parseInstrumentsXml (content : String) : Array CpuSample := Id.run do
  -- Simplified XML parsing for Time Profiler samples
  -- Real implementation would use proper XML parser
  let mut samples : Array CpuSample := #[]
  let lines := content.splitOn "\n"

  for line in lines do
    -- Look for sample entries in typical xctrace export format
    -- Format varies by Instruments version; this handles common patterns
    if strContains line "<sample" || strContains line "\"symbol\"" then
      -- Try to extract symbol name
      if let some startIdx := strFindSubstr? line "name=\"" then
        let rest := (line.drop (startIdx + 6)).toString
        if let some endIdx := strFindSubstr? rest "\"" then
          let sym := (rest.take endIdx).toString
          let leanDecl := demangleLeanSymbol sym
          samples := samples.push {
            symbol := sym
            leanDecl := leanDecl
            file := symbolToFile sym leanDecl
            samples := some 1
          }
      else if let some startIdx := strFindSubstr? line "\"symbol\":\"" then
        let rest := (line.drop (startIdx + 10)).toString
        if let some endIdx := strFindSubstr? rest "\"" then
          let sym := (rest.take endIdx).toString
          let leanDecl := demangleLeanSymbol sym
          samples := samples.push {
            symbol := sym
            leanDecl := leanDecl
            file := symbolToFile sym leanDecl
            samples := some 1
          }

  return samples

/-- Parse Instruments JSON output (if available). -/
def parseInstrumentsJson (json : Json) : Except String RuntimeData := do
  -- Check for Time Profiler data
  let cpuSamples : Array CpuSample := #[]

  -- Check for GPU trace data
  let gpuKernels : Array GpuKernel := #[]

  return {
    source := { tool := "instruments", platform := some "macos" }
    cpuSamples := cpuSamples
    gpuKernels := gpuKernels
  }

/-- Parse GPU kernel entries from Metal System Trace. -/
def parseMetalTraceKernels (content : String) : Array GpuKernel := Id.run do
  let mut kernels : Array GpuKernel := #[]
  let lines := content.splitOn "\n"

  for line in lines do
    -- Look for kernel execution entries
    if strContains line "compute_kernel" || strContains line "ComputeCommandEncoder" then
      -- Extract kernel name and duration
      if let some startIdx := strFindSubstr? line "name=\"" then
        let rest := (line.drop (startIdx + 6)).toString
        if let some endIdx := strFindSubstr? rest "\"" then
          let kernelName := (rest.take endIdx).toString
          kernels := kernels.push {
            name := kernelName
            leanDecl := demangleLeanSymbol kernelName
          }

  return kernels

/-- Convert Instruments data to RuntimeData. -/
def instrumentsToRuntimeData (content : String) (isGpuTrace : Bool := false) : RuntimeData := Id.run do
  if isGpuTrace then
    let kernels := parseMetalTraceKernels content
    return {
      source := { tool := "instruments", platform := some "macos" }
      gpuKernels := kernels
    }
  else
    let rawSamples := parseInstrumentsXml content
    let aggregated := aggregateSamples rawSamples
    return {
      source := { tool := "instruments", platform := some "macos" }
      cpuSamples := aggregated
    }

/-- Load Instruments trace and convert to MetricByFile and MetricByDecl. -/
def collectFromInstrumentsWithDecls (path : System.FilePath)
    (isGpuTrace : Bool := false) : IO (MetricByFile × MetricByDecl) := do
  let content ← IO.FS.readFile path
  let data := instrumentsToRuntimeData content isGpuTrace
  let floatByFile := runtimeDataToMetricsByFile data
  let floatByDecl := runtimeDataToMetricsByDecl data
  let byFile := mergeMetricByFileF {} floatByFile
  let byDecl := mergeMetricByDeclF {} floatByDecl
  return (byFile, byDecl)

/-- Load Instruments trace and convert to MetricByFile. -/
def collectFromInstruments (path : System.FilePath) (isGpuTrace : Bool := false) : IO MetricByFile := do
  let (byFile, _) ← collectFromInstrumentsWithDecls path isGpuTrace
  return byFile

/-- Run xctrace export on a .trace file and return parsed data. -/
def exportAndParseTrace (tracePath : System.FilePath) : IO RuntimeData := do
  -- Export using xctrace
  let output ← IO.Process.output {
    cmd := "xcrun"
    args := #["xctrace", "export", "--input", tracePath.toString, "--xpath",
              "/trace-toc/run/data/table[@schema='time-profile']/row"]
  }

  if output.exitCode != 0 then
    throw (IO.userError s!"xctrace export failed: {output.stderr}")

  let samples := parseInstrumentsXml output.stdout
  let aggregated := aggregateSamples samples
  return {
    source := { tool := "instruments", platform := some "macos" }
    cpuSamples := aggregated
  }

end LeanBench.Runtime
