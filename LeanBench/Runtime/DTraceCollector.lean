import LeanBench.Runtime.Types
import LeanBench.Runtime.GenericCollector
import LeanBench.Runtime.PerfCollector

namespace LeanBench.Runtime

/-- Trim whitespace from a string. -/
@[inline] def trimStr (s : String) : String := s.trimAscii.toString

/-- DTrace aggregation entry. -/
structure DTraceAggEntry where
  key : String  -- Function name or stack
  value : Nat   -- Count, sum, or time
  aggType : String := "count"  -- "count", "sum", "avg"
  deriving Inhabited

/-- Extract function name from DTrace stack line.
    Input: `  lean_foo+0x42` or `0x123456 lean_foo+0x42`
    Output: `lean_foo` -/
def extractFunctionName (line : String) : String := Id.run do
  let trimmedLine := trimStr line
  let parts : List String := trimmedLine.splitOn " " |>.filter (fun s => s.length > 0)

  for part in parts.reverse do
    -- Skip hex addresses
    if part.startsWith "0x" then continue
    -- Remove offset suffix
    let name := match part.splitOn "+" with
      | base :: _ => base
      | [] => part
    -- Skip if still looks like an address
    if name.startsWith "0x" then continue
    if name.length > 0 then return name

  return ""

/-- Parse DTrace stack sample output.
    Format:
    ```
                  function1
                  function2
                  ...
                42
    ```
    Each stack ends with a count on its own line.
-/
def parseDTraceStackSamples (content : String) : Array CpuSample := Id.run do
  let lines := content.splitOn "\n"
  let mut samples : Array CpuSample := #[]
  let mut currentStack : Array String := #[]
  let mut byFunc : Std.HashMap String Nat := {}

  for line in lines do
    let trimmed := trimStr line
    if trimmed.isEmpty then
      -- End of a stack trace block
      currentStack := #[]
    else if let some count := trimmed.toNat? then
      -- This is the count for the previous stack
      -- Attribute samples to each function in the stack
      for func in currentStack do
        let prev := byFunc.getD func 0
        byFunc := byFunc.insert func (prev + count)
      currentStack := #[]
    else
      -- This is a function in the stack
      -- Clean up the function name (remove addresses, whitespace)
      let funcName := extractFunctionName trimmed
      if funcName.length > 0 then
        currentStack := currentStack.push funcName

  -- Convert to CpuSamples
  for (func, count) in byFunc.toList do
    let leanDecl := if func.startsWith "lean_" then
      demangleLeanSymbol func
    else if func.startsWith "_Z" then
      -- C++ mangled
      some func  -- TODO: proper demangling
    else
      none

    samples := samples.push {
      symbol := func
      leanDecl := leanDecl
      file := leanDecl.bind (fun d => symbolToFile func (some d))
      samples := some count
    }

  return samples

/-- Parse DTrace aggregation output.
    Format:
    ```
    function_name                                              12345
    another_function                                            6789
    ```
-/
def parseDTraceAggregation (content : String) : Array DTraceAggEntry := Id.run do
  let lines := content.splitOn "\n"
  let mut entries : Array DTraceAggEntry := #[]

  for line in lines do
    let trimmed := trimStr line
    if trimmed.isEmpty then continue

    -- Try to split into key and value
    -- DTrace uses variable spacing, so we look for the last number
    let parts : List String := trimmed.splitOn " " |>.filter (fun s => s.length > 0)
    if parts.length < 2 then continue

    -- Last part should be the value
    if let some value := parts[parts.length - 1]!.toNat? then
      -- Everything else is the key
      let key := String.intercalate " " (List.take (parts.length - 1) parts)
      if key.length > 0 then
        entries := entries.push {
          key := key
          value := value
          aggType := "sum"  -- Assume timing aggregation
        }

  return entries

/-- Convert DTrace aggregation to CPU samples (for timing data). -/
def dtraceAggToCpuSamples (entries : Array DTraceAggEntry) : Array CpuSample := Id.run do
  let mut samples : Array CpuSample := #[]

  for entry in entries do
    let func := extractFunctionName entry.key
    if func.isEmpty then continue

    let leanDecl := if func.startsWith "lean_" then
      demangleLeanSymbol func
    else
      none

    samples := samples.push {
      symbol := func
      leanDecl := leanDecl
      file := leanDecl.bind (fun d => symbolToFile func (some d))
      timeNs := some entry.value.toFloat  -- Assume nanoseconds
    }

  return samples

/-- Detect DTrace output format and parse accordingly. -/
def parseDTraceOutput (content : String) : Array CpuSample := Id.run do
  -- Heuristic: if content has many lines starting with whitespace, it's likely stack traces
  let lines := content.splitOn "\n"
  let indentedLines := lines.filter fun l => l.startsWith "  " || l.startsWith "\t"

  if indentedLines.length > lines.length / 4 then
    -- Likely stack traces
    return parseDTraceStackSamples content
  else
    -- Likely aggregation output
    let entries := parseDTraceAggregation content
    return dtraceAggToCpuSamples entries

/-- Convert DTrace data to RuntimeData. -/
def dtraceToRuntimeData (content : String) : RuntimeData := Id.run do
  let cpuSamples := parseDTraceOutput content
  return {
    source := { tool := "dtrace", platform := some "macos" }
    cpuSamples := cpuSamples
  }

/-- Load DTrace output and convert to MetricByFile and MetricByDecl. -/
def collectFromDTraceWithDecls (path : System.FilePath) : IO (MetricByFile × MetricByDecl) := do
  let content ← IO.FS.readFile path
  let data := dtraceToRuntimeData content
  let floatByFile := runtimeDataToMetricsByFile data
  let floatByDecl := runtimeDataToMetricsByDecl data
  let byFile := mergeMetricByFileF {} floatByFile
  let byDecl := mergeMetricByDeclF {} floatByDecl
  return (byFile, byDecl)

/-- Load DTrace output and convert to MetricByFile. -/
def collectFromDTrace (path : System.FilePath) : IO MetricByFile := do
  let (byFile, _) ← collectFromDTraceWithDecls path
  return byFile

end LeanBench.Runtime
