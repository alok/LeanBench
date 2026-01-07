import LeanBench.Runtime.Types
import LeanBench.Runtime.GenericCollector

/-!
# Linux perf Collector

Parses `perf script` and `perf stat` output into runtime metrics.

Usage:
1. Record: `perf record -g ./your_binary`
2. Export: `perf script > perf.txt`
3. Import: pass `--perf perf.txt` to leanobserve

Also supports `perf stat -e cycles,instructions,...` output.
-/

namespace LeanBench.Runtime

/-- Check if string contains substring (local helper). -/
@[inline] def strContains (s sub : String) : Bool :=
  containsSubstr s sub

/-- Find the first index where a character matches. -/
def findCharIdx (s : String) (c : Char) : Option Nat := Id.run do
  let mut i := 0
  for ch in s.toList do
    if ch == c then return some i
    i := i + 1
  return none

/-- Strip common symbol suffixes like +0x42, .cold, .part.0, etc. -/
def stripSymbolSuffix (sym : String) : String := Id.run do
  let mut s := sym
  -- Remove offset suffix: symbol+0x42
  if let some idx := findCharIdx s '+' then
    s := String.ofList (s.toList.take idx)
  -- Remove .cold suffix
  if s.endsWith ".cold" then
    let chars := s.toList
    s := String.ofList (chars.take (chars.length - 5))
  -- Remove .part.N suffix
  if strContains s ".part." then
    if let some idx := findCharIdx s '.' then
      let rest := String.ofList (s.toList.drop idx)
      if strContains rest "part" then
        s := String.ofList (s.toList.take idx)
  -- Remove .constprop.N suffix
  if strContains s ".constprop" then
    if let some idx := findCharIdx s '.' then
      s := String.ofList (s.toList.take idx)
  return s

/-- Demangle Lean symbol name to declaration name.
    Handles multiple mangling conventions:
    - `lean_Foo_Bar_baz` -> `Foo.Bar.baz` (standard Lean)
    - `_lean_Foo_Bar_baz` -> `Foo.Bar.baz` (macOS)
    - `l_Foo___Bar___baz` -> `Foo.Bar.baz` (Lean4 IR)
    - `Foo::Bar::baz` -> `Foo.Bar.baz` (C++ style)
-/
def demangleLeanSymbol (sym : String) : Option String := Id.run do
  let cleaned := stripSymbolSuffix sym

  -- Standard Lean: lean_Foo_Bar_baz
  if cleaned.startsWith "lean_" then
    let rest := (cleaned.drop 5).toString
    let parts := rest.splitOn "_"
    if parts.length > 1 then
      return some (String.intercalate "." parts)
    else
      return some rest

  -- macOS mangled: _lean_Foo_Bar_baz
  if cleaned.startsWith "_lean_" then
    let rest := (cleaned.drop 6).toString
    return some (String.intercalate "." (rest.splitOn "_"))

  -- Lean4 IR style: l_Foo___Bar___baz (triple underscore for dot)
  if cleaned.startsWith "l_" then
    let rest := (cleaned.drop 2).toString
    -- First replace ___ with a placeholder, then handle remaining _
    let withDots := rest.replace "___" "."
    return some withDots

  -- C++ namespace style: Foo::Bar::baz or Lean::Foo::Bar
  if strContains cleaned "::" then
    let parts := cleaned.splitOn "::"
    -- Check if it looks like Lean code
    if parts.any (fun p => p.startsWith "Lean" || p.startsWith "lean") then
      return some (String.intercalate "." parts)
    else
      return some (String.intercalate "." parts)

  -- Rust-style mangling: _ZN prefix
  if cleaned.startsWith "_ZN" then
    -- Basic Rust demangling - extract the name parts
    let rest := (cleaned.drop 3).toString
    let mut parts : Array String := #[]
    let mut i := 0
    let chars := rest.toList
    while i < chars.length do
      -- Parse length-prefixed name: 4Lean3Foo3bar
      let mut len := 0
      while i < chars.length && (chars[i]!).isDigit do
        len := len * 10 + ((chars[i]!).toNat - '0'.toNat)
        i := i + 1
      if len > 0 && i + len <= chars.length then
        let name := String.ofList (chars.drop i |>.take len)
        parts := parts.push name
        i := i + len
      else
        break
    if parts.size > 0 then
      return some (String.intercalate "." parts.toList)

  return none

/-- Parse a line from `perf script` output.
    Format: `binary pid tid timestamp: samples event: symbol+offset (module)`
    Example: `lean 12345 12345 1234.567890:      1 cycles:  lean_Foo_bar+0x10 (/path/to/lean)`
-/
def parsePerfScriptLine (line : String) : Option CpuSample := Id.run do
  let parts := line.splitOn " "
  let parts := parts.filter (· != "")
  if parts.length < 6 then return none

  -- Find the symbol (usually after the event name ending with :)
  let mut symbolIdx : Option Nat := none
  for i in [0:parts.length] do
    if let some p := parts[i]? then
      if p.endsWith ":" && (strContains p "cycles" || strContains p "instructions"
          || strContains p "cache" || strContains p "branch") then
        symbolIdx := some (i + 1)

  match symbolIdx with
  | none => return none
  | some idx =>
    if let some sym := parts[idx]? then
      -- Extract symbol name (strip +offset)
      let symName := if strContains sym "+" then
        (sym.splitOn "+")[0]!
      else
        sym
      let leanDecl := demangleLeanSymbol symName
      return some {
        symbol := symName
        leanDecl := leanDecl
        samples := some 1
      }
    else
      return none

/-- Parse `perf script` output into CPU samples. -/
def parsePerfScript (content : String) : Array CpuSample := Id.run do
  let lines := content.splitOn "\n"
  let mut samples : Array CpuSample := #[]
  for line in lines do
    if let some sample := parsePerfScriptLine line then
      samples := samples.push sample
  return samples

/-- Aggregate samples by symbol. -/
def aggregateSamples (samples : Array CpuSample) : Array CpuSample := Id.run do
  let mut bySymbol : Std.HashMap String CpuSample := {}
  for sample in samples do
    if let some existing := bySymbol[sample.symbol]? then
      let newSamples := (existing.samples.getD 0) + (sample.samples.getD 0)
      bySymbol := bySymbol.insert sample.symbol { existing with samples := some newSamples }
    else
      bySymbol := bySymbol.insert sample.symbol sample
  return bySymbol.fold (fun acc _ v => acc.push v) #[]

/-- Parse `perf stat` output for hardware counters.
    Format: `  12,345,678      cycles                    #    3.456 GHz`
-/
structure PerfStatCounters where
  cycles : Option Nat := none
  instructions : Option Nat := none
  cacheMisses : Option Nat := none
  branchMisses : Option Nat := none
  contextSwitches : Option Nat := none
  pageFaults : Option Nat := none
  deriving Inhabited

def parseCounterValue (s : String) : Option Nat :=
  let clean := String.ofList (s.toList.filter (· != ','))
  String.toNat? clean

def parsePerfStatLine (line : String) (counters : PerfStatCounters) : PerfStatCounters :=
  let parts := line.splitOn " "
  let parts := parts.filter (· != "")
  if parts.length < 2 then counters
  else
    if let some valStr := parts[0]? then
      if let some eventName := parts[1]? then
        if let some val := parseCounterValue valStr then
          if strContains eventName "cycles" && !strContains eventName "cache" then
            { counters with cycles := some val }
          else if strContains eventName "instructions" then
            { counters with instructions := some val }
          else if strContains eventName "cache-misses" || strContains eventName "LLC-load-misses" then
            { counters with cacheMisses := some val }
          else if strContains eventName "branch-misses" then
            { counters with branchMisses := some val }
          else if strContains eventName "context-switch" then
            { counters with contextSwitches := some val }
          else if strContains eventName "page-fault" then
            { counters with pageFaults := some val }
          else counters
        else counters
      else counters
    else counters

def parsePerfStat (content : String) : PerfStatCounters := Id.run do
  let lines := content.splitOn "\n"
  let mut counters : PerfStatCounters := {}
  for line in lines do
    counters := parsePerfStatLine line counters
  return counters

/-- Map Lean symbol to source file using module name heuristics.
    Examples:
    - `Foo.Bar.baz` -> `Foo/Bar.lean`
    - `Lean.Meta.transform` -> `Lean/Meta.lean` or `lean4/src/Lean/Meta.lean`
    - `Init.Data.List.map` -> `Init/Data/List.lean`
-/
def symbolToFile (sym : String) (leanDecl : Option String) : Option String :=
  match leanDecl with
  | some decl =>
    -- Convert Foo.Bar.baz -> Foo/Bar.lean
    let parts := decl.splitOn "."
    if parts.length >= 2 then
      -- Take all but the last part (function name) as module path
      let moduleParts := parts.take (parts.length - 1)
      -- Check for common standard library prefixes
      let first := moduleParts[0]!
      let modulePath := String.intercalate "/" moduleParts ++ ".lean"
      -- Could add prefix handling for Lean stdlib, Mathlib, etc. here
      if first == "Lean" || first == "Init" || first == "Std" then
        some modulePath  -- Standard paths
      else
        some modulePath
    else if parts.length == 1 then
      some (parts[0]! ++ ".lean")
    else
      none
  | none =>
    -- Try to extract file info from raw symbol if no decl available
    -- Handle symbols that might contain path info directly
    if strContains sym ".lean" then
      some sym
    else
      none

/-- Enrich samples with file info from symbol demangling. -/
def enrichSamplesWithFiles (samples : Array CpuSample) : Array CpuSample :=
  samples.map fun s =>
    if s.file.isNone then
      { s with file := symbolToFile s.symbol s.leanDecl }
    else
      s

/-- Convert perf data to RuntimeData. -/
def perfToRuntimeData (scriptContent : Option String) (statContent : Option String)
    (tool : String := "perf") : RuntimeData := Id.run do
  let mut cpuSamples : Array CpuSample := #[]

  -- Parse script output
  if let some script := scriptContent then
    let rawSamples := parsePerfScript script
    let aggregated := aggregateSamples rawSamples
    cpuSamples := enrichSamplesWithFiles aggregated

  -- Parse stat output (add aggregate counters to first sample or create synthetic)
  if let some stat := statContent then
    let counters := parsePerfStat stat
    -- Add a synthetic global sample with aggregate counters
    let globalSample : CpuSample := {
      symbol := "_global"
      file := some "_global"
      cycles := counters.cycles
      instructions := counters.instructions
      cacheMisses := counters.cacheMisses
      branchMisses := counters.branchMisses
    }
    cpuSamples := cpuSamples.push globalSample

  return {
    source := { tool := tool, platform := some "linux" }
    cpuSamples := cpuSamples
  }

/-- Load perf data from file(s) and convert to MetricByFile. -/
def collectFromPerf (scriptPath : Option System.FilePath)
    (statPath : Option System.FilePath := none) : IO MetricByFile := do
  let scriptContent ← match scriptPath with
    | some p => some <$> IO.FS.readFile p
    | none => pure none
  let statContent ← match statPath with
    | some p => some <$> IO.FS.readFile p
    | none => pure none
  let data := perfToRuntimeData scriptContent statContent
  let floatMetrics := runtimeDataToMetricsByFile data
  return mergeMetricByFileF {} floatMetrics

end LeanBench.Runtime
