import Std
import Lean.Data.Json
import LeanBench
import LeanBench.Runner
import LeanBench.Plan
import LeanBench.Json
import LeanBench.TextScan
import LeanBench.ParsecScan

open LeanBench

def dummyCfg : BenchConfig := {
  suite := some "leanbench"
  tags := ["leanbench", "internal"]
  bytes := some 1024
  flops := some 2048
  items := some 256
}

def mkStats (base : Nat) : BenchStats :=
  { meanNs := Float.ofNat base
    stddevNs := 0.0
    minNs := base
    maxNs := base + 1
    medianNs := base
    p95Ns := base
    p99Ns := base
    totalNs := base * 10
    samples := 10 }

def mkResult (i : Nat) : BenchResult :=
  let entry : Bench := {
    name := s!"bench_{i}"
    action := pure ()
    config := dummyCfg
    report? := none
  }
  { entry := entry
    config := dummyCfg
    stats := mkStats (1000 + i)
    samples := #[(1000 + i)]
    extras := none }

def mkPlanEntry (i : Nat) : BenchPlanEntry :=
  { id := s!"leanbench/bench_{i}"
    name := s!"bench_{i}"
    suite := some "leanbench"
    tags := ["leanbench", "internal"]
    config := dummyCfg }

def dummyCount : Nat := 500

structure BenchFixtures where
  results : Array BenchResult
  planCore : PlanCore
  escapeInput : String
  scanLine : String
  scanLines : List String
  parseInput : String
  parseLoops : Nat
  deriving Inhabited

def buildFixtures : BenchFixtures := Id.run do
  let mut results : Array BenchResult := #[]
  for i in [0:dummyCount] do
    results := results.push (mkResult i)
  let mut planEntries : Array BenchPlanEntry := #[]
  for i in [0:dummyCount] do
    planEntries := planEntries.push (mkPlanEntry i)
  let planCore : PlanCore := {
    version := 1
    seed := 42
    shuffled := true
    partition := none
    entries := planEntries
  }
  let mut chars : Array Char := #[]
  for _ in [0:200000] do
    chars := chars.push '"'
    chars := chars.push '\\'
    chars := chars.push '\n'
    chars := chars.push '\t'
    chars := chars.push 'a'
  let escapeInput := String.ofList chars.toList
  let scanLine :=
    "  if foo then bar := baz -- TODO porting note #adaptation_note nolint"
  let scanLines := List.replicate 200 scanLine
  let parseInput := "123456789012345"
  let parseLoops := 50000
  return {
    results := results,
    planCore := planCore,
    escapeInput := escapeInput,
    scanLine := scanLine,
    scanLines := scanLines,
    parseInput := parseInput,
    parseLoops := parseLoops
  }

def fixturesForConfig : BenchFixtures := buildFixtures

initialize fixturesRef : IO.Ref BenchFixtures <- IO.mkRef buildFixtures

def escapeBytes : Nat := fixturesForConfig.escapeInput.length
def prettyBytes : Nat := (renderPrettyTable fixturesForConfig.results none false).length
def jsonBytes : Nat := (renderResultsJson fixturesForConfig.results).length
def planBytes : Nat := (renderPlanCoreJson fixturesForConfig.planCore).length
def scanItems : Nat := fixturesForConfig.scanLines.length
def scanBytes : Nat := fixturesForConfig.scanLines.foldl (fun acc line => acc + line.length) 0
def parseItems : Nat := fixturesForConfig.parseLoops
def parseBytes : Nat := fixturesForConfig.parseLoops * fixturesForConfig.parseInput.length

def cfgJsonEscape : BenchConfig := {
  suite := some "leanbench"
  tags := ["leanbench", "json"]
  bytes := some escapeBytes
}

def cfgPretty : BenchConfig := {
  suite := some "leanbench"
  tags := ["leanbench", "render", "pretty"]
  items := some dummyCount
  bytes := some prettyBytes
}

def cfgJsonRender : BenchConfig := {
  suite := some "leanbench"
  tags := ["leanbench", "render", "json"]
  items := some dummyCount
  bytes := some jsonBytes
}

def cfgPlanRender : BenchConfig := {
  suite := some "leanbench"
  tags := ["leanbench", "render", "plan"]
  items := some dummyCount
  bytes := some planBytes
}

def cfgScanLines : BenchConfig := {
  suite := some "leanbench"
  tags := ["leanbench", "observe", "scan"]
  items := some scanItems
  bytes := some scanBytes
  samples := 10
}

def cfgScanLinesFast : BenchConfig := {
  suite := some "leanbench"
  tags := ["leanbench", "observe", "scan", "fast"]
  items := some scanItems
  bytes := some scanBytes
  samples := 10
}

def cfgParseNat : BenchConfig := {
  suite := some "leanbench"
  tags := ["leanbench", "observe", "parse"]
  items := some parseItems
  bytes := some parseBytes
  samples := 10
}

def cfgParsecTokens : BenchConfig := {
  suite := some "leanbench"
  tags := ["leanbench", "observe", "parsec"]
  items := some scanItems
  bytes := some scanBytes
  samples := 10
}

bench "leanbench/json_escape_1m" (cfgJsonEscape) do
  let fixtures ← fixturesRef.get
  let out := jsonEscape fixtures.escapeInput
  if out.length == 0 then
    IO.println ""

bench "leanbench/render_pretty_500" (cfgPretty) do
  let fixtures ← fixturesRef.get
  let out := renderPrettyTable fixtures.results none false
  if out.length == 0 then
    IO.println ""

bench "leanbench/render_json_500" (cfgJsonRender) do
  let fixtures ← fixturesRef.get
  let out := renderResultsJson fixtures.results
  if out.length == 0 then
    IO.println ""

bench "leanbench/render_plan_500" (cfgPlanRender) do
  let fixtures ← fixturesRef.get
  let out := renderPlanCoreJson fixtures.planCore
  if out.length == 0 then
    IO.println ""

bench "leanbench/scan_lines_200" (cfgScanLines) do
  let fixtures ← fixturesRef.get
  let acc := LeanBench.ParsecScan.scanLinesParsec fixtures.scanLines
  if acc.loc == 0 then
    IO.println ""

bench "leanbench/parse_nat_50000" (cfgParseNat) do
  let fixtures ← fixturesRef.get
  let mut sum := 0
  for _ in [0:fixtures.parseLoops] do
    match parseNat? fixtures.parseInput with
    | some n => sum := sum + n
    | none => pure ()
  if sum == 0 then
    IO.println ""

bench "leanbench/parsec_tokenize_200" (cfgParsecTokens) do
  let fixtures ← fixturesRef.get
  let mut total := 0
  for line in fixtures.scanLines do
    let tokens := LeanBench.ParsecScan.tokenizeLineParsec line
    total := total + tokens.length
  if total == 0 then
    IO.println ""

bench "leanbench/scan_lines_200_fast" (cfgScanLinesFast) do
  let fixtures ← fixturesRef.get
  let acc := scanLines fixtures.scanLines
  if acc.loc == 0 then
    IO.println ""
