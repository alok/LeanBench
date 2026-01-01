import Std
import Lean.Data.Json
import Cli
import LeanBench.Bench
import LeanBench.Json
import LeanBench.Plan

namespace LeanBench

inductive OutputFormat where
  | pretty
  | prettyFull
  | json
  | radar
  deriving Inhabited, BEq

structure CliConfig where
  format : OutputFormat := .pretty
  listOnly : Bool := false
  listTags : Bool := false
  listSuites : Bool := false
  planOnly : Bool := false
  filter : Option String := none
  filterSuite : Option String := none
  filterTags : Array String := #[]
  allTags : Bool := false
  samples? : Option Nat := none
  warmup? : Option Nat := none
  minTimeMs? : Option Nat := none
  seed : Nat := 0
  shuffle : Bool := false
  partition : Option PartitionSpec := none
  planOut : Option System.FilePath := none
  manifestOut : Option System.FilePath := none
  groupBySuite : Bool := false
  radarSuite : Option String := none
  radarOut : Option System.FilePath := none
  jsonOut : Option System.FilePath := none
  comparePath : Option System.FilePath := none
  savePath : Option System.FilePath := none
  quiet : Bool := false

structure BenchStats where
  meanNs : Float
  stddevNs : Float
  minNs : Nat
  maxNs : Nat
  medianNs : Nat
  p95Ns : Nat
  p99Ns : Nat
  totalNs : Nat
  samples : Nat

structure BenchResult where
  entry : Bench
  config : BenchConfig
  stats : BenchStats
  samples : Array Nat

@[inline] def nsPerMs : Nat := 1_000_000
@[inline] def nsPerUs : Nat := 1_000
@[inline] def nsPerS : Nat := 1_000_000_000

@[inline] def pad3 (n : Nat) : String :=
  let s := toString n
  if s.length == 1 then "00" ++ s
  else if s.length == 2 then "0" ++ s
  else s

@[inline] def formatNs (ns : Nat) : String :=
  if ns < nsPerUs then
    s!"{ns} ns"
  else if ns < nsPerMs then
    let whole := ns / nsPerUs
    let frac := (ns % nsPerUs)
    s!"{whole}.{pad3 frac} us"
  else if ns < nsPerS then
    let whole := ns / nsPerMs
    let frac := (ns % nsPerMs) / 1_000
    s!"{whole}.{pad3 frac} ms"
  else
    let whole := ns / nsPerS
    let frac := (ns % nsPerS) / nsPerMs
    s!"{whole}.{pad3 frac} s"

@[inline] def formatNsFloat (ns : Float) : String :=
  if ns < (Float.ofNat nsPerUs) then
    s!"{ns.toString} ns"
  else if ns < (Float.ofNat nsPerMs) then
    let v := ns / (Float.ofNat nsPerUs)
    s!"{v.toString} us"
  else if ns < (Float.ofNat nsPerS) then
    let v := ns / (Float.ofNat nsPerMs)
    s!"{v.toString} ms"
  else
    let v := ns / (Float.ofNat nsPerS)
    s!"{v.toString} s"

@[inline] def statsOf (samples : Array Nat) : BenchStats :=
  if samples.size == 0 then
    { meanNs := 0.0, stddevNs := 0.0, minNs := 0, maxNs := 0, medianNs := 0, p95Ns := 0, p99Ns := 0, totalNs := 0, samples := 0 }
  else
    let total := samples.foldl (fun acc x => acc + x) 0
    let sorted := samples.qsort (· < ·)
    let min := sorted[0]!
    let max := sorted[sorted.size - 1]!
    let percentile (p : Nat) : Nat :=
      if sorted.size == 0 then 0
      else
        let idx := (p * (sorted.size - 1)) / 100
        sorted[idx]!
    let n := samples.size
    let mean := (Float.ofNat total) / (Float.ofNat n)
    let variance := samples.foldl (fun acc x =>
      let d := (Float.ofNat x) - mean
      acc + d * d) 0.0 / (Float.ofNat n)
    { meanNs := mean
      stddevNs := Float.sqrt variance
      minNs := min
      maxNs := max
      medianNs := percentile 50
      p95Ns := percentile 95
      p99Ns := percentile 99
      totalNs := total
      samples := n }

@[inline] def applyOverrides (cfg : BenchConfig) (cli : CliConfig) : BenchConfig :=
  { cfg with
    warmup := cli.warmup?.getD cfg.warmup
    samples := cli.samples?.getD cfg.samples
    minTimeMs := cli.minTimeMs?.getD cfg.minTimeMs
  }

@[inline] def containsSubstr (s pat : String) : Bool :=
  if pat.length == 0 then
    true
  else
    let s := s.toList
    let p := pat.toList
    let rec isPrefix : List Char → List Char → Bool
      | _, [] => true
      | [], _ => false
      | c::cs, d::ds => c == d && isPrefix cs ds
    let rec loop : List Char → Bool
      | [] => false
      | l@(_::tl) => if isPrefix l p then true else loop tl
    loop s

@[inline] def matchesTags (benchTags : List String) (filterTags : Array String) (allTags : Bool) : Bool :=
  if filterTags.isEmpty then
    true
  else if allTags then
    filterTags.all (fun t => benchTags.contains t)
  else
    filterTags.any (fun t => benchTags.contains t)

@[inline] def shouldKeep (entry : Bench) (cli : CliConfig) (suitePred? : Option (Bench → Bool)) : Bool :=
  let nameOk :=
    match cli.filter with
    | none => true
    | some pat => containsSubstr entry.name pat
  let suiteOk :=
    match suitePred? with
    | none => true
    | some pred => pred entry
  nameOk && suiteOk && matchesTags entry.config.tags cli.filterTags cli.allTags

@[inline] def resolveSuiteFilter (suiteName? : Option String) : IO (Option (Bench → Bool)) := do
  match suiteName? with
  | none => pure none
  | some name =>
      match (← findSuite? name) with
      | some suiteDef => pure (some suiteDef.filter)
      | none => pure (some fun b => b.config.suite == some name)

@[inline] def collectTags (benches : Array Bench) : Array String := Id.run do
  let mut set : Std.TreeSet String compare := {}
  for b in benches do
    for t in b.config.tags do
      set := set.insert t
  return set.toArray

@[inline] def collectSuites (benches : Array Bench) (suites : Array Suite) : Array String := Id.run do
  let mut set : Std.TreeSet String compare := {}
  if suites.isEmpty then
    for b in benches do
      match b.config.suite with
      | none => pure ()
      | some s => set := set.insert s
  else
    for s in suites do
      if benches.any s.filter then
        set := set.insert s.name
  return set.toArray

@[inline] def runBench (entry : Bench) (cli : CliConfig) : IO BenchResult := do
  let cfg := applyOverrides entry.config cli
  for _ in [0:cfg.warmup] do
    entry.action
  let minTimeNs := cfg.minTimeMs * nsPerMs
  let mut samples : Array Nat := #[]
  let mut total : Nat := 0
  while samples.size < cfg.samples || (minTimeNs > 0 && total < minTimeNs) do
    let start ← IO.monoNanosNow
    entry.action
    let stop ← IO.monoNanosNow
    let elapsed := stop - start
    samples := samples.push elapsed
    total := total + elapsed
  let stats := statsOf samples
  pure { entry := entry, config := cfg, stats := stats, samples := samples }

@[inline] def padRight (s : String) (n : Nat) : String :=
  let len := s.length
  if len >= n then s else s ++ String.ofList (List.replicate (n - len) ' ')

@[inline] def formatFloat3 (x : Float) : String :=
  let y := Float.round (x * 1000.0) / 1000.0
  y.toString

@[inline] def formatSigned (x : Float) : String :=
  if x < 0.0 then formatFloat3 x else "+" ++ formatFloat3 x

@[inline] def rateFromNs (units : Nat) (meanNs : Float) : Float :=
  if meanNs <= 0.0 then 0.0 else units.toFloat / meanNs

@[inline] def bandwidthGbps? (r : BenchResult) : Option Float :=
  r.entry.config.bytes.map (fun bytes => rateFromNs bytes r.stats.meanNs)

@[inline] def throughputGflops? (r : BenchResult) : Option Float :=
  r.entry.config.flops.map (fun flops => rateFromNs flops r.stats.meanNs)

@[inline] def baselineFor (baseline? : Option (Std.HashMap String Float)) (name : String) : Option Float :=
  match baseline? with
  | none => none
  | some m => m.get? name

@[inline] def renderPrettyTable (results : Array BenchResult)
    (baseline? : Option (Std.HashMap String Float)) (full : Bool) : String := Id.run do
  let hasBytes := results.any (fun r => r.entry.config.bytes.isSome)
  let hasFlops := results.any (fun r => r.entry.config.flops.isSome)
  let baseHeader := #["benchmark", "mean", "min", "max", "stddev", "samples"]
  let extraHeader := if full then #["median", "p95", "p99"] else #[]
  let metricHeader :=
    (if hasBytes then #["GB/s"] else #[]) ++
    (if hasFlops then #["GFLOP/s"] else #[])
  let compareHeader :=
    match baseline? with
    | none => #[]
    | some _ => #["baseline", "ratio", "delta"]
  let header := baseHeader ++ extraHeader ++ metricHeader ++ compareHeader
  let rows := results.map fun r =>
    let baseCols := #[r.entry.name,
      formatNsFloat r.stats.meanNs,
      formatNs r.stats.minNs,
      formatNs r.stats.maxNs,
      formatNsFloat r.stats.stddevNs,
      toString r.stats.samples]
    let extraCols :=
      if full then
        #[formatNs r.stats.medianNs,
          formatNs r.stats.p95Ns,
          formatNs r.stats.p99Ns]
      else
        #[]
    let metricCols :=
      (if hasBytes then
        #[match bandwidthGbps? r with
          | some v => formatFloat3 v
          | none => "-"]
      else #[]) ++
      (if hasFlops then
        #[match throughputGflops? r with
          | some v => formatFloat3 v
          | none => "-"]
      else #[])
    let compareCols :=
      match baselineFor baseline? r.entry.name with
      | none => #[]
      | some base =>
          if base <= 0.0 then
            #["-", "-", "-"]
          else
            let ratio := r.stats.meanNs / base
            let delta := (r.stats.meanNs - base) / base * 100.0
            #[formatNsFloat base, formatFloat3 ratio ++ "x", formatSigned delta ++ "%"]
    baseCols ++ extraCols ++ metricCols ++ compareCols
  let mut widths := header.map String.length
  for row in rows do
    for i in [:row.size] do
      let len := row[i]!.length
      if len > widths[i]! then
        widths := widths.set! i len
  let renderRow (row : Array String) : String :=
    let cols := (List.range row.size).map fun i => padRight row[i]! (widths[i]!)
    String.intercalate "  " cols
  let mut out := ""
  out := out ++ renderRow header ++ "\n"
  out := out ++ renderRow (header.map (fun h => String.ofList (List.replicate h.length '-'))) ++ "\n"
  for row in rows do
    out := out ++ renderRow row ++ "\n"
  return out

@[inline] def suiteKey (s : Option String) : String :=
  s.getD "(none)"

@[inline] def groupResultsBySuite (results : Array BenchResult) : Array (String × Array BenchResult) := Id.run do
  let mut map : Std.TreeMap String (Array BenchResult) := {}
  for r in results do
    let key := suiteKey r.entry.config.suite
    let arr := map.getD key #[]
    map := map.insert key (arr.push r)
  return map.toArray

@[inline] def renderPrettyWithBaseline (results : Array BenchResult)
    (baseline? : Option (Std.HashMap String Float)) (full : Bool) (groupBySuite : Bool) : String := Id.run do
  if !groupBySuite then
    return renderPrettyTable results baseline? full
  let groups := groupResultsBySuite results
  let mut out := ""
  for idx in [:groups.size] do
    let (name, group) := groups[idx]!
    out := out ++ s!"suite: {name}\n"
    out := out ++ renderPrettyTable group baseline? full
    if idx + 1 < groups.size then
      out := out ++ "\n"
  return out

@[inline] def renderPretty (results : Array BenchResult) : String :=
  renderPrettyWithBaseline results none false false

@[inline] def renderJson (results : Array BenchResult) : String :=
  let items := results.toList.map fun r =>
    let name := jsonEscape r.entry.name
    let bandwidth := match bandwidthGbps? r with
      | some v => formatFloat3 v
      | none => "null"
    let gflops := match throughputGflops? r with
      | some v => formatFloat3 v
      | none => "null"
    let fields := #[
      "\"name\":\"" ++ name ++ "\"",
      "\"mean_ns\":" ++ r.stats.meanNs.toString,
      "\"stddev_ns\":" ++ r.stats.stddevNs.toString,
      "\"min_ns\":" ++ toString r.stats.minNs,
      "\"max_ns\":" ++ toString r.stats.maxNs,
      "\"median_ns\":" ++ toString r.stats.medianNs,
      "\"p95_ns\":" ++ toString r.stats.p95Ns,
      "\"p99_ns\":" ++ toString r.stats.p99Ns,
      "\"samples\":" ++ toString r.stats.samples,
      "\"total_ns\":" ++ toString r.stats.totalNs,
      "\"bandwidth_gbps\":" ++ bandwidth,
      "\"throughput_gflops\":" ++ gflops
    ]
    "{" ++ String.intercalate "," fields.toList ++ "}"
  "[" ++ String.intercalate "," items ++ "]"

@[inline] def radarName (benchName : String) (benchSuite : Option String) (radarSuite : Option String) : String :=
  match radarSuite with
  | some s => s ++ "//" ++ benchName
  | none =>
      match benchSuite with
      | some s => s ++ "//" ++ benchName
      | none => benchName

@[inline] def renderRadarLines (results : Array BenchResult) (radarSuite : Option String) : Array String :=
  results.map fun r =>
    let name := jsonEscape (radarName r.entry.name r.entry.config.suite radarSuite)
    "{" ++
      "\"name\":\"" ++ name ++ "\"," ++
      "\"unit\":\"ns\"," ++
      "\"value\":" ++ r.stats.meanNs.toString ++
    "}"

@[inline] def jsonObjFind? (obj : Std.TreeMap.Raw String Lean.Json) (key : String) : Option Lean.Json :=
  obj.get? key

@[inline] def jsonAsString? (j : Lean.Json) : Option String :=
  match j with
  | .str s => some s
  | _ => none

@[inline] def jsonAsFloat? (j : Lean.Json) : Option Float :=
  match j with
  | .num n => some n.toFloat
  | _ => none

@[inline] def parseBaselineEntry (j : Lean.Json) : Except String (String × Float) :=
  match j with
  | .obj obj =>
      match jsonObjFind? obj "name" with
      | none => throw "baseline entry missing name"
      | some nameJson =>
          match jsonAsString? nameJson with
          | none => throw "baseline entry name is not a string"
          | some name =>
              match jsonObjFind? obj "mean_ns" with
              | none => throw "baseline entry missing mean_ns"
              | some meanJson =>
                  match jsonAsFloat? meanJson with
                  | none => throw "baseline mean_ns is not a number"
                  | some mean => pure (name, mean)
  | _ => throw "baseline JSON must be an array of objects"

@[inline] def loadBaseline (path : System.FilePath) : IO (Std.HashMap String Float) := do
  let data ← IO.FS.readFile path
  match Lean.Json.parse data with
  | .error err => throw <| IO.userError s!"baseline parse error: {err}"
  | .ok json =>
      match json with
      | .arr items =>
          let mut map : Std.HashMap String Float := ∅
          for item in items do
            match parseBaselineEntry item with
            | .ok (name, mean) =>
                map := map.insert name mean
            | .error err =>
                throw <| IO.userError s!"baseline parse error: {err}"
          return map
      | _ => throw <| IO.userError "baseline JSON must be an array"

@[inline] def parseFormat (s : String) : Option OutputFormat :=
  if s == "pretty" then some .pretty
  else if s == "full" || s == "pretty-full" then some .prettyFull
  else if s == "json" then some .json
  else if s == "radar" then some .radar
  else none

@[inline] def applySave (cfg : CliConfig) : CliConfig :=
  match cfg.savePath with
  | none => cfg
  | some path =>
      let jsonOut := match cfg.jsonOut with
        | some _ => cfg.jsonOut
        | none => some path
      { cfg with format := .json, jsonOut := jsonOut }

@[inline] def configFromParsed (p : Cli.Parsed) : IO CliConfig := do
  let mut cfg : CliConfig := {}
  if p.hasFlag "list" then
    cfg := { cfg with listOnly := true }
  if p.hasFlag "list-tags" then
    cfg := { cfg with listTags := true }
  if p.hasFlag "list-suites" then
    cfg := { cfg with listSuites := true }
  if p.hasFlag "plan" then
    cfg := { cfg with planOnly := true }
  if p.hasFlag "all-tags" then
    cfg := { cfg with allTags := true }
  if p.hasFlag "quiet" then
    cfg := { cfg with quiet := true }
  if p.hasFlag "shuffle" then
    cfg := { cfg with shuffle := true }
  match p.flag? "match" with
  | some f => cfg := { cfg with filter := some (f.as! String) }
  | none => pure ()
  match p.flag? "tags" with
  | some f => cfg := { cfg with filterTags := f.as! (Array String) }
  | none => pure ()
  match p.flag? "samples" with
  | some f => cfg := { cfg with samples? := some (f.as! Nat) }
  | none => pure ()
  match p.flag? "warmup" with
  | some f => cfg := { cfg with warmup? := some (f.as! Nat) }
  | none => pure ()
  match p.flag? "min-time-ms" with
  | some f => cfg := { cfg with minTimeMs? := some (f.as! Nat) }
  | none => pure ()
  match p.flag? "seed" with
  | some f => cfg := { cfg with seed := (f.as! Nat) }
  | none => pure ()
  match p.flag? "partition" with
  | some f =>
      match parsePartitionSpec (f.as! String) with
      | .ok spec => cfg := { cfg with partition := some spec }
      | .error err => throw <| IO.userError err
  | none => pure ()
  match p.flag? "group-by" with
  | some f =>
      let v := (f.as! String).toLower
      if v == "suite" then
        cfg := { cfg with groupBySuite := true }
      else
        throw <| IO.userError s!"unknown group-by: {v}"
  | none => pure ()
  match p.flag? "suite" with
  | some f => cfg := { cfg with filterSuite := some (f.as! String) }
  | none => pure ()
  match p.flag? "plan-out" with
  | some f => cfg := { cfg with planOut := some (System.FilePath.mk (f.as! String)) }
  | none => pure ()
  match p.flag? "manifest-out" with
  | some f => cfg := { cfg with manifestOut := some (System.FilePath.mk (f.as! String)) }
  | none => pure ()
  match p.flag? "radar-suite" with
  | some f => cfg := { cfg with radarSuite := some (f.as! String) }
  | none => pure ()
  match p.flag? "radar-out" with
  | some f => cfg := { cfg with radarOut := some (System.FilePath.mk (f.as! String)) }
  | none => pure ()
  match p.flag? "json-out" with
  | some f => cfg := { cfg with jsonOut := some (System.FilePath.mk (f.as! String)) }
  | none => pure ()
  match p.flag? "compare" with
  | some f => cfg := { cfg with comparePath := some (System.FilePath.mk (f.as! String)) }
  | none => pure ()
  match p.flag? "save" with
  | some f => cfg := { cfg with savePath := some (System.FilePath.mk (f.as! String)) }
  | none => pure ()
  match p.flag? "format" with
  | some f =>
      let fmt := (f.as! String).toLower
      match parseFormat fmt with
      | some out => cfg := { cfg with format := out }
      | none => throw <| IO.userError s!"unknown format: {fmt}"
  | none => pure ()
  let json := p.hasFlag "json"
  let radar := p.hasFlag "radar"
  if json && radar then
    throw <| IO.userError "cannot use --json and --radar together"
  else if json then
    cfg := { cfg with format := .json }
  else if radar then
    cfg := { cfg with format := .radar }
  return applySave cfg

@[inline] def radarSuiteFromEnv (cli : CliConfig) : IO (Option String) := do
  if cli.radarSuite.isSome then
    pure cli.radarSuite
  else if let some s ← IO.getEnv "LEANBENCH_RADAR_SUITE" then
    return some s
  else
    return (← IO.getEnv "LEANBENCH_SUITE")

@[inline] def radarOutPath (cli : CliConfig) : IO (Option System.FilePath) := do
  match cli.radarOut with
  | some p => pure (some p)
  | none =>
      match (← IO.getEnv "RADAR_JSONL") with
      | some p => pure (some (System.FilePath.mk p))
      | none => pure none

@[inline] def appendFile (path : System.FilePath) (content : String) : IO Unit :=
  IO.FS.withFile path .append (fun h => h.putStr content)

@[inline] def writeLines (lines : Array String) (path? : Option System.FilePath) : IO Unit := do
  let content := String.intercalate "\n" lines.toList
  match path? with
  | none => IO.println content
  | some path => appendFile path (content ++ "\n")

@[inline] def writeOutput (content : String) (path? : Option System.FilePath) : IO Unit := do
  match path? with
  | none => IO.println content
  | some path => IO.FS.writeFile path content

@[inline] def planCoreOf (benches : Array Bench) (cfg : CliConfig) : PlanCore :=
  let entries := benches.map fun b =>
    entryOf b (applyOverrides b.config cfg)
  { seed := cfg.seed
    shuffled := cfg.shuffle
    partition := cfg.partition
    entries := entries }

@[inline] def runWithConfig (cfg : CliConfig) : IO UInt32 := do
  let benches ← listBenches
  let suitePred? ← resolveSuiteFilter cfg.filterSuite
  let benches := benches.filter (fun b => shouldKeep b cfg suitePred?)
  let benches := if cfg.shuffle then shuffleBenches benches cfg.seed else benches
  let benches := match cfg.partition with
    | none => benches
    | some p => partitionBenches benches p
  if cfg.listTags || cfg.listSuites then
    let suites ← listSuites
    if cfg.listTags then
      for t in collectTags benches do
        IO.println t
    if cfg.listTags && cfg.listSuites then
      IO.println ""
    if cfg.listSuites then
      for s in collectSuites benches suites do
        IO.println s
    return (0 : UInt32)
  if cfg.listOnly then
    if cfg.format == .json then
      let entries := benches.map fun b => entryOf b (applyOverrides b.config cfg)
      let output := renderBenchListJson entries
      writeOutput output cfg.jsonOut
    else
      for b in benches do
        IO.println b.name
    return (0 : UInt32)
  if cfg.planOnly then
    let core := planCoreOf benches cfg
    let coreJson := renderPlanCoreJson core
    let planHash := hashStringHex coreJson
    let manifest ← buildManifest planHash benches.size cfg.partition
    let planJson := renderPlanJson core manifest
    writeOutput planJson cfg.planOut
    if let some path := cfg.manifestOut then
      writeOutput (renderManifestJson manifest) (some path)
    return (0 : UInt32)
  if benches.size == 0 then
    IO.eprintln "no benchmarks registered"
    return (1 : UInt32)
  if let some path := cfg.manifestOut then
    let core := planCoreOf benches cfg
    let coreJson := renderPlanCoreJson core
    let planHash := hashStringHex coreJson
    let manifest ← buildManifest planHash benches.size cfg.partition
    writeOutput (renderManifestJson manifest) (some path)
  let mut results : Array BenchResult := #[]
  for b in benches do
    unless cfg.quiet do
      let cfg' := applyOverrides b.config cfg
      IO.eprintln s!"bench {b.name} (warmup {cfg'.warmup}, samples {cfg'.samples})"
    let r ← runBench b cfg
    results := results.push r
  let baseline? ←
    match cfg.comparePath with
    | none => pure none
    | some path => some <$> loadBaseline path
  match cfg.format with
  | .pretty =>
      let output := renderPrettyWithBaseline results baseline? false cfg.groupBySuite
      IO.println output
      return (0 : UInt32)
  | .prettyFull =>
      let output := renderPrettyWithBaseline results baseline? true cfg.groupBySuite
      IO.println output
      return (0 : UInt32)
  | .json =>
      let output := renderJson results
      match cfg.jsonOut with
      | some path => IO.FS.writeFile path output
      | none => IO.println output
      return (0 : UInt32)
  | .radar =>
      let radarSuite ← radarSuiteFromEnv cfg
      let lines := renderRadarLines results radarSuite
      let outPath ← radarOutPath cfg
      writeLines lines outPath
      return (0 : UInt32)

@[inline] def runLeanbenchCmd (p : Cli.Parsed) : IO UInt32 := do
  try
    let cfg ← configFromParsed p
    runWithConfig cfg
  catch e =>
    IO.eprintln s!"{e}"
    return (1 : UInt32)

open Cli

def leanbenchCmd : Cmd := `[Cli|
  leanbench VIA runLeanbenchCmd; ["0.1.0"] "LeanBench benchmark runner."
  FLAGS:
    list; "List available benchmarks."
    "list-tags"; "List tags (respects filters)."
    "list-suites"; "List suites (respects filters)."
    plan; "Print a deterministic run plan as JSON and exit."
    "match" : String; "Only run benchmarks whose name contains this substring."
    tags : Array String; "Only run benchmarks with any of these tags (comma-separated)."
    "all-tags"; "Require all tags listed in --tags."
    samples : Nat; "Override sample count."
    warmup : Nat; "Override warmup count."
    "min-time-ms" : Nat; "Run until total time reaches N ms."
    seed : Nat; "Seed for deterministic shuffling."
    shuffle; "Shuffle benchmark order deterministically (use --seed)."
    partition : String; "Partition benches: count:m/n or hash:m/n."
    "plan-out" : String; "Write plan JSON to file."
    "manifest-out" : String; "Write run manifest JSON to file."
    "group-by" : String; "Group output by: suite."
    format : String; "Output format: pretty|full|json|radar."
    json; "Alias for --format json."
    radar; "Alias for --format radar."
    "suite" : String; "Only run benchmarks in this suite."
    "radar-suite" : String; "Prefix radar names with <suite>//."
    "radar-out" : String; "Write radar JSONL to file (defaults to RADAR_JSONL)."
    "json-out" : String; "Write JSON output to file."
    compare : String; "Compare against a JSON baseline."
    save : String; "Write JSON output to file and set format=json."
    quiet; "Suppress per-benchmark progress."
]

@[inline] def runMain (args : List String) : IO UInt32 :=
  leanbenchCmd.validate args

end LeanBench
