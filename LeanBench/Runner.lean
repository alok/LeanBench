import Std
import Lean.Data.Json
import Cli
import LeanBench.Bench
import LeanBench.Json
import LeanBench.Plan

namespace LeanBench

@[inline] def leanbenchVersion : String := "0.2.0"
@[inline] def jsonSchemaVersion : Nat := 1

inductive OutputFormat where
  | pretty
  | prettyFull
  | json
  | jsonl
  | radar
  deriving Inhabited, BEq

structure CliConfig where
  format : OutputFormat := .pretty
  args : Array String := #[]
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
  threads? : Option Nat := none
  seed : Nat := 0
  shuffle : Bool := false
  partition : Option PartitionSpec := none
  planOut : Option System.FilePath := none
  manifestOut : Option System.FilePath := none
  groupBySuite : Bool := false
  radarSuite : Option String := none
  radarOut : Option System.FilePath := none
  jsonOut : Option System.FilePath := none
  jsonlOut : Option System.FilePath := none
  comparePath : Option System.FilePath := none
  savePath : Option System.FilePath := none
  artifactsDir : Option System.FilePath := none
  failOnRegressions : Bool := false
  regressAbsNs? : Option Nat := none
  regressRatio? : Option Float := none
  regressPct? : Option Float := none
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
  extras : Option Lean.Json := none
  trace : Option Lean.Json := none
  tracePath : Option System.FilePath := none
  sampleExtrasPath : Option System.FilePath := none

structure Regression where
  name : String
  base : Float
  mean : Float
  ratio : Float
  deltaPct : Float
  deltaNs : Float

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
    threads := cli.threads?.getD cfg.threads
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

@[inline] def effectiveThreads (cfg : BenchConfig) : Nat :=
  if cfg.threads == 0 then 1 else cfg.threads

@[inline] def scaledUnits (cfg : BenchConfig) (units : Nat) : Nat :=
  units * effectiveThreads cfg

@[inline] def isAsciiAlphaNum (c : Char) : Bool :=
  ('a' <= c && c <= 'z') ||
  ('A' <= c && c <= 'Z') ||
  ('0' <= c && c <= '9')

@[inline] def safeBenchFileStem (benchId : String) : String := Id.run do
  let mut chars : Array Char := #[]
  for c in benchId.toList do
    if isAsciiAlphaNum c || c == '-' || c == '_' || c == '.' then
      chars := chars.push c
    else if c == '/' || c == '\\' then
      chars := chars.push '_'
      chars := chars.push '_'
    else
      chars := chars.push '_'
  let out := String.ofList chars.toList
  if out.length == 0 then "bench" else out

@[inline] def jsonNatArray (xs : Array Nat) : String :=
  "[" ++ String.intercalate "," (xs.toList.map toString) ++ "]"

@[inline] def runBench (entry : Bench) (cli : CliConfig) : IO BenchResult := do
  let cfg := applyOverrides entry.config cli
  let threads := effectiveThreads cfg
  let id := benchId entry
  let stem := safeBenchFileStem id
  let runOnce : IO Unit := do
    if threads <= 1 then
      entry.action
    else
      let mut tasks := Array.mkEmpty threads
      for _ in [:threads] do
        tasks := tasks.push (← IO.asTask (prio := .dedicated) entry.action)
      for t in tasks do
        let _ ← IO.wait t
  try
    if let some setup := entry.setup? then
      setup
    for _ in [0:cfg.warmup] do
      if let some beforeEach := entry.beforeEach? then
        beforeEach
      try
        runOnce
      finally
        if let some afterEach := entry.afterEach? then
          afterEach

    let minTimeNs := cfg.minTimeMs * nsPerMs
    let mut samples : Array Nat := #[]
    let mut sampleExtras : Array Lean.Json := #[]
    let mut total : Nat := 0
    while samples.size < cfg.samples || (minTimeNs > 0 && total < minTimeNs) do
      if let some beforeEach := entry.beforeEach? then
        beforeEach
      let start ← IO.monoNanosNow
      let stop ←
        try
          runOnce
          IO.monoNanosNow
        finally
          if let some afterEach := entry.afterEach? then
            afterEach
      let elapsed := stop - start
      let sampleIndex := samples.size
      samples := samples.push elapsed
      total := total + elapsed
      match entry.reportSample? with
      | none => pure ()
      | some reportSample =>
          match cli.artifactsDir with
          | none => pure ()
          | some _ =>
              let ctx : SampleCtx := {
                benchName := entry.name
                benchId := id
                suite := cfg.suite
                tags := cfg.tags
                config := cfg
                sampleIndex := sampleIndex
                elapsedNs := elapsed
                threads := threads
              }
              sampleExtras := sampleExtras.push (← reportSample ctx)

    let stats := statsOf samples
    let extras ←
      match entry.report? with
      | none => pure none
      | some report => some <$> report

    let sampleExtrasPath? ←
      match (entry.reportSample?, cli.artifactsDir) with
      | (some _, some dir) =>
          let rel : System.FilePath := (System.FilePath.mk "sample_extras") / (stem ++ ".json")
          let out := dir / rel
          IO.FS.createDirAll (dir / "sample_extras")
          let sampleExtrasJson := jsonArray (sampleExtras.map Lean.Json.compress)
          let content :=
            "{" ++
              "\"schema_version\":" ++ toString jsonSchemaVersion ++ "," ++
              "\"bench_id\":" ++ jsonString id ++ "," ++
              "\"bench_name\":" ++ jsonString entry.name ++ "," ++
              "\"samples_ns\":" ++ jsonNatArray samples ++ "," ++
              "\"sample_extras\":" ++ sampleExtrasJson ++
            "}"
          IO.FS.writeFile out content
          pure (some rel)
      | _ => pure none

    let (trace?, tracePath?) ←
      match entry.trace? with
      | none => pure (none, none)
      | some report => do
          let j ← report
          match cli.artifactsDir with
          | none => pure (some j, none)
          | some dir =>
              let rel : System.FilePath := (System.FilePath.mk "trace") / (stem ++ ".json")
              let out := dir / rel
              IO.FS.createDirAll (dir / "trace")
              IO.FS.writeFile out (Lean.Json.compress j)
              pure (none, some rel)

    pure ({
      entry := entry
      config := cfg
      stats := stats
      samples := samples
      extras := extras
      trace := trace?
      tracePath := tracePath?
      sampleExtrasPath := sampleExtrasPath?
    } : BenchResult)
  finally
    if let some teardown := entry.teardown? then
      teardown

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

@[inline] def ratePerSec (units : Nat) (meanNs : Float) : Float :=
  rateFromNs units meanNs * (Float.ofNat nsPerS)

@[inline] def bandwidthGbps? (r : BenchResult) : Option Float :=
  r.config.bytes.map (fun bytes => ratePerSec (scaledUnits r.config bytes) r.stats.meanNs / 1.0e9)

@[inline] def throughputGflops? (r : BenchResult) : Option Float :=
  r.config.flops.map (fun flops => ratePerSec (scaledUnits r.config flops) r.stats.meanNs / 1.0e9)

@[inline] def itemsPerSec? (r : BenchResult) : Option Float :=
  r.config.items.map (fun items => ratePerSec (scaledUnits r.config items) r.stats.meanNs)

@[inline] def hasRegressionThresholds (cfg : CliConfig) : Bool :=
  cfg.regressAbsNs?.isSome || cfg.regressRatio?.isSome || cfg.regressPct?.isSome

@[inline] def baselineFor (baseline? : Option (Std.HashMap String Float)) (name : String) : Option Float :=
  match baseline? with
  | none => none
  | some m => m.get? name

@[inline] def renderPrettyTable (results : Array BenchResult)
    (baseline? : Option (Std.HashMap String Float)) (full : Bool) : String := Id.run do
  let hasBytes := results.any (fun r => r.entry.config.bytes.isSome)
  let hasFlops := results.any (fun r => r.entry.config.flops.isSome)
  let hasItems := results.any (fun r => r.entry.config.items.isSome)
  let baseHeader := #["benchmark", "mean", "min", "max", "stddev", "samples"]
  let extraHeader := if full then #["median", "p95", "p99"] else #[]
  let metricHeader :=
    (if hasBytes then #["GB/s"] else #[]) ++
    (if hasFlops then #["GFLOP/s"] else #[]) ++
    (if hasItems then #["items/s"] else #[])
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
      else #[]) ++
      (if hasItems then
        #[match itemsPerSec? r with
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
  let headerLine := renderRow header
  let ruleLine := renderRow (header.map (fun h => String.ofList (List.replicate h.length '-')))
  let mut lines : Array String := #[headerLine, ruleLine]
  for row in rows do
    lines := lines.push (renderRow row)
  return String.intercalate "\n" lines.toList ++ "\n"

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
  let mut blocks : Array String := #[]
  for idx in [:groups.size] do
    let (name, group) := groups[idx]!
    let mut block := s!"suite: {name}\n" ++ renderPrettyTable group baseline? full
    if idx + 1 < groups.size then
      block := block ++ "\n"
    blocks := blocks.push block
  return String.intercalate "" blocks.toList

@[inline] def regressionThresholdsSummary (cfg : CliConfig) : String := Id.run do
  let mut parts : Array String := #[]
  match cfg.regressAbsNs? with
  | none => pure ()
  | some v => parts := parts.push s!"abs_ns>={v}"
  match cfg.regressRatio? with
  | none => pure ()
  | some v => parts := parts.push s!"ratio>={formatFloat3 v}"
  match cfg.regressPct? with
  | none => pure ()
  | some v => parts := parts.push s!"pct>={formatFloat3 v}%"
  if parts.isEmpty then
    return "any regression"
  return String.intercalate ", " parts.toList

@[inline] def isRegression (cfg : CliConfig) (base mean : Float) : Bool :=
  if mean <= base then
    false
  else
    let absHit :=
      match cfg.regressAbsNs? with
      | none => false
      | some v => mean - base >= Float.ofNat v
    let ratioHit :=
      match cfg.regressRatio? with
      | none => false
      | some v => mean / base >= v
    let pctHit :=
      match cfg.regressPct? with
      | none => false
      | some v => ((mean - base) / base * 100.0) >= v
    if !hasRegressionThresholds cfg then
      true
    else
      absHit || ratioHit || pctHit

@[inline] def collectRegressions (results : Array BenchResult) (baseline : Std.HashMap String Float)
    (cfg : CliConfig) : Array Regression := Id.run do
  let mut out : Array Regression := #[]
  for r in results do
    match baseline.get? r.entry.name with
    | none => pure ()
    | some base =>
        if base > 0.0 then
          let mean := r.stats.meanNs
          if isRegression cfg base mean then
            let ratio := mean / base
            let deltaPct := (mean - base) / base * 100.0
            let deltaNs := mean - base
            out := out.push {
              name := r.entry.name
              base := base
              mean := mean
              ratio := ratio
              deltaPct := deltaPct
              deltaNs := deltaNs
            }
  return out

@[inline] def renderRegressionSummary (regressions : Array Regression) (cfg : CliConfig) : String :=
  if regressions.isEmpty then
    ""
  else
    let header := s!"Regressions ({regressions.size}) [thresholds: {regressionThresholdsSummary cfg}]:"
    let lines := regressions.map fun r =>
      s!"- {r.name}: {formatSigned r.deltaPct}% ({formatFloat3 r.ratio}x, {formatNsFloat r.deltaNs})"
    String.intercalate "\n" (header :: lines.toList)

@[inline] def renderPretty (results : Array BenchResult) : String :=
  renderPrettyWithBaseline results none false false

@[inline] def renderBenchResultJson (r : BenchResult) : String :=
  let name := jsonEscape r.entry.name
  let bandwidth := match bandwidthGbps? r with
    | some v => formatFloat3 v
    | none => "null"
  let gflops := match throughputGflops? r with
    | some v => formatFloat3 v
    | none => "null"
  let itemsPerSec := match itemsPerSec? r with
    | some v => formatFloat3 v
    | none => "null"
  let extras := match r.extras with
    | some v => Lean.Json.compress v
    | none => "null"
  let trace := match r.trace with
    | some v => Lean.Json.compress v
    | none => "null"
  let tracePath := match r.tracePath with
    | some p => jsonString p.toString
    | none => "null"
  let sampleExtrasPath := match r.sampleExtrasPath with
    | some p => jsonString p.toString
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
    "\"throughput_gflops\":" ++ gflops,
    "\"items_per_s\":" ++ itemsPerSec,
    "\"extras\":" ++ extras,
    "\"trace\":" ++ trace,
    "\"trace_path\":" ++ tracePath,
    "\"sample_extras_path\":" ++ sampleExtrasPath
  ]
  "{" ++ String.intercalate "," fields.toList ++ "}"

@[inline] def renderResultsJson (results : Array BenchResult) : String :=
  jsonArray (results.map renderBenchResultJson)

@[inline] def renderRunMetaJson (manifest : RunManifest) (manifestHash : String) (args : Array String) : String :=
  let arch := match manifest.arch with | none => "null" | some s => jsonString s
  let lake := match manifest.lakeVersion with | none => "null" | some s => jsonString s
  let git := match manifest.gitSha with | none => "null" | some s => jsonString s
  let ts := match manifest.generatedEpochSec with | none => "null" | some s => jsonString s
  let partition := match manifest.partition with | none => "null" | some p => renderPartitionJson p
  let argsJson := jsonArray (args.map jsonString)
  "{" ++
    "\"tool\":" ++ jsonString "leanbench" ++ "," ++
    "\"version\":" ++ jsonString leanbenchVersion ++ "," ++
    "\"plan_hash\":" ++ jsonString manifest.planHash ++ "," ++
    "\"manifest_hash\":" ++ jsonString manifestHash ++ "," ++
    "\"bench_count\":" ++ toString manifest.benchCount ++ "," ++
    "\"partition\":" ++ partition ++ "," ++
    "\"os\":" ++ jsonString manifest.os ++ "," ++
    "\"arch\":" ++ arch ++ "," ++
    "\"lean_version\":" ++ jsonString manifest.leanVersion ++ "," ++
    "\"lake_version\":" ++ lake ++ "," ++
    "\"git_sha\":" ++ git ++ "," ++
    "\"generated_epoch_sec\":" ++ ts ++ "," ++
    "\"args\":" ++ argsJson ++
  "}"

@[inline] def renderRunResultsJson (results : Array BenchResult) (runMetaJson : String) : String :=
  let items := jsonArray (results.map renderBenchResultJson)
  "{" ++
    "\"schema_version\":" ++ toString jsonSchemaVersion ++ "," ++
    "\"run\":" ++ runMetaJson ++ "," ++
    "\"results\":" ++ items ++
  "}"

@[inline] def renderRunResultJsonLine (r : BenchResult) (runMetaJson : String) : String :=
  "{" ++
    "\"schema_version\":" ++ toString jsonSchemaVersion ++ "," ++
    "\"run\":" ++ runMetaJson ++ "," ++
    "\"result\":" ++ renderBenchResultJson r ++
  "}"

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
      "\"metric\":\"" ++ name ++ "\"," ++
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

@[inline] def baselineItemsFromJson (json : Lean.Json) : Except String (Array Lean.Json) :=
  match json with
  | .arr items => return items
  | .obj obj =>
      match jsonObjFind? obj "results" with
      | some (.arr items) => return items
      | some _ => throw "baseline JSON results must be an array"
      | none => throw "baseline JSON object missing results"
  | _ => throw "baseline JSON must be an array or object with results"

@[inline] def loadBaseline (path : System.FilePath) : IO (Std.HashMap String Float) := do
  let data ← IO.FS.readFile path
  match Lean.Json.parse data with
  | .error err => throw <| IO.userError s!"baseline parse error: {err}"
  | .ok json =>
      match baselineItemsFromJson json with
      | .error err => throw <| IO.userError s!"baseline parse error: {err}"
      | .ok items =>
          let mut map : Std.HashMap String Float := ∅
          for item in items do
            match parseBaselineEntry item with
            | .ok (name, mean) =>
                map := map.insert name mean
            | .error err =>
                throw <| IO.userError s!"baseline parse error: {err}"
          return map

@[inline] def parseFormat (s : String) : Option OutputFormat :=
  if s == "pretty" then some .pretty
  else if s == "full" || s == "pretty-full" then some .prettyFull
  else if s == "json" then some .json
  else if s == "jsonl" then some .jsonl
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

@[inline] def parseFloatFlag (label : String) (raw : String) : IO Float := do
  match Lean.Json.parse raw with
  | .ok (.num n) => return n.toFloat
  | .ok _ => throw <| IO.userError s!"{label} must be a number"
  | .error err => throw <| IO.userError s!"{label} parse error: {err}"

@[inline] def applyArtifacts (cfg : CliConfig) : IO CliConfig := do
  match cfg.artifactsDir with
  | none => pure cfg
  | some dir =>
      IO.FS.createDirAll dir
      let planOut :=
        if cfg.planOnly then
          match cfg.planOut with
          | some path => some path
          | none => some (dir / "plan.json")
        else
          cfg.planOut
      let manifestOut :=
        match cfg.manifestOut with
        | some path => some path
        | none => some (dir / "manifest.json")
      let jsonOut :=
        if cfg.format == .json then
          match cfg.jsonOut with
          | some path => some path
          | none => some (dir / "results.json")
        else
          cfg.jsonOut
      let jsonlOut :=
        if cfg.format == .jsonl then
          match cfg.jsonlOut with
          | some path => some path
          | none => some (dir / "results.jsonl")
        else
          cfg.jsonlOut
      return {
        cfg with
        planOut := planOut
        manifestOut := manifestOut
        jsonOut := jsonOut
        jsonlOut := jsonlOut
      }

@[inline] def argsFromParsed (p : Cli.Parsed) : Array String := Id.run do
  let mut out : Array String := #[]
  for f in p.flags do
    out := out.push (toString f)
  for a in p.positionalArgs do
    out := out.push a.value
  for a in p.variableArgs do
    out := out.push a.value
  return out

@[inline] def configFromParsed (p : Cli.Parsed) : IO CliConfig := do
  let mut cfg : CliConfig := { args := argsFromParsed p }
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
  match p.flag? "threads" with
  | some f => cfg := { cfg with threads? := some (f.as! Nat) }
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
  match p.flag? "jsonl-out" with
  | some f => cfg := { cfg with jsonlOut := some (System.FilePath.mk (f.as! String)) }
  | none => pure ()
  match p.flag? "artifacts" with
  | some f => cfg := { cfg with artifactsDir := some (System.FilePath.mk (f.as! String)) }
  | none => pure ()
  match p.flag? "compare" with
  | some f => cfg := { cfg with comparePath := some (System.FilePath.mk (f.as! String)) }
  | none => pure ()
  match p.flag? "save" with
  | some f => cfg := { cfg with savePath := some (System.FilePath.mk (f.as! String)) }
  | none => pure ()
  if p.hasFlag "fail-on-regressions" then
    cfg := { cfg with failOnRegressions := true }
  match p.flag? "regress-abs-ns" with
  | some f => cfg := { cfg with regressAbsNs? := some (f.as! Nat) }
  | none => pure ()
  match p.flag? "regress-ratio" with
  | some f =>
      let v ← parseFloatFlag "regress-ratio" (f.as! String)
      if v <= 0.0 then
        throw <| IO.userError "regress-ratio must be > 0"
      cfg := { cfg with regressRatio? := some v }
  | none => pure ()
  match p.flag? "regress-pct" with
  | some f =>
      let v ← parseFloatFlag "regress-pct" (f.as! String)
      if v < 0.0 then
        throw <| IO.userError "regress-pct must be >= 0"
      cfg := { cfg with regressPct? := some v }
  | none => pure ()
  match p.flag? "format" with
  | some f =>
      let fmt := (f.as! String).toLower
      match parseFormat fmt with
      | some out => cfg := { cfg with format := out }
      | none => throw <| IO.userError s!"unknown format: {fmt}"
  | none => pure ()
  let json := p.hasFlag "json"
  let jsonl := p.hasFlag "jsonl"
  let radar := p.hasFlag "radar"
  if json && radar then
    throw <| IO.userError "cannot use --json and --radar together"
  else if json && jsonl then
    throw <| IO.userError "cannot use --json and --jsonl together"
  else if jsonl && radar then
    throw <| IO.userError "cannot use --jsonl and --radar together"
  else if json then
    cfg := { cfg with format := .json }
  else if jsonl then
    cfg := { cfg with format := .jsonl }
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

@[inline] def writeRunResultJsonLines (results : Array BenchResult) (runMetaJson : String)
    (path? : Option System.FilePath) : IO Unit := do
  match path? with
  | none =>
      for r in results do
        IO.println (renderRunResultJsonLine r runMetaJson)
  | some path =>
      IO.FS.withFile path .write (fun h => do
        for r in results do
          h.putStr (renderRunResultJsonLine r runMetaJson)
          h.putStr "\n")

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

@[inline] def buildPlanAndManifest (benches : Array Bench) (cfg : CliConfig) :
    IO (PlanCore × String × RunManifest) := do
  let core := planCoreOf benches cfg
  let coreJson := renderPlanCoreJson core
  let planHash := hashStringHex coreJson
  let manifest ← buildManifest planHash benches.size cfg.partition
  return (core, planHash, manifest)

@[inline] def runWithConfig (cfg : CliConfig) : IO UInt32 := do
  let cfg ← applyArtifacts cfg
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
  if (cfg.failOnRegressions || hasRegressionThresholds cfg) && cfg.comparePath.isNone then
    throw <| IO.userError "regression checks require --compare"
  if cfg.planOnly then
    let (core, _, manifest) ← buildPlanAndManifest benches cfg
    let planJson := renderPlanJson core manifest
    writeOutput planJson cfg.planOut
    if let some path := cfg.manifestOut then
      writeOutput (renderManifestJson manifest) (some path)
    return (0 : UInt32)
  if benches.size == 0 then
    IO.eprintln "no benchmarks registered"
    return (1 : UInt32)
  let needManifest := cfg.manifestOut.isSome || cfg.format == .json || cfg.format == .jsonl
  let mut runMetaJson? : Option String := none
  if needManifest then
    let (_, _, manifest) ← buildPlanAndManifest benches cfg
    if let some path := cfg.manifestOut then
      writeOutput (renderManifestJson manifest) (some path)
    if cfg.format == .json || cfg.format == .jsonl then
      let manifestJson := renderManifestJson manifest
      let manifestHash := hashStringHex manifestJson
      runMetaJson? := some (renderRunMetaJson manifest manifestHash cfg.args)
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
  let regressions :=
    match baseline? with
    | none => #[]
    | some baseline => collectRegressions results baseline cfg
  let exitCode : UInt32 :=
    if cfg.failOnRegressions && !regressions.isEmpty then 1 else 0
  match cfg.format with
  | .pretty =>
      let output := renderPrettyWithBaseline results baseline? false cfg.groupBySuite
      IO.println output
      let summary := renderRegressionSummary regressions cfg
      if summary != "" then
        IO.println summary
  | .prettyFull =>
      let output := renderPrettyWithBaseline results baseline? true cfg.groupBySuite
      IO.println output
      let summary := renderRegressionSummary regressions cfg
      if summary != "" then
        IO.println summary
  | .json =>
      match runMetaJson? with
      | none => throw <| IO.userError "missing run metadata for JSON output"
      | some runMetaJson =>
          let output := renderRunResultsJson results runMetaJson
          writeOutput output cfg.jsonOut
  | .jsonl =>
      match runMetaJson? with
      | none => throw <| IO.userError "missing run metadata for JSONL output"
      | some runMetaJson =>
          writeRunResultJsonLines results runMetaJson cfg.jsonlOut
  | .radar =>
      let radarSuite ← radarSuiteFromEnv cfg
      let lines := renderRadarLines results radarSuite
      let outPath ← radarOutPath cfg
      writeLines lines outPath
  return exitCode

@[inline] def runLeanbenchCmd (p : Cli.Parsed) : IO UInt32 := do
  try
    let cfg ← configFromParsed p
    runWithConfig cfg
  catch e =>
    IO.eprintln s!"{e}"
    return (1 : UInt32)

open Cli

def leanbenchCmd : Cmd := `[Cli|
  leanbench VIA runLeanbenchCmd; [leanbenchVersion] "LeanBench benchmark runner."
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
    threads : Nat; "Run each sample with N parallel action tasks."
    seed : Nat; "Seed for deterministic shuffling."
    shuffle; "Shuffle benchmark order deterministically (use --seed)."
    partition : String; "Partition benches: count:m/n or hash:m/n."
    "plan-out" : String; "Write plan JSON to file."
    "manifest-out" : String; "Write run manifest JSON to file."
    "group-by" : String; "Group output by: suite."
    format : String; "Output format: pretty|full|json|jsonl|radar."
    json; "Alias for --format json."
    jsonl; "Alias for --format jsonl."
    radar; "Alias for --format radar."
    "suite" : String; "Only run benchmarks in this suite."
    "radar-suite" : String; "Prefix radar names with <suite>//."
    "radar-out" : String; "Write radar JSONL to file (defaults to RADAR_JSONL)."
    "json-out" : String; "Write JSON output to file."
    "jsonl-out" : String; "Write JSONL output to file."
    artifacts : String; "Write outputs under this directory."
    compare : String; "Compare against a JSON baseline."
    save : String; "Write JSON output to file and set format=json."
    "fail-on-regressions"; "Exit non-zero if regressions exceed thresholds."
    "regress-abs-ns" : Nat; "Fail if mean_ns regression exceeds N nanoseconds."
    "regress-ratio" : String; "Fail if mean_ns regression ratio exceeds this value."
    "regress-pct" : String; "Fail if mean_ns regression exceeds this percent."
    quiet; "Suppress per-benchmark progress."
]

@[inline] def runMain (args : List String) : IO UInt32 :=
  leanbenchCmd.validate args

end LeanBench
