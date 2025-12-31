import Std
import Init.Data.String.Pattern.String
import LeanBench.Bench

namespace LeanBench

inductive OutputFormat where
  | pretty
  | json
  | radar
  deriving Inhabited, BEq

structure CliConfig where
  format : OutputFormat := .pretty
  listOnly : Bool := false
  filter : Option String := none
  samples? : Option Nat := none
  warmup? : Option Nat := none
  minTimeMs? : Option Nat := none
  suite : Option String := none
  radarOut : Option System.FilePath := none
  jsonOut : Option System.FilePath := none
  quiet : Bool := false
  showHelp : Bool := false

structure BenchStats where
  meanNs : Float
  stddevNs : Float
  minNs : Nat
  maxNs : Nat
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
    { meanNs := 0.0, stddevNs := 0.0, minNs := 0, maxNs := 0, totalNs := 0, samples := 0 }
  else
    let total := samples.foldl (fun acc x => acc + x) 0
    let min := samples.foldl (fun acc x => if x < acc then x else acc) samples[0]!
    let max := samples.foldl (fun acc x => if x > acc then x else acc) samples[0]!
    let n := samples.size
    let mean := (Float.ofNat total) / (Float.ofNat n)
    let variance := samples.foldl (fun acc x =>
      let d := (Float.ofNat x) - mean
      acc + d * d) 0.0 / (Float.ofNat n)
    { meanNs := mean, stddevNs := Float.sqrt variance, minNs := min, maxNs := max, totalNs := total, samples := n }

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

@[inline] def shouldKeep (entry : Bench) (cli : CliConfig) : Bool :=
  match cli.filter with
  | none => true
  | some pat => containsSubstr entry.name pat

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

@[inline] def renderPretty (results : Array BenchResult) : String := Id.run do
  let header := #["benchmark", "mean", "min", "max", "stddev", "samples"]
  let rows := results.map fun r =>
    #[r.entry.name,
      formatNsFloat r.stats.meanNs,
      formatNs r.stats.minNs,
      formatNs r.stats.maxNs,
      formatNsFloat r.stats.stddevNs,
      toString r.stats.samples]
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

@[inline] def jsonEscape (s : String) : String :=
  s.foldl (fun acc c =>
    match c with
    | '"' => acc ++ "\\\""
    | '\\' => acc ++ "\\\\"
    | '\n' => acc ++ "\\n"
    | '\r' => acc ++ "\\r"
    | '\t' => acc ++ "\\t"
    | _ => acc.push c) ""

@[inline] def renderJson (results : Array BenchResult) : String :=
  let items := results.toList.map fun r =>
    let name := jsonEscape r.entry.name
    "{" ++
      "\"name\":\"" ++ name ++ "\"," ++
      "\"mean_ns\":" ++ r.stats.meanNs.toString ++ "," ++
      "\"stddev_ns\":" ++ r.stats.stddevNs.toString ++ "," ++
      "\"min_ns\":" ++ toString r.stats.minNs ++ "," ++
      "\"max_ns\":" ++ toString r.stats.maxNs ++ "," ++
      "\"samples\":" ++ toString r.stats.samples ++ "," ++
      "\"total_ns\":" ++ toString r.stats.totalNs ++
    "}"
  "[" ++ String.intercalate "," items ++ "]"

@[inline] def radarName (benchName : String) (suite : Option String) : String :=
  match suite with
  | none => benchName
  | some s => s ++ "//" ++ benchName

@[inline] def renderRadarLines (results : Array BenchResult) (suite : Option String) : Array String :=
  results.map fun r =>
    let name := jsonEscape (radarName r.entry.name suite)
    "{" ++
      "\"name\":\"" ++ name ++ "\"," ++
      "\"unit\":\"ns\"," ++
      "\"value\":" ++ r.stats.meanNs.toString ++
    "}"

@[inline] def usage : String :=
  String.intercalate "\n" [
    "LeanBench: simple benchmarking runner for Lean",
    "",
    "Usage:",
    "  leanbench [options]",
    "",
    "Options:",
    "  --list                 List available benchmarks",
    "  --match <substr>       Only run benchmarks whose name contains <substr>",
    "  --samples <n>           Override sample count",
    "  --warmup <n>            Override warmup count",
    "  --min-time-ms <n>       Run until total time reaches <n> ms",
    "  --format <pretty|json|radar>",
    "  --json                 Alias for --format json",
    "  --radar                Alias for --format radar",
    "  --suite <name>          Prefix radar names with <name>//...",
    "  --radar-out <path>      Write radar JSONL to file (defaults to RADAR_JSONL env)",
    "  --json-out <path>       Write JSON output to file",
    "  --quiet                Suppress per-benchmark progress",
    "  --help                 Show this help"
  ]

@[inline] def parseNat (s : String) : IO Nat :=
  match s.toNat? with
  | some n => pure n
  | none => throw <| IO.userError s!"invalid Nat: {s}"

@[inline] def parseArgs (args : List String) : IO CliConfig :=
  let rec loop (cfg : CliConfig) (args : List String) : IO CliConfig :=
    match args with
    | [] => pure cfg
    | "--help" :: rest => loop { cfg with showHelp := true } rest
    | "--list" :: rest => loop { cfg with listOnly := true } rest
    | "--json" :: rest => loop { cfg with format := .json } rest
    | "--radar" :: rest => loop { cfg with format := .radar } rest
    | "--quiet" :: rest => loop { cfg with quiet := true } rest
    | "--format" :: fmt :: rest => do
        let fmt := fmt.toLower
        let out :=
          if fmt == "pretty" then .pretty
          else if fmt == "json" then .json
          else if fmt == "radar" then .radar
          else cfg.format
        let known :=
          if fmt == "pretty" then true
          else if fmt == "json" then true
          else if fmt == "radar" then true
          else false
        if out == cfg.format && !known then
          throw <| IO.userError s!"unknown format: {fmt}"
        loop { cfg with format := out } rest
    | "--match" :: pat :: rest => loop { cfg with filter := some pat } rest
    | "--samples" :: n :: rest => do
        let n ← parseNat n
        loop { cfg with samples? := some n } rest
    | "--warmup" :: n :: rest => do
        let n ← parseNat n
        loop { cfg with warmup? := some n } rest
    | "--min-time-ms" :: n :: rest => do
        let n ← parseNat n
        loop { cfg with minTimeMs? := some n } rest
    | "--suite" :: s :: rest => loop { cfg with suite := some s } rest
    | "--radar-out" :: p :: rest => loop { cfg with radarOut := some (System.FilePath.mk p) } rest
    | "--json-out" :: p :: rest => loop { cfg with jsonOut := some (System.FilePath.mk p) } rest
    | flag :: _ => throw <| IO.userError s!"unknown argument: {flag}"
  loop {} args

@[inline] def suiteFromEnv (cli : CliConfig) : IO (Option String) := do
  if cli.suite.isSome then
    pure cli.suite
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

@[inline] def runMain (args : List String) : IO Unit := do
  let cfg ← parseArgs args
  if cfg.showHelp then
    IO.println usage
    return
  let benches ← listBenches
  let benches := benches.filter (fun b => shouldKeep b cfg)
  if cfg.listOnly then
    for b in benches do
      IO.println b.name
    return
  if benches.size == 0 then
    IO.eprintln "no benchmarks registered"
    return
  let mut results : Array BenchResult := #[]
  for b in benches do
    unless cfg.quiet do
      let cfg' := applyOverrides b.config cfg
      IO.eprintln s!"bench {b.name} (warmup {cfg'.warmup}, samples {cfg'.samples})"
    let r ← runBench b cfg
    results := results.push r
  match cfg.format with
  | .pretty =>
      let output := renderPretty results
      IO.println output
  | .json =>
      let output := renderJson results
      match cfg.jsonOut with
      | some path => IO.FS.writeFile path output
      | none => IO.println output
  | .radar =>
      let suite ← suiteFromEnv cfg
      let lines := renderRadarLines results suite
      let outPath ← radarOutPath cfg
      writeLines lines outPath

end LeanBench
