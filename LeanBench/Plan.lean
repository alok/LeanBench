import Std
import LeanBench.Bench
import LeanBench.Json

namespace LeanBench

inductive PartitionKind where
  | count
  | hash
  deriving Inhabited, BEq

structure PartitionSpec where
  kind : PartitionKind
  index : Nat
  count : Nat
  deriving Inhabited

structure BenchPlanEntry where
  id : String
  name : String
  suite : Option String
  tags : List String
  config : BenchConfig
  deriving Inhabited

structure PlanCore where
  version : Nat := 1
  seed : Nat := 0
  shuffled : Bool := false
  partition : Option PartitionSpec := none
  entries : Array BenchPlanEntry := #[]
  deriving Inhabited

structure RunManifest where
  planHash : String
  benchCount : Nat
  os : String
  arch : Option String := none
  leanVersion : String
  lakeVersion : Option String := none
  gitSha : Option String := none
  generatedEpochSec : Option String := none
  partition : Option PartitionSpec := none
  deriving Inhabited

@[inline] def benchId (b : Bench) : String :=
  match b.config.suite with
  | none => b.name
  | some s => s ++ "/" ++ b.name

@[inline] def entryOf (b : Bench) (cfg : BenchConfig) : BenchPlanEntry :=
  { id := benchId b
    name := b.name
    suite := b.config.suite
    tags := b.config.tags
    config := cfg }

@[inline] def parsePartitionSpec (s : String) : Except String PartitionSpec := do
  let parts := s.splitOn ":"
  if parts.length != 2 then
    throw s!"invalid partition '{s}', expected kind:idx/count"
  let kindStr := parts[0]!
  let rest := parts[1]!
  let kind ←
    if kindStr == "count" then
      pure PartitionKind.count
    else if kindStr == "hash" then
      pure PartitionKind.hash
    else
      throw s!"unknown partition kind '{kindStr}'"
  let frac := rest.splitOn "/"
  if frac.length != 2 then
    throw s!"invalid partition '{s}', expected idx/count"
  let some idx := frac[0]!.toNat? | throw s!"invalid partition index '{frac[0]!}'"
  let some count := frac[1]!.toNat? | throw s!"invalid partition count '{frac[1]!}'"
  if count == 0 then
    throw "partition count must be > 0"
  if idx == 0 || idx > count then
    throw s!"partition index must be in 1..{count}"
  return { kind := kind, index := idx, count := count }

@[inline] def partitionToString (p : PartitionSpec) : String :=
  let kind :=
    match p.kind with
    | .count => "count"
    | .hash => "hash"
  s!"{kind}:{p.index}/{p.count}"

@[inline] def fnv1a64 (bytes : ByteArray) : UInt64 := Id.run do
  let offset : UInt64 := 14695981039346656037
  let prime : UInt64 := 1099511628211
  let mut h := offset
  for b in bytes do
    h := (h.xor (UInt64.ofNat b.toNat)) * prime
  return h

@[inline] def hexChar (n : Nat) : Char :=
  if n < 10 then
    Char.ofNat (n + 48)
  else
    Char.ofNat (n - 10 + 97)

@[inline] def hexUInt64 (n : UInt64) : String := Id.run do
  let mut v := n
  let mut chars : Array Char := #[]
  for _ in [0:16] do
    let digit := (v.land (0xF : UInt64)).toNat
    chars := chars.push (hexChar digit)
    v := v.shiftRight 4
  return String.ofList chars.toList.reverse

@[inline] def hashStringHex (s : String) : String :=
  hexUInt64 (fnv1a64 s.toUTF8)

@[inline] def partitionBenches (benches : Array Bench) (p : PartitionSpec) : Array Bench :=
  let idx0 := p.index - 1
  let n := p.count
  Id.run do
    let mut out : Array Bench := #[]
    for idx in [0:benches.size] do
      let b := benches[idx]!
      let keep :=
        match p.kind with
        | .count => idx % n == idx0
        | .hash =>
            let h := fnv1a64 (benchId b).toUTF8
            (h.toNat % n) == idx0
      match keep with
      | true => out := out.push b
      | false => pure ()
    return out

structure Lcg where
  state : UInt64

@[inline] def Lcg.next (g : Lcg) : UInt64 × Lcg :=
  let a : UInt64 := 6364136223846793005
  let c : UInt64 := 1442695040888963407
  let next := a * g.state + c
  (next, { state := next })

@[inline] def shuffleBenches (benches : Array Bench) (seed : Nat) : Array Bench := Id.run do
  if benches.size <= 1 then
    return benches
  let mut arr := benches
  let mut rng : Lcg := { state := UInt64.ofNat seed }
  for i in [0:arr.size] do
    let remaining := arr.size - i
    if remaining == 0 then
      continue
    let (r, rng') := rng.next
    rng := rng'
    let j := i + (r.toNat % remaining)
    let ai := arr[i]!
    let aj := arr[j]!
    arr := arr.set! i aj
    arr := arr.set! j ai
  return arr

@[inline] def renderBenchConfigJson (cfg : BenchConfig) : String :=
  let bytes := match cfg.bytes with | none => "null" | some v => toString v
  let flops := match cfg.flops with | none => "null" | some v => toString v
  let items := match cfg.items with | none => "null" | some v => toString v
  let timeoutMs := match cfg.timeoutMs with | none => "null" | some v => toString v
  let group := match cfg.group with | none => "null" | some s => jsonString s
  "{" ++
    "\"warmup\":" ++ toString cfg.warmup ++ "," ++
    "\"samples\":" ++ toString cfg.samples ++ "," ++
    "\"min_time_ms\":" ++ toString cfg.minTimeMs ++ "," ++
    "\"threads\":" ++ toString cfg.threads ++ "," ++
    "\"timeout_ms\":" ++ timeoutMs ++ "," ++
    "\"retries\":" ++ toString cfg.retries ++ "," ++
    "\"priority\":" ++ toString cfg.priority ++ "," ++
    "\"group\":" ++ group ++ "," ++
    "\"threads_required\":" ++ toString cfg.threadsRequired ++ "," ++
    "\"bytes\":" ++ bytes ++ "," ++
    "\"flops\":" ++ flops ++ "," ++
    "\"items\":" ++ items ++
  "}"

@[inline] def renderTagsJson (tags : List String) : String :=
  jsonArray (tags.toArray.map jsonString)

@[inline] def renderPlanEntryJson (e : BenchPlanEntry) : String :=
  let suite := match e.suite with
    | none => "null"
    | some s => jsonString s
  "{" ++
    "\"id\":" ++ jsonString e.id ++ "," ++
    "\"name\":" ++ jsonString e.name ++ "," ++
    "\"suite\":" ++ suite ++ "," ++
    "\"tags\":" ++ renderTagsJson e.tags ++ "," ++
    "\"config\":" ++ renderBenchConfigJson e.config ++
  "}"

@[inline] def renderPartitionJson (p : PartitionSpec) : String :=
  "{" ++
    "\"kind\":" ++ jsonString (match p.kind with | .count => "count" | .hash => "hash") ++ "," ++
    "\"index\":" ++ toString p.index ++ "," ++
    "\"count\":" ++ toString p.count ++
  "}"

@[inline] def renderPlanCoreJson (core : PlanCore) : String :=
  let entries := jsonArray (core.entries.map renderPlanEntryJson)
  let partition :=
    match core.partition with
    | none => "null"
    | some p => renderPartitionJson p
  "{" ++
    "\"version\":" ++ toString core.version ++ "," ++
    "\"seed\":" ++ toString core.seed ++ "," ++
    "\"shuffled\":" ++ (if core.shuffled then "true" else "false") ++ "," ++
    "\"partition\":" ++ partition ++ "," ++
    "\"entries\":" ++ entries ++
  "}"

@[inline] def renderManifestJson (m : RunManifest) : String :=
  let arch := match m.arch with | none => "null" | some s => jsonString s
  let lake := match m.lakeVersion with | none => "null" | some s => jsonString s
  let git := match m.gitSha with | none => "null" | some s => jsonString s
  let ts := match m.generatedEpochSec with | none => "null" | some s => jsonString s
  let partition := match m.partition with | none => "null" | some p => renderPartitionJson p
  "{" ++
    "\"plan_hash\":" ++ jsonString m.planHash ++ "," ++
    "\"bench_count\":" ++ toString m.benchCount ++ "," ++
    "\"os\":" ++ jsonString m.os ++ "," ++
    "\"arch\":" ++ arch ++ "," ++
    "\"lean_version\":" ++ jsonString m.leanVersion ++ "," ++
    "\"lake_version\":" ++ lake ++ "," ++
    "\"git_sha\":" ++ git ++ "," ++
    "\"generated_epoch_sec\":" ++ ts ++ "," ++
    "\"partition\":" ++ partition ++
  "}"

@[inline] def renderPlanJson (core : PlanCore) (manifest : RunManifest) : String :=
  let planHash := jsonString manifest.planHash
  "{" ++
    "\"plan_hash\":" ++ planHash ++ "," ++
    "\"core\":" ++ renderPlanCoreJson core ++ "," ++
    "\"manifest\":" ++ renderManifestJson manifest ++
  "}"

@[inline] def renderBenchListJson (entries : Array BenchPlanEntry) : String :=
  jsonArray (entries.map renderPlanEntryJson)

@[inline] def cmdOutput? (cmd : String) (args : Array String := #[]) : IO (Option String) := do
  try
    let out ← IO.Process.output { cmd := cmd, args := args }
    if out.exitCode == 0 then
      return some out.stdout.trimAscii.toString
    else
      return none
  catch _ =>
    return none

@[inline] def detectOs : String :=
  if System.Platform.isWindows then "windows"
  else if System.Platform.isOSX then "macos"
  else "linux"

@[inline] def detectArch : IO (Option String) := do
  if System.Platform.isWindows then
    return (← IO.getEnv "PROCESSOR_ARCHITECTURE")
  else
    cmdOutput? "uname" #["-m"]

@[inline] def detectGitSha : IO (Option String) :=
  cmdOutput? "git" #["rev-parse", "HEAD"]

@[inline] def detectLakeVersion : IO (Option String) :=
  cmdOutput? "lake" #["--version"]

@[inline] def detectEpochSec : IO (Option String) :=
  cmdOutput? "date" #["-u", "+%s"]

@[inline] def buildManifest (planHash : String) (benchCount : Nat) (partition : Option PartitionSpec) : IO RunManifest := do
  let arch ← detectArch
  let lake ← detectLakeVersion
  let git ← detectGitSha
  let ts ← detectEpochSec
  return {
    planHash := planHash
    benchCount := benchCount
    os := detectOs
    arch := arch
    leanVersion := Lean.versionString
    lakeVersion := lake
    gitSha := git
    generatedEpochSec := ts
    partition := partition
  }

end LeanBench
