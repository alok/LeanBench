import Lean
import Lean.Data.Json
import Std

namespace LeanBench

structure BenchConfig where
  warmup : Nat := 2
  samples : Nat := 20
  minTimeMs : Nat := 0
  threads : Nat := 1
  suite : Option String := none
  tags : List String := []
  bytes : Option Nat := none
  flops : Option Nat := none
  items : Option Nat := none
  deriving Inhabited

/-- Context for per-sample reporting hooks. -/
structure SampleCtx where
  benchName : String
  benchId : String
  suite : Option String
  tags : List String
  config : BenchConfig
  sampleIndex : Nat
  elapsedNs : Nat
  threads : Nat

structure Bench where
  name : String
  action : IO Unit
  config : BenchConfig := {}
  setup? : Option (IO Unit) := none
  teardown? : Option (IO Unit) := none
  beforeEach? : Option (IO Unit) := none
  afterEach? : Option (IO Unit) := none
  report? : Option (IO Lean.Json) := none
  reportSample? : Option (SampleCtx → IO Lean.Json) := none
  trace? : Option (IO Lean.Json) := none
  deriving Inhabited

initialize benchRegistry : IO.Ref (Array Bench) <- IO.mkRef #[]

@[inline] def register (bench : Bench) : IO Unit :=
  benchRegistry.modify (·.push bench)

@[inline] def listBenches : IO (Array Bench) :=
  benchRegistry.get

structure SuiteConfig where
  description : String := ""
  tags : List String := []
  benches : List String := []
  excludeTags : List String := []
  excludeBenches : List String := []
  deriving Inhabited

structure Suite where
  name : String
  description : String := ""
  filter : Bench → Bool

initialize suiteRegistry : IO.Ref (Array Suite) <- IO.mkRef #[]

@[inline] def registerSuite (suite : Suite) : IO Unit :=
  suiteRegistry.modify (·.push suite)

@[inline] def listSuites : IO (Array Suite) :=
  suiteRegistry.get

@[inline] def findSuite? (name : String) : IO (Option Suite) := do
  return (← listSuites).find? (·.name == name)

@[inline] def suiteFromConfig (name : String) (cfg : SuiteConfig) : Suite :=
  let nameMatch (bench : Bench) : Bool :=
    if cfg.benches.isEmpty then true else cfg.benches.contains bench.name
  let tagMatch (bench : Bench) : Bool :=
    if cfg.tags.isEmpty then true else cfg.tags.any (bench.config.tags.contains ·)
  let suiteMatch (bench : Bench) : Bool :=
    if cfg.tags.isEmpty && cfg.benches.isEmpty then
      bench.config.suite == some name
    else
      true
  let excluded (bench : Bench) : Bool :=
    cfg.excludeBenches.contains bench.name ||
      cfg.excludeTags.any (bench.config.tags.contains ·)
  let filter (bench : Bench) : Bool :=
    suiteMatch bench && nameMatch bench && tagMatch bench && !excluded bench
  { name := name, description := cfg.description, filter := filter }

/--
Register a benchmark that runs an external command. Stdout/stderr are suppressed.
Throws if the command exits non-zero.
-/
@[inline] def benchCmd (cmd : String) (args : Array String := #[])
    (cwd : Option System.FilePath := none) : IO Unit := do
  let child ← IO.Process.spawn { cmd := cmd, args := args, cwd := cwd, stdout := .null, stderr := .null }
  let code ← child.wait
  if code != 0 then
    throw <| IO.userError s!"benchCmd: `{cmd}` exited with {code}"

end LeanBench

macro "bench " name:str " do " seq:doSeq : command =>
  `(initialize (LeanBench.register
    { name := $name, action := (do $seq), config := (default : LeanBench.BenchConfig) }))

macro "bench " name:str " (" cfg:term ")" " do " seq:doSeq : command =>
  `(initialize (LeanBench.register
    { name := $name, action := (do $seq), config := $cfg }))

macro "bench_report " name:str " (" report:term ")" " do " seq:doSeq : command =>
  `(initialize (LeanBench.register
    { name := $name, action := (do $seq), report? := some $report, config := (default : LeanBench.BenchConfig) }))

macro "bench_report " name:str " (" cfg:term ")" " (" report:term ")" " do " seq:doSeq : command =>
  `(initialize (LeanBench.register
    { name := $name, action := (do $seq), report? := some $report, config := $cfg }))

macro "bench_suite " name:str : command =>
  `(initialize (LeanBench.registerSuite
    (LeanBench.suiteFromConfig $name {})))

macro "bench_suite " name:str " (" cfg:term ")" : command =>
  `(initialize (LeanBench.registerSuite
    (LeanBench.suiteFromConfig $name $cfg)))
