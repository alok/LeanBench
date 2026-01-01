import Lean
import Std

namespace LeanBench

structure BenchConfig where
  warmup : Nat := 2
  samples : Nat := 20
  minTimeMs : Nat := 0
  suite : Option String := none
  tags : List String := []
  deriving Inhabited

structure Bench where
  name : String
  action : IO Unit
  config : BenchConfig := {}

initialize benchRegistry : IO.Ref (Array Bench) <- IO.mkRef #[]

@[inline] def register (bench : Bench) : IO Unit :=
  benchRegistry.modify (·.push bench)

@[inline] def listBenches : IO (Array Bench) :=
  benchRegistry.get

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
