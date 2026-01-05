import LeanBench.Bench
import Std

open LeanBench

@[inline] def observeArgs (extra : Array String) : Array String :=
  #["--root", "benchdata"] ++ extra

@[inline] def runCmd (cmd : String) (args : Array String) : IO Unit := do
  let child ← IO.Process.spawn { cmd := cmd, args := args, stdout := .null, stderr := .null }
  let code ← child.wait
  if code != 0 then
    throw <| IO.userError s!"{cmd} exited with {code}"

@[inline] def runLeanObserve (extra : Array String) : IO Unit := do
  let args := observeArgs extra
  let bin := System.FilePath.mk ".lake/build/bin/leanobserve"
  let hasBin ← bin.pathExists
  if hasBin then
    runCmd bin.toString args
  else
    let lakeArgs := #["exe", "leanobserve", "--"] ++ args
    runCmd "lake" lakeArgs

@[inline] def mkObserveBenchCfg (tags : List String) (samples : Nat) : BenchConfig := {
  suite := some "observe"
  tags := ["observe"] ++ tags
  warmup := 0
  samples := samples
}

bench "observe/textscan_small" (mkObserveBenchCfg ["textscan"] 5) do
  runLeanObserve #[]

bench "observe/infotree_small" (mkObserveBenchCfg ["infotree"] 2) do
  runLeanObserve #["--infotree"]

bench "observe/infotree_cmdnodes_small" (mkObserveBenchCfg ["infotree", "command-nodes"] 2) do
  runLeanObserve #["--infotree", "--command-nodes"]
