import LeanBench.Bench
import LeanObserve

open LeanBench

@[inline] def observeRoot : String := "benchdata"

@[inline] def mkObserveCfg (infoTree : Bool) (commandNodes : Bool) (declOnly : Bool) : ObserveConfig := {
  root := observeRoot
  out? := none
  schemaVersion := "0.1.0"
  sample := false
  maxFiles? := some 2
  buildLog? := none
  profileJson? := none
  infoTree := infoTree
  infoTreeJobs := 1
  commandNodes := commandNodes
  declNodesOnly := declOnly
  reportJson? := none
  reportMd? := none
  reportTop := 30
}

@[inline] def mkObserveBenchCfg (tags : List String) (samples : Nat) : BenchConfig := {
  suite := some "observe"
  tags := ["observe"] ++ tags
  warmup := 0
  samples := samples
}

bench "observe/textscan_small" (mkObserveBenchCfg ["textscan"] 5) do
  let cfg := mkObserveCfg false false false
  let _ ← artifactJson cfg "bench"
  pure ()

bench "observe/infotree_small" (mkObserveBenchCfg ["infotree"] 2) do
  let cfg := mkObserveCfg true false false
  let _ ← artifactJson cfg "bench"
  pure ()

bench "observe/infotree_cmdnodes_small" (mkObserveBenchCfg ["infotree", "command-nodes"] 2) do
  let cfg := mkObserveCfg true true false
  let _ ← artifactJson cfg "bench"
  pure ()

bench "observe/decl_nodes_small" (mkObserveBenchCfg ["infotree", "decl-nodes"] 2) do
  let cfg := mkObserveCfg true true true
  let _ ← artifactJson cfg "bench"
  pure ()
