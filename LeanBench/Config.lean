import Std
import Lean
import Lean.Parser
import Lake.Toml
import Lake.Util.Message
import LeanBench.Bench
import LeanBench.Filter

open Lean

namespace LeanBench

structure BenchConfigPatch where
  warmup? : Option Nat := none
  samples? : Option Nat := none
  minTimeMs? : Option Nat := none
  threads? : Option Nat := none
  timeoutMs? : Option Nat := none
  retries? : Option Nat := none
  priority? : Option Nat := none
  group? : Option String := none
  threadsRequired? : Option Nat := none
  deriving Inhabited

@[inline] def tomlKey (s : String) : Name :=
  Name.mkSimple s

@[inline] def mergeOpt (a b : Option α) : Option α :=
  match a with
  | some _ => a
  | none => b

def BenchConfigPatch.merge (base overlay : BenchConfigPatch) : BenchConfigPatch :=
  { warmup? := mergeOpt overlay.warmup? base.warmup?
    samples? := mergeOpt overlay.samples? base.samples?
    minTimeMs? := mergeOpt overlay.minTimeMs? base.minTimeMs?
    threads? := mergeOpt overlay.threads? base.threads?
    timeoutMs? := mergeOpt overlay.timeoutMs? base.timeoutMs?
    retries? := mergeOpt overlay.retries? base.retries?
    priority? := mergeOpt overlay.priority? base.priority?
    group? := mergeOpt overlay.group? base.group?
    threadsRequired? := mergeOpt overlay.threadsRequired? base.threadsRequired?
  }

def BenchConfig.applyPatch (cfg : BenchConfig) (patch : BenchConfigPatch) : BenchConfig :=
  { cfg with
    warmup := patch.warmup?.getD cfg.warmup
    samples := patch.samples?.getD cfg.samples
    minTimeMs := patch.minTimeMs?.getD cfg.minTimeMs
    threads := patch.threads?.getD cfg.threads
    timeoutMs := mergeOpt patch.timeoutMs? cfg.timeoutMs
    retries := patch.retries?.getD cfg.retries
    priority := patch.priority?.getD cfg.priority
    group := mergeOpt patch.group? cfg.group
    threadsRequired := patch.threadsRequired?.getD cfg.threadsRequired
  }

structure BenchOverride where
  whereExpr : FilterExpr
  patch : BenchConfigPatch := default
  deriving Inhabited

structure BenchProfile where
  inherits? : Option Name := none
  select? : Option FilterExpr := none
  config : BenchConfigPatch := default
  overrides : Array BenchOverride := #[]
  deriving Inhabited

structure BenchConfigFile where
  profiles : Std.HashMap Name BenchProfile := {}
  deriving Inhabited

def decodePatchFromTable (t : Lake.Toml.Table) : Lake.Toml.DecodeM BenchConfigPatch := do
  let warmup? ← t.tryDecode? (tomlKey "warmup")
  let samples? ← t.tryDecode? (tomlKey "samples")
  let minTimeMs? ← t.tryDecode? (tomlKey "min-time-ms")
  let threads? ← t.tryDecode? (tomlKey "threads")
  let timeoutMs? ← t.tryDecode? (tomlKey "timeout-ms")
  let retries? ← t.tryDecode? (tomlKey "retries")
  let priority? ← t.tryDecode? (tomlKey "priority")
  let group? ← t.tryDecode? (tomlKey "group")
  let threadsRequired? ← t.tryDecode? (tomlKey "threads-required")
  return {
    warmup? := warmup?
    samples? := samples?
    minTimeMs? := minTimeMs?
    threads? := threads?
    timeoutMs? := timeoutMs?
    retries? := retries?
    priority? := priority?
    group? := group?
    threadsRequired? := threadsRequired?
  }

def decodeOverrideFromTable (ref : Syntax) (t : Lake.Toml.Table) : Lake.Toml.DecodeM BenchOverride := do
  let whereVal? :=
    match t.find? (tomlKey "where") with
    | some v => some v
    | none => t.find? (tomlKey "filter")
  let whereRaw ←
    match whereVal? with
    | some (.string _ s) => pure s
    | some v =>
        modify fun es => es.push { ref := v.ref, msg := "expected 'where' to be a string" }
        pure ""
    | none =>
        modify fun es => es.push { ref := ref, msg := "override missing required 'where' string" }
        pure ""
  let whereExpr ←
    match parseFilterExpr whereRaw with
    | .ok e => pure e
    | .error err =>
        modify fun es => es.push { ref := ref, msg := s!"invalid override where: {err}" }
        -- Dummy expression (ignored if config load fails due to errors).
        pure (FilterExpr.tag "__invalid__")
  let patch ← decodePatchFromTable t
  return { whereExpr := whereExpr, patch := patch }

def decodeOverrides (t : Lake.Toml.Table) : Lake.Toml.DecodeM (Array BenchOverride) := do
  let mut out : Array BenchOverride := #[]
  let key := tomlKey "overrides"
  match t.find? key with
  | none => return out
  | some (.array _ xs) =>
      for v in xs do
        match v with
        | .table ref table =>
            out := out.push (← decodeOverrideFromTable ref table)
        | _ =>
            modify fun es => es.push {
              ref := v.ref
              msg := "expected override entry to be a table"
            }
      return out
  | some v =>
      modify fun es => es.push {
        ref := v.ref
        msg := "expected 'overrides' to be an array of tables"
      }
      return out

def decodeProfileFromTable (ref : Syntax) (t : Lake.Toml.Table) : Lake.Toml.DecodeM BenchProfile := do
  let inherits? ← t.tryDecode? (tomlKey "inherits")
  let selectRaw? : Option String ← t.tryDecode? (tomlKey "select")
  let select? ←
    match selectRaw? with
    | none => pure none
    | some s =>
        match parseFilterExpr s with
        | .ok e => pure (some e)
        | .error err =>
            modify fun es => es.push { ref := ref, msg := s!"invalid select filter: {err}" }
            pure none
  let cfg ← decodePatchFromTable t
  let overrides ← decodeOverrides t
  return {
    inherits? := inherits?.map Name.mkSimple
    select? := select?
    config := cfg
    overrides := overrides
  }

def decodeProfiles (t : Lake.Toml.Table) : Lake.Toml.DecodeM (Std.HashMap Name BenchProfile) := do
  let mut profiles : Std.HashMap Name BenchProfile := {}
  for (k, v) in t.items do
    match v with
    | .table ref table =>
        profiles := profiles.insert k (← decodeProfileFromTable ref table)
    | _ =>
        modify fun es => es.push {
          ref := v.ref
          msg := s!"expected table for profile {Lake.Toml.ppKey k}"
        }
  return profiles

def formatDecodeErrors (ictx : Parser.InputContext) (errs : Array Lake.Toml.DecodeError) : IO String := do
  errs.foldlM (init := "") fun acc e => do
    let pos := ictx.fileMap.toPosition (e.ref.getPos?.getD 0)
    let msg := Lean.mkErrorStringWithPos ictx.fileName pos e.msg (kind := some "error")
    return acc ++ msg ++ "\n"

def loadConfigFile (path : System.FilePath) : IO BenchConfigFile := do
  let input ← IO.FS.readFile path
  let ictx := Parser.mkInputContext input path.toString
  let table ← match (← Lake.Toml.loadToml ictx |>.toBaseIO) with
    | .ok table => pure table
    | .error log =>
        let msg ← Lake.mkMessageLogString log
        throw <| IO.userError msg
  let .ok cfg errs := EStateM.run (s := #[]) do
    let profiles? ← table.tryDecode? (tomlKey "profile")
    let profileMap ←
      match profiles? with
      | some profiles => decodeProfiles profiles
      | none => pure {}
    return { profiles := profileMap }
  if !errs.isEmpty then
    let msg ← formatDecodeErrors ictx errs
    throw <| IO.userError msg
  return cfg

partial def resolveProfile
    (profiles : Std.HashMap Name BenchProfile)
    (name : Name)
    (stack : List Name := []) : Except String BenchProfile := do
  if stack.contains name then
    throw s!"profile inheritance cycle involving '{name}'"
  match profiles.get? name with
  | none =>
      if profiles.isEmpty && name == `default then
        return default
      else
        throw s!"unknown profile '{name}'"
  | some profile =>
      match profile.inherits? with
      | none =>
          return { profile with inherits? := none }
      | some parent =>
          let base ← resolveProfile profiles parent (name :: stack)
          let mergedConfig := base.config.merge profile.config
          let select? := mergeOpt profile.select? base.select?
          let overrides := base.overrides ++ profile.overrides
          return {
            inherits? := none
            select? := select?
            config := mergedConfig
            overrides := overrides
          }

end LeanBench
