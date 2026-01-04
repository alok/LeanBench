import Cli
import Lean

open Cli
open Lean

structure ObserveConfig where
  root : String := "."
  out? : Option String := none
  schemaVersion : String := "0.1.0"
  sample : Bool := false

@[inline] def configFromParsed (p : Cli.Parsed) : ObserveConfig :=
  let mut cfg : ObserveConfig := {}
  if p.hasFlag "sample" then
    cfg := { cfg with sample := true }
  match p.flag? "root" with
  | some f => cfg := { cfg with root := f.as! String }
  | none => pure ()
  match p.flag? "out" with
  | some f => cfg := { cfg with out? := some (f.as! String) }
  | none => pure ()
  match p.flag? "schema-version" with
  | some f => cfg := { cfg with schemaVersion := f.as! String }
  | none => pure ()
  cfg

@[inline] def metricsJson (sample : Bool) : Json :=
  if sample then
    Json.mkObj
      [ ("loc", Json.num 1200)
      , ("expr_nodes", Json.num 25000)
      , ("elab_time_ms", Json.num 4200)
      ]
  else
    Json.mkObj
      [ ("loc", Json.num 0)
      , ("expr_nodes", Json.num 0)
      ]

@[inline] def rootNodeJson (cfg : ObserveConfig) : Json :=
  Json.mkObj
    [ ("id", Json.str "root")
    , ("kind", Json.str "module")
    , ("name", Json.str "root")
    , ("path", Json.str cfg.root)
    , ("span", Json.mkObj
        [ ("file", Json.str "")
        , ("line", Json.num 1)
        , ("col", Json.num 1)
        ])
    , ("metrics", metricsJson cfg.sample)
    , ("tags", Json.arr #[])
    , ("children", Json.arr #[])
    ]

@[inline] def artifactJson (cfg : ObserveConfig) (generatedAt : String) : Json :=
  Json.mkObj
    [ ("schema_version", Json.str cfg.schemaVersion)
    , ("generated_at", Json.str generatedAt)
    , ("project", Json.mkObj
        [ ("name", Json.str "LeanProject")
        , ("root", Json.str cfg.root)
        , ("commit", Json.str "")
        , ("tool", Json.str "leanobserve")
        ])
    , ("root", rootNodeJson cfg)
    ]

@[inline] def runLeanObserveCmd (p : Cli.Parsed) : IO UInt32 := do
  try
    let cfg := configFromParsed p
    let now ← IO.now
    let output := artifactJson cfg (toString now)
    match cfg.out? with
    | some path =>
        IO.FS.writeFile (System.FilePath.mk path) (toString output)
    | none =>
        IO.println (toString output)
    return 0
  catch e =>
    IO.eprintln s!"{e}"
    return 1

@[inline] def leanObserveCmd : Cmd := `[Cli|
  leanobserve VIA runLeanObserveCmd; ["0.1.0"] "Lean Observatory metrics exporter (stub)."
  FLAGS:
    root : String; "Project root (default: .)"
    out : String; "Output path (default: stdout)"
    "schema-version" : String; "Schema version (default: 0.1.0)"
    sample; "Emit sample metrics values"
]

def main (args : List String) : IO UInt32 :=
  leanObserveCmd.validate args
