import Cli
import Lean

open Cli
open Lean

structure ObserveConfig where
  root : String := "."
  out? : Option String := none
  schemaVersion : String := "0.1.0"
  sample : Bool := false
  maxFiles? : Option Nat := none

@[inline] def configFromParsed (p : Cli.Parsed) : ObserveConfig :=
  Id.run do
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
    match p.flag? "max-files" with
    | some f => cfg := { cfg with maxFiles? := some (f.as! Nat) }
    | none => pure ()
    return cfg

structure FileMetrics where
  loc : Nat := 0
  locNonEmpty : Nat := 0
  commentLines : Nat := 0
  todo : Nat := 0
  fixme : Nat := 0
  portingNote : Nat := 0
  adaptationNote : Nat := 0
  nolint : Nat := 0

@[inline] def FileMetrics.add (a b : FileMetrics) : FileMetrics :=
  { loc := a.loc + b.loc
  , locNonEmpty := a.locNonEmpty + b.locNonEmpty
  , commentLines := a.commentLines + b.commentLines
  , todo := a.todo + b.todo
  , fixme := a.fixme + b.fixme
  , portingNote := a.portingNote + b.portingNote
  , adaptationNote := a.adaptationNote + b.adaptationNote
  , nolint := a.nolint + b.nolint
  }

@[inline] def isPrefixChars (pre s : List Char) : Bool :=
  match pre, s with
  | [], _ => true
  | _, [] => false
  | p :: ps, c :: cs => p == c && isPrefixChars ps cs

@[inline] def containsSubstr (s sub : String) : Bool :=
  let sChars := s.toList
  let subChars := sub.toList
  if subChars.isEmpty then
    true
  else
    let rec loop (xs : List Char) : Bool :=
      match xs with
      | [] => false
      | _ :: xs' =>
          if isPrefixChars subChars xs then true else loop xs'
    loop sChars

@[inline] def lineHas (lowerLine : String) (needle : String) : Bool :=
  containsSubstr lowerLine needle

@[inline] def lineMetrics (line : String) (inBlock : Bool) (acc : FileMetrics) : FileMetrics × Bool :=
  let acc := { acc with loc := acc.loc + 1 }
  let trimmed := line.trimAscii
  let acc := if trimmed.isEmpty then acc else { acc with locNonEmpty := acc.locNonEmpty + 1 }
  let lower := line.toLower
  let acc := if lineHas lower "todo" then { acc with todo := acc.todo + 1 } else acc
  let acc := if lineHas lower "fixme" then { acc with fixme := acc.fixme + 1 } else acc
  let acc := if lineHas lower "porting note" then { acc with portingNote := acc.portingNote + 1 } else acc
  let acc := if lineHas lower "#adaptation_note" then { acc with adaptationNote := acc.adaptationNote + 1 } else acc
  let acc := if lineHas lower "nolint" then { acc with nolint := acc.nolint + 1 } else acc
  let hasBlockStart := lineHas line "/-"
  let hasBlockEnd := lineHas line "-/"
  if inBlock then
    let acc := { acc with commentLines := acc.commentLines + 1 }
    let inBlock := if hasBlockEnd then false else true
    (acc, inBlock)
  else if trimmed.startsWith "--" then
    ({ acc with commentLines := acc.commentLines + 1 }, false)
  else if hasBlockStart then
    let acc := { acc with commentLines := acc.commentLines + 1 }
    let inBlock := if hasBlockEnd then false else true
    (acc, inBlock)
  else
    (acc, false)

@[inline] def scanLines (lines : List String) : FileMetrics :=
  let rec go (lines : List String) (inBlock : Bool) (acc : FileMetrics) : FileMetrics :=
    match lines with
    | [] => acc
    | line :: rest =>
        let (acc, inBlock) := lineMetrics line inBlock acc
        go rest inBlock acc
  go lines false {}

@[inline] def readFileMetrics (path : System.FilePath) : IO FileMetrics := do
  let content ← IO.FS.readFile path
  let lines := content.splitOn "\n"
  return scanLines lines

@[inline] def shouldSkipDirName (name : String) : Bool :=
  name == ".git" || name == ".lake" || name == "artifacts" || name == "build" || name == ".github"

partial def listLeanFiles (root : System.FilePath) : IO (Array System.FilePath) := do
  let mut acc := #[]
  for entry in (← root.readDir) do
    let p := entry.path
    if (← p.isDir) then
      let name := p.fileName.getD ""
      if shouldSkipDirName name then
        pure ()
      else
        acc := acc ++ (← listLeanFiles p)
    else if p.extension == some "lean" then
      acc := acc.push p
  return acc

@[inline] def takeArray (xs : Array α) (n : Nat) : Array α :=
  (xs.toList.take n).toArray

@[inline] def num (n : Nat) : Json :=
  Json.num (n : JsonNumber)

@[inline] def ratioBp (num denom : Nat) : Nat :=
  if denom == 0 then 0 else (num * 10000) / denom

@[inline] def metricsJsonFrom (m : FileMetrics) : Json :=
  Json.mkObj
    [ ("loc", num m.loc)
    , ("loc_nonempty", num m.locNonEmpty)
    , ("comment_lines", num m.commentLines)
    , ("comment_ratio_bp", num (ratioBp m.commentLines m.locNonEmpty))
    , ("todo", num m.todo)
    , ("fixme", num m.fixme)
    , ("porting_note", num m.portingNote)
    , ("adaptation_note", num m.adaptationNote)
    , ("nolint", num m.nolint)
    ]

@[inline] def fileNodeJson (path : System.FilePath) (m : FileMetrics) : Json :=
  let pathStr := path.toString
  let name := path.fileName.getD pathStr
  Json.mkObj
    [ ("id", Json.str pathStr)
    , ("kind", Json.str "module")
    , ("name", Json.str name)
    , ("path", Json.str pathStr)
    , ("span", Json.mkObj
        [ ("file", Json.str pathStr)
        , ("line", Json.num 1)
        , ("col", Json.num 1)
        ])
    , ("metrics", metricsJsonFrom m)
    , ("tags", Json.arr #[])
    , ("children", Json.arr #[])
    ]

@[inline] def rootNodeJsonFrom (root : String) (children : Array Json) (m : FileMetrics) : Json :=
  Json.mkObj
    [ ("id", Json.str "root")
    , ("kind", Json.str "module")
    , ("name", Json.str "root")
    , ("path", Json.str root)
    , ("span", Json.mkObj
        [ ("file", Json.str "")
        , ("line", Json.num 1)
        , ("col", Json.num 1)
        ])
    , ("metrics", metricsJsonFrom m)
    , ("tags", Json.arr #[])
    , ("children", Json.arr children)
    ]

@[inline] def artifactJson (cfg : ObserveConfig) (generatedAt : String) : IO Json := do
  if cfg.sample then
    let sampleMetrics : FileMetrics := { loc := 1200, locNonEmpty := 1100, commentLines := 200 }
    return Json.mkObj
      [ ("schema_version", Json.str cfg.schemaVersion)
      , ("generated_at", Json.str generatedAt)
      , ("project", Json.mkObj
          [ ("name", Json.str "LeanProject")
          , ("root", Json.str cfg.root)
          , ("commit", Json.str "")
          , ("tool", Json.str "leanobserve")
          ])
      , ("root", rootNodeJsonFrom cfg.root #[] sampleMetrics)
      ]
  let root := System.FilePath.mk cfg.root
  let filesAll ← listLeanFiles root
  let files := match cfg.maxFiles? with
    | some n => takeArray filesAll n
    | none => filesAll
  let mut total : FileMetrics := {}
  let mut children : Array Json := #[]
  for p in files do
    let m ← readFileMetrics p
    total := total.add m
    children := children.push (fileNodeJson p m)
  return Json.mkObj
    [ ("schema_version", Json.str cfg.schemaVersion)
    , ("generated_at", Json.str generatedAt)
    , ("project", Json.mkObj
        [ ("name", Json.str "LeanProject")
        , ("root", Json.str cfg.root)
        , ("commit", Json.str "")
        , ("tool", Json.str "leanobserve")
        ])
    , ("root", rootNodeJsonFrom cfg.root children total)
    ]

@[inline] def runLeanObserveCmd (p : Cli.Parsed) : IO UInt32 := do
  try
    let cfg := configFromParsed p
    let now ← IO.monoMsNow
    let output ← artifactJson cfg (toString now)
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
    "max-files" : Nat; "Limit number of files scanned"
    sample; "Emit sample metrics values"
]

def main (args : List String) : IO UInt32 :=
  leanObserveCmd.validate args
