import Cli
import Lean
import Std

open Cli
open Lean

structure ObserveConfig where
  root : String := "."
  out? : Option String := none
  schemaVersion : String := "0.1.0"
  sample : Bool := false
  maxFiles? : Option Nat := none
  buildLog? : Option String := none

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
    match p.flag? "build-log" with
    | some f => cfg := { cfg with buildLog? := some (f.as! String) }
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
  buildTimeMs : Nat := 0

@[inline] def FileMetrics.add (a b : FileMetrics) : FileMetrics :=
  { loc := a.loc + b.loc
  , locNonEmpty := a.locNonEmpty + b.locNonEmpty
  , commentLines := a.commentLines + b.commentLines
  , todo := a.todo + b.todo
  , fixme := a.fixme + b.fixme
  , portingNote := a.portingNote + b.portingNote
  , adaptationNote := a.adaptationNote + b.adaptationNote
  , nolint := a.nolint + b.nolint
  , buildTimeMs := a.buildTimeMs + b.buildTimeMs
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

@[inline] def readFileMetrics (path : System.FilePath) (buildTimeMs : Nat) : IO FileMetrics := do
  let content ← IO.FS.readFile path
  let lines := content.splitOn "\n"
  let m := scanLines lines
  return { m with buildTimeMs := buildTimeMs }

@[inline] def trimAsciiString (s : String) : String :=
  s.toSlice.trimAscii.toString

@[inline] def dropWhileChars (p : Char → Bool) (xs : List Char) : List Char :=
  match xs with
  | [] => []
  | x :: xs => if p x then dropWhileChars p xs else x :: xs

@[inline] def dropRightWhileChars (p : Char → Bool) (xs : List Char) : List Char :=
  (dropWhileChars p xs.reverse).reverse

@[inline] def dropRightNChars (n : Nat) (xs : List Char) : List Char :=
  (xs.reverse.drop n).reverse

@[inline] def dropRightN (s : String) (n : Nat) : String :=
  String.ofList (dropRightNChars n s.toList)

@[inline] def isDigitChar (c : Char) : Bool :=
  c >= '0' && c <= '9'

@[inline] def parseNat? (s : String) : Option Nat :=
  if s.isEmpty then
    none
  else if s.toList.all isDigitChar then
    s.toNat?
  else
    none

@[inline] def parseMsToken? (tok : String) : Option Nat :=
  let t := trimAsciiString tok
  let t := String.ofList (dropRightWhileChars (fun c => c == ',' || c == ')' || c == ']') t.toList)
  if t.endsWith "ms" then
    parseNat? (dropRightN t 2)
  else if t.endsWith "s" then
    let base := dropRightN t 1
    match base.splitOn "." with
    | [whole] =>
        parseNat? whole |>.map (fun n => n * 1000)
    | [whole, frac] =>
        match parseNat? whole with
        | none => none
        | some wholeNat =>
            let fracChars := frac.toList.take 3
            let fracDigits := fracChars.foldl (fun acc c => acc * 10 + (c.toNat - '0'.toNat)) 0
            let scale := match fracChars.length with
              | 0 => 1000
              | 1 => 100
              | 2 => 10
              | _ => 1
            some (wholeNat * 1000 + fracDigits * scale)
    | _ => none
  else
    none

@[inline] def trimPathToken (tok : String) : String :=
  let t := trimAsciiString tok
  let t := String.ofList (dropWhileChars (fun c => c == '"' || c == '(' || c == '[') t.toList)
  String.ofList (dropRightWhileChars (fun c => c == '"' || c == ')' || c == ']' || c == ',' || c == ':') t.toList)

@[inline] def extractLeanPath? (line : String) : Option String :=
  let tokens := (line.splitOn " ").filter (fun s => s != "")
  tokens.findSome? (fun t =>
    if containsSubstr t ".lean" then
      some (trimPathToken t)
    else
      none)

@[inline] def extractDurationMs? (line : String) : Option Nat :=
  let tokens := (line.splitOn " ").filter (fun s => s != "")
  tokens.findSome? parseMsToken?

@[inline] def parseBuildLog (path : System.FilePath) : IO (List (String × Nat)) := do
  let content ← IO.FS.readFile path
  let mut acc : List (String × Nat) := []
  for line in content.splitOn "\n" do
    match extractLeanPath? line, extractDurationMs? line with
    | some p, some ms => acc := (p, ms) :: acc
    | _, _ => pure ()
  return acc

@[inline] def findBuildTimeMs? (entries : List (String × Nat)) (path : System.FilePath) : Option Nat :=
  let p := path.toString
  entries.findSome? (fun (k, v) =>
    if k.endsWith p || p.endsWith k then some v else none)

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
    , ("build_time_ms", num m.buildTimeMs)
    ]

structure NodeAcc where
  name : String
  path : System.FilePath
  isFile : Bool := false
  metrics : FileMetrics := {}
  children : List NodeAcc := []

@[inline] def emptyNode (name : String) (path : System.FilePath) : NodeAcc :=
  { name := name, path := path }

@[inline] def dropPrefix (pre xs : List String) : List String :=
  match pre, xs with
  | [], xs => xs
  | _, [] => []
  | p :: ps, x :: xs =>
      if p == x then dropPrefix ps xs else x :: xs

@[inline] def relativeComponents (root path : System.FilePath) : List String :=
  dropPrefix root.components path.components

partial def upsertChild (children : List NodeAcc) (name : String) (path : System.FilePath)
    (f : NodeAcc → NodeAcc) : List NodeAcc :=
  match children with
  | [] => [f (emptyNode name path)]
  | c :: cs =>
      if c.name == name then
        f c :: cs
      else
        c :: upsertChild cs name path f

partial def insertNode (node : NodeAcc) (basePath : System.FilePath) (components : List String)
    (filePath : System.FilePath) (m : FileMetrics) : NodeAcc :=
  let node := { node with metrics := node.metrics.add m }
  match components with
  | [] =>
      { node with isFile := true, path := filePath }
  | c :: cs =>
      let childPath := basePath / c
      let children := upsertChild node.children c childPath (fun child => insertNode child childPath cs filePath m)
      { node with children := children }

@[inline] def tagsJson (isFile : Bool) : Json :=
  if isFile then
    Json.arr #[Json.str "file"]
  else
    Json.arr #[Json.str "dir"]

partial def nodeAccToJson (node : NodeAcc) : Json :=
  let childrenJson := (node.children.map nodeAccToJson).toArray
  let pathStr := node.path.toString
  Json.mkObj
    [ ("id", Json.str pathStr)
    , ("kind", Json.str "module")
    , ("name", Json.str node.name)
    , ("path", Json.str pathStr)
    , ("span", Json.mkObj
        [ ("file", Json.str pathStr)
        , ("line", Json.num 1)
        , ("col", Json.num 1)
        ])
    , ("metrics", metricsJsonFrom node.metrics)
    , ("tags", tagsJson node.isFile)
    , ("children", Json.arr childrenJson)
    ]

@[inline] def artifactJson (cfg : ObserveConfig) (generatedAt : String) : IO Json := do
  if cfg.sample then
    let sampleMetrics : FileMetrics := { loc := 1200, locNonEmpty := 1100, commentLines := 200, buildTimeMs := 4200 }
    let rootPath := System.FilePath.mk cfg.root
    let rootAcc : NodeAcc := { (emptyNode "root" rootPath) with metrics := sampleMetrics }
    return Json.mkObj
      [ ("schema_version", Json.str cfg.schemaVersion)
      , ("generated_at", Json.str generatedAt)
      , ("project", Json.mkObj
          [ ("name", Json.str "LeanProject")
          , ("root", Json.str cfg.root)
          , ("commit", Json.str "")
          , ("tool", Json.str "leanobserve")
          ])
      , ("root", nodeAccToJson rootAcc)
      ]
  let root := System.FilePath.mk cfg.root
  let filesAll ← listLeanFiles root
  let files := match cfg.maxFiles? with
    | some n => takeArray filesAll n
    | none => filesAll
  let buildTimes ← match cfg.buildLog? with
    | some path => parseBuildLog (System.FilePath.mk path)
    | none => pure []
  let mut rootAcc := emptyNode "root" root
  for p in files do
    let buildTime := findBuildTimeMs? buildTimes p |>.getD 0
    let m ← readFileMetrics p buildTime
    let comps := relativeComponents root p
    rootAcc := insertNode rootAcc root comps p m
  return Json.mkObj
    [ ("schema_version", Json.str cfg.schemaVersion)
    , ("generated_at", Json.str generatedAt)
    , ("project", Json.mkObj
        [ ("name", Json.str "LeanProject")
        , ("root", Json.str cfg.root)
        , ("commit", Json.str "")
        , ("tool", Json.str "leanobserve")
        ])
    , ("root", nodeAccToJson rootAcc)
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
    "build-log" : String; "Path to build log with timing entries (optional)"
    sample; "Emit sample metrics values"
]

def main (args : List String) : IO UInt32 :=
  leanObserveCmd.validate args
