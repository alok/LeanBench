import Cli
import Lean
import Std
import LeanBench.TextScan
import LeanBench.ParsecScan

open Cli
open Lean
open Lean.Elab

structure ObserveConfig where
  root : String := "."
  out? : Option String := none
  schemaVersion : String := "0.1.0"
  sample : Bool := false
  maxFiles? : Option Nat := none
  buildLog? : Option String := none
  profileJson? : Option String := none
  infoTree : Bool := false
  infoTreeJobs : Nat := 1
  commandNodes : Bool := false
  declNodesOnly : Bool := false
  reportJson? : Option String := none
  reportMd? : Option String := none
  reportTop : Nat := 30
  entriesOut? : Option String := none

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
    match p.flag? "profile-json" with
    | some f => cfg := { cfg with profileJson? := some (f.as! String) }
    | none => pure ()
    if p.hasFlag "infotree" then
      cfg := { cfg with infoTree := true }
    match p.flag? "infotree-jobs" with
    | some f => cfg := { cfg with infoTreeJobs := f.as! Nat }
    | none => pure ()
    if p.hasFlag "command-nodes" then
      cfg := { cfg with commandNodes := true }
    if p.hasFlag "decl-nodes" then
      cfg := { cfg with commandNodes := true, declNodesOnly := true }
    match p.flag? "report-json" with
    | some f => cfg := { cfg with reportJson? := some (f.as! String) }
    | none => pure ()
    match p.flag? "report-md" with
    | some f => cfg := { cfg with reportMd? := some (f.as! String) }
    | none => pure ()
    match p.flag? "report-top" with
    | some f => cfg := { cfg with reportTop := f.as! Nat }
    | none => pure ()
    match p.flag? "entries-out" with
    | some f => cfg := { cfg with entriesOut? := some (f.as! String) }
    | none => pure ()
    return cfg

structure NodeSpan where
  file : String
  line : Nat
  col : Nat
  endLine : Nat := line
  endCol : Nat := col

structure NodeAcc where
  id : String
  name : String
  path : System.FilePath
  kind : String := "module"
  span : NodeSpan
  isFile : Bool := false
  metrics : MetricMap := {}
  children : List NodeAcc := []

structure MetricSpec where
  key : String
  label : String
  kind : String := "count"
  unit : String := ""
  description : String := ""
  group : String := ""
  defaultBlendWeight : Nat := 0

@[inline] def MetricSpec.toJson (m : MetricSpec) : Json :=
  Json.mkObj
    [ ("key", Json.str m.key)
    , ("label", Json.str m.label)
    , ("kind", Json.str m.kind)
    , ("unit", Json.str m.unit)
    , ("description", Json.str m.description)
    , ("group", Json.str m.group)
    , ("default_blend_weight", Json.num (m.defaultBlendWeight : JsonNumber))
    ]

structure CollectContext where
  cfg : ObserveConfig
  root : System.FilePath
  files : Array System.FilePath
  buildTimes : Std.HashMap String Nat
  commandNodesRef : IO.Ref (Std.HashMap System.FilePath (Array NodeAcc))

structure Collector where
  id : String
  specs : Array MetricSpec
  collect : CollectContext → IO MetricByFile

@[inline] def emptyMetricMap : MetricMap := {}

@[inline] def mergeMetricMap (a b : MetricMap) : MetricMap :=
  b.fold (init := a) (fun acc k v =>
    let prev := acc.getD k 0
    acc.insert k (prev + v))

@[inline] def mergeByFile (a b : MetricByFile) : MetricByFile :=
  b.fold (init := a) (fun acc path metrics =>
    let prev := acc.getD path emptyMetricMap
    acc.insert path (mergeMetricMap prev metrics))

@[inline] def readFileMetricMap (path : System.FilePath) (buildTimeMs : Nat) : IO MetricMap := do
  let content ← IO.FS.readFile path
  let lines := content.splitOn "\n"
  let acc := LeanBench.ParsecScan.scanLinesParsec lines
  return metricMapFromScan acc buildTimeMs

@[inline] def stxSpan? (fileMap : FileMap) (filePath : System.FilePath) (stx : Syntax) : Option NodeSpan :=
  match stx.getPos? with
  | none => none
  | some startPos =>
      let endPos := stx.getTailPos?.getD startPos
      let start := fileMap.toPosition startPos
      let stop := fileMap.toPosition endPos
      some {
        file := filePath.toString
        line := start.line
        col := start.column
        endLine := stop.line
        endCol := stop.column
      }

@[inline] def sliceLines (lines : Array String) (span : NodeSpan) : List String :=
  let startLine := if span.line == 0 then 1 else span.line
  let endLine := if span.endLine < startLine then startLine else span.endLine
  let startIdx := startLine - 1
  let endIdx := endLine - 1
  let total := lines.size
  if total == 0 then
    []
  else
    let startIdx := if startIdx >= total then total - 1 else startIdx
    let endIdx := if endIdx >= total then total - 1 else endIdx
    let len := endIdx + 1 - startIdx
    (lines.toList.drop startIdx).take len

@[inline] def isNamedDecl (stx : Syntax) : Bool :=
  if !stx.isOfKind ``Lean.Parser.Command.declaration then
    false
  else
    let decl := stx[1]
    let k := decl.getKind
    k == ``Lean.Parser.Command.abbrev ||
    k == ``Lean.Parser.Command.definition ||
    k == ``Lean.Parser.Command.theorem ||
    k == ``Lean.Parser.Command.opaque ||
    k == ``Lean.Parser.Command.axiom ||
    k == ``Lean.Parser.Command.inductive ||
    k == ``Lean.Parser.Command.classInductive ||
    k == ``Lean.Parser.Command.structure

@[inline] def isInstanceDecl (stx : Syntax) : Bool :=
  stx.isOfKind ``Lean.Parser.Command.declaration &&
  stx[1].getKind == ``Lean.Parser.Command.instance

@[inline] def declNameFromCommand? (stx : Syntax) : Option Name := do
  if isNamedDecl stx then
    let (id, _) := expandDeclIdCore stx[1][1]
    some id
  else if isInstanceDecl stx then
    let optDeclId := stx[1][3]
    if optDeclId.isNone then none
    else
      let (id, _) := expandDeclIdCore optDeclId[0]
      some id
  else
    none

@[inline] def commandNameAndKind (stx : Syntax) : String × String :=
  match declNameFromCommand? stx with
  | some name => (toString name, "decl")
  | none => (toString stx.getKind, "command")

@[inline] def makeCommandNode (filePath : System.FilePath) (span : NodeSpan) (name kind : String)
    (metrics : MetricMap) : NodeAcc :=
  let id := s!"{filePath}:{span.line}:{span.col}:{span.endLine}:{span.endCol}:{name}"
  { id := id
    name := name
    path := filePath
    kind := kind
    span := span
    metrics := metrics
    isFile := false
    children := []
  }

partial def findCommandStx? (t : InfoTree) : Option Syntax :=
  match t with
  | .context _ child => findCommandStx? child
  | .node info children =>
      match info with
      | .ofCommandInfo ci => some ci.stx
      | .ofDocElabInfo di => some di.stx
      | .ofDocInfo di => some di.stx
      | _ =>
          children.findSome? findCommandStx?
  | .hole _ => none

@[inline] def trimAsciiString (s : String) : String :=
  s.toSlice.trimAscii.toString

@[inline] def parseMsToken? (tok : String) : Option Nat :=
  let t := trimAsciiString tok
  let t := (t.toSubstring.dropRightWhile (fun c => c == ',' || c == ')' || c == ']')).toString
  if t.endsWith "ms" then
    parseNat? ((t.toSubstring.dropRight 2).toString)
  else if t.endsWith "s" then
    let base := (t.toSubstring.dropRight 1).toString
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
  let t := t.toSubstring.dropWhile (fun c => c == '"' || c == '(' || c == '[')
  (t.dropRightWhile (fun c => c == '"' || c == ')' || c == ']' || c == ',' || c == ':')).toString

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

@[inline] def parseBuildLog (path : System.FilePath) : IO (Std.HashMap String Nat) := do
  let content ← IO.FS.readFile path
  let mut acc : Std.HashMap String Nat := {}
  for line in content.splitOn "\n" do
    match extractLeanPath? line, extractDurationMs? line with
    | some p, some ms => acc := acc.insert p ms
    | _, _ => pure ()
  return acc

@[inline] def jsonGetObj? (j : Json) : Option (Std.TreeMap.Raw String Json compare) :=
  match j with
  | .obj kv => some kv
  | _ => none

@[inline] def jsonGetArr? (j : Json) : Option (Array Json) :=
  match j with
  | .arr xs => some xs
  | _ => none

@[inline] def jsonGetNum? (j : Json) : Option Float :=
  match j with
  | .num n => some n.toFloat
  | _ => none

@[inline] def jsonGetField? (obj : Std.TreeMap.Raw String Json compare) (key : String) : Option Json :=
  obj.get? key

@[inline] def listGet? (xs : List α) (idx : Nat) : Option α :=
  match xs, idx with
  | [], _ => none
  | x :: _, 0 => some x
  | _ :: xs, n + 1 => listGet? xs n

@[inline] def arrayGet? (xs : Array α) (idx : Nat) : Option α :=
  listGet? xs.toList idx

@[inline] def sumJsonNumbers (arr : Array Json) : Float :=
  arr.foldl (fun acc j => acc + (jsonGetNum? j |>.getD 0.0)) 0.0

@[inline] def extractProfilerWeight (j : Json) : Float :=
  match jsonGetObj? j with
  | none => 0.0
  | some obj =>
      match jsonGetField? obj "threads" >>= jsonGetArr? with
      | none => 0.0
      | some threads =>
          match arrayGet? threads 0 with
          | none => 0.0
          | some thread =>
              match jsonGetObj? thread with
              | none => 0.0
              | some threadObj =>
                  match jsonGetField? threadObj "samples" >>= jsonGetObj? with
                  | none => 0.0
                  | some samplesObj =>
                      match jsonGetField? samplesObj "weight" >>= jsonGetArr? with
                      | none => 0.0
                      | some weights => sumJsonNumbers weights

@[inline] def jsonGetStr? (j : Json) : Option String :=
  match j with
  | .str s => some s
  | _ => none

@[inline] def jsonGetNat? (j : Json) : Option Nat :=
  match jsonGetNum? j with
  | none => none
  | some n =>
      if n < 0.0 then
        none
      else
        some (Float.toUInt64 n).toNat

@[inline] def jsonGetNumArray? (j : Json) : Option (Array Float) :=
  match jsonGetArr? j with
  | none => none
  | some arr =>
      Id.run do
        let mut out : Array Float := Array.mkEmpty arr.size
        for x in arr do
          match jsonGetNum? x with
          | some v => out := out.push v
          | none => return none
        return some out

@[inline] def jsonGetNatArray? (j : Json) : Option (Array Nat) :=
  match jsonGetArr? j with
  | none => none
  | some arr =>
      Id.run do
        let mut out : Array Nat := Array.mkEmpty arr.size
        for x in arr do
          match jsonGetNat? x with
          | some v => out := out.push v
          | none => return none
        return some out

@[inline] def jsonGetStringArray? (j : Json) : Option (Array String) :=
  match jsonGetArr? j with
  | none => none
  | some arr =>
      Id.run do
        let mut out : Array String := Array.mkEmpty arr.size
        for x in arr do
          match jsonGetStr? x with
          | some v => out := out.push v
          | none => return none
        return some out

@[inline] def dropLeadingPathSeparators (s : String) : String :=
  let rec go (cs : List Char) : List Char :=
    match cs with
    | '/' :: rest => go rest
    | '\\' :: rest => go rest
    | _ => cs
  String.ofList (go s.toList)

@[inline] def dropSuffixString (s : String) (suffix : String) : String :=
  if s.endsWith suffix then
    (s.toSubstring.dropRight suffix.length).toString
  else
    s

@[inline] def pathToModuleName (root path : System.FilePath) : String :=
  let rootStr := root.toString
  let pathStr := path.toString
  let rel :=
    if pathStr.startsWith rootStr then
      (pathStr.drop rootStr.length).toString
    else
      pathStr
  let rel := dropLeadingPathSeparators rel
  let rel := dropSuffixString rel ".lean"
  let chars := rel.toList.map (fun c => if c == '/' || c == '\\' then '.' else c)
  String.ofList chars

@[inline] def moduleMapFromFiles (root : System.FilePath) (files : Array System.FilePath) :
    Std.HashMap String System.FilePath :=
  Id.run do
    let mut out : Std.HashMap String System.FilePath := {}
    for p in files do
      let modName := pathToModuleName root p
      if modName.isEmpty then
        pure ()
      else
        out := out.insert modName p
    return out

@[inline] def declMapFromNodes (cmds : Std.HashMap System.FilePath (Array NodeAcc)) :
    Std.HashMap String System.FilePath :=
  Id.run do
    let mut out : Std.HashMap String System.FilePath := {}
    for (path, nodes) in cmds.toList do
      for n in nodes do
        if n.name.isEmpty then
          pure ()
        else if out.contains n.name then
          pure ()
        else
          out := out.insert n.name path
    return out

@[inline] def lastSegment (xs : List String) : String :=
  match xs.reverse with
  | [] => ""
  | x :: _ => x

@[inline] def dropLastSegment (xs : List String) : List String :=
  match xs with
  | [] => []
  | _ :: [] => []
  | x :: rest => x :: dropLastSegment rest

@[inline] def funcNameCandidate (name : String) : String :=
  (lastSegment (name.splitOn ":")).trimAscii.toString

@[inline] def moduleFromFuncName (name : String) : Option String :=
  let candidate := funcNameCandidate name
  if !candidate.contains '.' then
    none
  else
    let parts := candidate.splitOn "."
    let modParts := dropLastSegment parts
    let modName := String.intercalate "." modParts
    if modName.isEmpty then none else some modName

@[inline] def declFullFromFuncName (name : String) : Option String :=
  let candidate := funcNameCandidate name
  if candidate.isEmpty then none else some candidate

@[inline] def declFromFuncName (name : String) : Option String :=
  let candidate := funcNameCandidate name
  if candidate.isEmpty then
    none
  else if candidate.contains '.' then
    let parts := candidate.splitOn "."
    let decl := lastSegment parts
    if decl.isEmpty then none else some decl
  else
    some candidate

@[inline] def addWeight (m : Std.HashMap System.FilePath Float) (path : System.FilePath)
    (w : Float) : Std.HashMap System.FilePath Float :=
  let prev := m.getD path 0.0
  m.insert path (prev + w)

@[inline] def addDeclWeight (m : Std.HashMap String Float) (name : String)
    (w : Float) : Std.HashMap String Float :=
  let prev := m.getD name 0.0
  m.insert name (prev + w)

structure ProfilerWeights where
  fileWeights : Std.HashMap System.FilePath Float := {}
  declWeights : Std.HashMap String Float := {}

@[inline] def weightsFromProfiler (j : Json) (moduleMap : Std.HashMap String System.FilePath)
    (declMap : Std.HashMap String System.FilePath) : ProfilerWeights :=
  match jsonGetObj? j with
  | none => { fileWeights := {}, declWeights := {} }
  | some obj =>
      match jsonGetField? obj "threads" >>= jsonGetArr? with
      | none => { fileWeights := {}, declWeights := {} }
      | some threads =>
          Id.run do
            let mut outFiles : Std.HashMap System.FilePath Float := {}
            let mut outDecls : Std.HashMap String Float := {}
            for thread in threads do
              match jsonGetObj? thread with
              | none => pure ()
              | some threadObj =>
                  let some samplesObj := jsonGetField? threadObj "samples" >>= jsonGetObj?
                    | pure ()
                  let some stacks := jsonGetField? samplesObj "stack" >>= jsonGetNatArray?
                    | pure ()
                  let some weights := jsonGetField? samplesObj "weight" >>= jsonGetNumArray?
                    | pure ()
                  let some stackTableObj := jsonGetField? threadObj "stackTable" >>= jsonGetObj?
                    | pure ()
                  let some stackFrames := jsonGetField? stackTableObj "frame" >>= jsonGetNatArray?
                    | pure ()
                  let some frameTableObj := jsonGetField? threadObj "frameTable" >>= jsonGetObj?
                    | pure ()
                  let some frameFuncs := jsonGetField? frameTableObj "func" >>= jsonGetNatArray?
                    | pure ()
                  let some funcTableObj := jsonGetField? threadObj "funcTable" >>= jsonGetObj?
                    | pure ()
                  let some funcNames := jsonGetField? funcTableObj "name" >>= jsonGetNatArray?
                    | pure ()
                  let some strings := jsonGetField? threadObj "stringArray" >>= jsonGetStringArray?
                    | pure ()
                  let n := weights.size
                  for i in [0:n] do
                    match arrayGet? stacks i, arrayGet? weights i with
                    | some stackIdx, some weight =>
                        match arrayGet? stackFrames stackIdx with
                        | none => pure ()
                        | some frameIdx =>
                            match arrayGet? frameFuncs frameIdx with
                            | none => pure ()
                            | some funcIdx =>
                                match arrayGet? funcNames funcIdx with
                                | none => pure ()
                                | some nameIdx =>
                                    match arrayGet? strings nameIdx with
                                    | none => pure ()
                                    | some funcName =>
                                        match moduleFromFuncName funcName with
                                        | some modName =>
                                            match moduleMap.get? modName with
                                            | some path => outFiles := addWeight outFiles path weight
                                            | none =>
                                                match declFullFromFuncName funcName with
                                                | some decl =>
                                                    match declMap.get? decl with
                                                    | some path => outFiles := addWeight outFiles path weight
                                                    | none => pure ()
                                                | none => pure ()
                                        | none =>
                                            match declFullFromFuncName funcName with
                                            | some decl =>
                                                match declMap.get? decl with
                                                | some path => outFiles := addWeight outFiles path weight
                                                | none => pure ()
                                            | none => pure ()
                                        match declFullFromFuncName funcName with
                                        | some decl =>
                                            if declMap.contains decl then
                                              outDecls := addDeclWeight outDecls decl weight
                                            else
                                              pure ()
                                        | none => pure ()
                    | _, _ => pure ()
            return { fileWeights := outFiles, declWeights := outDecls }

@[inline] def floatToNat (f : Float) : Nat :=
  if f <= 0.0 then 0 else (Float.toUInt64 f).toNat

@[inline] def findBuildTimeMs (entries : Std.HashMap String Nat) (path : System.FilePath) : Nat :=
  let p := path.toString
  match entries.get? p with
  | some v => v
  | none =>
      entries.toList.findSome? (fun (k, v) =>
        if String.endsWith k p || String.endsWith p k then some v else none) |>.getD 0

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

@[inline] def sliceArray (xs : Array α) (start stop : Nat) : Array α :=
  let stop := min stop xs.size
  let len := stop - start
  (xs.toList.drop start |>.take len).toArray

@[inline] def num (n : Nat) : Json :=
  Json.num (n : JsonNumber)

@[inline] def ratioBp (num denom : Nat) : Nat :=
  if denom == 0 then 0 else (num * 10000) / denom

@[inline] def metricGetD (m : MetricMap) (key : String) : Nat :=
  m.getD key 0

@[inline] def defaultSpan (path : System.FilePath) : NodeSpan :=
  let pathStr := path.toString
  { file := pathStr, line := 1, col := 1, endLine := 1, endCol := 1 }

@[inline] def addDerivedMetrics (m : MetricMap) : MetricMap :=
  let commentRatio := ratioBp (metricGetD m "comment_lines") (metricGetD m "loc_nonempty")
  let ifDensity := ratioBp (metricGetD m "if_points") (metricGetD m "size")
  let callSites := metricGetD m "call_sites"
  let callQualified := metricGetD m "call_qualified"
  let callDistinct := metricGetD m "call_distinct"
  let callUnqualified := if callSites > callQualified then callSites - callQualified else 0
  let callLocalFile := ratioBp callUnqualified callSites
  let callLocalModule := ratioBp callQualified callSites
  let callConstancy :=
    if callSites == 0 then 0
    else
      let distinctRatio := ratioBp callDistinct callSites
      if distinctRatio >= 10000 then 0 else 10000 - distinctRatio
  let m := m.insert "comment_ratio_bp" commentRatio
  let m := m.insert "if_density_bp" ifDensity
  let m := m.insert "call_locality_file_bp" callLocalFile
  let m := m.insert "call_locality_module_bp" callLocalModule
  m.insert "call_constancy_bp" callConstancy

@[inline] def metricsJsonFrom (m0 : MetricMap) : Json :=
  let m := addDerivedMetrics m0
  let entries := m.toList.map (fun (k, v) => (k, num v))
  Json.mkObj entries

@[inline] def emptyNode (name : String) (path : System.FilePath) : NodeAcc :=
  let pathStr := path.toString
  { id := pathStr
    name := name
    path := path
    span := defaultSpan path
  }

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
    (filePath : System.FilePath) (m : MetricMap) : NodeAcc :=
  let node := { node with metrics := mergeMetricMap node.metrics m }
  match components with
  | [] =>
      let pathStr := filePath.toString
      { node with
        isFile := true
        path := filePath
        id := pathStr
        span := { file := pathStr, line := 1, col := 1, endLine := 1, endCol := 1 }
      }
  | c :: cs =>
      let childPath := basePath / c
      let children := upsertChild node.children c childPath (fun child => insertNode child childPath cs filePath m)
      { node with children := children }

@[inline] def tagsJson (node : NodeAcc) : Json :=
  if node.kind == "command" then
    Json.arr #[Json.str "command"]
  else if node.kind == "decl" then
    Json.arr #[Json.str "decl"]
  else if node.isFile then
    Json.arr #[Json.str "file"]
  else
    Json.arr #[Json.str "dir"]

partial def nodeAccToJson (node : NodeAcc) : Json :=
  let childrenJson := (node.children.map nodeAccToJson).toArray
  let pathStr := node.path.toString
  let span := node.span
  Json.mkObj
    [ ("id", Json.str node.id)
    , ("kind", Json.str node.kind)
    , ("name", Json.str node.name)
    , ("path", Json.str pathStr)
    , ("span", Json.mkObj
        [ ("file", Json.str span.file)
        , ("line", Json.num span.line)
        , ("col", Json.num span.col)
        , ("end_line", Json.num span.endLine)
        , ("end_col", Json.num span.endCol)
        ])
    , ("metrics", metricsJsonFrom node.metrics)
    , ("tags", tagsJson node)
    , ("children", Json.arr childrenJson)
    ]

structure ReportItem where
  name : String
  path : String
  line : Nat
  col : Nat
  value : Nat

structure MetricReport where
  key : String
  label : String
  top : List ReportItem

structure Report where
  generatedAt : String
  root : String
  metrics : List MetricReport

partial def collectLeaves (node : NodeAcc) : List NodeAcc :=
  let rec go (node : NodeAcc) (acc : List NodeAcc) : List NodeAcc :=
    if node.kind == "command" || node.kind == "decl" then
      node :: acc
    else if node.children.isEmpty then
      node :: acc
    else
      node.children.foldl (fun acc child => go child acc) acc
  (go node []).reverse

@[inline] def reportMetricValue (m : MetricMap) (key : String) : Nat :=
  let derived := addDerivedMetrics m
  metricGetD derived key

@[inline] def buildMetricReport (spec : MetricSpec) (leaves : List NodeAcc) (topN : Nat) : MetricReport :=
  let items := leaves.map (fun leaf =>
    { name := leaf.name
      path := leaf.path.toString
      line := leaf.span.line
      col := leaf.span.col
      value := reportMetricValue leaf.metrics spec.key })
  let items := (items.toArray.qsort (fun a b => a.value > b.value)).toList
  let top := items.take topN
  { key := spec.key, label := spec.label, top := top }

@[inline] def reportToJson (r : Report) : Json :=
  let metricsJson := r.metrics.map (fun m =>
    Json.mkObj
      [ ("key", Json.str m.key)
      , ("label", Json.str m.label)
      , ("top", Json.arr <| (m.top.map (fun item =>
          Json.mkObj
            [ ("name", Json.str item.name)
            , ("path", Json.str item.path)
            , ("line", Json.num item.line)
            , ("col", Json.num item.col)
            , ("value", num item.value)
            ])).toArray)
      ])
  Json.mkObj
    [ ("generated_at", Json.str r.generatedAt)
    , ("root", Json.str r.root)
    , ("metrics", Json.arr metricsJson.toArray)
    ]

@[inline] def reportToMarkdown (r : Report) : String :=
  let header := s!"# Lean Observatory Report\n\nGenerated: {r.generatedAt}\nRoot: {r.root}\n"
  let sections :=
    r.metrics.map (fun m =>
      let items :=
        (enumerate 0 m.top).map (fun (idx, item) =>
          s!"{idx + 1}. {item.value} — {item.name} ({item.path}:{item.line})"
        ) |> String.intercalate "\n"
      s!"\n## {m.label} (`{m.key}`)\n\n{items}\n"
    ) |> String.intercalate "\n"
  header ++ sections

where
  enumerate (n : Nat) (xs : List ReportItem) : List (Nat × ReportItem) :=
    match xs with
    | [] => []
    | x :: xs => (n, x) :: enumerate (n + 1) xs

@[inline] def writeReport (cfg : ObserveConfig) (rootAcc : NodeAcc) (specs : Array MetricSpec) (generatedAt : String) : IO Unit := do
  if cfg.reportJson?.isNone && cfg.reportMd?.isNone then
    return ()
  let leaves := collectLeaves rootAcc
  let metrics := specs.toList.map (fun spec => buildMetricReport spec leaves cfg.reportTop)
  let report : Report := { generatedAt := generatedAt, root := cfg.root, metrics := metrics }
  match cfg.reportJson? with
  | some path =>
      IO.FS.writeFile (System.FilePath.mk path) (toString <| reportToJson report)
  | none => pure ()
  match cfg.reportMd? with
  | some path =>
      IO.FS.writeFile (System.FilePath.mk path) (reportToMarkdown report)
  | none => pure ()

@[inline] def rootLabel (root : System.FilePath) : String :=
  match root.components.reverse with
  | [] => root.toString
  | c :: _ => c

@[inline] def entryPath (root path : System.FilePath) : String :=
  let comps := relativeComponents root path
  String.intercalate "/" (rootLabel root :: comps)

partial def collectFileNodes (node : NodeAcc) : List NodeAcc :=
  if node.isFile then
    [node]
  else
    node.children.foldr (fun child acc => collectFileNodes child ++ acc) []

@[inline] def entriesJsonFrom (root : System.FilePath) (node : NodeAcc) : Json :=
  let entries :=
    collectFileNodes node |>.map (fun leaf =>
      Json.mkObj
        [ ("path", Json.str (entryPath root leaf.path))
        , ("series", metricsJsonFrom leaf.metrics)
        ])
  Json.mkObj [("entries", Json.arr entries.toArray)]

@[inline] def writeEntries (cfg : ObserveConfig) (root : System.FilePath) (rootAcc : NodeAcc) : IO Unit := do
  match cfg.entriesOut? with
  | some path =>
      IO.FS.writeFile (System.FilePath.mk path) (toString <| entriesJsonFrom root rootAcc)
  | none => pure ()

partial def attachCommandNodes (node : NodeAcc) (cmds : Std.HashMap System.FilePath (Array NodeAcc)) : NodeAcc :=
  if node.isFile then
    match cmds.get? node.path with
    | some arr => { node with children := node.children ++ arr.toList }
    | none => node
  else
    let children := node.children.map (fun child => attachCommandNodes child cmds)
    { node with children := children }

@[inline] def dedupSpecs (specs : Array MetricSpec) : Array MetricSpec :=
  Id.run do
    let mut seen : Std.HashSet String := {}
    let mut out : Array MetricSpec := #[]
    for s in specs do
      if seen.contains s.key then
        pure ()
      else
        seen := seen.insert s.key
        out := out.push s
    return out

@[inline] def textScanSpecs : Array MetricSpec :=
  #[
    { key := "loc", label := "LOC", kind := "count", unit := "lines", group := "structure" },
    { key := "loc_nonempty", label := "LOC (non-empty)", kind := "count", unit := "lines", group := "structure" },
    { key := "comment_lines", label := "Comment lines", kind := "count", unit := "lines", group := "quality" },
    { key := "comment_ratio_bp", label := "Comment ratio", kind := "ratio_bp", unit := "bp", group := "quality" },
    { key := "todo", label := "TODO", kind := "count", unit := "lines", group := "quality" },
    { key := "fixme", label := "FIXME", kind := "count", unit := "lines", group := "quality" },
    { key := "porting_note", label := "Porting notes", kind := "count", unit := "lines", group := "quality" },
    { key := "adaptation_note", label := "Adaptation notes", kind := "count", unit := "lines", group := "quality" },
    { key := "nolint", label := "nolint", kind := "count", unit := "lines", group := "quality" },
    { key := "size", label := "Size (tokens)", kind := "count", unit := "tokens", group := "structure" },
    { key := "if_points", label := "If points", kind := "count", unit := "count", group := "complexity" },
    { key := "if_density_bp", label := "If density", kind := "ratio_bp", unit := "bp", group := "complexity",
      defaultBlendWeight := 20 },
    { key := "if_depth_max", label := "Max if depth", kind := "max", unit := "depth", group := "complexity" },
    { key := "loop_depth_max", label := "Max loop depth", kind := "max", unit := "depth", group := "complexity",
      defaultBlendWeight := 20 },
    { key := "assignments", label := "Assignments", kind := "count", unit := "count", group := "complexity" },
    { key := "global_reads", label := "Global reads (heuristic)", kind := "count", unit := "count", group := "effects" },
    { key := "global_writes", label := "Global writes (heuristic)", kind := "count", unit := "count", group := "effects" },
    { key := "heap_allocations", label := "Heap allocations (heuristic)", kind := "count", unit := "count", group := "perf",
      defaultBlendWeight := 30 },
    { key := "heap_frees", label := "Heap frees (heuristic)", kind := "count", unit := "count", group := "perf" },
    { key := "call_sites", label := "Call sites", kind := "count", unit := "count", group := "calls" },
    { key := "call_qualified", label := "Call sites (qualified)", kind := "count", unit := "count", group := "calls" },
    { key := "call_distinct", label := "Distinct callees", kind := "count", unit := "count", group := "calls" },
    { key := "call_locality_file_bp", label := "Call locality (file)", kind := "ratio_bp", unit := "bp", group := "calls" },
    { key := "call_locality_module_bp", label := "Call locality (module)", kind := "ratio_bp", unit := "bp", group := "calls" },
    { key := "call_constancy_bp", label := "Call constancy", kind := "ratio_bp", unit := "bp", group := "calls" },
    { key := "build_time_ms", label := "Build time", kind := "time_ms", unit := "ms", group := "build" }
  ]

@[inline] def profileSpecs : Array MetricSpec :=
  #[
    { key := "profile_weight", label := "Profile weight", kind := "weight", unit := "ticks", group := "profile",
      defaultBlendWeight := 20 }
  ]

@[inline] def collectTextScan (ctx : CollectContext) : IO MetricByFile := do
  let mut acc : MetricByFile := {}
  for p in ctx.files do
    let buildTime := findBuildTimeMs ctx.buildTimes p
    let m ← readFileMetricMap p buildTime
    acc := acc.insert p m
  return acc


structure InfoTreeAcc where
  nodes : Nat := 0
  infoNodes : Nat := 0
  contexts : Nat := 0
  holes : Nat := 0
  tacticInfos : Nat := 0
  termInfos : Nat := 0
  partialTermInfos : Nat := 0
  commandInfos : Nat := 0
  macroInfos : Nat := 0
  optionInfos : Nat := 0
  errorNameInfos : Nat := 0
  completionInfos : Nat := 0
  fieldInfos : Nat := 0
  widgetInfos : Nat := 0
  customInfos : Nat := 0
  fvarAliasInfos : Nat := 0
  fieldRedeclInfos : Nat := 0
  delabTermInfos : Nat := 0
  choiceInfos : Nat := 0
  docInfos : Nat := 0
  docElabInfos : Nat := 0

partial def countInfoTree (t : InfoTree) (acc : InfoTreeAcc) : InfoTreeAcc :=
  match t with
  | .context _ child =>
      let acc := { acc with nodes := acc.nodes + 1, contexts := acc.contexts + 1 }
      countInfoTree child acc
  | .hole _ =>
      { acc with nodes := acc.nodes + 1, holes := acc.holes + 1 }
  | .node info children =>
      let acc := { acc with nodes := acc.nodes + 1, infoNodes := acc.infoNodes + 1 }
      let acc :=
        match info with
        | .ofTacticInfo _ => { acc with tacticInfos := acc.tacticInfos + 1 }
        | .ofTermInfo _ => { acc with termInfos := acc.termInfos + 1 }
        | .ofPartialTermInfo _ => { acc with partialTermInfos := acc.partialTermInfos + 1 }
        | .ofCommandInfo _ => { acc with commandInfos := acc.commandInfos + 1 }
        | .ofMacroExpansionInfo _ => { acc with macroInfos := acc.macroInfos + 1 }
        | .ofOptionInfo _ => { acc with optionInfos := acc.optionInfos + 1 }
        | .ofErrorNameInfo _ => { acc with errorNameInfos := acc.errorNameInfos + 1 }
        | .ofCompletionInfo _ => { acc with completionInfos := acc.completionInfos + 1 }
        | .ofFieldInfo _ => { acc with fieldInfos := acc.fieldInfos + 1 }
        | .ofUserWidgetInfo _ => { acc with widgetInfos := acc.widgetInfos + 1 }
        | .ofCustomInfo _ => { acc with customInfos := acc.customInfos + 1 }
        | .ofFVarAliasInfo _ => { acc with fvarAliasInfos := acc.fvarAliasInfos + 1 }
        | .ofFieldRedeclInfo _ => { acc with fieldRedeclInfos := acc.fieldRedeclInfos + 1 }
        | .ofDelabTermInfo _ => { acc with delabTermInfos := acc.delabTermInfos + 1 }
        | .ofChoiceInfo _ => { acc with choiceInfos := acc.choiceInfos + 1 }
        | .ofDocInfo _ => { acc with docInfos := acc.docInfos + 1 }
        | .ofDocElabInfo _ => { acc with docElabInfos := acc.docElabInfos + 1 }
      children.foldl (fun acc child => countInfoTree child acc) acc

@[inline] def substituteInfoTrees (info : InfoState) : Array InfoTree :=
  info.trees.toArray.map (fun t => InfoTree.substitute t info.assignment)

@[inline] def countInfoTreesArray (trees : Array InfoTree) : InfoTreeAcc :=
  trees.foldl (fun acc t => countInfoTree t acc) {}

@[inline] def infoTreeMetricMap (acc : InfoTreeAcc) : MetricMap :=
  Id.run do
    let mut m : MetricMap := {}
    m := m.insert "infotree_nodes" acc.nodes
    m := m.insert "infotree_info_nodes" acc.infoNodes
    m := m.insert "infotree_contexts" acc.contexts
    m := m.insert "infotree_holes" acc.holes
    m := m.insert "infotree_tactic_info" acc.tacticInfos
    m := m.insert "infotree_term_info" acc.termInfos
    m := m.insert "infotree_partial_term_info" acc.partialTermInfos
    m := m.insert "infotree_command_info" acc.commandInfos
    m := m.insert "infotree_macro_expansions" acc.macroInfos
    m := m.insert "infotree_option_info" acc.optionInfos
    m := m.insert "infotree_error_name_info" acc.errorNameInfos
    m := m.insert "infotree_completion_info" acc.completionInfos
    m := m.insert "infotree_field_info" acc.fieldInfos
    m := m.insert "infotree_user_widgets" acc.widgetInfos
    m := m.insert "infotree_custom_info" acc.customInfos
    m := m.insert "infotree_fvar_alias_info" acc.fvarAliasInfos
    m := m.insert "infotree_field_redecl_info" acc.fieldRedeclInfos
    m := m.insert "infotree_delab_term_info" acc.delabTermInfos
    m := m.insert "infotree_choice_info" acc.choiceInfos
    m := m.insert "infotree_doc_info" acc.docInfos
    m := m.insert "infotree_doc_elab_info" acc.docElabInfos
    return m

@[inline] def addCommandNodes (ref : IO.Ref (Std.HashMap System.FilePath (Array NodeAcc)))
    (path : System.FilePath) (nodes : Array NodeAcc) : IO Unit := do
  ref.modify (fun m =>
    let prev := m.getD path #[]
    m.insert path (prev ++ nodes))

@[inline] def commandNodesFromTrees (filePath : System.FilePath) (inputCtx : Parser.InputContext)
    (lines : Array String) (trees : Array InfoTree) (declOnly : Bool) : Array NodeAcc :=
  Id.run do
    let mut out : Array NodeAcc := #[]
    for tree in trees do
      match findCommandStx? tree with
      | none => pure ()
      | some stx =>
          match stxSpan? inputCtx.fileMap filePath stx with
          | none => pure ()
          | some span =>
              let acc := LeanBench.ParsecScan.scanLinesParsec (sliceLines lines span)
              let textMetrics := metricMapFromScan acc 0
              let infoMetrics := infoTreeMetricMap (countInfoTree tree {})
              let metrics := mergeMetricMap textMetrics infoMetrics
              let (name, kind) := commandNameAndKind stx
              if declOnly && kind != "decl" then
                pure ()
              else
                let node := makeCommandNode filePath span name kind metrics
                out := out.push node
    return out

@[inline] def infoTreeSpecs : Array MetricSpec :=
  #[
    { key := "infotree_nodes", label := "InfoTree nodes", kind := "count", unit := "nodes", group := "infotree" },
    { key := "infotree_info_nodes", label := "InfoTree info nodes", kind := "count", unit := "nodes", group := "infotree" },
    { key := "infotree_contexts", label := "InfoTree contexts", kind := "count", unit := "nodes", group := "infotree" },
    { key := "infotree_holes", label := "InfoTree holes", kind := "count", unit := "nodes", group := "infotree" },
    { key := "infotree_tactic_info", label := "Tactic info nodes", kind := "count", unit := "nodes", group := "infotree",
      defaultBlendWeight := 30 },
    { key := "infotree_term_info", label := "Term info nodes", kind := "count", unit := "nodes", group := "infotree" },
    { key := "infotree_partial_term_info", label := "Partial term info nodes", kind := "count", unit := "nodes", group := "infotree" },
    { key := "infotree_command_info", label := "Command info nodes", kind := "count", unit := "nodes", group := "infotree" },
    { key := "infotree_macro_expansions", label := "Macro expansions", kind := "count", unit := "nodes", group := "infotree" },
    { key := "infotree_option_info", label := "Option info nodes", kind := "count", unit := "nodes", group := "infotree" },
    { key := "infotree_error_name_info", label := "Error name nodes", kind := "count", unit := "nodes", group := "infotree" },
    { key := "infotree_completion_info", label := "Completion info nodes", kind := "count", unit := "nodes", group := "infotree" },
    { key := "infotree_field_info", label := "Field info nodes", kind := "count", unit := "nodes", group := "infotree" },
    { key := "infotree_user_widgets", label := "User widgets", kind := "count", unit := "nodes", group := "infotree" },
    { key := "infotree_custom_info", label := "Custom info nodes", kind := "count", unit := "nodes", group := "infotree" },
    { key := "infotree_fvar_alias_info", label := "FVar alias nodes", kind := "count", unit := "nodes", group := "infotree" },
    { key := "infotree_field_redecl_info", label := "Field redecl nodes", kind := "count", unit := "nodes", group := "infotree" },
    { key := "infotree_delab_term_info", label := "Delab term nodes", kind := "count", unit := "nodes", group := "infotree" },
    { key := "infotree_choice_info", label := "Choice info nodes", kind := "count", unit := "nodes", group := "infotree" },
    { key := "infotree_doc_info", label := "Doc info nodes", kind := "count", unit := "nodes", group := "infotree" },
    { key := "infotree_doc_elab_info", label := "Doc elab info nodes", kind := "count", unit := "nodes", group := "infotree" }
  ]

initialize searchPathInitRef : IO.Ref Bool ← IO.mkRef false

@[inline] def ensureSearchPath : IO Unit := do
  let initialized ← searchPathInitRef.get
  if !initialized then
    Lean.initSearchPath (← Lean.findSysroot)
    searchPathInitRef.set true

@[inline] def collectInfoTreeForFile (ctx : CollectContext) (path : System.FilePath) : IO MetricMap := do
  let content ← IO.FS.readFile path
  let lines := if ctx.cfg.commandNodes then content.splitOn "\n" |>.toArray else #[]
  let fileName := path.toString
  let mainModuleName ← Lean.moduleNameOfFileName path (some ctx.root)
  let opts : Options := {}
  let inputCtx := Parser.mkInputContext content fileName
  let (header, parserState, messages) ← Parser.parseHeader inputCtx
  let (env, messages) ← Elab.processHeader (leakEnv := true) header opts messages inputCtx
  let env := env.setMainModule mainModuleName
  let mut commandState := Elab.Command.mkState env messages opts
  commandState := { commandState with infoState := { commandState.infoState with enabled := true } }
    let s ← IO.processCommands inputCtx parserState commandState
    let info := s.commandState.infoState
    let trees := substituteInfoTrees info
    if ctx.cfg.commandNodes then
      let nodes := commandNodesFromTrees path inputCtx lines trees ctx.cfg.declNodesOnly
      addCommandNodes ctx.commandNodesRef path nodes
    let acc := countInfoTreesArray trees
    return infoTreeMetricMap acc

@[inline] def collectInfoTree (ctx : CollectContext) : IO MetricByFile := do
  if !ctx.cfg.infoTree then
    return {}
  ensureSearchPath
  let collectOne := fun (p : System.FilePath) => do
    try
      collectInfoTreeForFile ctx p
    catch e =>
      IO.eprintln s!"infotree: {p}: {e}"
      pure emptyMetricMap
  let jobs := if ctx.cfg.infoTreeJobs == 0 then 1 else ctx.cfg.infoTreeJobs
  if ctx.cfg.commandNodes || jobs <= 1 || ctx.files.size <= 1 then
    let mut acc : MetricByFile := {}
    for p in ctx.files do
      let m ← collectOne p
      acc := acc.insert p m
    return acc
  let mut acc : MetricByFile := {}
  let mut i := 0
  while i < ctx.files.size do
    let chunk := sliceArray ctx.files i (i + jobs)
    let mut tasks : Array (Task (Except IO.Error MetricMap)) := #[]
    for p in chunk do
      tasks := tasks.push (← IO.asTask (collectOne p))
    for idx in [0:tasks.size] do
      let p := chunk[idx]!
      match (tasks[idx]!).get with
      | .ok m => acc := acc.insert p m
      | .error e =>
          IO.eprintln s!"infotree: {p}: {e}"
          acc := acc.insert p emptyMetricMap
    i := i + jobs
  return acc


@[inline] def distributeProfilerWeightWith (ctx : CollectContext) (total : Float)
    (sumWeights : Nat) (weightFor : System.FilePath → Nat) : MetricByFile :=
  Id.run do
    let files := ctx.files
    if files.isEmpty then
      return {}
    let nF := Float.ofNat files.size
    let mut out : MetricByFile := {}
    for p in files do
      let weight :=
        if sumWeights == 0 then
          total / nF
        else
          total * (Float.ofNat (weightFor p)) / (Float.ofNat sumWeights)
      let mut m : MetricMap := {}
      m := m.insert "profile_weight" (floatToNat weight)
      out := out.insert p m
    return out

@[inline] def collectProfiler (ctx : CollectContext) : IO MetricByFile := do
  match ctx.cfg.profileJson? with
  | none => pure {}
  | some path =>
      let content ← IO.FS.readFile path
      match Json.parse content with
      | .error _ => pure {}
      | .ok j =>
          let total := extractProfilerWeight j
          let cmdMap ← ctx.commandNodesRef.get
          let moduleMap := moduleMapFromFiles ctx.root ctx.files
          let declMap := declMapFromNodes cmdMap
          let weights := weightsFromProfiler j moduleMap declMap
          if !weights.declWeights.isEmpty && !cmdMap.isEmpty then
            let mut updated : Std.HashMap System.FilePath (Array NodeAcc) := {}
            for (path, nodes) in cmdMap.toList do
              let nodes := nodes.map (fun n =>
                if n.kind == "decl" then
                  let w := weights.declWeights.getD n.name 0.0
                  if w == 0.0 then n else { n with metrics := n.metrics.insert "profile_weight" (floatToNat w) }
                else
                  n)
              updated := updated.insert path nodes
            ctx.commandNodesRef.set updated
          if !weights.fileWeights.isEmpty then
            let mut out : MetricByFile := {}
            for p in ctx.files do
              let w := weights.fileWeights.getD p 0.0
              let mut m : MetricMap := {}
              m := m.insert "profile_weight" (floatToNat w)
              out := out.insert p m
            return out
          else
            let infoWeights : Std.HashMap System.FilePath Nat :=
              Id.run do
                let mut acc : Std.HashMap System.FilePath Nat := {}
                for (path, nodes) in cmdMap.toList do
                  let mut totalNodes := 0
                  for n in nodes do
                    totalNodes := totalNodes + (n.metrics.getD "infotree_nodes" 0)
                  if totalNodes > 0 then
                    acc := acc.insert path totalNodes
                return acc
            if !infoWeights.isEmpty then
              let sum := infoWeights.fold (fun acc _ v => acc + v) 0
              return distributeProfilerWeightWith ctx total sum (fun p => infoWeights.getD p 0)
            else
              let totalBuild : Nat := ctx.files.foldl (fun acc p => acc + findBuildTimeMs ctx.buildTimes p) 0
              return distributeProfilerWeightWith ctx total totalBuild (fun p => findBuildTimeMs ctx.buildTimes p)

@[inline] def defaultCollectors : Array Collector :=
  #[
    { id := "text-scan", specs := textScanSpecs, collect := collectTextScan },
    { id := "infotree", specs := infoTreeSpecs, collect := collectInfoTree },
    { id := "profiler", specs := profileSpecs, collect := collectProfiler }
  ]

initialize collectorRegistry : IO.Ref (Array Collector) ← IO.mkRef defaultCollectors

def registerCollector (c : Collector) : IO Unit :=
  collectorRegistry.modify (·.push c)

@[inline] def getCollectors : IO (Array Collector) :=
  collectorRegistry.get

@[inline] def collectorPriority (id : String) : Nat :=
  if id == "text-scan" then 10
  else if id == "infotree" then 20
  else if id == "profiler" then 30
  else 100

@[inline] def collectAllMetrics (ctx : CollectContext) : IO MetricByFile := do
  let collectors ← getCollectors
  let (profilers, others) :=
    collectors.foldl
      (fun (p, o) c =>
        if c.id == "profiler" then (p.push c, o) else (p, o.push c))
      (#[], #[])
  let others := others.qsort (fun a b => collectorPriority a.id < collectorPriority b.id)
  let collectors := others ++ profilers
  let mut acc : MetricByFile := {}
  for c in collectors do
    let m ← c.collect ctx
    acc := mergeByFile acc m
  return acc


@[inline] def artifactJson (cfg : ObserveConfig) (generatedAt : String) : IO Json := do
  if cfg.sample then
    let mut sampleMetrics : MetricMap := {}
    sampleMetrics := sampleMetrics.insert "loc" 1200
    sampleMetrics := sampleMetrics.insert "loc_nonempty" 1100
    sampleMetrics := sampleMetrics.insert "comment_lines" 200
    sampleMetrics := sampleMetrics.insert "size" 9000
    sampleMetrics := sampleMetrics.insert "if_points" 42
    sampleMetrics := sampleMetrics.insert "if_depth_max" 4
    sampleMetrics := sampleMetrics.insert "loop_depth_max" 2
    sampleMetrics := sampleMetrics.insert "assignments" 120
    sampleMetrics := sampleMetrics.insert "global_reads" 3
    sampleMetrics := sampleMetrics.insert "global_writes" 1
    sampleMetrics := sampleMetrics.insert "heap_allocations" 18
    sampleMetrics := sampleMetrics.insert "heap_frees" 2
    sampleMetrics := sampleMetrics.insert "call_sites" 300
    sampleMetrics := sampleMetrics.insert "call_qualified" 40
    sampleMetrics := sampleMetrics.insert "call_distinct" 80
    sampleMetrics := sampleMetrics.insert "build_time_ms" 4200
    sampleMetrics := sampleMetrics.insert "infotree_nodes" 32000
    sampleMetrics := sampleMetrics.insert "infotree_info_nodes" 21000
    sampleMetrics := sampleMetrics.insert "infotree_contexts" 6200
    sampleMetrics := sampleMetrics.insert "infotree_holes" 140
    sampleMetrics := sampleMetrics.insert "infotree_tactic_info" 900
    sampleMetrics := sampleMetrics.insert "infotree_term_info" 12000
    sampleMetrics := sampleMetrics.insert "infotree_partial_term_info" 400
    sampleMetrics := sampleMetrics.insert "infotree_command_info" 800
    sampleMetrics := sampleMetrics.insert "infotree_macro_expansions" 1200
    sampleMetrics := sampleMetrics.insert "infotree_option_info" 30
    sampleMetrics := sampleMetrics.insert "infotree_error_name_info" 8
    sampleMetrics := sampleMetrics.insert "infotree_completion_info" 5
    sampleMetrics := sampleMetrics.insert "infotree_field_info" 60
    sampleMetrics := sampleMetrics.insert "infotree_user_widgets" 10
    sampleMetrics := sampleMetrics.insert "infotree_custom_info" 25
    sampleMetrics := sampleMetrics.insert "infotree_fvar_alias_info" 200
    sampleMetrics := sampleMetrics.insert "infotree_field_redecl_info" 2
    sampleMetrics := sampleMetrics.insert "infotree_delab_term_info" 40
    sampleMetrics := sampleMetrics.insert "infotree_choice_info" 16
    sampleMetrics := sampleMetrics.insert "infotree_doc_info" 12
    sampleMetrics := sampleMetrics.insert "infotree_doc_elab_info" 18
    sampleMetrics := sampleMetrics.insert "profile_weight" 1800
    let rootPath := System.FilePath.mk cfg.root
    let rootAcc : NodeAcc := { (emptyNode "root" rootPath) with metrics := sampleMetrics }
    let specs := dedupSpecs (textScanSpecs ++ infoTreeSpecs ++ profileSpecs)
    writeReport cfg rootAcc specs generatedAt
    writeEntries cfg rootPath rootAcc
    return Json.mkObj
      [ ("schema_version", Json.str cfg.schemaVersion)
      , ("generated_at", Json.str generatedAt)
      , ("project", Json.mkObj
          [ ("name", Json.str "LeanProject")
          , ("root", Json.str cfg.root)
          , ("commit", Json.str "")
          , ("tool", Json.str "leanobserve")
          ])
      , ("metrics", Json.arr (specs.map MetricSpec.toJson))
      , ("root", nodeAccToJson rootAcc)
      ]
  let root := System.FilePath.mk cfg.root
  let filesAll ← listLeanFiles root
  let files := match cfg.maxFiles? with
    | some n => takeArray filesAll n
    | none => filesAll
  let buildTimes ← match cfg.buildLog? with
    | some path => parseBuildLog (System.FilePath.mk path)
    | none => pure {}
  let commandNodesRef ← IO.mkRef {}
  let ctx : CollectContext := { cfg := cfg, root := root, files := files, buildTimes := buildTimes, commandNodesRef := commandNodesRef }
  let metricsByFile ← collectAllMetrics ctx
  let collectors ← getCollectors
  let specs := dedupSpecs (collectors.foldl (fun acc c => acc ++ c.specs) #[])
  let mut rootAcc := emptyNode "root" root
  for p in files do
    let m := metricsByFile.getD p emptyMetricMap
    let comps := relativeComponents root p
    rootAcc := insertNode rootAcc root comps p m
  let cmdMap ← commandNodesRef.get
  rootAcc := attachCommandNodes rootAcc cmdMap
  writeReport cfg rootAcc specs generatedAt
  writeEntries cfg root rootAcc
  return Json.mkObj
    [ ("schema_version", Json.str cfg.schemaVersion)
    , ("generated_at", Json.str generatedAt)
    , ("project", Json.mkObj
        [ ("name", Json.str "LeanProject")
        , ("root", Json.str cfg.root)
        , ("commit", Json.str "")
        , ("tool", Json.str "leanobserve")
        ])
    , ("metrics", Json.arr (specs.map MetricSpec.toJson))
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
  leanobserve VIA runLeanObserveCmd; ["0.1.0"] "Lean Observatory metrics exporter."
  FLAGS:
    root : String; "Project root (default: .)"
    out : String; "Output path (default: stdout)"
    "schema-version" : String; "Schema version (default: 0.1.0)"
    "max-files" : Nat; "Limit number of files scanned"
    "build-log" : String; "Path to build log with timing entries (optional)"
    "profile-json" : String; "Path to trace.profiler.output JSON (optional)"
    infotree; "Collect InfoTree summary metrics (slow; requires lake env)"
    "infotree-jobs" : Nat; "Parallel jobs for infotree collection (default: 1)"
    "command-nodes"; "Emit command/decl nodes under each file (requires infotree)"
    "decl-nodes"; "Emit only decl nodes under each file (implies --command-nodes)"
    "report-json" : String; "Write agent report JSON to path (optional)"
    "report-md" : String; "Write agent report Markdown to path (optional)"
    "report-top" : Nat; "Top N items per metric in report (default: 30)"
    "entries-out" : String; "Write entries schema JSON to path (optional)"
    sample; "Emit sample metrics values"
]

def runLeanObserve (args : List String) : IO UInt32 :=
  leanObserveCmd.validate args
