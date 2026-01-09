import Cli
import Lean
import Std
import LeanBench.TextScan
import LeanBench.ParsecScan
import LeanBench.Runtime

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
  spansOut? : Option String := none
  -- Runtime profiling imports
  perfScript? : Option String := none
  perfStat? : Option String := none
  instrumentsTrace? : Option String := none
  metalTrace? : Option String := none
  gpuJson? : Option String := none
  cudaKernels? : Option String := none
  cudaApi? : Option String := none
  tracyTrace? : Option String := none
  dtraceOutput? : Option String := none

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
      cfg := { cfg with commandNodes := true, infoTree := true }
    if p.hasFlag "decl-nodes" then
      cfg := { cfg with commandNodes := true, declNodesOnly := true, infoTree := true }
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
    match p.flag? "spans-out" with
    | some f => cfg := { cfg with spansOut? := some (f.as! String) }
    | none => pure ()
    -- Runtime profiling flags
    match p.flag? "perf" with
    | some f => cfg := { cfg with perfScript? := some (f.as! String) }
    | none => pure ()
    match p.flag? "perf-stat" with
    | some f => cfg := { cfg with perfStat? := some (f.as! String) }
    | none => pure ()
    match p.flag? "instruments" with
    | some f => cfg := { cfg with instrumentsTrace? := some (f.as! String) }
    | none => pure ()
    match p.flag? "metal" with
    | some f => cfg := { cfg with metalTrace? := some (f.as! String) }
    | none => pure ()
    match p.flag? "gpu-json" with
    | some f => cfg := { cfg with gpuJson? := some (f.as! String) }
    | none => pure ()
    match p.flag? "cuda" with
    | some f => cfg := { cfg with cudaKernels? := some (f.as! String) }
    | none => pure ()
    match p.flag? "cuda-api" with
    | some f => cfg := { cfg with cudaApi? := some (f.as! String) }
    | none => pure ()
    match p.flag? "tracy" with
    | some f => cfg := { cfg with tracyTrace? := some (f.as! String) }
    | none => pure ()
    match p.flag? "dtrace" with
    | some f => cfg := { cfg with dtraceOutput? := some (f.as! String) }
    | none => pure ()
    if cfg.spansOut?.isSome then
      cfg := { cfg with commandNodes := true, infoTree := true }
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

def defaultNodeAcc : NodeAcc :=
  { id := ""
    name := ""
    path := System.FilePath.mk ""
    kind := "module"
    span := { file := "", line := 0, col := 0, endLine := 0, endCol := 0 }
    isFile := false
    metrics := {}
    children := []
  }

instance : Inhabited NodeAcc := ⟨defaultNodeAcc⟩

instance : Inhabited (NodeAcc × Nat) := ⟨(defaultNodeAcc, 0)⟩

structure DeclShare where
  node : NodeAcc
  base : Nat
  rem : Float

instance : Inhabited DeclShare := ⟨{ node := defaultNodeAcc, base := 0, rem := 0.0 }⟩

@[inline] def profileWeightSourceDirect : Nat := 1
@[inline] def profileWeightSourceDistributed : Nat := 2

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

@[inline] def mergeByDecl (a b : MetricByDecl) : MetricByDecl :=
  b.fold (init := a) (fun acc decl metrics =>
    let prev := acc.getD decl emptyMetricMap
    acc.insert decl (mergeMetricMap prev metrics))

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

@[inline] def jsonGetNatOptionArray? (j : Json) : Option (Array (Option Nat)) :=
  match jsonGetArr? j with
  | none => none
  | some arr =>
      Id.run do
        let mut out : Array (Option Nat) := Array.mkEmpty arr.size
        for x in arr do
          match x with
          | .null => out := out.push none
          | _ =>
              match jsonGetNat? x with
              | some v => out := out.push (some v)
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

@[inline] def lastSegment (xs : List String) : String :=
  match xs.reverse with
  | [] => ""
  | x :: _ => x

@[inline] def dropLastSegment (xs : List String) : List String :=
  match xs with
  | [] => []
  | _ :: [] => []
  | x :: rest => x :: dropLastSegment rest

@[inline] def declMapFromNodes (cmds : Std.HashMap System.FilePath (Array NodeAcc)) :
    Std.HashMap String System.FilePath :=
  Id.run do
    let mut counts : Std.HashMap String Nat := {}
    for (_, nodes) in cmds.toList do
      for n in nodes do
        if n.kind != "decl" || n.name.isEmpty then
          pure ()
        else
          let short := lastSegment (n.name.splitOn ".")
          let prev := counts.getD short 0
          counts := counts.insert short (prev + 1)
    let mut out : Std.HashMap String System.FilePath := {}
    for (path, nodes) in cmds.toList do
      for n in nodes do
        if n.kind != "decl" || n.name.isEmpty then
          pure ()
        else if !out.contains n.name then
          out := out.insert n.name path
        let short := lastSegment (n.name.splitOn ".")
        if counts.getD short 0 == 1 && !out.contains short then
          out := out.insert short path
    return out

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

@[inline] def lookupDeclPath? (declMap : Std.HashMap String System.FilePath) (name : String) :
    Option System.FilePath :=
  match declMap.get? name with
  | some path => some path
  | none =>
      let parts := name.splitOn "."
      match parts.reverse with
      | [] => none
      | _ :: rest =>
          let parent := String.intercalate "." rest.reverse
          if parent.isEmpty then none else declMap.get? parent

@[inline] def lookupDeclKey? (declMap : Std.HashMap String System.FilePath) (name : String) :
    Option String :=
  if declMap.contains name then
    some name
  else
    let parts := name.splitOn "."
    match parts.reverse with
    | [] => none
    | _ :: rest =>
        let parent := String.intercalate "." rest.reverse
        if !parent.isEmpty && declMap.contains parent then
          some parent
        else
        let short := lastSegment parts
        if declMap.contains short then some short else none

@[inline] def lookupDeclMetric? (declMetrics : MetricByDecl) (name : String) :
    Option MetricMap :=
  match declMetrics.get? name with
  | some m => some m
  | none =>
      let parts := name.splitOn "."
      match parts.reverse with
      | [] => none
      | _ :: rest =>
          let parent := String.intercalate "." rest.reverse
          if !parent.isEmpty && declMetrics.contains parent then
            declMetrics.get? parent
          else
            let short := lastSegment parts
            declMetrics.get? short

@[inline] def applyDeclMetrics (cmds : Std.HashMap System.FilePath (Array NodeAcc))
    (declMetrics : MetricByDecl) : Std.HashMap System.FilePath (Array NodeAcc) :=
  Id.run do
    let mut updated : Std.HashMap System.FilePath (Array NodeAcc) := {}
    for (path, nodes) in cmds.toList do
      let mut newNodes := nodes
      for idx in [0:nodes.size] do
        let n := nodes[idx]!
        if n.kind != "decl" || n.name.isEmpty then
          pure ()
        else
          match lookupDeclMetric? declMetrics n.name with
          | some m =>
              let merged := mergeMetricMap n.metrics m
              newNodes := newNodes.set! idx { n with metrics := merged }
          | none => pure ()
      updated := updated.insert path newNodes
    return updated

@[inline] def stackFrameList (stackIdx : Nat) (stackFrames : Array Nat)
    (stackPrefix : Array (Option Nat)) : List Nat :=
  Id.run do
    let mut idx? : Option Nat := some stackIdx
    let mut out : List Nat := []
    let mut steps := 0
    let maxSteps := stackFrames.size
    while steps < maxSteps do
      match idx? with
      | none => break
      | some idx =>
          if idx >= stackFrames.size then
            break
          out := stackFrames[idx]! :: out
          idx? := stackPrefix.getD idx none
          steps := steps + 1
    return out.reverse

@[inline] def funcNameFromFrame? (frameIdx : Nat)
    (frameFuncs : Array Nat) (funcNames : Array Nat)
    (strings : Array String) : Option String :=
  match arrayGet? frameFuncs frameIdx with
  | none => none
  | some funcIdx =>
      match arrayGet? funcNames funcIdx with
      | none => none
      | some nameIdx =>
          arrayGet? strings nameIdx

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
                  let some stackPrefix := jsonGetField? stackTableObj "prefix" >>= jsonGetNatOptionArray?
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
                        let frames := (stackFrameList stackIdx stackFrames stackPrefix).reverse
                        let mut declKey : Option String := none
                        let mut declPath : Option System.FilePath := none
                        let mut modulePath : Option System.FilePath := none
                        for frameIdx in frames do
                          if declKey.isSome && modulePath.isSome then
                            break
                          match funcNameFromFrame? frameIdx frameFuncs funcNames strings with
                          | none => pure ()
                          | some funcName =>
                              if declKey.isNone then
                                match declFullFromFuncName funcName with
                                | some decl =>
                                    match lookupDeclKey? declMap decl with
                                    | some key =>
                                        declKey := some key
                                        declPath := declMap.get? key
                                    | none => pure ()
                                | none => pure ()
                              if declKey.isNone then
                                match declFromFuncName funcName with
                                | some decl =>
                                    match lookupDeclKey? declMap decl with
                                    | some key =>
                                        declKey := some key
                                        declPath := declMap.get? key
                                    | none => pure ()
                                | none => pure ()
                              if modulePath.isNone then
                                match moduleFromFuncName funcName with
                                | some modName =>
                                    modulePath := moduleMap.get? modName
                                | none => pure ()
                        let filePath := match declPath, modulePath with
                          | some p, _ => some p
                          | none, some p => some p
                          | none, none => none
                        match filePath with
                        | some path => outFiles := addWeight outFiles path weight
                        | none => pure ()
                        match declKey with
                        | some key => outDecls := addDeclWeight outDecls key weight
                        | none => pure ()
                    | _, _ => pure ()
            return { fileWeights := outFiles, declWeights := outDecls }

-- floatToNat is defined in TextScan.lean

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
        kind := "file"
        path := filePath
        id := pathStr
        span := { file := pathStr, line := 1, col := 1, endLine := 1, endCol := 1 }
      }
  | c :: cs =>
      let childPath := basePath / c
      let children := upsertChild node.children c childPath (fun child => insertNode child childPath cs filePath m)
      { node with children := children }

@[inline] def tagsJson (node : NodeAcc) : Json :=
  let base :=
    if node.kind == "command" then #["command"]
    else if node.kind == "decl" then #["decl"]
    else if node.isFile then #["file"]
    else #["dir"]
  let source := node.metrics.getD "profile_weight_source" 0
  let tags :=
    if source == profileWeightSourceDirect then
      base.push "profile_weight_direct"
    else if source == profileWeightSourceDistributed then
      base.push "profile_weight_distributed"
    else
      base
  Json.arr (tags.map Json.str)

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
  profileTopDecls : List ReportItem := []

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

@[inline] def topProfileDeclItems (rootAcc : NodeAcc) (topN : Nat) : List ReportItem :=
  let leaves := collectLeaves rootAcc
  let items := leaves.filter (fun leaf => leaf.kind == "decl")
    |>.map (fun leaf =>
      { name := leaf.name
        path := leaf.path.toString
        line := leaf.span.line
        col := leaf.span.col
        value := reportMetricValue leaf.metrics "profile_weight" })
    |>.filter (fun item => item.value > 0)
  let items := (items.toArray.qsort (fun a b => a.value > b.value)).toList
  items.take topN

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
  let profileJson :=
    Json.arr <| (r.profileTopDecls.map (fun item =>
      Json.mkObj
        [ ("name", Json.str item.name)
        , ("path", Json.str item.path)
        , ("line", Json.num item.line)
        , ("col", Json.num item.col)
        , ("value", num item.value)
        ])).toArray
  Json.mkObj
    [ ("generated_at", Json.str r.generatedAt)
    , ("root", Json.str r.root)
    , ("metrics", Json.arr metricsJson.toArray)
    , ("profile_top_decls", profileJson)
    ]

@[inline] def reportToMarkdown (r : Report) : String :=
  let header := s!"# Lean Observatory Report\n\nGenerated: {r.generatedAt}\nRoot: {r.root}\n"
  let profileSection :=
    if r.profileTopDecls.isEmpty then
      ""
    else
      let items :=
        (enumerate 0 r.profileTopDecls).map (fun (idx, item) =>
          s!"{idx + 1}. {item.value} — {item.name} ({item.path}:{item.line})"
        ) |> String.intercalate "\n"
      s!"\n## Profile weight top decls (`profile_weight`)\n\n{items}\n"
  let sections :=
    r.metrics.map (fun m =>
      let items :=
        (enumerate 0 m.top).map (fun (idx, item) =>
          s!"{idx + 1}. {item.value} — {item.name} ({item.path}:{item.line})"
        ) |> String.intercalate "\n"
      s!"\n## {m.label} (`{m.key}`)\n\n{items}\n"
    ) |> String.intercalate "\n"
  header ++ profileSection ++ sections

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
  let profileTopDecls := topProfileDeclItems rootAcc cfg.reportTop
  let report : Report :=
    { generatedAt := generatedAt, root := cfg.root, metrics := metrics, profileTopDecls := profileTopDecls }
  match cfg.reportJson? with
  | some path =>
      IO.FS.writeFile (System.FilePath.mk path) (toString <| reportToJson report)
  | none => pure ()
  match cfg.reportMd? with
  | some path =>
      IO.FS.writeFile (System.FilePath.mk path) (reportToMarkdown report)
  | none => pure ()

@[inline] def topProfileDecls (cmds : Std.HashMap System.FilePath (Array NodeAcc)) (topN : Nat) :
    Array (NodeAcc × Nat) :=
  Id.run do
    let mut acc : Array (NodeAcc × Nat) := #[]
    for (_, nodes) in cmds.toList do
      for n in nodes do
        if n.kind == "decl" then
          let w := n.metrics.getD "profile_weight" 0
          if w > 0 then
            acc := acc.push (n, w)
    takeArray (acc.qsort (fun a b => a.snd > b.snd)) topN

@[inline] def profileWeightScore (m : MetricMap) : Nat :=
  let info := metricGetD m "infotree_nodes"
  let size := metricGetD m "size"
  let base := Nat.max info size
  let base := if base == 0 then 1 else base
  let calls := metricGetD m "call_sites"
  let loops := metricGetD m "loop_depth_max"
  let ifDensity := metricGetD m "if_density_bp"
  let base := base + calls + (loops * 10)
  let weighted := (base * (10000 + ifDensity)) / 10000
  if weighted == 0 then 1 else weighted

@[inline] def distributeFileWeightsToDecls (cmds : Std.HashMap System.FilePath (Array NodeAcc))
    (fileWeights : Std.HashMap System.FilePath Float) : Std.HashMap System.FilePath (Array NodeAcc) :=
  Id.run do
    let mut updated : Std.HashMap System.FilePath (Array NodeAcc) := {}
    for (path, nodes) in cmds.toList do
      let w := fileWeights.getD path 0.0
      if w <= 0.0 then
        updated := updated.insert path nodes
      else
        let mut total : Nat := 0
        let mut decls : Array DeclShare := #[]
        for n in nodes do
          if n.kind == "decl" then
            let c := profileWeightScore n.metrics
            total := total + c
            decls := decls.push { node := n, base := 0, rem := 0.0 }
        let denom := if total == 0 then decls.size else total
        let totalNat := floatToNat w
        let mut withShares : Array DeclShare := #[]
        for i in [0:decls.size] do
          let d := decls[i]!
          let c := profileWeightScore d.node.metrics
          let share := if denom == 0 then 0.0 else (Float.ofNat totalNat) * (Float.ofNat c) / (Float.ofNat denom)
          let base := (Float.toUInt64 share).toNat
          let rem := share - (Float.ofNat base)
          withShares := withShares.push { d with base := base, rem := rem }
        let totalBase := withShares.foldl (fun acc d => acc + d.base) 0
        let mut remaining := if totalBase >= totalNat then 0 else totalNat - totalBase
        let mut sorted := withShares.qsort (fun a b => a.rem > b.rem)
        let mut i := 0
        while i < sorted.size && remaining > 0 do
          let d := sorted[i]!
          sorted := sorted.set! i { d with base := d.base + 1 }
          remaining := remaining - 1
          i := i + 1
        let mut weightMap : Std.HashMap String Nat := {}
        for d in sorted do
          weightMap := weightMap.insert d.node.id d.base
        let nodes := nodes.map (fun n =>
          if n.kind != "decl" then
            n
          else
            let prev := n.metrics.getD "profile_weight" 0
            if prev != 0 then
              n
            else
              let share := weightMap.getD n.id 0
              if share == 0 then n else
                { n with metrics :=
                    (n.metrics.insert "profile_weight" share).insert "profile_weight_source" profileWeightSourceDistributed })
        updated := updated.insert path nodes
    return updated

@[inline] def printProfileTop (cfg : ObserveConfig) (cmds : Std.HashMap System.FilePath (Array NodeAcc)) : IO Unit := do
  if cfg.profileJson?.isNone || !cfg.commandNodes then
    return ()
  let topN := if cfg.reportTop == 0 then 20 else cfg.reportTop
  let tops := topProfileDecls cmds topN
  if tops.isEmpty then
    return ()
  IO.println "profile_weight top decls:"
  for i in [0:tops.size] do
    let (n, w) := tops[i]!
    let span := n.span
    IO.println s!"  #{i + 1} {w} {n.name} ({n.path}:{span.line}:{span.col})"

@[inline] def rootLabel (root : System.FilePath) : String :=
  match root.components.reverse with
  | [] => root.toString
  | c :: _ => c

@[inline] def entryPath (root path : System.FilePath) : String :=
  let comps := relativeComponents root path
  String.intercalate "/" (rootLabel root :: comps)

@[inline] def relativePath (root path : System.FilePath) : String :=
  let comps := relativeComponents root path
  String.intercalate "/" comps

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

@[inline] def spanJson (span : NodeSpan) : Json :=
  Json.mkObj
    [ ("line", Json.num (span.line : JsonNumber))
    , ("col", Json.num (span.col : JsonNumber))
    , ("end_line", Json.num (span.endLine : JsonNumber))
    , ("end_col", Json.num (span.endCol : JsonNumber))
    ]

@[inline] def spanEntryJson (root : System.FilePath) (node : NodeAcc) : Json :=
  Json.mkObj
    [ ("id", Json.str node.id)
    , ("name", Json.str node.name)
    , ("kind", Json.str node.kind)
    , ("path", Json.str (relativePath root node.path))
    , ("span", spanJson node.span)
    , ("metrics", metricsJsonFrom node.metrics)
    ]

partial def collectAllNodes (node : NodeAcc) : List NodeAcc :=
  let children := node.children.foldl (fun acc child => acc ++ collectAllNodes child) []
  node :: children

@[inline] def spansJsonFrom (root : System.FilePath) (rootAcc : NodeAcc) : Json :=
  let nodes := collectAllNodes rootAcc
  let spans := nodes.toArray.map (spanEntryJson root)
  Json.mkObj
    [ ("root", Json.str (root.toString))
    , ("spans", Json.arr spans)
    ]

@[inline] def writeSpans (cfg : ObserveConfig) (root : System.FilePath)
    (rootAcc : NodeAcc) : IO Unit := do
  match cfg.spansOut? with
  | some path =>
      IO.FS.writeFile (System.FilePath.mk path) (toString <| spansJsonFrom root rootAcc)
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
  , { key := "profile_weight_source", label := "Profile weight source", kind := "enum", unit := "", group := "profile" }
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

  /-- CPU profiling metrics (perf, Instruments, Tracy). -/
  def cpuSpecs : Array MetricSpec := #[
    { key := "cpu_time_ns", label := "CPU Time", kind := "time_ns", unit := "ns", group := "cpu",
      defaultBlendWeight := 80 },
    { key := "cpu_samples", label := "CPU Samples", kind := "count", unit := "samples", group := "cpu",
      defaultBlendWeight := 60 },
    { key := "cpu_cycles", label := "CPU Cycles", kind := "count", unit := "cycles", group := "cpu" },
    { key := "cpu_instructions", label := "Instructions", kind := "count", unit := "instrs", group := "cpu" },
    { key := "cpu_cache_misses", label := "Cache Misses", kind := "count", unit := "misses", group := "cpu",
      defaultBlendWeight := 40 },
    { key := "cpu_branch_misses", label := "Branch Misses", kind := "count", unit := "misses", group := "cpu" },
    { key := "cpu_ipc_bp", label := "Instructions/Cycle", kind := "ratio_bp", unit := "bp", group := "cpu" }
  ]

  /-- GPU kernel execution metrics (Metal, CUDA, generic). -/
  def gpuSpecs : Array MetricSpec := #[
    { key := "gpu_kernel_time_ns", label := "GPU Kernel Time", kind := "time_ns", unit := "ns", group := "gpu",
      defaultBlendWeight := 80 },
    { key := "gpu_kernel_count", label := "Kernel Launches", kind := "count", unit := "launches", group := "gpu",
      defaultBlendWeight := 50 },
    { key := "gpu_memory_read_bytes", label := "GPU Memory Read", kind := "bytes", unit := "bytes", group := "gpu" },
    { key := "gpu_memory_write_bytes", label := "GPU Memory Write", kind := "bytes", unit := "bytes", group := "gpu" },
    { key := "gpu_occupancy_bp", label := "SM Occupancy", kind := "ratio_bp", unit := "bp", group := "gpu",
      defaultBlendWeight := 30 },
    { key := "gpu_bandwidth_gbps_bp", label := "Memory Bandwidth", kind := "rate_bp", unit := "bp", group := "gpu" },
    { key := "gpu_compute_util_bp", label := "Compute Utilization", kind := "ratio_bp", unit := "bp", group := "gpu" }
  ]

  /-- Memory allocation metrics (Blow "Allocs" style). -/
  def memorySpecs : Array MetricSpec := #[
    { key := "alloc_count", label := "Allocations", kind := "count", unit := "allocs", group := "memory",
      defaultBlendWeight := 40 },
    { key := "free_count", label := "Frees", kind := "count", unit := "frees", group := "memory" },
    { key := "alloc_bytes", label := "Allocated Bytes", kind := "bytes", unit := "bytes", group := "memory",
      defaultBlendWeight := 50 },
    { key := "free_bytes", label := "Freed Bytes", kind := "bytes", unit := "bytes", group := "memory" },
    { key := "live_alloc_bytes", label := "Live Alloc Bytes", kind := "bytes", unit := "bytes", group := "memory",
      defaultBlendWeight := 30 },
    { key := "gc_count", label := "GC Runs", kind := "count", unit := "runs", group := "memory" },
    { key := "gc_bytes", label := "GC Bytes", kind := "bytes", unit := "bytes", group := "memory" },
    { key := "peak_memory_bytes", label := "Peak Memory", kind := "bytes", unit := "bytes", group := "memory" },
    { key := "live_allocs", label := "Live Allocations", kind := "count", unit := "allocs", group := "memory" }
  ]

  /-- FFI call metrics. -/
  def ffiSpecs : Array MetricSpec := #[
    { key := "ffi_call_count", label := "FFI Calls", kind := "count", unit := "calls", group := "ffi",
      defaultBlendWeight := 30 },
    { key := "ffi_time_ns", label := "FFI Time", kind := "time_ns", unit := "ns", group := "ffi",
      defaultBlendWeight := 50 }
  ]

  /-- All runtime profiling specs (CPU + GPU + memory + FFI). -/
  def runtimeSpecs : Array MetricSpec := cpuSpecs ++ gpuSpecs ++ memorySpecs ++ ffiSpecs

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


@[inline] def distributeProfilerWeightFloatWith (ctx : CollectContext) (total : Float)
    (sumWeights : Nat) (weightFor : System.FilePath → Nat) : Std.HashMap System.FilePath Float :=
  Id.run do
    let files := ctx.files
    if files.isEmpty then
      return {}
    let nF := Float.ofNat files.size
    let mut out : Std.HashMap System.FilePath Float := {}
    for p in files do
      let weight :=
        if sumWeights == 0 then
          total / nF
        else
          total * (Float.ofNat (weightFor p)) / (Float.ofNat sumWeights)
      out := out.insert p weight
    return out

@[inline] def metricByFileFromWeightsWithSource (ctx : CollectContext)
    (weights : Std.HashMap System.FilePath Float) (source : Nat) : MetricByFile :=
  Id.run do
    let mut out : MetricByFile := {}
    for p in ctx.files do
      let w := weights.getD p 0.0
      let mut m : MetricMap := {}
      m := m.insert "profile_weight" (floatToNat w)
      if source != 0 then
        m := m.insert "profile_weight_source" source
      out := out.insert p m
    return out

@[inline] def metricByFileFromWeights (ctx : CollectContext)
    (weights : Std.HashMap System.FilePath Float) : MetricByFile :=
  metricByFileFromWeightsWithSource ctx weights 0

@[inline] def distributeProfilerWeightWith (ctx : CollectContext) (total : Float)
    (sumWeights : Nat) (weightFor : System.FilePath → Nat) : MetricByFile :=
  metricByFileFromWeightsWithSource ctx (distributeProfilerWeightFloatWith ctx total sumWeights weightFor)
    profileWeightSourceDistributed

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
          let mut fileWeights : Std.HashMap System.FilePath Float := {}
          let mut fileWeightSource := 0
          if !weights.fileWeights.isEmpty then
            fileWeights := weights.fileWeights
            fileWeightSource := profileWeightSourceDirect
          let mut appliedDeclWeights := false
          if !weights.declWeights.isEmpty && !cmdMap.isEmpty then
            let mut updated : Std.HashMap System.FilePath (Array NodeAcc) := {}
            for (path, nodes) in cmdMap.toList do
              let mut newNodes : Array NodeAcc := #[]
              for n in nodes do
                if n.kind == "decl" then
                  let w := weights.declWeights.getD n.name 0.0
                  if w == 0.0 then
                    newNodes := newNodes.push n
                  else
                    appliedDeclWeights := true
                    let m := (n.metrics.insert "profile_weight" (floatToNat w)).insert
                      "profile_weight_source" profileWeightSourceDirect
                    newNodes := newNodes.push { n with metrics := m }
                else
                  newNodes := newNodes.push n
              updated := updated.insert path newNodes
            if appliedDeclWeights then
              ctx.commandNodesRef.set updated
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
          if fileWeights.isEmpty then
            if !infoWeights.isEmpty then
              let sum := infoWeights.fold (fun acc _ v => acc + v) 0
              fileWeights := distributeProfilerWeightFloatWith ctx total sum (fun p => infoWeights.getD p 0)
              fileWeightSource := profileWeightSourceDistributed
            else
              let totalBuild : Nat := ctx.files.foldl (fun acc p => acc + findBuildTimeMs ctx.buildTimes p) 0
              fileWeights := distributeProfilerWeightFloatWith ctx total totalBuild (fun p => findBuildTimeMs ctx.buildTimes p)
              fileWeightSource := profileWeightSourceDistributed
          if !appliedDeclWeights && !fileWeights.isEmpty && !cmdMap.isEmpty then
            let updated := distributeFileWeightsToDecls cmdMap fileWeights
            ctx.commandNodesRef.set updated
          if !fileWeights.isEmpty then
            return metricByFileFromWeightsWithSource ctx fileWeights fileWeightSource
          else
            return {}

/-- Runtime collector - imports CPU/GPU/memory metrics from external profiler data. -/
@[inline] def collectRuntime (ctx : CollectContext) : IO MetricByFile := do
  let mut acc : MetricByFile := {}
  let mut declAcc : MetricByDecl := {}

  -- Import Linux perf data
  if ctx.cfg.perfScript?.isSome || ctx.cfg.perfStat?.isSome then
    let scriptPath := ctx.cfg.perfScript?.map System.FilePath.mk
    let statPath := ctx.cfg.perfStat?.map System.FilePath.mk
    let (perf, decls) ← LeanBench.Runtime.collectFromPerfWithDecls scriptPath statPath
    acc := mergeByFile acc perf
    declAcc := mergeByDecl declAcc decls

  -- Import macOS Instruments trace
  if let some path := ctx.cfg.instrumentsTrace? then
    let (inst, decls) ← LeanBench.Runtime.collectFromInstrumentsWithDecls ⟨path⟩
    acc := mergeByFile acc inst
    declAcc := mergeByDecl declAcc decls

  -- Import Metal GPU trace
  if let some path := ctx.cfg.metalTrace? then
    let (metal, decls) ← LeanBench.Runtime.collectFromMetalWithDecls ⟨path⟩
    acc := mergeByFile acc metal
    declAcc := mergeByDecl declAcc decls

  -- Import vendor-neutral GPU JSON
  if let some path := ctx.cfg.gpuJson? then
    let (gpu, decls) ← LeanBench.Runtime.collectFromRuntimeJsonWithDecls ⟨path⟩
    acc := mergeByFile acc gpu
    declAcc := mergeByDecl declAcc decls

  -- Import CUDA/NSight data
  if ctx.cfg.cudaKernels?.isSome || ctx.cfg.cudaApi?.isSome then
    let kernelPath := ctx.cfg.cudaKernels?.map System.FilePath.mk
    let apiPath := ctx.cfg.cudaApi?.map System.FilePath.mk
    let (cuda, decls) ← LeanBench.Runtime.collectFromNsightWithDecls kernelPath apiPath
    acc := mergeByFile acc cuda
    declAcc := mergeByDecl declAcc decls

  -- Import Tracy profiler data
  if let some path := ctx.cfg.tracyTrace? then
    let (tracy, decls) ← LeanBench.Runtime.collectFromTracyWithDecls ⟨path⟩
    acc := mergeByFile acc tracy
    declAcc := mergeByDecl declAcc decls

  -- Import macOS DTrace data
  if let some path := ctx.cfg.dtraceOutput? then
    let (dtrace, decls) ← LeanBench.Runtime.collectFromDTraceWithDecls ⟨path⟩
    acc := mergeByFile acc dtrace
    declAcc := mergeByDecl declAcc decls

  if !declAcc.isEmpty then
    let cmdMap ← ctx.commandNodesRef.get
    if !cmdMap.isEmpty then
      ctx.commandNodesRef.set (applyDeclMetrics cmdMap declAcc)

  return acc

@[inline] def defaultCollectors : Array Collector :=
  #[
    { id := "text-scan", specs := textScanSpecs, collect := collectTextScan },
    { id := "infotree", specs := infoTreeSpecs, collect := collectInfoTree },
    { id := "profiler", specs := profileSpecs, collect := collectProfiler },
    { id := "runtime", specs := runtimeSpecs, collect := collectRuntime }
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
  else if id == "runtime" then 40
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
    sampleMetrics := sampleMetrics.insert "profile_weight_source" profileWeightSourceDirect
    let rootPath := System.FilePath.mk cfg.root
    let rootAcc : NodeAcc := { (emptyNode "root" rootPath) with metrics := sampleMetrics }
    let specs := dedupSpecs (textScanSpecs ++ infoTreeSpecs ++ profileSpecs ++ runtimeSpecs)
    writeReport cfg rootAcc specs generatedAt
    writeEntries cfg rootPath rootAcc
    writeSpans cfg rootPath rootAcc
    printProfileTop cfg {}
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
  writeSpans cfg root rootAcc
  printProfileTop cfg cmdMap
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
    "spans-out" : String; "Write span map JSON to path (optional)"
    sample; "Emit sample metrics values"
    perf : String; "Import Linux perf script output (perf script > file.txt)"
    "perf-stat" : String; "Import Linux perf stat output (perf stat ... 2> file.txt)"
    instruments : String; "Import macOS Instruments trace XML (xcrun xctrace export)"
    metal : String; "Import Metal System Trace XML"
    "gpu-json" : String; "Import vendor-neutral GPU metrics JSON"
    cuda : String; "Import CUDA/NSight kernel JSON (nsys stats --report gpukernsum --format json)"
    "cuda-api" : String; "Import CUDA/NSight API JSON (nsys stats --report cudaapisum --format json)"
    tracy : String; "Import Tracy profiler CSV (tracy-csvexport capture.tracy)"
    dtrace : String; "Import macOS DTrace output"
]

def runLeanObserve (args : List String) : IO UInt32 :=
  leanObserveCmd.validate args
