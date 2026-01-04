import Cli
import Lean
import Std

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
  reportJson? : Option String := none
  reportMd? : Option String := none
  reportTop : Nat := 30

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
    match p.flag? "report-json" with
    | some f => cfg := { cfg with reportJson? := some (f.as! String) }
    | none => pure ()
    match p.flag? "report-md" with
    | some f => cfg := { cfg with reportMd? := some (f.as! String) }
    | none => pure ()
    match p.flag? "report-top" with
    | some f => cfg := { cfg with reportTop := f.as! Nat }
    | none => pure ()
    return cfg

abbrev MetricKey := String
abbrev MetricMap := Std.HashMap MetricKey Nat
abbrev MetricByFile := Std.HashMap System.FilePath MetricMap

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

structure Collector where
  id : String
  specs : Array MetricSpec
  collect : CollectContext → IO MetricByFile

initialize collectorRegistry : IO.Ref (Array Collector) ← IO.mkRef #[]

def registerCollector (c : Collector) : IO Unit :=
  collectorRegistry.modify (·.push c)

@[inline] def getCollectors : IO (Array Collector) :=
  collectorRegistry.get

@[inline] def emptyMetricMap : MetricMap := {}

@[inline] def mergeMetricMap (a b : MetricMap) : MetricMap :=
  b.fold (init := a) (fun acc k v =>
    let prev := acc.getD k 0
    acc.insert k (prev + v))

@[inline] def mergeByFile (a b : MetricByFile) : MetricByFile :=
  b.fold (init := a) (fun acc path metrics =>
    let prev := acc.getD path emptyMetricMap
    acc.insert path (mergeMetricMap prev metrics))

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

@[inline] def isWhitespace (c : Char) : Bool :=
  c == ' ' || c == '\t' || c == '\n' || c == '\r'

@[inline] def isWordChar (c : Char) : Bool :=
  Char.isAlphanum c || c == '_' || c == '.'

@[inline] def isWordToken (tok : String) : Bool :=
  let chars := tok.toList
  chars.any Char.isAlphanum && chars.all isWordChar

partial def tokenizeLine (line : String) : List String :=
  let rec flush (current : List Char) (acc : List String) : List String :=
    if current.isEmpty then acc else (String.ofList current.reverse) :: acc
  let rec go (chars : List Char) (current : List Char) (acc : List String) : List String :=
    match chars with
    | [] => (flush current acc).reverse
    | c :: cs =>
        if isWhitespace c then
          go cs [] (flush current acc)
        else if isWordChar c then
          go cs (c :: current) acc
        else
          let acc := flush current acc
          match c, cs with
          | ':', '=' :: rest => go rest [] (":=" :: acc)
          | '<', '-' :: rest => go rest [] ("<-" :: acc)
          | '-', '>' :: rest => go rest [] ("->" :: acc)
          | _, _ => go cs [] (String.singleton c :: acc)
  go line.toList [] []

@[inline] def countIndent (chars : List Char) : Nat :=
  match chars with
  | [] => 0
  | ' ' :: cs => 1 + countIndent cs
  | '\t' :: cs => 2 + countIndent cs
  | _ :: _ => 0

@[inline] def indentLevel (line : String) : Nat :=
  countIndent line.toList

@[inline] def popWhile (stack : List Nat) (p : Nat → Bool) : List Nat :=
  match stack with
  | [] => []
  | x :: xs => if p x then popWhile xs p else stack

structure ScanAcc where
  loc : Nat := 0
  locNonEmpty : Nat := 0
  commentLines : Nat := 0
  todo : Nat := 0
  fixme : Nat := 0
  portingNote : Nat := 0
  adaptationNote : Nat := 0
  nolint : Nat := 0
  size : Nat := 0
  ifPoints : Nat := 0
  ifDepthMax : Nat := 0
  loopDepthMax : Nat := 0
  assignments : Nat := 0
  globalReads : Nat := 0
  globalWrites : Nat := 0
  heapAllocs : Nat := 0
  heapFrees : Nat := 0
  callSites : Nat := 0
  callQualified : Nat := 0
  callNames : Std.HashSet String := {}
  ifStack : List Nat := []
  loopStack : List Nat := []

@[inline] def pushIf (acc : ScanAcc) (indent : Nat) : ScanAcc :=
  let stack := indent :: acc.ifStack
  let depth := stack.length
  let maxDepth := if depth > acc.ifDepthMax then depth else acc.ifDepthMax
  { acc with ifStack := stack, ifDepthMax := maxDepth }

@[inline] def pushLoop (acc : ScanAcc) (indent : Nat) : ScanAcc :=
  let stack := indent :: acc.loopStack
  let depth := stack.length
  let maxDepth := if depth > acc.loopDepthMax then depth else acc.loopDepthMax
  { acc with loopStack := stack, loopDepthMax := maxDepth }

partial def pushIfTimes (acc : ScanAcc) (indent : Nat) (n : Nat) : ScanAcc :=
  if n == 0 then acc else pushIfTimes (pushIf acc indent) indent (n - 1)

partial def pushLoopTimes (acc : ScanAcc) (indent : Nat) (n : Nat) : ScanAcc :=
  if n == 0 then acc else pushLoopTimes (pushLoop acc indent) indent (n - 1)

@[inline] def endsWithAny (tok : String) (suffixes : List String) : Bool :=
  suffixes.any (fun s => tok.endsWith s)

@[inline] def countSuffixMatches (tokens : List String) (suffixes : List String) : Nat :=
  tokens.foldl (fun acc t => if endsWithAny t suffixes then acc + 1 else acc) 0

def keywordList : List String :=
  [ "if", "then", "else", "match", "with", "let", "in", "do", "for", "while", "repeat"
  , "by", "have", "show", "fun", "namespace", "section", "open", "export"
  , "theorem", "lemma", "def", "structure", "inductive", "class", "instance", "abbrev"
  , "macro", "syntax", "notation", "set_option", "local", "protected", "private", "public"
  , "where", "case", "deriving", "return", "simp"
  ]

def keywordSet : Std.HashSet String :=
  Id.run do
    let mut s : Std.HashSet String := {}
    for k in keywordList do
      s := s.insert k
    return s

def defLinePrefixes : List String :=
  [ "def", "theorem", "lemma", "example", "abbrev", "structure", "inductive", "class"
  , "instance", "macro", "syntax", "notation", "axiom", "constant", "opaque"
  ]

@[inline] def isDefLine (trimmed : String) : Bool :=
  defLinePrefixes.any (fun k => trimmed.startsWith s!"{k} ")

@[inline] def isCallCandidate (tok : String) : Bool :=
  isWordToken tok && !(keywordSet.contains tok)

@[inline] def isCallNext (tok : String) : Bool :=
  isWordToken tok || tok == "(" || tok == "[" || tok == "{"

@[inline] def countCalls (tokens : List String) (callNames : Std.HashSet String) :
    Nat × Nat × Std.HashSet String :=
  let rec go (ts : List String) (sites : Nat) (qualified : Nat)
      (names : Std.HashSet String) : Nat × Nat × Std.HashSet String :=
    match ts with
    | [] => (sites, qualified, names)
    | t :: rest =>
        let next? := rest.head?
        let isCall :=
          match next? with
          | some nxt => isCallCandidate t && isCallNext nxt
          | none => false
        let (sites, qualified, names) :=
          if isCall then
            let sites := sites + 1
            let qualified := if containsSubstr t "." then qualified + 1 else qualified
            let names := names.insert t
            (sites, qualified, names)
          else
            (sites, qualified, names)
        go rest sites qualified names
  go tokens 0 0 callNames

@[inline] def lineMetrics (line : String) (inBlock : Bool) (acc : ScanAcc) : ScanAcc × Bool :=
  let acc := { acc with loc := acc.loc + 1 }
  let trimmed := line.trimAscii.toString
  let acc := if trimmed.isEmpty then acc else { acc with locNonEmpty := acc.locNonEmpty + 1 }
  let lower := line.toLower
  let acc := if lineHas lower "todo" then { acc with todo := acc.todo + 1 } else acc
  let acc := if lineHas lower "fixme" then { acc with fixme := acc.fixme + 1 } else acc
  let acc := if lineHas lower "porting note" then { acc with portingNote := acc.portingNote + 1 } else acc
  let acc := if lineHas lower "#adaptation_note" then { acc with adaptationNote := acc.adaptationNote + 1 } else acc
  let acc := if lineHas lower "nolint" then { acc with nolint := acc.nolint + 1 } else acc
  let hasBlockStart := lineHas line "/-"
  let hasBlockEnd := lineHas line "-/"
  let isLineComment := trimmed.startsWith "--"
  let isCommentLine := inBlock || isLineComment || hasBlockStart
  let acc := if isCommentLine then { acc with commentLines := acc.commentLines + 1 } else acc
  let inBlock :=
    if inBlock then
      if hasBlockEnd then false else true
    else if hasBlockStart && !hasBlockEnd then
      true
    else
      false
  if isCommentLine then
    (acc, inBlock)
  else
    let indent := indentLevel line
    let keepIf := trimmed.startsWith "else"
    let ifStack := popWhile acc.ifStack (fun lvl => indent <= lvl && !keepIf)
    let loopStack := popWhile acc.loopStack (fun lvl => indent <= lvl)
    let acc := { acc with ifStack := ifStack, loopStack := loopStack }
    let tokens := tokenizeLine line
    let wordTokens := tokens.filter isWordToken
    let acc := { acc with size := acc.size + wordTokens.length }
    let ifCount := wordTokens.foldl (fun n t => if t == "if" then n + 1 else n) 0
    let loopCount := wordTokens.foldl (fun n t =>
      if t == "for" || t == "while" || t == "repeat" then n + 1 else n) 0
    let acc := { acc with ifPoints := acc.ifPoints + ifCount }
    let acc := pushIfTimes acc indent ifCount
    let acc := pushLoopTimes acc indent loopCount
    let assignmentCount := tokens.foldl (fun n t => if t == ":=" || t == "<-" then n + 1 else n) 0
    let acc := { acc with assignments := acc.assignments + assignmentCount }
    let readSuffixes := [".get", ".get!", ".getD", ".get?", ".read", ".read?"]
    let writeSuffixes := [".set", ".set!", ".modify", ".write", ".write?"]
    let allocSuffixes :=
      [".mkRef", ".mkArray", ".mkEmpty", ".empty", ".singleton", ".push"]
    let freeSuffixes :=
      [".clear", ".erase", ".eraseD", ".pop", ".pop?", ".reset", ".shrink", ".release", ".dispose"]
    let acc := { acc with globalReads := acc.globalReads + countSuffixMatches wordTokens readSuffixes }
    let acc := { acc with globalWrites := acc.globalWrites + countSuffixMatches wordTokens writeSuffixes }
    let acc := { acc with heapAllocs := acc.heapAllocs + countSuffixMatches wordTokens allocSuffixes }
    let acc := { acc with heapFrees := acc.heapFrees + countSuffixMatches wordTokens freeSuffixes }
    let acc :=
      if isDefLine trimmed then
        acc
      else
        let (sites, qualified, names) := countCalls tokens acc.callNames
        { acc with
          callSites := acc.callSites + sites
          callQualified := acc.callQualified + qualified
          callNames := names }
    (acc, inBlock)

@[inline] def scanLines (lines : List String) : ScanAcc :=
  let rec go (lines : List String) (inBlock : Bool) (acc : ScanAcc) : ScanAcc :=
    match lines with
    | [] => acc
    | line :: rest =>
        let (acc, inBlock) := lineMetrics line inBlock acc
        go rest inBlock acc
  go lines false {}

@[inline] def readFileMetricMap (path : System.FilePath) (buildTimeMs : Nat) : IO MetricMap := do
  let content ← IO.FS.readFile path
  let lines := content.splitOn "\n"
  let acc := scanLines lines
  let mut m : MetricMap := {}
  m := m.insert "loc" acc.loc
  m := m.insert "loc_nonempty" acc.locNonEmpty
  m := m.insert "comment_lines" acc.commentLines
  m := m.insert "todo" acc.todo
  m := m.insert "fixme" acc.fixme
  m := m.insert "porting_note" acc.portingNote
  m := m.insert "adaptation_note" acc.adaptationNote
  m := m.insert "nolint" acc.nolint
  m := m.insert "size" acc.size
  m := m.insert "if_points" acc.ifPoints
  m := m.insert "if_depth_max" acc.ifDepthMax
  m := m.insert "loop_depth_max" acc.loopDepthMax
  m := m.insert "assignments" acc.assignments
  m := m.insert "global_reads" acc.globalReads
  m := m.insert "global_writes" acc.globalWrites
  m := m.insert "heap_allocations" acc.heapAllocs
  m := m.insert "heap_frees" acc.heapFrees
  m := m.insert "call_sites" acc.callSites
  m := m.insert "call_qualified" acc.callQualified
  m := m.insert "call_distinct" acc.callNames.size
  m := m.insert "build_time_ms" buildTimeMs
  return m

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

@[inline] def num (n : Nat) : Json :=
  Json.num (n : JsonNumber)

@[inline] def ratioBp (num denom : Nat) : Nat :=
  if denom == 0 then 0 else (num * 10000) / denom

@[inline] def metricGetD (m : MetricMap) (key : String) : Nat :=
  m.getD key 0

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

structure NodeAcc where
  name : String
  path : System.FilePath
  isFile : Bool := false
  metrics : MetricMap := {}
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
    (filePath : System.FilePath) (m : MetricMap) : NodeAcc :=
  let node := { node with metrics := mergeMetricMap node.metrics m }
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

structure ReportItem where
  name : String
  path : String
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
  if node.isFile || node.children.isEmpty then
    [node]
  else
    node.children.foldl (fun acc child => acc ++ collectLeaves child) []

@[inline] def reportMetricValue (m : MetricMap) (key : String) : Nat :=
  let derived := addDerivedMetrics m
  metricGetD derived key

@[inline] def buildMetricReport (spec : MetricSpec) (leaves : List NodeAcc) (topN : Nat) : MetricReport :=
  let items := leaves.map (fun leaf =>
    { name := leaf.name, path := leaf.path.toString, value := reportMetricValue leaf.metrics spec.key })
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
          s!"{idx + 1}. {item.value} — {item.path}"
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

@[inline] def collectAllMetrics (ctx : CollectContext) : IO MetricByFile := do
  let collectors ← getCollectors
  let mut acc : MetricByFile := {}
  for c in collectors do
    let m ← c.collect ctx
    acc := mergeByFile acc m
  return acc

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

initialize registerCollector { id := "text-scan", specs := textScanSpecs, collect := collectTextScan }

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

@[inline] def countInfoTrees (trees : PersistentArray InfoTree) (assign : PersistentHashMap MVarId InfoTree) : InfoTreeAcc :=
  trees.foldl (fun acc t => countInfoTree (InfoTree.substitute t assign) acc) {}

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
  let acc := countInfoTrees info.trees info.assignment
  return infoTreeMetricMap acc

@[inline] def collectInfoTree (ctx : CollectContext) : IO MetricByFile := do
  if !ctx.cfg.infoTree then
    return {}
  ensureSearchPath
  let mut acc : MetricByFile := {}
  for p in ctx.files do
    let m ←
      try
        collectInfoTreeForFile ctx p
      catch e =>
        IO.eprintln s!"infotree: {p}: {e}"
        pure emptyMetricMap
    acc := acc.insert p m
  return acc

initialize registerCollector { id := "infotree", specs := infoTreeSpecs, collect := collectInfoTree }

@[inline] def distributeProfilerWeight (ctx : CollectContext) (total : Float) : MetricByFile :=
  Id.run do
    let files := ctx.files
    if files.isEmpty then
      return {}
    let totalBuild : Nat := files.foldl (fun acc p => acc + findBuildTimeMs ctx.buildTimes p) 0
    let totalBuildF := Float.ofNat totalBuild
    let nF := Float.ofNat files.size
    let mut out : MetricByFile := {}
    for p in files do
      let build := findBuildTimeMs ctx.buildTimes p
      let weight :=
        if totalBuild == 0 then
          total / nF
        else
          total * (Float.ofNat build) / totalBuildF
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
          return distributeProfilerWeight ctx total

initialize registerCollector { id := "profiler", specs := profileSpecs, collect := collectProfiler }

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
  let ctx : CollectContext := { cfg := cfg, root := root, files := files, buildTimes := buildTimes }
  let metricsByFile ← collectAllMetrics ctx
  let collectors ← getCollectors
  let specs := dedupSpecs (collectors.foldl (fun acc c => acc ++ c.specs) #[])
  let mut rootAcc := emptyNode "root" root
  for p in files do
    let m := metricsByFile.getD p emptyMetricMap
    let comps := relativeComponents root p
    rootAcc := insertNode rootAcc root comps p m
  writeReport cfg rootAcc specs generatedAt
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
    "report-json" : String; "Write agent report JSON to path (optional)"
    "report-md" : String; "Write agent report Markdown to path (optional)"
    "report-top" : Nat; "Top N items per metric in report (default: 30)"
    sample; "Emit sample metrics values"
]

def main (args : List String) : IO UInt32 :=
  leanObserveCmd.validate args
