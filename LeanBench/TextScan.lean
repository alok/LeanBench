import Std
import Lean


abbrev MetricKey := String
abbrev MetricMap := Std.HashMap MetricKey Nat
abbrev MetricByFile := Std.HashMap System.FilePath MetricMap

@[inline] def isPrefixChars (pre s : List Char) : Bool :=
  match pre, s with
  | [], _ => true
  | _, [] => false
  | p :: ps, c :: cs => p == c && isPrefixChars ps cs

@[inline] def containsSubstrChars (sChars subChars : List Char) : Bool :=
  if subChars.isEmpty then
    true
  else
    let rec loop (xs : List Char) : Bool :=
      match xs with
      | [] => false
      | _ :: xs' =>
          if isPrefixChars subChars xs then true else loop xs'
    loop sChars

@[inline] def containsSubstr (s sub : String) : Bool :=
  containsSubstrChars s.toList sub.toList

@[inline] def lineHasChars (lineChars : List Char) (needleChars : List Char) : Bool :=
  containsSubstrChars lineChars needleChars

@[inline] def isWhitespace (c : Char) : Bool :=
  c == ' ' || c == '\t' || c == '\n' || c == '\r'

@[inline] def isWordChar (c : Char) : Bool :=
  Char.isAlphanum c || c == '_' || c == '.'

@[inline] def isWordToken (tok : String) : Bool :=
  let chars := tok.toList
  chars.any Char.isAlphanum && chars.all isWordChar

partial def tokenizeLine (chars : List Char) : List String :=
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
  go chars [] []

@[inline] def countIndent (chars : List Char) : Nat :=
  match chars with
  | [] => 0
  | ' ' :: cs => 1 + countIndent cs
  | '\t' :: cs => 2 + countIndent cs
  | _ :: _ => 0

@[inline] def indentLevel (line : String) : Nat :=
  countIndent line.toList

@[inline] def indentLevelChars (lineChars : List Char) : Nat :=
  countIndent lineChars

@[inline] def popWhile (stack : List Nat) (p : Nat → Bool) : List Nat :=
  match stack with
  | [] => []
  | x :: xs => if p x then popWhile xs p else stack

@[inline] def isDigitChar (c : Char) : Bool :=
  c >= '0' && c <= '9'

@[inline] def parseNat? (s : String) : Option Nat :=
  Id.run do
    if s.isEmpty then
      return none
    let mut acc : Nat := 0
    for c in s.toList do
      if !isDigitChar c then
        return none
      acc := acc * 10 + (c.toNat - '0'.toNat)
    return some acc

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
  deriving Inhabited

@[inline] def pushIf (acc : ScanAcc) (indent : Nat) : ScanAcc :=
  { acc with
    ifStack := indent :: acc.ifStack,
    ifDepthMax := Nat.max acc.ifDepthMax (acc.ifStack.length + 1) }

@[inline] def pushLoop (acc : ScanAcc) (indent : Nat) : ScanAcc :=
  { acc with
    loopStack := indent :: acc.loopStack,
    loopDepthMax := Nat.max acc.loopDepthMax (acc.loopStack.length + 1) }

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

def todoNeedle : List Char := "todo".toList
def fixmeNeedle : List Char := "fixme".toList
def portingNoteNeedle : List Char := "porting note".toList
def adaptationNoteNeedle : List Char := "#adaptation_note".toList
def nolintNeedle : List Char := "nolint".toList
def blockStartNeedle : List Char := "/-".toList
def blockEndNeedle : List Char := "-/".toList

def simpleName (s : String) : Lean.Name :=
  Lean.Name.str Lean.Name.anonymous s

def suffixesFromNames (names : List Lean.Name) : List String :=
  names.map (fun name => s!".{name.toString}")

def readSuffixNames : List Lean.Name :=
  [simpleName "get", simpleName "get!", simpleName "getD",
   simpleName "get?", simpleName "read", simpleName "read?"]
def writeSuffixNames : List Lean.Name :=
  [simpleName "set", simpleName "set!", simpleName "modify",
   simpleName "write", simpleName "write?"]
def allocSuffixNames : List Lean.Name :=
  [simpleName "mkRef", simpleName "mkArray", simpleName "mkEmpty",
   simpleName "empty", simpleName "singleton", simpleName "push"]
def freeSuffixNames : List Lean.Name :=
  [simpleName "clear", simpleName "erase", simpleName "eraseD",
   simpleName "pop", simpleName "pop?", simpleName "reset",
   simpleName "shrink", simpleName "release", simpleName "dispose"]

def readSuffixes : List String := suffixesFromNames readSuffixNames
def writeSuffixes : List String := suffixesFromNames writeSuffixNames
def allocSuffixes : List String := suffixesFromNames allocSuffixNames
def freeSuffixes : List String := suffixesFromNames freeSuffixNames

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
            let qualified := if t.contains '.' then qualified + 1 else qualified
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
  let lineChars := line.toList
  let lowerChars := lower.toList
  let acc := if lineHasChars lowerChars todoNeedle then { acc with todo := acc.todo + 1 } else acc
  let acc := if lineHasChars lowerChars fixmeNeedle then { acc with fixme := acc.fixme + 1 } else acc
  let acc := if lineHasChars lowerChars portingNoteNeedle then { acc with portingNote := acc.portingNote + 1 } else acc
  let acc := if lineHasChars lowerChars adaptationNoteNeedle then { acc with adaptationNote := acc.adaptationNote + 1 } else acc
  let acc := if lineHasChars lowerChars nolintNeedle then { acc with nolint := acc.nolint + 1 } else acc
  let hasBlockStart := lineHasChars lineChars blockStartNeedle
  let hasBlockEnd := lineHasChars lineChars blockEndNeedle
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
    let indent := indentLevelChars lineChars
    let keepIf := trimmed.startsWith "else"
    let ifStack := popWhile acc.ifStack (fun lvl => indent <= lvl && !keepIf)
    let loopStack := popWhile acc.loopStack (fun lvl => indent <= lvl)
    let acc := { acc with ifStack := ifStack, loopStack := loopStack }
    let tokens := tokenizeLine lineChars
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

@[inline] def metricMapFromScan (acc : ScanAcc) (buildTimeMs : Nat) : MetricMap :=
  Id.run do
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
