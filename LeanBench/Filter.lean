import Std
import LeanBench.Bench
import LeanBench.Plan

namespace LeanBench

/-- Minimal benchmark metadata needed for filtering. -/
structure BenchMeta where
  name : String
  id : String
  suite : Option String
  tags : List String
  deriving Inhabited

@[inline] def BenchMeta.ofBench (b : Bench) : BenchMeta :=
  { name := b.name
    id := benchId b
    suite := b.config.suite
    tags := b.config.tags }

inductive RegexTok where
  | char (c : Char)
  | any
  deriving Inhabited, BEq

inductive RegexQuant where
  | one
  | zeroOrOne
  | zeroOrMore
  | oneOrMore
  deriving Inhabited, BEq

structure RegexItem where
  tok : RegexTok
  quant : RegexQuant := .one
  deriving Inhabited, BEq

structure SimpleRegex where
  anchoredStart : Bool := false
  anchoredEnd : Bool := false
  items : Array RegexItem := #[]
  deriving Inhabited, BEq

@[inline] def charMatches (t : RegexTok) (c : Char) : Bool :=
  match t with
  | .any => true
  | .char d => c == d

@[inline] def listGet? (xs : List Char) (i : Nat) : Option Char :=
  match xs, i with
  | [], _ => none
  | x :: _, 0 => some x
  | _ :: xs, Nat.succ i => listGet? xs i

partial def SimpleRegex.matchesFrom (re : SimpleRegex) (s : List Char) (pos : Nat) : Bool :=
  let len := s.length
  let rec go (idx : Nat) (p : Nat) : Bool :=
    if hdone : idx >= re.items.size then
      if re.anchoredEnd then
        p == len
      else
        true
    else
      let item := re.items[idx]!
      match item.quant with
      | .one =>
          match listGet? s p with
          | none => false
          | some c => charMatches item.tok c && go (idx + 1) (p + 1)
      | .zeroOrOne =>
          go (idx + 1) p ||
            match listGet? s p with
            | none => false
            | some c => charMatches item.tok c && go (idx + 1) (p + 1)
      | .zeroOrMore =>
          go (idx + 1) p ||
            match listGet? s p with
            | none => false
            | some c => charMatches item.tok c && go idx (p + 1)
      | .oneOrMore =>
          match listGet? s p with
          | none => false
          | some c =>
              charMatches item.tok c && (go (idx + 1) (p + 1) || go idx (p + 1))
  go 0 pos

/-- Search semantics: unanchored patterns match anywhere. -/
@[inline] def SimpleRegex.matches (re : SimpleRegex) (s : String) : Bool :=
  let chars := s.toList
  if re.anchoredStart then
    re.matchesFrom chars 0
  else
    let rec loop (pos : Nat) (rest : List Char) : Bool :=
      if re.matchesFrom chars pos then
        true
      else
        match rest with
        | [] => false
        | _ :: tl => loop (pos + 1) tl
    loop 0 chars

@[inline] def isRegexMeta (c : Char) : Bool :=
  c == '.' || c == '*' || c == '?' || c == '+' || c == '^' || c == '$' || c == '\\'

def parseSimpleRegex (raw : String) : Except String SimpleRegex := Id.run do
  let cs := raw.toList
  let mut anchoredStart := false
  let mut anchoredEnd := false
  let mut i : Nat := 0
  if listGet? cs 0 == some '^' then
    anchoredStart := true
    i := i + 1

  let mut items : Array RegexItem := #[]
  let mut escaped := false
  while i < cs.length do
    let c := cs[i]!
    i := i + 1
    if !escaped && c == '\\' then
      escaped := true
      continue
    if !escaped && c == '$' && i == cs.length then
      anchoredEnd := true
      break
    let tok : RegexTok :=
      if !escaped && c == '.' then .any else .char c
    escaped := false
    let mut quant : RegexQuant := .one
    if i < cs.length then
      let q := cs[i]!
      if q == '*' then
        quant := .zeroOrMore
        i := i + 1
      else if q == '?' then
        quant := .zeroOrOne
        i := i + 1
      else if q == '+' then
        quant := .oneOrMore
        i := i + 1
    items := items.push { tok := tok, quant := quant }

  if escaped then
    return .error "unterminated escape sequence in /.../ pattern"
  return .ok { anchoredStart := anchoredStart, anchoredEnd := anchoredEnd, items := items }

inductive FilterExpr where
  | suite (name : String)
  | tag (name : String)
  | nameRe (re : SimpleRegex)
  | not (e : FilterExpr)
  | and (a b : FilterExpr)
  | or (a b : FilterExpr)
  deriving Inhabited, BEq

@[inline] def FilterExpr.matches (e : FilterExpr) (m : BenchMeta) : Bool :=
  match e with
  | .suite s => m.suite == some s
  | .tag t => m.tags.contains t
  | .nameRe re => re.matches m.name
  | .not x => !(x.matches m)
  | .and a b => a.matches m && b.matches m
  | .or a b => a.matches m || b.matches m

structure FilterParser where
  input : List Char
  idx : Nat := 0
  deriving Inhabited

@[inline] def FilterParser.curr? (p : FilterParser) : Option Char :=
  listGet? p.input p.idx

@[inline] def FilterParser.bump (p : FilterParser) (n : Nat := 1) : FilterParser :=
  { p with idx := p.idx + n }

@[inline] def FilterParser.skipWs (p : FilterParser) : FilterParser :=
  let rest := p.input.drop p.idx
  let rec wsLen (xs : List Char) : Nat :=
    match xs with
    | [] => 0
    | c :: cs =>
        if c == ' ' || c == '\n' || c == '\t' || c == '\r' then
          1 + wsLen cs
        else
          0
  let n := wsLen rest
  { p with idx := p.idx + n }

@[inline] def FilterParser.expect (p : FilterParser) (c : Char) : Except String FilterParser := do
  let p := p.skipWs
  match p.curr? with
  | some d =>
      if d == c then
        return p.bump 1
      else
        throw s!"expected '{c}', got '{d}'"
  | none =>
      throw s!"expected '{c}', got end of input"

@[inline] def isIdentChar (c : Char) : Bool :=
  ('a' <= c && c <= 'z') ||
  ('A' <= c && c <= 'Z') ||
  ('0' <= c && c <= '9') ||
  c == '_' || c == '-' || c == '/'

def FilterParser.parseIdent (p : FilterParser) : Except String (String × FilterParser) := do
  let p := p.skipWs
  let rest := p.input.drop p.idx
  let rec takeIdent (xs : List Char) : List Char :=
    match xs with
    | [] => []
    | c :: cs =>
        if isIdentChar c then
          c :: takeIdent cs
        else
          []
  let identChars := takeIdent rest
  if identChars.isEmpty then
    throw "expected identifier"
  let s := String.ofList identChars
  return (s, { p with idx := p.idx + identChars.length })

def FilterParser.parseQuoted (p : FilterParser) : Except String (String × FilterParser) := do
  let p := p.skipWs
  let some q := p.curr? | throw "expected quoted string"
  if q != '"' && q != '\'' then
    throw "expected quoted string"
  let mut out : Array Char := #[]
  let mut i := p.idx + 1
  let mut escaped := false
  while i < p.input.length do
    let c := p.input[i]!
    i := i + 1
    if escaped then
      out := out.push c
      escaped := false
      continue
    if c == '\\' then
      escaped := true
      continue
    if c == q then
      return (String.ofList out.toList, { p with idx := i })
    out := out.push c
  throw "unterminated quoted string"

def FilterParser.parseRegexLiteral (p : FilterParser) : Except String (String × FilterParser) := do
  let p := p.skipWs
  match p.curr? with
  | some '/' =>
      let mut out : Array Char := #[]
      let mut i := p.idx + 1
      let mut escaped := false
      while i < p.input.length do
        let c := p.input[i]!
        i := i + 1
        if escaped then
          out := out.push c
          escaped := false
          continue
        if c == '\\' then
          escaped := true
          continue
        if c == '/' then
          return (String.ofList out.toList, { p with idx := i })
        out := out.push c
      throw "unterminated /.../ pattern"
  | _ =>
      throw "expected /.../ pattern"

def FilterParser.parseArgString (p : FilterParser) : Except String (String × FilterParser) := do
  let p := p.skipWs
  match p.curr? with
  | some '"' => p.parseQuoted
  | some '\'' => p.parseQuoted
  | some '/' => p.parseRegexLiteral
  | _ => p.parseIdent

mutual
  partial def FilterParser.parseTerm (p : FilterParser) : Except String (FilterExpr × FilterParser) := do
    let p := p.skipWs
    match p.curr? with
    | some '(' =>
        let p ← p.expect '('
        let (e, p) ← p.parseOr
        let p ← p.expect ')'
        return (e, p)
    | _ =>
        let (head, p) ← p.parseIdent
        let p ← p.expect '('
        let (arg, p) ← p.parseArgString
        let p ← p.expect ')'
        match head with
        | "suite" => return (.suite arg, p)
        | "tag" => return (.tag arg, p)
        | "name" =>
            let re ←
              match parseSimpleRegex arg with
              | .ok re => pure re
              | .error err => throw err
            return (.nameRe re, p)
        | _ => throw s!"unknown predicate '{head}'"

  partial def FilterParser.parseNot (p : FilterParser) : Except String (FilterExpr × FilterParser) := do
    let p := p.skipWs
    match p.curr? with
    | some _ =>
        if (String.ofList (p.input.drop p.idx |>.take 3)) == "not" &&
            match listGet? p.input (p.idx + 3) with
            | some c => c == ' ' || c == '\t' || c == '\n' || c == '('
            | none => true then
          let p := p.bump 3
          let (e, p) ← p.parseNot
          return (.not e, p)
        else
          p.parseTerm
    | none => throw "unexpected end of input"

  partial def FilterParser.parseAnd (p : FilterParser) : Except String (FilterExpr × FilterParser) := do
    let (lhs, p) ← p.parseNot
    let mut lhs := lhs
    let mut p := p.skipWs
    while (String.ofList (p.input.drop p.idx |>.take 3)) == "and" &&
        match listGet? p.input (p.idx + 3) with
        | some c => c == ' ' || c == '\t' || c == '\n' || c == '('
        | none => true do
      p := p.bump 3
      let (rhs, p') ← p.parseNot
      lhs := .and lhs rhs
      p := p'.skipWs
    return (lhs, p)

  partial def FilterParser.parseOr (p : FilterParser) : Except String (FilterExpr × FilterParser) := do
    let (lhs, p) ← p.parseAnd
    let mut lhs := lhs
    let mut p := p.skipWs
    while (String.ofList (p.input.drop p.idx |>.take 2)) == "or" &&
        match listGet? p.input (p.idx + 2) with
        | some c => c == ' ' || c == '\t' || c == '\n' || c == '('
        | none => true do
      p := p.bump 2
      let (rhs, p') ← p.parseAnd
      lhs := .or lhs rhs
      p := p'.skipWs
    return (lhs, p)
end

@[inline] def parseFilterExpr (s : String) : Except String FilterExpr := do
  let p : FilterParser := { input := s.toList, idx := 0 }
  let (e, p) ← p.parseOr
  let p := p.skipWs
  if p.idx != p.input.length then
    throw s!"unexpected trailing input at offset {p.idx}"
  return e

end LeanBench
