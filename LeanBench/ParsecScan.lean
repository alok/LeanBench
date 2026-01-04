import Std
import Std.Internal.Parsec
import LeanBench.TextScan

open Std.Internal.Parsec
open Std.Internal.Parsec.String

namespace LeanBench.ParsecScan

@[inline] def wordChar (c : Char) : Bool :=
  isWordChar c

@[inline] def wordToken : Parser String :=
  many1Chars (satisfy wordChar)

@[inline] def punctToken : Parser String :=
  attempt (pstring ":=")
    <|> attempt (pstring "<-")
    <|> attempt (pstring "->")
    <|> (String.singleton <$> any)

@[inline] def token : Parser String :=
  wordToken <|> punctToken

@[inline] def tokens : Parser (Array String) :=
  ws *> many (token <* ws)

@[inline] def tokenizeLineParsec (line : String) : List String :=
  match Parser.run tokens line with
  | .ok ts => ts.toList
  | .error _ => tokenizeLine line.toList

end LeanBench.ParsecScan
