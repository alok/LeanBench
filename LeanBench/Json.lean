namespace LeanBench

@[inline] def jsonEscape (s : String) : String := Id.run do
  let mut chars : Array Char := #[]
  for c in s.toList do
    match c with
    | '"' =>
        chars := chars.push '\\'
        chars := chars.push '"'
    | '\\' =>
        chars := chars.push '\\'
        chars := chars.push '\\'
    | '\n' =>
        chars := chars.push '\\'
        chars := chars.push 'n'
    | '\r' =>
        chars := chars.push '\\'
        chars := chars.push 'r'
    | '\t' =>
        chars := chars.push '\\'
        chars := chars.push 't'
    | _ =>
        chars := chars.push c
  return String.ofList chars.toList

@[inline] def jsonString (s : String) : String :=
  "\"" ++ jsonEscape s ++ "\""

@[inline] def jsonArray (items : Array String) : String :=
  "[" ++ String.intercalate "," items.toList ++ "]"

end LeanBench
