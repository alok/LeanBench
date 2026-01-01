namespace LeanBench

@[inline] def jsonEscape (s : String) : String :=
  s.foldl (fun acc c =>
    match c with
    | '"' => acc ++ "\\\""
    | '\\' => acc ++ "\\\\"
    | '\n' => acc ++ "\\n"
    | '\r' => acc ++ "\\r"
    | '\t' => acc ++ "\\t"
    | _ => acc.push c) ""

@[inline] def jsonString (s : String) : String :=
  "\"" ++ jsonEscape s ++ "\""

@[inline] def jsonArray (items : Array String) : String :=
  "[" ++ String.intercalate "," items.toList ++ "]"

end LeanBench
