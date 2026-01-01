import Std
import Lean.Data.Json
import LeanBench

open LeanBench

bench "sum" do
  let mut acc := 0
  for i in [0:100000] do
    acc := acc + i
  if acc == 0 then
    IO.println ""

bench "list-fold" do
  let xs := List.range 100000
  let mut acc := 0
  for x in xs do
    acc := acc + x
  if acc == 0 then
    IO.println ""

bench "list-map-filter" do
  let xs := List.range 50000
  let ys := xs.map (· + 1) |>.filter (fun x => x % 2 == 0)
  if ys.length == 0 then
    IO.println ""

bench "array-map" do
  let arr := (List.range 10000).toArray
  let arr := arr.map (· + 1)
  if arr.size == 0 then
    IO.println ""

bench "array-fold" do
  let arr := (List.range 100000).toArray
  let mut acc := 0
  for x in arr do
    acc := acc + x
  if acc == 0 then
    IO.println ""

bench "array-qsort" do
  let arr := (List.range 20000).reverse.toArray
  let arr := arr.qsort (· < ·)
  if arr.size == 0 then
    IO.println ""

bench "hashmap-insert" do
  let mut m : Std.HashMap Nat Nat := ∅
  for i in [0:20000] do
    m := m.insert i i
  if Std.HashMap.size m == 0 then
    IO.println ""

bench "hashmap-lookup" do
  let mut m : Std.HashMap Nat Nat := ∅
  for i in [0:20000] do
    m := m.insert i i
  let mut acc := 0
  for i in [0:20000] do
    acc := acc + Std.HashMap.getD m i 0
  if acc == 0 then
    IO.println ""

bench "string-concat" do
  let xs := List.replicate 20000 "x"
  let s := xs.foldl (fun acc x => acc ++ x) ""
  if s.length == 0 then
    IO.println ""

bench "string-split" do
  let s := String.intercalate "," (List.replicate 1000 "abc")
  let parts := s.splitOn ","
  if parts.length == 0 then
    IO.println ""

bench "json-parse" do
  let s :=
    "{" ++
    "\"items\":[" ++
    (String.intercalate "," (List.replicate 200 "{\\\"k\\\":42,\\\"v\\\":\\\"abc\\\"}")) ++
    "]}"
  let r := Lean.Json.parse s
  match r with
  | .error _ => IO.println ""
  | .ok _ => pure ()
