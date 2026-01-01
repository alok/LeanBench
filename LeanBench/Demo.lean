import LeanBench

open LeanBench

bench "sum" do
  let mut acc := 0
  for i in [0:100000] do
    acc := acc + i
  if acc == 0 then
    IO.println ""

bench "array-map" do
  let arr := (List.range 10000).toArray
  let arr := arr.map (· + 1)
  if arr.size == 0 then
    IO.println ""

bench "string-split" do
  let s := String.intercalate "," (List.replicate 1000 "abc")
  let parts := s.splitOn ","
  if parts.length == 0 then
    IO.println ""
