import LeanBench

open LeanBench

bench_suite "core"
bench_suite "list"
bench_suite "array"
bench_suite "hashmap"
bench_suite "string"
bench_suite "json"
bench_suite "leanbench" ({
  description := "LeanBench internal rendering/JSON/plan benches"
  tags := ["leanbench", "internal"]
})

bench_suite "text" ({
  description := "string + json benches"
  tags := ["string", "json"]
})
