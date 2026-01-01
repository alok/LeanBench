# LeanBench

Hyperfine-inspired benchmarking library + CLI for Lean projects (no compiler changes).

## Quick start

Create a bench module and import it from your `Main.lean`:

```lean
import LeanBench

bench "sum" do
  let mut acc := 0
  for i in [0:100000] do
    acc := acc + i
  if acc == 0 then
    IO.println ""

def main (args : List String) : IO UInt32 :=
  LeanBench.runMain args
```

Build and run:

```sh
lake build
lake exe leanbench --samples 20 --warmup 2
```

## CLI

- `--list` list benches
- `--list-tags` list tags (after filters)
- `--list-suites` list suites (after filters)
- `--match <substr>` filter by name substring
- `--tags <t1,t2,...>` filter by any tag
- `--all-tags` require all tags in `--tags`
- `--suite <name>` filter by suite
- `--samples <n>` override sample count
- `--warmup <n>` override warmup count
- `--min-time-ms <n>` run until total time exceeds N ms
- `--group-by suite` group pretty output by suite
- `--format pretty|full|json|radar` (`full` adds median/p95/p99)
- `--json` alias for `--format json`
- `--radar` alias for `--format radar`
- `--radar-suite <name>` prefix radar names with `<name>//` (or set `LEANBENCH_RADAR_SUITE`)
- `--radar-out <path>` or `RADAR_JSONL=/path` for JSONL
- `--json-out <path>` write JSON to file
- `--save <path>` write JSON to file and set format to json
- `--compare <path>` compare against a JSON baseline

## Suites

Attach a suite directly in `BenchConfig`, or define a suite driver that
collects benches by tag or name:

```lean
import LeanBench

bench_suite "text" ({
  description := "string + json benches"
  tags := ["string", "json"]
})
```

## Lake integration

Lake already supports `bench` drivers (like `test`). Configure your package to
use LeanBench as the driver:

```toml
benchDriver = "LeanBench/leanbench"
```

Then run:

```sh
lake bench -- --tags array
```

## External commands

Use `benchCmd` to benchmark external commands:

```lean
bench "lean --version" do
  LeanBench.benchCmd "lean" #["--version"]
```

## Comparison example

```sh
# create baseline
lake exe leanbench --save baseline.json

# compare current run to baseline
lake exe leanbench --compare baseline.json
```
