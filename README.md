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

## Install

Add LeanBench as a Lake dependency:

```toml
[[require]]
name = "LeanBench"
git = "https://github.com/alok/LeanBench"
rev = "main"
```

For local development, you can use a path dependency instead:

```toml
[[require]]
name = "LeanBench"
path = "../LeanBench"
```

Then set the bench driver (optional, enables `lake bench`):

```toml
benchDriver = "LeanBench/leanbench"
```

## CLI

- `--list` list benches
- `--list --format json` machine-readable bench list
- `--list-tags` list tags (after filters)
- `--list-suites` list suites (after filters)
- `--plan` emit deterministic plan JSON and exit
- `--match <substr>` filter by name substring
- `--tags <t1,t2,...>` filter by any tag
- `--all-tags` require all tags in `--tags`
- `--suite <name>` filter by suite
- `--samples <n>` override sample count
- `--warmup <n>` override warmup count
- `--min-time-ms <n>` run until total time exceeds N ms
- `--seed <n>` seed for deterministic shuffling
- `--shuffle` shuffle bench order deterministically
- `--partition count:m/n|hash:m/n` shard benches across workers
- `--plan-out <path>` write plan JSON to file
- `--manifest-out <path>` write run manifest JSON to file
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

## GPU metrics

Provide per-iteration byte and flop counts in `BenchConfig` to emit
bandwidth (GB/s) and throughput (GFLOP/s) in pretty/JSON output:

```lean
import LeanBench

def cfg : BenchConfig := {
  bytes := some (3 * 1_000_000 * 4)  -- read 2 + write 1 float arrays
  flops := some 1_000_000           -- one add per element
}

bench "vector_add" (cfg) do
  -- run kernel
  pure ()
```

## Item throughput

Provide per-iteration item counts to emit `items/s`:

```lean
import LeanBench

def cfg : BenchConfig := {
  items := some 1024
}

bench "batch_next" (cfg) do
  -- run one batch
  pure ()
```

## Extras

Attach additional JSON metrics (e.g., stage timing) with `bench_report`:

```lean
import LeanBench
import Lean.Data.Json

def reportStats : IO Lean.Json := do
  pure <| Lean.Json.mkObj [("stage_wait_ns", Lean.Json.num 12345)]

bench_report "pipeline" (reportStats) do
  -- run one pipeline step
  pure ()
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

## License

Apache-2.0
