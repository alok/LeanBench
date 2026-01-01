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
- `--match <substr>` filter by name substring
- `--samples <n>` override sample count
- `--warmup <n>` override warmup count
- `--min-time-ms <n>` run until total time exceeds N ms
- `--format pretty|full|json|radar` (`full` adds median/p95/p99)
- `--json` alias for `--format json`
- `--radar` alias for `--format radar`
- `--radar-out <path>` or `RADAR_JSONL=/path` for JSONL
- `--json-out <path>` write JSON to file
- `--save <path>` write JSON to file and set format to json
- `--compare <path>` compare against a JSON baseline
- `--suite <name>` prefix radar names

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
