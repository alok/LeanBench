# LeanBench Artifacts

LeanBench can write "side channel" outputs under an artifacts directory via
`leanbench --artifacts <dir>`.

This is intended for CI and tooling integrations where you want:
- stable, path-addressable outputs (separate from stdout)
- large JSON data kept out of the main results payload

## Directory Layout

When `--artifacts <dir>` is set, LeanBench will write some or all of the
following files:

- `manifest.json`: run manifest (tool/version/host metadata)
- `results.json`: results when `--format json` is active (or `--json`)
- `results.jsonl`: results when `--format jsonl` is active (or `--jsonl`)
- `plan.json`: run plan when `--plan` is active
- `trace/*.json`: per-benchmark trace outputs (when the bench defines `trace?`)
- `sample_extras/*.json`: per-benchmark per-sample outputs (when the bench defines `reportSample?`)

The per-benchmark files are named using a deterministic file stem derived from
the benchmark ID. The exact mapping is an implementation detail, but the main
JSON output includes a `trace_path` and/or `sample_extras_path` that points at
the correct file relative to `<dir>`.

## Trace Artifact (`trace/*.json`)

If a benchmark defines `trace? : Option (IO Json)`, then:
- without `--artifacts`: the trace JSON is inlined as `trace` in the main result
- with `--artifacts`: the trace JSON is written to `trace/<bench>.json`, and the
  main result contains:
  - `trace: null`
  - `trace_path: "trace/<bench>.json"`

**Format:** The trace artifact file contains the raw JSON value produced by the
benchmark's `trace?` hook. LeanBench intentionally does not enforce a schema
here; downstream tooling should treat the JSON as opaque unless you control the
bench implementation.

Minimal example (recommended shape):

```json
{
  "schema_version": 1,
  "meta": {"backend": "metal"},
  "events": [
    {"phase": "compile", "start_ns": 0, "duration_ns": 123456}
  ]
}
```

## Sample Extras Artifact (`sample_extras/*.json`)

If a benchmark defines `reportSample? : Option (SampleCtx → IO Json)`, then the
hook is called once per recorded timing sample **only when** `--artifacts <dir>`
is set. The outputs are collected and written to:

- `sample_extras/<bench>.json`

The main result contains `sample_extras_path: "sample_extras/<bench>.json"`.

**Format:** The file is a single JSON object with:

- `schema_version` (currently `1`)
- `bench_id`: stable benchmark ID (suite-qualified)
- `bench_name`: the registered benchmark name
- `samples_ns`: array of recorded sample timings (nanoseconds)
- `sample_extras`: array of JSON values, aligned 1:1 with `samples_ns`

Schema: `docs/leanbench-sample-extras-schema.json`.

Example:

```json
{
  "schema_version": 1,
  "bench_id": "core/hooked",
  "bench_name": "hooked",
  "samples_ns": [1234, 1200, 1250],
  "sample_extras": [
    {"sample_index": 0, "elapsed_ns": 1234},
    {"sample_index": 1, "elapsed_ns": 1200},
    {"sample_index": 2, "elapsed_ns": 1250}
  ]
}
```

