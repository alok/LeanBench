# LeanBench config

LeanBench can read default configuration and per-benchmark overrides from a TOML
file in the current directory:

- `leanbench.toml` (preferred)
- `observatory.toml` (fallback; allows sharing profiles with LeanObserve)

Use `--config <path>` (or `LEANBENCH_CONFIG=/path`) to point at a specific file.

LeanBench only reads the `[profile.*]` tables and ignores other top-level keys.

## Profiles

Profiles live under `[profile.<name>]` and can inherit from one another:

```toml
[profile.default]
warmup = 2
samples = 10

[profile.ci]
inherits = "default"
samples = 5
retries = 1
```

Inheritance applies first, then the profile's own keys override the parent.

Supported profile keys:

- `inherits = "<parent>"`
- `select = "<filter expr>"` (optional bench selector)
- `warmup = <nat>`
- `samples = <nat>`
- `min-time-ms = <nat>`
- `threads = <nat>`
- `timeout-ms = <nat>` (metadata only)
- `retries = <nat>`
- `priority = <nat>` (metadata only)
- `group = "<string>"` (metadata only)
- `threads-required = <nat>` (metadata only)

## Filter DSL (`select` / `where`)

LeanBench uses a small filter expression DSL in two places:

- `--where "<expr>"` / `LEANBENCH_WHERE="<expr>"` (bench selection)
- `select = "<expr>"` / `where = "<expr>"` in config profiles

Primitives:

- `suite(<name>)`
- `tag(<name>)`
- `name(/regex/)` (simple regex literal)

Composition:

- `and`, `or`, `not`
- parentheses: `( ... )`

Example:

```toml
[profile.default]
select = "suite(leanbench) and not tag(slow)"
```

## Per-benchmark overrides

Profiles can apply per-benchmark patches via `[[profile.<name>.overrides]]`.
Each override selects benches with `where = "<expr>"` (alias: `filter = "<expr>"`)
and then applies a config patch:

```toml
[profile.default]
warmup = 1
samples = 10

[[profile.default.overrides]]
where = "name(/json_escape/) or tag(json)"
samples = 3
group = "json"
```

Override patches apply after the profile keys, but before environment variables
and CLI flags.

## Precedence

Resolved `BenchConfig` uses this order (highest wins):

1. CLI flags (e.g. `--samples`, `--threads`)
2. Environment variables (e.g. `LEANBENCH_SAMPLES`, `LEANBENCH_THREADS`)
3. Matching `[[profile.<name>.overrides]]` patches (in file order)
4. `[profile.<name>]` patch (including inheritance)
5. The benchmark's registered `BenchConfig`

To see the resolved config inputs, use `--print-config` (or
`LEANBENCH_PRINT_CONFIG=1`). LeanBench prints a small JSON blob to stderr
describing:

- the config file path (if any)
- selected profile name
- `--where` expression (if any)
- patch sources (profile/env/cli)
- number of profile overrides

## Environment variables

Selectors:

- `LEANBENCH_CONFIG`
- `LEANBENCH_PROFILE`
- `LEANBENCH_WHERE`
- `LEANBENCH_PRINT_CONFIG`

Bench selection:

- `LEANBENCH_SUITE` (equivalent to `--suite`)

Config patch keys:

- `LEANBENCH_WARMUP`
- `LEANBENCH_SAMPLES`
- `LEANBENCH_MIN_TIME_MS`
- `LEANBENCH_THREADS`
- `LEANBENCH_TIMEOUT_MS`
- `LEANBENCH_RETRIES`
- `LEANBENCH_PRIORITY`
- `LEANBENCH_GROUP`
- `LEANBENCH_THREADS_REQUIRED`
