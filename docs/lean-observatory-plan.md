# Lean Observatory: Proposal + Plan

## Summary
Lean Observatory is a compile-time and elaboration-time observability pipeline for Lean projects, inspired by Jonathan Blow's program visualizer demo. It exports a stable, versioned metrics artifact (tree JSON/Parquet) and renders it in an interactive UI (treemap + timeline + traces) with deep links back to source.

The emphasis is on:
- static, reproducible artifacts (can be versioned in the repo)
- treemap UX (size + color metrics with relative normalization)
- fast navigation to code locations
- extensible metric collection (cheap regex metrics through deep InfoTree/profiler data)

## Goals
- Produce a stable metrics artifact for any Lean project
- Provide an interactive treemap view with selectable metrics
- Make the collector pipeline incremental and cheap by default
- Support deep linking into editors (VSCode, nvim, emacs)

## Non-goals (initial)
- Full semantic indexing of all declarations (unless performance allows)
- Tight IDE integration beyond deep links
- A single "correct" quality score (metrics are exploratory)

## Architecture (high level)
1) Collectors
   - cheap scan: LOC, comments, notes, TODOs, regex metrics
   - build timing: parse `lake build -v` output
   - elaboration metrics: InfoTree summaries, heartbeats, profiler

2) Aggregator
   - merge all collector outputs into a project tree
   - apply metric-specific aggregation rules
   - emit schema-versioned JSON (and optional Parquet)

3) Visualizer
   - treemap: size metric + color metric
   - filters: by namespace/module/threshold
   - deep link on click
   - additional views: build timeline, flame graph, dependency graph

## Extensibility / Plugins
- Metrics registry: collectors provide a `metrics` spec list (label, unit, kind, default blend weight).
- Collector registry: each collector registers itself; the CLI discovers and merges outputs.
- External metrics: design for optional merge files (entries keyed by file path) so non-Lean tooling can plug in.
- UI reads specs from the JSON artifact, so new metrics appear without frontend changes.

## Compiler Hook Path (planned)
- Short term: run `lake build` with `-Dtrace.profiler=true` and `-Dtrace.profiler.useHeartbeats=true`,
  capture `trace.profiler.output` (Firefox profiler JSON), and aggregate per-file heartbeats by
  distributing total profiler weight across files using build times.
- Mid term: instrument the frontend with an InfoTree visitor to collect decl-level metrics at compile time.
- Long term: use compiler internals (Environment + Kernel) for precise dependency and term-size metrics.

### Profiler run (current)
```
LEANFLAGS="-Dtrace.profiler=true -Dtrace.profiler.useHeartbeats=true -Dtrace.profiler.output=artifacts/trace-profiler.json" \\
  lake build -v 2>&1 | tee artifacts/build.log

./.lake/build/bin/leanobserve \\
  --root . \\
  --build-log artifacts/build.log \\
  --profile-json artifacts/trace-profiler.json \\
  --out artifacts/metrics.json
```
Convenience wrapper:
```
./scripts/leanobserve_profile.sh . artifacts
```
Notes:
- `profile_weight` is currently distributed across files in proportion to `build_time_ms`.
- For more precise attribution, we’ll move to InfoTree and compiler hooks in the next phase.

## Data Model (schema sketch)
Each node is a module/command/decl with metrics and source span.

Example (conceptual):
```
{
  "id": "Mathlib/Algebra/Group/Defs.lean#mul_assoc",
  "kind": "decl",
  "name": "mul_assoc",
  "path": "Mathlib/Algebra/Group/Defs.lean",
  "span": { "file": "...", "line": 123, "col": 4 },
  "children": [],
  "metrics": {
    "loc": 18,
    "expr_nodes": 290,
    "infotree_nodes": 1200,
    "elab_time_ms": 12.3,
    "heartbeats": 145000,
    "tactic_calls": 24,
    "simp_steps": 300,
    "tc_instances": 88,
    "global_reads": 10,
    "global_writes": 2,
    "if_density": 0.12,
    "loop_depth_max": 3,
    "alloc_calls": 2
  },
  "tags": ["theorem", "simp"]
}
```

Aggregation rules:
- sum: size, counts, time, heartbeats
- max: maximum nesting / depth
- weighted mean: densities, ratios

## Metrics (initial list)
- Structural: LOC, tokens, AST node count, InfoTree node count
- Performance: elaboration time, heartbeats, profiler samples
- Tactics: count by tactic name, simp steps, instance search depth
- Complexity: max nested `by`/`have`, macro expansion counts
- Hygiene: nolint/porting/adaptation notes, TODO/FIXME counts
- Dependency: import fan-in/out, decl dependency fan-in/out

## Collection Strategy
Phase 0 (cheap):
- regex and file scans
- parse `lake build -v`

Phase 1 (InfoTree summary):
- export compact per-file summary (implemented in `leanobserve --infotree`)
- per-decl summary and cache by file hash (next)

Usage note:
- Run `leanobserve` under `lake env` so `LEAN_PATH` is set for imports, e.g.
  `lake env ./.lake/build/bin/leanobserve --root . --out artifacts/metrics.json --infotree`

Agent loop:
- `./scripts/observe_agent_loop.sh . artifacts infotree` generates metrics + report and starts a local server.
- `--command-nodes` emits per-command/decl nodes under each file (requires `--infotree`).

Phase 2 (profiling/tracing):
- enable `trace.profiler` globally (lakefile or wrapper)
- aggregate by decl and file

Phase 3 (semantic):
- decl dependency and kernel term sizes
- typeclass resolution metrics

## UI (MVP)
- Treemap view
  - size = expr_nodes or loc
  - color = selectable metric
  - relative normalization
- Tooltip: decl name, path, key metrics
- Click: deep link to editor (`vscode://file/...:line:col`)

## Milestones
M1: Schema + CLI skeleton + cheap collectors + treemap UI
M2: InfoTree summary export + incremental cache
M3: Profiler integration + flame graph + diff view

## Risks / Constraints
- InfoTree data is massive: must summarize
- Per-decl attribution can be fuzzy; spans needed
- Performance budget: default mode should be fast

## Open Questions
- Where will the repository live? (standalone vs. inside an existing tool)
- Preferred UI stack (web, Tauri, VSCode extension)?
- Should mathlib be a special-case profile in v1?
