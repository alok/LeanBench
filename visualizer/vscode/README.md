# Lean Observatory VSCode Extension

Highlights Lean source ranges by a blended "hotness" score computed from `spans.json` (produced by `leanobserve --spans-out`).

## Quick start

1. Generate spans:
   ```
   lake exe leanobserve -- --root . --out artifacts/metrics.json --spans-out artifacts/spans.json
   ```
2. In VSCode, set settings:
   ```json
   {
     "leanObservatory.spansJsonPath": "artifacts/spans.json",
     "leanObservatory.metricsJsonPath": "artifacts/metrics.json",
     "leanObservatory.weights": {
       "profile_weight": 1.0,
       "heap_allocs_heuristic": 0.3
     }
   }
   ```
3. Run **Lean Observatory: Reload Spans** from the command palette.

## Settings

- `leanObservatory.spansJsonPath`: path to `spans.json` (absolute or workspace-relative)
- `leanObservatory.metricsJsonPath`: path to `metrics.json` (optional; used for labels/units)
- `leanObservatory.weights`: map of metric key → weight (linear combination)
- `leanObservatory.normalize`: `max` (default) or `minmax`
- `leanObservatory.includeKinds`: e.g. `["decl", "command"]`
- `leanObservatory.minScore`: minimum normalized score to show
- `leanObservatory.palette`: `heat`, `blueOrange`, or `viridis`
- `leanObservatory.showLegend`: show palette status bar item + legend command
- `leanObservatory.maxDecorationsPerFile`: performance cap
- `leanObservatory.showHover`: show metric breakdown on hover

## Notes

- Metric keys come from the JSON (e.g. `profile_weight`, `if_points`, `heap_allocs_heuristic`).
- The extension watches the spans file and reloads on changes.
- Use **Lean Observatory: Edit Weights** to update weights from the command palette.
- Use **Lean Observatory: Show Legend** to see the palette scale.
