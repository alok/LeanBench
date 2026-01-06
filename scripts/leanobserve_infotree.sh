#!/usr/bin/env bash
set -euo pipefail

root="${1:-.}"
outdir="${2:-artifacts}"
shift 2 || true

mkdir -p "$outdir"

entries_json="$outdir/entries.json"

lake build leanobserve
lake env ./.lake/build/bin/leanobserve \
  --root "$root" \
  --out "$outdir/metrics.json" \
  --entries-out "$entries_json" \
  --infotree \
  "$@"
