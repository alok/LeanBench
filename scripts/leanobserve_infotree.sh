#!/usr/bin/env bash
set -euo pipefail

root="${1:-.}"
outdir="${2:-artifacts}"

mkdir -p "$outdir"

lake build leanobserve
lake env ./.lake/build/bin/leanobserve \
  --root "$root" \
  --out "$outdir/metrics.json" \
  --infotree
