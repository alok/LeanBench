#!/usr/bin/env bash
set -euo pipefail

root="${1:-.}"
out_dir="${2:-artifacts}"
mode="${3:-infotree}"
serve="${OBSERVE_SERVE:-1}"

mkdir -p "$out_dir"

case "$mode" in
  profile)
    ./scripts/leanobserve_profile.sh "$root" "$out_dir" \
      --report-json "$out_dir/report.json" \
      --report-md "$out_dir/report.md" \
      --report-top 40
    ;;
  infotree)
    ./scripts/leanobserve_infotree.sh "$root" "$out_dir" \
      --report-json "$out_dir/report.json" \
      --report-md "$out_dir/report.md" \
      --report-top 40
    ;;
  *)
    echo "unknown mode: $mode (use 'infotree' or 'profile')" >&2
    exit 1
    ;;
esac

if [ "$serve" = "1" ]; then
  node scripts/observe_server.mjs --root "$(pwd)" --port 8765
else
  echo "report: $out_dir/report.md"
  echo "report json: $out_dir/report.json"
  echo "open: http://localhost:8765/visualizer/index.html?data=/artifacts/metrics.json&root=$(pwd)"
fi
