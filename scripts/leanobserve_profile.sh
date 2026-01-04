#!/usr/bin/env bash
set -euo pipefail

root="${1:-.}"
out_dir="${2:-artifacts}"
shift 2 || true

mkdir -p "$out_dir"

profile_json="$out_dir/trace-profiler.json"
build_log="$out_dir/build.log"
metrics_json="$out_dir/metrics.json"

lakefile_src="$(pwd)/lakefile.toml"
tmp_lakefile="$out_dir/lakefile.profile.toml"
cp "$lakefile_src" "$tmp_lakefile"
cat >> "$tmp_lakefile" <<EOF2

moreLeanArgs = [
  "-Dtrace.profiler=true",
  "-Dtrace.profiler.useHeartbeats=true",
  "-Dtrace.profiler.output=$profile_json"
]
EOF2

lake --file "$tmp_lakefile" build --rehash --no-cache -v 2>&1 | tee "$build_log"

./.lake/build/bin/leanobserve \
  --root "$root" \
  --build-log "$build_log" \
  --profile-json "$profile_json" \
  --out "$metrics_json" \
  "$@"

echo "wrote $metrics_json"
