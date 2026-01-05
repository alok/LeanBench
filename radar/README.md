# Radar (example configs)

This directory contains **example** configs for `github.com/leanprover/radar` to
run LeanBench benches via `scripts/radar/leanbench.sh`.

## Quick start (local)

1. Copy the example configs and edit URLs/tokens:

```bash
cp radar/config-server.example.yaml radar/config-server.yaml
cp radar/config-runner.example.yaml radar/config-runner.yaml
```

2. Update `repo`/`bench` URLs in `radar/config-server.yaml` to point at the repo you want to benchmark.

3. Start radar server + runner (from a radar checkout):

```bash
# server
java -jar radar/target/radar-*.jar server /path/to/LeanBench/radar/config-server.yaml

# runner
java -jar radar/target/radar-*.jar runner /path/to/LeanBench/radar/config-runner.yaml
```

The bench script will clone the target repo and invoke:

```bash
lake exe leanbench --radar --radar-out <jsonl>
```

You can pass extra bench args via `LEANBENCH_ARGS`, e.g.

```bash
LEANBENCH_ARGS="--suite leanbench --samples 5" \
  scripts/radar/leanbench.sh /path/to/LeanBench /tmp/radar.jsonl
```
