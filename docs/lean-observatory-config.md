# Lean Observatory config

LeanObserve reads configuration from `observatory.toml` (preferred) or `leanbench.toml`
in the current directory. Use `--config` to point at a specific file.

## Profiles

Profiles live under `[profile.<name>]` and can inherit from one another:

```
[profile.default]
infotree = true
command-nodes = true

[profile.ci]
inherits = "default"
infotree-jobs = 2
```

Inheritance applies first, then the profile's own keys override the parent.

## Tool overrides

Tool-specific overrides live under `[tool.<id>]` and apply after the profile
so you can layer profiler-specific settings without duplicating the profile:

```
[tool.perf]
perf = "artifacts/perf.txt"
```

Select the tool with `--tool perf` or `LEANOBSERVATORY_TOOL=perf`.

## Precedence

Resolved configuration uses this order (highest wins):

1. CLI flags
2. Environment variables
3. `[tool.<id>]` overrides
4. `[profile.<name>]` (including inheritance)
5. Built-in defaults

Use `--print-config` (or `LEANOBSERVATORY_PRINT_CONFIG=1`) to print the resolved
config before metrics are collected.

## Environment variables

Common selectors:
- `LEANOBSERVATORY_CONFIG`
- `LEANOBSERVATORY_PROFILE`
- `LEANOBSERVATORY_TOOL`

Every CLI flag has a matching `LEANOBSERVATORY_...` variable, e.g.
`LEANOBSERVATORY_INFOTREE=1`, `LEANOBSERVATORY_MAX_FILES=200`,
`LEANOBSERVATORY_TRACY=artifacts/tracy.csv`.
