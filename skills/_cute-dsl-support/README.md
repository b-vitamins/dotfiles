# CuTe DSL skill suite tooling

Maintainer tooling for the `cute-dsl-*` skills; it is not deployed as a skill.

`sync_cute_dsl_suite.py` copies each skill's selected NVIDIA docs and examples
from a cute-docs checkout into its `references/`, writes `references/catalog.md`,
and records every skill's selection and source snapshot in
`suite-coverage.json`. The hand-written `SKILL.md`, `agents/`, and skill-specific
scripts are left alone.

Skills that build or run kernels also receive copies of the shared environment
files kept here, so they assume no particular package manager:

| Source here | Copied to |
|---|---|
| `check_env.py` | `scripts/check_env.py` - stdlib-only readiness probe |
| `environment.md` | `references/environment.md` - requirements and setup recipes |
| `guix-manifest.scm` | `assets/guix-manifest.scm` - optional Guix recipe |

Edit these sources, not the copies, then resync. The documentation-only
`cute-dsl-doc-navigator` receives none of them.

## Refreshing

The cute-docs checkout defaults to `$CUTE_DOCS_ROOT`, or `cute-docs` next to
this repository; its `scripts/sync_to_dotfiles.py` wraps this script. The
navigator mirrors the whole `latest` snapshot:

```bash
python3 skills/_cute-dsl-support/sync_cute_dsl_suite.py --skill cute-dsl-doc-navigator
```

The other skills keep curated selections (`SKILL_COVERAGE`) that still name
4.4.1 documentation paths, so resync them from that snapshot until their
selections are moved to the current layout:

```bash
python3 skills/_cute-dsl-support/sync_cute_dsl_suite.py \
  --docs-root <cute-docs>/docs/cutlass/cute-dsl/4.4.1 \
  --skill cute-dsl-benchmarker --skill cute-dsl-debugger ...
```

Each run updates only the synced skills' entries in `suite-coverage.json`.
