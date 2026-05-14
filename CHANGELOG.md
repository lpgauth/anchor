# Changelog

## 0.3.7

### Added

- One telemetry event at the request boundary:

  | Event | Measurements | Metadata |
  |---|---|---|
  | `[anchor, request, sent]` | `count => 1` | `operation, async` |

  Fires from the two internal dispatch helpers — `call/2` and `cast/3`
  — so all 77 exported public functions are covered without
  per-arity instrumentation. The `operation` metadata is the
  memcached op (`get`, `set`, `add`, `replace`, `delete`, `flush`,
  `noop`, `quit`, `version`, `increment`, `decrement`), extracted
  from either an atom message or `element(1, Tuple)`. Attach
  handlers via `telemetry:attach/4`.

  Per-request shackle lifecycle (queue / send / receive) remains
  observable via shackle's own telemetry hooks — anchor's event
  surfaces the memcached-level intent without duplicating that work.

  No `error` event yet: all error paths flow through shackle (e.g.
  `no_server`, `pool_not_started`) and are already covered by
  shackle's telemetry. anchor itself has no separate routing layer
  to fail.

- `telemetry` (1.4.2) is now a direct dependency (was already
  transitively present via shackle).

No source or API changes beyond the instrumentation.

## 0.3.6

Pure infrastructure modernization. No source or API changes.

### Changed

- Bumped `shackle` from git ref `0.6.2` to hex `0.7.1` -- fixes the
  OTP 27+ build break inherited transitively via the old
  `granderl 0.1.5` pin in older shackle releases.
- CI moved from Travis (decommissioned years ago) to GitHub Actions.
  Matrix now covers OTP 25, 26, 27, 28.
- Documentation migrated from `edown` to `rebar3_ex_doc`.

### Removed

- `.travis.yml`, `elvis.config`, `bin/elvis`, `rebar.config.script`
  (rebar2 compatibility) -- all unused.
- `coveralls` plugin and Makefile target -- Travis-specific tooling.
