# Changelog

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
