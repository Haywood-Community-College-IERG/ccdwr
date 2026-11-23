# ccdwr 1.1.1

## New features

`getColleagueData_SQL()` can run queries on the DuckDB mirror. It has the same
`source` argument as `getColleagueData()` ("auto", "server", "mirror"). On the
mirror, the T-SQL query is translated to DuckDB SQL with the Python package
sqlglot, and the `CCDW_HIST.` / `CCDW.` database prefixes are removed (set
`duckdb: databases:` to change these). Use `translate = FALSE` for queries
already written for DuckDB.

New `translateSQL_DuckDB()` returns the translated query. It requires Python
with sqlglot (`pip install sqlglot`); set `duckdb: python:` in the
configuration file to choose the Python executable.

## Changes

`getCfg()`: the `ccdw.cfg.full_path`, `ccdw.cfg.path`, and `ccdw.cfg.fn`
options were never used, because unset environment variables were treated
as set. The lookup order is now: `./config.yml`, `CCDW_CFG_FULL_PATH`,
option `ccdw.cfg.full_path`, `CCDW_CFG_PATH`/`CCDW_CFG_FN`, options
`ccdw.cfg.path`/`ccdw.cfg.fn`, then `./config.yml`. A configuration that
redirects with `config: location:` now keeps its file name.

The mirror freshness check reads the build time from `_extract.info` (used by
current mirror builds), falling back to `meta._mirror_info`. Before this, a
current mirror always looked stale and every call went to the server.

# ccdwr 1.0.10

## New features

None.

## Changes

Fixed error setting schema and version to defaults.

# ccdwr 1.0.9

## New features

None.

## Changes

Convert some is.na checks to purrr::pluck calls.

# ccdwr 1.0.8

## New features

None.

## Changes

Added package cli and used for information, warning, and abort conditions.

# ccdwr 1.0.7

## New features

Added checks for use of Core Technologies tools.

## Changes

Added database option to getColleagueData to allow referencing different databases.

# ccdwr 1.0.6

## New features

Added checks for use of Core Technologies tools.

## Changes

Changed all references of HAYWOODCC to CCDWR.
Added code to check for use of CCDW source.

# ccdwr 1.0.5

## New features

None.

## Changes

More fixes to check on a list variable for NAs to return one TRUE/FALSE

# ccdwr 1.0.4

## New features

None.

## Changes

Fixed check on a list variable for NAs to return one TRUE/FALSE

# ccdwr 1.0.3

## New features

Moved all files related to Colleague data from haywoodcc package to ccdwr

## Changes

None.
