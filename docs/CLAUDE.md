# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working
with code in this repository.

## Package Overview

RXKCD is an R package (v2.0.0) that provides access to XKCD comics via
the XKCD JSON API. v1.x is on CRAN; v2.0.0 is the current development
version. `R CMD check` passes clean (`Status: OK`) on this branch.

## Common Commands

``` r
# Load package during development
devtools::load_all()

# Regenerate NAMESPACE and man/*.Rd from roxygen2 comments
roxygen2::roxygenise()

# Run R CMD CHECK
devtools::check()

# Build the package
devtools::build()

# Rebuild pkgdown documentation site (outputs to docs/)
pkgdown::build_site()
```

From the shell:

``` bash
R CMD build .
R CMD check RXKCD_*.tar.gz
```

## Architecture

All source code lives in a single file: `R/getXKCD.R`. There are no
internal helper functions — the three exported functions use `httr`,
`jsonlite`, `DBI`, and `duckdb` directly.

**Public API (3 exported functions):** -
`getXKCD(which, display, html, saveImg)` — always hits the live XKCD
API; `which` accepts `"current"`, `"random"`, or a comic number; returns
a list with `num`, `title`, `date`, `img`, `alt`, `link`, `transcript` -
[`updateConfig()`](https://onertipaday.github.io/RXKCD/reference/updateConfig.md)
— smart incremental sync: connects to the local DuckDB, determines which
comic IDs are missing (skipping the non-existent \#404), downloads their
metadata, and appends them in chunks of 100 to cap peak memory;
rate-limited at 0.05s per request - `searchXKCD(query)` — queries the
local DuckDB with SQL `ILIKE` across `title`, `alt`, and `transcript`
fields; requires
[`updateConfig()`](https://onertipaday.github.io/RXKCD/reference/updateConfig.md)
to have been run first

**Local database:** - Stored at `~/.RXKCD/xkcd.duckdb` (DuckDB,
auto-created by
[`updateConfig()`](https://onertipaday.github.io/RXKCD/reference/updateConfig.md)) -
Schema:
`xkcd(num INTEGER PRIMARY KEY, title, date, alt, img, transcript)` -
[`getXKCD()`](https://onertipaday.github.io/RXKCD/reference/getXKCD.md)
is fully independent of this database; only
[`searchXKCD()`](https://onertipaday.github.io/RXKCD/reference/searchXKCD.md)
and
[`updateConfig()`](https://onertipaday.github.io/RXKCD/reference/updateConfig.md)
use it

**Note on DuckDB `read_only`:** Pass it to `dbConnect()`, not `duckdb()`
— `duckdb(read_only=TRUE)` defaults to in-memory and DuckDB rejects
read-only on in-memory databases. Correct pattern:
`DBI::dbConnect(duckdb::duckdb(), dbdir = path, read_only = TRUE)`.

## Documentation

Documentation is written as roxygen2 comments (`#'`) in `R/getXKCD.R`.
After editing, run
[`roxygen2::roxygenise()`](https://roxygen2.r-lib.org/reference/roxygenize.html)
to regenerate `NAMESPACE` and `man/*.Rd`. The `NAMESPACE` file is
auto-generated — do not edit it manually.

`.Rbuildignore` excludes `.claude/`, `CLAUDE.md`, and `RXKCD_*.tar.gz`
from the built package.

## No Test Suite

There is no `tests/` directory. Testing is done manually via
[`devtools::load_all()`](https://devtools.r-lib.org/reference/load_all.html).
