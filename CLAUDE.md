# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Package Overview

RXKCD is an R package (v2.0.2) that provides access to XKCD comics via the XKCD JSON API. v2.0.1 is on CRAN; v2.0.2 removes the `text2vec` dependency (CRAN notice 2026-09-30: `float`, pulled in via `text2vec -> rsparse`, is scheduled for archival).

## Common Commands

```r
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
```bash
R CMD build .
R CMD check RXKCD_*.tar.gz
```

## Architecture

Exported functions live in `R/getXKCD.R`. `R/embeddings.R` holds the pure internal helpers `tokenize_xkcd()` and `build_lsa_embeddings()` (TF-IDF + truncated SVD via the document Gram matrix; base R + `Matrix` only). Do not reintroduce `text2vec` or anything depending on `rsparse`/`float`.

**Public API (4 exported functions):**
- `getXKCD(which, display, html, saveImg)` — always hits the live XKCD API; `which` accepts `"current"`, `"random"`, or a comic number; returns a list with `num`, `title`, `date`, `img`, `alt`, `link`, `transcript`
- `updateConfig()` — smart incremental sync: connects to the local DuckDB, determines which comic IDs are missing (skipping the non-existent #404), downloads their metadata, and appends them in chunks of 100 to cap peak memory; rate-limited at 0.05s per request
- `searchXKCD(query)` — DuckDB full-text search (BM25) across `title`, `alt`, and `transcript`; requires `updateConfig()` to have been run first
- `similarXKCD(query, n)` — cosine similarity between the query's mean LSA word vector and the per-comic LSA embeddings; requires `updateConfig()` first

**Local database:**
- Stored at `~/.RXKCD/xkcd.duckdb` (DuckDB, auto-created by `updateConfig()`)
- Schema: `xkcd(num INTEGER PRIMARY KEY, title, date, alt, img, transcript)`
- Embeddings: `~/.RXKCD/word_vectors.rds` and `~/.RXKCD/embeddings.rds`, rebuilt by `updateConfig()`
- `getXKCD()` is fully independent of this database; `searchXKCD()`, `similarXKCD()` and `updateConfig()` use it

**Note on DuckDB `read_only`:** Pass it to `dbConnect()`, not `duckdb()` — `duckdb(read_only=TRUE)` defaults to in-memory and DuckDB rejects read-only on in-memory databases. Correct pattern: `DBI::dbConnect(duckdb::duckdb(), dbdir = path, read_only = TRUE)`.

## Documentation

Documentation is written as roxygen2 comments (`#'`) in `R/*.R`. After editing, run `roxygen2::roxygenise()` to regenerate `NAMESPACE` and `man/*.Rd`. The `NAMESPACE` file is auto-generated — do not edit it manually.

`.Rbuildignore` excludes `.claude/`, `CLAUDE.md`, and `RXKCD_*.tar.gz` from the built package.

## No Test Suite

There is no `tests/` directory. Testing is done manually via `devtools::load_all()`.
