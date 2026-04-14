## RXKCD

__Authors:__ Paolo Sonego, Mikko Korpela<br/>
__License:__ [GPL-2.0](https://opensource.org/licenses/GPL-2.0)<br/>
__Version:__ 2.0.1<br/>
__Status:__ Stable

[![rstudio mirror downloads](http://cranlogs.r-pkg.org/badges/RXKCD)](https://github.com/r-hub/cranlogs.app)
[![rstudio mirror downloads grand-total](http://cranlogs.r-pkg.org/badges/grand-total/RXKCD)](https://github.com/r-hub/cranlogs.app)

### Description

Visualize your favorite XKCD comic strip directly from R. Includes live archive searching with full-text BM25 ranking and semantic similarity search via local GloVe embeddings — all powered by a local DuckDB cache, no external API required.

### Installation

The CRAN version can be retrieved with:

```r
install.packages("RXKCD")
```

The latest version (v2.0.1) can be obtained via:

```r
# install.packages("pak")
pak::pak("onertipaday/RXKCD")
```

### Usage

```r
library(RXKCD)

# First run: downloads all comic metadata, builds FTS index and GloVe embeddings
# Subsequent runs: only syncs new comics
updateConfig()

# Fetch and display a comic
getXKCD("current")       # latest comic
getXKCD("random")        # random comic
getXKCD(353)             # specific comic by number

# Keyword search with BM25 ranking (results ordered by relevance)
searchXKCD("python")
searchXKCD("significant")

# Semantic similarity search using GloVe embeddings
similarXKCD("feeling lonely")
similarXKCD("space exploration", n = 10)
```

### Functions

| Function | Description |
|----------|-------------|
| `getXKCD(which, display, html, saveImg)` | Fetch a comic from the live XKCD API. `which` accepts `"current"`, `"random"`, or a comic number. |
| `updateConfig()` | Sync local DuckDB cache with new comics, rebuild the FTS index, and retrain GloVe embeddings. |
| `searchXKCD(query)` | Full-text search across title, alt text, and transcript using BM25 ranking. Returns results with a relevance `score`. |
| `similarXKCD(query, n = 5)` | Semantic similarity search using local GloVe embeddings. Returns top `n` results with a `similarity` score (0–1). |
