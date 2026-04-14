# Search XKCD comics

Searches the local DuckDB cache using full-text search (BM25 ranking)
across comic titles, alt text, and transcripts. Results are ranked by
relevance. Supports stemming (e.g., "running" matches "run") and
stopword removal. Run
[`updateConfig()`](https://onertipaday.github.io/RXKCD/reference/updateConfig.md)
first to populate the database and build the search index.

## Usage

``` r
searchXKCD(query)
```

## Arguments

- query:

  A single string to search for in comic titles, alt text, and
  transcripts.

## Value

A data frame containing matching XKCD comics with columns `num`, `date`,
`title`, `alt`, and `score` (BM25 relevance), ordered by relevance. If
no matches are found, an empty data frame is returned invisibly, and a
message is printed. The function will stop with an error if the local
database or search index is not found.
