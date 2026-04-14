# Find semantically similar XKCD comics

Uses pre-computed GloVe embeddings to find comics that are semantically
similar to the given query. Unlike
[`searchXKCD()`](https://onertipaday.github.io/RXKCD/reference/searchXKCD.md),
which matches keywords, this function captures meaning (e.g., "feeling
lonely" can match comics about isolation even if they don't contain the
word "lonely"). Run
[`updateConfig()`](https://onertipaday.github.io/RXKCD/reference/updateConfig.md)
first to build the embeddings.

## Usage

``` r
similarXKCD(query, n = 5L)
```

## Arguments

- query:

  A single string describing the topic or feeling to search for.

- n:

  A single integer specifying the number of results to return. Defaults
  to 5.

## Value

A data frame with columns `num`, `date`, `title`, `alt`, and
`similarity` (cosine similarity score, 0-1), ordered by similarity. If
no similar comics are found, an empty data frame is returned invisibly.
The function will stop with an error if the embeddings have not been
built.
