# Changelog

## RXKCD 2.0.2

- Removed the dependency on ‘text2vec’, which pulled in ‘rsparse’ and
  ‘float’ (scheduled for archival on CRAN).
  [`similarXKCD()`](https://onertipaday.github.io/RXKCD/reference/similarXKCD.md)
  now uses latent semantic analysis (TF-IDF + truncated SVD) computed
  with base R and ‘Matrix’ instead of ‘GloVe’ embeddings. The function
  signature and return value are unchanged.
- [`updateConfig()`](https://onertipaday.github.io/RXKCD/reference/updateConfig.md)
  now stores word vectors in `~/.RXKCD/word_vectors.rds`; run it once
  after upgrading to rebuild the embeddings.
- Query and corpus tokenization are now shared and strip punctuation.
- Fixed
  [`updateConfig()`](https://onertipaday.github.io/RXKCD/reference/updateConfig.md)
  failing on every second run while rebuilding the full-text search
  index, which also left
  [`searchXKCD()`](https://onertipaday.github.io/RXKCD/reference/searchXKCD.md)
  without an index.

## RXKCD 2.0.1

CRAN release: 2026-04-15

- Full-text search with BM25 ranking in
  [`searchXKCD()`](https://onertipaday.github.io/RXKCD/reference/searchXKCD.md)
  and semantic similarity search in
  [`similarXKCD()`](https://onertipaday.github.io/RXKCD/reference/similarXKCD.md),
  backed by a local ‘DuckDB’ cache.
