# RXKCD 2.0.2

* Removed the dependency on 'text2vec', which pulled in 'rsparse' and 'float'
  (scheduled for archival on CRAN). `similarXKCD()` now uses latent semantic
  analysis (TF-IDF + truncated SVD) computed with base R and 'Matrix' instead
  of 'GloVe' embeddings. The function signature and return value are unchanged.
* `updateConfig()` now stores word vectors in `~/.RXKCD/word_vectors.rds`;
  run it once after upgrading to rebuild the embeddings.
* Query and corpus tokenization are now shared and strip punctuation.
* Fixed `updateConfig()` failing on every second run while rebuilding the
  full-text search index, which also left `searchXKCD()` without an index.

# RXKCD 2.0.1

* Full-text search with BM25 ranking in `searchXKCD()` and semantic
  similarity search in `similarXKCD()`, backed by a local 'DuckDB' cache.
