# Internal helpers for the semantic embeddings used by similarXKCD().
# Pure functions: no I/O; updateConfig() handles reading and saving.

#' Tokenize text into lowercase words
#'
#' @param text A character vector.
#' @returns A list of character vectors, one per element of `text`.
#' @keywords internal
#' @noRd
tokenize_xkcd <- function(text) {
  lapply(strsplit(tolower(text), "[^a-z0-9']+"), function(tok) {
    tok <- gsub("^'+|'+$", "", tok)
    tok[nzchar(tok)]
  })
}

#' Build LSA word and document embeddings
#'
#' Latent semantic analysis: a log-scaled TF-IDF document-term matrix with
#' L2-normalised rows is reduced by a truncated SVD. The SVD is computed from
#' the eigendecomposition of the (small) document Gram matrix, so only base R
#' and Matrix are needed. Word vectors are the right singular vectors scaled
#' by IDF, so that the mean of a query's word vectors is proportional to the
#' standard LSA fold-in of that query; document embeddings are U * Sigma.
#'
#' @param corpus A character vector, one document per element.
#' @param rank Maximum number of latent dimensions.
#' @param min_count Minimum corpus frequency for a word to enter the vocabulary.
#' @returns A list with `word_vectors` (vocabulary x rank, words as rownames)
#'   and `doc_embeddings` (documents x rank).
#' @keywords internal
#' @noRd
build_lsa_embeddings <- function(corpus, rank = 100L, min_count = 2L) {
  tokens <- tokenize_xkcd(corpus)
  n_docs <- length(tokens)
  words <- unlist(tokens, use.names = FALSE)
  doc_ids <- rep.int(seq_len(n_docs), lengths(tokens))

  freq <- table(words)
  vocab <- names(freq)[freq >= min_count]
  if (length(vocab) == 0L) stop("Not enough text to build embeddings.")
  word_ids <- match(words, vocab)
  in_vocab <- !is.na(word_ids)

  # Duplicate (doc, word) pairs are summed into term counts
  counts <- Matrix::sparseMatrix(i = doc_ids[in_vocab], j = word_ids[in_vocab], x = 1,
                                 dims = c(n_docs, length(vocab)))
  idf <- log(n_docs / Matrix::colSums(counts > 0))
  tfidf <- log1p(counts) %*% Matrix::Diagonal(x = idf)
  row_norms <- sqrt(Matrix::rowSums(tfidf^2))
  row_norms[row_norms == 0] <- 1
  tfidf <- Matrix::Diagonal(x = 1 / row_norms) %*% tfidf

  # Truncated SVD via the document Gram matrix: tfidf = U Sigma V'
  eig <- eigen(as.matrix(Matrix::tcrossprod(tfidf)), symmetric = TRUE)
  k <- min(rank, sum(eig$values > 1e-8))
  if (k == 0L) stop("Not enough text to build embeddings.")
  u <- eig$vectors[, seq_len(k), drop = FALSE]
  sigma <- sqrt(eig$values[seq_len(k)])
  v <- as.matrix(Matrix::crossprod(tfidf, u)) %*% diag(1 / sigma, nrow = k)

  word_vectors <- idf * v
  rownames(word_vectors) <- vocab
  list(word_vectors = word_vectors,
       doc_embeddings = u %*% diag(sigma, nrow = k))
}
