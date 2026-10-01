## Reason for update

This update responds to the CRAN notice of 2026-09-30 that package 'float'
is scheduled for archival on 2026-10-21, which would require archiving its
strong reverse dependencies. RXKCD depended on 'float' only indirectly,
through 'text2vec' -> 'rsparse' -> 'float'.

'text2vec' has been removed from Imports. The semantic similarity feature
(`similarXKCD()`) is now implemented with latent semantic analysis using only
base R and the recommended package 'Matrix'. RXKCD no longer has any direct
or recursive dependency on 'text2vec', 'rsparse', 'MatrixExtra' or 'float'.

## Test environments

* Local: Fedora Linux 44, R 4.6.1, `R CMD check --as-cran`

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

There are no reverse dependencies on CRAN.
