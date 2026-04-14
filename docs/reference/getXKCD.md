# Get XKCD comic

Fetches comic metadata and image from the XKCD JSON API.

## Usage

``` r
getXKCD(which = "current", display = TRUE, html = FALSE, saveImg = FALSE)
```

## Arguments

- which:

  A single string or a single numeric value. Specifies which comic to
  retrieve. Can be \`"current"\` for the latest comic, \`"random"\` for
  a random comic, or a comic number. Optional.

- display:

  A single logical value. If \`TRUE\`, the comic image will be displayed
  in the R graphics device. Optional.

- html:

  A single logical value. If \`TRUE\`, the comic's URL will be opened in
  the default web browser. Optional.

- saveImg:

  A single logical value. If \`TRUE\`, the comic image will be saved to
  a file in the current working directory. Optional.

## Value

A list containing details about the XKCD comic (number, title, date,
image URL, alt text, link, and transcript). If \`html\` is \`TRUE\`, the
list is returned invisibly. If the comic is not found or connection
fails, it returns \`NULL\` invisibly. Errors will occur if \`which\` is
invalid or if the XKCD API cannot be reached when attempting to get a
random comic.
