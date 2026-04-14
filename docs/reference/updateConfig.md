# Update XKCD Database (Smart Sync)

Connects to the local DuckDB. Checks for missing comics and fetches full
metadata (Title, Alt Text, Transcript) from the JSON API for any gaps.

## Usage

``` r
updateConfig()
```

## Value

\`TRUE\`, invisibly, if the database is successfully updated or
determined to be up-to-date. The function will stop with an error if it
cannot connect to \`xkcd.com\`.
