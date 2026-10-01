# RXKCD

\$ library(RXKCD)\_

# RXKCD

Get, search & discover XKCD comics directly from R

v2.0.1 Paolo Sonego · Mikko Korpela CRAN · GPL-2

Context

## What is XKCD?

A webcomic by Randall Munroe — "romance, sarcasm, math, and language."  
Beloved by the programming community for its wit, precision, and
occasional existential dread.

3000+ comics published

2006 first published

JSON public API

RXKCD brings the entire archive into your R session — fetch, search, and
explore.

How it works

## Architecture

🌐

XKCD API xkcd.com/N/info.0.json

→

⚙

updateConfig() incremental sync

→

🦆

DuckDB ~/.RXKCD/xkcd.duckdb

→

🔍

search() BM25 + GloVe

- Schema: `xkcd(num, title, date, alt, img, transcript)`
- Chunk-downloads in batches of 100 — caps peak memory
- Rate-limited at 0.05 s/request — polite to the API
- Comic \#404 skipped — it intentionally does not exist

Function 1 of 4

## getXKCD()

getXKCD(which, display, html, saveImg)

- Always hits the live XKCD API — no local DB needed
- `"current"` · `"random"` · comic number
- Returns list: num, title, date, img, alt, transcript
- Optionally display inline or save image to disk

\# latest comic getXKCD("current") \# random comic getXKCD("random") \#
specific number getXKCD(353) \# Python! \# save to file getXKCD(1,
saveImg = TRUE)

Function 2 of 4

## updateConfig()

Smart incremental sync — downloads only what you are missing.

01

Connect to local DuckDB Auto-creates ~/.RXKCD/xkcd.duckdb on first run

02

Diff against live API Fetches max comic number, finds missing IDs (skips
\#404)

03

Chunk-download & append Batches of 100 comics, 0.05 s rate limit per
request

04

Rebuild FTS index & GloVe embeddings BM25 index + 50-dim GloVe vectors
from text2vec

Function 3 of 4

## searchXKCD()

Full-text BM25 search across title, alt text, and transcript. Results
ranked by relevance.

\> searchXKCD("significant")

num

title

alt (excerpt)

score

882

Significant

So, uh, we did the x-ray…

9.1

1478

P-Values

If all else fails, use…

7.4

2400

Statistics

…technically statistically…

5.8

1462

Blind Trials

…significant at p\<0.05…

4.2

searchXKCD("python") \# full-text BM25 searchXKCD("climate change")

Function 4 of 4

## similarXKCD()

Semantic search via local GloVe embeddings. Find comics by *meaning*,
not just keywords.

"feeling  
lonely"

query

→

GloVe  
50-dim

embed

→

cosine  
sim

rank

similarXKCD("feeling lonely") similarXKCD("space exploration", n = 10)

\#483 Fiction Rule of Thumb

0.88

\#915 Connoisseur

0.81

\#721 Flatland

0.76

\#1314 Dating Pools

0.71

\#520 Alien Abduction

0.65

Get started

## Installation

CRAN (stable)

install.packages("RXKCD")

GitHub (latest)

\# install.packages("pak") pak::pak("onertipaday/RXKCD")

Quick start

library(RXKCD) updateConfig() \# first run: build cache
getXKCD("current") \# display latest searchXKCD("python") \# BM25 search
similarXKCD("feeling lonely") \# semantic search

onertipaday.github.io/RXKCD

github.com/onertipaday/RXKCD

Summary

# Thank you.

- ✓ Live API fetch — current, random, numbered
- ✓ Local DuckDB cache — offline, fast, incremental
- ✓ BM25 full-text search across all comic text
- ✓ GloVe semantic similarity — search by meaning
- ✓ On CRAN · GPL-2 · v2.0.1

install.packages("RXKCD")

github/  
onertipaday/  
RXKCD
