# RXKCD Comic Recommender — Design Spec

**Date:** 2026-04-01
**Status:** Approved
**Goal:** Add `recommendXKCD(query)` to the RXKCD R package, backed by a locally fine-tuned LLM served via Ollama.

---

## Overview

The system has two phases:

- **Offline training pipeline** (Python, run once — re-run when the comic corpus grows significantly): exports comic text from DuckDB, generates synthetic training pairs using a large local Ollama model, fine-tunes a small model with QLoRA, and loads the result into Ollama as a custom model.
- **Online inference** (R): `recommendXKCD()` sends a natural language query to the fine-tuned Ollama model via HTTP, parses the JSON response, cross-references local DuckDB for full comic metadata, and returns a data frame.

```
OFFLINE (Python)
  DuckDB → 01_export_comics.py → comics.csv
                               ↓
           02_generate_training_data.py (llama3.1:8b via Ollama) → training.jsonl
                               ↓
           03_finetune.py (unsloth + QLoRA, Qwen2.5-1.5B-Instruct)
                               ↓
           04_export_gguf.py → rxkcd-recommender.gguf + Modelfile
                               ↓
           ollama create rxkcd-recommender -f Modelfile

ONLINE (R)
  recommendXKCD("query") → httr POST → Ollama HTTP API (localhost:11434)
                                      → rxkcd-recommender model
                                      → parse JSON response
                                      → cross-ref DuckDB
                                      → return data frame
```

No new R package dependencies are required. `httr`, `DBI`, and `duckdb` are already imported.

---

## Section 1: File Layout

```
R/
  getXKCD.R              # recommendXKCD() added here alongside existing functions
inst/
  scripts/
    01_export_comics.py
    02_generate_training_data.py
    03_finetune.py
    04_export_gguf.py
```

The Python scripts are shipped with the package in `inst/scripts/` but are not loaded by R. Users run them manually, once, to build the fine-tuned model.

---

## Section 2: Training Data Pipeline

### `01_export_comics.py`

Reads all comics from `~/.RXKCD/xkcd.duckdb` and writes `comics.csv` with columns: `num`, `title`, `alt`, `transcript`. Comics where all three text fields are empty are skipped.

### `02_generate_training_data.py`

For each comic, calls `llama3.1:8b` via Ollama (`POST /api/chat`) with a prompt:

> *"Given this XKCD comic — Title: '{title}', Alt text: '{alt}', Transcript: '{transcript[:500]}' — generate 6 diverse natural language queries a user might type when searching for this comic. Vary the phrasing: some literal, some conceptual, some humorous. Output as a JSON array of strings."*

Comics with very short text (title-only or alt-only) receive 3 queries instead of 6.

Each query becomes one record in `training.jsonl`:

```json
{
  "messages": [
    {
      "role": "system",
      "content": "You are an XKCD comic recommendation assistant. Always respond with valid JSON in the format: {\"comics\": [{\"num\": <int>, \"title\": \"<str>\", \"reason\": \"<str>\"}]}"
    },
    {
      "role": "user",
      "content": "something about procrastination graphs"
    },
    {
      "role": "assistant",
      "content": "{\"comics\": [{\"num\": 605, \"title\": \"Extrapolating\", \"reason\": \"Shows someone graphing a brief uptick to convince themselves of rising productivity\"}]}"
    }
  ]
}
```

Expected output: ~15,000–18,000 training pairs from ~3,000 comics.

---

## Section 3: Fine-tuning

### `03_finetune.py`

- **Base model:** `Qwen/Qwen2.5-1.5B-Instruct`
- **Method:** QLoRA via `unsloth`
- **Quantization:** 4-bit
- **LoRA config:** rank=16, alpha=32, target modules: q/k/v/o projections
- **Training:** 2 epochs, batch size 4, gradient accumulation ×4 (effective batch 16), learning rate 2e-4 with cosine decay
- **Hardware:** NVIDIA RTX A2000 8GB — expected runtime ~1–2 hours
- **Output:** merged adapter weights saved to disk

### `04_export_gguf.py`

Converts merged weights to GGUF format (Q4_K_M quantization) and writes a `Modelfile`:

```
FROM ./rxkcd-recommender.gguf
SYSTEM "You are an XKCD comic recommendation assistant. Always respond with valid JSON in the format: {\"comics\": [{\"num\": <int>, \"title\": \"<str>\", \"reason\": \"<str>\"}]}"
```

Load into Ollama:

```bash
ollama create rxkcd-recommender -f Modelfile
```

Scripts are run once in order: `01` → `02` → `03` → `04` → `ollama create`.

---

## Section 4: R Integration

### Function signature

```r
recommendXKCD(query, n = 3, model = "rxkcd-recommender", host = "http://localhost:11434")
```

Added to `R/getXKCD.R` alongside the existing three functions.

### Behaviour

1. Check that `~/.RXKCD/xkcd.duckdb` exists — stop with a helpful message if not (directs user to run `updateConfig()`)
2. POST to `{host}/api/chat` with `stream = FALSE`, passing `query` as user message and `n` embedded in the system prompt ("Recommend exactly {n} comics.")
3. Parse the JSON `comics` array from the model response — extract `num`, `title`, `reason`
4. Open a read-only DuckDB connection and look up each `num` to retrieve `img`, `alt`, `link`
5. Return a data frame with columns: `num`, `title`, `reason`, `img`, `link`, `alt`

### Error handling

| Condition | Behaviour |
|---|---|
| Ollama not reachable | `stop()` with message to start Ollama (`ollama serve`) |
| Model not found (HTTP 404) | `stop()` telling user to run `ollama create rxkcd-recommender` |
| Malformed JSON from model | Retry once with stricter prompt; if still broken, `stop()` with raw response |
| Hallucinated comic number (not in DB) | Silently drop that row; `message()` noting it was skipped |
| `n` out of range | `stop()` — must be a positive integer |

### Prerequisites

- `updateConfig()` must have been run to populate the local DuckDB
- Ollama must be running (`ollama serve`) with `rxkcd-recommender` available
- Both are checked early with clear, actionable error messages

---

## Out of Scope

- Automatic re-training when new comics are published (can be added later as a scheduled script)
- A Shiny UI or interactive browser (out of scope for this R package)
- Multi-turn conversation or follow-up queries
- Packaging the Python environment — users are expected to have Python + `unsloth` installed
