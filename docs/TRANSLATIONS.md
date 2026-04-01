# Translation System

Akilimo uses a single CSV file (`data/input/translations.csv`) to manage all user-facing text across languages. This document explains how the system works and how to extend it.

---

## CSV format

The file uses a **wide format**: one row per translation key, one column per language code.

```
key,en,sw,rw
norecom,"We do not have a recommendation...","Hatuna mapendekezo...","rw"
recPopt,"Your revenue will be highest at your proposed planting date, on {date}.","Mapato yako yatakuwa makubwa zaidi ukipanda tarehe {date}.","rw"
```

| Column | Contents |
|--------|----------|
| `key`  | Unique identifier used in R code |
| `en`   | English text (default / fallback) |
| `sw`   | Swahili text |
| `rw`   | Kinyarwanda text (placeholder — falls back to `en`) |

**Rule:** every key must have an `en` value. Other columns may be empty or contain a placeholder — the `tr()` function falls back to `en` automatically.

---

## The `tr()` function

Defined in `R/misc.R`:

```r
tr(key, lang, ...)
```

| Parameter | Type | Description |
|-----------|------|-------------|
| `key` | character | Translation key (must exist in the CSV) |
| `lang` | character | Language code: `"en"` or `"sw"` |
| `...` | named values | Token substitutions (see below) |

**Lookup logic:**
1. Find the row where `key` matches.
2. Return `row[[lang]]`.
3. If the value is empty or blank, fall back to `row[["en"]]`.
4. If `en` is also missing, stop with an error.

The translation table is cached in memory after the first load — repeated calls are essentially free.

---

## Named token substitution

Translation strings may contain `{name}` placeholders. Pass values as named arguments to `tr()`:

```r
tr("recPopt", lang, date = format(ds$PD, "%d %B %Y"))
```

CSV:

```
recPopt,en,"Your revenue will be highest at your proposed planting date, on {date}."
recPopt,sw,"Mapato yako yatakuwa makubwa zaidi ukipanda tarehe {date}."
```

Multiple tokens are supported:

```r
tr("recPln", lang,
   date      = format(ds$PD, "%d %B %Y"),
   weeks     = abs(ds$rPWnr),
   direction = ifelse(ds$rPWnr < 0, tr("early", lang), tr("late", lang)))
```

Token values can themselves be `tr()` calls, which allows reusing atomic translated words (directions, conjunctions) inside composite sentences.

---

## Selecting the language at request time

Callers pass a `lang` field in the JSON request body. The API extracts it in `parse_request()` (R/AkilimoMain.R):

```json
{ "lang": "sw", "country": "TZ", ... }
```

| Value | Language |
|-------|----------|
| `"en"` | English (default) |
| `"sw"` | Swahili |

Any unrecognised value silently falls back to `"en"`. The `lang` field is independent of `country` — a Nigerian user can request Swahili output by passing `"lang": "sw"`, and a Tanzanian user will receive English by passing `"lang": "en"` (or omitting the field).

---

## Adding a new translation key

1. Add a row to `data/input/translations.csv`:

   ```
   myKey,"English text with optional {token}.","Swahili text with optional {token}.","rw"
   ```

2. Use it in R:

   ```r
   tr("myKey", lang)
   # or with a token:
   tr("myKey", lang, token = some_value)
   ```

No R code changes are required unless you are introducing a new composite sentence that replaces an existing `paste0()` pattern.

---

## Adding a new language

1. Add a column to `translations.csv` with the [IETF language tag](https://en.wikipedia.org/wiki/IETF_language_tag) as the header (e.g. `fr` for French):

   ```
   key,en,sw,rw,fr
   norecom,"We do not...","Hatuna...","rw","Nous n'avons pas..."
   ```

2. Add `"fr"` to the `VALID_LANGS` check in `parse_request()` (R/AkilimoMain.R):

   ```r
   lang <- if (lang_raw %in% c("en", "sw", "fr")) lang_raw else "en"
   ```

3. Publish the updated `translations.csv` to Zenodo (see [SETUP.md](../SETUP.md#maintainer-publishing-data-updates)).

Keys that lack a translation for the new language automatically fall back to English.

---

## Dead keys

Some keys remain in the CSV but are no longer referenced in R code. They were absorbed into composite keys during the QUA-10 refactor:

| Dead key | Absorbed into |
|----------|---------------|
| `recRatt2` | `recRatt1` |
| `wks` | `recPln`, `recHvs` |
| `recPlnP` | `recPln` |
| `recPhv` | `recHvs` |
| `extrap`, `tonof`, `netincr` | `frImpact` |
| `cisNetPfx`, `cisNetSfx` | `cisNet` |

These can be safely removed from the CSV in a future cleanup pass once confirmed unused by `grep -r` across the `R/` directory.
