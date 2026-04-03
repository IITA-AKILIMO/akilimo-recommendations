?# I18N — Hardcoded String Removal Plan


Audit of remaining hardcoded English strings across the codebase, grouped by
location and effort. All findings are in PDF/email output — recommendation
text (`tr()` path) is fully internationalised.

---

## How the label system works

| Path | Mechanism | Who reads it |
|------|-----------|--------------|
| `data/input/translations.csv` | `tr(key, lang)` in `misc.R` | Recommendation text (SMS, email body, JSON) |
| `.PDF_LABELS` in `html_helpers.R` | `html_label(key, lang)` in `pdf_builders.R` | PDF section headings, chart labels, UI labels |

Chart labels and PDF-specific strings belong in `.PDF_LABELS`. Strings that
appear in both PDFs and recommendation text belong in `translations.csv`.

---

## Group A — PDF chart and body strings (`pdf_builders.R`)

### A1 — Keys already in `.PDF_LABELS` but not used (wire-up only)

These strings have a correct key in `.PDF_LABELS` but `pdf_builders.R` still
uses the hardcoded literal. Fix: replace the literal with `html_label(key, lang)`.

| File | Line | Hardcoded string | Existing key |
|------|------|-----------------|--------------|
| `pdf_builders.R` | 408 | `"Your current practice"` | `current_practice` |
| `pdf_builders.R` | 429 | `"Ploughing"` | `ploughing_label` |
| `pdf_builders.R` | 430 | `"Ridging"` | `ridging_label` |
| `pdf_builders.R` | 574 | `"Planting date"` | `planting_date` |
| `pdf_builders.R` | 575 | `"Harvest date"` | `harvest_date` |

### A2 — New keys needed in `.PDF_LABELS`

Add these entries to `.PDF_LABELS` in `html_helpers.R`, then replace each
hardcoded literal in `pdf_builders.R` with `html_label(key, lang)`.

#### IC PDF (maize pricing and production, `build_ic_pdf`)

| Line | Hardcoded string | Proposed key | Notes |
|------|-----------------|--------------|-------|
| 244 | `"per fresh cob"` | `per_fresh_cob` | Maize unit label |
| 246 | `"per %s kg of grain"` | `per_kg_grain` | sprintf-style; arg = maizeUW |
| 298 | `"%s kg extra maize grain"` | `extra_maize_grain_fmt` | sprintf-style; arg = dMP_fmt |
| 300 | `"%s extra maize cobs"` | `extra_maize_cobs_fmt` | sprintf-style; arg = dMP_fmt |

#### IC PDF (CIS summary, `build_ic_pdf`)

| Line | Hardcoded string | Proposed key |
|------|-----------------|--------------|
| 307a | `"Intercropping with sweet potato is recommended."` | `cis_rec_ic` |
| 307b | `"Intercropping is recommended but fertilizer is not profitable."` | `cis_rec_ic_no_fert` |
| 309  | `"Cassava monocrop is more profitable for this location."` | `cis_no_ic` |

#### PP PDF (chart labels, `build_pp_pdf`)

| Line | Hardcoded string | Proposed key |
|------|-----------------|--------------|
| 411 | `"Net value: "` | `chart_net_value` |
| 419 | `"\n Cost: "` | `chart_cost` |
| 428 | `"Recommended practice\n\n\n"` | `chart_recommended` |

#### PP PDF (table column headers, `build_pp_pdf`)

| Line | Hardcoded string | Proposed key |
|------|-----------------|--------------|
| 479 | `"Operation"` | `lmo_col_operation` |
| 479 | `"Method"` | `lmo_col_method` |
| 479 | `"Cost/ha"` | `lmo_col_cost` |

#### SP PDF (chart labels, `build_sp_pdf`)

| Line | Hardcoded string | Proposed key |
|------|-----------------|--------------|
| 566 | `"Current"` | `chart_current` |
| 567 | `"Recommended"` | `chart_recommended` (shared with PP) |

---

## Group B — Coordinate card labels (`html_helpers.R`)

The offline fallback coordinate card in `html_location_map()` has two
hardcoded labels baked into the sprintf format string.

| Line | Hardcoded string | Proposed key |
|------|-----------------|--------------|
| 352 | `"Lat"` | `coord_lat` |
| 353 | `"Lon"` | `coord_lon` |

Fix: add keys to `.PDF_LABELS`; pull them out of the sprintf format string
into `html_label()` calls.

---

## Group C — Email copy (`sms_email.R`)

`sendEmailReport()` has a hardcoded English subject and body. The function
currently receives no `lang` parameter.

| Line | Hardcoded string |
|------|-----------------|
| 215 | `"AKILIMO recommendation"` |
| 216 | `"Please find attached your AKILIMO recommendation.\n\nBest regards,\nThe AKILIMO team"` |

**Fix:**
1. Add `lang = "en"` parameter to `sendEmailReport()`.
2. Thread `lang` through from `run_akilimo()` → `sendEmailReport()`.
3. Add two keys to `.PDF_LABELS`: `email_subject` and `email_body`.

---

## Group D — API validation errors (`AkilimoMain.R`)

`validate_request()` and `bad_request()` return English error messages to
API consumers. These are developer/integration-facing, not end-user-facing,
and are part of the documented API contract.

**Decision: leave as-is.** Translating API error messages would break
integrations that parse the message string. If localised errors are needed in
future, add a separate `message_key` field alongside `message`.

---

## Implementation order

1. **A1** — Wire-up only; no new keys. Lowest risk. (~5 line changes)
2. **A2 IC** — Add 7 IC keys to `.PDF_LABELS`; update `build_ic_pdf`. (~20 lines)
3. **A2 PP** — Add 6 PP keys; update `build_pp_pdf`. (~15 lines)
4. **A2 SP** — Add/share 2 SP keys; update `build_sp_pdf`. (~5 lines)
5. **B** — Add 2 coordinate keys; refactor sprintf in `html_location_map`. (~5 lines)
6. **C** — Add `lang` param to `sendEmailReport`; add 2 email keys. (~10 lines)

Each step is independent and can be committed separately.

---

## New `.PDF_LABELS` entries summary

```r
# ── IC — maize pricing and production ────────────────────────────────────
per_fresh_cob         = c(en = "per fresh cob",           sw = "kwa bibo moja"),
per_kg_grain          = c(en = "per %s kg of grain",      sw = "kwa kilo %s za nafaka"),
extra_maize_grain_fmt = c(en = "%s kg extra maize grain", sw = "kilo %s za nafaka ya mahindi zaidi"),
extra_maize_cobs_fmt  = c(en = "%s extra maize cobs",     sw = "mabibo %s ya mahindi zaidi"),

# ── IC — CIS intercropping summary ───────────────────────────────────────
cis_rec_ic            = c(en = "Intercropping with sweet potato is recommended.",
                           sw = "Kilimo mseto na viazi vitamu kinapendekezwa."),
cis_rec_ic_no_fert    = c(en = "Intercropping is recommended but fertilizer is not profitable.",
                           sw = "Kilimo mseto kinapendekezwa lakini mbolea haileti faida ya kutosha."),
cis_no_ic             = c(en = "Cassava monocrop is more profitable for this location.",
                           sw = "Kupanda muhogo peke yake ni yenye faida zaidi kwa eneo hili."),

# ── PP — chart labels ─────────────────────────────────────────────────────
chart_net_value       = c(en = "Net value: ",             sw = "Thamani halisi: "),
chart_cost            = c(en = "\n Cost: ",               sw = "\n Gharama: "),
chart_recommended     = c(en = "Recommended practice",    sw = "Mbinu inayopendekezwa"),

# ── PP — LMO table column headers ────────────────────────────────────────
lmo_col_operation     = c(en = "Operation",               sw = "Shughuli"),
lmo_col_method        = c(en = "Method",                  sw = "Njia"),
lmo_col_cost          = c(en = "Cost/ha",                 sw = "Gharama/hekta"),

# ── SP — chart labels ─────────────────────────────────────────────────────
chart_current         = c(en = "Current",                 sw = "Sasa"),
# chart_recommended is shared with PP above

# ── Location card ─────────────────────────────────────────────────────────
coord_lat             = c(en = "Lat",                     sw = "Lat"),
coord_lon             = c(en = "Lon",                     sw = "Lon"),

# ── Email ─────────────────────────────────────────────────────────────────
email_subject         = c(en = "AKILIMO recommendation",
                           sw = "Mapendekezo ya AKILIMO"),
email_body            = c(en = "Please find attached your AKILIMO recommendation.\n\nBest regards,\nThe AKILIMO team",
                           sw = "Tafadhali tazama mapendekezo yako ya AKILIMO yaliyoambatanishwa.\n\nKwa heshima,\nTimu ya AKILIMO")
```

---

## Files changed per step

| Step | Files modified |
|------|---------------|
| A1 | `pdf_builders.R` |
| A2 IC | `html_helpers.R`, `pdf_builders.R` |
| A2 PP | `html_helpers.R`, `pdf_builders.R` |
| A2 SP | `html_helpers.R`, `pdf_builders.R` |
| B | `html_helpers.R` |
| C | `html_helpers.R`, `sms_email.R`, `AkilimoMain.R` |

---

## Task list

| Task # | Step | Subject | Files |
|--------|------|---------|-------|
| #8 | A1 | Wire up existing `.PDF_LABELS` keys in `pdf_builders.R` | `pdf_builders.R` |
| #9 | A2-IC | Add IC maize/CIS label keys and update `build_ic_pdf` | `html_helpers.R`, `pdf_builders.R` |
| #10 | A2-PP | Add PP chart/table label keys and update `build_pp_pdf` | `html_helpers.R`, `pdf_builders.R` |
| #11 | A2-SP | Add SP chart label keys and update `build_sp_pdf` | `html_helpers.R`, `pdf_builders.R` |
| #12 | B | Add coordinate card label keys and update `html_location_map` | `html_helpers.R` |
| #13 | C | Add `lang` param to `sendEmailReport` and translate subject/body | `html_helpers.R`, `sms_email.R`, `AkilimoMain.R` |
