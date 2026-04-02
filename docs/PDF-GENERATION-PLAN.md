# PDF Generation: WeasyPrint Pipeline (Completed)

## 1. Executive Summary

### What changes

The current PDF pipeline uses `webshot::rmdshot()` (driven by PhantomJS, which is unmaintained) to render one of eight flexdashboard Rmd templates. Each Rmd reads data from CSV files that the R processor functions wrote to a per-request temp directory. This is a three-layer serialisation: R objects → CSV → Rmd knit → PhantomJS screenshot → PDF.

The replacement removes the intermediate layers entirely:

- The eight Rmd templates (`Rmd/*.Rmd`) are deleted.
- The CSV-writing functions in `markdown.R` (`FR_MarkdownText`, `IC_MarkdownText`, `CIS_MarkdownText`, `PPSP_MarkdownText`, `fertilizerAdviseTable`, and associated helpers) are replaced by in-memory HTML-building functions.
- `generate_pdfs()` in `sms_email.R` is rewritten to write an HTML file to the request's temp dir and call WeasyPrint via `system2()`.
- `webshot`, `flexdashboard`, `knitr`, and `rmarkdown` are removed from the runtime dependency list.
- No R package change is needed — WeasyPrint is a Python CLI tool installed on the server.

### What stays the same

- All recommendation computation logic: `process-FR.R`, `process-IC.R`, `process-PP.R`, `process-SP.R`, `quefts.R`, `optimize_fert.R`.
- The `run_akilimo()` / `dispatch_recommendations()` / `build_response()` orchestration in `AkilimoMain.R` — only the `generate_pdfs()` and `sendEmailReport()` calls at the end change.
- The translation system (`tr()`, `translations.csv`).
- The `setup_temp_dir()` / `tp()` / `temp_dir()` / `set_temp_dir()` mechanism (still needed for map PNG, ggplot PNGs, and the final PDFs).
- `net/akilimo.css` (carried over and extended).
- All `net/*/` bag and cash PNG assets (embedded as base64 in HTML).
- `sendEmailReport()` is unchanged.
- All test fixtures in `tests/input/`.

### Key benefits

| Concern | Before | After |
|---|---|---|
| Renderer | PhantomJS (deprecated 2018) | WeasyPrint (actively maintained Python) |
| Data path | R → 10–15 CSV files → Rmd reads them back | R objects passed directly |
| Template count | 8 Rmd files (en + sw × 4 types) | 4 R functions + 1 `lang` parameter |
| Startup overhead | PhantomJS cold-boot + knitr knit | Single `weasyprint` process call |
| Browser dependency | PhantomJS (headless WebKit) | None — WeasyPrint is pure Python |
| CSS overrides | Needed (navbar, chart-title hacks) | Not needed (plain HTML) |
| Debugging | Rmd errors are opaque | HTML inspectable in any browser |
| Server footprint | PhantomJS binary (~50 MB) | `pip install weasyprint` |

---

## 2. New Dependency: WeasyPrint

WeasyPrint is a Python-based HTML/CSS → PDF converter. It implements the CSS Paged Media spec natively — no browser, no JavaScript engine, no Chrome. It is actively maintained and supports CSS Grid (v60+), Flexbox, `@page` rules, and base64-embedded images.

### Installation

**Linux (production server)**

```bash
pip install weasyprint
# or via apt on Debian/Ubuntu:
apt-get install python3-weasyprint

# Verify:
weasyprint --version
```

WeasyPrint requires some system libraries for font rendering:

```bash
apt-get install -y libpango-1.0-0 libpangoft2-1.0-0 libgdk-pixbuf2.0-0
```

**Docker**

```dockerfile
RUN apt-get update && apt-get install -y \
    python3-pip libpango-1.0-0 libpangoft2-1.0-0 \
    && pip3 install weasyprint
```

**Windows (development)**

```powershell
pip install weasyprint
# GTK runtime also required — install via:
# https://github.com/tschoonj/GTK-for-Windows-Runtime-Environment-Installer
```

Or use WSL2 with the Linux instructions above.

### How R calls it

```r
render_pdf <- function(html, path) {
    html_tmp <- tp("render_tmp.html")
    writeLines(html, html_tmp, useBytes = TRUE)
    result <- system2("weasyprint", args = c(html_tmp, path),
                      stdout = TRUE, stderr = TRUE)
    if (!file.exists(path) || file.size(path) == 0) {
        stop("WeasyPrint failed: ", paste(result, collapse = "\n"))
    }
    invisible(path)
}
```

WeasyPrint resolves relative paths (images, CSS) from the HTML file's location — so writing the HTML to `tp("render_tmp.html")` and embedding all assets as base64 (or using absolute paths) is the cleanest approach.

### install_packages.R changes

Remove:
```r
"webshot",
"knitr",
"rmarkdown",
"flexdashboard",
```

Add:
```r
"base64enc",
```

Remove the `webshot::install_phantomjs()` block entirely.

Add a server setup check for WeasyPrint:
```r
if (nchar(Sys.which("weasyprint")) == 0) {
    warning("WeasyPrint not found on PATH. PDF generation will fail. ",
            "Install with: pip install weasyprint")
}
```

---

## 3. Architecture Overview

### Old data flow

```
process_FR(...)
    ├── FR_MarkdownText(...)       writes FR_MarkDownText.csv
    ├── fertilizerAdviseTable()    writes datall1..5.csv,
    │                              totalCostmoney.csv,
    │                              totalSalemoney.csv,
    │                              totalRevenuemoney.csv
    └── write.csv(recText, 'FR_recText.csv')

generate_pdfs(...)
    └── webshot::rmdshot('Rmd/FR_markdown_VFT.Rmd', ...)
              ├── knitr reads back all CSV files
              ├── mapshot() → map.png
              └── PhantomJS → PDF
```

### New data flow

```
process_FR(...)
    └── returns: response (data + fertilizer_rates), recText, fertilizers
        (NO CSV writes for PDF input)

generate_pdfs(user, FR, IC, PP, SP, country, lang, result, params)
    └── build_fr_pdf(rr, fertilizers, user, country, ..., lang, out_path)
              ├── calc_fertilizer_recom()     (existing, unchanged)
              ├── html_page_header()
              ├── html_personal_info()
              ├── html_fertilizer_table()
              ├── html_cost_benefit()
              ├── html_location_map()         → map.png in temp dir
              ├── html_recommendation()
              └── render_pdf(html, out_path)
                        ├── writeLines(html, tp("render_tmp.html"))
                        └── system2("weasyprint", ...) → PDF
```

The per-request temp directory (`temp/<uuid>/`) is still created by `setup_temp_dir()` but now only holds the map PNG, the ggplot chart PNG (SP/PP), the intermediate `render_tmp.html`, and the final PDFs. No CSV intermediaries.

---

## 4. New File Structure

### Files to CREATE

| Path | Purpose |
|---|---|
| `R/pdf_builders.R` | `build_fr_pdf()`, `build_ic_pdf()`, `build_pp_pdf()`, `build_sp_pdf()`, `render_pdf()` |
| `R/html_helpers.R` | Shared HTML fragment generators: `html_page_header()`, `html_personal_info()`, `html_location_map()`, `html_fertilizer_table()`, `html_cost_benefit()`, `html_recommendation()`, `html_table()`, `img_base64()` |
| `net/akilimo_print.css` | Print-optimised stylesheet (`@page` rules, two/three-column grid, WeasyPrint-compatible CSS) |

### Files to MODIFY

| Path | Changes |
|---|---|
| `R/markdown.R` | Remove CSV-writing functions (listed in §12); keep `tp()`, `temp_dir()`, `set_temp_dir()`, `safe_filename_part()`, `FERT_COLOUR`, `FERT_LABEL`, `calc_fertilizer_recom()`, `pivot_fertilizers_wide()` |
| `R/sms_email.R` | Rewrite `generate_pdfs()` to accept `result` + `params` and call `build_*_pdf()` |
| `R/AkilimoMain.R` | Pass `result` and `params` into the `generate_pdfs()` call |
| `R/process-FR.R` | Remove `FR_MarkdownText()` and `fertilizerAdviseTable()` calls; add `fertilizers` to return value |
| `R/process-IC.R` | Same pattern: remove markdown calls, add data to return value |
| `R/process-PP.R` | Remove `PP_MarkdownText()` and `write.csv(res, ...)` calls; add `res`, `costLMO`, `recText` to return value |
| `R/process-SP.R` | Remove `SP_MarkdownText()` and `write.csv(ds, ...)` calls; add `ds`, `recText` to return value |
| `install_packages.R` | Remove `webshot`/`flexdashboard`; add `base64enc`; add WeasyPrint PATH check |
| `tests/test_pdf.R` | Rewrite to call `build_*_pdf()` directly |
| `CLAUDE.md` | Update architecture section, key modules table, required packages list |

### Files to DELETE

```
Rmd/FR_markdown_VFT.Rmd
Rmd/FR_markdown_swa.Rmd
Rmd/IC_markdown_VFT.Rmd
Rmd/CIS_markdown_swa.Rmd
Rmd/PP_markdownVFT.Rmd
Rmd/PP_markdown_swa.Rmd
Rmd/SP_markdownVFT.Rmd
Rmd/SP_markdown_swa.Rmd
Rmd/map.png              (stale render artefact)
Rmd/spgg.png             (stale render artefact)
```

---

## 5. Implementation Phases

### Phase 1 — Infrastructure

**Scope:** Foundation only. No existing behaviour changes.

1. Install WeasyPrint on dev machine and production server. Verify with `weasyprint --version`.
2. Add `base64enc` to `install_packages.R`. Remove `webshot` and PhantomJS block. Add WeasyPrint PATH check.
3. Create `net/akilimo_print.css` (WeasyPrint-compatible — see §CSS notes below).
4. Create `R/html_helpers.R` with stub implementations of all shared helpers.
5. Create `R/pdf_builders.R` with `render_pdf()` only. Run a smoke test: write a minimal HTML string and call `render_pdf()` to confirm WeasyPrint produces a valid PDF.

**CSS notes for WeasyPrint compatibility:**
- Use `display: grid` with `grid-template-columns` — supported in WeasyPrint ≥ 60.
- Avoid `position: fixed` (not supported) — use `@page` margin boxes for headers/footers instead.
- `@page { size: A4; margin: 15mm; }` is fully supported.
- `page-break-before: always` and the newer `break-before: page` both work.
- Avoid CSS `filter`, `backdrop-filter`, `clip-path` — not supported.
- All images must be either embedded as base64 or accessible via absolute file path from the HTML file's location.

**Deliverable:** `net/akilimo_print.css`, `R/html_helpers.R` stubs, `render_pdf()` smoke test passes.

---

### Phase 2 — FR PDF Builder

**Scope:** First complete builder end-to-end. Proves the pattern before touching IC/PP/SP.

1. Implement all `html_helpers.R` functions needed for FR: `html_page_header`, `html_personal_info`, `html_fertilizer_table`, `html_cost_benefit`, `html_location_map`, `html_recommendation`, `img_base64`.
2. Implement `build_fr_pdf()` in `pdf_builders.R`.
3. Modify `process-FR.R`:
   - Remove `FR_MarkdownText()` and `fertilizerAdviseTable()` calls.
   - Remove `write.csv(recText, tp('FR_recText.csv'), ...)`.
   - Add `fertilizers` to the return list.
4. Modify `AkilimoMain.R` `run_akilimo()`: pass `result` and `params` to `generate_pdfs()`.
5. Modify `generate_pdfs()` in `sms_email.R`: for the FR branch, call `build_fr_pdf()`.
6. Run `Rscript tests/test_pdf.R` (FR cases). Visually compare against `pdf-samples/FR.pdf`.

**Deliverable:** FR PDF generated via WeasyPrint, visually verified.

---

### Phase 3 — IC, PP, SP PDF Builders

**Scope:** Remaining three builders following the Phase 2 pattern.

**IC builder (`build_ic_pdf`)**
- `subtype` parameter: `"IC"` (NG maize) or `"CIS"` (TZ sweet potato).
- Remove `IC_MarkdownText()`, `CIS_MarkdownText()`, `fertilizerAdviseTable()` from `process-IC.R`.
- Add maize/sweet-potato price fields and `fertilizers` to the return value.

**PP builder (`build_pp_pdf`)**
- Receives `res` (ploughing × ridging matrix with `dNR`, `dTC`) and `costLMO` data frame.
- Renders the PP ggplot matrix to `tp("pp_chart.png")` via `ggplot2::ggsave()`, embeds via `img_base64()`.
- `costLMO` rendered as HTML table via `html_table()`.
- Remove `PP_MarkdownText()` and CSV writes from `process-PP.R`; add `res`, `costLMO`, `recText` to return value.

**SP builder (`build_sp_pdf`)**
- Receives `ds` (PD × HD grid with `dGR`).
- Renders SP heatmap to `tp("spgg.png")` via `ggplot2::ggsave()`, embeds via `img_base64()`.
- Remove `SP_MarkdownText()` and CSV writes from `process-SP.R`; add `ds`, `recText` to return value.
- Remove stray `spgg.png` cleanup from `generate_pdfs()`.

**Deliverable:** All 8 PDF variants (en + sw × 4 types) passing `tests/test_pdf.R`.

---

### Phase 4 — Remove Old Pipeline

**Scope:** Delete obsolete files and functions once all builders pass tests.

1. Delete all 8 `Rmd/*.Rmd` files and stray PNGs.
2. Remove dead functions from `R/markdown.R` (see §12).
3. Remove the `costLMO.csv` write in `get_costLMO()`.
4. Remove `webshot`, `knitr`, `rmarkdown`, `flexdashboard` from `install_packages.R`.
5. Confirm the per-request temp dir now only contains: `map.png`, `spgg.png`/`pp_chart.png` (when applicable), `render_tmp.html`, and the output PDFs.

**Deliverable:** Clean repo with no Rmd or webshot references in live code.

---

### Phase 5 — Testing and Cleanup

1. Rewrite `tests/test_pdf.R` (see §13).
2. Run `tests/test_full.R` (3203 regression cases) — must pass unchanged.
3. Visual comparison of each new PDF against `pdf-samples/`.
4. Load-test concurrent requests (each request has an isolated temp dir; WeasyPrint processes are independent).
5. Update `CLAUDE.md` architecture section, required packages list, and `docs/SETUP.md` WeasyPrint installation notes.

---

## 6. Detailed Function Specifications

### `render_pdf(html, path)` — `R/pdf_builders.R`

```r
render_pdf <- function(html, path) {
    html_tmp <- tp("render_tmp.html")
    writeLines(html, html_tmp, useBytes = TRUE)
    result <- system2("weasyprint", args = c(html_tmp, path),
                      stdout = TRUE, stderr = TRUE)
    if (!file.exists(path) || file.size(path) == 0) {
        stop("WeasyPrint failed:\n", paste(result, collapse = "\n"))
    }
    invisible(path)
}
```

All images must be base64-embedded or referenced via absolute paths so WeasyPrint can resolve them from the temp directory.

---

### `img_base64(path, alt = "")` — `R/html_helpers.R`

```r
img_base64 <- function(path, alt = "") {
    raw   <- readBin(path, "raw", n = file.info(path)$size)
    b64   <- base64enc::base64encode(raw)
    sprintf('<img src="data:image/png;base64,%s" alt="%s">', b64, alt)
}
```

Used for: banner images, bag count visuals, cash-stack images, map PNG, ggplot chart PNGs.

---

### `html_page_header(title, lang, banner_path)` — `R/html_helpers.R`

```r
html_page_header <- function(title, lang, banner_path = NULL) {
    # Returns: full <!DOCTYPE html><head>...</head><body> opening string
    # Includes: <link> to akilimo_print.css, banner <img> as base64 if provided
}
```

The CSS link uses an absolute path to `net/akilimo_print.css`. Alternatively all CSS can be inlined in a `<style>` tag to make the HTML fully self-contained (recommended for WeasyPrint — avoids path resolution issues).

---

### `html_personal_info(user, country, userField, area, areaUnits, PD, HD, current_yield = NULL, lang)` — `R/html_helpers.R`

```r
# Returns: character(1) — <section> with labelled rows:
#   Name, Phone, Field, Field area, Planting date, Harvest date,
#   optionally Current yield (FR and IC only).
# Dates formatted as "day-Mon-YYYY".
```

---

### `html_location_map(lat, lon, height_px = 150)` — `R/html_helpers.R`

```r
# Saves leaflet map to tp("map.png") via mapview::mapshot(),
# reads it back, returns base64 <img> tag.
# PNG remains on disk until temp dir TTL expires (1 hour).
```

---

### `html_fertilizer_table(fr, area, areaUnits, currency, rootUP, cassPD, cassUW, maxInv, lang)` — `R/html_helpers.R`

```r
# fr : data frame from calc_fertilizer_recom() — may have 0 rows
#
# Returns: <section> containing:
#   - Fertilizer price table (type × cost-per-bag)
#   - One recommendation row per fertilizer:
#       name | kg amount | approx bags | bag images (base64) | total cost
#   - Cassava price and max investment lines
#   - If nrow(fr) == 0: "No fertilizer recommended" message
```

Bag image colour from `FERT_COLOUR`; display name from `FERT_LABEL`. Cash images from `net/cash/Picture{n}.png`.

---

### `html_cost_benefit(totalCost, totalRevenue, netRevenue, currency, ratios, lang)` — `R/html_helpers.R`

```r
# ratios : named list(fertCost = n, totalSale = n, revenue = n)
#          integers 1–10 selecting net/cash/Picture{n}.png
#
# Returns: <section> with three rows:
#   Total cost:               {formatted amount}  [cash image]
#   Total calculated revenue: {formatted amount}  [cash image]
#   Expected net revenue:     {formatted amount}  [cash image]
```

---

### `html_recommendation(recText, lang)` — `R/html_helpers.R`

```r
# Returns: <section> with:
#   <h3>Recommendation generated on {format(Sys.Date(), "%B %d, %Y")}</h3>
#   <p>{recText — \n replaced with <br>}</p>
```

---

### `html_table(df, col_names = NULL)` — `R/html_helpers.R`

```r
# df        : data frame
# col_names : optional column header overrides
# Returns: <table> HTML with <thead> and <tbody>, WeasyPrint-compatible
```

Used for the PP cost-of-LMO table.

---

### `build_fr_pdf(rr, fertilizers, user, country, userField, area, areaUnits, PD, HD, lat, lon, rootUP, cassPD, cassUW, maxInv, recText, lang, out_path)` — `R/pdf_builders.R`

Internal steps:
1. `fr <- calc_fertilizer_recom(fertilizers, rr)`.
2. Compute cost/revenue totals and cash-stack ratios (logic from `fertilizerAdviseTable()`).
3. Assemble HTML:
   - `html_page_header("Tailored Fertilizer Recommendation", lang, banner = "net/Akilimo_Dashboard_FR.png")`
   - Two-column grid: left = personal info + fertilizer prices; right = map + expected production
   - Full-width: fertilizer recommendation rows
   - Full-width: cost-benefit
   - Full-width: recommendation text + closing `</body></html>`
4. `render_pdf(html, out_path)`.

---

### `build_ic_pdf(..., subtype, maize_or_potato_info, recText, lang, out_path)` — `R/pdf_builders.R`

```r
# subtype              : "IC" (NG maize) | "CIS" (TZ sweet potato)
# maize_or_potato_info : named list
#   IC-NG:  list(maizeUP, maizeUW, maizePD, cobUP, CMP)
#   CIS-TZ: list(sweetPotatoUP, sweetPotatoUW, sweetPotatoPD, tuberUP)
```

Banner: `Akilimo_Dashboard_IC.png` or `Akilimo_Dashboard_CIS.png`. Right column shows expected cob increase instead of cassava tonnes.

---

### `build_pp_pdf(res, costLMO, user, country, userField, area, areaUnits, PD, HD, lat, lon, rootUP, cassPD, cassUW, ploughing, ridging, method_ploughing, method_ridging, recText, lang, out_path)` — `R/pdf_builders.R`

- Renders PP ggplot matrix: `ggplot2::ggsave(tp("pp_chart.png"), plot, width=9, height=5, dpi=150)`.
- Embeds chart and map as base64.
- Layout: top two-column (personal info + practice | LMO table), bottom two-column (map | chart).

---

### `build_sp_pdf(ds, user, country, userField, area, areaUnits, PD, HD, lat, lon, cassUP, cassUW, cassPD, saleSF, nameSF, PD_window, HD_window, recText, lang, out_path)` — `R/pdf_builders.R`

- Renders SP heatmap: `ggplot2::ggsave(tp("spgg.png"), gg, width=12, height=8, dpi=150)`.
- Embeds heatmap and map as base64.
- Layout: top three-column (personal info | current practice with windows | cost info), bottom two-column (map | SP heatmap).

---

## 7. HTML Structure per Recommendation Type

### FR — Tailored Fertilizer Recommendation

```
[banner image — full width]
──────────────────────────────────────────────────────
┌──────────────────────┬─────────────────────────────┐
│ What you told us     │ Your location               │
│ name, phone, field,  │ [map — base64]              │
│ area, dates, yield   │                             │
│                      │ Expected gain               │
│ Fertilizer prices    │ {tonnes} ~ {bags} × 100 kg  │
│ [price table]        │                             │
│                      │                             │
│ Cassava @ {price}    │                             │
│ Max investment: ...  │                             │
└──────────────────────┴─────────────────────────────┘
──────────────────────────────────────────────────────
Recommendation generated on {date}
[fertilizer rows: name · kg · bags · [bag images] · cost]
──────────────────────────────────────────────────────
Cost benefit analysis
Total cost:               {amount}  [cash image]
Total calculated revenue: {amount}  [cash image]
Expected net revenue:     {amount}  [cash image]
──────────────────────────────────────────────────────
[recommendation text]
```

### PP — Cassava Tillage Advice

```
[banner image — full width]
──────────────────────────────────────────────────────
┌──────────────────────┬─────────────────────────────┐
│ What you told us     │ Cost of land management ops │
│ name, phone, field,  │ [html_table(costLMO)]       │
│ area, dates          │                             │
│ Current practice:    │                             │
│ {ploughing/ridging}  │                             │
│ Cassava @ {price}    │                             │
└──────────────────────┴─────────────────────────────┘
┌──────────────────────┬─────────────────────────────┐
│ Your location        │ Cost-benefit analysis       │
│ [map — base64]       │ [ggplot matrix — base64]    │
└──────────────────────┴─────────────────────────────┘
──────────────────────────────────────────────────────
[recommendation text]
```

### SP — Scheduled Planting Advice

```
[banner image — full width]
──────────────────────────────────────────────────────
┌────────────┬──────────────────────┬────────────────┐
│ What you   │ Your current         │ Cost info      │
│ told us    │ practice             │                │
│            │ Planting: {date}     │ Sold to:       │
│            │ Window: {n} months   │ {factory}      │
│            │ Harvest: {date}      │ or             │
│            │ Window: {n} months   │ @ {price}/unit │
└────────────┴──────────────────────┴────────────────┘
┌──────────────────────┬─────────────────────────────┐
│ Your location        │ Expected gain               │
│ [map — base64]       │ [SP heatmap — base64]       │
└──────────────────────┴─────────────────────────────┘
──────────────────────────────────────────────────────
[recommendation text]
```

---

## 8. Map Generation

**Decision: Keep the existing `leaflet` + `mapview::mapshot()` approach.**

WeasyPrint does not make network requests during rendering. All images must be embedded as base64 or accessible as local files. `mapshot()` saves the map as a local PNG which is then base64-encoded — this is exactly the right pattern.

Inside `html_location_map()`:
1. Build leaflet map (same code as current Rmd chunks).
2. `mapshot(my_map, file = tp("map.png"), height = height_px)`.
3. Return `img_base64(tp("map.png"))`.

---

## 9. Chart Generation

Both ggplot charts are saved to the per-request temp dir and embedded as base64.

### PP matrix heatmap

```r
chart_path <- tp("pp_chart.png")
ggplot2::ggsave(chart_path, plot, width = 9, height = 5, units = "in", dpi = 150)
# → 1350 × 750 px PNG, sharp at ~6in in A4
chart_html <- img_base64(chart_path)
```

### SP planting-date heatmap

```r
chart_path <- tp("spgg.png")
ggplot2::ggsave(chart_path, gg, width = 12, height = 8, units = "in", dpi = 150)
chart_html <- img_base64(chart_path)
```

The stray global `if (file.exists("spgg.png")) file.remove("spgg.png")` blocks in the old `generate_pdfs()` are removed.

---

## 10. Language Support

`lang` is passed as a parameter to every builder and helper function.

### Two-system design (implemented)

PDF user-facing strings are handled by **two separate systems** with different scopes:

| System | Location | Used for | Accessor |
|---|---|---|---|
| `translations.csv` | `data/input/translations.csv` | Recommendation body text (shared across SMS, email, HTML) | `tr(key, lang)` |
| `.PDF_LABELS` | `R/html_helpers.R` | PDF section headings and UI labels (PDF-only) | `html_label(key, lang)` |

### Why not a single system?

`translations.csv` + `tr()` is used by the SMS and email paths as well as HTML reports. Several PDF label strings contain raw HTML (e.g. `<strong>` tags in `expected_gain_fmt`). Putting those in `translations.csv` would cause HTML markup to appear in SMS messages if the key were ever called from a non-HTML context.

Additionally, `translations.csv` holds agronomic recommendation content (what the farmer should do). PDF section headings like `"What you told us"` are UI chrome — mixing them with recommendation content makes the CSV harder to hand to a translator who should only touch agronomic text.

### Adding a new language

To add a new language (e.g. French `fr`):

1. Add a `fr` column to `data/input/translations.csv` for recommendation text.
2. Add `fr` entries to each key in `.PDF_LABELS` in `R/html_helpers.R`.
3. Pass `lang = "fr"` through the call stack — no other changes needed.

### Upgrade path

If PDF labels need to be editable without a code deployment (e.g. by a translator), migrate `.PDF_LABELS` to `data/input/pdf_labels.csv` and load it at startup:

```r
.PDF_LABELS <- local({
  path <- file.path(akpath, "data", "input", "pdf_labels.csv")
  df   <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  setNames(Map(function(en, sw) c(en = en, sw = sw), df$en, df$sw), df$key)
})
```

This keeps the `html_label()` API unchanged while making the backing store a CSV. Only do this if there is a concrete operational need — the in-code list is simpler and all these strings are stable UI chrome.

---

## 11. Integration Points

### `R/AkilimoMain.R` — `run_akilimo()`

Change the `generate_pdfs()` call from:

```r
PDFs <- generate_pdfs(
    user    = params$user,
    FR      = params$FR,
    IC      = params$IC,
    PP      = params$PP,
    SP      = params$SPP || params$SPH,
    country = params$country
)
```

to:

```r
PDFs <- generate_pdfs(
    user    = params$user,
    FR      = params$FR,
    IC      = params$IC,
    PP      = params$PP,
    SP      = params$SPP || params$SPH,
    country = params$country,
    lang    = params$lang,
    result  = result,
    params  = params
)
```

---

### `R/process-FR.R`

Remove:
```r
FR_MarkdownText(rr = response, fertilizers = fertilizers, ...)
fertilizerAdviseTable(FR = TRUE, IC = FALSE, country = country, areaUnits = areaUnits)
write.csv(recText, tp('FR_recText.csv'), row.names = FALSE)
```

Add `fertilizers` to the return list:
```r
c(rec_type = "FR", recommendation = recText, response,
  fertilizers = list(fertilizers))
```

---

### `R/process-IC.R`

Both `process_IC_NG()` and `process_IC_TZ()`:
- Remove `IC_MarkdownText()` / `CIS_MarkdownText()` and `fertilizerAdviseTable()` calls.
- Remove `write.csv(recText, ...)` calls.
- Add to return value: `fertilizers`, crop-specific price fields.

---

### `R/process-PP.R`

Remove `PP_MarkdownText()`, `write.csv(res, tp('PP_rec.csv'), ...)`, `write.csv(recText, ...)`.
Add to return value: `res`, `costLMO`, `recText`.

---

### `R/process-SP.R`

Remove `SP_MarkdownText()`, `write.csv(ds, tp("SP_rec.csv"), ...)`, `write.csv(recText, ...)`.
Add to return value: `ds`, `recText`.

---

### `R/sms_email.R` — `generate_pdfs()`

New signature:
```r
generate_pdfs <- function(user, FR, IC, PP, SP, country, lang, result, params)
```

New body (outline):
```r
PDFs <- NULL
add_pdf <- function(f) { PDFs <<- c(PDFs, f); f }
phone <- safe_filename_part(user$PhoneNr)

if (FR && !IC && !is.null(result$data)) {
    fname <- add_pdf(tp(paste0("fertilizer_advice_", phone, ".pdf")))
    build_fr_pdf(..., lang = lang, out_path = fname)
}

if (FR && IC && !is.null(result$data)) {
    subtype <- if (params$country == "TZ") "CIS" else "IC"
    prefix  <- if (params$country == "TZ") "CIS_advice_" else "intercrop_advice_"
    fname   <- add_pdf(tp(paste0(prefix, phone, ".pdf")))
    build_ic_pdf(..., subtype = subtype, lang = lang, out_path = fname)
}

if (PP && !is.null(result$res)) {
    fname <- add_pdf(tp(paste0("PP_advice_", phone, ".pdf")))
    build_pp_pdf(..., lang = lang, out_path = fname)
}

if (SP && !is.null(result$ds)) {
    fname <- add_pdf(tp(paste0("SP_advice_", phone, ".pdf")))
    build_sp_pdf(..., lang = lang, out_path = fname)
}

PDFs
```

---

### `R/markdown.R`

No interface changes. Functions that remain (used by `pdf_builders.R`):
- `tp()`, `temp_dir()`, `set_temp_dir()`, `safe_filename_part()`
- `FERT_COLOUR`, `FERT_LABEL`
- `calc_fertilizer_recom()`, `pivot_fertilizers_wide()`, `round_bags()`

---

## 12. Deletion List

### Rmd files

```
Rmd/FR_markdown_VFT.Rmd     Rmd/FR_markdown_swa.Rmd
Rmd/IC_markdown_VFT.Rmd     Rmd/CIS_markdown_swa.Rmd
Rmd/PP_markdownVFT.Rmd      Rmd/PP_markdown_swa.Rmd
Rmd/SP_markdownVFT.Rmd      Rmd/SP_markdown_swa.Rmd
Rmd/map.png                 Rmd/spgg.png
```

### Functions to remove from `R/markdown.R`

```
FR_MarkdownText()       IC_MarkdownText()
CIS_MarkdownText()      PPSP_MarkdownText()
PP_MarkdownText()       SP_MarkdownText()
fertilizerAdviseTable() get_markdown_text()
```

### CSV files no longer written per request

```
FR_MarkDownText.csv     IC_MarkDownText.csv     CIS_MarkDownText.csv
PP_MarkDownText.csv     SP_MarkDownText.csv     FR_recText.csv
IC_recText.csv          PP_recText.csv          SP_recText.csv
PP_rec.csv              SP_rec.csv              datall1..5.csv
totalCostmoney.csv      totalSalemoney.csv      totalRevenuemoney.csv
costLMO.csv             personalized_info_{phone}.csv
```

---

## 13. Testing Strategy

### Rewritten `tests/test_pdf.R`

Removes PhantomJS dependency check. Calls `build_*_pdf()` directly.

```r
check_pdf <- function(label, path) {
  cat(sprintf("%-55s", paste0(label, " ... ")))
  valid <- file.exists(path) &&
           file.size(path) > 5000 &&          # blank pages ≈ 1KB
           rawToChar(readBin(path, "raw", n = 4)) == "%PDF"
  if (valid) { cat("PASS\n"); pass <<- pass + 1L }
  else       { cat("FAIL\n"); fail <<- fail + 1L }
}
```

| Test | Fixture | Builder |
|---|---|---|
| FR English (NG) | `in_2_NG_FR_default_prices_riskAtt2_v1` | `build_fr_pdf(..., lang="en")` |
| FR Swahili (TZ) | `in_34_TZ_FR_sw_riskAtt2` | `build_fr_pdf(..., lang="sw")` |
| IC English (NG) | `in_18_NG_IC_fresh_cob_manual_sms_email_riskAtt1` | `build_ic_pdf(..., subtype="IC")` |
| CIS Swahili (TZ) | `in_33_TZ_IC_CIS_riskAtt2` | `build_ic_pdf(..., subtype="CIS")` |
| PP English (NG) | `in_30_NG_PP_riskAtt0` | `build_pp_pdf(..., lang="en")` |
| PP Swahili (TZ) | `in_31_TZ_PP_riskAtt0` | `build_pp_pdf(..., lang="sw")` |
| SP English (NG) | `in_29_NG_SP_riskAtt0` | `build_sp_pdf(..., lang="en")` |
| SP Swahili (TZ) | `in_32_TZ_SP_riskAtt0` | `build_sp_pdf(..., lang="sw")` |

Additional checks:
- Each PDF must be > 5 KB (guards against blank WeasyPrint output).
- Verify `render_pdf()` raises a clear error when `weasyprint` is not on PATH.
- `tests/test_full.R` (3203 regression cases) must pass unchanged throughout all phases.

---

## 14. Rollback Plan

### During Phases 1–3

Rmd files are not yet deleted. Old CSV-writing functions still exist alongside new HTML builders. To revert: restore the old `generate_pdfs()` body in `sms_email.R`.

### Feature-flag approach (recommended for production cutover)

```r
# In generate_pdfs():
use_weasyprint <- nchar(Sys.which("weasyprint")) > 0 &&
                  identical(Sys.getenv("AKILIMO_PDF_ENGINE"), "weasyprint")
```

- Default: old `webshot::rmdshot()` path (while WeasyPrint is being validated).
- `AKILIMO_PDF_ENGINE=weasyprint`: new `build_*_pdf()` path.

Set in the systemd service unit once verified stable. Remove the webshot branch in Phase 4.

### After Phase 4 (full cutover)

All Rmd files and old functions are preserved in Git history. To roll back:
1. `git revert` Phase 4 deletion commit and Phase 2–3 process/sms_email commits.
2. Reinstall PhantomJS: `webshot::install_phantomjs()`.
3. Revert `install_packages.R`.

Estimated rollback time: < 30 minutes including package reinstall.
