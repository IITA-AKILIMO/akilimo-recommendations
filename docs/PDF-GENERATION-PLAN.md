# PDF Generation Plan: Replacing Rmd/webshot with htmltools + pagedown

## 1. Executive Summary

### What changes

The current PDF pipeline uses `webshot::rmdshot()` (driven by PhantomJS, which is unmaintained) to render one of eight flexdashboard Rmd templates. Each Rmd reads data from CSV files that the R processor functions wrote to a per-request temp directory. This is a three-layer serialisation: R objects → CSV → Rmd knit → PhantomJS screenshot → PDF.

The replacement removes the intermediate layers entirely:

- The eight Rmd templates (`Rmd/*.Rmd`) are deleted.
- The CSV-writing functions in `markdown.R` (`FR_MarkdownText`, `IC_MarkdownText`, `CIS_MarkdownText`, `PPSP_MarkdownText`, `fertilizerAdviseTable`, and associated helpers) are replaced by in-memory HTML-building functions.
- `generate_pdfs()` in `sms_email.R` is rewritten to call `pagedown::chrome_print()` on the in-memory HTML.
- `webshot` and `flexdashboard` are removed from the dependency list.

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
| Renderer | PhantomJS (deprecated 2018) | Chrome/Chromium (maintained) |
| Data path | R → 10–15 CSV files → Rmd reads them back | R objects passed directly |
| Template count | 8 Rmd files (en + sw × 4 types) | 4 R functions + 1 `lang` parameter |
| Startup overhead | PhantomJS cold-boot + knitr knit | Chrome headless |
| CSS overrides | Needed (navbar, chart-title hacks) | Not needed (plain HTML) |
| Debugging | Rmd errors are opaque | HTML inspectable in browser |

---

## 2. New Dependency: pagedown

### Package

```r
install.packages("pagedown")
```

`pagedown` wraps Chrome/Chromium headless (`--print-to-pdf`) and produces print-quality PDFs with proper page-break control via CSS Paged Media. No PhantomJS, no Java.

### Chrome requirement

`pagedown::chrome_print()` locates Chrome via the `PAGEDOWN_CHROME` environment variable, then common system paths.

**Linux (production server)**

```bash
apt-get install -y chromium-browser
# or
apt-get install -y google-chrome-stable
```

Verify: `chromium-browser --version`

**Docker / CI**

```dockerfile
RUN apt-get update && apt-get install -y chromium-browser --no-install-recommends
ENV PAGEDOWN_CHROME=/usr/bin/chromium-browser
```

**Windows (development)**

Chrome is typically already installed. No extra steps.

### install_packages.R changes

Replace:
```r
"webshot",
"knitr",
"rmarkdown",
"flexdashboard",
```
with:
```r
"pagedown",
"base64enc",
```

Remove the `webshot::install_phantomjs()` block.

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
        (NO CSV writes for HTML input)

generate_pdfs(user, FR, IC, PP, SP, country, lang, result, params)
    └── build_fr_pdf(rr, fertilizers, user, country, ..., lang)
              ├── calc_fertilizer_recom()    (existing, unchanged)
              ├── html_page_header()
              ├── html_personal_info()
              ├── html_fertilizer_table()
              ├── html_cost_benefit()
              ├── html_location_map()        → map.png in temp dir
              ├── html_recommendation()
              └── render_pdf(html, out_path)
                        ├── write HTML to temp file
                        └── pagedown::chrome_print() → PDF
```

The per-request temp directory (`temp/<uuid>/`) is still created by `setup_temp_dir()` but now only holds the map PNG, the ggplot chart PNG (SP/PP), and the final PDFs. No CSV intermediaries.

---

## 4. New File Structure

### Files to CREATE

| Path | Purpose |
|---|---|
| `R/pdf_builders.R` | `build_fr_pdf()`, `build_ic_pdf()`, `build_pp_pdf()`, `build_sp_pdf()`, `render_pdf()` |
| `R/html_helpers.R` | Shared HTML fragment generators: `html_page_header()`, `html_personal_info()`, `html_location_map()`, `html_fertilizer_table()`, `html_cost_benefit()`, `html_recommendation()`, `html_table()`, `img_base64()` |
| `net/akilimo_print.css` | Print-optimised stylesheet (`@page` rules, two/three-column grid, no dashboard chrome) |

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
| `install_packages.R` | Swap `webshot`/`flexdashboard` for `pagedown`/`base64enc` |
| `tests/test_pdf.R` | Rewrite to call `build_*_pdf()` directly |
| `CLAUDE.md` | Update architecture section, key modules table |

### Files to DELETE

All 8 Rmd templates plus stray render artefacts:

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

1. Add `pagedown` and `base64enc` to `install_packages.R`. Remove `webshot` and PhantomJS block.
2. Create `net/akilimo_print.css`:
   - `@page { size: A4; margin: 15mm; }`
   - `.page-break { page-break-before: always; }`
   - Two-column grid: `.grid-2col { display: grid; grid-template-columns: 1fr 1fr; gap: 16px; }`
   - Three-column grid: `.grid-3col { display: grid; grid-template-columns: 1fr 1fr 1fr; gap: 16px; }`
   - Section headings: `h3 { font-size: 16px; font-weight: normal; border-bottom: 1px solid #ccc; }`
   - Table striping, no outer borders.
   - No flexdashboard-specific overrides needed.
3. Create `R/html_helpers.R` with stub implementations of all shared helpers.
4. Verify `pagedown::chrome_print()` works on the server with a "Hello World" HTML string.

**Deliverable:** `net/akilimo_print.css`, `R/html_helpers.R` stubs, confirmed Chrome path.

---

### Phase 2 — FR PDF Builder

**Scope:** First complete builder end-to-end. Proves the pattern before touching IC/PP/SP.

1. Create `R/pdf_builders.R` with `build_fr_pdf()` and `render_pdf()` (full spec in §6).
2. Implement all `html_helpers.R` functions needed for FR.
3. Modify `process-FR.R`:
   - Remove `FR_MarkdownText()` and `fertilizerAdviseTable()` calls.
   - Remove `write.csv(recText, tp('FR_recText.csv'), ...)`.
   - Add `fertilizers` to the return list.
4. Modify `AkilimoMain.R` `run_akilimo()`: pass `result` and `params` to `generate_pdfs()`.
5. Modify `generate_pdfs()` in `sms_email.R`: for the FR branch, call `build_fr_pdf()` instead of `webshot::rmdshot()`.
6. Run `Rscript tests/test_pdf.R` (FR cases). Visually compare output against `pdf-samples/FR.pdf`.

**Deliverable:** FR PDF generated via Chrome, visually verified.

---

### Phase 3 — IC, PP, SP PDF Builders

**Scope:** Remaining three builders following the Phase 2 pattern.

**IC builder (`build_ic_pdf`)**
- `subtype` parameter: `"IC"` (NG maize) or `"CIS"` (TZ sweet potato).
- Remove `IC_MarkdownText()`, `CIS_MarkdownText()`, `fertilizerAdviseTable()` from `process-IC.R`.
- Add maize/sweet-potato price fields and `fertilizers` to the return value.

**PP builder (`build_pp_pdf`)**
- `res` (ploughing × ridging matrix with `dNR`, `dTC`) comes from `getPPrecommendations()`.
- Renders the PP ggplot matrix to `tp("pp_chart.png")`, embeds via `img_base64()`.
- `costLMO` data frame rendered as HTML table.
- Remove `PP_MarkdownText()` and CSV writes from `process-PP.R`; add `res`, `costLMO`, `recText` to return value.

**SP builder (`build_sp_pdf`)**
- `ds` (PD × HD grid with `dGR`) rendered as SP heatmap to `tp("spgg.png")`, embedded via `img_base64()`.
- Remove `SP_MarkdownText()` and CSV writes from `process-SP.R`; add `ds`, `recText` to return value.
- Remove stray `spgg.png` cleanup from `generate_pdfs()`.

**Deliverable:** All 8 PDF variants (en + sw × 4 types) passing `tests/test_pdf.R`.

---

### Phase 4 — Remove Old Pipeline

**Scope:** Delete obsolete files and functions once all builders pass tests.

1. Delete all 8 `Rmd/*.Rmd` files and stray PNGs.
2. Remove dead functions from `R/markdown.R` (see §12).
3. Remove the `costLMO.csv` write in `get_costLMO()` — was only consumed by the PP Rmd.
4. Remove `webshot`, `knitr`, `rmarkdown`, `flexdashboard` from `install_packages.R`.
5. Remove any remaining `write.csv(recText, ...)` calls no longer consumed.
6. Confirm the per-request temp dir now only contains: `map.png`, `spgg.png`/`pp_chart.png` (when applicable), and the output PDFs.

**Deliverable:** Clean repo with no Rmd references in live code.

---

### Phase 5 — Testing and Cleanup

1. Rewrite `tests/test_pdf.R` (see §13).
2. Run `tests/test_full.R` (3203 regression cases) — must all pass.
3. Visual comparison of each new PDF against `pdf-samples/`.
4. Load-test concurrent requests (each gets isolated temp dir; Chrome processes do not collide).
5. Update `CLAUDE.md` architecture section and `docs/SETUP.md` Chrome installation notes.
6. Remove `flexdashboard` from `renv.lock` or package manifest if present.

---

## 6. Detailed Function Specifications

### `render_pdf(html, path)` — `R/pdf_builders.R`

```r
render_pdf(html, path)
# html : character(1) — complete HTML document string
# path : character(1) — absolute path for the output PDF
# Returns: path invisibly; stops on error
```

Writes `html` to `tp("render_tmp.html")` (so Chrome can resolve relative-path assets from the temp dir), calls `pagedown::chrome_print(input = html_tmp, output = path, wait = 5)`, then deletes the temp HTML file.

---

### `img_base64(path, alt = "")` — `R/html_helpers.R`

```r
img_base64(path, alt = "")
# Returns: character(1) — <img src="data:image/png;base64,..." alt="...">
```

Uses `base64enc::base64encode(readBin(path, "raw", file.info(path)$size))`. Adds `base64enc` to the dependency list (commonly already a transitive dependency).

---

### `html_page_header(title, lang, banner_path)` — `R/html_helpers.R`

```r
html_page_header(title, lang, banner_path = NULL)
# title       : character(1) — document title
# lang        : "en" | "sw"
# banner_path : character(1) — path to banner PNG; NULL omits it
# Returns: character(1) — full <!DOCTYPE html><head>...<body> opening
#          including <link> to akilimo_print.css and banner <img> if provided
```

Inlines the banner as base64. Sets `lang` attribute on `<html>` tag.

---

### `html_personal_info(user, country, userField, area, areaUnits, PD, HD, current_yield = NULL, lang)` — `R/html_helpers.R`

```r
# Returns: character(1) — <section> with definition list:
#   Name, Phone, Field, Field area, Planting date, Harvest date,
#   and optionally Current yield (FR and IC only).
```

Dates formatted as `day-Mon-YYYY`. `current_yield` rounded to 0 decimal places.

---

### `html_location_map(lat, lon, height_px = 150)` — `R/html_helpers.R`

```r
# Returns: character(1) — <img> tag with map embedded as base64
```

Uses the existing `leaflet` + `mapview::mapshot()` approach. Saves to `tp("map.png")`, reads back, base64-encodes. PNG remains on disk until temp dir TTL expires (1 hour).

---

### `html_fertilizer_table(fr, area, areaUnits, currency, rootUP, cassPD, cassUW, maxInv, lang)` — `R/html_helpers.R`

```r
# fr        : data frame from calc_fertilizer_recom()
# Returns: character(1) — <section> containing:
#   - Fertilizer price table (type × cost-per-bag)
#   - One recommendation row per fertilizer: name, kg, bags, bag images, total cost
#   - Cassava price and max investment lines
#   - If nrow(fr) == 0: "No fertilizer recommended" message
```

Bag image colour uses `FERT_COLOUR`; label uses `FERT_LABEL`. Images embedded as base64. Cash-stack images from `net/cash/Picture{n}.png`.

---

### `html_cost_benefit(totalCost, totalRevenue, netRevenue, currency, ratios, lang)` — `R/html_helpers.R`

```r
# ratios : named list(fertCost = n, totalSale = n, revenue = n)
#          — integers 1–10 selecting Picture{n}.png from net/cash/
# Returns: character(1) — <section> with three rows:
#   Total cost:             {formatted amount}  [cash image]
#   Total calculated revenue: ...
#   Expected net revenue:   ...
```

---

### `html_recommendation(recText, lang)` — `R/html_helpers.R`

```r
# Returns: character(1) — <section> with:
#   <h3>Recommendation generated on {format(Sys.Date(), "%B %d, %Y")}</h3>
#   <p>{recText with \n replaced by <br>}</p>
```

---

### `html_table(df, col_names = NULL)` — `R/html_helpers.R`

```r
# df        : data frame
# col_names : character vector of column header labels (NULL = use df names)
# Returns: character(1) — <table> HTML with thead and tbody
```

Used for the PP cost-of-LMO table.

---

### `build_fr_pdf(rr, fertilizers, user, country, userField, area, areaUnits, PD, HD, lat, lon, rootUP, cassPD, cassUW, maxInv, recText, lang, out_path)` — `R/pdf_builders.R`

```r
# rr          : list — $data (data frame row), $fertilizer_rates
# fertilizers : data frame from get_fertilizers2()
# recText     : character(1) from getFRrecText()
# out_path    : character(1) output PDF path
# Returns: out_path invisibly
```

Internal steps:
1. `fr <- calc_fertilizer_recom(fertilizers, rr)`.
2. Compute cost/revenue totals and cash-stack ratios (logic from `fertilizerAdviseTable()`).
3. Assemble HTML: header → two-column grid (personal info + prices | map + production) → fertilizer recommendation rows → cost-benefit → recommendation text.
4. Call `render_pdf(html, out_path)`.

---

### `build_ic_pdf(..., subtype, maize_or_potato_info, recText, lang, out_path)` — `R/pdf_builders.R`

```r
# subtype              : "IC" (NG maize) | "CIS" (TZ sweet potato)
# maize_or_potato_info : named list with crop-specific price fields
#   IC-NG:  list(maizeUP, maizeUW, maizePD, cobUP, CMP)
#   CIS-TZ: list(sweetPotatoUP, sweetPotatoUW, sweetPotatoPD, tuberUP)
```

Structure mirrors FR. Uses `Akilimo_Dashboard_IC.png` or `Akilimo_Dashboard_CIS.png` banner. Right column shows "Expected increase in cobs: {dMP}" instead of cassava tonnes.

---

### `build_pp_pdf(res, costLMO, user, country, userField, area, areaUnits, PD, HD, lat, lon, rootUP, cassPD, cassUW, maxInv, ploughing, ridging, method_ploughing, method_ridging, recText, lang, out_path)` — `R/pdf_builders.R`

```r
# res     : data frame from getPPrecommendations() (ploughing/ridging matrix)
# costLMO : data frame (operations, methods, costs)
```

- Renders PP ggplot matrix to `tp("pp_chart.png")` via `ggplot2::ggsave()`, embeds as base64.
- `costLMO` rendered via `html_table()`.
- Layout: two-column top (personal info + practice | LMO table), two-column bottom (map | chart).

---

### `build_sp_pdf(ds, user, country, userField, area, areaUnits, PD, HD, lat, lon, cassUP, cassUW, cassPD, saleSF, nameSF, PD_window, HD_window, recText, lang, out_path)` — `R/pdf_builders.R`

```r
# ds : data frame from getSPrecommendations() (PD × HD grid with dGR)
```

- Renders SP heatmap to `tp("spgg.png")` via `ggplot2::ggsave()`, embeds as base64.
- Three-column top row (personal info | current practice with windows | cost info), two-column bottom (map | SP heatmap).

---

## 7. HTML Structure per Recommendation Type

### FR — Fertilizer Recommendation

```
[banner image — full width]
─────────────────────────────────────────────────────
┌──────────────────────┬────────────────────────────┐
│ What you told us     │ Your location              │
│ name, phone, field,  │ [map image]                │
│ area, dates, yield   │                            │
│                      │ Expected gain              │
│ Fertilizer prices    │ {tonnes} ~ {bags} × 100kg  │
│ [price table]        │                            │
│                      │                            │
│ Cassava @ {price}    │                            │
│ Max investment: ...  │                            │
└──────────────────────┴────────────────────────────┘
─────────────────────────────────────────────────────
Recommendation generated on {date}
[per-fertilizer rows: name · kg · bags · bag images · cost]
─────────────────────────────────────────────────────
Cost benefit analysis
Total cost:              {amount} [cash image]
Total calculated revenue:{amount} [cash image]
Expected net revenue:    {amount} [cash image]
─────────────────────────────────────────────────────
[recommendation text]
```

### PP — Cassava Tillage Advice

```
[banner image — full width]
─────────────────────────────────────────────────────
┌──────────────────────┬────────────────────────────┐
│ What you told us     │ Cost of LMO                │
│ name, phone, field,  │ [costLMO table]            │
│ area, dates          │                            │
│ Current practice:    │                            │
│ {ploughing/ridging}  │                            │
│ Cassava @ {price}    │                            │
└──────────────────────┴────────────────────────────┘
┌──────────────────────┬────────────────────────────┐
│ Your location        │ Cost-benefit analysis      │
│ [map image]          │ [ggplot heatmap]           │
└──────────────────────┴────────────────────────────┘
─────────────────────────────────────────────────────
[recommendation text]
```

### SP — Scheduled Planting Advice

```
[banner image — full width]
─────────────────────────────────────────────────────
┌──────────┬─────────────────────┬──────────────────┐
│ What you │ Your current        │ Cost information │
│ told us  │ practice            │                  │
│          │ Planting: {date}    │ Cassava sold to: │
│          │ Window: {n} months  │ {factory}        │
│          │ Harvest: {date}     │ or               │
│          │ Window: {n} months  │ @ {price}/{unit} │
└──────────┴─────────────────────┴──────────────────┘
┌──────────────────────┬────────────────────────────┐
│ Your location        │ Expected gain in total     │
│ [map image]          │ production                 │
│                      │ [SP heatmap]               │
└──────────────────────┴────────────────────────────┘
─────────────────────────────────────────────────────
[recommendation text]
```

---

## 8. Map Generation

**Decision: Keep the existing `leaflet` + `mapview::mapshot()` approach.**

Chrome headless can embed PNGs as base64 without making live network tile requests, avoiding the need for an external maps API key.

Inside `html_location_map()`:
1. Build leaflet map (same code as current Rmd chunks).
2. `mapshot(my_map, file = tp("map.png"), height = height_px)`.
3. `img_base64(tp("map.png"))` — returns the `<img data:image/png;base64,...>` tag.
4. The PNG stays on disk until the temp dir is cleaned up (1 hour TTL).

---

## 9. Chart Generation

Both ggplot charts are saved to the per-request temp directory and embedded as base64, so Chrome does not need to resolve any file path.

### PP matrix heatmap

```r
chart_path <- tp("pp_chart.png")
ggplot2::ggsave(chart_path, plot, width = 9, height = 5, units = "in", dpi = 150)
# → 1350 × 750 px PNG
chart_html <- img_base64(chart_path)
```

### SP planting-date heatmap

```r
chart_path <- tp("spgg.png")
ggplot2::ggsave(chart_path, gg, width = 12, height = 8, units = "in", dpi = 150)
chart_html <- img_base64(chart_path)
```

The stray global `if (file.exists("spgg.png")) file.remove("spgg.png")` blocks in the old `generate_pdfs()` are removed — they existed because the Rmd rendered in the project root.

---

## 10. Language Support

`lang` is passed as a parameter to every builder and helper. All user-facing strings in the HTML are resolved via `tr(key, lang)`, matching how processor functions already localise the JSON response text.

Static section headings (currently hardcoded English in Rmd) must be added to `data/input/translations.csv`:

| Key | English | Swahili |
|---|---|---|
| `pdf_what_told` | "What you told us" | *(confirm with translator)* |
| `pdf_fert_prices` | "Fertilizer prices" | |
| `pdf_your_location` | "Your location" | |
| `pdf_exp_gain` | "Expected gain in total production" | |
| `pdf_cost_benefit` | "Cost benefit analysis" | |
| `pdf_lmo_cost` | "Cost of land management operations" | |
| `pdf_your_practice` | "Your current practice" | |
| `pdf_cost_info` | "Cost information" | |
| `pdf_rec_date` | "Recommendation generated on" | |

`tr()` falls back to English for missing Swahili entries; the Swahili values should be confirmed with the translation team before Phase 3 ships.

This replaces the current approach of maintaining two separate Rmd files per recommendation type.

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

No other changes to `AkilimoMain.R`.

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
- Add to return value: `fertilizers`, and the crop-specific price fields (`maize_or_potato_info`).

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
    subtype <- if (country == "TZ") "CIS" else "IC"
    prefix  <- if (country == "TZ") "CIS_advice_" else "intercrop_advice_"
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

No interface changes. The following remain and are used by `pdf_builders.R`:
- `tp()`, `temp_dir()`, `set_temp_dir()`, `safe_filename_part()`
- `FERT_COLOUR`, `FERT_LABEL`
- `calc_fertilizer_recom()`, `pivot_fertilizers_wide()`, `round_bags()`

---

## 12. Deletion List

### Rmd files

```
Rmd/FR_markdown_VFT.Rmd
Rmd/FR_markdown_swa.Rmd
Rmd/IC_markdown_VFT.Rmd
Rmd/CIS_markdown_swa.Rmd
Rmd/PP_markdownVFT.Rmd
Rmd/PP_markdown_swa.Rmd
Rmd/SP_markdownVFT.Rmd
Rmd/SP_markdown_swa.Rmd
Rmd/map.png
Rmd/spgg.png
```

### Functions to remove from `R/markdown.R`

```
FR_MarkdownText()
IC_MarkdownText()
CIS_MarkdownText()
PPSP_MarkdownText()
PP_MarkdownText()
SP_MarkdownText()
fertilizerAdviseTable()
get_markdown_text()
```

### CSV files no longer written per request

```
FR_MarkDownText.csv        IC_MarkDownText.csv
CIS_MarkDownText.csv       PP_MarkDownText.csv
SP_MarkDownText.csv        FR_recText.csv
IC_recText.csv             PP_recText.csv
SP_recText.csv             PP_rec.csv
SP_rec.csv                 datall1.csv … datall5.csv
totalCostmoney.csv         totalSalemoney.csv
totalRevenuemoney.csv      costLMO.csv
personalized_info_{phone}.csv
```

---

## 13. Testing Strategy

### Rewritten `tests/test_pdf.R`

Removes the PhantomJS dependency check. Calls `build_*_pdf()` directly with the result from running each fixture through `run_akilimo()`.

```r
check_pdf <- function(label, path) {
  cat(sprintf("%-55s", paste0(label, " ... ")))
  valid <- file.exists(path) &&
           file.size(path) > 5000 &&  # >5KB — blank pages are ~1KB
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

### Additional checks

- Each PDF must be > 5 KB (guards against blank Chrome output).
- Verify `render_pdf()` raises a clear error when Chrome is not found.
- `tests/test_full.R` (3203 regression cases) must pass unchanged throughout all phases.

---

## 14. Rollback Plan

### During Phases 1–3 (old and new both present)

The Rmd files are not yet deleted. Old CSV-writing functions still exist alongside new HTML builders. To revert: restore the old `generate_pdfs()` body in `sms_email.R`, remove the new builder calls.

### Feature-flag approach (lower risk for production cutover)

```r
# In generate_pdfs():
use_pagedown <- identical(Sys.getenv("AKILIMO_PDF_ENGINE"), "pagedown")
```

- Default (`webshot`): old `rmdshot()` path.
- `AKILIMO_PDF_ENGINE=pagedown`: new `build_*_pdf()` path.

Set in the systemd service unit once verified stable. Remove the webshot branch in Phase 4.

### After Phase 4 (full cutover)

All Rmd files and old functions are preserved in Git history. To roll back:
1. `git revert` the Phase 4 deletion commit and the Phase 2–3 process/sms_email commits.
2. Reinstall PhantomJS: `webshot::install_phantomjs()`.
3. Revert `install_packages.R`.

Estimated rollback time: < 30 minutes including package reinstall.
