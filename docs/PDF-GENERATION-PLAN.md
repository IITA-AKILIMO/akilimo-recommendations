# PDF Generation: WeasyPrint Pipeline

**Status: Implemented.** Migration from PhantomJS/Rmd/webshot to WeasyPrint is complete.
All four recommendation types (FR, IC, PP, SP) generate PDFs via WeasyPrint.
The old Rmd templates, PhantomJS dependency, and CSV intermediaries have been removed.

---

## 1. Summary of Changes

### What changed

| Concern | Before | After |
|---------|--------|-------|
| Renderer | PhantomJS (deprecated 2018) | WeasyPrint (Python, actively maintained) |
| Data path | R objects → 10–15 CSV files → Rmd knit → PhantomJS screenshot | R objects passed directly to HTML builders |
| Templates | 8 Rmd files (en + sw × 4 types) | 4 R functions + `lang` parameter |
| Browser dependency | PhantomJS (headless WebKit) | None — WeasyPrint is pure Python |
| Map | `leaflet` + `mapview::mapshot()` (requires pandoc) | Mapbox Static API → HTTP fallback → offline coordinate card |
| Headless charts | Required X11 display | `ragg` backend (no X11); Cairo fallback |
| Server footprint | PhantomJS binary (~50 MB) + pandoc | `pip install weasyprint` |

### What stayed the same

- All recommendation computation: `process-FR.R`, `process-IC.R`, `process-PP.R`, `process-SP.R`, `quefts.R`, `optimize_fert.R`.
- The `run_akilimo()` / `dispatch_recommendations()` / `build_response()` orchestration in `AkilimoMain.R`.
- Translation system (`tr()`, `translations.csv`).
- `tp()` / `temp_dir()` / `set_temp_dir()` path helpers in `markdown.R`.
- All `net/*/` bag and cash PNG assets (now embedded as base64 in HTML).
- `sendEmailReport()`.
- All test fixtures in `tests/input/`.

---

## 2. WeasyPrint

WeasyPrint is a Python-based HTML/CSS → PDF converter. It implements the CSS Paged Media spec natively — no browser, no JavaScript engine, no Chrome. It supports CSS Grid (v60+), `@page` rules, and base64-embedded images.

### Installation

**Linux — system-wide (recommended for production)**

```bash
# System libraries required for font layout and image support
sudo apt-get install -y libpango-1.0-0 libpangoft2-1.0-0 libgdk-pixbuf2.0-0

# Install WeasyPrint system-wide so it is on the default systemd PATH
sudo pip3 install weasyprint

# On Debian 12+ / Ubuntu 23.04+ pip blocks installs outside a virtualenv:
sudo pip3 install --break-system-packages weasyprint

# Or use the system package manager (may be an older version):
sudo apt-get install -y python3-weasyprint

weasyprint --version   # verify
```

> **Systemd PATH trap:** Services run with a minimal PATH (`/usr/bin:/bin`). Installing with
> `pip install --user weasyprint` puts the binary in `~/.local/bin`, which the service user
> cannot find. Always install system-wide for production, or explicitly add the path to the
> service unit — see [docs/SETUP.md §Production deployment](SETUP.md#production-deployment-systemd).

**Windows (development)**

```powershell
pip install weasyprint
# GTK runtime also required:
# https://github.com/tschoonj/GTK-for-Windows-Runtime-Environment-Installer
```

See [WEASYPRINT-WINDOWS.md](WEASYPRINT-WINDOWS.md) for a detailed walkthrough.

**Docker**

```dockerfile
RUN apt-get update && apt-get install -y \
    python3-pip libpango-1.0-0 libpangoft2-1.0-0 libgdk-pixbuf2.0-0 \
    && pip3 install weasyprint
```

### How R calls WeasyPrint — `render_pdf()`

`render_pdf()` is in `R/pdf_builders.R`. It writes the assembled HTML to a temp file and calls WeasyPrint via `system2()`.

```r
render_pdf <- function(html, path) {
  html_tmp <- tp("render_tmp.html")
  writeLines(html, html_tmp, useBytes = TRUE)
  log_write("DEBUG", "render_pdf: HTML written to", html_tmp, "— rendering to", path)

  result <- tryCatch(
    system2("weasyprint",
            args   = c(shQuote(html_tmp), shQuote(path)),
            stdout = TRUE, stderr = TRUE),
    error = function(e) {
      log_write("ERROR", "render_pdf: system2 threw — HTML preserved at:", html_tmp)
      log_write("ERROR", "system2 error:", conditionMessage(e))
      stop("WeasyPrint could not be started (", conditionMessage(e),
           ") — HTML at: ", html_tmp)
    }
  )

  status <- attr(result, "status") %||% 0L
  if (!file.exists(path) || file.size(path) == 0 || (!is.null(status) && status != 0L)) {
    output <- paste(result, collapse = "\n")
    log_write("ERROR", "WeasyPrint exited", status, "— HTML preserved at:", html_tmp)
    log_write("ERROR", "WeasyPrint output:\n", output)
    stop("WeasyPrint failed (exit ", status, "):\n", output)
  }

  invisible(path)
}
```

Key points:
- All assets (banner images, bag PNGs, cash images, map PNGs, chart PNGs) are embedded as base64 data URIs — WeasyPrint does not make network requests during rendering.
- The HTML temp file is preserved on failure so it can be inspected manually or re-run with `weasyprint render_tmp.html debug.pdf`.
- The API logs a WeasyPrint version check at startup: `[INFO] WeasyPrint: weasyprint X.Y.Z` — if missing: `[ERROR] WeasyPrint not found`.

### CSS constraints

- Use `display: grid` with `grid-template-columns` — supported in WeasyPrint ≥ 60.
- `@page { size: A4; margin: 15mm; }` is fully supported.
- `break-before: page` / `page-break-before: always` both work.
- Avoid: `position: fixed`, `filter`, `backdrop-filter`, `clip-path` — not supported.
- CSS is inlined in a `<style>` tag (loaded from `net/akilimo_print.css` at build time) to make each HTML file self-contained.

---

## 3. Architecture

### Data flow

```
POST /compute (api.R)
    → run_akilimo() (AkilimoMain.R)
        → dispatch_recommendations() → process_FR / IC / PP / SP
              returns: result list (data, fertilizer_rates, recommendation, ...)

        → generate_pdfs(user, FR, IC, PP, SP, country, result, params)
              → build_fr_pdf()  / build_ic_pdf() / build_pp_pdf() / build_sp_pdf()
                    → html_open()              (banner + inlined CSS)
                    → html_personal_info()
                    → html_fertilizer_table()
                    → html_location_map()      (Mapbox → HTTP → offline card)
                    → html_cost_benefit()
                    → html_recommendation()
                    → render_pdf(html, out_path)
                          → writeLines(html, render_tmp.html)
                          → system2("weasyprint", ...) → PDF

        → sendEmailReport()   (if user$send_email)
```

The per-request temp directory (`temp/YYYYMMDD_HHMMSS_COUNTRY_TYPE_rand4/`) holds:
- `map.png` — static map image (if fetched)
- `pp_chart.png` / `spgg.png` — ggplot chart (PP and SP only)
- `render_tmp.html` — last HTML written (preserved for debugging on failure)
- `*.pdf` — output PDFs sent by email

---

## 4. File Structure

### Created

| File | Purpose |
|------|---------|
| `R/html_helpers.R` | HTML fragment builders and `.PDF_LABELS` label lookup |
| `R/pdf_builders.R` | `build_fr/ic/pp/sp_pdf()` + `render_pdf()` |
| `net/akilimo_print.css` | Print-optimised stylesheet (A4, two-column grid, WeasyPrint-compatible) |

### Modified

| File | Changes |
|------|---------|
| `R/markdown.R` | Removed all CSV-writing functions; kept `tp()`, `FERT_COLOUR`, `FERT_LABEL`, `calc_fertilizer_recom()` |
| `R/sms_email.R` | `generate_pdfs()` rewritten to call `build_*_pdf()`; `.try_pdf()` wrapper with per-type error isolation |
| `R/AkilimoMain.R` | Passes `result` and `params` into `generate_pdfs()` |
| `R/process-FR/IC/PP/SP.R` | Removed markdown/CSV calls; added structured data to return values |
| `api.R` | Added WeasyPrint startup check; headless ggplot2 device setup (`ragg` / Cairo fallback) |
| `install_packages.R` | Removed `webshot`, `flexdashboard`, `knitr`, `rmarkdown`, `leaflet`, `mapview`; added `DBI`, `RSQLite`, `ragg`, `base64enc` |

### Deleted

```
Rmd/FR_markdown_VFT.Rmd     Rmd/FR_markdown_swa.Rmd
Rmd/IC_markdown_VFT.Rmd     Rmd/CIS_markdown_swa.Rmd
Rmd/PP_markdownVFT.Rmd      Rmd/PP_markdown_swa.Rmd
Rmd/SP_markdownVFT.Rmd      Rmd/SP_markdown_swa.Rmd
```

CSV files that were previously written per request are no longer produced:
`FR_MarkDownText.csv`, `IC_MarkDownText.csv`, `PP_MarkDownText.csv`, `SP_MarkDownText.csv`,
`datall{1..5}.csv`, `totalCostmoney.csv`, `totalSalemoney.csv`, `totalRevenuemoney.csv`,
`costLMO.csv`, `PP_rec.csv`, `SP_rec.csv`, `personalized_info_{phone}.csv`, `*_recText.csv`.

---

## 5. Map Generation

Farm location maps use a three-tier resolution in `html_location_map()` (`R/html_helpers.R`):

1. **Mapbox Static Images API** — if `MAPBOX_TOKEN` is set in `.env`.
   URL format: `https://api.mapbox.com/styles/v1/mapbox/streets-v12/static/pin-s+ee4d5f({lon},{lat})/{lon},{lat},10,0/800x300@2x?access_token={token}`

2. **Generic HTTP map service** — if `MAP_API_URL` is set in `.env`.
   URL format: `{MAP_API_URL}?lat={lat}&lon={lon}&zoom=10&size=800x300`

3. **Offline coordinate card** — always available, no network required.
   Renders a styled `<div>` showing latitude and longitude values.
   CSS classes: `.location-coords`, `.coord-row`, `.coord-label`, `.coord-value` in `akilimo_print.css`.

HTTP fetches use `httr::GET()` with a 10-second timeout and `httr::write_disk()`. Failures are caught and logged; the fallback tier is tried automatically. PDF generation never fails due to map unavailability.

The previous approach (`leaflet` + `mapview::mapshot()`) was removed because `mapshot()` calls `htmlwidgets::saveWidget(selfcontained = TRUE)` internally, which requires pandoc — an unnecessary dependency.

---

## 6. Chart Generation (PP and SP)

ggplot2 charts are rendered to PNG in the per-request temp dir and embedded as base64.

### Headless rendering

On Linux servers without an X11 display, `png()` and the default ggplot2 device fail. The API uses:

1. **ragg** (`ragg::agg_png`) — preferred, highest quality, no X11.
   `ggsave()` calls in `pdf_builders.R` pass `device = ragg::agg_png` explicitly.
   On ggplot2 ≥ 3.5.0, `ggplot2::set_default_device(ragg::agg_png)` is also called at startup.
2. **Cairo** (`options(bitmapType = "cairo")`) — fallback if `ragg` is not installed.

Both are set in `api.R` at startup.

### PP matrix chart

```r
dev <- if (requireNamespace("ragg", quietly = TRUE)) ragg::agg_png else "png"
ggplot2::ggsave(tp("pp_chart.png"), pp_plot,
                width = 9, height = 5, units = "in", dpi = 150, device = dev)
```

Chart shows all ploughing × ridging combinations as coloured tiles with two text lines each:
net value change and cost. Tile text colour: green (positive), red (negative), grey (neutral).

### SP heatmap

```r
ggplot2::ggsave(tp("spgg.png"), sp_plot,
                width = 12, height = 8, units = "in", dpi = 150, device = dev)
```

---

## 7. Language Support

`lang` is passed as a parameter through every builder and helper function. Two separate label systems are used:

| System | Location | Used for | Accessor |
|--------|----------|----------|----------|
| `translations.csv` | `data/input/translations.csv` | Recommendation body text (shared with SMS/email) | `tr(key, lang)` |
| `.PDF_LABELS` | `R/html_helpers.R` | PDF section headings and UI chrome | `html_label(key, lang)` |

The split exists because `.PDF_LABELS` entries contain HTML markup (e.g. `<strong>` tags) that must not appear in SMS messages, and because agronomic content and UI labels should be maintained separately.

**Supported languages:** `en` (English, default), `sw` (Swahili).

**Adding a new language** (e.g. French `fr`):
1. Add a `fr` column to `data/input/translations.csv`.
2. Add `fr` entries to each key in `.PDF_LABELS` in `R/html_helpers.R`.
3. The `lang` parameter already flows through the full call stack — no other changes needed.

---

## 8. PDF Layout per Recommendation Type

### FR — Tailored Fertilizer Recommendation

```
[banner — full width]
┌──────────────────────┬──────────────────────────────┐
│ What you told us     │ Your location                │
│ name, field, area,   │ [map or coordinate card]     │
│ dates                │                              │
│                      │ Expected gain                │
│ Fertilizer prices    │ {tonnes cassava}             │
│ Cassava price        │                              │
│ Max investment       │                              │
└──────────────────────┴──────────────────────────────┘
Recommendation — [fertilizer rows with bag images]
Cost-benefit analysis — [cash-stack images]
[recommendation text]
```

### IC — Cassava-Maize Intercropping (NG) / Cassava-Sweet Potato (TZ)

```
[banner — full width]
┌──────────────────────┬──────────────────────────────┐
│ What you told us     │ Your location                │
│ Fertilizer prices    │ [map or coordinate card]     │
│ Maize/SP price       │                              │
│ Fertilizer rec rows  │ Expected extra production    │
│                      │ Cost-benefit analysis        │
└──────────────────────┴──────────────────────────────┘
[recommendation text]
```

### PP — Post-Planting Tillage Advice

```
[banner — full width]
┌──────────────────────┬──────────────────────────────┐
│ What you told us     │ Cost of land management ops  │
│ Current practice     │ [html_table(costLMO)]        │
│ Cassava price        │                              │
└──────────────────────┴──────────────────────────────┘
┌──────────────────────┬──────────────────────────────┐
│ Your location        │ Cost-benefit analysis        │
│ [map or coord card]  │ [PP ggplot matrix — base64]  │
└──────────────────────┴──────────────────────────────┘
[recommendation text]
```

### SP — Scheduled Planting Advice

```
[banner — full width]
┌────────────┬──────────────────────┬─────────────────┐
│ What you   │ Your current         │ Cost info       │
│ told us    │ practice             │ (price/factory) │
└────────────┴──────────────────────┴─────────────────┘
┌──────────────────────┬──────────────────────────────┐
│ Your location        │ Expected gain                │
│ [map or coord card]  │ [SP heatmap — base64]        │
└──────────────────────┴──────────────────────────────┘
[recommendation text]
```

---

## 9. Error Handling

PDF failures are isolated per recommendation type. If one PDF fails (e.g. IC), the others (PP, SP) are still attempted. The recommendation JSON response is always returned — PDF failures are never surfaced to API callers.

`.try_pdf()` in `sms_email.R` wraps each builder call:
```r
.try_pdf <- function(label, expr) {
  tryCatch(expr, error = function(e) {
    log_write("WARN", label, "PDF generation failed:", conditionMessage(e))
    warning(label, " PDF generation failed: ", conditionMessage(e))
    invisible(NULL)
  })
}
```

`render_pdf()` distinguishes two failure modes:
- **`system2()` throws** (WeasyPrint not on PATH, resource limit) — logs the HTML path for manual inspection.
- **WeasyPrint exits non-zero** — logs the full WeasyPrint stderr output and the HTML path.

---

## 10. Testing

```bash
# WeasyPrint smoke test (no server required)
Rscript tests/test_weasyprint_smoke.R

# Full regression suite (3203 cases — must pass unchanged)
Rscript tests/test_full.R

# API integration tests (requires running server)
Rscript tests/test_api.R
```

The smoke test (`tests/test_weasyprint_smoke.R`) verifies:
1. WeasyPrint is on PATH and callable.
2. `render_pdf()` produces a valid PDF from minimal HTML.
3. `img_base64()` correctly encodes a PNG to a data URI.
4. `render_pdf()` with an embedded banner image produces a valid PDF.

When debugging a WeasyPrint failure, the HTML temp file path is logged. To reproduce manually:
```bash
weasyprint temp/<request-id>/render_tmp.html /tmp/debug.pdf
```
