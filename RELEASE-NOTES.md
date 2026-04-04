# AKILIMO Recommendation Engine — Release Notes

---

## Update: April 2026 (v1.8.2 – v1.8.4)

This update delivered a wave of correctness and quality fixes across the recommendation engine, with no changes to the recommendation logic itself.

---

### What farmers and field officers will notice

#### PDF reports and emails are fully translated

All section headings, chart labels, table headers, coordinate card text, and email subject/body in PDF reports are now translated into Swahili when `lang=sw` is set on the request. Previously, several PDF labels and the email notification were always in English regardless of the requested language.

#### Intercropping recommendation text now translated

The recommendation text produced for intercropping (IC) requests was always generated in English. It now respects the `lang` field and uses Swahili where available.

#### "sow the seeds" typo corrected

The intercropping recommendation for CMP1 incorrectly read "saw the seeds" — this has been corrected to "sow the seeds".

---

### What changes for system administrators

#### Translation key validator added at startup

The server now scans all source files at startup and logs an error for any translation key used in code that is missing from `translations.csv`. This catches misspelled or deleted keys immediately on startup rather than on the first live request that hits the affected code path. Look for lines like:

```
[INFO] check_translation_keys: all 47 translation key(s) verified OK
```

or

```
[ERROR] check_translation_keys: 1 key(s) used in source but absent from translations.csv: somekey
```

#### Window parameters validated at startup

`PD_window` and `HD_window` (planting/harvest date search windows) are now validated as non-negative integers. Previously a non-numeric or negative value would pass validation and cause a downstream error.

#### Performance: two large RDS files are now cached

`WLY_365` (yield raster data) and `soil_NPK-4` (soil NPK properties) are now loaded once and held in memory on first use, rather than read from disk on every SP and FR request. This reduces per-request disk I/O noticeably for high-traffic deployments.

#### Fertilizer NPK content table externalised

The fertilizer nutrient content table (N%, P%, K% per product) has been moved from hardcoded R source into `data/input/fertilizer_npk.csv`. This file must be present in the data bundle — run `poetry run setup-data` after updating to ensure it is in place.

#### Profitability threshold extracted to shared function

The minimum net-revenue multiplier used in FR, IC, and PP recommendations is now a single shared function (`min_nr_multiplier(riskAtt)`). This has no visible effect on recommendations but eliminates four copies of the same threshold logic, making future calibration changes a one-line edit.

---

## Release: April 2026

This release focused on making PDF reports reliable, fixing recommendation errors that could produce wrong or missing advice, and making the server easier to set up and maintain in production.

---

### What farmers and field officers will notice

#### PDF reports are now generated without a browser

PDF recommendation reports no longer require a web browser (Chrome/Chromium) installed on the server. They are now produced by a lightweight tool called WeasyPrint. This means:

- Reports generate faster and use less memory.
- The server no longer needs a graphical display (X11) to function — it works cleanly in headless server environments.
- Reports look the same as before.

#### Farm location shown on reports

Each PDF report now includes a farm location section showing the GPS coordinates (latitude and longitude) provided during the request. If a Mapbox map token is configured, a proper map image is embedded. If no internet connection is available, the coordinates are shown as a clean text card — the report always generates regardless of network availability.

#### Post-planting (PP) chart fixed

The tillage comparison chart in Post-Planting recommendation PDFs was missing text labels inside the tiles (showing net value and cost per scenario). This has been restored to match the original reference design.

#### Phone number and email address removed from PDF filenames

Previously, the user's phone number appeared in temporary file names used during report generation. This has been changed to a random identifier for privacy.

---

### What changes for system administrators

#### Prices now stored in a database

Fertilizer, labour, cassava, and starch factory prices were previously loaded from CSV files each time a request came in. They are now stored in a small SQLite database (`data/input/akilimo_compute.sqlite`), which is created automatically when the server starts.

Benefits:
- Prices can be refreshed without restarting the server.
- A price history (audit log) is kept — every update is recorded with a timestamp.
- Price data can be auto-refreshed from an external price service, configurable via environment variables.

A command-line tool (`refresh_prices.R`) is provided for manual or scheduled price updates.

#### Intercropping (IC) fertilizer bug fixed

When a request for intercropping advice (IC) was submitted with fertilizer prices set to zero, the recommendation engine returned an empty fertilizer list instead of using the database defaults. This has been fixed — the engine now correctly falls back to default prices when none are supplied.

#### Charts no longer require an X11 display

On Linux servers without a graphical display (the typical production setup), generating the PP and SP recommendation charts previously failed with an error about "unable to open connection to X11 display". The server now uses the `ragg` graphics library for chart rendering, which works fully headless. A Cairo fallback is also in place for older environments.

#### WeasyPrint must be on the system PATH for the service user

**This is the most common reason PDF generation fails in production.**

The API server runs as a dedicated system user (`akilimo`) via systemd. Systemd gives services a minimal PATH — it does not inherit the PATH from your interactive shell or `.bashrc`. If WeasyPrint was installed with `pip install --user` by a different user (e.g. the deployment user), the `akilimo` service user will not find it, and every PDF request will fail.

**Fix:** Install WeasyPrint system-wide so it is available to all users:

```bash
sudo pip3 install weasyprint
# On Debian 12+ / Ubuntu 23.04+:
sudo pip3 install --break-system-packages weasyprint
# Or via apt:
sudo apt-get install -y python3-weasyprint
```

Then verify the service user can find it:
```bash
sudo -u akilimo weasyprint --version
```

The server now checks WeasyPrint availability at startup and logs a clear error if it is missing, rather than failing silently on the first PDF request.

Full instructions are in [docs/SETUP.md](docs/SETUP.md#production-deployment-systemd).

#### Systemd service file updated

The example service file (`systemd/akilimo-api.service.example`) has been corrected and expanded:

- Fixed the startup script path (was pointing to a non-existent file).
- Added PATH entries covering all common WeasyPrint install locations.
- Added writable directory entries for `temp/` (needed for PDF generation) and `data/` (needed for the price database).
- Added `EnvironmentFile` so the `.env` configuration is loaded automatically.

#### Better error messages when PDF generation fails

If a PDF fails to generate, the server now logs:
- The exact error message from WeasyPrint (previously swallowed).
- The path to the HTML file that was being rendered, so it can be inspected or manually re-processed.
- A startup log entry confirming whether WeasyPrint was found (`[INFO] WeasyPrint: X.Y.Z`) or not (`[ERROR] WeasyPrint not found`).

---

### Upgrade steps

1. **Pull the latest code** and restart the service.
2. **Install WeasyPrint system-wide** if not already done (see above).
3. **Verify writable directories exist** on the server:
   ```bash
   cd /home/akilimo/projects/akilimo-recommendations
   mkdir -p temp data/input logs
   chown -R akilimo:akilimo temp data logs
   ```
4. **Check the startup log** after restart:
   ```bash
   journalctl -u akilimo-api -n 50
   ```
   You should see `[INFO] WeasyPrint: weasyprint X.Y.Z` and `[INFO] Price database opened`.
5. **Run a test request** to confirm PDFs are generating:
   ```bash
   curl -s -X POST http://localhost:8000/compute \
     -H "Content-Type: application/json" \
     --data "@./tests/input/in_1_TZ_FR_starch_factory_riskAtt0.json"
   ```

---

### New environment variables

The following variables can be added to `.env` to enable new features. All are optional — the server works without them.

| Variable | What it does |
|----------|-------------|
| `MAPBOX_TOKEN` | Embeds a real map image in PDF reports (requires a Mapbox account) |
| `MAP_API_URL` | Alternative map image provider URL |
| `AKILIMO_DB_PATH` | Custom path for the SQLite price database (default: `data/input/akilimo_compute.sqlite`) |
| `AKILIMO_API_URL` | URL of an external price service for automatic price refresh |
| `AKILIMO_API_TOKEN` | Authentication token for the price service |
| `PRICE_MAX_AGE_DAYS` | How many days before fertilizer prices are considered stale (default: 7) |
| `STARCH_PRICE_MAX_AGE_DAYS` | How many days before starch factory prices are considered stale (default: 30) |
