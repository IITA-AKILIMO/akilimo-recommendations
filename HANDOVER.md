# Handover — AKILIMO Recommendations API

For the next AI agent (or developer) picking up this repository. Read this,
then [CLAUDE.md](CLAUDE.md), before changing anything.

**IITA Internal Use.** Last updated: 2026-11-08.

---

## 1. Ground rules (from the repository owner)

1. **Never run `git push`.** The owner pushes by hand. Pushing `experimental`
   deploys the **beta** server, and pushing `main` deploys **production**. Commit
   locally, then report the commit hashes.
2. **No AI co-author lines** in commits. Use only co-authors the owner names
   (e.g. `munywele-bot`).
3. **Beta (`experimental`) is the primary branch.** Production (`main`) is
   unstable and about 300 commits behind. Base new work on `experimental`,
   assume reported bugs come from beta, and never cut worktrees from `main`.
4. **IITA rules:**
   - Never put credentials, PII or donor data in code, output or logs.
   - Refer to the organisation as "IITA".
   - `.env` holds secrets: edit single lines, never print the file, never echo
     `AKILIMO_API_TOKEN`.
5. **Workflow actions use version tags, never commit SHAs** (e.g. `actions/checkout@v7`).
6. Don't touch `old/` or `R/preprocess/`.
7. Commit style: gitmoji + conventional type, e.g. `🐛 fix(scope): …`, with a body
   that says why.

## 2. Current state

- Branch `experimental`. Pushed up to `462c2ac`. The handover commits after it are local.
- **Latest commits** (newest first):

| Commit | What |
|--------|------|
| (handover) | This handover update |
| `d90f8b3` | Reverts `8dd1b29`: Tanzania weeding defaults back to 60,000 / 45,000 TZS per acre (see the warning in §8) |
| `2f9a168` | Tanzania `tractor_ridging` default 115,000 → 100,000 TZS per acre, matching the price service |
| `8dd1b29` | Wrong change (weeding ×10), reverted by `d90f8b3`. Net effect of the two: none |
| `0e20346` | Handover: DATA-1 needs no new LINTUL runs; build steps in §7 |

- Also pushed in this round:
  - `d95c46b`: the starch seed CSV reads an optional `currency` column.
  - `b37fe29`: `docs/API-REFERENCE.md` starch feed notes (GH/RW estimates, BI none).
- **Local data (gitignored) changed, not published:** `data/input/starchPrices.csv` gained a `currency` column and two estimated, inactive-in-API factories, `AyensuStarch` (GH) and `KinaziCassavaPlant` (RW). Servers only get it after the owner runs `bundle-assets` and uploads to OSF/Zenodo.
- **Verified locally (Windows, R):**

| Check | Result |
|---|---|
| Parse check | 33 files, 0 errors |
| `test_akilimo_db` | 43/43 |
| `test_validate_request` | 49/49 |
| `test_pdf_template` | 48/48 |
| `test_ic_units` | 12/12 |
| `test_small` | 164/164 (re-run after `d90f8b3`) |
| `test_rw_regressions` | 8/8 |

  The translations sync was also dry-run against the local price service: 60 rows, no 401.
- **Not verified:**
  - The new pipeline on GitHub. First run after push: the OSF data download (~560 MB) on a cold cache, the `github-tag-action@v7` behaviour on `main`, and whether Windows-built `test_small.rds` matches Linux results (1% numeric tolerance, exact recommendation text).
  - Any request over HTTP against a running server.
  - The beta server's first deploy with these changes. It removes the untracked `data/input/translations.csv` before `git pull` and migrates the DB to v4.

## 3. Open gaps

| ID | Gap | Status / next step |
|----|-----|--------------------|
| DATA-1 | No scheduled planting for RW and BI: no `{CC}_WLY_LINTUL_2020SP.nc`, and no BI points in `SoilData_4Country.RDS` | **Not blocked on LINTUL runs.** The `.nc` files can be built from the existing yield RDS; see [§7](#7-next-task-sp-for-rw-and-bi-data-1). Open: the BI soil source (owner's decision). The API returns a clear 400 until then; `/coverage?type=SP` is empty |
| DATA-2 | Kinyarwanda (`rw`) text empty | Needs a translator; fill the `rw` column of `translations.csv` |
| DATA-3 | Price service data gaps | **Mostly done in new-akilimo-api:**<br>- Burundi `BU`→`BI` (`3681ce2`, pushed).<br>- Currency set on every default and starch price (`2bd8760`).<br>- GH/RW starch factories added **inactive** with estimated prices; inactive factories are hidden from `/starch-prices` (`d80ccf5`, pushed).<br>Left: confirm the GH/RW starch prices, then activate them in the admin. BI has no starch buyer |
| DATA-4 | Default prices are stale (fertiliser 1.3–7× below market in all five countries; app price ranges reject real prices; Burundi FOMI is per 25 kg bag) | Reviewed with sources in new-akilimo-api `docs/data/price-review-2026-11.md`. Tanzania fertiliser updated to TFRA Sep 2025 (`87721bc`). The rest waits for the price catalogue (§8) and country-team review in the admin |
| DATA-5 | No post-planting labour defaults for RW, GH, BI. The engine has them only for NG/TZ, hardcoded in `R/AkilimoMain.R` (`default_lmo_costs`) | Draft estimates in new-akilimo-api `docs/data/labour-costs-rw-gh-bi.md` (low confidence). They go in through the catalogue (§8) |
| ARCH-1 | Prices live in three places that drift apart (API choice lists, API `default_prices`, hardcoded R values) | Design agreed: new-akilimo-api `docs/PRICE_CATALOGUE_DESIGN.md`. **Phase 1 completed**; see §8 |
| AGR-1 | PP advice is minimal | Needs agronomist-approved wording; see `docs/TECH-DEBT.md` |
| AGR-2 | Intercrop coefficients flagged "check with Pieter" | Agronomist review |
| — | `main` far behind `experimental` | Owner's decision when to merge |
| — | Coverage polygons have holes | Documented; map clients must draw interior rings |

All of these are also in [docs/TECH-DEBT.md](docs/TECH-DEBT.md) ("Blocked on data or expert input").

## 4. Related work in other places

- **new-akilimo-api** (Laravel price/translation service, `D:\Dev\php\new-akilimo-api`, branch `develop`, `http://127.0.0.1:9100/api/v1` locally). It has no handover of its own; this file covers it.
  - **`8acb962` (not pushed): price catalogue phase 1**, by another agent. It was `690ff69` before its garbled message was reworded. Checked 2026-11-08:
    - 250 tests pass and no migrations are pending.
    - `GET /v1/prices?country=TZ` answers 200 with an `ETag`, `default_price_basis` and the new row fields.
    - The ±30 % second review is in `app/Service/PriceCatalogueService.php`; its threshold and the units per item are in `config/price_catalogue.php`.
    - Research labour estimates are imported as `draft` rows (migration `…000009`).
    - Existing rows have no `min_price`/`max_price`/source yet; country teams fill those in through the admin.
    - **`phpstan.neon`:** that commit had replaced the whole file. **Restored in `068b868` (not pushed)**, with the custom `EnforceNamedArgumentsRule`, the Larastan options and the strict rules back.
      - The old file never ran: one `ignoreErrors` entry lacked `#…#` delimiters, and its extensions weren't installed.
      - `068b868` fixes that entry, adds `phpstan/phpstan-strict-rules` as a dev dependency and `includes` both extensions explicitly.
      - Run it with `vendor\bin\phpstan analyse --memory-limit=1G`. It found 19 issues in `app/Http/Controllers/Web`; **all were fixed in `512f373` (not pushed), and it now reports no errors.** Eloquent `exists()`/`orderBy()` false positives were added to the existing `selectRaw` ignore entry. It analyses only `app/Http/Controllers/Web` (the `paths` setting); widening that will surface more.
      - `composer require` also reported 8 security advisories affecting 5 existing packages (`composer audit`).
  - **Earlier commits there**, all pushed to `develop` (newest first):
    - `d6c1e00`: the owner's own navigation-param fix.
    - `179880f`, `1fa40a5`, `5e2bcc6`: price catalogue design doc with decisions.
    - `87721bc`: Tanzania fertiliser defaults set to TFRA Sep 2025.
    - `dedf85c`: price review of every component.
    - `4f553f5`, `32f8953`, `3707d75`, `3cb5f1a`: labour-cost notes.
    - `2bd8760`: currencies.

    241 tests pass. The local DB is migrated.
  - **API keys:**
    - Shipped: the admin create-key page shows the key once, and `php artisan api-key:generate` is interactive (`5543858`, `d652171`, `49631f7`).
    - `ak_` keys work as Bearer tokens or `X-Api-Key` on `/translations`, the only route that checks a key (ability `translations:read`). The price routes are unauthenticated.
    - The R `.env` key works (200).
    - A new key: `php artisan api-key:generate --user=<user> --name="R recommendations" --ability=translations:read`.
    - Not verified: the admin create-key page in a browser. The fix (encrypt history only over HTTPS) is unit-tested only.
  - **Inactive starch factories** are left out of `GET /v1/starch-prices` and `/v1/starch-factories/country/{cc}`. `GET /v1/starch-factories` still lists all of them, with `factory_active`.
  - **Research notes** (all marked IITA Internal Use) are in `docs/data/`: starch buyers, labour costs, and the full price review.
- **Branch `claude/vigilant-mclean-c38063`** is fully merged into `experimental` (fast-forward) and can be deleted.

## 5. How to work here

- **Tests:** commands are in CLAUDE.md. Set `AKILIMO_ROOT` to the repo root first, or the tinytest files can't find `R/`.
  - The data-backed tests need `data/` (`cd scripts && poetry run setup-data`).
  - After an intended result change, rebuild the baseline with `UPDATE_BASELINE=1` and commit `tests/test_small.rds`.
- **Running requests in R without the server:** source `R/*.R` except `api.R`, call `open_akilimo_db()`, then call `run_akilimo(json)`. To keep the real DB untouched, point `AKILIMO_DB_PATH` at a scratch file.
- **Which types each country supports:** `REC_TYPE_COUNTRIES` in `R/AkilimoMain.R` is the only source. `validate_request()` and `/coverage` both read it.
- **Windows quirks seen in this repo:**
  - The `bash` on PATH is WSL; use `/mnt/c/...` paths.
  - `.bat` files must be ASCII with CRLF line endings.
  - watchexec 2.7 needs absolute `--watch` paths (`%CD%\R`).
  - Git converts LF to CRLF in the working copy; that's harmless.
- **CI (`.github/workflows/pipeline.yml`):**
  - Order: Plan → Wait → R checks → Update code → Pull data (optional) → Restart → Health → Tag (production only).
  - Deploy targets live in `.github/deploy-targets.json`.
  - Composite actions are in `.github/actions/`.

## 6. Suggested next steps

1. Both repos were pushed on 2026-11-08. Check the "Build and deploy" run for `experimental`, especially the OSF data download and `test_small`.
2. Refresh prices on beta (`Rscript refresh_prices.R --type all`) so it picks up the currencies, the Burundi fix and the TZ fertiliser prices.
3. **Price catalogue phase 2** ([§8](#8-price-catalogue-arch-1-phase-1-done-phase-2-next)): the engine syncs `/v1/prices`. Phase 1 is done (new-akilimo-api `8acb962`). Country teams can now enter prices in the admin.
4. Do DATA-1 ([§7](#7-next-task-sp-for-rw-and-bi-data-1)) once the owner picks the BI soil source. Hand DATA-2 / AGR-1 / AGR-2 to the IITA translation and agronomy owners.

## 7. Next task: SP for RW and BI (DATA-1)

The owner asked how to build the LINTUL planting-window files for the missing countries. Nothing has been built or committed yet.

**No new LINTUL runs are needed.** `data/yield/Rwanda_WLY_LINTUL.RDS` and `Burundi_WLY_LINTUL.RDS`, the files FR uses, are the same LINTUL output in wide form:
- Columns: `plantingDate, lat, long, WLY_235 … WLY_452`. That is 32 harvest ages, the same as the `.nc` `daysOnField` dimension.
- No NAs, and no duplicate cell × planting date.
- Reshaped long, they give exactly the layout `get_WLY_15M_ncdf()` (`R/get_data.R`) reads:
  - dims `daysOnField`, `plant` and `cell` (`cellFromLonLat()` at 0.05°)
  - variable `WLY` in kg/ha
  - sorted by cell, then plant day, then daysOnField, so daysOnField varies fastest

| Country | Planting dates | Cells | SP soil (`SoilData_4Country.RDS`) |
|---------|----------------|-------|-----------------------------------|
| BI | 53, weekly, all year | 805 | **none** |
| RW | 17: weeks 6–14 and 37–44 (the two seasons) | 782 | all cells |

Steps:
1. **Build the files.** Add a script under `tools/`; `R/preprocess/` is off limits. It mirrors `R/preprocess/WLY_LINTUL.R` but starts from the wide RDS:
   ```r
   for (f in c("misc.R", "get_data.R")) source(file.path("R", f))
   library(ncdf4)
   build_sp_nc <- function(cc, rds) {
     w    <- long2lon(readRDS(file.path("data/yield", rds)))
     cols <- grep("^WLY_", names(w), value = TRUE)
     d <- data.frame(
       cell        = rep(cellFromLonLat(round5min(w$lon), round5min(w$lat)), times = length(cols)),
       plantDay    = rep(w$plantingDate, times = length(cols)),
       daysOnField = rep(as.integer(sub("WLY_", "", cols)), each = nrow(w)),
       WLY         = unlist(w[cols], use.names = FALSE))
     d  <- d[order(d$cell, d$plantDay, d$daysOnField), ]
     dC <- ncdim_def("cell", "", as.integer(sort(unique(d$cell))))
     dP <- ncdim_def("plant", "", as.integer(sort(unique(d$plantDay))))
     dH <- ncdim_def("daysOnField", "", as.integer(sort(unique(d$daysOnField))))
     v  <- ncvar_def("WLY", "kg/ha", list(dH, dP, dC), -1, longname = "water limited yield", prec = "double")
     nc <- nc_create(sprintf("data/yield/%s_WLY_LINTUL_2020SP.nc", cc), v)
     ncvar_put(nc, v, d$WLY); nc_close(nc)
   }
   build_sp_nc("RW", "Rwanda_WLY_LINTUL.RDS")
   build_sp_nc("BI", "Burundi_WLY_LINTUL.RDS")
   ```
   Check: read a few cells back with `get_WLY_15M_ncdf()` and compare them with the RDS values.
2. **BI soil: the owner decides.** Either:
   - (a) the producer of `SoilData_4Country.RDS` adds BI, which keeps one method for all countries; or
   - (b) `getSPrecommendations()` (`R/process-SP.R`) uses `BI_FCY{1..5}_soilNPK.RDS`, picking the class from FCY the way FR does.

   RW can go ahead without waiting for this.
3. **Code:**
   - Remove `if (country == "BI") return(NULL)` in `get_WLY_15M_ncdf()`.
   - Add RW (and BI once soil is settled) to `REC_TYPE_COUNTRIES$SP` in `R/AkilimoMain.R`. `/coverage?type=SP` (`coverage_points()` in `R/coverage.R`) picks them up from the `.nc` file and soil data.
4. **Tests and docs:**
   - Update `tests/test_validate_request.R`.
   - Fixtures `in_38` (RW SP, now expected to succeed) and `in_39`.
   - `tests/test_rw_regressions.R`.
   - Rebuild `tests/test_small.rds` with `UPDATE_BASELINE=1`.
   - In `docs/API-REFERENCE.md`, update the "Availability by country" table and the error list.
5. **Ship the data (owner only; this publishes data):** run `cd scripts && poetry run bundle-assets`, upload to OSF/Zenodo, then run "Data setup" with target `beta`.

Limits to tell the owner:
- RW SP only works when PD ± `PD_window` falls in planting weeks 6–14 or 37–44. Other dates get the existing `SP_DATES_OUT_OF_RANGE` 400 ("Scheduled planting cannot be computed for PD=… and HD=…"). Full-year RW would need new LINTUL runs.
- SP converts dry to fresh yield with the NG curve for every country (`R/process-SP.R`, the `getRFY(..., country = "NG")` loop).

## 8. Price catalogue (ARCH-1): phase 1 done, phase 2 next

**Read first:** new-akilimo-api `docs/PRICE_CATALOGUE_DESIGN.md`. It holds the design, the rollout phases and the owner's decisions D1–D6.

**Phase 2, the next task (this repo):**
1. `refresh_prices()` in `R/akilimo_db.R` reads `GET /v1/prices?country=CC[&updated_since=]` instead of `/default-prices`.
   - Store `subsidised_price`, `unit`, `bag_kg`, `min_price`, `max_price` and `currency`: a schema bump, kept across deploys like v4.
   - Keep the rule that a failed or empty refresh keeps the last good data.
2. Replace the hardcoded defaults in `R/AkilimoMain.R` with DB lookups: `default_lmo_costs`, maize `50`/`230` and sweet potato `120000`/`384000`. Keep the hardcoded values only as a last resort, logged at WARN.
3. Add the optional request field `subsidisedFertilizer` (D1). If absent, use the country's `default_price_basis` from the response. Prices the farmer typed always win.
4. Out-of-range request prices: accept, log WARN, and add a response note with the range (D6). Never reject or clamp.
5. Add a contract-test fixture of a `/v1/prices` response under `tests/`, and run the sync against it in `test_akilimo_db.R`.
6. `test_small` must pass unchanged at today's values, then update `docs/API-REFERENCE.md`.

Burundi FOMI rows are per 25 kg (`bag_kg = 25`), so use `bag_kg` from the API, not an assumed 50.

**Phase 1, done** in new-akilimo-api `8acb962` (see §4 for the checks and the PHPStan restore). Its scope was:
1. **Extend `default_prices`:** add `region` (nullable), `subsidised_price`, `bag_kg`, `min_price`, `max_price`, `status` (`draft` / `pending_second_review` / `active`), `source`, `source_date`, `confidence`, `effective_from` and `updated_by`. Correct `unit` (labour is `per_acre`, not `per_bag`; Burundi FOMI is `bag_kg = 25`). Add a `default_price_history` table.
2. **Add `countries.default_price_basis`,** default `open_market`.
3. **Add `GET /v1/prices?country=&updated_since=`:** active, national, effective rows only, with an `ETag`. Keep `/v1/default-prices` working for the engine.
4. **Admin screens:**
   - edit prices as drafts, then review and activate (a change over ±30 % needs a second admin, D4);
   - the per-country price basis;
   - reviewer assignment per country.
5. **Import the research drafts** from `docs/data/*_candidates.csv` and `price-review-2026-11.md` as `draft` rows. Never as `active`: those prices are unconfirmed.
6. **Tests** in the existing Pest style; the suite is now at 250 passing.

After phase 2 comes phase 3, in new-akilimo-api: choice lists generated at ±10 %/±30 %, which fixes the allowed ranges.

**Pitfalls learned the hard way:**
- **Compare against the seed CSVs, not the local database.** Rows may be stale until re-seeded. The Tanzania weeding mix-up (`8dd1b29`, reverted in `d90f8b3`) came from reading a stale DB row (600,000) when the seed file and an owner commit (`b8db4fd`, 27 Oct) said 60,000.
- **Price "allowed ranges" are not a column.** They are the min and max of hand-made option rows in `fertilizer_prices` and similar tables, including a 0 "don't know" row and odd negative sentinel rows. Don't hand-edit ~190 rows; phase 3 generates them.
- **The engine ignores the API for some defaults:** labour (`default_lmo_costs`, NG/TZ only), maize (50 per cob, 230 per kg) and sweet potato (120,000 / 384,000 per t) are hardcoded in `R/AkilimoMain.R`. It does read `default_prices` for missing fertiliser costs (`R/fertilizers.R`).
- **Each data file has its own route to production:**
  - The R repo's `data/input/*.csv` is gitignored runtime data, published only via `bundle-assets` and an OSF/Zenodo upload (owner only).
  - The API's `database/seeders/data/*.csv` is in git, and seeders run only on demand. Ship data changes as an idempotent migration, as `2026_11_08_00000{1..4}` do.
