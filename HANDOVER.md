# Handover — AKILIMO Recommendations API

For the next AI agent (or developer) picking up this repository. Read this,
then [CLAUDE.md](CLAUDE.md), before changing anything.

**IITA Internal Use.** Last updated: 2026-11-10.

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

- **Branches (2026-11-10):**
  - `experimental` (beta) has price catalogue phase 2 (PR #31, `06e88aa`), the docs PR #32 (`961902e`), planting-date-and-cultivar (PDC) support (`4a5fdfa`) and scheduled planting for Rwanda (PR #33, merged as `adbc7a5` on 2026-11-10). Its CI run and the beta deploy that followed (run `42758184761`) succeeded.
  - A plain `git push` from any branch that tracks `origin/experimental` pushes to `experimental` and deploys beta.
  - Old stashes remain in the repo. `stash@{0}` ("handover DATA-1 next-step update") holds only an earlier draft of the §6 next steps, now superseded, so it is safe to drop (`git stash drop 'stash@{0}'`). Check the older ones before dropping them.
- **PR #33** (`feat/data-1-rw-sp`, scheduled planting for Rwanda) is **merged** (`adbc7a5`, 2026-11-10). Its CI was green, including the step that builds the RW NetCDF on the runner, and the beta deploy built the file too (log: "Wrote data/yield/RW_WLY_LINTUL_2020SP.nc: 782 cells… Verified"). Details:
  - The conflict with `experimental` is resolved by merging `experimental` into the branch (PDC and RW scheduled planting sit side by side in `REC_TYPE_COUNTRIES`).
  - CI failed because `RW_WLY_LINTUL_2020SP.nc` is not in the OSF bundle. `tools/ensure_sp_files.sh` now builds it from `data/yield/Rwanda_WLY_LINTUL.RDS` when it is missing: in CI before the checks, in the deploy's code update, and after a server data pull. A file published in the bundle always wins. Reproduced locally without the file (5 of 12 RW checks fail), then built it: `test_small` 164/164, RW regressions 12/12.

  Resolve the branch conflict and the CI/runtime-data issue before merging. Do not bypass these checks.
- **Latest commits** (newest first):

| Commit | What |
|--------|------|
| `4a5fdfa` | PDC support (`process-PDC.R`, request flags, dispatch, docs and tests), on `experimental` |
| `961902e` | Merges documentation PR #32 into `experimental` |
| `7cafb35` | PR #33 branch: enable scheduled planting for Rwanda; pending conflict and CI fixes |
| `572c469` | Adds initial Rwanda risk-assessment data for PR #33 |
| `06e88aa` | Merges price catalogue phase 2 PR #31 into `experimental` |
| `caad7d8` | CI restores the cached runtime data before the R checks |
| `a78c312` | Splits the pipeline into the CI and Deploy workflows |
| `94bdee0` | Fixes phase 2 review findings (§8): free-fertiliser fallback, repeated price refresh, NG/TZ-only labour lookup. Schema v6 |
| `c052864` | Price catalogue phase 2: engine syncs reviewed `/v1/prices`, applies catalogue defaults and records range notes |
| `d90f8b3` | Reverts `8dd1b29`: Tanzania weeding defaults back to 60,000 / 45,000 TZS per acre (see the warning in §8) |
| `2f9a168` | Tanzania `tractor_ridging` default 115,000 → 100,000 TZS per acre, matching the price service |
| `8dd1b29` | Wrong change (weeding ×10), reverted by `d90f8b3`. Net effect of the two: none |
| `0e20346` | Handover: DATA-1 needs no new LINTUL runs; build steps in §7 |
| `227c2f7` | `main`: merges PR #30, the original planting-date-and-cultivar scripts that `4a5fdfa` brought into `experimental` |


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
| Phase 2 at `94bdee0`: parse check | 33 files, 0 errors |
| Phase 2 at `94bdee0`: `test_akilimo_db` | 53/53 |
| Phase 2 at `94bdee0`: `test_validate_request` | 51/51 |
| Phase 2 at `94bdee0`: `test_pdf_template`, `test_ic_units` | 48/48, 12/12 |
| Phase 2 at `94bdee0`: `test_small` | **164/164** (31 of 164 failed at `c052864`) |
| Phase 2 at `94bdee0`: live refresh | TZ refreshed twice from the running `feat/price-review` API into a DB copy: 16 rows, then 0; the cursor is kept and the country isn't stale |

  The translations sync was also dry-run against the local price service: 60 rows, no 401.
- **Recently verified:** CI on `experimental` was green at `4a5fdfa`, CI on the PR #33 branch was green at `94adc6e` (the `Build the Rwanda scheduled-planting file` step ran on the Linux runner: 782 cells, verified), and the beta price refresh (`Rscript refresh_prices.R --type all`) completed successfully on 2026-11-09 (as reported by the owner).
- **Not verified:**
  - **Health check and release tag.** Until the fix in `deploy.yml` (2026-11-10), `Check health` and `Tag release` were skipped on every push deploy, because the skipped optional data pull upstream made GitHub's implicit `success()` false. Both now name the restart/health result explicitly. Not verified: the first deploy after the fix is the first time the health check runs for real, and the first push to `main` is the first time `github-tag-action@v7` and the GitHub release run. Look at those two jobs. (The Windows-built `test_small.rds` baseline now passes on Linux CI on `experimental`.)
  - Any request over HTTP against a running server.
  - A complete `/compute` request over HTTP that uses an activated catalogue price, e.g. a draft RW labour price activated in the admin.
  - A RW scheduled-planting `/compute` request against beta (the file is built there, but no request has been sent), and an end-to-end `/compute` request using an activated catalogue price.

## 3. Open gaps

| ID | Gap | Status / next step |
|----|-----|--------------------|
| DATA-1 | Scheduled planting is unavailable for BI. RW is done: PR #33 merged and beta built its file. BI has no planting-window file and no points in `SoilData_4Country.RDS` | RW: send a scheduled-planting request to beta to confirm. BI remains blocked on the owner's soil-source decision; see [§7](#7-next-task-sp-for-rw-and-bi-data-1) |
| DATA-2 | Kinyarwanda (`rw`) text empty | Needs a translator; fill the `rw` column of `translations.csv` |
| DATA-3 | Price service data gaps | **Mostly done in new-akilimo-api:**<br>- Burundi `BU`→`BI` (`3681ce2`, pushed).<br>- Currency set on every default and starch price (`2bd8760`).<br>- GH/RW starch factories added **inactive** with estimated prices; inactive factories are hidden from `/starch-prices` (`d80ccf5`, pushed).<br>Left: confirm the GH/RW starch prices, then activate them in the admin. BI has no starch buyer |
| DATA-4 | Prices are stale (fertiliser 1.3–7× below market in all five countries, both the defaults and the app's price options; Burundi FOMI is per 25 kg bag) | Reviewed with sources in new-akilimo-api `docs/data/price-review-2026-11.md`. Tanzania fertiliser updated to TFRA Sep 2025 (`87721bc`). The rest waits for the price catalogue (§8) and country-team review in the admin |
| DATA-5 | No confirmed post-planting labour defaults for RW, GH, BI. The engine has built-in fallback values only for NG/TZ; catalogue lookups run for every country | Draft estimates in new-akilimo-api `docs/data/labour-costs-rw-gh-bi.md` (low confidence). Country teams need to review and activate them in the catalogue (§8) |
| ARCH-1 | Prices live in three places that drift apart (API choice lists, API `default_prices`, hardcoded R values) | Design agreed: new-akilimo-api `docs/PRICE_CATALOGUE_DESIGN.md`. **Phase 1 completed**; see §8 |
| AGR-1 | PP advice is minimal | Needs agronomist-approved wording; see `docs/TECH-DEBT.md` |
| AGR-2 | Intercrop coefficients flagged "check with Pieter" | Agronomist review |
| — | `main` far behind `experimental` | Owner's decision when to merge |
| — | Coverage polygons have holes | Documented; map clients must draw interior rings |
| DATA-PDC | DSSAT data missing for PDC (NG maize) | Owner will upload data/dssat/planting-date-and-cultivar/NGA |


These data and expert-input gaps are tracked in [docs/TECH-DEBT.md](docs/TECH-DEBT.md) where applicable; PR-specific status is recorded here.

## 4. Related work in other places

- **new-akilimo-api** (Laravel price/translation service, `D:\Dev\php\new-akilimo-api`, `http://127.0.0.1:9100/api/v1` locally). It has no handover of its own; this file covers it.
  - **Branches (checked 2026-11-10):**
    - **Phase 1 of the price catalogue is on `develop`**: `feat/price-review` was merged by PR #79 (`56fd703`). `develop` is at `97b5ce3`, three CI-only commits later.
    - The local checkout is `develop`, in step with origin. The earlier plan to keep phase 1 off `develop` no longer applies.
    - On `develop`, `/v1/default-prices` returns only `active` rows, so draft prices can't reach the engine. The earlier local-database caveat about running `develop` is obsolete.
  - **`8acb962`: price catalogue phase 1**, by another agent, now on `develop`. It was `690ff69` before its garbled message was reworded. Other commits that came in with it:
    - `068b868`: PHPStan restore;
    - `512f373`: PHPStan fixes;
    - `edcf3a8`, `a359a6f`, `310a503`: migration refactors, including a foreign key on `country_reviewers.country_code`.
    - `404006c`: user ↔ price catalogue relationships.
    - `2de7504`: base models regenerated by the model generator.
    - `2902773`: the `Crop` and `TrafficExclusion` scopes and `Crop.countries` cast moved into the child models, because the regeneration had deleted them from the Base models (see the pitfall in §8).
    - `e1bfc74`: `DefaultPriceHistory` extends its generated base.
    - `6b65226`: licence headers and formatting.
    - `ef2153b`: migration renamed `…0000010` → `…000010`. The seven-digit name sorted before `…000007`, so a fresh database added the foreign key before its table existed, and 190 of 250 tests failed at setup.
    - `c81b828`: `DefaultPriceHistory` `old_values`/`new_values` array casts restored. `e1bfc74`'s generated base lacked them, so the audit trail failed with "Array to string conversion".
    - `afaf210`: docs.

    These are all on `develop`. **Any database that already ran the old migration file name** must have its `migrations` row renamed to `2026_11_08_000010_add_country_code_fk_to_country_reviewers_table`, or the foreign key runs twice. The owner's local DB is already fixed.

    Re-checked 2026-11-08 at `afaf210`: **250 tests pass**, PHPStan reports no errors, and no migrations are pending. Earlier checks (at `512f373`):
    - 250 tests pass and no migrations are pending.
    - `GET /v1/prices?country=TZ` answers 200 with an `ETag`, `default_price_basis` and the new row fields.
    - The ±30 % second review is in `app/Service/PriceCatalogueService.php`; its threshold and the units per item are in `config/price_catalogue.php`.
    - Research labour estimates are imported as `draft` rows (migration `…000009`). Starch drafts aren't imported here: they stay as inactive factories in `starch_prices`.
    - On this branch `/v1/default-prices` returns only `active` rows, so drafts can't reach the engine. The local-DB caveat above applies only when running `develop`.
    - Existing rows have no `min_price`/`max_price`/source yet; country teams fill those in through the admin.
    - **`phpstan.neon`:** that commit had replaced the whole file. **Restored in `068b868`**, with the custom `EnforceNamedArgumentsRule`, the Larastan options and the strict rules back.
      - The old file never ran: one `ignoreErrors` entry lacked `#…#` delimiters, and its extensions weren't installed.
      - `068b868` fixes that entry, adds `phpstan/phpstan-strict-rules` as a dev dependency and `includes` both extensions explicitly.
      - Run it with `vendor\bin\phpstan analyse --memory-limit=1G`. It found 19 issues in `app/Http/Controllers/Web`; **all were fixed in `512f373`, and it reported no errors at that commit.** Eloquent `exists()`/`orderBy()` false positives were added to the existing `selectRaw` ignore entry. It analyses only `app/Http/Controllers/Web` (the `paths` setting); widening that will surface more.
      - `composer require` also reported 8 security advisories affecting 5 existing packages (`composer audit`).
  - **Later commits on `develop`:** `d39848a`, `f236fc6` and `97b5ce3` change only the API's own CI test setup (migration setup, default test database name, an unused cache-table step).
  - **Earlier commits**, all on `develop` (newest first):
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
- **akilimo-mobile** (Android app, Kotlin/Compose, `D:\Dev\android\akilimo-mobile`, `git@github.com:IITA-AKILIMO/akilimo-mobile.git`; checked out on `fix/ui-review-findings` on 2026-11-08). The owner works on it in a separate session. Read its `AGENTS.md` before changing it.
  - **How it reaches this engine:** it posts to new-akilimo-api `v1/recommendations/compute` (base URL `AKILIMO_BASE_URL`, default `https://api.akilimo.org`), which forwards to this repo's `/compute`. It never calls this engine directly.
  - **Reference data it reads** (`app/src/main/java/com/akilimo/mobile/network/AkilimoApi.kt`): `/v1/fertilizers`, `/v1/fertilizer-prices`, `/v1/cassava-prices`, `/v1/maize-prices`, `/v1/investment-amounts`, `/v1/starch-factories`, `/v1/cassava-units` and `/v1/coverage/{country}`. It does **not** read `/v1/default-prices`, `/v1/prices`, `/v1/potato-prices` or `/v1/operation-costs`.
  - **Fertiliser price sheet** (`ui/screens/usecases/FertilizerScreen.kt`): one radio option per `fertilizer_prices` row.
    - `price_per_bag = 0` is **"I don't know"**: sent as price 0, so this engine fills in the catalogue price.
    - A negative value is **"Exact price"**: free text, any value.
    - Anything else shows its `price_range`.
    - `min_allowed_price`/`max_allowed_price` are stored but **never used**.
  - **Starch factories:** it keeps only rows with `factory_active` true (`dao/StarchFactoryDao.kt`), so inactive factories stay hidden even though the unfiltered `/v1/starch-factories` returns them.
  - **Labour costs** are only what the farmer types; there is no price list for them in the app.
  - **When to check it:** any change to the shape of a price or factory endpoint, or to what a `0`/negative price means, must be checked against these files.

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
- **CI and deploy** (split on 2026-11-09; was `pipeline.yml`):
  - `ci.yml` ("CI"): Wait for more pushes (deploy branches only) → R checks on every push and PR. On pushes it then calls `deploy.yml` as a reusable workflow, which runs from the pushed branch, not from `main`.
  - The wait is first on purpose (changed 2026-11-10): a newer push cancels the older run's wait, so a burst of pushes runs the checks and deploy once. For that to work, runs on `main`/`experimental` get their own concurrency group; with one shared group GitHub queues the new run behind the old one, and it never reaches its wait. Don't put a shared workflow-level `concurrency` group back on deploy branches. Deploys still take turns through the per-server lock in `deploy.yml`.
  - `deploy.yml` ("Deploy"): Plan → Update code → Pull data (optional) → Restart → Health → Tag (production only). It can be run by hand from the Actions tab, but a manual run does **not** run the R checks. Jobs after an optional one need an explicit `if` naming the results they accept (`needs.X.result == 'success'`); the default "all upstream jobs succeeded" skips them whenever the optional job is skipped.
  - `data-setup.yml` ("Data setup"): pulls OSF data onto a server on its own.
  - Deploy targets live in `.github/deploy-targets.json`.
  - Composite actions are in `.github/actions/`.

## 6. Suggested next steps

**Urgent (2026-11-10): the beta service is crash-looping.** The first deploy where `Check health` really ran (run `42758918609`, after the fix in `549a97f`) failed: `akilimo-api-beta.service` exits at start with `createTcpServer: address already in use`, and systemd's restart counter was at **6691** (a restart about every 18 seconds, so it has been failing for roughly a day and a half). Earlier green deploys hid this because the health check was being skipped. The cause is not known; nobody has looked on the server. Things to check there:

- Which process holds the port: `sudo ss -ltnp | grep -E ':(8000|8001)\b'`. Beta is meant to use 8001 and production 8000 (README, "Beta Instance"). A leftover R process from an earlier run would explain a port that stays taken.
- Where beta's port comes from: `grep API_PORT /home/akilimo/projects/akilimo-beta/.env`, and `systemctl cat akilimo-api-beta.service`. The README never says to set `API_PORT=8001` in beta's `.env`. The health check read no port there and polled 8000, which is production's port.
- `sudo journalctl -u akilimo-api-beta -n 50 --no-pager`, and `systemctl status akilimo-api.service` to confirm production is fine.

Until beta is fixed, the Deploy job's `Check health` will fail on every push to `experimental`: that failure is real, not a flaky check. A health check that follows the service's real port (from the unit file as well as `.env`) would be a safer check, because today it can pass against the wrong service.


1. **Completed:** CI on the current work runs successfully; the beta all-prices refresh succeeded on 2026-11-09. PR #32 (documentation) is merged as `961902e`.
2. **Completed 2026-11-10:** PR #33 (RW scheduled planting) is merged (`adbc7a5`) and beta's deploy built `RW_WLY_LINTUL_2020SP.nc`. Still to do: send a RW scheduled-planting request to beta.
3. Before release, verify a `/compute` request against the deployed `/v1/prices` endpoint that uses an activated catalogue price.
4. BI scheduled planting remains blocked on the owner's soil-source decision. Hand DATA-2 / AGR-1 / AGR-2 to the IITA translation and agronomy owners.

## 7. Next task: SP for RW and BI (DATA-1)

PR #33 (`feat/data-1-rw-sp`) implemented scheduled planting for Rwanda from the existing yield RDS files and is merged (`adbc7a5`). Its generator is `tools/build_sp_nc.R`; `tools/ensure_sp_files.sh` runs it where the file is missing (CI, deploy, data pull); beta built the file on 2026-11-10. BI remains unimplemented pending the owner's soil-source decision.

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
1. **RW NetCDF generation — implemented in PR #33.** The script under `tools/` mirrors the existing preprocessing layout but starts from the wide RDS; `R/preprocess/` remains off limits. Its core transformation is:
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
  The script also verifies the output by reading sample cells through `get_WLY_15M_ncdf()` and comparing with the RDS. The generated `.nc` is not a tracked source file and needn't be published: `tools/ensure_sp_files.sh` rebuilds it, identically (checked byte for byte), wherever it is missing and `Rwanda_WLY_LINTUL.RDS` is present.
2. **BI soil: the owner decides (pending).** Either:
   - (a) the producer of `SoilData_4Country.RDS` adds BI, which keeps one method for all countries; or
   - (b) `getSPrecommendations()` (`R/process-SP.R`) uses `BI_FCY{1..5}_soilNPK.RDS`, picking the class from FCY the way FR does.

   RW can go ahead without waiting for this.
3. **RW code — included in PR #33; BI code remains pending:** the PR enables RW in `REC_TYPE_COUNTRIES$SP`, removes the unconditional BI early return from `get_WLY_15M_ncdf()`, and relies on data/soil coverage for availability. Add BI only after its soil source is settled.
4. **Tests and docs, included in PR #33:**
   - Update `tests/test_validate_request.R`.
   - Fixtures `in_38` (RW SP, now expected to succeed) and `in_39`.
   - `tests/test_rw_regressions.R`.
  - Rebuild `tests/test_small.rds` with `UPDATE_BASELINE=1` (done on the PR branch; it still passes after merging `experimental`).
   - In `docs/API-REFERENCE.md`, update the "Availability by country" table and the error list.
5. **Data: nothing to publish for RW.** CI and the deploy build the file (step 1). Publishing a real LINTUL-run file in the bundle later is still possible and wins over the generated one. BI needs its own file and a soil decision first.

Limits to tell the owner:
- RW SP only works when PD ± `PD_window` falls in planting weeks 6–14 or 37–44. Other dates get the existing `SP_DATES_OUT_OF_RANGE` 400 ("Scheduled planting cannot be computed for PD=… and HD=…"). Full-year RW would need new LINTUL runs.
- SP converts dry to fresh yield with the NG curve for every country (`R/process-SP.R`, the `getRFY(..., country = "NG")` loop).

## 8. Price catalogue (ARCH-1): phase 1 and phase 2 merged; phase 3 remains

**Read first:** new-akilimo-api `docs/PRICE_CATALOGUE_DESIGN.md`. It holds the design, the rollout phases and the owner's decisions D1–D6.

**Phase 2 (this repo).** Implemented in `c052864` and fixed in `94bdee0`; PR #31 is merged into `experimental` (`06e88aa`). The beta all-prices refresh succeeded on 2026-11-09; an HTTP `/compute` request using an activated catalogue row still needs verification.
- **Branch/deploy:** only pushes to `experimental` and `main` have deploy targets. The owner handles pushes; never run `git push`.
- **Release dependency:** the engine needs new-akilimo-api with phase 1, which provides `GET /v1/prices`. That is now on its `develop` branch (`feat/price-review` was merged by #79). Against an older API the engine logs the refresh failure and keeps its existing seed/cache prices.
- **API to build against:** new-akilimo-api `develop`, run locally at `http://127.0.0.1:9100/api/v1`.

Implemented scope:
1. `refresh_prices()` in `R/akilimo_db.R` reads `GET /v1/prices?country=CC[&updated_since=]` instead of `/default-prices`.
   - Store `subsidised_price`, `unit`, `bag_kg`, `min_price`, `max_price` and `currency`: a schema bump, kept across deploys like v4.
   - Keep the rule that a failed or empty refresh keeps the last good data.
   - **Sync state (v6, `94bdee0`):** each row's `updated_at` is the local sync time in ISO format, which `prices_are_stale()` parses. Per country, `country_price_settings` holds:
     - `api_cursor`: the newest price-service `updated_at` seen, in the API's own format (`2026-11-08 12:33:42`), sent back verbatim as `updated_since`;
     - `refreshed_at`: the last successful refresh, which staleness uses even when nothing changed.
2. Replace the hardcoded defaults in `R/AkilimoMain.R` with DB lookups: `default_lmo_costs`, maize `50`/`230` and sweet potato `120000`/`384000`. Keep the hardcoded values only as a last resort, logged at WARN.
3. Add the optional request field `subsidisedFertilizer` (D1). If absent, use the country's `default_price_basis` from the response. Prices the farmer typed always win.
4. Out-of-range request prices: accept, log WARN, and add a response note with the range (D6). Never reject or clamp.
5. Add a contract-test fixture of a `/v1/prices` response under `tests/`, and run the sync against it in `test_akilimo_db.R`.
6. `test_small` must pass unchanged at today's values, then update `docs/API-REFERENCE.md`.

**Issues found in `c052864`, fixed in `94bdee0` before PR #31 was merged:**
- **Free fertilisers.** A fertiliser with no price from the farmer or the catalogue got a cost of 0 instead of `NA`. Before phase 2 such fertilisers were dropped with "missing values for fertilizer type: …". At 0 the optimiser treated them as free: 13 regression cases changed, and some were "problem infeasible".
- **Refresh on every request.** Rows stored the API's `updated_at` (`2026-11-08 12:33:42`), which `prices_are_stale()` can't parse, so every `/compute` re-ran the refresh. Even in the right format, rows unchanged for a week stayed stale, because incremental syncs don't touch them.
- **Labour lookups only for NG/TZ.** RW/GH/BI labour prices were never read from the catalogue. Now every country is looked up; the hardcoded NG/TZ values are only a fallback.
- **Minor:** the `subsidisedFertilizer` check was inside the `PD_window`/`HD_window` loop, and tabs were mixed into `R/AkilimoMain.R`.

**Still to verify:** a `/compute` request over HTTP that uses an activated catalogue row, e.g. activate a draft RW labour price in the admin and run a RW PP request.

**Known limitation:** incremental sync never removes a row that was deactivated in the API, so the engine keeps its last price. A full refresh is the workaround: clear `api_cursor` for the country.

Burundi FOMI rows are per 25 kg (`bag_kg = 25`), so use `bag_kg` from the API, not an assumed 50.

**Phase 1, done** and merged into new-akilimo-api `develop` (`8acb962` and later; see §4). Its scope was:
1. **Extend `default_prices`:** add `region` (nullable), `subsidised_price`, `bag_kg`, `min_price`, `max_price`, `status` (`draft` / `pending_second_review` / `active`), `source`, `source_date`, `confidence`, `effective_from` and `updated_by`. Correct `unit` (labour is `per_acre`, not `per_bag`; Burundi FOMI is `bag_kg = 25`). Add a `default_price_history` table.
2. **Add `countries.default_price_basis`,** default `open_market`.
3. **Add `GET /v1/prices?country=&updated_since=`:** active, national, effective rows only, with an `ETag`. Keep `/v1/default-prices` working for the engine.
4. **Admin screens:**
   - edit prices as drafts, then review and activate (a change over ±30 % needs a second admin, D4);
   - the per-country price basis;
   - reviewer assignment per country.
5. **Import the research drafts** from `docs/data/*_candidates.csv` and `price-review-2026-11.md` as `draft` rows. Never as `active`: those prices are unconfirmed.
6. **Tests** in the existing Pest style; the suite is now at 250 passing.

After phase 2 comes phase 3, in new-akilimo-api: the app's price options generated from the catalogue at −30/−10/+10/+30 %, so they stay current. Keep the `0` ("I don't know") and negative ("Exact price") marker rows and generate the `price_range` labels, because the mobile app relies on all three (§4, akilimo-mobile).

**Pitfalls learned the hard way:**
- **Compare against the seed CSVs, not the local database.** Rows may be stale until re-seeded. The Tanzania weeding mix-up (`8dd1b29`, reverted in `d90f8b3`) came from reading a stale DB row (600,000) when the seed file and an owner commit (`b8db4fd`, 27 Oct) said 60,000.
- **Price "allowed ranges" are not a column, and nothing enforces them.** They are the min and max of hand-made option rows in `fertilizer_prices` and similar tables.
  - Those tables also hold marker rows: `0` = "I don't know" and negative = "Exact price". They are the app's own options, not junk.
  - The mobile app never uses the ranges; the stale part is the options themselves.
  - Don't hand-edit ~190 rows; phase 3 generates them.
- **The engine still has built-in fallback defaults:** labour (`default_lmo_costs`, NG/TZ only), maize (50 per cob, 230 per kg) and sweet potato (120,000 / 384,000 per t) have hardcoded last-resort values in `R/AkilimoMain.R`. The price catalogue is checked first for these values, and for missing fertiliser costs (`R/fertilizers.R`).
- **Each data file has its own route to production:**
  - The R repo's `data/input/*.csv` is gitignored runtime data, published only via `bundle-assets` and an OSF/Zenodo upload (owner only).
  - The API's `database/seeders/data/*.csv` is in git, and seeders run only on demand. Ship data changes as an idempotent migration, as `2026_11_08_00000{1..4}` do.
- **The API's model generator overwrites `app/Models/Base/*`.**
  - Put scopes, casts and other hand-written code in the child model (`app/Models/X.php`), never in `Base/`.
  - Regenerating in `2de7504` silently deleted the `Crop`/`TrafficExclusion` scopes and broke 190 of 250 tests, until `2902773` moved them.
  - After any regeneration, diff `Base/` for removed methods **and casts** and run the tests. `e1bfc74` lost the `DefaultPriceHistory` array casts the same way; `c81b828` fixed it.
- **API migration names need six-digit sequence numbers** (`2026_11_08_000010_…`). Laravel runs migrations in file-name order, so `…0000010` ran before `…000007` (fixed in `ef2153b`). Run the full test suite after adding a migration, because only a fresh database shows order bugs.
