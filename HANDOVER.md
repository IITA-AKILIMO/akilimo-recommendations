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

- Branch `experimental`, working tree clean once the handover commit lands.
- **11 local commits are not pushed** (newest first):

| Commit | What |
|--------|------|
| (handover) | `HANDOVER.md` and the CLAUDE.md updates |
| `af1f57b` | Cassava–sweet potato intercrop revenue now per field (fixes area unit mix-up); `tests/test_ic_units.R` |
| `505b3fe` | Actions moved to Node 24 releases (`checkout@v7`, `cache@v6`, `setup-python@v7`, `ssh-action@v1.2.5`, `release-action@v1.21.0`, `mathieudutour/github-tag-action@v7`); runners pinned to `ubuntu-24.04` |
| `e11ae62` | Regression baseline `tests/test_small.rds` + CI downloads runtime data from OSF and runs recommendation tests; data bundles no longer ship/overwrite `translations.csv` or the SQLite DB |
| `78b1ddc` | `data/input/translations.csv` tracked in git (with an empty `rw` column); `lang: "rw"` accepted (falls back to English) |
| `6812c2f` | `GET /coverage?type=SP` footprints (empty for RW/BI) |
| `c093251` | Price DB kept across deploys/restarts: schema v4, `sync_seed_data()` never overwrites API-refreshed rows; deploy and `dev.bat`/`dev.sh` no longer delete it |
| `bae37fa`, `6da8279` | RW/BI fixes: clear 400s for SP/IC where unavailable, `ekari`/`hekta`/`string` area units, small-field crash, `rec_types` in `/coverage` |
| `d82b906`, `eea4fa9` | CI: single "R checks" job; data pull after code update |

- **Verified locally (Windows, R):**

| Check | Result |
|---|---|
| Parse check | 33 files, 0 errors |
| `test_akilimo_db` | 42/42 |
| `test_validate_request` | 49/49 |
| `test_pdf_template` | 48/48 |
| `test_ic_units` | 12/12 |
| `test_small` | 164/164 |
| `test_rw_regressions` | 8/8 |

  The translations sync was also dry-run against the local price service: 60 rows, no 401.
- **Not verified:**
  - The new pipeline on GitHub. First run after push: the OSF data download (~560 MB) on a cold cache, the `github-tag-action@v7` behaviour on `main`, and whether Windows-built `test_small.rds` matches Linux results (1% numeric tolerance, exact recommendation text).
  - Any request over HTTP against a running server.
  - The beta server's first deploy with these changes. It removes the untracked `data/input/translations.csv` before `git pull` and migrates the DB to v4.

## 3. Open gaps

| ID | Gap | Status / next step |
|----|-----|--------------------|
| DATA-1 | No scheduled planting for RW and BI: no `{CC}_WLY_LINTUL_2020SP.nc`, and no BI points in `SoilData_4Country.RDS` | Blocked on LINTUL runs. The API returns a clear 400; `/coverage?type=SP` is empty |
| DATA-2 | Kinyarwanda (`rw`) text empty | Needs a translator; fill the `rw` column of `translations.csv` |
| DATA-3 | Burundi default prices missing in the price service: `database/seeders/data/default_prices.csv` has the 4 BI rows under `BU`, so `?country=BI` returns 0 | In new-akilimo-api, change `BU`→`BI` in the CSV and add a data migration updating the 4 rows. A fix was requested from its session; **check that it was committed**. Its price CSV also has no currency column (no currency for any country), and there are no starch prices for BI, RW or GH |
| AGR-1 | PP advice is minimal | Needs agronomist-approved wording; see `docs/TECH-DEBT.md` |
| AGR-2 | Intercrop coefficients flagged "check with Pieter" | Agronomist review |
| — | `main` far behind `experimental` | Owner's decision when to merge |
| — | Coverage polygons have holes | Documented; map clients must draw interior rings |

All of these are also in [docs/TECH-DEBT.md](docs/TECH-DEBT.md) ("Blocked on data or expert input").

## 4. Related work in other places

- **new-akilimo-api** (Laravel price/translation service, `http://127.0.0.1:9100/api/v1` locally):
  - The API key admin page and `php artisan api-key:generate` are committed there (`5543858`, `d652171`, `49631f7`; 227 tests pass).
  - `ak_` keys now work as Bearer tokens on `/translations`. It is the only route that checks the key (ability `translations:read`); the price routes are unauthenticated.
    - The old 401 was a token that matched no stored key.
    - The current `.env` key works (200).
    - A new key needs `translations:read` or wildcard: `php artisan api-key:generate --user=<user> --name="R recommendations" --ability=translations:read`.
  - The Burundi `BU`→`BI` fix was requested but not confirmed.
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

1. The owner pushes `experimental`. Watch the first "Build and deploy" run, especially the OSF data download and `test_small`.
2. Confirm the new-akilimo-api Burundi fix, then refresh prices on beta (`Rscript refresh_prices.R --type default`).
3. Hand DATA-1 / DATA-2 / AGR-1 / AGR-2 to the IITA data, translation and agronomy owners.
