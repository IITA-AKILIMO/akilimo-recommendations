# Handover — AKILIMO Recommendations API

For the next AI agent (or developer) picking up this repository. Read this,
then [CLAUDE.md](CLAUDE.md), before changing anything.

**IITA Internal Use.** Last updated: 2026-11-13.

---

## 0. Start here: what changed on 2026-11-11 and 2026-11-12

The rest of this file was last fully revised on 2026-11-10. This section is newer; where they disagree, this one wins.

### 0.1 Engine (this repo): logging rewrite, BI scheduled planting, price deactivation, health check — on `fix/engine-price-sync-and-health-port`

- **Where it is:** pushed to `origin/fix/engine-price-sync-and-health-port`, 13 commits ahead of `experimental` (`8968f08`) including this handover commit. It is **not on `experimental`**, so beta does not have it. Pushing this branch deploys nothing; merging it into `experimental` and pushing that does (the owner's call). Commit hashes were rewritten when the branch was rebased: the logging commits are `c085244`, `a862602`, `47b91d5`, `c35a44c` and `923ebb5`.
- **The branch also carries three non-logging fixes** (all by the owner, 2026-11-11/12):
  - `49bb594` **BI scheduled planting**, using the FR yield-class soil (`get_sp_soil()` in `R/get_data.R`) until the owner confirms a dedicated SP soil; `REC_TYPE_COUNTRIES$SP` now includes `"BI"` (§7).
  - `393bee3` **removes prices the price service has deactivated**: a refresh is a full fetch when there is no cursor, `--full` is passed, or the last full fetch is older than `PRICE_FULL_SYNC_HOURS` (24 h). After a complete fetch it deletes API-sourced rows the service no longer lists, with guards (empty/failed answer removes nothing, seed rows and other countries untouched). Schema **v7** adds `country_price_settings.full_synced_at`; this replaces the old "clear `api_cursor` by hand" limitation (§8).
  - `564bd6f` **health check on the port the service really listens on**: `tools/check_health.sh` reads the service's systemd cgroup and `ss` (unit `Environment=`/`EnvironmentFile=` and `.env` fallback) instead of trusting `API_PORT`; `tests/test_check_health.sh` runs in CI. This is the "check against the wrong service" gap in §6.
- **What it does** (`R/logging.R`, tests in `tests/test_logging.R`, settings in `.env.example` and CLAUDE.md "Path and Server Configuration"):
  - **Channel stack, like Laravel's:** a log line goes to the channels in `LOG_STACK` (`file`, `stderr`, `stdout`; default `file,stderr`); `LOG_CHANNEL=stderr` uses one channel alone; `LOG_STACK=null` logs nothing. Each channel has its own level (`LOG_FILE_LEVEL`, `LOG_STDERR_LEVEL`, `LOG_STDOUT_LEVEL`, else `LOG_LEVEL`) and format (`LOG_FILE_FORMAT`, ... else `LOG_FORMAT`, `text` or `json`). The older `LOG_STDERR=false` still works: it means `LOG_STDERR_LEVEL=INFO`.
  - **Request id on every line** (`[req=...]`: the `requestId` the API sends, else a short one) and one `REQUEST DONE status=... type=... country=... duration_ms=...` line per request; slower than `LOG_SLOW_MS` (30 s) it is a WARN.
  - **Personal data is kept out of the log:** `redact_request()` replaces every `user*` and phone field with `<redacted>`; the email senders log the recipient through `mask_email()` (before this, seven log lines printed the farmer's address); `reports[].content` (base64 PDFs from `returnPdf`) is replaced by a size note (`redact_reports()`); `LOG_MAX_CHARS` (20000) cuts any single line.
  - **Logging never fails a request** (an unwritable `LOG_DIR` falls back to stderr). R warnings raised while serving a request become WARN lines with the request id; a warning the code already logged is not logged twice. DEBUG logs a short result summary, TRACE the whole result. `LOG_KEEP_DAYS` deletes old daily files. A `STARTUP` line lists the settings (never keys).
- **Why it was done:** the engine crashed when a very large line was logged (the `returnPdf` base64 PDFs). Reproduction attempts did not crash R itself, even with 264 KB lines; the cause may be the terminal or tool showing the output (a Rust panic `crates\events\src\process.rs ... not implemented` was in the crash text, not from R). `LOG_STACK`/levels give a way to keep large output off the console.
- **Verified (2026-11-12, re-run on this branch):** `test_logging` 165, `test_validate_request` 62, `test_akilimo_db` 82, `test_pdf_template` 48, `test_ic_units` 12, `test_small` 168 (includes `in_42_BI_SP_window2_riskAtt2`), `test_rw_regressions` 17, parse check 36 files. CI also runs `tests/test_check_health.sh` (fake `systemctl`/`ss`). Real-server runs of several stack configurations (file TRACE as JSON + console INFO as text; stdout only).
- **Pitfalls:**
  - `logging.R` loads the project's `.env` with `dotenv::load_dot_env()`, which **overrides variables already set in the environment** (so a systemd `Environment=LOG_...` loses to `.env`). Not changed; flag it to the owner before changing it.
  - `tests/test_logging.R` pins its own `LOG_*` settings after loading (`pin_log_settings()`), so it passes under any `.env`; keep that when adding tests. State lives in `.log_ctx`, which every `source()` of the file recreates: use the `ctx_reset()` helper in tests.
  - `logging.R` defines nothing called `mask_email`; `R/html_helpers.R` does, and `R/email.R` uses it.
- **Other owner commits on that branch (not this session):** `de90819` and `d4ae52b` validate that a FR or IC request offers at least one fertilizer (`has_available_fertilizer()` in `R/AkilimoMain.R`), plus two `dev.bat` debounce commits (`521accb`, `9f602bb`).

### 0.2 new-akilimo-api (Laravel, `D:\Dev\php\new-akilimo-api`): PDF reports on S3

Branch `feat/price-catalogue-views`, 9 commits not pushed when checked on 2026-11-12 (**the owner has pushed part of the branch since: see §0.6 for the current count**), newest then `bd4f288` (the owner's own `eadcb60` adds the Flysystem S3 adapter to `composer.json`; `bd4f288` updates Composer dependencies).

- **Reports go to S3 now (Alarik, `https://s3.munywele.co.ke`, bucket `akilimo-rec`, path-style).** The reports disk is `AKILIMO_REPORTS_DISK` if set, otherwise the app default `FILESYSTEM_DISK` (a `public` default becomes `local`, because a report holds the farmer's name, masked phone and location). Before this the reports setting defaulted to `local`, so PDFs kept landing in `storage/app/private/reports` even with `FILESYSTEM_DISK=s3`. `RecommendationReportService::resolveDisk()` is the rule.
- **Layout (flat):** `reports/{kind}-{request_id}.pdf`, e.g. `reports/fertilizer_advice-<uuid>.pdf`; `request_id` is the computation's row in `api_requests`. The engine makes four kinds (`RecommendationReportService::FILES`: `fertilizer_advice`, `intercrop_advice`, `PP_advice`, `SP_advice`); a report is found by those names, not by listing the folder, and a new kind must be added to `FILES` (otherwise it is skipped with a warning). Download links and the API answer are unchanged (`/v1/recommendations/reports/{id}/{file}`). Reports kept the old way (`reports/{id}/{file}`) are still read, never written. `links($id, $files)` takes the names `store()` returned, and the cached answer keeps them, so no lookup is needed per response.
- **Alarik quirks (found by testing against the real bucket):** it answers `exists()`'s HEAD request with **403** even for a file it holds, so the code asks with `size()` instead; and a lookup made within a few milliseconds of a write fails, so `reports:check-storage` waits up to 3 s. Reads, listing, size and downloads work. Alarik was briefly unreachable once (`cURL error 7`); retry before debugging code.
- **S3 disk defaults** (`config/filesystems.php`): `visibility private`, `throw true`, region `us-east-1`, path-style on, 5 s connect / 30 s timeout, 2 retries, endpoint and bucket defaulted. `RecommendationReportService::store()` catches a storage error, logs the message (never the PDF) and skips the file, so a storage outage cannot fail a recommendation.
- **`php artisan reports:check-storage`:** saves a tiny fake report the way a recommendation does, reads it back, makes a signed link, streams the download, fails if an S3 object is readable without credentials, deletes the test files (`--disk=`, `--keep`). Never prints keys. **Run it on every server after changing the env.**
- **To switch a server to S3:** `composer install` (adapter), set `AWS_ACCESS_KEY_ID` and `AWS_SECRET_ACCESS_KEY` and `FILESYSTEM_DISK=s3` (or `AKILIMO_REPORTS_DISK=s3`), leave `AWS_BUCKET`/`AWS_ENDPOINT` at their defaults (`.env.example` has them commented out on purpose: an empty value there would override the defaults), clear cached config, run `reports:check-storage`. Reports already on the local disk are **not moved** (the owner declined a migration command); their links keep working only while the disk is `local`.
- **Tests:** whole suite **379 passed** (2026-11-12), including 19 Feature report tests and `tests/Unit/RecommendationReportServiceTest.php` / `tests/Unit/Console/CheckReportStorageTest.php` (no database needed).
- **Test database trap:** Laravel's `migrate:fresh` skips the wipe when the `migrations` table is missing. A failed schema load left `akilimo_test` with views and no `migrations` table, so every run failed with "Table 'cassava_prices' already exists" and `$dropViews = true` never got a chance. Fix: `php artisan db:wipe --env=testing --drop-views --force` (the owner ran it). A self-healing hook in `tests/TestCase.php` was proposed, **not done**.
- **Moved:** `ComputePayloadTypesTest` is now in `tests/Feature` (it seeds and posts to the API, so it needs the database reset that `tests/Pest.php` gives only `Feature`).
- **Not mine, left alone:** `composer.lock` and other owner changes.

### 0.3 akilimo-mobile (Android app): UI redesign and hub sync/offline status strip

Uses the repo at `D:\Dev\android\akilimo-mobile`; read its `AGENTS.md` before changing it, and `docs/UI_UX_CHECKLIST.md` (the reference for building and reviewing screens, with what is done and what is open). **State on 2026-11-12:** everything below is **committed** on `feat/ui-design-system`, which is **8 commits ahead of `origin/feat/ui-design-system` (`95aced54`) and 36 ahead of `origin/develop`**. Not pushed; no PR for that branch (the owner pushes and opens it). Last checked at `8a275b3d`: `:app:compileDebugKotlin`, `:app:testDebugUnitTest` and `detekt` pass. Nobody has run it on a device yet (the owner runs it from Android Studio).

**UI redesign (this fork's work, `local_98c08994`; every visible change was shown as a preview and approved first):**
- **Look:** palette follows the logo (deep green `#006F45`, leaf lime `#9ACA3C` used only as a fill with dark text); cards and buttons use 12dp corners; the app starts in the **light** theme and the owner picks Light / System / Dark in Settings.
- **Navigation:** first run is `LegalWizardRoute` -> `OnboardingRoute` (first-run wizard only) -> `HubRoute`; every later launch starts at `HubRoute`. The hub has cards for Recommendations, Profile, **Your farm** (`FarmRoute`) and Settings, plus the sync strip below. Settings has no profile entry any more.
- **Profile and Your farm** are two pages of one shared editor (`DetailsEditorScreen`, same sections and view model as the wizard): Profile = About you; Your farm = Country, Location, Field size and **Investment preference**. Planting and harvest dates, tillage and weed control are edited in their own recommendation steps (not repeated there); the wizard still asks for all of them on first run.
- **Screens:** recommendation result (readable text, text-size and landscape controls; merged earlier as #559), steps screen (progress summary and raised step cards with status chips), raised option cards, a segmented market choice, label-above form fields, grouped form cards, Settings as grouped cards, fertilizer search and category bar (only when the list has more than 10 items), the planting picker opening on today with each date field showing its allowed window (the harvest span is the owner's 235 to 452 days), a "Total weeding cost" row, one shared look for loading (skeleton rows), empty and error states, and a location step with a plain reason, a "Where is your farm?" title and an inside/outside chip. The manual location map opens on the chosen country (built-in centre and zoom for NG, TZ, GH, RW, BI) and no longer jumps to the phone's GPS.
- **Naming:** the engine's `risk_attitude` is called **Investment preference** in the app (Rarely / Sometimes / Often = 0 / 1 / 2). The MCP agent now uses the same words (§0.3a).
- **Open (mobile):** translations (sw, rw, ha) for the new strings (English only now; `MissingTranslation` lint is off); an **app-wide text size** setting (Settings selector plus `textScale` in `AkilimoTheme`) is built but the owner **deferred the decision** on 2026-11-12, so keep or remove it before release and do not add a second scale next to the result screen's own size control; the fertilizer category bar needs `/v1/fertilizers` to send `category` as `STRAIGHT`, `COMPOUND` or `OTHER` (not checked against live data); save/share/print of a recommendation, a shorter first run and landscape/tablet layouts on every screen are in the checklist, not started.

**Hub sync/offline status strip** (written by the Rwanda session, committed as `1851b605`):

- **What it shows:** a strip pinned to the hub's bottom bar — offline ("Offline — showing saved data"), updating ("Updating data…"), the last update time ("Updated just now" / "N minutes|hours|days ago"), a permanent failure with a **Retry** button, or "Not updated yet". Offline wins over a running sync; only `RUNNING` work counts as syncing (ENQUEUED work may be waiting for a network that is absent).
- **Where it lives:** `workers/SyncStatus.kt` (phase model: OFFLINE > SYNCING > FAILED > SYNCED > NEVER), `workers/SyncStatusSource.kt` (`@Singleton`; combines `NetworkMonitor.isConnected`, six `getWorkInfosForUniqueWorkFlow` names and two DataStore timestamps), `ui/viewmodels/HubViewModel.kt`, `ui/components/compose/SyncStatusStrip.kt`, wired in `ui/screens/hub/HubScreen.kt` (`hiltViewModel()` plus `Scaffold(bottomBar = …)`).
- **How the timestamps are written:** `base/workers/BaseApiWorker.kt` calls `AppSettingsDataStore.recordSyncSuccess/recordSyncFailure` (`LONG_PREF_LAST_SYNC_AT`, `LONG_PREF_LAST_SYNC_FAILED_AT`); Retry runs `DataRefresher.all()`, a new parallel wrapper over the app-start syncs (fertilizer chain, investment amounts, cassava market/units/starch factories, maize units).
- **Other changes:** `NetworkMonitor` is now a Hilt `@Singleton` (`di/AppModule.kt`) and is injected into `AkilimoApp`, so the app and the status source share the connectivity callbacks; `DataRefresher.maizeUnits()` was added because app start already syncs maize units.
- **Strings and lint:** the new strings in `res/values/strings.xml` stay English until the sw/rw translation pass, and `app/build.gradle.kts` now disables the `MissingTranslation` lint check (`disable += "MissingTranslation"`) so that gap does not fail `lintDebug`. **Never** use `tools:ignore` on string resources.
- **Verified:** `:app:compileDebugKotlin` and `:app:testDebugUnitTest` pass (new `SyncStatusTest` 5 tests and `HubViewModelTest` 3 tests, 0 failures); `detekt` reports 0 smells; `lintDebug` passes with 51 warnings and 0 errors.


### 0.3a akilimo-mcp-agent (MCP server and web chat): investment preference wording

Repo `D:\Dev\php\akilimo-mcp-agent` (the **Akilimo 1.0** MCP server `akilimo-1.0` and its web UI; its `AGENTS.md` points to its own `docs/HANDOVER.md`, which was **not** updated). Part of the same ecosystem: it calls new-akilimo-api, which forwards to this engine, like the mobile app.

- **Where it is:** branch `fix/investment-preference-wording`, one commit `99f23f8` on top of `main` (`46418ed`); **not pushed, no PR**. Six files: `packages/server/src/chat/agent.ts`, `packages/server/src/tools/compute-recommendations.ts`, `packages/shared/src/farm.ts`, `packages/web/src/components/ChatPickers.tsx`, `ChatView.tsx` and `GuidedParts.tsx`.
- **What changed:** the farmer-facing "risk attitude" is now **investment preference**, to match the mobile app. The question reads "What is your investment preference?" with the options **Rarely**, **Sometimes**, **Often**, followed by the app's explanation. The fixed chat message is now "My investment preference is <word> (risk_attitude <0-2>)" and the agent prompt reads exactly that sentence and the new rule ("0 rarely (careful), 1 sometimes (balanced), 2 often (willing to take a risk)"). **Unchanged on purpose:** the engine field `risk_attitude` and the saved profile key `risk` (API contract and saved profiles).
- **Mapping as the agent documents it:** 0 Rarely = careful (fertilizer advised only when the extra income is at least 1.8 times its cost), 1 Sometimes = balanced (at least the cost), 2 Often = willing to take a risk (even a small gain).
- **Verified:** `pnpm build` passes. There is no test suite or linter there. **Not verified:** a chat that sends the new sentence and shows the agent reading it; do that once before this branch ships.
- **Tracking the MCP repo (checked 2026-11-13):**
  - **Git:** `main` = `origin/main` = `46418ed` ("advice and its PDF report in Kiswahili or English"). Work tree clean. The investment-preference branch is one commit ahead of `main` and **has no upstream** (never pushed). Pushing to `main` runs `docker-publish.yml`, which also publishes the weather image, so push the branch, not `main`.
  - **Its own `docs/HANDOVER.md` is stale** (written 2026-10-26, updated 2026-11-03): it still says `main` has unpushed local commits (they are pushed now) and it does not mention the investment-preference change or the farmer accounts. By the owner's rule this file is the handover; the MCP repo's file is only a deeper reference (architecture, features and how each was verified, decisions, gotchas, local run, open items, leftovers on the dev machine).
  - **How it touches `new-akilimo-api`:** it calls the API with **its own key** (`X-Api-Key`, ability `mcp:verify-keys` plus the compute and reference abilities), checks users' MCP keys through `POST /v1/mcp/verify-key` (`docs/MCP_KEYS.md` in the API repo), and gets report links through `reports:read`. From the API's point of view its compute calls are **anonymous**: they carry no farmer token, so they get no `user_id` and still get a fresh `request_id` per call. **The farmer accounts work needs no change in the MCP repo.**
  - **Three separate account systems now exist and none is linked:** (1) the MCP web chat's own accounts (PostgreSQL, email + password + unique username, saved chats, roles; no email verification, password reset, deletion or export), (2) the Laravel web playground users (`/playground`), and (3) the new farmer accounts in the API (`/v1/account`, which does have verification, reset and deletion). The owner should decide whether the chat should later sign in with farmer accounts, and whether farmer accounts and the playground users should stay apart.
  - **Open items from its handover (not done by these sessions):** push the branch; the planned **AKILIMO MCP Hub** (`docs/PRD_MCP_HUB.md`, decisions of 2026-11-05, terms text still to be written); production deployment through `proxy-tool` (`stacks/akilimo-mcp`, `agent.akilimo.org` and `akilimo-mcp.akilimo.org`, never deployed, DNS and database `akilimo_mcp` still needed; the images were never built on the dev machine); data-protection agreement with IITA IT Operations & Information Security (chats hold farm locations); no automated tests; one full compute conversation through chat not run (it would write to production); suggested API fixes (surface `data.message` of compute errors in `RecommendationService.php`, align rule names with DTO field names); a Nominatim lookup that sends rounded coordinates to OpenStreetMap; leftovers on the dev machine (throwaway PostgreSQL container `akilimo-test-pg`, a dev server on port 3001, a weather MCP via `uvx` on `127.0.0.1:8090`).
- **Other naming differences left alone (decide later):** the agent says "field" where the app says "farm" ("Field size" vs "Farm size"), "budget" (`maxInvestment`, for the whole field) where the app says "Investment amount" (per area unit), and "planting/harvest window" where the app says "Flexible dates".
### 0.3b new-akilimo-api: farmer accounts (optional sign-in, saved advice)

Same repo and branch as §0.2 (`D:\Dev\php\new-akilimo-api`, `feat/price-catalogue-views`, **partly pushed, see §0.6**; the owner chose **not to split the branch**). Built from `docs/FARMER_ACCOUNTS_DESIGN.md` (decisions in its §8 are final); steps 1 to 5 of its §5 are done and committed (gitmoji, no co-author): `d37a246`, `845c0a4`, `e5a85d7`, `1677679`, `9d8e6fd`, `4e07aa2`, `5368d7b`, `d74d7c5`, `6c0608b`, `261386f`, `2382b12`, docs/config cleanup `9003d87`, then `19a6b6d` (`compute` answers a signed-in farmer with the saved row's id), `db5e338` and `83a7b38` (Google on by default). **Reference for the next agent: `docs/FARMER_ACCOUNTS_API.md` in that repo** (endpoints, config, code map, open items); `docs/BREAKING_CHANGES.md` has the deploy notes.

- **What exists:** `/api/v1/account` register, login, consent, logout, email verify/resend, password forgot/reset, Google and other social sign-in (`/google`, `/social/{provider}`), history list/detail/delete, and account deletion (past requests kept with `user_id` and `device_token` cleared). `POST /v1/recommendations/compute` stays open; a valid farmer token (with consent) links the saved row to the account (`farmer.optional`), and that farmer's result is cached apart from the shared cache. Farmer tokens are named `farmer` and carry only `recommendations:compute` and `history:own`; a staff token or API key gets 403 on account routes.
- **Social sign-in uses laravel/socialite (the owner's choice), access-token flow, not ID tokens.** Socialite does not check which app a token was issued to, so Google is checked through tokeninfo against `GOOGLE_CLIENT_ID`; add a check per further provider before switching it on. Identities are in `user_identities` (not `users.google_id`). Provider ID, secret and redirect URI all come from env (`<PROVIDER>_CLIENT_ID|CLIENT_SECRET|REDIRECT_URI`, blocks for more providers are commented in `config/services.php`); secret and redirect are unused by the token flow.
- **Off until configured:** social sign-in needs `FARMER_SOCIAL_PROVIDERS` and the client IDs; verification and reset email are sent only when `MAIL_MAILER` is not `log` (or outside production). `FARMER_CONSENT_VERSION` labels the consent; the **final consent and benefit wording is still the owner's to supply**.
- **Migrations** `2026_11_12_000021` to `000023`: `users.password` nullable plus `consent_at`/`consent_version`; table `user_identities`; `api_requests.user_id` (FK `ON DELETE SET NULL`). New dependency `laravel/socialite`.
- **Verified (2026-11-12/13):** account tests in `tests/Feature/Account/` (57) pass; the whole suite passed **436** when the network was up. On 2026-11-13 the last full run showed **5 `SeedersTest` failures that are not from this work**: `CountriesSeeder` downloads a countries CSV from `raw.githubusercontent.com` while seeding and the machine had no internet ("No such host is known"); they pass when the host is reachable (making that seeder offline is a separate fix). Two older tests (`FertilizerNormalisationTest`, `PriceChoiceViewsTest`) were adjusted (`2382b12`) because an owner commit (`af6285d`, regenerated Base models) made the read-only views refuse mass assignment. The API's own docs: `docs/FARMER_ACCOUNTS_API.md`, `docs/BREAKING_CHANGES.md`, `CLAUDE.md`, `docs/ARCHITECTURE.md`.
- **Open:** (1) owner: the hosted email service and sending domain, the Google Cloud client IDs, the consent wording; (2) **mobile app:** built (see the next bullet), still to run on a device and translate; (3) audience checks for other providers; (4) keep `user_id` and `device_token` out of any public export of `api_requests`.
- **Mobile side (`D:\Dev\android\akilimo-mobile`, branch `feat/ui-design-system`, committed, not pushed): built, not run on a device.** Commits `8ae0b481` (data layer), `19e4f5ab` (screens), `fc59a7ae` and `a20794ed` (start-up prompt, snooze, Google default), `b98bb3bf` (signed-in cues and theme), `07b6cb92`, `924db420`, `210cce0a` and `4d9de49a` (logo, About). Package `com.akilimo.mobile`:
  - **Data:** `network/AccountApi.kt` (+ `network/dto/AccountDtos.kt`); `ApiClient` takes an optional `bearerToken` provider; `data/account/AccountSessionStore` keeps the session encrypted with an Android Keystore AES-GCM key (`TokenCipher.kt`; below API 23 it stays in memory only); `repos/account/` has `AccountRepo`, `SavedAdviceRepo`, `AccountCalls` (all outcomes become `AccountResult`; a 401 on a signed-in call signs the farmer out); Room v9 table `saved_advice` (migration `MIGRATION_8_9`, schema `9.json`); `GetRecommendationViewModel` sends the farmer's Bearer token on `compute` and keeps a copy of the advice (the answer's `request_id` is the saved row's id).
  - **Screens:** `ui/screens/account/` — `AccountScreen` (sign in / create account tabs, Google button, consent step, signed-in details with email confirmation, sign out and delete account), `ForgotPasswordScreen`, `SavedAdviceScreen` and `SavedAdviceDetailScreen` (read, share, delete), `ResultAccountNote` (under the advice: "saved in your account" or an offer to sign in). Entry points: a Hub card (Saved advice when signed in, "Sign in to save your advice" otherwise) and an Account row in Settings. Routes `AccountRoute`, `ForgotPasswordRoute`, `SavedAdviceRoute`, `SavedAdviceDetailRoute`; strings in `res/values/strings_account.xml` (English only).
  - **Start-up prompt and sign out (added later):** after start-up the hub shows a dialog "Sign in to keep your advice" with Sign in, Create account and Not now (`AccountPromptState`: once per app launch, only while signed out; **Not now** = this launch only, **Remind me tomorrow** = 24 h and **Remind me in a week** = 7 days, kept across restarts in SharedPreferences `account_prompt`; `AccountRoute(signUp = true)` opens the sign-up tab). A signed-in farmer is never prompted. **Settings → Account** shows the account row plus a Sign out row (with a confirm dialog; it clears the session and the phone's saved copies).
  - **Signed-in cues (added later):** a successful sign-in now stays on the account screen with "You are signed in." (it no longer pops back silently), the Hub card reads "Signed in as <name>", and a signed-in farmer gets an **IITA-coloured theme**: `AkilimoTheme(signedIn)` swaps in `AkilimoIitaLightColorScheme` (the CGIAR dark green `#07362A` as the primary) or `AkilimoIitaDarkColorScheme` (the IITA orange `#F26422` as the primary), an orange line under every `BackTopAppBar` (`LocalSignedIn`, `bottomAccentLine`) and an orange "Signed in as" pill in the Hub header. The two colours were **sampled from the full IITA + CGIAR logo image the owner supplied** (orange is the IITA wordmark, the dark green is the CGIAR network colour; AKILIMO is an IITA product and IITA is a CGIAR centre, so both belong to the app's identity), not taken from brand guidelines; the owner should confirm them. That logo is bundled as `res/drawable-nodpi/logo_iita_cgiar.webp` (a raster with a white background) and shown, on a white card of its own and with **no tagline or credit line**, by `PartnerLockup`: at the bottom of the Hub and under the sign-in form (for everyone), and as the **last card on the Settings page**. A transparent or vector version from IITA would let it blend into the header. Check IITA's and CGIAR's brand guidelines for minimum size and clear space. **Settings → About** also has a DRAFT "About AKILIMO" paragraph (`settings_about_body`, adapted from the description on akilimonigeria.org and narrowed to what holds for every country) and an "AKILIMO Nigeria website" row that opens https://akilimonigeria.org in the in-app web view; that site is **AKILIMO's Nigeria chapter** (the owner's description; the site itself calls it the AKILIMO Nigeria Association, "ANA"), covering Nigeria and cassava, with its own "Join ANA" sign-up and login. The owner should approve the wording, and decide whether the row should show only to farmers in Nigeria (today every farmer sees it, labelled "For farmers in Nigeria") and whether Nigerian farmers need one sign-in across the chapter site and the app. Contrast is unit-tested (`SignedInThemeTest`). Not seen on a device yet.
  - **Deleting is a soft delete (owner's rule, 2026-11-13).** Deleting one saved item, all saved items or the account **never removes a row from `api_requests`**, because the requests are still needed for statistics. The row is released: `user_id` set to null and the phone's `device_token` **cleared** (without clearing it the same phone would claim the item back at the next sign-in; a test covers that). Code: `ApiRequestRepo::releaseForUser` / `releaseAllForUser`, and `FarmerAccountService::deleteAccount` for accounts (API commit `5d64f01`). The app's delete texts now say the request may be kept without the farmer's name, for statistics (draft wording for the owner to approve). The location and answers stay in those rows, so keep them out of any public export; PDF reports stored for a released request are also kept. **Not done:** statistics queries were not reviewed for this; they should treat `user_id IS NULL` rows as normal. If the owner prefers a `deleted_at` column that keeps the link to the person, the global scope would have to be checked against every request/statistics query first.
  - **Swipe and multi-select on saved advice (built 2026-11-13, like a mail inbox).** **Swipe an item to the right to delete it**; **long-press** an item to start selecting, then **tap** to add or remove (the checkbox replaces the icon, the top bar shows "N selected" with **Select all** and **Delete**, Back or ✕ leaves selection mode); swiping is off while selecting. Both delete paths hide the items at once and show an **"N deleted. Undo"** snackbar; the request goes to the server **5 seconds later** (`SavedAdviceViewModel.UNDO_MILLIS`), or **at once when the farmer leaves the screen** (`flushPending`), unless undone; undo only brings back its own batch; if the request fails the items come back with a message. **Swipe left shares** the advice text through the system share sheet (the row springs back; `SavedAdviceViewModel.share`, `Event.Share`; if the text is not on the phone yet it is fetched first, and if that fails the farmer is told and nothing is shared; commit `feat(account): swipe left to share`, 19 view-model tests). API: new **`POST /v1/account/history/delete`** `{"ids": [...]}` (up to 500, throttle `write`, the same soft delete, ids that are not the caller's or no longer exist are skipped so a repeat is harmless; API commit `7acbd41`, 447 tests pass), because one request per item would hit the 30 a minute write limit; the app sends 500 ids per request at most. App: `SavedAdviceRepo.deleteMany`, selection and undo in `SavedAdviceViewModel`, `SwipeableAdviceRow` in `SavedAdviceScreen`; tests `SavedAdviceViewModelTest` (16) and `SavedAdviceRepoTest` (24). Known edge: if the app is killed during the 5 seconds, the item simply reappears at the next sync (the safe direction). Not seen on a device.
  - **Long histories (1000+ saved items), built 2026-11-13.** Problems found and fixed: the app's sync **capped at 500 items** (10 pages x 50) and **re-downloaded every page on each open**; the API list read whole rows including the large stored request and answer; the screen held every row at once; no filter and no "delete many". **API** (`new-akilimo-api`, commit `53b99c4`): the list selects only the columns it shows; new `GET /v1/account/history/ids` (all ids, newest first, at most 5000, `meta.truncated`) and `DELETE /v1/account/history` (delete all of the caller's saved advice, keep the account, throttle `sensitive`). Tests: 1200 items paged without loss or repeat, the list query never selects `plumber_response`/`droid_request`/`plumber_request`, ids, truncation, delete all and its scoping (63 account tests; whole suite 442 passed). **App** (this repo): `SavedAdviceRepo.refresh` asks for the ids first (one small request); drops local items the server no longer has (deleted on another phone); downloads **only the missing items**, newest first, 100 a page, and stops as soon as they are all found (so "nothing new" is one request; a new phone with 1200 items is 12 page requests; safety stop at 20,000 items; if the id list is cut, a page already on the phone ends the paging and nothing is dropped). The list reads from Room **a screenful (50) at a time** and grows as the farmer scrolls (`observePage` with `LIMIT`, `loadMore`), shows the total count, and has **filter chips by use case and country** (country chips only with more than one country) and a **Delete all saved advice** menu item with a confirm dialog. Tests: `SavedAdviceRepoTest` (20) and `SavedAdviceViewModelTest` (7). **Not built:** an automatic "keep only the last N" rule (the design says advice is kept until the farmer deletes it, so that needs the owner's approval); text search inside the advice (the list carries no text); prefetching the text of old items for offline reading (an old item still needs a connection the first time it is opened). Not run on a device with a real 1000-item account.
  - **Logo in dark mode (done with raster files, vector still wanted):** `PartnerLockup` now shows two **official** files on a card in the theme's own colour: `res/drawable-nodpi/logo_iita_cgiar.webp` (standard, white background, light theme) and `res/drawable-nodpi/logo_iita_cgiar_reversed.webp` (the **reversed** logo the owner supplied: transparent background, white IITA wordmark and tagline, CGIAR green blocks, dark theme; it has no "Science for a food-secure future" line). The choice is made **in code from the app's own theme** (`colorScheme.background` luminance), **not with a `-night` resource**, because the app's Light/System/Dark setting can differ from the phone's and a `-night` file would put a white wordmark on a light page. Nothing was recolored or cut out. Checked by compositing both files on the real surface colours (`#FDFDF6` and `#141811`); not seen on a device. The 2000x645 rasters are enough for a card at this size. **Still wanted, optional:** the `.ai` vector exported as two SVGs (standard and reversed; text converted to outlines, flat colours, transparent artboard), converted to Android vector drawables, to replace the two webp files and stay sharp at any size; keep the same resource names. Original plan for that, kept for reference: export **two SVGs** (standard and the **reversed version for dark backgrounds**, from the brand pack; text converted to outlines, flat colours, transparent artboard), convert them to Android vector drawables (Android Studio Vector Asset, or a converter), save as `res/drawable/logo_iita_cgiar.xml` and `res/drawable-night/logo_iita_cgiar.xml` (Android then switches with the theme), delete the white card in `PartnerLockup`, and rerun build, tests and lint. Until the files exist, an interim option is a softer off-white plate in dark mode.
  - **Google Cloud setup (for the owner):** create an **Android** OAuth client per signing key (debug, release, and Play App Signing's certificate), with package `com.akilimo.mobile` and the SHA-1 (`./gradlew signingReport`); no secret, **no redirect URIs and no JavaScript origins** (those belong to Web clients, which the app does not use); put the client ID(s) in the API's `GOOGLE_CLIENT_ID` (the app itself needs no client ID). Consent screen: scopes `openid`, `email`, `profile` only (no Google review needed), a **public https privacy-policy page** (the app's `privacy_policy.html` is bundled and cannot be reached by Google, and it **does not yet describe accounts**: the owner approves that wording), the home page, and the authorized domain. Keep it in Testing with test users until release, then publish. **Not verified:** which client ID Google puts in the token's `aud` for an Android sign-in: check once with `https://oauth2.googleapis.com/tokeninfo?access_token=<token>` and make sure that value is in `GOOGLE_CLIENT_ID`.
  - **Dev build against a server:** the app has no cleartext-http allowance, so Android 9+ blocks `http://localhost` and `http://10.0.2.2`; use an https address (beta or a tunnel) or add a debug-only network security config. Set `AKILIMO_BASE_URL` for the build. With `MAIL_MAILER=log` the email codes are written to the API's `storage/logs/laravel.log`. The phone or emulator needs Google Play Services and a Google account that is listed as a test user.
  - **Google:** `GoogleSignIn.kt` uses Play Services `AuthorizationClient` and sends the **access token**; the button is **on by default** (`GOOGLE_SIGN_IN=false` or `-Pakilimo.google.signin=false` hides it) and the API also has Google on by default (`FARMER_SOCIAL_PROVIDERS=google`), but it only works once the owner's Google Cloud OAuth client exists (package name + signing SHA-1) and `GOOGLE_CLIENT_ID` is set on the server; until then the button shows and fails with a message. Credential Manager was not used: it returns an ID token, which the API does not take. For other providers AppAuth-Android would be the library; not added.
  - **Verified:** `compileDebugKotlin`, the whole unit suite (including 36 new account tests), `detekt` (0 smells) and `lintDebug` pass. **Not done:** a run on a device or emulator against a server with the account routes; sw/rw/ha translations; the owner's final consent text (the app shows a clearly marked draft, `account_consent_body`).
- **Pitfall:** the tests share the MariaDB `akilimo_test` database; two test runs at once (two worktrees, an IDE) fail at random with missing tables or deadlocks. The old worktree `D:\Dev\php\new-akilimo-api-accounts` (branch `feat/farmer-accounts`) was deregistered; its folder may remain and can be deleted, as can the branch.

### 0.4 Sessions (all of them, including forks)

Seven Claude Code sessions worked in this family. They form a tree; the notes folder `C:\Users\masgeek\AppData\Local\Claude-Data\claude_pzs8sxrjxfjjc\side-session-notes\local_fd7b1fcd-cc6d-45f9-9bab-efff8b3a7f84` holds shared notes (an older engine note there is superseded by this file). To see what a session did, read its transcript (`search_session_transcripts`, `list_events`); only metadata is recorded here. Session ids are in the app, not secrets.

| Session (name) | Id | Folder | Started by | State on 2026-11-12 |
|---|---|---|---|---|
| **R akilimo** (root) | `local_fd7b1fcd-cc6d-45f9-9bab-efff8b3a7f84` | repo root | the owner | idle; started the sessions below and a Rwanda-coverage task |
| **Rwanda requests rejected as outside coverage** (this one) | `local_249e466c-2862-4629-addd-a86a859176c7` | worktree `.claude/worktrees/vigilant-mclean-c38063` (detached HEAD; its branch was merged and deleted) | R akilimo | running; did §0.1, §0.2, §0.3 (mobile hub status strip) and the Rwanda/Burundi fixes below |
| **Show API key once after creation in new-akilimo admin** (`show-api-key-once-after`) | `local_69d0f50c-254a-4af6-b5ad-dbb9c30a8923` | repo root | R akilimo | idle; new-akilimo-api API-key work (§4) |
| **JSON payload troubleshooting** (`payload-debug`) | `local_a881294c-0ca2-423d-9f82-406e96764287` | repo root | show-api-key-once-after | idle |
| **Mobile app: adapt to API breaking changes** (`mobile-api-updates`) | `local_b268e454-4a06-42ed-850d-41ee4827df0f` | `D:\Dev\android\akilimo-mobile` | show-api-key-once-after | done; akilimo-mobile PR #554 **merged** |
| **Mobile app UI review** (`ui-review`) | `local_e452191d-022a-4d34-b9e3-a517b78d6826` | akilimo-mobile | mobile-api-updates | done; akilimo-mobile PR #558 **merged** |
| **Mobile app UI review (fork)** (`mobile-app-ui-review-for`) | `local_98c08994-ad9b-4959-9cdd-2c3e32b09aa1` | akilimo-mobile | mobile-api-updates | idle; a **fork** of the UI review: did the mobile UI redesign (§0.3) and the MCP wording change (§0.3a); no PR |

Tree: R akilimo → { this session; show-api-key-once-after → { payload-debug; mobile-api-updates → { ui-review; the UI-review fork } } }. None is archived. This session's mobile work (§0.3) and the three mobile sessions touch only `akilimo-mobile`; check its `AGENTS.md` (§4) before continuing that work.

What this session did earlier, in short (all merged into the owner's branches; see git history):
- **Rwanda `/compute` errors:** the cause was scheduled planting and intercropping returning `success` with the fertilizer "out of domain" text or an empty recommendation, while `/coverage` showed the point as covered. They now return clear 400s (`REC_TYPE_COUNTRIES` in `R/AkilimoMain.R`), `ekari`/`hekta`/`string` area units are accepted, `getRFY()` falls back to the NG curve for RW/GH/BI (a small-field FR crash), the coverage gate applies only to FR and SP, and `/coverage` features list `rec_types`.
- The real failing request was never received; the SP fix is the likely cause, not a confirmed one.
- **Mobile hub sync/offline status strip** on akilimo-mobile `feat/ui-design-system` (§0.3): new sync-state source, hub ViewModel and strip, success/failure timestamps from the worker base class, a parallel `DataRefresher.all()` for Retry, plus tests and a `MissingTranslation` lint exemption. Uncommitted; the owner commits and opens the PR.

### 0.5 Open items, in order

1. **Owner:** merge `fix/engine-price-sync-and-health-port` into `experimental` when ready (beta then gets logging, BI scheduled planting, price deactivation sync and the new health check); push `feat/price-catalogue-views` in new-akilimo-api (the 9 S3/report commits plus the farmer accounts commits of §0.3b, one branch); run, review and push akilimo-mobile `feat/ui-design-system` (§0.3: hub strip and UI redesign, committed, 8 commits not pushed) and open its PR; review and push the akilimo-mcp-agent branch `fix/investment-preference-wording` (§0.3a) after trying one chat.
2. **On each server that should use S3:** set the env, `composer install`, clear config, run `php artisan reports:check-storage` (§0.2).
3. **Optional:** a self-healing hook in `tests/TestCase.php` for the `migrations`-table trap (§0.2); decide whether real environment variables should beat `.env` in `logging.R` (§0.1).
4. **Owner decisions (mobile/MCP):** keep or remove the deferred app-wide text size (§0.3); translations for the new mobile strings; check the `category` values in `/v1/fertilizers`.
5. **Farmer accounts (§0.3b):** supply the email provider, Google client IDs and consent wording; **send the IITA/CGIAR logo as SVG (standard and reversed) so the dark-mode logo can be fixed**; publish a public privacy-policy page that covers accounts; run the app against a server with the account routes (API and app are both built and unit-tested, never run together); translate the new strings; add audience checks before enabling any provider besides Google.
6. Still open from the older sections: confirm the beta crash-loop is gone after `API_PORT=8001` and the `564bd6f` health check; send a RW **and a BI** scheduled-planting request to beta; decide whether BI keeps the FR yield-class soil for SP or gets a dedicated source (§7).
7. **MCP agent repo (§0.3a, tracked in detail):** push `fix/investment-preference-wording` (not `main`), try one chat with the new wording, and decide the three-account-systems question and the MCP Hub / production deployment items listed there.

### 0.5a Unit-test workflows reviewed and improved (2026-11-13)

Reviewed `unit-test.yml` in `new-akilimo-api` and `akilimo-mobile` (the engine's `ci.yml` was read and is fine; its R tests were not reviewed). **Nothing was run on GitHub**: each change was checked locally (the new Gradle command, the summary scripts on real reports, the YAML parses, the suite with `public/build` removed), so **watch the first run of each**. Commits are on the current feature branches, not pushed.
- **API** (`491d6be`, `db5a6bf`): removed the test-result sentinel cache (it skipped re-runs of a passed SHA and hid flaky tests, and the push and pull-request runs raced it anyway) and its ~14 `if:` conditions; **removed the Node install and `pnpm build`** (`tests/TestCase.php` now calls `withoutVite()`; the full suite, 447 tests, passes with `public/build` removed); removed the `migrate` step (the Feature tests migrate themselves and the Unit tests do not use the database), `key:generate` and `pdo_pgsql` (`composer.json` asks only for `ext-pdo`); runs **`composer run test:ci`** (plain Pest) instead of `composer test` (`pest --bail`, which hid every failure after the first), with a JUnit report, a step summary and an uploaded `test-results` artifact; added `composer lint:check` (passes today) and a non-blocking `composer audit --no-dev`; added `permissions: contents: read`; one concurrency group per branch (`head_ref || ref_name`) so a push and its pull request replace each other; the new reusable action **`.github/actions/php-setup`** (PHP 8.4, `pdo_mysql`, `mbstring`, Composer download cache) is used by `unit-test.yml` and by `quality-checks.yml`, which **referenced `php-setup` and `node-build` actions that did not exist and would have failed**; its Node build step was dropped; added `.github/dependabot.yml` (actions, weekly); `CLAUDE.md` now says the tests use MariaDB 11.2 and PHP 8.4 (it said SQLite and 8.3). PowerShell note: `composer run test:ci -- --filter=X` needs `composer --% run ...` on Windows PowerShell, which swallows `--`; in CI (bash) it is fine. Direct `composer test:ci` does not work here, so the workflow uses `composer run`.
- **App** (`a8fdc37f`): removed the "fetch latest tag" step (it installed a pip package and used `GITHUB_TOKEN`; its output was unused); only `MAPBOX_DOWNLOADS_TOKEN` stays in the environment (the AKILIMO API key and the other tokens were visible to pull-request code and unit tests do not need them; the build falls back to empty values) and the `-PMAPBOX_DOWNLOADS_TOKEN` copy on the command line is gone (`settings.gradle.kts` reads the environment variable); `--continue --build-cache` so a failing test no longer stops lint and detekt; the schema-drift check runs even after a failure; path filters now include `app/schemas/**`, `app/*.xml|yml|pro|json` and `gradle/**` (a schema-only change used to skip the schema check) and pull requests are checked for every target branch, not only `develop`; `timeout-minutes: 30`; a step summary of the test results; `.github/dependabot.yml`.
- **Not done:** pinning third-party actions by commit SHA; uploading the detekt SARIF to code scanning; trying `test:parallel` for the API suite (needs a database user that can create databases); a Room migration test (the app has none, and `androidTest` never runs in CI, so migrations such as 8 to 9 are untested by CI; an emulator job on `develop` would fix that); the engine's R tests.

### 0.6 Repository map (checked 2026-11-13)

Every git repository on this machine under `D:\Dev\akilimo`, `D:\Dev\php`, `D:\Dev\android` and `D:\Dev\tools` was listed. "Unpushed" is the commit count ahead of the branch's upstream. Only the first four are covered by this file; the rest were **not touched by these sessions** and their state is only what git shows.

| Repo | Branch | State | In this handover |
|---|---|---|---|
| `D:\Dev\akilimo\projects\akilimo-recommendations` (R engine) | `fix/engine-price-sync-and-health-port` | this file has uncommitted edits (the owner commits it); 2 commits unpushed | yes, §0.1 |
| `D:\Dev\php\new-akilimo-api` (Laravel API) | `feat/price-catalogue-views` | **partly pushed**: the remote branch is at `d74d7c5`, **7 commits are unpushed** (from `6c0608b` to `83a7b38`: secrets and redirects from env, `request_id`, Google on by default, docs); this corrects earlier "not pushed" notes in §0.2 and §0.3b | yes, §0.2 and §0.3b |
| `D:\Dev\android\akilimo-mobile` (Android app) | `feat/ui-design-system` | clean, **9 commits unpushed**, no PR | yes, §0.3 and §0.3b |
| `D:\Dev\php\akilimo-mcp-agent` (MCP server and chat) | `fix/investment-preference-wording` | clean; `main` equals `origin/main` (`46418ed`); the branch is **1 commit ahead (`99f23f8`) with no upstream, so not pushed** | yes, §0.3a (tracked in detail there) |
| `D:\Dev\tools\proxy-tool` (deployment stacks and Caddy) | `develop` | clean, nothing unpushed | partly: it holds the `akilimo-mcp` stack (§0.3a); it has its own `HANDOVER.md`, not updated by these sessions |
| `D:\Dev\php\akilimo-site` (WordPress site, `IITA-AKILIMO/akilimo-site`) | `develop` | clean | **no**. Likely home for the **public privacy-policy page** the Google consent screen needs; not checked |
| `D:\Dev\android\akilimo-nova` (Kotlin Multiplatform, Android and iOS, `IITA-AKILIMO/akilimo-nova`) | `feat/android` | clean | **no**. Appears to be a newer app; its relation to `akilimo-mobile` and to the farmer accounts is unknown |
| `D:\Dev\akilimo\shiny_server\PaperbasedDashboard_NG` (R Shiny dashboard) | `main` | **31 uncommitted files, 10 unpushed commits** | **no** |
| `D:\Dev\akilimo\shiny_server\UseUptake3` (R Shiny) | `ci/docker` | clean | **no** |

Other repos on the machine are unrelated to AKILIMO (fuelrod, secret-garden, fee tools, Laravel helper packages, personal tools, `rya-android`, `agwise-site`). `sg`, `deploy` and `personal` appear in this file only for older reasons.

**Decision for the owner:** whether `akilimo-nova` is meant to replace `akilimo-mobile` (the farmer accounts API would then need the same client work there), and which repo publishes the public privacy page.

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

> The branch and commit lists in this section are from 2026-11-10 and are kept for history. For the branch that is ahead of `experimental` now, see §0.1.

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
  - **CI image.** Written (`Dockerfile.ci`, the `Build CI image` job, the container on `R checks`) but never built or run; see §5.
  - **Health check and release tag.** Until the fix in `deploy.yml` (2026-11-10), `Check health` and `Tag release` were skipped on every push deploy, because the skipped optional data pull upstream made GitHub's implicit `success()` false. Both now name the restart/health result explicitly. Not verified: the first deploy after the fix is the first time the health check runs for real, and the first healthy deploy of either branch is the first time `github-tag-action@v7` and the GitHub release run (beta now tags too: `beta-x.y.z` pre-releases, first expected `beta-0.0.1`, and the tag action's version lookup should ignore tags with the other prefix, which is unverified). Look at those two jobs. (The Windows-built `test_small.rds` baseline now passes on Linux CI on `experimental`.)
  - Any request over HTTP against a running server.
  - A complete `/compute` request over HTTP that uses an activated catalogue price, e.g. a draft RW labour price activated in the admin.
  - A RW scheduled-planting `/compute` request against beta (the file is built there, but no request has been sent), and an end-to-end `/compute` request using an activated catalogue price.

## 3. Open gaps

| ID | Gap | Status / next step |
|----|-----|--------------------|
| DATA-1 | Scheduled planting: RW done (PR #33 merged, beta built its file); **BI implemented on `fix/engine-price-sync-and-health-port`** (`49bb594`), using the FR yield-class soil because BI has no points in `SoilData_4Country.RDS` | RW and BI: send a scheduled-planting request to beta once the branch is merged. BI: owner decides whether to keep the FR soil by yield class or add a dedicated SP soil source; see [§7](#7-next-task-sp-for-rw-and-bi-data-1) |
| DATA-2 | Kinyarwanda (`rw`) text empty | Needs a translator; fill the `rw` column of `translations.csv` |
| DATA-3 | Price service data gaps | **Mostly done in new-akilimo-api:**<br>- Burundi `BU`→`BI` (`3681ce2`, pushed).<br>- Currency set on every default and starch price (`2bd8760`).<br>- GH/RW starch factories added **inactive** with estimated prices; inactive factories are hidden from `/starch-prices` (`d80ccf5`, pushed).<br>Left: confirm the GH/RW starch prices, then activate them in the admin. BI has no starch buyer |
| DATA-4 | Prices are stale (fertiliser 1.3–7× below market in all five countries, both the defaults and the app's price options; Burundi FOMI is per 25 kg bag) | Reviewed with sources in new-akilimo-api `docs/data/price-review-2026-11.md`. Tanzania fertiliser updated to TFRA Sep 2025 (`87721bc`). The rest waits for the price catalogue (§8) and country-team review in the admin |
| DATA-5 | No confirmed post-planting labour defaults for RW, GH, BI. The engine has built-in fallback values only for NG/TZ; catalogue lookups run for every country | Draft estimates in new-akilimo-api `docs/data/labour-costs-rw-gh-bi.md` (low confidence). Country teams need to review and activate them in the catalogue (§8) |
| ARCH-1 | Prices live in three places that drift apart (API choice lists, API `default_prices`, hardcoded R values) | Design agreed: new-akilimo-api `docs/PRICE_CATALOGUE_DESIGN.md`. **Phase 1 completed**; see §8 |
| AGR-1 | PP advice is minimal | Needs agronomist-approved wording; see `docs/TECH-DEBT.md` |
| AGR-2 | Intercrop coefficients flagged "check with Pieter" | Agronomist review |
| — | `main` and `experimental` have diverged | Not a gap: the owner confirmed on 2026-11-10 that `experimental` will never be merged into `main`; they are separate lines of work with huge differences. Don't plan a merge; fixes needed on both branches are ported by hand |
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
- **akilimo-mobile** (Android app, Kotlin/Compose, `D:\Dev\android\akilimo-mobile`, `git@github.com:IITA-AKILIMO/akilimo-mobile.git`; on 2026-11-12 checked out on `feat/ui-design-system`, 8 unpushed commits ahead of `origin/feat/ui-design-system` `95aced54` and 36 ahead of `origin/develop`; no PR for that branch yet, while PR #552 "Next release" develop→main is open). PRs #553–#559 merged 2026-11-06 to 11-11. The owner works on it in a separate session. Read its `AGENTS.md` before changing it.
  - **How it reaches this engine:** it posts to new-akilimo-api `v1/recommendations/compute` (base URL `AKILIMO_BASE_URL`, default `https://api.akilimo.org`), which forwards to this repo's `/compute`. It never calls this engine directly.
  - **Reference data it reads** (`app/src/main/java/com/akilimo/mobile/network/AkilimoApi.kt`): `/v1/fertilizers`, `/v1/fertilizer-prices`, `/v1/fertilizer-prices/{key}`, `/v1/cassava-prices`, `/v1/maize-units`, `/v1/investment-amounts`, `/v1/starch-factories`, `/v1/cassava-units` and `/v1/coverage/{country}`. It does **not** read `/v1/maize-prices` (removed), `/v1/default-prices`, `/v1/prices`, `/v1/potato-prices` or `/v1/operation-costs`.
  - **Fertiliser price sheet** (`ui/screens/usecases/FertilizerScreen.kt`): one radio option per `fertilizer_prices` row.
    - `price_per_bag = 0` is **"I don't know"**: sent as price 0, so this engine fills in the catalogue price.
    - A negative value is **"Exact price"**: free text, any value.
    - Anything else shows its `price_range`.
    - `min_allowed_price`/`max_allowed_price` are stored but **never used**.
  - **Starch factories:** it keeps only rows with `factory_active` true (`dao/StarchFactoryDao.kt`), so inactive factories stay hidden even though the unfiltered `/v1/starch-factories` returns them.
  - **Labour costs** are only what the farmer types; there is no price list for them in the app.
  - **Hub sync status strip** (committed on `feat/ui-design-system` as `1851b605`, 2026-11-12; full write-up and the UI redesign in §0.3): `workers/SyncStatusSource.kt` combines `NetworkMonitor.isConnected`, the six watched unique-work names and two `AppSettingsDataStore` timestamps; `ui/viewmodels/HubViewModel.kt` plus `ui/components/compose/SyncStatusStrip.kt` render offline / updating / last update / failed with a Retry that calls `DataRefresher.all()`. `BaseApiWorker` records success and permanent failure via `AppSettingsDataStore.recordSyncSuccess/recordSyncFailure`. The new strings are English only; `app/build.gradle.kts` disables `MissingTranslation` (never `tools:ignore` on strings).
  - **When to check it:** any change to the shape of a price or factory endpoint, or to what a `0`/negative price means, must be checked against these files.
- **akilimo-mcp-agent** (MCP server and web chat, `D:\Dev\php\akilimo-mcp-agent`): the Akilimo 1.0 agent; see §0.3a. It reaches this engine the same way as the app: through new-akilimo-api `v1/recommendations/compute`.

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
  - `ci.yml` ("CI"): Wait for more pushes (deploy branches only) → Build CI image → R checks on every push and PR. On pushes it then calls `deploy.yml` as a reusable workflow, which runs from the pushed branch, not from `main`.
  - **CI image** (added 2026-11-10): `Dockerfile.ci` holds R, the system libraries and every R package. The `Build CI image` job pushes it to GHCR as `ghcr.io/iita-akilimo/akilimo-recommendations-ci:<sha>`, where `<sha>` is the full output of `git rev-parse HEAD:install_packages.R`, and builds only when that tag does not exist. `R checks` runs inside it (`container:` in `ci.yml`), so nothing is installed per run; the old `setup-r-env` action and R package cache are gone.
    - Only `install_packages.R` changes the tag. Editing `Dockerfile.ci` alone rebuilds nothing: also change `install_packages.R` (even a comment) or delete the tag in GHCR.
    - Use the full ID and never pipe it through `cut`: a shell that adds a byte-order mark gave 13 and 14 characters on two machines. Hashing the working file differs too, because Windows git converts line endings; the Git ID does not.
    - `docker-compose.yml` builds the same `Dockerfile.ci` locally (`docker compose build ci`, `docker compose run --rm ci`), tagged `local`, or whatever `GITHUB_SHA` holds if set. Nothing in CI uses it.
    - **GHCR package is public** (owner's choice, 2026-11-10; it holds only R, system libraries and public CRAN packages). The first pull was "denied" while the package was internal, because the workflow token is not an organisation member. `R checks` also has its own `packages: read` permission, since the workflow default is `contents: read` only.
    - **Not verified yet:** a green run that pulls the image. Watch it for `git` safe-directory warnings and file permissions on the restored data cache inside the container. The data download still uses `setup-python` 3.13 on a cache miss.
    - `tests/run_ci_test.R` runs one tinytest file the way CI does and fails if the file ran no checks.
  - The wait is first on purpose (changed 2026-11-10): a newer push cancels the older run's wait, so a burst of pushes runs the checks and deploy once. For that to work, runs on `main`/`experimental` get their own concurrency group; with one shared group GitHub queues the new run behind the old one, and it never reaches its wait. Don't put a shared workflow-level `concurrency` group back on deploy branches. Deploys still take turns through the per-server lock in `deploy.yml`.
  - `deploy.yml` ("Deploy"): Plan → Deploy to server → Tag release. `main` and `experimental` are separate lines of work that **will not be merged** (the owner, 2026-11-10), so each target has its own version sequence: production tags are plain (`1.2.3`), beta tags are `beta-1.2.3`, published as GitHub pre-releases that never become "latest". The tag job runs on either target once the deploy is healthy. Since 2026-11-10 the server work is one job with ordered steps (update code and packages, pull runtime data when `setup_data` is ticked, restart, check health) so the run graph stays readable; a failed health check fails the job and blocks the tag. A step that is skipped does not skip the steps after it. A required-check rule that names the old `Check health` job would need updating. It can be run by hand from the Actions tab, but a manual run does **not** run the R checks. Jobs after an optional job still need an explicit `if` naming the results they accept (`needs.X.result == 'success'`), as `Tag release` does.
  - `data-setup.yml` ("Data setup"): pulls OSF data onto a server on its own.
  - Deploy targets live in `.github/deploy-targets.json`.
  - Composite actions are in `.github/actions/`.

## 6. Suggested next steps

**Urgent (2026-11-10): the beta service is crash-looping.** The first deploy where `Check health` really ran (run `42758918609`, after the fix in `549a97f`) failed: `akilimo-api-beta.service` exits at start with `createTcpServer: address already in use`, and systemd's restart counter was at **6691** (a restart about every 18 seconds, so it has been failing for roughly a day and a half). Earlier green deploys hid this because the health check was being skipped. The cause is not known; nobody has looked on the server. Things to check there:

Run `sh tools/check_service.sh beta` in the beta checkout on the server (it exists there after the next deploy; `production` works too). It is read-only and takes every path from `.github/deploy-targets.json`, so nothing is typed by hand. It shows:

- whether the directory the deploy updates (`app_dir` in `deploy-targets.json`) is the directory the systemd unit runs from. This matters because the API reads `.env`, including `API_PORT`, from the folder it starts in, and the health check reads `$APP_DIR/.env`; if the two folders differ, they read different files, and the deploy may update a different copy than the service runs;
- `API_PORT` from that folder's `.env`. Beta is meant to use 8001 and production 8000, but the README and the unit file never said to set it, so beta may be falling back to 8000, production's port;
- what is listening on 8000 and 8001, and with `sudo` which process. A leftover R process from an earlier run would explain a port that stays taken;
- the service's state and its last log lines.

The repo used to name three different beta paths (`deploy-targets.json`: `/data/extra_storage/projects/akilimo-beta`; the README, `docs/SETUP.md` and the unit example: `/home/akilimo/projects/akilimo-beta`, stale; the owner reports `/home/akilimo/extra_storage/projects/akilimo-beta` on the server). `deploy-targets.json` is the source of truth; the README, SETUP and the unit examples now say so.

**Update 2026-11-10:** the owner has set `API_PORT=8001` in beta's `.env`, because production holds 8000. That matches the symptom (beta was binding production's port), but nobody has confirmed it yet: restart the service, then run the script above and `curl http://127.0.0.1:8001/health`. The deploy's health check already reads `API_PORT` from `$APP_DIR/.env` (default 8000), so it follows the `.env` value as long as `app_dir` is the folder the service runs from.

Until beta is fixed, the Deploy job's `Check health` step will fail on every push to `experimental`: that failure is real, not a flaky check. A health check that follows the service's real port (from the unit file as well as `.env`) would be a safer check, because today it can pass against the wrong service.

**Update 2026-11-12:** the safer check exists on `fix/engine-price-sync-and-health-port` (`564bd6f`): `tools/check_health.sh` finds the processes of the service's own systemd cgroup and the ports they listen on, and only accepts `/health` from there (falling back to the configured port, read from the unit and `.env`, when that cannot be worked out). It is tested by `tests/test_check_health.sh` in CI. This does not by itself fix the beta crash-loop; after the branch is merged and `API_PORT=8001` is in place, watch the first deploy and confirm with `sh tools/check_service.sh beta` and `curl http://127.0.0.1:8001/health`.


1. **Completed:** CI on the current work runs successfully; the beta all-prices refresh succeeded on 2026-11-09. PR #32 (documentation) is merged as `961902e`.
2. **Completed 2026-11-10:** PR #33 (RW scheduled planting) is merged (`adbc7a5`) and beta's deploy built `RW_WLY_LINTUL_2020SP.nc`. Still to do: send a RW scheduled-planting request to beta.
3. Before release, verify a `/compute` request against the deployed `/v1/prices` endpoint that uses an activated catalogue price.
4. BI scheduled planting remains blocked on the owner's soil-source decision. Hand DATA-2 / AGR-1 / AGR-2 to the IITA translation and agronomy owners.

## 7. Next task: SP for RW and BI (DATA-1)

PR #33 (`feat/data-1-rw-sp`) implemented scheduled planting for Rwanda from the existing yield RDS files and is merged (`adbc7a5`). Its generator is `tools/build_sp_nc.R`; `tools/ensure_sp_files.sh` runs it where the file is missing (CI, deploy, data pull); beta built the file on 2026-11-10. **BI is implemented on `fix/engine-price-sync-and-health-port` (`49bb594`, not yet merged):** it builds the same `.nc` from `Burundi_WLY_LINTUL.RDS` and uses the FR yield-class soil because BI has no points in `SoilData_4Country.RDS` (`get_sp_soil()` in `R/get_data.R`).

**No new LINTUL runs are needed.** `data/yield/Rwanda_WLY_LINTUL.RDS` and `Burundi_WLY_LINTUL.RDS`, the files FR uses, are the same LINTUL output in wide form:
- Columns: `plantingDate, lat, long, WLY_235 … WLY_452`. That is 32 harvest ages, the same as the `.nc` `daysOnField` dimension.
- No NAs, and no duplicate cell × planting date.
- Reshaped long, they give exactly the layout `get_WLY_15M_ncdf()` (`R/get_data.R`) reads:
  - dims `daysOnField`, `plant` and `cell` (`cellFromLonLat()` at 0.05°)
  - variable `WLY` in kg/ha
  - sorted by cell, then plant day, then daysOnField, so daysOnField varies fastest

| Country | Planting dates | Cells | SP soil |
|---------|----------------|-------|---------|
| BI | 53, weekly, all year | 805 | **none in `SoilData_4Country.RDS`**; uses `BI_FCY{1..5}_soilNPK.RDS` by yield class (the FR rule) |
| RW | 17: weeks 6–14 and 37–44 (the two seasons) | 782 | `SoilData_4Country.RDS`, all cells |

Steps:
1. **RW and BI NetCDF generation — implemented (RW in PR #33, BI in `49bb594`).** The script under `tools/` mirrors the existing preprocessing layout but starts from the wide RDS; `R/preprocess/` remains off limits. Its core transformation is:
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
  The script also verifies the output by reading sample cells through `get_WLY_15M_ncdf()` and comparing with the RDS. The generated `.nc` is not a tracked source file and needn't be published: `tools/ensure_sp_files.sh` rebuilds it, identically (checked byte for byte), wherever it is missing and the country's `*_WLY_LINTUL.RDS` is present.
2. **BI soil — implemented as option (b); the owner decides whether to keep it.** `get_sp_soil()` (`R/get_data.R`) uses `BI_FCY{1..5}_soilNPK.RDS`, picking the class from FCY the way FR does (`SP_SOIL_BY_FCY_COUNTRIES`). Option (a) — the producer of `SoilData_4Country.RDS` adds BI, one method for all countries — remains open and would only require removing BI from `SP_SOIL_BY_FCY_COUNTRIES`. RW was never blocked on this.
3. **Code — implemented:** RW is enabled in `REC_TYPE_COUNTRIES$SP` (PR #33) and BI in `49bb594`; the unconditional BI early return was removed from `get_WLY_15M_ncdf()`, and availability is decided by data/soil coverage (`get_sp_soil()` and `sp_soil_points()` feed both the recommendation and `/coverage`, so they cannot disagree).
4. **Tests and docs, included in PR #33 and `49bb594`:**
   - Update `tests/test_validate_request.R`.
   - Fixtures `in_38` (RW SP) and `in_39`, plus `in_42_BI_SP_window2_riskAtt2` for BI.
   - `tests/test_rw_regressions.R`.
   - Rebuild `tests/test_small.rds` with `UPDATE_BASELINE=1` after each intended result change (done on both branches).
   - In `docs/API-REFERENCE.md`, update the "Availability by country" table and the error list.
5. **Data: nothing to publish for RW or BI.** CI and the deploy build both files (step 1). Publishing a real LINTUL-run file in the bundle later is still possible and wins over the generated one. BI needs no separate soil file, because it uses the FR yield-class soil already in the bundle.

Limits to tell the owner:
- RW SP only works when PD ± `PD_window` falls in planting weeks 6–14 or 37–44. Other dates get the existing `SP_DATES_OUT_OF_RANGE` 400 ("Scheduled planting cannot be computed for PD=… and HD=…"). Full-year RW would need new LINTUL runs.
- BI SP covers all 53 weekly planting dates, so no window limit like RW's.
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
   - **Sync state (v6, `94bdee0`; v7 adds `full_synced_at` in `393bee3`):** each row's `updated_at` is the local sync time in ISO format, which `prices_are_stale()` parses. Per country, `country_price_settings` holds:
     - `api_cursor`: the newest price-service `updated_at` seen, in the API's own format (`2026-11-08 12:33:42`), sent back verbatim as `updated_since`;
     - `refreshed_at`: the last successful refresh, which staleness uses even when nothing changed;
     - `full_synced_at`: the last complete (uncursored) fetch. A refresh is full when there is no cursor, `refresh_prices.R --full` is passed, or this is older than `PRICE_FULL_SYNC_HOURS` (default 24). Only a complete fetch deletes API-sourced rows the service no longer lists (see the limitation below).
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

**Deactivations (`393bee3`, schema v7, not yet merged):** an incremental fetch only returns rows that changed, so it cannot see a price that disappeared. The first fetch (no cursor), `refresh_prices.R --full`, and any refresh whose last full fetch is older than `PRICE_FULL_SYNC_HOURS` are full fetches; after a complete one, the engine removes API-sourced rows the service no longer lists. Guards: an empty answer or a failed page removes nothing, an invalid row is skipped rather than removed, and seed rows and other countries are never touched. `--dry-run` reports what would go. Before this, the workaround was to clear `api_cursor` for the country.

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
