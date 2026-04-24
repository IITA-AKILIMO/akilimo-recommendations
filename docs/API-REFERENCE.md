# Akilimo Recommendations API — Developer Reference

Base URL: `http://<host>:8000` (default port 8000; set via `API_PORT` env var)

---

## Table of Contents

1. [Endpoints](#endpoints)
2. [Request Body](#request-body)
3. [Fertilizer Catalogue](#fertilizer-catalogue)
4. [Response Body](#response-body)
5. [Error Responses](#error-responses)
6. [Country Coverage](#country-coverage)
7. [Example Requests](#example-requests)
8. [Integration Notes](#integration-notes)
9. [Translation Data Feed](#translation-data-feed)

---

## Endpoints

### `GET /health`

Liveness probe — no request body.

**Response `200`**
```json
{
  "status": "ok",
  "version": "20251228",
  "time": "2025-12-28T10:00:00Z"
}
```

```bash
curl http://localhost:8000/health
```

---

### `POST /compute`

Compute one cassava farming recommendation.

```
POST /compute
Content-Type: application/json
```

Returns `200` on success, `400` on a bad request, `500` on an unexpected server error.

---

## Request Body

All fields are JSON. **R** = required, **O** = optional (default shown).

### Identity and location

| Field | Type | | Default | Notes |
|-------|------|-|---------|-------|
| `country` | string | **R** | — | `NG` `TZ` `RW` `GH` `BI` — case-insensitive |
| `lat` | number | **R** | — | Decimal degrees, −90 to 90 |
| `lon` | number | **R** | — | Decimal degrees, −180 to 180 |
| `lang` | string | O | `"en"` | Response language: `en` or `sw` (Swahili) |

### Farm area

| Field | Type | | Default | Notes |
|-------|------|-|---------|-------|
| `area` | number | **R** | — | Positive number in the units below |
| `areaUnits` | string | **R** | — | `ha` `acre` `ekari` `hekta` `are` `m2` `string` |

### Recommendation flags

At least one flag must be `true`. Only the first active flag (FR → IC → PP → SP) is processed per request.

| Field | Type | Default | Description |
|-------|------|---------|-------------|
| `FR` | boolean | `false` | Fertilizer recommendation |
| `IC` | boolean | `false` | Intercropping (NG: cassava–maize; TZ: cassava–sweet potato) |
| `PP` | boolean | `false` | Post-planting tillage and ridging advice |
| `SPP` | boolean | `false` | Scheduled planting — optimise planting date |
| `SPH` | boolean | `false` | Scheduled planting — optimise harvest date |

> **Note:** `SPP` and `SPH` are processed together as a single SP request. Set one or both to `true`.

### Crop and yield

| Field | Type | | Default | Notes |
|-------|------|-|---------|-------|
| `FCY` | number | O | — | Farmer's current cassava yield, t/ha fresh weight (0–100) |
| `cassPD` | string | O | `"roots"` | Cassava product type: `roots` `chips` `flour` `gari` |
| `cassUP` | number | O | Country default | Cassava unit price (local currency per `cassUW` kg). Send `0` to use country defaults. |
| `cassUW` | number | O | `1000` | Weight basis for `cassUP` in kg |
| `CMP` | integer | O | — | Cassava management practice class (1–5); required for IC |
| `maxInv` | number | O | unlimited | Maximum investment the farmer will make, local currency, for the whole field. Send `0` for no cap. |
| `riskAtt` | integer | O | `0` | Risk attitude: `0` risk-averse, `1` neutral, `2` risk-tolerant |

### Planting and harvest dates

| Field | Type | | Default | Notes |
|-------|------|-|---------|-------|
| `PD` | string | O | — | Planting date `YYYY-MM-DD` |
| `HD` | string | O | — | Harvest date `YYYY-MM-DD` |
| `PD_window` | integer | O | `0` | Weeks around `PD` to search (SP only). `0` returns no SP advice. |
| `HD_window` | integer | O | `0` | Weeks around `HD` to search (SP only). `0` returns no SP advice. |

### Cassava price window (SP only)

Price at ±2 and ±1 months around the intended harvest date. Used to fit a seasonal price curve. Send `0` to use the same price as `cassUP` at all points.

| Field | Description |
|-------|-------------|
| `cassUP_m2` | Price 2 months before harvest |
| `cassUP_m1` | Price 1 month before harvest |
| `cassUP_p1` | Price 1 month after harvest |
| `cassUP_p2` | Price 2 months after harvest |

### Starch factory sale (SP, FR)

| Field | Type | | Default | Notes |
|-------|------|-|---------|-------|
| `saleSF` | boolean | O | `false` | `true` when selling to a starch factory |
| `nameSF` | string | O | — | Factory identifier; required when `saleSF` is `true` |

### Intercropping — Nigeria (cassava–maize)

Only read when `IC: true` and `country: "NG"`.

| Field | Type | | Default | Notes |
|-------|------|-|---------|-------|
| `maizePD` | string | O | `"fresh_cob"` | Maize product sold: `fresh_cob` or `grain` |
| `maizeUP` | number | O | Country default | Maize unit price (local currency per cob, or per `maizeUW` kg when `grain`) |
| `maizeUW` | number | O | — | Weight basis for `maizeUP` in kg. Required (> 0) when `maizePD` is `"grain"`. |

### Intercropping — Tanzania (cassava–sweet potato)

Only read when `IC: true` and `country: "TZ"`.

| Field | Type | | Default | Notes |
|-------|------|-|---------|-------|
| `sweetPotatoPD` | string | O | `"tubers"` | Sweet potato product sold: `tubers` or `flour` |
| `sweetPotatoUP` | number | O | Country default | Unit price (local currency per `sweetPotatoUW` kg) |
| `sweetPotatoUW` | number | O | `1000` | Weight basis in kg |

### Tillage and ridging (PP and SP)

| Field | Type | | Default | Notes |
|-------|------|-|---------|-------|
| `ploughing` | boolean | O | `false` | Farmer currently ploughs |
| `ridging` | boolean | O | `false` | Farmer currently ridges |
| `method_ploughing` | string | O | `"NA"` | `manual` `tractor` `NA` |
| `method_ridging` | string | O | `"NA"` | `manual` `tractor` `NA` |

### Land management operation costs (PP and SP)

All costs are optional; country defaults are applied when a value is absent or `0`.

| Field | Description |
|-------|-------------|
| `cost_LMO_areaBasis` | Unit the costs below are expressed in: `areaUnit` (uses `areaUnits`), `ha`, `acre`, `m2`. Default: `areaUnit`. |
| `cost_manual_ploughing` | Manual ploughing cost |
| `cost_manual_harrowing` | Manual harrowing cost |
| `cost_manual_ridging` | Manual ridging cost |
| `cost_tractor_ploughing` | Tractor ploughing cost |
| `cost_tractor_harrowing` | Tractor harrowing cost |
| `cost_tractor_ridging` | Tractor ridging cost |
| `cost_weeding1` | First weeding cost |
| `cost_weeding2` | Second weeding cost |

### User and notifications

| Field | Type | | Default | Notes |
|-------|------|-|---------|-------|
| `userName` | string | O | — | Appears in generated PDF report |
| `userEmail` | string | O | — | Recipient for email delivery |
| `userField` | string | O | — | Farm/plot name shown in report |
| `email` | boolean | O | `false` | Send PDF report by email (requires `userEmail`) |

---

## Fertilizer Catalogue

For each fertilizer type, send three fields: `{type}available` (boolean), `{type}CostperBag` (number, local currency), `{type}BagWt` (number, kg).

Set `{type}CostperBag` to `0` to use the country default price. Set `{type}available` to `false` (or omit) to exclude the type.

### Standard types

| Type key | Description |
|----------|-------------|
| `urea` | Urea (46-0-0) |
| `MOP` | Muriate of Potash (0-0-60) |
| `DAP` | Di-Ammonium Phosphate (18-46-0) |
| `TSP` | Triple Super Phosphate (0-46-0) |
| `SSP` | Single Super Phosphate (0-18-0) |
| `CAN` | Calcium Ammonium Nitrate (27-0-0) |
| `NPK201010` | NPK 20-10-10 |
| `NPK151515` | NPK 15-15-15 |
| `NPK171717` | NPK 17-17-17 |
| `NPK201226` | NPK 20-12-26 |
| `NPK201216` | NPK 20-12-16 |
| `NPK112221` | NPK 11-22-21 |
| `NPK251010` | NPK 25-10-10 |
| `NPK152020` | NPK 15-20-20 |
| `NPK23105` | NPK 23-10-5 |
| `NPK123017` | NPK 12-30-17 |
| `DOLOMITEA` | Dolomite (lime; Ca/Mg) |
| `YaraMila_UNIK` | YaraMila UNIK blended NPK |

### Rwanda / Burundi FOMI blends

| Type key | Description |
|----------|-------------|
| `FOMIBAGARA` | FOMI Bagara blend |
| `FOMIIMBURA` | FOMI Imbura blend |
| `FOMITOTAHAZA` | FOMI Totahaza blend |

### Custom fertilizers

Up to 5 custom fertilizer types can be supplied. Use the pattern below for each (replace `N` with 1–5):

| Field | Type | Description |
|-------|------|-------------|
| `newFertNname` | string | Fertilizer name / identifier |
| `newFertNN_cont` | number | N content (fraction, 0–1) |
| `newFertNP2O5` | number | P₂O₅ content (fraction, 0–1) |
| `newFertNK2O` | number | K₂O content (fraction, 0–1) |
| `newFertNCostperBag` | number | Price per bag (local currency) |
| `newFertNBagWt` | number | Bag weight (kg) |

---

## Response Body

All responses share the same envelope:

```json
{
  "status":  "success | 400 - bad request | error",
  "version": "20251228",
  "data":    { ... }
}
```

### Success (`200`)

```json
{
  "status":  "success",
  "version": "20251228",
  "data": {
    "rec_type":       "FR",
    "recommendation": "We recommend applying\n50 kg of Urea per hectare...",
    "data":           { ... },
    "fertilizer_rates": [ { "type": "Urea", "rate": 50 } ]
  }
}
```

| Field | Type | Description |
|-------|------|-------------|
| `status` | string | `"success"` |
| `version` | string | Engine version (`YYYYMMDD`) |
| `data.rec_type` | string | Processor that responded: `FR` `IC` `PP` `SP` |
| `data.recommendation` | string | Human-readable advice (language from `lang`). Newlines are `\n`. |
| `data.data` | object | Processor-specific numeric output — see per-type tables below |
| `data.fertilizer_rates` | array | `[{ "type": "...", "rate": <kg total for field> }]` — FR and IC only |

#### `data.data` — FR and IC

| Field | Unit | Description |
|-------|------|-------------|
| `lat`, `lon` | degrees | Echoed coordinates |
| `plDate` | date | Planting date used |
| `N`, `P`, `K` | kg | Recommended N / P / K for the whole field |
| `WLY` | t FW | Water-limited yield for the field |
| `CurrentY` | t FW | Estimated current yield for the field |
| `TargetY` | t FW | Expected yield at the recommended NPK rate |
| `TC` | local currency | Total fertilizer cost (whole field) |
| `NR` | local currency | Estimated net revenue gain (whole field) |

#### `data.data` — PP

Each row represents one land-management scenario. The first row is the recommendation.

| Field | Type | Description |
|-------|------|-------------|
| `ploughing` | boolean | Whether ploughing is included |
| `ridging` | boolean | Whether ridging is included |
| `method_ploughing` | string | `manual` `tractor` `N/A` |
| `method_ridging` | string | `manual` `tractor` `N/A` |
| `TC` | number | Total cost (local currency, whole field) |
| `NR` | number | Net revenue (local currency, whole field) |
| `dNR` | number | Net revenue change vs current practice |
| `CP` | boolean | `true` on the row that matches the farmer's current practice |

#### `data.data` — SP

Each row is a planting × harvest date combination. The first row is the recommendation.

| Field | Type | Description |
|-------|------|-------------|
| `PD` | date | Candidate planting date |
| `HD` | date | Candidate harvest date |
| `rPWnr` | integer | Weeks from requested `PD` (negative = earlier) |
| `rHWnr` | integer | Weeks from requested `HD` (negative = earlier) |
| `GR` | number | Gross revenue (local currency, whole field) |
| `dGR` | number | Gross revenue change vs current practice |
| `CP` | boolean | `true` on the row matching the farmer's current dates |
| `rootUP` | number | Cassava price applied for this date |

---

## Error Responses

### `400 Bad Request`

Returned for validation failures or malformed input.

```json
{
  "status":  "400 - bad request",
  "version": "20251228",
  "data": {
    "message": "Missing required field: country"
  }
}
```

Common causes:

| Message | Cause |
|---------|-------|
| `Missing required field: country` | `country` absent or blank |
| `Invalid country: xx — must be one of: NG, TZ, RW, GH, BI` | Unrecognised country code |
| `Invalid or missing lat — must be numeric between -90 and 90` | `lat` out of range or non-numeric |
| `Invalid or missing area — must be a positive number` | `area` ≤ 0 or absent |
| `At least one recommendation flag must be TRUE` | All of FR/IC/PP/SPP/SPH are false |
| `PD must be a valid date in YYYY-MM-DD format` | Bad date string |
| `maizeUW must be a positive number when maizePD is 'grain'` | Missing weight for grain maize price |
| `Intercropping (IC) not yet available for country: RW` | IC only supported for NG and TZ |

### `500 Internal Server Error`

Returned for unexpected failures. Includes the `request_token` field (echoed from the request) for log correlation.

```json
{
  "status": "error",
  "data": {
    "request_token": "abc123",
    "message": "object 'x' not found"
  }
}
```

---

## Country Coverage

| Country | FR | IC | PP | SP |
|---------|----|----|----|----|
| Nigeria (NG) | ✓ | ✓ cassava–maize | ✓ | ✓ |
| Tanzania (TZ) | ✓ | ✓ cassava–sweet potato | ✓ | ✓ |
| Rwanda (RW) | ✓ | — | ✓ | ✓ |
| Ghana (GH) | ✓ | — | ✓ | ✓ |
| Burundi (BI) | ✓ | — | ✓ | ✓ |

### Default cassava prices (when `cassUP` is `0`)

| Country | Roots | Chips | Flour | Gari |
|---------|-------|-------|-------|------|
| NG | 12,000 | 36,000 | 38,400 | 42,000 |
| TZ | 180,000 | 540,000 | 576,000 | 630,000 |
| GH | 450 | 450 | 450 | 450 |
| RW | 75,000 | 75,000 | 75,000 | 75,000 |
| BI | 700,000 | 700,000 | 700,000 | 700,000 |

All prices are per 1,000 kg in local currency.

---

## Example Requests

### Fertilizer Recommendation — Nigeria

```bash
curl -s -X POST http://localhost:8000/compute \
  -H "Content-Type: application/json" \
  -d '{
    "country": "NG",
    "lat": 4.775,
    "lon": 8.415,
    "area": 1,
    "areaUnits": "ha",
    "FR": true,
    "PD": "2024-04-01",
    "HD": "2025-01-01",
    "FCY": 11,
    "cassUP": 12000,
    "cassUW": 1000,
    "cassPD": "roots",
    "maxInv": 50000,
    "riskAtt": 1,
    "ureaavailable": true,
    "ureaCostperBag": 22000,
    "ureaBagWt": 50,
    "NPK201010available": true,
    "NPK201010CostperBag": 18000,
    "NPK201010BagWt": 50,
    "MOPavailable": true,
    "MOPCostperBag": 15000,
    "MOPBagWt": 50
  }'
```

### Fertilizer Recommendation — Tanzania, starch factory sale

```bash
curl -s -X POST http://localhost:8000/compute \
  -H "Content-Type: application/json" \
  -d '{
    "country": "TZ",
    "lat": -9.89,
    "lon": 37.85,
    "area": 1,
    "areaUnits": "acre",
    "FR": true,
    "PD": "2025-05-21",
    "HD": "2026-01-21",
    "FCY": 11,
    "saleSF": true,
    "nameSF": "FJS",
    "riskAtt": 0,
    "MOPavailable": true,
    "MOPCostperBag": 119704,
    "MOPBagWt": 50,
    "SSPavailable": true,
    "SSPCostperBag": 135818,
    "SSPBagWt": 50
  }'
```

### Intercropping — Nigeria (cassava–maize, grain price)

```bash
curl -s -X POST http://localhost:8000/compute \
  -H "Content-Type: application/json" \
  -d '{
    "country": "NG",
    "lat": 9.10,
    "lon": 9.23,
    "area": 1,
    "areaUnits": "ha",
    "IC": true,
    "PD": "2024-08-02",
    "HD": "2025-04-02",
    "FCY": 11,
    "CMP": 4,
    "cassUW": 50,
    "maizePD": "grain",
    "maizeUW": 50,
    "maizeUP": 230,
    "riskAtt": 0,
    "ureaavailable": true,
    "ureaBagWt": 50
  }'
```

### Post-Planting advice — Nigeria

```bash
curl -s -X POST http://localhost:8000/compute \
  -H "Content-Type: application/json" \
  -d '{
    "country": "NG",
    "lat": 4.775,
    "lon": 8.415,
    "area": 1,
    "areaUnits": "ha",
    "PP": true,
    "FCY": 10,
    "cassUP": 12000,
    "cassUW": 1000,
    "cassPD": "roots",
    "ploughing": true,
    "ridging": false,
    "method_ploughing": "manual",
    "method_ridging": "NA",
    "riskAtt": 1
  }'
```

### Scheduled Planting — Ghana (Swahili, 2-month window)

```bash
curl -s -X POST http://localhost:8000/compute \
  -H "Content-Type: application/json" \
  -d '{
    "country": "GH",
    "lang": "en",
    "lat": 6.68,
    "lon": -1.62,
    "area": 1,
    "areaUnits": "acre",
    "SPP": true,
    "SPH": true,
    "PD": "2025-05-29",
    "HD": "2026-01-29",
    "PD_window": 2,
    "HD_window": 2,
    "FCY": 11,
    "cassUP": 450,
    "cassUW": 1000,
    "cassPD": "roots",
    "cassUP_m2": 400,
    "cassUP_m1": 430,
    "cassUP_p1": 460,
    "cassUP_p2": 480,
    "riskAtt": 2
  }'
```

---

## Integration Notes

**Priority order.** Only one recommendation type is returned per request. If multiple flags are `true`, the server processes the first in this order: FR → IC → PP → SP. Send separate requests for multiple recommendation types.

**Currency.** All monetary values (prices, costs, revenues) are in the local currency of the `country` field. No currency conversion is performed.

**Zero prices.** Sending `cassUP: 0`, `maizeUP: 0`, or `sweetPotatoUP: 0` triggers country-specific defaults. Send an explicit positive value to override.

**Fertilizer cost defaults.** Setting `{type}CostperBag: 0` with `{type}available: true` causes the server to use the stored country default price for that type. The request will still succeed.

**SP window.** Both `PD_window` and `HD_window` must be `> 0` for SP advice to be returned. If either is `0`, the response contains an informational message rather than scheduling data.

**PDF delivery.** Set `"email": true` and supply `userEmail` to receive a PDF report by email. PDF generation failures do not suppress the JSON recommendation — `data.recommendation` is always populated.

**Version field.** The `version` string in every response is a date (`YYYYMMDD`) identifying the engine build, not a semantic version. Use it for support correlation only.

**Response `data.data` stability.** The shape of `data.data` varies by `rec_type` and may gain new fields in future releases. Parse defensively; do not hard-code field positions.

---

## Translation Data Feed

The recommendations engine loads user-facing strings from `data/input/translations.csv`. When a remote translation service is configured, the engine fetches translations from a **Laravel paginated API resource** and rebuilds the local CSV from the full result set.

### Expected endpoint

```
GET {AKILIMO_API_URL}/translations?page={n}
Accept: application/json
Authorization: Bearer <token>
```

The engine walks every page until `links.next` is `null`.

### Paginated response envelope

Laravel's resource paginator produces the following envelope. The engine only reads `data` and `links.next` — everything inside `meta` is informational and not consumed.

```json
{
  "data": [ ... ],
  "links": {
    "first": "https://api.example.com/api/translations?page=1",
    "last":  "https://api.example.com/api/translations?page=4",
    "prev":  null,
    "next":  "https://api.example.com/api/translations?page=2"
  },
  "meta": {
    "current_page": 1,
    "from": 1,
    "last_page": 4,
    "links": [
      { "url": null,                                                    "label": "&laquo; Previous", "page": null, "active": false },
      { "url": "https://api.example.com/api/translations?page=1",      "label": "1",               "page": 1,    "active": true  },
      { "url": "https://api.example.com/api/translations?page=2",      "label": "2",               "page": 2,    "active": false },
      { "url": "https://api.example.com/api/translations?page=2",      "label": "Next &raquo;",    "page": 2,    "active": false }
    ],
    "path": "https://api.example.com/api/translations",
    "per_page": 50,
    "to": 50,
    "total": 183
  }
}
```

| Envelope field | Required | Type | Description |
|----------------|----------|------|-------------|
| `data` | **yes** | array | Translation records for this page (see below) |
| `links.next` | **yes** | string\|null | URL of the next page; `null` signals the last page and stops pagination |
| `links.first` | no | string | Absolute URL of the first page |
| `links.last` | no | string | Absolute URL of the last page |
| `links.prev` | no | string\|null | URL of the previous page |
| `meta.*` | no | object | Informational only — not read by the engine |

### Translation record (`data` items)

Each element in the `data` array represents one translatable string:

```json
{
  "id": 42,
  "key": "rec_apply_fertilizer",
  "en": "Apply {rate} kg of {fertilizer} per hectare.",
  "sw": "Tumia {rate} kg ya {fertilizer} kwa hekta.",
  "rw": "Shyira {rate} kg ya {fertilizer} ku hekitari.",
  "context": "FR recommendation body",
  "created_at": "2024-06-01T08:00:00Z",
  "updated_at": "2025-01-15T11:32:00Z"
}
```

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `id` | integer | yes | Surrogate key (ignored after import; used for deduplication) |
| `key` | string | yes | Lookup key used in `tr(key, lang)` calls |
| `en` | string | yes | English text (authoritative fallback) |
| `sw` | string | no | Swahili translation; falls back to `en` if blank or absent |
| `rw` | string | no | Kinyarwanda translation; falls back to `en` if blank or absent |
| `context` | string | no | Human note for translators — not used by the engine |
| `created_at` | string | no | ISO 8601 timestamp |
| `updated_at` | string | no | ISO 8601 timestamp |

### Placeholder syntax

Translation strings may contain `{token}` placeholders. The engine replaces them at render time via the `tr(key, lang, ...)` helper. The remote API must preserve placeholders exactly — do not translate the token names inside `{}`.

```
en: "Apply {rate} kg of {fertilizer} per hectare."
sw: "Tumia {rate} kg ya {fertilizer} kwa hekta."
         ^^^^^^         ^^^^^^^^^^  ← token names unchanged
```

### Sync behaviour

- The engine reads all pages sequentially (page 1 → last page) before writing the local CSV.
- If the remote API is unreachable the engine falls back to the existing `translations.csv` and logs a warning — recommendations are not blocked.
- Duplicate `key` values: the last record wins (matches `translations.csv` row-order semantics).
- Keys present in `translations.csv` but absent from the remote feed are preserved during a partial sync.

---

## Fertilizer Prices Feed

The engine loads fertilizer and input prices from the local SQLite database. When `AKILIMO_API_URL` is set, the engine refreshes prices on startup (if stale) by calling `refresh_prices()`.

### Expected endpoint

```
GET {AKILIMO_API_URL}/prices?country={COUNTRY}&page={n}
Accept: application/json
Authorization: Bearer <token>
```

`COUNTRY` is one of `NG`, `TZ`, `RW`, `GH`, `BI`. The engine calls this endpoint once per country and walks all pages. Only `data` and `links.next` are consumed from each page.

Staleness threshold: `PRICE_MAX_AGE_DAYS` env var (default `7` days).

### Response

```json
{
  "data": [
    {
      "country":  "TZ",
      "item":     "CAN",
      "price":    45000,
      "unit":     "per_bag",
      "currency": "TZS"
    }
  ],
  "links": {
    "first": "https://api.example.com/api/prices?country=TZ&page=1",
    "last":  "https://api.example.com/api/prices?country=TZ&page=2",
    "prev":  null,
    "next":  "https://api.example.com/api/prices?country=TZ&page=2"
  },
  "meta": { "...": "informational only" }
}
```

| Field | Required | Type | Description |
|-------|----------|------|-------------|
| `country` | **yes** | string | Country code — must be one of `NG`, `TZ`, `RW`, `GH`, `BI` |
| `item` | **yes** | string | Fertilizer or input identifier (e.g. `"CAN"`, `"UREA"`) |
| `price` | **yes** | number | Price value — must be > 0 |
| `unit` | **yes** | string | Price unit — one of `per_bag`, `per_kg`, `per_tonne`, `per_acre`, `per_ha` |
| `currency` | no | string | ISO currency code (e.g. `"TZS"`) |

Rows with an invalid `country`, blank `item`, non-positive `price`, or unrecognised `unit` are skipped with a WARN log entry.

---

## Starch Prices Feed

Starch factory gate prices used for Tanzania cassava processing recommendations. Refreshed by `refresh_starch_prices()` when `AKILIMO_API_URL` is set.

### Expected endpoint

```
GET {AKILIMO_API_URL}/starch-prices?country={COUNTRY}&page={n}
Accept: application/json
Authorization: Bearer <token>
```

`COUNTRY` is one of `NG`, `TZ`, `RW`, `GH`, `BI`. The engine walks all pages. On each refresh the entire set for the country is replaced atomically after all pages are collected. Only `data` and `links.next` are consumed from each page.

Staleness threshold: `STARCH_PRICE_MAX_AGE_DAYS` env var (default `30` days).

### Response

```json
{
  "data": [
    {
      "starch_factory":       "Kibaigwa",
      "starch_factory_label": "Kibaigwa Starch Factory",
      "class":                1,
      "country":              "TZ",
      "key":                  "kibaigwa_cls1",
      "min_starch":           20.0,
      "range_starch":         "20–24%",
      "price":                180000,
      "currency":             "TZS"
    }
  ],
  "links": {
    "first": "https://api.example.com/api/starch-prices?country=TZ&page=1",
    "last":  "https://api.example.com/api/starch-prices?country=TZ&page=2",
    "prev":  null,
    "next":  "https://api.example.com/api/starch-prices?country=TZ&page=2"
  },
  "meta": { "...": "informational only" }
}
```

| Field | Required | Type | Description |
|-------|----------|------|-------------|
| `starch_factory` | **yes** | string | Machine-readable factory identifier |
| `key` | **yes** | string | Unique row key — duplicates are skipped |
| `class` | **yes** | integer | Starch class (≥ 1) |
| `country` | **yes** | string | Country code — must be one of `NG`, `TZ`, `RW`, `GH`, `BI` |
| `min_starch` | **yes** | number | Minimum starch content (%) for this class — must be ≥ 0 |
| `price` | **yes** | number | Price per tonne — must be > 0 |
| `starch_factory_label` | no | string | Human-readable factory name |
| `range_starch` | no | string | Display string for the starch range (e.g. `"20–24%"`) |
| `currency` | no | string | ISO currency code (e.g. `"TZS"`) |

Rows with an invalid `country`, blank `starch_factory` or `key`, duplicate `key`, invalid `class`, or non-positive `price`/`min_starch` are skipped with a WARN log entry.

---

## Common API conventions

All three data-feed endpoints share the same auth and env-var conventions.

### Authentication

All requests carry a `Bearer` token when `AKILIMO_API_TOKEN` is set:

```
Authorization: Bearer <AKILIMO_API_TOKEN>
```

If the env var is unset the header is omitted (useful for local development without auth).

### Environment variables

| Variable | Default | Description |
|----------|---------|-------------|
| `AKILIMO_API_URL` | _(none)_ | Versioned base URL for all data-feed endpoints, e.g. `https://api.example.com/api/v1`. No trailing slash. If unset, all refreshes are skipped silently |
| `AKILIMO_API_TOKEN` | _(none)_ | Bearer token sent with every request |
| `PRICE_MAX_AGE_DAYS` | `7` | Days before fertilizer prices are considered stale |
| `STARCH_PRICE_MAX_AGE_DAYS` | `30` | Days before starch prices are considered stale |
| `TRANSLATIONS_MAX_AGE_DAYS` | `30` | Days before translations are considered stale |

### Error handling

All refresh functions return a list with three fields:

| Field | Type | Values |
|-------|------|--------|
| `status` | string | `"ok"` / `"error"` / `"skipped"` |
| `rows_upserted` | integer | Number of rows written to the database |
| `message` | string\|null | Human-readable error detail; `null` on success |

A `"skipped"` status means the env var was unset or data was still fresh — not a failure. Refresh failures never block a recommendation request; the engine uses cached data from the last successful sync.
