# API Reference

The server listens on port **8000** by default.

---

## Endpoints

### `GET /health`

Returns the service status, current version, and UTC timestamp. No request body required.

**Response**
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

```
POST /compute
Content-Type: application/json
```

---

## Request body

All fields are JSON. Required fields are marked **R**; optional fields show their default value.

### Identity and location

| Field | Type | Req | Description |
|-------|------|-----|-------------|
| `country` | string | **R** | Country code: `NG`, `TZ`, `RW`, `GH`, `BI` |
| `lat` | number | **R** | Latitude in decimal degrees (−90 to 90) |
| `lon` | number | **R** | Longitude in decimal degrees (−180 to 180) |
| `lang` | string | | Response language: `en` (default) or `sw` (Swahili) |

### Farm area

| Field | Type | Req | Description |
|-------|------|-----|-------------|
| `area` | number | **R** | Farm size (positive number) |
| `areaUnits` | string | **R** | Unit of `area`: `ha`, `acre`, `ekari`, `are`, `m2`, `string`, `hekta` |

### Recommendation flags

At least one flag must be `true`.

| Field | Type | Default | Description |
|-------|------|---------|-------------|
| `FR` | boolean | `false` | Fertilizer Recommendation |
| `IC` | boolean | `false` | Intercropping (NG: cassava–maize; TZ: cassava–sweet potato) |
| `PP` | boolean | `false` | Post-Planting (tillage and ridging advice) |
| `SPP` | boolean | `false` | Schedule Planting — optimise planting date |
| `SPH` | boolean | `false` | Schedule Planting — optimise harvest date |

### Crop and yield

| Field | Type | Default | Description |
|-------|------|---------|-------------|
| `FCY` | number | | Farmer's current cassava yield (t/ha fresh weight, 1–100) |
| `cassPD` | string | `"roots"` | Cassava product type sold: `roots`, `chips`, `flour`, `gari` |
| `cassUP` | number | country default | Cassava unit price (local currency per `cassUW` kg) |
| `cassUW` | number | `1000` | Weight basis for `cassUP` (kg) |
| `CMP` | integer | | Cassava management practice class (1–5; used for IC) |

### Planting and harvest dates

| Field | Type | Default | Description |
|-------|------|---------|-------------|
| `PD` | string | | Planting date (`YYYY-MM-DD`) |
| `HD` | string | | Harvest date (`YYYY-MM-DD`) |
| `PD_window` | integer | `0` | Weeks around `PD` to search (SP only; 0 = no SP advice) |
| `HD_window` | integer | `0` | Weeks around `HD` to search (SP only; 0 = no SP advice) |

### Cassava price window (SP only)

Prices at ±2 and ±1 months from the intended harvest date — used to fit a price curve.

| Field | Type | Description |
|-------|------|-------------|
| `cassUP_m2` | number | Price 2 months before harvest |
| `cassUP_m1` | number | Price 1 month before harvest |
| `cassUP_p1` | number | Price 1 month after harvest |
| `cassUP_p2` | number | Price 2 months after harvest |

### Starch factory sale (SP only)

| Field | Type | Default | Description |
|-------|------|---------|-------------|
| `saleSF` | boolean | `false` | `true` if selling to a starch factory |
| `nameSF` | string | | Factory name (required when `saleSF` is `true`) |

### Fertilizer entries

Repeat the pattern below for each available fertilizer type (replace `*` with the type name, e.g. `urea`, `NPK`, `MOP`):

| Field | Type | Description |
|-------|------|-------------|
| `*available` | boolean | `true` if this fertilizer is available to the farmer |
| `*CostperBag` | number | Price per bag in local currency |
| `*BagWt` | number | Bag weight in kg |

### Tillage and ridging (PP / SP)

| Field | Type | Default | Description |
|-------|------|---------|-------------|
| `ploughing` | boolean | `false` | Farmer currently ploughs |
| `ridging` | boolean | `false` | Farmer currently ridges |
| `method_ploughing` | string | | `manual`, `tractor`, or `NA` |
| `method_ridging` | string | | `manual`, `tractor`, or `NA` |
| `riskAtt` | integer | `0` | Risk attitude: `0` = risk-averse, `1` = neutral, `2` = risk-tolerant |
| `maxInv` | number | | Maximum investment the farmer is willing to make (local currency, total field) |

### Land management operation costs (PP / SP)

All costs are optional; country defaults are used for any missing value.

| Field | Type | Description |
|-------|------|-------------|
| `cost_manual_ploughing` | number | Manual ploughing cost per area unit |
| `cost_tractor_ploughing` | number | Tractor ploughing cost per area unit |
| `cost_manual_ridging` | number | Manual ridging cost per area unit |
| `cost_tractor_ridging` | number | Tractor ridging cost per area unit |
| `cost_weeding1` | number | First weeding cost per area unit |
| `cost_weeding2` | number | Second weeding cost per area unit |
| `cost_LMO_areaBasis` | string | Unit for the costs above: `areaUnit` (default — uses `areaUnits`), `ha`, `acre`, `m2` |

### Intercropping — Nigeria (cassava–maize)

| Field | Type | Default | Description |
|-------|------|---------|-------------|
| `maizePD` | string | `fresh_cob` | Maize product type: `fresh_cob` or `grain` |
| `maizeUP` | number | | Maize unit price (local currency per `maizeUW` kg or per cob) |
| `maizeUW` | number | | Weight basis for `maizeUP` (kg; ignored for `fresh_cob`) |

### Intercropping — Tanzania (cassava–sweet potato)

| Field | Type | Default | Description |
|-------|------|---------|-------------|
| `sweetPotatoPD` | string | `tubers` | Sweet potato product type: `tubers` or `flour` |
| `sweetPotatoUP` | number | country default | Sweet potato unit price (local currency per `sweetPotatoUW` kg) |
| `sweetPotatoUW` | number | `1000` | Weight basis for `sweetPotatoUP` (kg) |

### User and notifications

| Field | Type | Default | Description |
|-------|------|---------|-------------|
| `userName` | string | | User's name (appears in generated report) |
| `userEmail` | string | | Email address |
| `userPhoneCC` | string | | Phone country code (e.g. `+255`) |
| `userPhoneNr` | string | | Phone number |
| `userField` | string | | User's field/plot name (appears in report) |
| `email` | boolean | `false` | Send the HTML report by email |
| `SMS` | boolean | `false` | Send a brief recommendation by SMS |

---

## Response body

### Success

```json
{
  "status": "success",
  "version": "20251222",
  "rec_type": "FR",
  "recommendation": "We recommend applying\n50 kg of Urea ...",
  "data": { ... },
  "fertilizer_rates": [ { "type": "Urea", "rate": 50 } ]
}
```

| Field | Type | Description |
|-------|------|-------------|
| `status` | string | `"success"` |
| `version` | string | Engine version date (`YYYYMMDD`) |
| `rec_type` | string | Which processor responded: `FR`, `IC`, `PP`, or `SP` |
| `recommendation` | string | Human-readable recommendation text (language controlled by `lang`) |
| `data` | object | Processor-specific numeric output (yield, cost, revenue figures) |
| `fertilizer_rates` | array | `[{ "type": "...", "rate": <kg> }]` — present for FR and IC only |

### Error (400)

Returned when the request is malformed or a required field is missing:

```json
{
  "status": "400 - bad request",
  "data": { "message": "Missing required field: country" }
}
```

---

## Example requests

### Fertilizer Recommendation — Nigeria, English

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
    "FCY": 11,
    "PD": "2024-04-01",
    "HD": "2025-01-01",
    "maxInv": 50000,
    "cassUP": 12000,
    "cassUW": 1000,
    "cassPD": "roots",
    "riskAtt": 1,
    "ureaaAvailable": true,
    "ureaCostperBag": 22000,
    "ureaBagWt": 50,
    "NPKavailable": true,
    "NPKCostperBag": 18000,
    "NPKBagWt": 50
  }'
```

### Same request — Swahili output

Add `"lang": "sw"` to any request body to receive the `recommendation` field in Swahili:

```json
{ "lang": "sw", "country": "TZ", ... }
```

### Schedule Planting — Tanzania

```bash
curl -s -X POST http://localhost:8000/compute \
  -H "Content-Type: application/json" \
  -d '{
    "country": "TZ",
    "lang": "sw",
    "lat": -6.17,
    "lon": 35.74,
    "area": 2,
    "areaUnits": "acre",
    "SPP": true,
    "SPH": true,
    "PD": "2024-03-01",
    "HD": "2025-01-01",
    "PD_window": 1,
    "HD_window": 1,
    "FCY": 9,
    "cassUP": 180000,
    "cassUW": 1000,
    "cassPD": "roots",
    "cassUP_m2": 160000,
    "cassUP_m1": 170000,
    "cassUP_p1": 190000,
    "cassUP_p2": 200000
  }'
```

---

## Notes

- Only **one** recommendation type is returned per request (whichever flag is `true`; if multiple are set, the first in FR → IC → PP → SP order wins).
- The `data` field shape varies by `rec_type`; treat it as supplementary numeric detail rather than a stable contract.
- Response text is normalised: multiple consecutive spaces are collapsed to one.
