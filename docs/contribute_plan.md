# Plan: "Contribute your peat depths": a standardization and archiving add-on for CarbonViewer

> **For the implementing session:** this plan was drafted and agreed with the user on 2026-10-09. Implement **Phase 1 only** on branch `standardize-archive-depths`. The standard files are in `standard/`. The app runs in Docker; see `docs/project_structure.md`. Ask the user before changing any decision listed under "Decisions made".

## Context

People submit peat depth points to CarbonViewer (a zip with a polygon shapefile and a CSV of `x, y, torvdybde_cm`), get a volume and carbon estimate, and then the points are thrown away. `session$onSessionEnded` deletes the per-session folder (`appScripts/server.R:583`). The goal is to offer an **opt-in** step that turns those points into records that follow a standard (Natural England *Data Exchange Standard for Peat Surveys v1.0*, with a Norwegian profile), with a small set of plain questions for metadata and licence. Over time the records can be gathered as training data for national peat mapping.

## Feasibility verdict: yes, this fits well as an add-on

- **The data is already parsed and cleaned in the app.** After upload, `df_reactive$points` is an `sf` point object in EPSG:25833 with `torvdybde_cm`, and `df_reactive$shape` holds the area polygon (`server.R:161-173`). The add-on needs no new parsing.
- **The app's units already match the standard.** `surfacePeatDepth` is in **cm**, so `torvdybde_cm` maps across directly.
- **Shiny handles guided multi-step forms well.** A wizard can be built from `tabsetPanel(type = "hidden")`, with `shinyvalidate` for field checks and `DT` (already loaded) with `editable = TRUE` for per-row overrides.
- **The app is already bilingual** (`shiny.i18n` + `translation.json`).
- **The changes are small and self-contained.** One new module touches the existing app at only three points.
- **The hard parts are organizational:** durable storage, who curates the archive, GDPR (`surveyor` and the contact email are personal data), and NINA's buy-in for an upstream PR.

## Decisions made (2026-10-09)
- **Scope now = Phase 1 only** (standardize + download). Phases 2–3 stay as a roadmap, with a `store_submission()` stub left in place.
- **Upstream PR target:** a clean, optional module that can be switched off (`options(carbonviewer.contribute = TRUE)`), with minimal edits to existing files and NO/EN strings.
- **Entry point:** a modal `Yes / Not now` popup after a successful calculation, shown once per session and never for the test-data run. "Yes" switches to a "Bidra med data / Contribute data" tab, which also stays in the sidebar.
- **Standard:** the NE v1.0 attribute names and structure via the corrected machine-readable profile (below), plus a thin Norwegian profile.

## Standard reference files (in `standard/`)
They were moved from `data/`, which is gitignored and is the app's per-session scratch area (`data/<session token>`), into a tracked top-level `standard/` folder that also holds the source PDF. In the container they are at `/home/rstudio/app/standard/`; the `Dockerfile`'s `COPY .` already includes them. For development with compose, add a `./standard:/home/rstudio/app/standard` volume to `docker-compose.yaml`.

The user transcribed the standard into machine-readable files. **These, not the PDF, are the authoritative input for the implementation.**

| File | Content |
|---|---|
| `peat_standard_v1.0.yaml` | The full standard as a corrected profile (`1.0-corr.1`): 24 peat-data attributes and 21 metadata terms, each with requirement level, inferred type/unit, allowed values, Darwin Core mapping, description and example. It also has machine-readable cross-field `rules` (location groups, conditional requirements, defaults), footnotes and references. |
| `peat_data_attributes.csv` | A flat table of the attributes, with `order` (the recommended column order). |
| `peat_metadata_terms.csv` | A flat table of the metadata terms. |
| `peat_standard_changes.csv` | Corrections C1–C6 against the published v1.0, e.g. C1 (EPSG:27700), C3 (easting/northing with an explicit `verbatimSRS`) and C5 (`verticalDatum` required with elevation). |

The app reads the **YAML** at runtime using the `yaml` package, which ships with rocker/geospatial. The CSVs are human-readable views of it. The Norwegian profile is a small overlay file (`standard/peat_standard_no_profile.yaml`). It holds NO/EN labels and help text, defaults (`country = "Norway"`, `geodeticDatum = "EPSG:4326"`, `verbatimSRS = "EPSG:25833"`), the licence choices shown to users, and the attributes CarbonViewer hides (`gridReference`).

### What the reference files changed in this plan
- **No depth conversion.** Depth is already in cm.
- **`probeReachedBottom` is *required*** (Yes/No, and "No" if unsure). The wizard asks one direct question: *"Did you reach the bottom of the peat at every point?"* There are three answers:
  - **Yes, at all points** → every point gets `Yes`.
  - **No / not sure at some points** → every point gets `Yes`, and the user sets the affected points to `No` with a toggle column in the editable table.
  - **Not sure for any point** → every point gets `No`, as the standard advises.

  If the uploaded CSV already has a `probeReachedBottom` column, its values are used and the question is skipped. There is no question about probe length.
- **`peatPresence` is not needed**, because we always supply `surfacePeatDepth` (the `depth_or_presence` rule). Zero depths are valid.
- **Norway needs fewer deviations than first assumed.** Thanks to C3, `easting`/`northing` in EPSG:25833 are valid if `verbatimSRS` is **always written**: it defaults to EPSG:27700 when it is missing, which would put Norwegian points in the wrong place. Each record gets both lat/lon (EPSG:4326, which has highest priority) and the original easting/northing. `gridReference` is omitted.
- **Required fields that the app cannot work out, and must ask for:**
  - data fields: `recordDate` and `surveyor`. `surveyor` may be an organisation name to avoid personal data, and the GDPR note goes in the consent step.
  - metadata terms: `title`, `organizationName`, `abstract`, `license` and `contactElectronicMailAddress`. `institution` is filled from `organizationName`.
- **The app works out these values itself:**
  - lat/lon, easting/northing and the bounding box (`west/east/north/southBoundingCoordinate`)
  - `temporalCoverageStart/EndDate` (from `recordDate`)
  - `coordinateUncertainty`, preset from the positioning method (phone ≈ 10 m, handheld GNSS ≈ 5 m, RTK ≈ 0.1 m, or a custom value)
  - `peatDepthResolution` (e.g. "Nearest 1cm")
  - a draft `methods` text built from the answers
  - a suggested `bibliographicCitation`
- **Licence:** the NE list `CC0, CC-BY, CC-BY-NC, OGL, NCGL, Other` includes UK-only licences. The Norwegian profile shows **CC0 / CC-BY (recommended) / CC-BY-NC / Other** (with `rights` text). For someone who needs NLOD, the choice is `Other` with `rights = "NLOD 2.0"`, which keeps files valid under NE. OGL and NCGL are hidden.
- **`probeType`:** use the NE vocabulary with NO labels (e.g. "peat probe" → "torvsonde/jordsonde"), plus `other` + `peatDepthRemarks`. No new values for now.
- **Optional extras that stay out of the Phase 1 wizard:** `surfaceElevation`/`verticalDatum` (NN2000 = EPSG:5941), buried peat layers, and `locationRemarks`. They are left blank and still allowed per row in the editable table if a user's CSV already contains them (columns are passed through by name).
- **Validation is driven by the schema.** A small generic validator implements the YAML `rules` types: `at_least_one_group_complete`, `required_if_present`, `default_if_absent`, `at_least_one_present`. It also checks types and enums from `attributes`. The same function produces the wizard's error messages and can later be used to check standalone submissions.

## Recommended approach (phased)

### Phase 1: Standardize and download (no infrastructure needed)
Wizard steps (steps 2 and 3 were swapped after the first test run, see the end of this plan):
1. **Intro and consent:** what happens to the data, that taking part is optional, and a GDPR note.
2. **Review points:** a map and an editable table, with derived fields filled in.
3. **Measurement questions,** asked once for the whole dataset:
   - survey date or date range
   - surveyor
   - probe type
   - whether the bottom of the peat was reached (see above)
   - depth resolution
   - positioning method → coordinate uncertainty
   - remarks
4. **Dataset metadata:** title, organisation, abstract, purpose, contact email, licence, location and geographic description.
5. **Validate and preview:** run the schema validator and show a summary.
6. **Download:** a zip containing `peat_data.csv` (columns in the standard's `order`, RFC 4180 with CRLF via `data.table::fwrite(eol = "\r\n")`), `metadata.csv` (one header row + one value row), and a `README.txt` that cites the standard and profile versions. It reuses the pattern from `output$downloadData` (`server.R:557`).

### Phase 2: Submit to a staging store (later)
A "Submit" button writes the package and a manifest (UUID, timestamp, app and profile version, licence, consent) through `store_submission()`. The backend could be an S3-compatible bucket, PostGIS, or a mounted volume. Submissions go into a curator review queue, and credentials come from deployment secrets.

### Phase 3: Curated publication (outside the app)
A curator merges reviewed submissions into a versioned national dataset with a DOI (Zenodo via `zen4R`, or DataverseNO), keeping each record's licence and attribution. The standard's Darwin Core mappings (`dwc_iri`, `dwc_note`) make a later DwC/GBIF export easy.

## Files to add or modify
- **New** `appScripts/contribute.R`: `mod_contribute_ui()` / `mod_contribute_server(id, points, shape)`, plus the helpers `standardize_points()`, `build_metadata()`, `write_standard_package()`, and the `store_submission()` stub.
- **New** `appScripts/standard.R`: `load_standard()` (YAML + NO overlay), `validate_against_standard()`, and helpers that build form inputs from the schema.
- **New** `standard/peat_standard_no_profile.yaml`: the Norwegian overlay.
- `app.R`: `source()` the new files.
- `docker-compose.yaml`: add the `./standard` volume mount for development.
- `appScripts/ui.R`: a sidebar `menuItem` and a `tabItem`.
- `appScripts/server.R`: call the module, and show the invitation modal after `interpolation()` succeeds (~line 183), skipping it when `values$upload_state == 'test'`.
- `appScripts/dependancies.R` + `Dockerfile`: add `shinyvalidate` and `yaml`.
- `translation.json`: the UI strings (field labels come from the profile overlay).
- `docs/instructions_*.md` and `docs/project_structure.md`: document the feature and the new data files.

## Existing code to reuse
- `df_reactive$points` / `df_reactive$shape` (already validated, EPSG:25833): `server.R:161-173`
- Zip-download pattern: `server.R:557-578`
- Error-modal style: the `print_*` helpers in `global.R`
- i18n setup: `dependancies.R:24-26`

## Verification
- Run `docker compose up`, load a real-format zip (`test/test_dataset_cm.zip` uploaded through the file input, so the invitation appears), and step through the wizard in NO and EN.
- Unit-check `validate_against_standard()` with small data frames: a missing `probeReachedBottom`, lat/lon without `geodeticDatum`, elevation without `verticalDatum`, and a bad enum value.
- Check the output: columns and order match `peat_data_attributes.csv`; required metadata terms from `peat_metadata_terms.csv` are present; `verbatimSRS = EPSG:25833` on every row; coordinates round-trip 25833 ↔ 4326; the CSV parses as RFC 4180.
- Edge cases:
  - zero depths
  - each of the three "reached bottom" answers
  - an uploaded CSV that already contains `probeReachedBottom`
  - a multi-day survey
  - thousands of points

## Feedback from the first test run (2026-10-09)
Notes from stepping through the wizard in Docker, and how each is handled:
1. **Depth sorted as text** in the review table. Number columns are now numbers, and the table opens sorted by depth, deepest first (stated above the table).
2. **Clearer review instructions.** The step says what is required (only marking the points where the bottom was not reached, when that was the answer) and what is optional (dates and remarks per point). Column headings show a description on hover, and the same descriptions are listed under the table.
3. **Select points on the map.** Clicking a point selects or deselects its row. Selected points are blue like selected rows, points where the bottom was not reached are orange, and a legend explains the colours.
4. **Map beside the table** (map on the right). The coordinate columns are left out of the review table to make it narrower; they remain in the preview in the check step.
5. **"Select all" and "Clear selection" buttons**, with a count of selected points. "Select all" respects the table's search box.
6. **Measurement questions before the review** (old steps 2 and 3 swapped), so the table is complete when the user first sees it.
7. **Shorter probe-type list:** torvsonde/jordsonde, skredsøkestang, kabelstang, torvbor (f.eks. russerbor), jordprøvetaker, annet. The other NE values stay valid in uploaded files.
8. **NLOD 2.0 in the licence list.** It writes `license = Other` and `rights = NLOD 2.0`, as decided above, so the files stay valid under the NE standard.
9. **Clickable step names** at the top. Jumping forward runs the same checks as "Neste" for every step it passes.
