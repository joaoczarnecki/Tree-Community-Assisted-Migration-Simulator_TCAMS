# TCAMS — Tree Community-Assisted Migration Simulator

Interactive R Shiny application that lets users explore, for 13 dominant tree
species of Quebec (Canada), how predicted occurrence probability and
community-level suitability (Community Suitability Index, CSI) are expected
to change from the current climate baseline (1991–2020) through three future
horizons (2011–2040, 2041–2070, 2071–2100) under four Shared Socioeconomic
Pathways (SSP1‑2.6, SSP2‑4.5, SSP3‑7.0, SSP5‑8.5).

Predictions come from a Bayesian Joint Species Distribution Model (JSDM)
fitted with the [Hmsc](https://github.com/hmsc-r/HMSC) R package. The
modelling/analysis pipeline that produced the data bundled here lives in the
companion repository **PEP_QC_HMSC_Analysis** (sibling folder in this
workspace) — see that repo's README for how the model was fitted and how the
files below were generated.

This folder is a **safeguarded, self-contained snapshot** of the app as it
stood when this reorganization was done, with a handful of bug fixes and a
new "Trajectories" tab (see below). The previous, unversioned working copies
remain untouched at `PEP_QC/script/app.R` and
`PEP_QC/results/undisturbed/CAMS_Shiny_App_PA_Advanced/`.

## Running locally

Single-folder deployment: `app.R` and every data file it needs
(`*.RData`, `*_preds.Rda`, `posterior_draws/*_preds_subsampled.Rda`,
`data/trajectories/*.csv`) must stay together in this folder.

```r
# install once
install.packages(c("shiny","shinythemes","tidyverse","leaflet","DT","sf"))

# from this folder
shiny::runApp(".")
```

## Deploying to shinyapps.io

```r
rsconnect::deployApp(appDir = ".", appName = "TCAMS_TreeCommunity-AssistedMigration_Simulator")
```

The app was previously published under this exact name (see
`SETUP_GIT.md` for the GitHub remote). Re-deploying from this folder will
overwrite that version — confirm before running if you want to keep the
previously published one as a reference.

## Contents

| Path | What it is |
|---|---|
| `app.R` | The Shiny application (UI + server) |
| `all_predictions_summary.RData` | Posterior mean / 2.5% / 97.5% predicted occurrence probability per plot × species, for all 13 scenarios |
| `Current_preds.Rda`, `8GCMs_ensemble_*.gcm_preds.Rda` | Per-scenario prediction summaries used to build the map layers |
| `species_map.RData` | Species lookup table: 3-letter inventory `code`, `scientific_name`, `original_name_in_data` (matches data columns, e.g. `"ERR_n"`), `pretty_name`, and the app-built `display_name` (see "Fixes" below) |
| `type_eco_data.RData` | `TYPE_ECO_list` (community code → member species) and `type_eco_descriptions` |
| `plot_coordinates.RData` | Plot longitude/latitude for the maps |
| `XData_future_list.RData` | Environmental covariates per scenario (used for the Climate Maps tab) |
| `posterior_draws/*_preds_subsampled.Rda` | Subsampled posterior draws (one list of per-draw prediction matrices per scenario), used when "Use posterior draws" is checked |
| `data/trajectories/*.csv` | Pre-aggregated long-format summaries (posterior mean + 95% quantiles) across **all 13 scenarios**, used by the new **Trajectories** tab: species-level raw probability (`species_prob_by_scenario_draws_summary_combined.csv`) and community-level gCSI/bCSI/wCSI (`gCSI_trajectory_summary_draws.csv`) |
| `001_JSDM_PEP_QC_Modeling_20250814.R`, `002_JSDM_PEP_TCAMS_predictions.r` | Bundled copies (for provenance) of the fitting/prediction scripts that produced the data above. The canonical, up-to-date versions live in `PEP_QC_HMSC_Analysis/R/`. |

## Fixes applied during this reorganization

1. **Species names disambiguated.** The UI previously showed only the
   3-letter genus-based `pretty_name` (e.g. `Pic.glau`, `Pic.mari`) in every
   species dropdown, weight input, and plot axis. Although no two of the 13
   species actually collide under this scheme, abbreviated codes for
   same-genus species (*Picea glauca*/*Picea mariana*, *Pinus strobus*/*Pinus
   banksiana*, *Acer saccharum*/*Acer rubrum*, *Betula alleghaniensis*/*Betula
   papyrifera*) are easy to misread at a glance. All dynamic, user-facing
   labels now use a new `species_map$display_name` (the full scientific
   binomial, falling back to `pretty_name`/`code` only if missing) instead of
   `pretty_name`. The static "How to Use" reference table already listed full
   scientific names and was left as-is.
2. **Credible interval default changed from 89% to 95%.** `hdi_prob` was
   hardcoded to `0.89` (a common default in Bayesian workflows, but
   inconsistent with the 95% credible intervals reported throughout the
   manuscript). It is now `0.95`, so the "HDI …%:" labels shown on hover and
   the highlighted interval match the article's reporting convention. Note
   this is still a genuine *highest-density interval*, not the equal-tailed
   2.5%/97.5% quantile interval also computed and stored (`q025`/`q975`) —
   the two can differ slightly for skewed posteriors; both are available in
   the downloadable CSV.
3. **Posterior-draws toggle was silently inert — now fixed.** The app looked
   for `*_preds_subsampled.Rda` files directly beside `app.R`
   (`draws_dir <- path`), but the actual files were nested one level down in
   `posterior_draws/`. Because `list.files()` is not recursive, `draw_files`
   was always empty, so checking "Use posterior draws (slower)" silently fell
   back to the summary-based approximation for every scenario, with no
   warning to the user. `draws_dir` now points at `./posterior_draws`
   (falling back to `.` for older, flat bundles).
4. **Known limitation kept, now documented instead of silent:** this bundle
   has subsampled draws for the 12 future scenarios but **not** for the
   `Current` baseline (`Current_preds_subsampled.Rda` is missing here — a
   ~215 MB version exists in the source workspace at
   `PEP_QC/posterior_draws_200/`, but that is too large for a plain GitHub
   repository without Git LFS). Checking "Use posterior draws" for the
   `Current` scenario will therefore still fall back to the summary
   approximation. If exact baseline uncertainty via draws is needed, either
   set up Git LFS for this repo, or shrink `Current_preds.Rda` with
   `PEP_QC_HMSC_Analysis/R/004_prepare_app_draws_subsample.R` to a size
   consistent with the other 12 files (~15–17 MB) before adding it here.

## New: "Trajectories" tab

Added between "Climate Maps" and "How to Use". Lets you pick either:

- a **potential vegetation community** (one of the 10 predefined types, e.g.
  `FE3` — Sugar Maple–Yellow Birch Forest), or
- a **custom set of individual species**,

and shows a stacked-area chart, faceted by SSP, of predicted occurrence
probability from the `Current` baseline through the three future horizons.
A checkbox toggles between:

- **absolute** stacking (area height reflects the combined predicted
  probability across the selected species — useful to see whether overall
  suitability is growing or shrinking), and
- **100% stacked composition** (`geom_area(position = "fill")` — always full
  height, showing only each species' *relative* share, i.e. how the
  community's composition is expected to shift).

When a predefined community is selected, a second panel plots its gCSI
trajectory (posterior mean line + 95% credible ribbon) across the same
horizons/SSPs, reusing `data/trajectories/gCSI_trajectory_summary_draws.csv`
— the same draws-based pipeline documented in
`PEP_QC_HMSC_Analysis/R/003_compute_csi_from_draws.R`.

## Data provenance / assumptions made during this reorganization

- **Canonical fitted model**: the deeper file inventory taken during this
  review points to `hmsc_presence_model22oct25.Rda`
  (`PEP_QC/results/undisturbed/hmsc/`, 22 Oct 2025) as the most recent full
  model object, superseding the untimestamped `hmsc_presence_model.Rdata` and
  three other intermediate MCMC-length variants in the same folder.
  **This is an assumption, not a confirmed fact** — please verify it matches
  the model whose AUC/Tjur R² are reported in Table 2 of the manuscript
  before treating any of these repos as final.
- **Canonical CSI pipeline**: `003_gCSI_trajectory_analysis_draws.R` (→
  `PEP_QC_HMSC_Analysis/R/003_compute_csi_from_draws.R`), which recomputes
  bCSI/wCSI/gCSI from the full posterior draws array, was used as the source
  of the two CSV files bundled here — not the older
  `003_gCSI_trajectory_analysis.R`, which applies the geometric mean to
  already-aggregated summary statistics and is not mathematically equivalent.
- Large binary artefacts (fitted model objects, full-resolution posterior
  prediction arrays, `PEP.gpkg`) were **not** copied into this repository —
  they remain in the original workspace (`PEP_QC/results/...`,
  `PEP_QC/databases/...`) and are far too large for a normal git remote.
  Only the lightweight, already-subsampled/aggregated artefacts the app
  actually needs at runtime are included here.

## Setup

See `SETUP_GIT.md` for exact commands to turn this folder into a git
repository and push it to
<https://github.com/joaoczarnecki/Tree-Community-Assisted-Migration-Simulator_TCAMS.git>
(git is not installed on the machine this reorganization was performed on —
those steps still need to be run manually).
