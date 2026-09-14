# TCAMS — Tree Community-Assisted Migration Simulator

This repository contains the Shiny app and all the necessary files to run and visualise the TCAMS app associated with the article in preparation, titled 'Beyond Moving a Single Species: Using Community Suitability to Inform Climate Adaptation in the Boreal-Temperate Ecotone".

João Paulo C. Liz¹, Nelson Thiffault², David Voyer¹, Loïc D’Orangeville¹, Nicholas C. Coops³ and Alexis Achim¹
¹Faculty of Forestry, Geography and Geomatics, Laval University, Quebec, Canada
²Canadian Forest Service, Natural Resources Canada, Quebec, Canada
³Department of Forest Resources Management, University of British Columbia, Vancouver, Canada


* **Main Application:** `app.R` (UI and Server).
* **Data & Predictions:** `.RData`, `.Rda`, and summaries in `data/trajectories/`.
* **Posterior Draws:** Support files located in `posterior_draws/`.
* **Modeling Scripts:** JSDM model support copies (`001_*.R` and `002_*.R`).

To run locally, install the required packages (`shiny`, `tidyverse`, `leaflet`, `DT`, `sf`, `shinythemes`) and execute `shiny::runApp(".")` in the R console.

Access the live web **shinyapp directly** at: https://zwlwgy-jo0o0paulo0czarnecki0de0liz.shinyapps.io/TCAMS_TreeCommunity-AssistedMigration_Simulator/
