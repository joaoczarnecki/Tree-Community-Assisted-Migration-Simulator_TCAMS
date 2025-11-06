# ===============================================================================
# 5. SHINY APP CODE (`app.R`)
# ===============================================================================
# This code should be saved as a separate file named `app.R` inside the `shiny_app_dir` directory.

# --- Start of app.R ---
library(shiny)
library(shinythemes)
library(tidyverse)
library(leaflet)
library(DT)
library(sf)

# --- Load pre-processed data ---
load("all_predictions_summary.RData")
load("type_eco_data.RData")
load("species_map.RData")
load("plot_coordinates.RData")
load("XData_future_list.RData")

# --- Calculate global climate ranges for consistent map legends ---
all_mats <- unlist(lapply(XData_future_list, function(df) df$MAT))
all_maps <- unlist(lapply(XData_future_list, function(df) df$MAP))
global_mat_range <- range(all_mats, na.rm = TRUE)
global_map_range <- range(all_maps, na.rm = TRUE)


# --- Helper Functions ---
calculate_csi <- function(community_species, site_suitability_probs, method = "bCSI", weights = NULL) {
  if (length(community_species) == 0) return(NA)
  valid_species <- intersect(community_species, names(site_suitability_probs))
  if (length(valid_species) == 0) return(NA)
  
  probs <- site_suitability_probs[valid_species]
  
  if (method == "bCSI") {
    return(mean(probs, na.rm = TRUE))
  } else if (method == "wCSI") {
    if (is.null(weights)) weights <- setNames(rep(1, length(valid_species)), valid_species)
    valid_weights <- weights[valid_species]
    return(weighted.mean(probs, valid_weights, na.rm = TRUE))
  } else if (method == "gCSI") {
    return(exp(mean(log(probs + 1e-9), na.rm = TRUE)))
  }
}

# --- UI ---
ui <- fluidPage(
  theme = shinytheme("sandstone"),
  titlePanel("Tree Community-Assisted Migration Simulator (TCAMS)"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Global Settings"),
      selectInput("climate_scenario", "1. Select Climate Scenario:", choices = names(all_predictions_summary)),
  selectInput("basemap", "Base Map:",
      choices = c("CartoDB.Positron", "OpenStreetMap", "Esri.WorldTopoMap", "Esri.WorldGrayCanvas", "Stamen.TonerLite"),
      selected = "CartoDB.Positron"),
      hr(),
      
      # Conditional controls based on the selected tab in the main panel
      conditionalPanel(
        condition = "input.main_tabs == 'Community-Centric'",
        h4("Community-Centric Analysis"),
        radioButtons("community_method", "2. Define Community:",
                     choices = c("Select Potential Vegetation" = "type_eco", "Create Custom Community" = "custom")),
        conditionalPanel("input.community_method == 'type_eco'",
                       selectInput("type_eco_choice", "Select Potential Vegetation:", choices = names(TYPE_ECO_list)),
                       wellPanel(style = "background: #f8f9fa;", textOutput("type_eco_desc"))),
        conditionalPanel("input.community_method == 'custom'",
                       selectizeInput("custom_species", "Select Species:", choices = setNames(species_map$code, species_map$pretty_name), multiple = TRUE)),
        selectInput("csi_metric", "3. Select Suitability Metric:",
                    choices = c("Basic CSI" = "bCSI", "Weighted CSI" = "wCSI", "Geometric Mean CSI" = "gCSI")),
        uiOutput("species_weights_ui"),
        actionButton("run_community_analysis", "Calculate Suitability", icon = icon("cogs"), class = "btn-primary"),
        actionButton("clear_selection", "Clear Plot Selection")
      ),
      
      conditionalPanel(
        condition = "input.main_tabs == 'Site-Centric'",
        h4("Site-Centric Analysis"),
        selectizeInput("plot_choice", "2. Select a Plot ID:", choices = NULL, options = list(placeholder = 'Select a plot...')),
        leafletOutput("site_location_map", height = "200px")
      )
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(id = "main_tabs",
        tabPanel("Community-Centric",
                 h3(textOutput("community_output_title")),
                 leafletOutput("csi_map", height = "600px"),
                 downloadButton("download_csv", "Download CSV"),
                 DT::dataTableOutput("csi_table")),
        tabPanel("Site-Centric",
                 h3(textOutput("site_output_title")),
                 plotOutput("predicted_composition_plot")),
        tabPanel("Climate Maps",
                 h4("Climate Variables for Selected Scenario"),
                 fluidRow(
                   column(6, leafletOutput("map_mat", height = "400px")),
                   column(6, leafletOutput("map_map", height = "400px"))
                 )
        ),
        tabPanel("How to Use",
                 h3("Welcome to the Tree Community-Assisted Migration Simulator (TCAMS)!"),
                 p("This app helps you explore how tree species communities might respond to climate change in Quebec, Canada. It uses a Joint Species Distribution Model (JSDM) to predict species suitability under different climate scenarios. Below, we'll explain the app's purpose, how to navigate it, and key concepts."),
                 hr(),
                 h4("1. Purpose of the App"),
                 p("TCAMS simulates 'assisted migration' – moving tree species or communities to new areas where climate conditions are more suitable in the future. It focuses on presence-absence data (whether a species is likely to occur or not) and provides maps and metrics to guide conservation and forestry decisions."),
                 p("Key features:"),
                 tags$ul(
                   tags$li("Predict species probabilities under current and future climates."),
                   tags$li("Calculate Community Suitability Indices (CSI) for entire communities."),
                   tags$li("Visualize results on interactive maps."),
                   tags$li("Compare scenarios and perspectives (community vs. site-focused).")
                 ),
                 hr(),
                 h4("2. How to Use the App"),
                 p("Follow these steps to explore the data:"),
                 tags$ol(
                   tags$li(tags$strong("Select a Climate Scenario:"), "Choose from current conditions or future projections (e.g., different GCMs like 'SSP3 7.0 - 2041_2070'). This affects all predictions."),
                   tags$li(tags$strong("Choose an Analysis Perspective:"), 
                      tags$ul(
                        tags$li(tags$strong("Community-Centric:"), "Focus on how well a whole community (e.g., a potential vegetation type) fits a site. Define the community, select a metric, and view suitability maps."),
                        tags$li(tags$strong("Site-Centric:"), "Focus on a specific plot. See predicted species composition and run 'what-if' scenarios.")
                      )),
                   tags$li(tags$strong("For Community-Centric:"),
                      tags$ul(
                        tags$li("Define the community: Select a potential vegetation type (pre-defined species lists) or create a custom one by picking species."),
                        tags$li("Choose a Suitability Metric: See explanations below."),
                        tags$li("Optionally, add weights for species importance."),
                        tags$li("Click 'Calculate Suitability' to generate maps and tables.")
                      )),
                   tags$li(tags$strong("For Site-Centric:"), "Pick a plot, view its predicted species, and adjust the community for custom CSI calculations."),
                   tags$li("Explore tabs: 'Main Analysis' for results, 'Climate Maps' for climate variables, and this 'How to Use' guide.")
                 ),
                 p("Tip: Use the sidebar to adjust settings and the main panel to view outputs. Hover over map points for details."),
                 hr(),
                 h4("3. Understanding the Suitability Metrics (CSI)"),
                 p("CSI measures how suitable a site is for a community of species. It's based on predicted probabilities from the model. Here’s a simple breakdown:"),
                 h5("• Basic CSI (bCSI)"),
                 p("The average probability of occurrence for all species in the community."),
                 p("Formula: Mean of probabilities. Example: For species with probs 0.8, 0.6, 0.9 → (0.8+0.6+0.9)/3 = 0.77."),
                 p("Use when: You want a straightforward average; treats all species equally."),
                 h5("• Weighted CSI (wCSI)"),
                 p("A weighted average, emphasizing important species (e.g., keystone species)."),
                 p("Formula: Weighted mean. Example: With weights 2,1,3 → (0.8*2 + 0.6*1 + 0.9*3)/(2+1+3) = 0.82."),
                 p("Use when: Some species matter more; customize with weights."),
                 h5("• Geometric Mean CSI (gCSI)"),
                 p("The geometric mean, which penalizes low probabilities more (multiplicative effect)."),
                 p("Formula: exp(average of logs). Example: exp( (log(0.8)+log(0.6)+log(0.9))/3 ) ≈ 0.76."),
                 p("Use when: All species must be present; sensitive to 'weak links'."),
                 p("Interpretation: Values near 1 mean high suitability; near 0 means low. Use maps to see spatial patterns."),
                 hr(),
                 h4("4. Tips and Troubleshooting"),
                 tags$ul(
                   tags$li("Start with 'Current' scenario to understand baselines."),
                   tags$li("For custom communities, select 2-5 species to avoid overload. Note that the model was trained with a limited number of species. The names of non trained species are available for future improvements."),
                   tags$li("Maps show interpolated data; click points for exact values."),
                   tags$li("If no data appears, check selections and ensure the model ran successfully."),
                   tags$li("Contact developers for model details or data sources.\n João Paulo Czarnecki de Liz \n jpczd@ulaval.ca")
                 ),
                 hr(),
                 h4("5. Methodological Details"),
                 p("This app uses the Hierarchical Model of Species Communities (Hmsc) for joint species distribution modeling, as described in:"),
                 p(tags$em("Tikhonov, G., Opedal, Ø.H., Abrego, N., Lehikoinen, A., de Jonge, M.M.J., Oksanen, J., Ovaskainen, O., 2020. Joint species distribution modelling with the r-package Hmsc. Methods in Ecology and Evolution 11, 442–447. https://doi.org/10.1111/2041-210X.13345")),
                 p("The simulations and modeling are based on permanent inventory sampled plots filtered for no human disturbances from the 4th campaign of the Quebec forest inventory. The variables as environmental predictors used in the modelling process were  Mean Annual Precipitation , Mean Annual Temperature, Organic Matter , Slope, and Drainage."),
                 p("Results for default communities (potential vegetation types) account for interactions among tree species through the joint modeling approach. However, when designing a custom community, the CSI index is calculated from the predicted probabilities of the selected species for a given plot, without iteratively considering interactions in the model predictions."),
                 p("The HMSC Bayesian model was fitted with the following settings: Posterior MCMC sampling with 4 chains each with 1,000 samples, thin 100 and transient 50,000."),
                 p("Model summary: Hmsc object with 5572 sampling units, 13 species, 26 covariates, 1 traits and 1 random levels."),
                 p("Convergence diagnostics: Percentage of parameters with Effective Sample Size (ESS) >= 1000 and Potential Scale Reduction Factor (PSRF) <= 1.1: 96.3%."),
                 h5("Explanatory Power (Tjur R²), AUC, RMSE and Prevalence per species"),
                 tags$table(
                   tags$thead(
                   tags$tr(
                     tags$th("Pretty name"),
                     tags$th("Quebec Inventory Standard Code"),
                     tags$th("French name"),
                     tags$th("Scientific name"),
                     tags$th("Prevalence"),
                     tags$th("AUC"),
                     tags$th("RMSE"),
                     tags$th("Tjur R²")
                   )
                   ),
                   tags$tbody(
                   tags$tr(tags$td("Ace.rubr"), tags$td("ERR"), tags$td("Érable rouge"), tags$td("Acer rubrum"), tags$td("1304"), tags$td("0.92"), tags$td("0.31"), tags$td("0.43")),
                   tags$tr(tags$td("Ace.sacc"), tags$td("ERS"), tags$td("Érable à sucre"), tags$td("Acer saccharum"), tags$td("745"), tags$td("0.99"), tags$td("0.19"), tags$td("0.62")),
                   tags$tr(tags$td("Bet.alle"), tags$td("BOJ"), tags$td("Bouleau jaune"), tags$td("Betula alleghaniensis"), tags$td("917"), tags$td("0.99"), tags$td("0.22"), tags$td("0.52")),
                   tags$tr(tags$td("Bet.papy"), tags$td("BOP"), tags$td("Bouleau à papier (blanc)"), tags$td("Betula papyrifera"), tags$td("2773"), tags$td("0.93"), tags$td("0.34"), tags$td("0.44")),
                   tags$tr(tags$td("Pic.glau"), tags$td("EPB"), tags$td("Épinette blanche"), tags$td("Picea glauca"), tags$td("1984"), tags$td("0.94"), tags$td("0.33"), tags$td("0.43")),
                   tags$tr(tags$td("Abi.bals"), tags$td("SAB"), tags$td("Sapin baumier"), tags$td("Abies balsamea"), tags$td("3740"), tags$td("0.98"), tags$td("0.25"), tags$td("0.59")),
                   tags$tr(tags$td("Pic.mari"), tags$td("EPN"), tags$td("Épinette noire"), tags$td("Picea mariana"), tags$td("3229"), tags$td("0.96"), tags$td("0.29"), tags$td("0.55")),
                   tags$tr(tags$td("Thu.occi"), tags$td("THO"), tags$td("Thuya occidental"), tags$td("Thuja occidentalis"), tags$td("540"), tags$td("0.92"), tags$td("0.25"), tags$td("0.25")),
                   tags$tr(tags$td("Pru.pens"), tags$td("PRP"), tags$td("Cerisier de Pennsylvanie"), tags$td("Prunus pensylvanica"), tags$td("350"), tags$td("0.83"), tags$td("0.23"), tags$td("0.08")),
                   tags$tr(tags$td("Pop.trem"), tags$td("PET"), tags$td("Peuplier faux-tremble"), tags$td("Populus tremuloides"), tags$td("1034"), tags$td("0.83"), tags$td("0.34"), tags$td("0.18")),
                   tags$tr(tags$td("Pin.stro"), tags$td("PIB"), tags$td("Pin blanc"), tags$td("Pinus strobus"), tags$td("330"), tags$td("0.93"), tags$td("0.20"), tags$td("0.24")),
                   tags$tr(tags$td("Lar.lari"), tags$td("MEL"), tags$td("Mélèze laricin"), tags$td("Larix laricina"), tags$td("241"), tags$td("0.90"), tags$td("0.19"), tags$td("0.13")),
                   tags$tr(tags$td("Pin.bank"), tags$td("PIG"), tags$td("Pin gris"), tags$td("Pinus banksiana"), tags$td("688"), tags$td("0.96"), tags$td("0.23"), tags$td("0.41"))
                   )
                 ),
                 p("Enjoy exploring climate adaptation strategies with CAMS!")
        )
      )
    )
  )
)

# --- SERVER ---
server <- function(input, output, session) {

  # Update plot_choice with server-side selectize for performance
  updateSelectizeInput(session, "plot_choice", choices = plots_for_map$plot_id, server = TRUE)

  # --- Reactive Data ---
  preds_summary <- reactive({
    all_predictions_summary[[input$climate_scenario]]
  })

  current_climate_data <- reactive({
    XData_future_list[[input$climate_scenario]] %>%
      as_tibble(rownames = "plot_id") %>%
      left_join(plots_for_map, by = "plot_id")
  })

  # --- Community-Centric Logic ---
  target_community_species <- reactive({
    if (input$community_method == "type_eco") {
      TYPE_ECO_list[[input$type_eco_choice]]
    } else {
      species_map$original_name_in_data[match(input$custom_species, species_map$code)]
    }
  })

  output$type_eco_desc <- renderText(
    type_eco_descriptions$Description[type_eco_descriptions$type_eco_prefix == str_sub(input$type_eco_choice, 1, 3)]
  )

  output$species_weights_ui <- renderUI({
    req(input$csi_metric == "wCSI")
    species <- target_community_species()
    map(species, ~ numericInput(paste0("weight_", .x), label = species_map$pretty_name[species_map$original_name_in_data == .x], value = 1, min = 0, max = 100))
  })

  csi_results <- eventReactive(input$run_community_analysis, {
    community_spp <- target_community_species()
    req(length(community_spp) > 0)
    pred_df <- preds_summary()$mean

    user_weights <- NULL
    if (input$csi_metric == "wCSI") {
      user_weights <- setNames(vapply(community_spp, function(x) {
        val <- input[[paste0("weight_", x)]]
        if (is.null(val)) 1 else val
      }, numeric(1)), community_spp)
    }

    mean_csi <- apply(pred_df, 1, function(p) calculate_csi(community_spp, p, input$csi_metric, user_weights))
    lower_csi <- apply(preds_summary()$lower, 1, function(p) calculate_csi(community_spp, p, input$csi_metric, user_weights))
    upper_csi <- apply(preds_summary()$upper, 1, function(p) calculate_csi(community_spp, p, input$csi_metric, user_weights))

    tibble(plot_id = rownames(pred_df), mean = mean_csi, lower = lower_csi, upper = upper_csi) %>%
      mutate(ci_width = upper - lower) %>%
      left_join(plots_for_map, by = "plot_id") %>%
      left_join(current_climate_data() %>% select(plot_id, MAP, MAT, pH, Clay, OM, rtp, twi, slope, drainage_class), by = "plot_id")
  })

  output$community_output_title <- renderText(paste("Community Suitability for:", input$climate_scenario))

  output$csi_map <- renderLeaflet({
    req(csi_results())
    df <- csi_results()
    pal <- colorNumeric(palette = "viridis", domain = df$mean, na.color = "transparent")
    leaflet(df) %>%
      addProviderTiles(providers[[input$basemap]]) %>%
      addCircleMarkers(lng = ~longitude, lat = ~latitude, color = ~pal(mean),
                       radius = 5, stroke = FALSE, fillOpacity = 0.8,
                       layerId = ~plot_id,
                       popup = ~paste("Plot:", plot_id, "<br>Mean CSI:", round(mean, 3))) %>%
      addLegend("bottomright", pal = pal, values = ~mean, title = "Mean CSI")
  })

  output$csi_table <- DT::renderDataTable({
    req(csi_results())
    csi_results() %>%
      select(plot_id, latitude, longitude, MAP, MAT, pH, Clay, OM, rtp, twi, slope, drainage_class, mean, lower, upper, ci_width) %>%
      arrange(desc(mean)) %>%
      mutate(across(where(is.numeric), ~round(., 3))) %>%
      DT::datatable(options = list(pageLength = 5), rownames = FALSE)
  })

  # Download handler for CSV
  output$download_csv <- downloadHandler(
    filename = function() {
      paste("csi_results_", input$climate_scenario, "_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(csi_results(), file, row.names = FALSE)
    }
  )

  # --- Site-Centric Logic ---
  selected_plot_preds <- reactive({
    req(input$plot_choice, input$climate_scenario, cancelOutput = TRUE)
    preds <- all_predictions_summary[[input$climate_scenario]]$mean
    message("Selected plot_id: ", input$plot_choice)
    message("Available plot_ids in preds: ", paste(head(rownames(preds)), collapse = ", "))
    if (!(input$plot_choice %in% rownames(preds))) {
      message("Error: plot_id not found in predictions")
      return(NULL)
    }
    selected <- preds[input$plot_choice, , drop = FALSE]
    message("Selected predictions: ", paste(selected, collapse = ", "))
    selected
  })

  output$site_location_map <- renderLeaflet({
    req(input$plot_choice)
    plot_info <- plots_for_map %>% filter(plot_id == input$plot_choice)
    leaflet() %>% addProviderTiles(providers[[input$basemap]]) %>%
      addMarkers(lng = plot_info$longitude, lat = plot_info$latitude) %>%
      setView(lng = plot_info$longitude, lat = plot_info$latitude, zoom = 10)
  })

  output$site_output_title <- renderText({
    req(input$plot_choice)
    paste("Predicted Community for Plot:", input$plot_choice, "under", input$climate_scenario)
  })

  output$predicted_composition_plot <- renderPlot({
    preds <- selected_plot_preds()
    req(nrow(preds) > 0)

    plot_data <- as.data.frame(preds) %>%
      pivot_longer(everything(), names_to = "original_name_in_data", values_to = "prob") %>%
      left_join(species_map, by = "original_name_in_data") %>%
      filter(!is.na(prob)) %>%
      arrange(desc(prob)) %>%
      head(20)

    ggplot(plot_data, aes(x = reorder(pretty_name, prob), y = prob)) +
      geom_col(fill = "steelblue") +
      coord_flip() +
      labs(x = "Species", y = "Probability of Occurrence", title = "Top 20 Most Probable Species") +
      theme_minimal(base_size = 14)
  })

  # --- Climate Maps Tab ---
  output$map_mat <- renderLeaflet({
    df <- current_climate_data()
    req(nrow(df) > 0)
    pal <- colorNumeric(palette = "viridis", domain = global_mat_range, na.color = "transparent")
    leaflet(df) %>% addProviderTiles(providers[[input$basemap]]) %>%
      addCircleMarkers(lng = ~longitude, lat = ~latitude, color = ~pal(MAT),
                       radius = 5, stroke = FALSE, fillOpacity = 0.8,
                       popup = ~paste("Plot:", plot_id, "<br>MAT:", round(MAT, 1), "°C")) %>%
      addLegend("bottomright", pal = pal, values = ~MAT, title = "MAT (°C)")
  })

  output$map_map <- renderLeaflet({
    df <- current_climate_data()
    req(nrow(df) > 0)
    pal <- colorNumeric(palette = "magma", domain = global_map_range, na.color = "transparent")
    leaflet(df) %>% addProviderTiles(providers[[input$basemap]]) %>%
      addCircleMarkers(lng = ~longitude, lat = ~latitude, color = ~pal(MAP),
                       radius = 5, stroke = FALSE, fillOpacity = 0.8,
                       popup = ~paste("Plot:", plot_id, "<br>MAP:", round(MAP, 0), "mm")) %>%
      addLegend("bottomright", pal = pal, values = ~MAP, title = "MAP (mm)")
  })
}

# --- Run the App ---
shinyApp(ui = ui, server = server)
# --- End of app.R ---
