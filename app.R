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
if (requireNamespace("shinycssloaders", quietly = TRUE)) {
  library(shinycssloaders)
  .with_spinner <- function(ui_el) shinycssloaders::withSpinner(ui_el, color = "#2c3e50", proxy.height = "300px")
} else {
  message("[TCAMS] Package 'shinycssloaders' not installed; loading indicators disabled. Run install.packages('shinycssloaders') to enable them.")
  .with_spinner <- function(ui_el) ui_el
}

## Simplified single-folder deployment: all data and draw files reside with app.R
## Set base path to current working directory. Place app.R and all *.RData / *_preds_subsampled.Rda together.
path <- "."
message("[TCAMS] Single-folder mode. Data path set to ", normalizePath(path, winslash="/", mustWork = FALSE))

# --- Load pre-processed data ---
load(file.path(path, "all_predictions_summary.RData"))
load(file.path(path, "type_eco_data.RData"))
load(file.path(path, "species_map.RData"))
load(file.path(path, "plot_coordinates.RData"))
load(file.path(path, "XData_future_list.RData"))

# --- Build a display name that always shows the full scientific name ---
# Some species share the same 3-letter genus abbreviation used by `pretty_name`
# (e.g. Picea glauca / Picea mariana -> "Pic."), which can look identical at a
# glance. The full binomial name removes that ambiguity everywhere species are
# shown to the user (dropdowns, weight labels, plot axes, tables).
if (exists("species_map") && is.data.frame(species_map)) {
  if (!"scientific_name" %in% names(species_map)) {
    species_map$scientific_name <- NA_character_
  }
  species_map$display_name <- dplyr::coalesce(
    species_map$scientific_name,
    species_map$pretty_name,
    species_map$code
  )

  # Common English/French names, for dropdowns only (kept out of plot axes/legends
  # to keep those compact and italicised on the scientific name alone). Mirrors the
  # static reference table in the "How to Use" tab.
  common_names_lookup <- tibble::tribble(
    ~code, ~common_name_en,          ~common_name_fr,
    "ERR", "Red Maple",              "Érable rouge",
    "ERS", "Sugar Maple",            "Érable à sucre",
    "BOJ", "Yellow Birch",           "Bouleau jaune",
    "BOP", "Paper Birch",            "Bouleau à papier (blanc)",
    "EPB", "White Spruce",           "Épinette blanche",
    "SAB", "Balsam Fir",             "Sapin baumier",
    "EPN", "Black Spruce",           "Épinette noire",
    "THO", "Eastern White-cedar",    "Thuya occidental",
    "PRP", "Pin Cherry",             "Cerisier de Pennsylvanie",
    "PET", "Trembling Aspen",        "Peuplier faux-tremble",
    "PIB", "Eastern White Pine",     "Pin blanc",
    "MEL", "Tamarack",               "Mélèze laricin",
    "PIG", "Jack Pine",              "Pin gris"
  )
  species_map <- species_map %>% dplyr::left_join(common_names_lookup, by = "code")
  species_map$dropdown_label <- dplyr::if_else(
    !is.na(species_map$common_name_en),
    paste0(species_map$display_name, " — ", species_map$common_name_en),
    species_map$display_name
  )
}

# --- Validated scope for this release ---
# Only these 13 species and 8 vegetation communities have been curated/validated
# for this app so far. Anything else that might appear in species_map/TYPE_ECO_list
# (e.g. from the broader official Quebec ecological classification) is shown in
# dropdowns as visually "ghosted" and blocked from actually being used until it
# goes through the same validation. This is enforced twice: (1) cosmetically in the
# dropdown labels below, and (2) functionally via server-side observers further
# down that revert any such selection and show a warning notification.
VALID_SPECIES_CODES <- c("ERR", "ERS", "BOJ", "BOP", "EPB", "SAB", "EPN", "THO", "PRP", "PET", "PIB", "MEL", "PIG")
VALID_COMMUNITY_CODES <- c("FE3", "ME1", "MS1", "MS6", "MS2", "RE2", "RC3", "RS2")
GHOST_MARKER <- "\U0001F6A7 "   # construction sign
ghost_label <- function(label) paste0(GHOST_MARKER, label, " (in development)")

if (exists("species_map") && is.data.frame(species_map)) {
  species_map$dropdown_label <- dplyr::if_else(
    species_map$code %in% VALID_SPECIES_CODES,
    species_map$dropdown_label,
    ghost_label(species_map$dropdown_label)
  )
  # Valid species first, then alphabetical, so the working options are easy to find.
  species_map <- species_map %>%
    dplyr::arrange(!(code %in% VALID_SPECIES_CODES), code)
}
# Same whitelist, expressed in the "original_name_in_data" form (e.g. "ERR_n")
# used inside TYPE_ECO_list / prediction matrix column names, for defensive
# filtering wherever a community's member species get resolved.
VALID_ORIGINAL_NAMES <- species_map$original_name_in_data[species_map$code %in% VALID_SPECIES_CODES]

# --- Trajectory data: species raw probability & community CSI across scenarios/horizons ---
# These two files are pre-computed long-format summaries (posterior mean + 95% quantiles)
# spanning the Current baseline and all 12 future horizon x SSP combinations. See
# script/003_compute_csi_from_draws.R in the companion analysis repository for how they
# are generated from the full posterior draws.
traj_dir <- file.path(path, "data", "trajectories")
horizon_levels <- c("Baseline", "2011-2040", "2041-2070", "2071-2100")
ssp_levels <- c("Current", "SSP1-2.6", "SSP2-4.5", "SSP3-7.0", "SSP5-8.5")

.derive_ssp <- function(scenario_label, projection_horizon) {
  dplyr::if_else(
    scenario_label == "Current" | projection_horizon == "Baseline",
    "Current",
    stringr::str_remove(scenario_label, "\\s+\\d{4}-\\d{4}$")
  )
}

species_traj_summary <- tryCatch({
  df <- readr::read_csv(file.path(traj_dir, "species_prob_by_scenario_draws_summary_combined.csv"), show_col_types = FALSE)
  df %>%
    mutate(
      ssp = .derive_ssp(scenario_label, projection_horizon),
      horizon = factor(projection_horizon, levels = horizon_levels),
      ssp = factor(ssp, levels = ssp_levels)
    ) %>%
    left_join(species_map %>% select(original_name_in_data, code, display_name), by = c("species" = "original_name_in_data")) %>%
    mutate(display_name = coalesce(display_name, species))
}, error = function(e) {
  warning("[TCAMS] Could not load species trajectory data: ", conditionMessage(e))
  tibble()
})

community_traj_summary <- tryCatch({
  df <- readr::read_csv(file.path(traj_dir, "gCSI_trajectory_summary_draws.csv"), show_col_types = FALSE)
  df %>%
    mutate(
      ssp = .derive_ssp(scenario_label, projection_horizon),
      horizon = factor(projection_horizon, levels = horizon_levels),
      ssp = factor(ssp, levels = ssp_levels)
    )
}, error = function(e) {
  warning("[TCAMS] Could not load community trajectory data: ", conditionMessage(e))
  tibble()
})

message("[TCAMS] Trajectory data loaded: ", nrow(species_traj_summary), " species-scenario rows, ",
        nrow(community_traj_summary), " community-scenario rows")

# Repeats the Current/Baseline rows of a trajectory data.frame across every future SSP,
# so each per-SSP facet starts from the same shared baseline point.
replicate_baseline_across_ssp <- function(df) {
  if (!nrow(df)) return(df)
  baseline <- df %>% filter(ssp == "Current")
  future <- df %>% filter(ssp != "Current")
  if (!nrow(baseline) || !nrow(future)) return(df)
  future_ssps <- setdiff(levels(future$ssp), "Current")
  baseline_rep <- purrr::map_dfr(future_ssps, function(s) baseline %>% mutate(ssp = factor(s, levels = ssp_levels)))
  dplyr::bind_rows(baseline_rep, future)
}

# Harmonise plot coordinate object naming and structure
if (!exists("plots_for_map")) {
  if (exists("plot_coordinates")) {
    plots_for_map <- plot_coordinates
  } else {
    warning("'plot_coordinates.RData' did not contain an object named 'plots_for_map'. Building placeholder coordinates with NA longitude/latitude; please provide plot_coordinates with 5572 rows.")
    first_pred <- all_predictions_summary[[1]]$mean
    fallback_ids <- rownames(first_pred)
    plots_for_map <- tibble(
      plot_id = fallback_ids,
      longitude = NA_real_,
      latitude = NA_real_
    )
  }
}

required_coord_cols <- c("plot_id", "longitude", "latitude")
missing_coord_cols <- setdiff(required_coord_cols, colnames(plots_for_map))
if (length(missing_coord_cols)) {
  for (mc in missing_coord_cols) {
    plots_for_map[[mc]] <- NA_real_
  }
}
plots_for_map <- plots_for_map %>%
  mutate(plot_id = as.character(plot_id)) %>%
  select(any_of(required_coord_cols))
plot_coordinates <- plots_for_map

# Use comprehensive `type_eco_descriptions` loaded from RData if available; otherwise minimal fallback.
if (!exists("type_eco_descriptions") || !is.data.frame(type_eco_descriptions) ||
    !all(c("type_eco_prefix","Description") %in% names(type_eco_descriptions))) {
  type_eco_descriptions <- data.frame(
    type_eco_prefix = c("ERS", "BOJ", "BOP", "EPB", "SAB", "EPN", "THO", "PRP", "PET", "PIB", "MEL", "PIG"),
    Description = c(
      "Érable à sucre (Sugar Maple)",
      "Bouleau jaune (Yellow Birch)",
      "Bouleau à papier (Paper Birch)",
      "Épinette blanche (White Spruce)",
      "Sapin baumier (Balsam Fir)",
      "Épinette noire (Black Spruce)",
      "Thuya occidental (Eastern White Cedar)",
      "Cerisier de Pennsylvanie (Pin Cherry)",
      "Peuplier faux-tremble (Trembling Aspen)",
      "Pin blanc (Eastern White Pine)",
      "Mélèze laricin (Tamarack)",
      "Pin gris (Jack Pine)"
    )
  )
  message("Fallback type_eco_descriptions created (comprehensive file not found or malformed)")
} else {
  message("Loaded type_eco_descriptions with ", nrow(type_eco_descriptions), " rows")
}

# Longest-prefix description lookup (e.g. FC10A -> FC10 -> FC1 if only FC1 exists)
find_type_eco_description <- function(code) {
  prefixes <- type_eco_descriptions$type_eco_prefix
  if (is.null(prefixes) || !length(prefixes)) return(NA_character_)
  matches <- prefixes[startsWith(code, prefixes)]
  if (!length(matches)) return(NA_character_)
  pref <- matches[which.max(nchar(matches))]
  desc <- type_eco_descriptions$Description[type_eco_descriptions$type_eco_prefix == pref]
  if (length(desc)) desc[[1]] else NA_character_
}

# Build labelled choices for potential vegetation (code - description)
community_choices <- {
  if (exists("TYPE_ECO_list") && length(TYPE_ECO_list)) {
    codes <- sort(unique(names(TYPE_ECO_list)))
    labels <- vapply(codes, function(k) {
      d <- find_type_eco_description(k)
      if (is.na(d) || !nzchar(d)) {
        # Build fallback description from first 3 species pretty names
        spp <- TYPE_ECO_list[[k]]
        spp_pretty <- species_map$display_name[match(spp, species_map$original_name_in_data)]
        spp_pretty <- spp_pretty[!is.na(spp_pretty)]
        if (length(spp_pretty)) {
          d <- paste("Community:", paste(head(spp_pretty, 3), collapse=", "))
        } else {
          d <- "Community definition unavailable"
        }
      }
      label <- paste(k, "-", d)
      if (!(k %in% VALID_COMMUNITY_CODES)) label <- ghost_label(label)
      label
    }, character(1))
    # Validated communities first (alphabetical), then everything else still under
    # development (also alphabetical), so the 8 working options are easy to find.
    ord <- order(!(codes %in% VALID_COMMUNITY_CODES), codes)
    setNames(codes[ord], labels[ord])
  } else {
    character(0)
  }
}
message("community_choices built: ", length(community_choices), " entries")
if (length(community_choices)) message("Example labels: ", paste(head(names(community_choices), 10), collapse = "; "))

# Standardize covariate order across all XData entries
covariate_order <- c("pH", "Clay", "OM", "rtp", "twi", "slope", "drainage_class", "MAT", "MAP")
if (exists("XData_future_list") && length(XData_future_list)) {
  reference_levels <- list()
  exemplar <- XData_future_list[[1]]
  factor_cols <- names(Filter(is.factor, as.data.frame(exemplar)))
  if (length(factor_cols)) {
    reference_levels <- lapply(factor_cols, function(fc) levels(exemplar[[fc]]))
    names(reference_levels) <- factor_cols
  }
  XData_future_list <- lapply(XData_future_list, function(df) {
    df <- as.data.frame(df)
    for (fc in names(reference_levels)) {
      if (fc %in% colnames(df)) {
        df[[fc]] <- factor(df[[fc]], levels = reference_levels[[fc]])
      }
    }
    desired_order <- if (all(covariate_order %in% colnames(df))) covariate_order else colnames(df)
    missing_preds <- setdiff(desired_order, colnames(df))
    if (length(missing_preds)) {
      stop("Predictor(s) missing in XData entry: ", paste(missing_preds, collapse = ", "))
    }
    df[, desired_order, drop = FALSE]
  })
}

# str(all_predictions_summary)
# str(TYPE_ECO_list)
# str(species_map)
# str(plot_coordinates)
# str(XData_future_list)


# --- Calculate global climate ranges for consistent map legends ---
all_mats <- unlist(lapply(XData_future_list, function(df) df$MAT))
all_maps <- unlist(lapply(XData_future_list, function(df) df$MAP))
global_mat_range <- range(all_mats, na.rm = TRUE)
global_map_range <- range(all_maps, na.rm = TRUE)

# --- Uncertainty parameters and draw file map ---
epsilon <- 1e-4
hdi_prob <- 0.95  # aligned with the 95% credible intervals reported in the manuscript
## Subsampled posterior draw files (*_preds_subsampled.Rda) live in ./posterior_draws.
## NOTE: this bundle currently ships subsampled draws for the 12 future SSP x horizon
## scenarios only -- there is no Current_preds_subsampled.Rda (baseline draws), so the
## "Use posterior draws" toggle will fall back to the summary-based estimate for the
## Current scenario even when enabled. See README.md "Known limitations".
draws_dir <- file.path(path, "posterior_draws")
if (!dir.exists(draws_dir)) draws_dir <- path  # defensive fallback for older/flat bundles
draw_files <- list.files(draws_dir, pattern = "_preds_subsampled\\.Rda$", full.names = TRUE)
message("[TCAMS] Draw files detected: ", length(draw_files))
if (length(draw_files)) message("[TCAMS] Example draw files: ", paste(head(basename(draw_files), 8), collapse = "; "))

sanitize_key <- function(x) {
  x %>%
    str_to_lower() %>%
    str_replace_all("[^a-z0-9]", "")
}

# Build scenario map for posterior draw files
scenario_map <- {
  base_map <- tibble(label = names(all_predictions_summary))
  if (!length(draw_files)) return(base_map %>% mutate(file_path = NA_character_))

  available_files <- tibble(file_path = draw_files) %>%
    mutate(base_name = basename(file_path),
           sans_ext = tools::file_path_sans_ext(base_name),
           # Primary candidate: remove the suffix; also remove optional .gcm
           label_candidate = str_remove(sans_ext, "_preds_subsampled$") %>% str_remove("\\.gcm$"),
           label_key = sanitize_key(label_candidate)) %>%
    filter(!is.na(label_key) & label_key != "") %>%
    distinct(label_key, .keep_all = TRUE)

  # Scenario keys (try both original and with .gcm removed if present)
  scenario_tbl <- base_map %>%
    mutate(label_key = sanitize_key(label),
           label_key_alt = sanitize_key(str_remove(label, "\\.gcm$")))

  # Initial exact join on label_key or alternate key
  mapped_primary <- scenario_tbl %>%
    left_join(available_files %>% select(label_key, file_path), by = "label_key")
  # Fill NAs using alt key
  mapped_primary <- mapped_primary %>%
    mutate(file_path = ifelse(is.na(file_path), available_files$file_path[match(label_key_alt, available_files$label_key)], file_path))

  # Fuzzy match remaining NA by prefix containment
  remaining <- mapped_primary %>% filter(is.na(file_path))
  if (nrow(remaining)) {
    af <- available_files
    for (i in seq_len(nrow(remaining))) {
      skey <- remaining$label_key[i]
      # Candidates where either available starts with scenario key or vice-versa
      cand <- af %>% filter(str_starts(label_key, skey) | str_starts(skey, label_key))
      if (nrow(cand) == 1) {
        mapped_primary$file_path[mapped_primary$label_key == skey] <- cand$file_path[1]
      }
    }
  }

  mapped <- mapped_primary %>% transmute(label, file_path)
  message("[TCAMS] Scenario draws mapped (exact+fuzzy): ", sum(!is.na(mapped$file_path)), "/", nrow(mapped))
  # Debug examples
  if (sum(!is.na(mapped$file_path))) {
    ex_present <- mapped %>% filter(!is.na(file_path)) %>% head(6)
    message("[TCAMS] Examples with draws: ", paste(ex_present$label, collapse = "; "))
  }
  ex_missing <- mapped %>% filter(is.na(file_path)) %>% head(6)
  if (nrow(ex_missing)) message("[TCAMS] Examples without draws: ", paste(ex_missing$label, collapse = "; "))
  mapped
}

# --- Helper Functions ---
# Compute HDI using sorted samples; fallback to quantiles on failure
safe_hdi <- function(x, prob = hdi_prob) {
  vals <- sort(x[is.finite(x)])
  n <- length(vals)
  if (!n) return(c(lower = NA_real_, upper = NA_real_))
  if (prob <= 0 || prob >= 1) return(c(lower = min(vals), upper = max(vals)))
  eq_probs <- c((1 - prob) / 2, 1 - (1 - prob) / 2)
  eq <- as.numeric(quantile(vals, probs = eq_probs, names = FALSE, type = 7))
  window <- max(floor(prob * n), 1)
  if (window >= n) return(c(lower = eq[1], upper = eq[2]))
  widths <- vals[(window + 1):n] - vals[1:(n - window)]
  if (!length(widths)) return(c(lower = eq[1], upper = eq[2]))
  idx <- which.min(widths)
  if (!length(idx) || is.na(idx)) {
    return(c(lower = eq[1], upper = eq[2]))
  }
  c(lower = vals[idx], upper = vals[idx + window])
}

# test posterior summarisation with a scenario


# Summarise posterior samples with center, HDI, and quantiles
summarise_posterior <- function(x, prob = hdi_prob, central = c("median", "mean")) {
  vals <- x[is.finite(x)]
  central <- match.arg(central)
  if (!length(vals)) {
    return(tibble(center = NA_real_, mean = NA_real_, sd = NA_real_, hdi_lower = NA_real_, hdi_upper = NA_real_, q025 = NA_real_, q975 = NA_real_))
  }
  hdi <- safe_hdi(vals, prob)
  qs <- quantile(vals, probs = c(0.025, 0.975), na.rm = TRUE, names = FALSE)
  tibble(
    center = if (central == "median") median(vals) else mean(vals),
    mean = mean(vals),
    sd = sd(vals),
    hdi_lower = hdi[["lower"]],
    hdi_upper = hdi[["upper"]],
    q025 = qs[[1]],
    q975 = qs[[2]]
  )
}

# Compute CSI per draw and site for requested metric
compute_csi_per_draw_site <- function(draws, species_vec, metric = c("bCSI", "wCSI", "gCSI"), weights = NULL, eps = epsilon, progress_fun = NULL) {
  metric <- match.arg(metric)
  draw_dimnames <- dimnames(draws)
  available_spp <- draw_dimnames[[3]]
  keep <- intersect(species_vec, available_spp)
  if (!length(keep)) return(tibble())

  weight_lookup <- if (is.null(weights)) setNames(rep(1, length(keep)), keep) else weights[keep]
  weight_lookup[is.na(weight_lookup)] <- 0

  n_draws <- dim(draws)[1]
  site_ids <- draw_dimnames[[2]]

  res <- map_dfr(seq_len(n_draws), function(d) {
    if (!is.null(progress_fun)) {
      tryCatch(progress_fun(d, n_draws), error = function(e) NULL)
    }
    mat <- draws[d, site_ids, keep, drop = FALSE]
    mat <- matrix(mat, nrow = length(site_ids), ncol = length(keep),
                  dimnames = list(site = site_ids, species = keep))
    map_dfr(site_ids, function(site_id) {
      site_slice <- mat[site_id, , drop = FALSE]
      probs <- as.numeric(site_slice)
      names(probs) <- colnames(site_slice)
      valid <- !is.na(probs)
      if (!any(valid)) return(NULL)
      probs_use <- probs[valid]
      weights_use <- weight_lookup[names(probs_use)]
      if (all(weights_use == 0) || all(is.na(weights_use))) {
        weights_use <- rep(1, length(probs_use))
      }
      weights_use[is.na(weights_use)] <- 0
      if (sum(weights_use) <= 0) {
        weights_use <- rep(1 / length(probs_use), length(probs_use))
      } else {
        weights_use <- weights_use / sum(weights_use)
      }

      if (metric == "bCSI") {
        tibble(site = site_id, value = mean(probs_use))
      } else if (metric == "wCSI") {
        tibble(site = site_id, value = sum(probs_use * weights_use))
      } else {
        log_probs <- log(pmax(probs_use, eps))
        log_val <- sum(log_probs * weights_use)
        tibble(site = site_id, value = exp(log_val), log_value = log_val)
      }
    }) %>% mutate(draw = d, .before = 1)
  })

  res
}

# Summarise CSI draws per site with HDI and quantiles
summarise_csi_over_draws <- function(draw_site_df, prob = hdi_prob, central = "median") {
  if (!nrow(draw_site_df)) return(tibble())
  central <- match.arg(central, c("median", "mean"))

  draw_site_df %>%
    group_by(site) %>%
    group_modify(~ {
      stats <- summarise_posterior(.x$value, prob = prob, central = central)
      if ("log_value" %in% names(.x)) {
        log_stats <- summarise_posterior(.x$log_value, prob = prob, central = central) %>%
          rename_with(~ paste0("log_", .x))
        bind_cols(stats, log_stats)
      } else {
        stats
      }
    }) %>%
    ungroup()
}

# Load posterior draws for a scenario and validate into array
load_draws_for_scenario <- function(scn_label) {
  entry <- scenario_map %>% filter(label == scn_label, !is.na(file_path)) %>% slice_head(n = 1)
  if (!nrow(entry)) {
    message("No draw file mapped for scenario: ", scn_label)
    return(NULL)
  }
  file_path <- entry$file_path
  if (!file.exists(file_path)) {
    message("Mapped draw file missing on disk: ", file_path)
    return(NULL)
  }
  env <- new.env(parent = emptyenv())
  tryCatch({
    load(file_path, envir = env)
    if (!exists("preds", envir = env)) {
      message("Object 'preds' not found in draw file: ", file_path)
      return(NULL)
    }
    preds_list <- get("preds", envir = env)
    if (!is.list(preds_list) || !length(preds_list)) {
      message("'preds' is not a non-empty list in ", file_path)
      return(NULL)
    }
    first_mat <- preds_list[[1]]
    if (!is.matrix(first_mat)) {
      message("First element of 'preds' not a matrix in ", file_path)
      return(NULL)
    }
    site_ids <- rownames(first_mat)
    species_ids <- colnames(first_mat)
    if (is.null(site_ids) || is.null(species_ids)) {
      message("Matrix missing dimnames (site/species) in ", file_path)
      return(NULL)
    }
    aligned <- map(preds_list, function(mat) {
      if (!is.matrix(mat)) return(NULL)
      if (is.null(rownames(mat)) || is.null(colnames(mat))) return(NULL)
      if (!all(site_ids %in% rownames(mat))) return(NULL)
      if (!all(species_ids %in% colnames(mat))) return(NULL)
      mat[site_ids, species_ids, drop = FALSE]
    })
    if (any(map_lgl(aligned, is.null))) {
      message("One or more draw matrices failed alignment for scenario: ", scn_label)
      return(NULL)
    }
    n_draws <- length(aligned)
    draws_array <- array(NA_real_, dim = c(n_draws, length(site_ids), length(species_ids)),
                        dimnames = list(draw = seq_len(n_draws), site = site_ids, species = species_ids))
    for (i in seq_len(n_draws)) draws_array[i, , ] <- aligned[[i]]
    message("Loaded ", n_draws, " draws for scenario: ", scn_label)
    draws_array
  }, error = function(e) {
    message("Failed to load draws for ", scn_label, ": ", conditionMessage(e))
    NULL
  })
}


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
    if (all(is.na(valid_weights)) || sum(valid_weights, na.rm = TRUE) == 0) {
      return(mean(probs, na.rm = TRUE))
    }
    return(weighted.mean(probs, valid_weights, na.rm = TRUE))
  } else if (method == "gCSI") {
    return(exp(mean(log(pmax(probs, epsilon)), na.rm = TRUE)))
  }
}

# --- Hexagon Map helpers ---
# NAD83 / Quebec Lambert (metres) — a metric CRS is needed so a 20 km hexagon
# actually measures 20 km, matching the projection already used elsewhere in
# this project's analysis pipeline (create_spatial_studyDesign_for_Hmsc.R).
HEX_CRS <- 32198
HEX_CELLSIZE_M <- 20000

# Builds a POINT sf object (in HEX_CRS) of plot_id/value pairs, dropping any
# plot without coordinates or without a finite value.
build_hex_point_sf <- function(plot_ids, values) {
  df <- tibble(plot_id = as.character(plot_ids), value = as.numeric(values)) %>%
    dplyr::left_join(plots_for_map, by = "plot_id") %>%
    dplyr::filter(!is.na(longitude), !is.na(latitude), is.finite(value))
  if (!nrow(df)) return(NULL)
  sf::st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326) %>% sf::st_transform(HEX_CRS)
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
      checkboxInput("use_draws", label = "Use posterior draws (slower)", value = FALSE),
      selectInput("comparison_scenario", "2. Select Comparison Scenario (optional):", choices = c("None", names(all_predictions_summary)), selected = "None"),
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
                 selectizeInput("type_eco_choice", "Select Potential Vegetation:", choices = community_choices, selected = VALID_COMMUNITY_CODES[1], options = list(placeholder = 'Type or pick a vegetation type...')),
                 wellPanel(style = "background: #f8f9fa;", textOutput("type_eco_desc"))),
        conditionalPanel("input.community_method == 'custom'",
                       selectizeInput("custom_species", "Select Species:", choices = setNames(species_map$code, species_map$dropdown_label), multiple = TRUE)),
        selectInput("csi_metric", "3. Select Suitability Metric:",
                    choices = c("Basic CSI" = "bCSI", "Weighted CSI" = "wCSI", "Geometric Mean CSI" = "gCSI")),
        tags$div(style = "font-size: 0.82em; color: #6c757d; margin-top: -6px; margin-bottom: 10px;",
          tags$strong("bCSI"), ": simple average of each species' occurrence probability. ",
          tags$strong("wCSI"), ": average weighted by the species importance you set below. ",
          tags$strong("gCSI"), ": geometric mean — penalises communities where even one species has low suitability (a \"weakest-link\" index)."
        ),
        uiOutput("species_weights_ui"),
        actionButton("run_community_analysis", "Calculate Suitability", icon = icon("cogs"), class = "btn-primary") 
      ),
      
      conditionalPanel(
        condition = "input.main_tabs == 'Site-Centric'",
        h4("Site-Centric Analysis"),
        selectizeInput("plot_choice", "2. Select a Plot ID:", choices = NULL, options = list(placeholder = 'Select a plot...')),
        leafletOutput("site_location_map", height = "200px")
      ),

      conditionalPanel(
        condition = "input.main_tabs == 'Trajectories'",
        h4("Trajectories: Baseline to Future"),
        tags$div(style = "font-size: 0.85em; color: #6c757d; margin-bottom: 8px;",
                 icon("info-circle"),
                 " The \"Select Climate Scenario\" selector above is ignored on this tab — it always plots the Current baseline plus all 12 future scenarios together."),
        radioButtons("traj_view", "2. View by:",
                     choices = c("Potential Vegetation Community" = "community",
                                 "Individual Species" = "species")),
        conditionalPanel(
          "input.traj_view == 'community'",
          selectInput("traj_community", "Select Community:", choices = community_choices, selected = VALID_COMMUNITY_CODES[1])
        ),
        conditionalPanel(
          "input.traj_view == 'species'",
          selectizeInput("traj_species", "Select Species:",
                          choices = setNames(species_map$code, species_map$dropdown_label),
                          multiple = TRUE,
                          selected = VALID_SPECIES_CODES)
        ),
        checkboxInput("traj_normalize", "Show as 100% stacked composition", value = FALSE),
        checkboxInput("traj_compare", "Compare with a second selection", value = FALSE),
        conditionalPanel(
          "input.traj_compare == true && input.traj_view == 'community'",
          selectInput("traj_community2", "Second community to compare:", choices = community_choices, selected = VALID_COMMUNITY_CODES[2])
        ),
        conditionalPanel(
          "input.traj_compare == true && input.traj_view == 'species'",
          selectizeInput("traj_species2", "Second species set to compare:",
                          choices = setNames(species_map$code, species_map$dropdown_label),
                          multiple = TRUE)
        ),
        helpText("Areas show the mean predicted probability of occurrence (spatial average across all plots) for the Current baseline and each future horizon × SSP scenario. When a community is selected, the lower panel also shows its gCSI trajectory with 95% credible intervals."),
        downloadButton("download_traj_csv", "Download trajectory data (CSV)")
      ),

      conditionalPanel(
        condition = "input.main_tabs == 'Hexagon Map'",
        h4("Hexagon Map (20 km grid)"),
        helpText("Aggregates the same plot-level predictions shown on the other maps into a 20 km hexagonal grid (mean value per hexagon), using the \"Select Climate Scenario\" chosen above. A colour-blind-friendly (viridis) palette is used throughout."),
        radioButtons("hex_mode", "Show:",
                     choices = c("Species occurrence probability" = "species",
                                 "Community suitability (CSI)" = "community")),
        conditionalPanel(
          "input.hex_mode == 'species'",
          selectizeInput("hex_species", "Select species:",
                          choices = setNames(species_map$code, species_map$dropdown_label),
                          selected = VALID_SPECIES_CODES[1])
        ),
        conditionalPanel(
          "input.hex_mode == 'community'",
          selectInput("hex_community", "Select community:", choices = community_choices, selected = VALID_COMMUNITY_CODES[1]),
          selectInput("hex_metric", "Suitability metric:",
                      choices = c("Basic CSI" = "bCSI", "Weighted CSI" = "wCSI", "Geometric Mean CSI" = "gCSI"))
        )
      )
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(id = "main_tabs",
        tabPanel("Community-Centric",
                 h3(textOutput("community_output_title")),
                 fluidRow(
                   column(6, h4("Selected Scenario"), .with_spinner(leafletOutput("csi_map", height = "500px"))),
                   column(6, conditionalPanel("input.comparison_scenario != 'None'", h4("Comparison Scenario"), .with_spinner(leafletOutput("csi_map_comparison", height = "500px"))))
                 ),
                 downloadButton("download_csv", "Download CSV"),
                 DT::dataTableOutput("csi_table")),
        tabPanel("Site-Centric",
                 h3(textOutput("site_output_title")),
                 .with_spinner(plotOutput("predicted_composition_plot"))),
        tabPanel("Climate Maps",
                 h4("Climate Variables for Selected Scenario"),
                 fluidRow(
                   column(6, .with_spinner(leafletOutput("map_mat", height = "400px"))),
                   column(6, .with_spinner(leafletOutput("map_map", height = "400px")))
                 )
        ),
        tabPanel("Trajectories",
                 h3("Trajectories from Baseline to Future Scenarios"),
                 p("Stacked-area view of how predicted occurrence probability changes from the current climate baseline (1991-2020) through three future horizons (2011-2040, 2041-2070, 2071-2100), under four SSP scenarios. Toggle between absolute probability (area height reflects overall community suitability) and 100% stacked composition (area height is always full, showing only the ", tags$em("relative"), " share of each species)."),
                 .with_spinner(plotOutput("traj_area_plot", height = "550px")),
                 conditionalPanel(
                   "input.traj_view == 'community'",
                   hr(),
                   h4("Community Suitability Index (gCSI) trajectory"),
                   .with_spinner(plotOutput("traj_csi_plot", height = "350px"))
                 ),
                 conditionalPanel(
                   "input.traj_compare == true",
                   hr(),
                   h4("Comparison"),
                   .with_spinner(plotOutput("traj_area_plot2", height = "550px")),
                   conditionalPanel(
                     "input.traj_view == 'community'",
                     .with_spinner(plotOutput("traj_csi_plot2", height = "350px"))
                   )
                 )
        ),
        tabPanel("Hexagon Map",
                 h3("Spatial Aggregation into a 20 km Hexagonal Grid"),
                 p("Same underlying plot-level predictions as the other maps, aggregated into 20 km hexagons (mean value per hexagon) for a less noisy, more readable view of broad spatial patterns. Hexagons with no inventory plots are left blank. Uses the colour-blind-friendly ", tags$em("viridis"), " palette. Set \"Select Comparison Scenario\" in Global Settings (sidebar) to see a second scenario side by side."),
                 fluidRow(
                   column(6, h4("Selected Scenario"), uiOutput("hex_map_heading"), .with_spinner(leafletOutput("hex_map", height = "600px"))),
                   column(6, conditionalPanel("input.comparison_scenario != 'None'", h4("Comparison Scenario"), uiOutput("hex_map_heading_comparison"), .with_spinner(leafletOutput("hex_map_comparison", height = "600px"))))
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
                   tags$li("Compare scenarios and perspectives (community vs. site-focused)."),
                   tags$li("Toggle posterior draws for full uncertainty (disabled by default for faster loading)."),
                   tags$li("Progress bars display computation status when draws are enabled.")
                 ),
                 hr(),
                 h4("2. How to Use the App"),
                 p("Follow these steps to explore the data:"),
                 tags$ol(
                   tags$li(tags$strong("Select a Climate Scenario:"), "Choose from current conditions or future projections (e.g., different GCMs like 'SSP3 7.0 - 2041_2070'). This affects all predictions."),
                   tags$li(tags$strong("(Optional) Enable Posterior Draws:"), "Check 'Use posterior draws (slower)' only if you need exact credible intervals; leave unchecked for rapid exploration."),
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
                 p("Credible intervals use posterior draws when the checkbox is enabled; otherwise, faster summary-based approximations are shown (less exact for non-linear metrics like gCSI)."),
                 hr(),
                 h4("3. Posterior Draws & Performance"),
                 p("Posterior draws provide precise uncertainty but increase computation time. The app defaults to the faster summary mode."),
                 tags$ul(
                   tags$li("Enable the 'Use posterior draws (slower)' checkbox to compute CSI per draw."),
                   tags$li("A progress bar tracks draw processing and summarisation."),
                   tags$li("If a scenario lacks a draw file, the app falls back automatically and notifies you."),
                   tags$li("Disable the checkbox again to regain speed for exploratory comparisons.")
                 ),
                 hr(),
                 h4("4. Understanding the Suitability Metrics (CSI)"),
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
                 h4("5. Tips and Troubleshooting"),
                 tags$ul(
                   tags$li("Start with 'Current' scenario to understand baselines."),
                   tags$li("For custom communities, select 2-5 species to avoid overload. Note that the model was trained with a limited number of species. The names of non trained species are available for future improvements."),
                   tags$li("Maps show interpolated data; click points for exact values."),
                   tags$li("If no data appears, check selections and ensure the model ran successfully."),
                   tags$li("Contact developers for model details or data sources.\n João Paulo Czarnecki de Liz \n jpczd@ulaval.ca")
                 ),
                 hr(),
                 h4("6. Methodological Details"),
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

  # --- Restrict species/community selections to the validated scope ---
  # Ghosted options are still visible (and, for plain HTML selects, technically
  # clickable), so this is the functional half of the restriction: any selection
  # outside VALID_SPECIES_CODES/VALID_COMMUNITY_CODES is immediately reverted and
  # the user is warned it's still in development. Combined with the ghosted
  # dropdown labels built above, and the defensive filtering inside
  # target_community_species()/resolve_traj_species_codes() below, this ensures
  # no not-yet-validated species or community can actually drive a computation.
  ghost_notice <- function(what) {
    showNotification(
      paste0("\"", what, "\" is still under development and hasn't been validated for this app yet. Please choose one of the available (non-greyed) options."),
      type = "warning", duration = 6
    )
  }

  enforce_valid_species <- function(input_id) {
    observeEvent(input[[input_id]], {
      current <- input[[input_id]]
      invalid <- setdiff(current, VALID_SPECIES_CODES)
      if (length(invalid)) {
        bad_labels <- species_map$display_name[match(invalid, species_map$code)]
        ghost_notice(paste(bad_labels[!is.na(bad_labels)], collapse = ", "))
        updateSelectizeInput(session, input_id, selected = intersect(current, VALID_SPECIES_CODES))
      }
    }, ignoreNULL = FALSE)
  }
  enforce_valid_species("custom_species")
  enforce_valid_species("traj_species")
  enforce_valid_species("traj_species2")

  enforce_valid_species_single <- function(input_id, default_code = VALID_SPECIES_CODES[1]) {
    last_valid <- reactiveVal(default_code)
    observeEvent(input[[input_id]], {
      current <- input[[input_id]]
      if (is.null(current) || !nzchar(current)) return(invisible(NULL))
      if (current %in% VALID_SPECIES_CODES) {
        last_valid(current)
      } else {
        bad_label <- species_map$display_name[match(current, species_map$code)]
        ghost_notice(if (length(bad_label) && !is.na(bad_label)) bad_label else current)
        updateSelectizeInput(session, input_id, selected = last_valid())
      }
    }, ignoreNULL = FALSE, ignoreInit = TRUE)
  }
  enforce_valid_species_single("hex_species")

  enforce_valid_community <- function(input_id, is_selectize, default_code = VALID_COMMUNITY_CODES[1]) {
    last_valid <- reactiveVal(default_code)
    observeEvent(input[[input_id]], {
      current <- input[[input_id]]
      if (is.null(current) || !nzchar(current)) return(invisible(NULL))
      if (current %in% VALID_COMMUNITY_CODES) {
        last_valid(current)
      } else {
        ghost_notice(find_type_eco_description(current))
        updater <- if (is_selectize) updateSelectizeInput else updateSelectInput
        updater(session, input_id, selected = last_valid())
      }
    }, ignoreNULL = FALSE, ignoreInit = TRUE)
  }
  enforce_valid_community("type_eco_choice", is_selectize = TRUE, default_code = VALID_COMMUNITY_CODES[1])
  enforce_valid_community("traj_community", is_selectize = FALSE, default_code = VALID_COMMUNITY_CODES[1])
  enforce_valid_community("traj_community2", is_selectize = FALSE, default_code = VALID_COMMUNITY_CODES[2])
  enforce_valid_community("hex_community", is_selectize = FALSE, default_code = VALID_COMMUNITY_CODES[1])

  # --- Reactive Data ---
  preds_summary <- reactive({
    all_predictions_summary[[input$climate_scenario]]
  })

  scenario_draws <- reactive({
    if (!isTRUE(input$use_draws)) {
      message("[TCAMS] Draws disabled by user; using summary predictions for scenario: ", input$climate_scenario)
      return(NULL)
    }
    drv <- load_draws_for_scenario(input$climate_scenario)
    if (is.null(drv)) {
      message("[TCAMS] No draws found; falling back to summary for scenario: ", input$climate_scenario)
    } else {
      message("[TCAMS] Draws loaded for scenario: ", input$climate_scenario, " dims=", paste(dim(drv), collapse="x"))
    }
    drv
  })

  current_climate_data <- reactive({
    XData_future_list[[input$climate_scenario]] %>%
      as_tibble(rownames = "plot_id") %>%
      left_join(plots_for_map, by = "plot_id")
  })

  # --- Community-Centric Logic ---
  target_community_species <- reactive({
    spp <- if (input$community_method == "type_eco") {
      if (isTRUE(input$type_eco_choice %in% VALID_COMMUNITY_CODES)) TYPE_ECO_list[[input$type_eco_choice]] else character(0)
    } else {
      species_map$original_name_in_data[match(input$custom_species, species_map$code)]
    }
    intersect(spp, VALID_ORIGINAL_NAMES)
  })

  output$type_eco_desc <- renderText({
    req(input$type_eco_choice)
    desc <- find_type_eco_description(input$type_eco_choice)
    ifelse(is.na(desc), "", desc)
  })

  output$species_weights_ui <- renderUI({
    req(input$csi_metric == "wCSI")
    species <- target_community_species()
    map(species, ~ numericInput(paste0("weight_", .x), label = species_map$display_name[species_map$original_name_in_data == .x], value = 1, min = 0, max = 100))
  })

  csi_results <- eventReactive(input$run_community_analysis, {
    community_spp <- target_community_species()
    req(length(community_spp) > 0)
    preds_obj <- preds_summary()
    pred_df <- preds_obj$mean
    plot_ids <- rownames(pred_df)
    metric <- input$csi_metric
    user_weights <- NULL
    if (metric == "wCSI") {
      user_weights <- setNames(vapply(community_spp, function(x) {
        val <- input[[paste0("weight_", x)]]
        if (is.null(val)) 1 else val
      }, numeric(1)), community_spp)
    }

    draws_obj <- scenario_draws()
    use_draws <- FALSE
    summary_tbl <- NULL

    summary_tbl <- withProgress(message = "Calculating community suitability", value = 0, {
      if (!is.null(draws_obj)) {
        n_draws <- dim(draws_obj)[1]
        progress_fun <- function(d, n) incProgress(0.8 / n, detail = paste("Processing draw", d, "of", n))
        csi_draws <- compute_csi_per_draw_site(draws_obj, community_spp, metric, user_weights, eps = epsilon, progress_fun = progress_fun)
        if (nrow(csi_draws)) {
          use_draws <<- TRUE
          incProgress(0.1, detail = "Summarising draws")
          site_summary <- summarise_csi_over_draws(csi_draws, prob = hdi_prob, central = "median") %>% rename(plot_id = site)
          tibble(plot_id = plot_ids) %>% left_join(site_summary, by = "plot_id")
        } else {
          NULL
        }
      } else {
        incProgress(0.3, detail = "Using summary predictions")
        NULL
      }
    })

    if (is.null(summary_tbl)) {
      shiny::showNotification(
        "Draws not found or disabled; using summary-based approximation. Credible intervals may be less accurate for non-linear metrics (e.g., gCSI).",
        type = if (is.null(draws_obj) || !use_draws) "message" else "warning", duration = 7
      )
      mean_csi <- apply(pred_df, 1, function(p) calculate_csi(community_spp, p, metric, user_weights))
      lower_csi <- apply(preds_obj$lower, 1, function(p) calculate_csi(community_spp, p, metric, user_weights))
      upper_csi <- apply(preds_obj$upper, 1, function(p) calculate_csi(community_spp, p, metric, user_weights))
      summary_tbl <- tibble(
        plot_id = plot_ids,
        center = mean_csi,
        mean = mean_csi,
        sd = NA_real_,
        hdi_lower = lower_csi,
        hdi_upper = upper_csi,
        q025 = lower_csi,
        q975 = upper_csi
      )
    }

    if (!"sd" %in% names(summary_tbl)) summary_tbl$sd <- NA_real_
    if (!"q025" %in% names(summary_tbl)) summary_tbl$q025 <- summary_tbl$hdi_lower
    if (!"q975" %in% names(summary_tbl)) summary_tbl$q975 <- summary_tbl$hdi_upper
    summary_tbl$draws_used <- use_draws
    summary_tbl$metric <- metric
    summary_tbl$center <- ifelse(is.na(summary_tbl$center) & !is.na(summary_tbl$mean), summary_tbl$mean, summary_tbl$center)
    summary_tbl$mean <- ifelse(is.na(summary_tbl$mean) & !is.na(summary_tbl$center), summary_tbl$center, summary_tbl$mean)
    summary_tbl$ci_width <- summary_tbl$hdi_upper - summary_tbl$hdi_lower
    summary_tbl$lower <- summary_tbl$hdi_lower
    summary_tbl$upper <- summary_tbl$hdi_upper

    summary_tbl %>%
      left_join(plots_for_map, by = "plot_id") %>%
      left_join(current_climate_data() %>% select(plot_id, MAP, MAT, pH, Clay, OM, rtp, twi, slope, drainage_class), by = "plot_id")
  })

  csi_results_comparison <- eventReactive(input$run_community_analysis, {
    req(input$comparison_scenario != "None")
    community_spp <- target_community_species()
    req(length(community_spp) > 0)

    preds_obj <- all_predictions_summary[[input$comparison_scenario]]
    pred_df <- preds_obj$mean
    plot_ids <- rownames(pred_df)
    metric <- input$csi_metric

    user_weights <- NULL
    if (metric == "wCSI") {
      user_weights <- setNames(vapply(community_spp, function(x) {
        val <- input[[paste0("weight_", x)]]
        if (is.null(val)) 1 else val
      }, numeric(1)), community_spp)
    }

    draws_obj <- if (isTRUE(input$use_draws)) load_draws_for_scenario(input$comparison_scenario) else NULL
    use_draws <- FALSE
    summary_tbl <- NULL

    summary_tbl <- withProgress(message = "Calculating comparison suitability", value = 0, {
      if (!is.null(draws_obj)) {
        n_draws <- dim(draws_obj)[1]
        progress_fun <- function(d, n) incProgress(0.8 / n, detail = paste("Draw", d, "of", n))
        csi_draws <- compute_csi_per_draw_site(draws_obj, community_spp, metric, user_weights, eps = epsilon, progress_fun = progress_fun)
        if (nrow(csi_draws)) {
          use_draws <<- TRUE
          incProgress(0.1, detail = "Summarising draws")
          site_summary <- summarise_csi_over_draws(csi_draws, prob = hdi_prob, central = "median") %>% rename(plot_id = site)
          tibble(plot_id = plot_ids) %>% left_join(site_summary, by = "plot_id")
        } else NULL
      } else {
        incProgress(0.3, detail = "Using summary predictions")
        NULL
      }
    })

    if (is.null(summary_tbl)) {
      mean_csi <- apply(pred_df, 1, function(p) calculate_csi(community_spp, p, metric, user_weights))
      lower_csi <- apply(preds_obj$lower, 1, function(p) calculate_csi(community_spp, p, metric, user_weights))
      upper_csi <- apply(preds_obj$upper, 1, function(p) calculate_csi(community_spp, p, metric, user_weights))
      summary_tbl <- tibble(
        plot_id = plot_ids,
        center = mean_csi,
        mean = mean_csi,
        sd = NA_real_,
        hdi_lower = lower_csi,
        hdi_upper = upper_csi,
        q025 = lower_csi,
        q975 = upper_csi
      )
    }

    if (!"sd" %in% names(summary_tbl)) summary_tbl$sd <- NA_real_
    if (!"q025" %in% names(summary_tbl)) summary_tbl$q025 <- summary_tbl$hdi_lower
    if (!"q975" %in% names(summary_tbl)) summary_tbl$q975 <- summary_tbl$hdi_upper
    summary_tbl$draws_used <- use_draws
    summary_tbl$metric <- metric
    summary_tbl$center <- ifelse(is.na(summary_tbl$center) & !is.na(summary_tbl$mean), summary_tbl$mean, summary_tbl$center)
    summary_tbl$mean <- ifelse(is.na(summary_tbl$mean) & !is.na(summary_tbl$center), summary_tbl$center, summary_tbl$mean)
    summary_tbl$ci_width <- summary_tbl$hdi_upper - summary_tbl$hdi_lower
    summary_tbl$lower <- summary_tbl$hdi_lower
    summary_tbl$upper <- summary_tbl$hdi_upper

    summary_tbl %>%
      left_join(plots_for_map, by = "plot_id") %>%
      left_join((XData_future_list[[input$comparison_scenario]] %>%
                   as_tibble(rownames = "plot_id") %>%
                   left_join(plots_for_map, by = "plot_id") %>%
                   select(plot_id, MAP, MAT, pH, Clay, OM, rtp, twi, slope, drainage_class)), by = "plot_id")
  })

  output$community_output_title <- renderText({
    if (input$comparison_scenario != "None") {
      paste("Community Suitability: ", input$climate_scenario, " vs ", input$comparison_scenario)
    } else {
      paste("Community Suitability for:", input$climate_scenario)
    }
  })

  output$csi_map <- renderLeaflet({
    req(csi_results())
    df <- csi_results()
    pal <- colorNumeric(palette = "viridis", domain = df$center, na.color = "transparent")
    leaflet(df) %>%
      addProviderTiles(providers[[input$basemap]]) %>%
      addCircleMarkers(lng = ~longitude, lat = ~latitude, color = ~pal(center),
                       radius = 5, stroke = FALSE, fillOpacity = 0.8,
                       layerId = ~plot_id,
                       popup = ~paste(
                         "Plot:", plot_id,
                         "<br>CSI (center):", sprintf("%.3f", center),
                         "<br>HDI ", sprintf("%.0f", hdi_prob * 100), "%:", sprintf("%.3f - %.3f", hdi_lower, hdi_upper),
                         "<br>q2.5-q97.5:", sprintf("%.3f - %.3f", q025, q975)
                       )) %>%
      addLegend("bottomright", pal = pal, values = ~center, title = "Mean CSI")
  })

  output$csi_map_comparison <- renderLeaflet({
    req(csi_results_comparison())
    df <- csi_results_comparison()
    pal <- colorNumeric(palette = "viridis", domain = df$center, na.color = "transparent")
    leaflet(df) %>%
      addProviderTiles(providers[[input$basemap]]) %>%
      addCircleMarkers(lng = ~longitude, lat = ~latitude, color = ~pal(center),
                       radius = 5, stroke = FALSE, fillOpacity = 0.8,
                       layerId = ~plot_id,
                       popup = ~paste(
                         "Plot:", plot_id,
                         "<br>CSI (center):", sprintf("%.3f", center),
                         "<br>HDI ", sprintf("%.0f", hdi_prob * 100), "%:", sprintf("%.3f - %.3f", hdi_lower, hdi_upper),
                         "<br>q2.5-q97.5:", sprintf("%.3f - %.3f", q025, q975)
                       )) %>%
      addLegend("bottomright", pal = pal, values = ~center, title = "Mean CSI")
  })

  output$csi_table <- DT::renderDataTable({
    req(csi_results())
    csi_results() %>%
      select(plot_id, metric, draws_used, latitude, longitude, MAP, MAT, pH, Clay, OM, rtp, twi, slope, drainage_class,
             center, mean, hdi_lower, hdi_upper, q025, q975, ci_width) %>%
      arrange(desc(center)) %>%
      mutate(across(where(is.numeric), ~round(., 3))) %>%
      DT::datatable(options = list(pageLength = 5), rownames = FALSE)
  })

  # Download handler for CSV
  output$download_csv <- downloadHandler(
    filename = function() {
      paste("csi_results_", input$climate_scenario, "_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      export_df <- csi_results() %>%
        select(plot_id, metric, draws_used, center, mean, hdi_lower, hdi_upper, q025, q975, ci_width,
               latitude, longitude, MAP, MAT, pH, Clay, OM, rtp, twi, slope, drainage_class, everything())
      write.csv(export_df, file, row.names = FALSE)
    }
  )

  # --- Site-Centric Logic ---
  selected_plot_preds <- reactive({
    req(input$plot_choice, input$climate_scenario, cancelOutput = TRUE)
    preds <- preds_summary()$mean
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

  selected_plot_posterior <- reactive({
    req(input$plot_choice, cancelOutput = TRUE)
    draws <- scenario_draws()
    if (is.null(draws)) return(NULL)
    site_ids <- dimnames(draws)[[2]]
    if (!(input$plot_choice %in% site_ids)) return(NULL)
    posterior_mat <- draws[, input$plot_choice, ]
    if (is.null(dim(posterior_mat))) {
      species_names <- dimnames(draws)[[3]]
      col_name <- if (length(species_names)) species_names[1] else "species"
      posterior_mat <- matrix(posterior_mat, ncol = 1)
      colnames(posterior_mat) <- col_name
    } else if (is.null(colnames(posterior_mat))) {
      species_names <- dimnames(draws)[[3]]
      if (length(species_names) >= ncol(posterior_mat)) {
        colnames(posterior_mat) <- species_names[seq_len(ncol(posterior_mat))]
      }
    }
    as_tibble(posterior_mat) %>%
      mutate(draw = row_number(), .before = 1) %>%
      pivot_longer(-draw, names_to = "original_name_in_data", values_to = "prob") %>%
      filter(!is.na(prob))
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
    posterior_df <- selected_plot_posterior()
    if (!is.null(posterior_df) && nrow(posterior_df) > 0) {
      summary_tbl <- posterior_df %>%
        left_join(species_map, by = "original_name_in_data") %>%
        mutate(display_name = coalesce(display_name, original_name_in_data)) %>%
        group_by(original_name_in_data, display_name) %>%
        summarise(
          center = median(prob, na.rm = TRUE),
          mean = mean(prob, na.rm = TRUE),
          q025 = quantile(prob, 0.025, na.rm = TRUE, names = FALSE),
          q975 = quantile(prob, 0.975, na.rm = TRUE, names = FALSE),
          .groups = "drop"
        ) %>%
        arrange(desc(center)) %>%
        slice_head(n = 20)

      req(nrow(summary_tbl) > 0)
      plot_data <- posterior_df %>%
        filter(original_name_in_data %in% summary_tbl$original_name_in_data) %>%
        left_join(summary_tbl %>% select(original_name_in_data, center), by = "original_name_in_data") %>%
        left_join(species_map, by = "original_name_in_data") %>%
        mutate(display_name = coalesce(display_name, original_name_in_data)) %>%
        mutate(display_name = reorder(display_name, center))

      summary_tbl <- summary_tbl %>%
        mutate(display_name = factor(display_name, levels = levels(plot_data$display_name)))

      ggplot(plot_data, aes(x = display_name, y = prob)) +
        geom_violin(fill = "steelblue", color = NA, alpha = 0.6, scale = "width") +
        geom_linerange(data = summary_tbl, aes(x = display_name, ymin = q025, ymax = q975), inherit.aes = FALSE, color = "navy", size = 1) +
        geom_point(data = summary_tbl, aes(x = display_name, y = center), inherit.aes = FALSE, color = "navy", size = 2) +
        coord_flip() +
        labs(x = "Species", y = "Probability of Occurrence", title = "Posterior distribution for top 20 species") +
        theme_minimal(base_size = 14) +
        theme(axis.text.y = element_text(face = "italic"))
    } else {
      preds <- selected_plot_preds()
      req(!is.null(preds), nrow(preds) > 0)

      plot_data <- as.data.frame(preds) %>%
        pivot_longer(everything(), names_to = "original_name_in_data", values_to = "prob") %>%
        left_join(species_map, by = "original_name_in_data") %>%
        mutate(display_name = coalesce(display_name, original_name_in_data)) %>%
        filter(!is.na(prob)) %>%
        arrange(desc(prob)) %>%
        slice_head(n = 20)

      ggplot(plot_data, aes(x = reorder(display_name, prob), y = prob)) +
        geom_col(fill = "steelblue") +
        coord_flip() +
        labs(x = "Species", y = "Probability of Occurrence", title = "Top 20 Most Probable Species") +
        theme_minimal(base_size = 14) +
        theme(axis.text.y = element_text(face = "italic"))
    }
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
    pal <- colorNumeric(palette = "viridis", domain = global_map_range, na.color = "transparent")
    leaflet(df) %>% addProviderTiles(providers[[input$basemap]]) %>%
      addCircleMarkers(lng = ~longitude, lat = ~latitude, color = ~pal(MAP),
                       radius = 5, stroke = FALSE, fillOpacity = 0.8,
                       popup = ~paste("Plot:", plot_id, "<br>MAP:", round(MAP, 0), "mm")) %>%
      addLegend("bottomright", pal = pal, values = ~MAP, title = "MAP (mm)")
  })

  # --- Hexagon Map Tab ---
  # The hexagon grid only depends on plot locations, which are fixed across
  # scenarios/species, so it's built once per session and reused.
  hex_grid_sf <- reactive({
    pts <- plots_for_map %>% dplyr::filter(!is.na(longitude), !is.na(latitude))
    req(nrow(pts) > 0)
    pts_sf <- sf::st_as_sf(pts, coords = c("longitude", "latitude"), crs = 4326) %>% sf::st_transform(HEX_CRS)
    grid <- sf::st_make_grid(pts_sf, cellsize = HEX_CELLSIZE_M, square = FALSE)
    sf::st_sf(hex_id = seq_along(grid), geometry = grid)
  })

  # Plot-level values (+ a legend title) for whichever metric is currently
  # selected, for a given scenario label. Reuses the same summary predictions
  # and CSI logic as the other tabs (species probability from
  # all_predictions_summary; community CSI via the same calculate_csi() used
  # by the Community-Centric tab). Parametrised by scenario so the same logic
  # drives both the primary and the side-by-side comparison map.
  # Note the split between `legend_title` (kept short — just the metric name —
  # so the on-map leaflet legend stays small) and `heading` (the full
  # species/community + scenario description, shown as page text above the
  # map instead, where there's room to wrap normally).
  compute_hex_values_for_scenario <- function(scenario_label) {
    preds_obj <- all_predictions_summary[[scenario_label]]
    req(!is.null(preds_obj), !is.null(preds_obj$mean))
    pred_df <- preds_obj$mean
    plot_ids <- rownames(pred_df)

    if (identical(input$hex_mode, "community")) {
      req(input$hex_community %in% VALID_COMMUNITY_CODES, input$hex_metric)
      community_spp <- intersect(TYPE_ECO_list[[input$hex_community]], VALID_ORIGINAL_NAMES)
      req(length(community_spp) > 0)
      values <- apply(pred_df, 1, function(p) calculate_csi(community_spp, p, input$hex_metric, NULL))
      legend_title <- input$hex_metric
      subject <- find_type_eco_description(input$hex_community)
    } else {
      req(input$hex_species %in% VALID_SPECIES_CODES)
      orig_col <- species_map$original_name_in_data[match(input$hex_species, species_map$code)]
      req(length(orig_col) == 1, orig_col %in% colnames(pred_df))
      values <- pred_df[, orig_col]
      legend_title <- "Probability"
      subject <- paste0(species_map$display_name[match(input$hex_species, species_map$code)], " — occurrence probability")
    }
    heading <- tags$span(tags$strong(subject), tags$br(), tags$span(style = "color:#6c757d;", scenario_label))
    list(plot_ids = plot_ids, values = as.numeric(values), legend_title = legend_title, heading = heading)
  }

  aggregate_hex_values <- function(hv) {
    pts_sf <- build_hex_point_sf(hv$plot_ids, hv$values)
    req(!is.null(pts_sf))
    grid <- hex_grid_sf()
    joined <- sf::st_join(pts_sf, grid)
    agg <- joined %>%
      sf::st_drop_geometry() %>%
      dplyr::filter(!is.na(hex_id)) %>%
      dplyr::group_by(hex_id) %>%
      dplyr::summarise(mean_value = mean(value, na.rm = TRUE), n_plots = dplyr::n(), .groups = "drop")
    req(nrow(agg) > 0)
    grid %>% dplyr::inner_join(agg, by = "hex_id") %>% sf::st_transform(4326)
  }

  render_hex_leaflet <- function(hex_sf, legend_title) {
    pal <- colorNumeric(palette = "viridis", domain = hex_sf$mean_value, na.color = "transparent")
    leaflet(hex_sf) %>%
      addProviderTiles(providers[[input$basemap]]) %>%
      addPolygons(fillColor = ~pal(mean_value), color = "white", weight = 0.6, fillOpacity = 0.8,
                  popup = ~paste0("Mean value: ", sprintf("%.3f", mean_value), "<br>Plots in hexagon: ", n_plots),
                  highlightOptions = highlightOptions(weight = 2, color = "#2c3e50", bringToFront = TRUE)) %>%
      addLegend("bottomright", pal = pal, values = ~mean_value, title = legend_title, opacity = 0.9)
  }

  hex_values <- reactive(compute_hex_values_for_scenario(input$climate_scenario))
  hex_aggregated <- reactive(aggregate_hex_values(hex_values()))
  output$hex_map <- renderLeaflet({
    hex_sf <- hex_aggregated()
    render_hex_leaflet(hex_sf, hex_values()$legend_title)
  })
  output$hex_map_heading <- renderUI(hex_values()$heading)

  hex_values_comparison <- reactive({
    req(input$comparison_scenario, input$comparison_scenario != "None")
    compute_hex_values_for_scenario(input$comparison_scenario)
  })
  hex_aggregated_comparison <- reactive(aggregate_hex_values(hex_values_comparison()))
  output$hex_map_comparison <- renderLeaflet({
    hex_sf <- hex_aggregated_comparison()
    render_hex_leaflet(hex_sf, hex_values_comparison()$legend_title)
  })
  output$hex_map_heading_comparison <- renderUI(hex_values_comparison()$heading)

  # --- Trajectories Tab ---
  # Resolve the original data-column species codes (e.g. "ERR_n") for either a
  # predefined community or a custom species selection.
  resolve_traj_species_codes <- function(view, community_code, species_codes) {
    spp <- if (identical(view, "community")) {
      if (is.null(community_code) || !nzchar(community_code) || !(community_code %in% VALID_COMMUNITY_CODES)) {
        character(0)
      } else {
        TYPE_ECO_list[[community_code]]
      }
    } else {
      if (is.null(species_codes) || !length(species_codes)) return(character(0))
      species_map$original_name_in_data[match(species_codes, species_map$code)]
    }
    intersect(spp, VALID_ORIGINAL_NAMES)
  }

  build_traj_area_data <- function(spp) {
    if (!nrow(species_traj_summary) || !length(spp)) return(tibble())
    df <- species_traj_summary %>% filter(species %in% spp, !is.na(horizon), !is.na(ssp))
    if (!nrow(df)) return(df)
    replicate_baseline_across_ssp(df)
  }

  render_traj_area_plot <- function(df, normalize, title_txt) {
    validate(need(nrow(df) > 0, "No data available for this selection."))
    stack_pos <- if (normalize) "fill" else "stack"
    y_lab <- if (normalize) {
      "Relative contribution to total predicted probability"
    } else {
      "Mean predicted probability of occurrence\n(spatial average across plots)"
    }

    p <- ggplot(df, aes(x = horizon, y = mean, fill = display_name, group = display_name)) +
      geom_area(position = stack_pos, alpha = 0.9, colour = "white", linewidth = 0.15) +
      facet_wrap(~ ssp, nrow = 1) +
      scale_fill_viridis_d(option = "turbo") +
      theme_minimal(base_size = 13) +
      theme(
        legend.text = element_text(face = "italic"),
        axis.text.x = element_text(angle = 30, hjust = 1),
        strip.text = element_text(face = "bold")
      ) +
      labs(x = NULL, y = y_lab, fill = "Species", title = title_txt)

    if (normalize) p <- p + scale_y_continuous(labels = scales::percent)
    p
  }

  render_traj_csi_plot <- function(community_code) {
    validate(need(!is.null(community_code) && nzchar(community_code), "Select a community to see its gCSI trajectory."))
    validate(need(nrow(community_traj_summary) > 0, "Community CSI trajectory data not available."))
    df <- community_traj_summary %>% filter(community_prefix == community_code, !is.na(horizon), !is.na(ssp))
    validate(need(nrow(df) > 0, "No gCSI trajectory data for this community."))
    plot_df <- replicate_baseline_across_ssp(df)

    ggplot(plot_df, aes(x = horizon, y = gCenter, ymin = gQ025, ymax = gQ975, group = ssp, color = ssp, fill = ssp)) +
      geom_ribbon(alpha = 0.15, colour = NA) +
      geom_line(linewidth = 1) +
      geom_point(size = 2) +
      facet_wrap(~ ssp, nrow = 1) +
      scale_color_viridis_d(option = "turbo") +
      scale_fill_viridis_d(option = "turbo") +
      theme_minimal(base_size = 13) +
      theme(legend.position = "none", axis.text.x = element_text(angle = 30, hjust = 1), strip.text = element_text(face = "bold")) +
      labs(x = NULL, y = "Geometric Community Suitability Index (gCSI)",
           title = paste0("gCSI trajectory (mean, 95% credible interval) — ", find_type_eco_description(community_code)))
  }

  traj_title <- function(view, community_code, prefix = "") {
    if (identical(view, "community")) {
      paste0(prefix, "Species composition — ", find_type_eco_description(community_code), " (", community_code, ")")
    } else {
      paste0(prefix, "Selected species: predicted occurrence probability")
    }
  }

  # --- Primary selection ---
  traj_species_codes <- reactive({
    resolve_traj_species_codes(input$traj_view, input$traj_community, input$traj_species)
  })
  traj_area_data <- reactive({
    req(length(traj_species_codes()) > 0)
    build_traj_area_data(traj_species_codes())
  })
  output$traj_area_plot <- renderPlot({
    render_traj_area_plot(traj_area_data(), isTRUE(input$traj_normalize), traj_title(input$traj_view, input$traj_community))
  })
  output$traj_csi_plot <- renderPlot({
    req(identical(input$traj_view, "community"))
    render_traj_csi_plot(input$traj_community)
  })

  # --- Comparison (second) selection ---
  traj_species_codes2 <- reactive({
    req(isTRUE(input$traj_compare))
    resolve_traj_species_codes(input$traj_view, input$traj_community2, input$traj_species2)
  })
  traj_area_data2 <- reactive({
    req(length(traj_species_codes2()) > 0)
    build_traj_area_data(traj_species_codes2())
  })
  output$traj_area_plot2 <- renderPlot({
    render_traj_area_plot(traj_area_data2(), isTRUE(input$traj_normalize), traj_title(input$traj_view, input$traj_community2, "Comparison: "))
  })
  output$traj_csi_plot2 <- renderPlot({
    req(isTRUE(input$traj_compare), identical(input$traj_view, "community"))
    render_traj_csi_plot(input$traj_community2)
  })

  # --- Download trajectory data behind the current plot(s) ---
  output$download_traj_csv <- downloadHandler(
    filename = function() paste0("tcams_trajectories_", Sys.Date(), ".csv"),
    content = function(file) {
      primary <- traj_area_data() %>% mutate(selection = "primary")
      out <- primary
      if (isTRUE(input$traj_compare) && length(traj_species_codes2()) > 0) {
        secondary <- build_traj_area_data(traj_species_codes2()) %>% mutate(selection = "comparison")
        out <- dplyr::bind_rows(primary, secondary)
      }
      readr::write_csv(out, file)
    }
  )
}

# --- Run the App ---
shinyApp(ui = ui, server = server)
# --- End of app.R ---
