############################################################################
######### Function for computing Mechanisms tables #########################
############################################################################

######################### Helper function #########################################
# Define a function to recode and create new variables
recode_and_generate <- function(df, columns_list) {
  df <- df %>%
    mutate(across(all_of(columns_list), ~ ifelse(. == 2, 0, .))) %>%   # Recode 2 -> 0
    mutate(across(all_of(columns_list), ~ ifelse(. == 97, NA, .))) %>% # Recode 97 -> NA
    mutate(across(all_of(columns_list), list(rec0 = ~ replace_na(., 0)))) %>%   # recode missing as 0
    mutate(across(all_of(columns_list), list(rec1 = ~ replace_na(., 1)))) # recode missing as 1
  return(df)
}


######################## HOLM-BONFERRONI CORRECTION FUNCTION ##########################################
compute_holm_rows_PI <- function(mainResults,
                                       depvar_binary,
                                       depvar_index) {
  # ---- internal helper: get p-value for a given coefficient in a felm model ----
  get_p_for_coef <- function(mod, coef_name) {
    if (is.null(mod)) return(NA_real_)
    sm <- tryCatch(summary(mod), error = function(e) NULL)
    if (is.null(sm) || is.null(sm$coefficients)) return(NA_real_)
    
    cf <- sm$coefficients
    if (!coef_name %in% rownames(cf)) return(NA_real_)
    
    p_col <- intersect(c("Pr(>|t|)", "Pr(>F)", "p.value"), colnames(cf))
    if (length(p_col) == 0) return(NA_real_)
    
    as.numeric(cf[coef_name, p_col[1]])
  }
  
  # ---- internal helper: format adjusted p with stars ----
  format_adjusted_p <- function(p_value, add_stars = TRUE) {
    if (is.na(p_value)) return("—")
    
    formatted <- sprintf("%.3f", p_value)
    
    if (add_stars) {
      if (p_value < 0.01) {
        formatted <- paste0(formatted, "***")
      } else if (p_value < 0.05) {
        formatted <- paste0(formatted, "**")
      } else if (p_value < 0.10) {
        formatted <- paste0(formatted, "*")
      }
    }
    formatted
  }
  
  # ---------- restrict to EI Inclusion (3 arms) ----------
  pi_3arms_bin <- mainResults %>%
    dplyr::filter(curr_treat_var == "treatment_pi",
                  depvar == depvar_binary) %>%
    dplyr::slice(1)
  
  pi_3arms_idx <- mainResults %>%
    dplyr::filter(curr_treat_var == "treatment_pi",
                  depvar == depvar_index) %>%
    dplyr::slice(1)
  
  if (nrow(pi_3arms_bin) == 0 || nrow(pi_3arms_idx) == 0) {
    warning("No PI 3-arms rows found in mainResults for the specified depvars.")
    return(NULL)
  }

  # Models for each column:
  # (1) binary, no controls
  m_bin_no   <- pi_3arms_bin$results_no_controls[[1]]
  # (2) binary, with controls
  m_bin_with <- pi_3arms_bin$results_with_controls[[1]]
  # (3) index, no controls
  m_idx_no   <- pi_3arms_idx$results_no_controls[[1]]
  # (4) index, with controls
  m_idx_with <- pi_3arms_idx$results_with_controls[[1]]
  
  # ---------- p-values for Capital / Psychosocial / Full in each column ----------
  p_col1 <- c(
    Capital      = get_p_for_coef(m_bin_no,   "treatment_piCapital"),
    Psychosocial = get_p_for_coef(m_bin_no,   "treatment_piPsychosocial"),
    Full         = get_p_for_coef(m_bin_no,   "treatment_piFull")
  )
  
  p_col2 <- c(
    Capital      = get_p_for_coef(m_bin_with, "treatment_piCapital"),
    Psychosocial = get_p_for_coef(m_bin_with, "treatment_piPsychosocial"),
    Full         = get_p_for_coef(m_bin_with, "treatment_piFull")
  )
  
  p_col3 <- c(
    Capital      = get_p_for_coef(m_idx_no,   "treatment_piCapital"),
    Psychosocial = get_p_for_coef(m_idx_no,   "treatment_piPsychosocial"),
    Full         = get_p_for_coef(m_idx_no,   "treatment_piFull")
  )
  
  p_col4 <- c(
    Capital      = get_p_for_coef(m_idx_with, "treatment_piCapital"),
    Psychosocial = get_p_for_coef(m_idx_with, "treatment_piPsychosocial"),
    Full         = get_p_for_coef(m_idx_with, "treatment_piFull")
  )
  
  # ---------- Holm per column (3 tests per column) ----------
  p_holm_col1 <- stats::p.adjust(p_col1, method = "holm")
  p_holm_col2 <- stats::p.adjust(p_col2, method = "holm")
  p_holm_col3 <- stats::p.adjust(p_col3, method = "holm")
  p_holm_col4 <- stats::p.adjust(p_col4, method = "holm")
  
  # ---------- Build rows for the big table ----------
  tibble::tibble(
    Panel   = "Multiple Testing Adjustment (Holm)",
    Outcome = c(
      "Holm p-value: Capital",
      "Holm p-value: Psychosocial",
      "Holm p-value: Full"
    ),
    `(1)` = c(
      format_adjusted_p(p_holm_col1["Capital"]),
      format_adjusted_p(p_holm_col1["Psychosocial"]),
      format_adjusted_p(p_holm_col1["Full"])
    ),
    `(2)` = c(
      format_adjusted_p(p_holm_col2["Capital"]),
      format_adjusted_p(p_holm_col2["Psychosocial"]),
      format_adjusted_p(p_holm_col2["Full"])
    ),
    `(3)` = c(
      format_adjusted_p(p_holm_col3["Capital"]),
      format_adjusted_p(p_holm_col3["Psychosocial"]),
      format_adjusted_p(p_holm_col3["Full"])
    ),
    `(4)` = c(
      format_adjusted_p(p_holm_col4["Capital"]),
      format_adjusted_p(p_holm_col4["Psychosocial"]),
      format_adjusted_p(p_holm_col4["Full"])
    )
  )
}

# Format a coefficient from a fixest::feglm model (e.g., logit), matched by regex.
# - term_patterns: character vector of regex patterns to match the row name.
# - digits: rounding digits.
# - transform: "none" (default) or "exp" (e.g., to show Odds Ratios for logit).
#              When "exp", SE is transformed by delta method: se_exp = exp(beta) * se_beta
fmt_est_feglm_regex <- function(mod, term_patterns, digits = 3, transform = c("none", "exp")) {
  transform <- match.arg(transform)
  
  if (is.null(mod)) return("")
  # coeftable() respects the SE/cluster used when fitting or in summary()
  ct <- tryCatch(fixest::coeftable(mod), error = function(e) NULL)
  if (is.null(ct) || NROW(ct) == 0) return("")
  
  rn <- rownames(ct)
  hit <- which(Reduce(`|`, lapply(term_patterns, function(p) grepl(p, rn))))
  if (length(hit) == 0) return("")
  i <- hit[1]
  
  # Pull estimate, SE, p-value (column names vary slightly across fixest versions)
  est <- as.numeric(ct[i, "Estimate"])
  
  se_name <- intersect(colnames(ct), c("Std. Error", "Std. error", "Std.Error"))
  if (length(se_name) == 0) {
    # fallback: any column containing "Std" and "Error"
    se_candidates <- grep("Std\\.?\\s*Error", colnames(ct), ignore.case = TRUE, value = TRUE)
    se_name <- if (length(se_candidates) > 0) se_candidates[1] else NA_character_
  }
  se <- if (!is.na(se_name)) as.numeric(ct[i, se_name]) else NA_real_
  
  p_name <- grep("^Pr\\s*\\(>|[tzf]\\|\\)$|^Pr\\.", colnames(ct), ignore.case = TRUE, value = TRUE)
  if (length(p_name) == 0) p_name <- grep("p\\.value|pvalue", colnames(ct), ignore.case = TRUE, value = TRUE)
  p <- if (length(p_name) > 0) as.numeric(ct[i, p_name[1]]) else NA_real_
  
  # Stars
  stars <- ""
  if (!is.na(p)) {
    stars <- if (p < 0.01) "***" else if (p < 0.05) "**" else if (p < 0.10) "*" else ""
  }
  
  # Optional transform (e.g., OR for logit)
  if (transform == "exp") {
    est <- exp(est)
    # Delta method for SE of exp(beta): se_exp = |exp(beta)| * se_beta
    if (!is.na(se)) se <- abs(est) * se
  }
  
  est_out <- round(est, digits)
  se_out  <- if (!is.na(se)) round(se, digits) else NA
  
  if (is.na(se_out)) {
    paste0(est_out, stars)
  } else {
    paste0(est_out, stars, "\n(", se_out, ")")
  }
}

# -------- Helper to pull estimate, stars, and SE from felm (robust term matching) --------
fmt_est_felm_regex <- function(mod, term_patterns, digits = 2) {
  if (is.null(mod)) return("")
  sm <- tryCatch(summary(mod), error = function(e) NULL)
  if (is.null(sm)) return("")
  cf <- sm$coefficients
  if (is.null(cf) || nrow(cf) == 0) return("")
  
  rn <- rownames(cf)
  hit <- which(Reduce(`|`, lapply(term_patterns, function(p) grepl(p, rn))))
  if (length(hit) == 0) return("")
  i <- hit[1]
  
  est <- round(cf[i, "Estimate"], digits)
  
  # Prefer clustered SE if present; else robust; else standard
  se_col <- intersect(c("Cluster s.e.", "Robust s.e.", "Std. Error", "Std. error"), colnames(cf))
  if (length(se_col) == 0) se_col <- grep("s\\.e\\.", colnames(cf), value = TRUE)
  se <- if (length(se_col) > 0) round(cf[i, se_col[1]], digits) else NA_real_
  
  # p-value (if available)
  p_col <- intersect(c("Pr(>|t|)", "Pr(>F)", "p.value"), colnames(cf))
  stars <- ""
  if (length(p_col) > 0) {
    p <- cf[i, p_col[1]]
    if (!is.na(p)) stars <- if (p < 0.01) "***" else if (p < 0.05) "**" else if (p < 0.10) "*" else ""
  }
  
  if (is.na(se)) paste0(est, stars) else paste0(est, stars, "\n(", se, ")")
}

# -------- Convenience: n and R2 from felm --------
get_n  <- function(mod) if (is.null(mod)) NA_integer_ else tryCatch(stats::nobs(mod), error = function(e) NA_integer_)
get_r2 <- function(mod, digits = 3) {
  if (is.null(mod)) return(NA_real_)
  out <- tryCatch(summary(mod)$r.squared, 
                  error = function(e) NA_real_)
  round(out, digits)
}
get_pseudo_r2 <- function(mod, digits = 3) {
  if (is.null(mod)) return(NA_real_)
  out <- tryCatch(summary(mod)$pseudo_r2, # Pseudo-R²
                 error = function(e) NA_real_)
  round(out, digits)
}
# -------- Fit the table --------
FitFlextableToPage <- function(ft, pgwidth = 8){
  
  ft_out <- ft %>% autofit()
  
  ft_out <- width(ft_out, width = dim(ft_out)$widths*pgwidth /(flextable_dim(ft_out)$widths))
  return(ft_out)
}

# -------- Regex patterns for terms (robust to suffixes like TRUE, factor levels, etc.) --------
## Main
pat_csh   <- c("^treatment_csh_trnsfrCash Assignment(\\b|$)")
pat_cap   <- c("^treatment_piCapital(\\b|$)")
pat_psy   <- c("^treatment_piPsychosocial(\\b|$)")
pat_full  <- c("^treatment_piFull(\\b|$)")
pat_pool  <- c("^treatment_pi_pool(\\b|$)", "^treatment_pi_poolPool(\\b|$)")

## Spill
patspill_cap   <- c("^treatment_pi_spillCapital(\\b|$)")
patspill_psy   <- c("^treatment_pi_spillPsychosocial(\\b|$)")
patspill_full  <- c("^treatment_pi_spillFull(\\b|$)")
patspill_pool  <- c("^treatment_pi_spillpool(\\b|$)", "^treatment_pi_pool_spillPool(\\b|$)")

######################## Modified Function for With/Without Controls Comparison ##########################################



plot_het_ipv <- function(depvar_group,
                         curr_het_var,
                         mainResults_het,
                         median_val,
                         out_dir   = "output/heterogeneity",
                         binary_het = FALSE) {
  # Filter to relevant outcomes and heterogeneity variable
  mainResults_reg_group <- mainResults_het %>%
    dplyr::filter(outcome %in% depvar_group,
                  het_var == curr_het_var) %>%
    dplyr::arrange(name, typeTreat) %>%
    dplyr::arrange(factor(typeTreat,
                          levels = c("Cash Assignment", "Capital", "Psychosocial", "Full", "Pool")))
  
  if (nrow(mainResults_reg_group) == 0) {
    warning("No rows for het_var = ", curr_het_var, " and these outcomes.")
    return(invisible(NULL))
  }
  
  # Order factors
  mainResults_reg_group$typeTreat <- forcats::fct_inorder(mainResults_reg_group$typeTreat)
  mainResults_reg_group$depvar    <- forcats::fct_inorder(mainResults_reg_group$depvar)
  
  # Remove prefixes "N high treat (pct):" and "N low treat (pct):" from lab
  if (nrow(mainResults_reg_group) > 4) {
    mainResults_reg_group$lab[-c(1:2)] <- gsub(
      "N high treat (pct): ", "", mainResults_reg_group$lab[-c(1:2)], fixed = TRUE
    )
    mainResults_reg_group$lab[-c(1:2)] <- gsub(
      "N low treat (pct): ",  "", mainResults_reg_group$lab[-c(1:2)], fixed = TRUE
    )
  } else {
    mainResults_reg_group$lab <- gsub("N high treat (pct): ", "", mainResults_reg_group$lab, fixed = TRUE)
    mainResults_reg_group$lab <- gsub("N low treat (pct): ",  "", mainResults_reg_group$lab, fixed = TRUE)
  }
  
  # Binary het: show "yes"/"no" in legend & labels
  if (binary_het) {
    mainResults_reg_group <- mainResults_reg_group %>%
      dplyr::mutate(
        estimate = dplyr::recode(estimate, low = "no", high = "yes"),
        lab      = gsub("high", "yes", lab, fixed = TRUE),
        lab      = gsub("low",  "no",  lab, fixed = TRUE)
      )
  }
  
  # Keep order of lab as in dataframe
  mainResults_reg_group$lab <- factor(mainResults_reg_group$lab,
                                      levels = unique(mainResults_reg_group$lab))
  
  # Caption & colors
  if (binary_het) {
    caption_txt <- sprintf(
      "N = %s Tekavoul, %s EI inclusion.",
      dplyr::first(mainResults_reg_group$N),
      dplyr::last(mainResults_reg_group$N)
    )
    color_values <- c("yes" = "grey69", "no" = "black")
  } else {
    median_het <- median_val %>%
      dplyr::filter(het_var == curr_het_var) %>%
      dplyr::pull(median_value) %>%
      as.numeric()
    
    caption_txt <- sprintf(
      paste0(
        "N = %s for Tekavoul and %s for EI inclusion. ",
        "Beneficiaries were divided into two groups based on the median value of %s ",
        "(low \u2264 %s and high > %s). "
      ),
      dplyr::first(mainResults_reg_group$N),
      dplyr::last(mainResults_reg_group$N),
      median_het, median_het, median_het
    )
    
    color_values <- c("high" = "grey69", "low" = "black")
  }

  p <- mainResults_reg_group %>%
    ggplot2::ggplot(
      ggplot2::aes(
        x    = lab,
        y    = pe,
        color = estimate,
        ymin = `CI2.5`,
        ymax = `CI97.5`
      )
    ) +
    ggplot2::geom_point(position = ggplot2::position_dodge(.5)) +
    ggplot2::geom_linerange(position = ggplot2::position_dodge(.5)) +
    ggplot2::geom_linerange(
      ggplot2::aes(ymin = CI5, ymax = CI95),
      lwd = 1,
      position = ggplot2::position_dodge(.5)
    ) +
    ggplot2::geom_hline(yintercept = 0, lty = "dotted") +
    ggplot2::facet_grid(depvar ~ name,
                        scales = "free",
                        space  = "free",
                        shrink = TRUE,
                        drop   = TRUE) +
    ggplot2::scale_color_manual(values = color_values) +
    ggplot2::labs(
      x       = "",
      y       = "Effect",
      color   = dplyr::first(mainResults_reg_group$het_var),
      caption = caption_txt
    ) +
    ggplot2::guides(
      colour = ggplot2::guide_legend(order = 1),
      shape  = ggplot2::guide_legend(order = 2)
    ) +
    ggplot2::theme_light(base_size = 8) +
    ggplot2::theme(
      legend.position  = "bottom",
      legend.direction = "horizontal",
      text             = ggplot2::element_text(size = 8)
    )
  
  # Ensure directory exists
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  
  # Save figure
  out_file <- file.path(
    out_dir,
    paste0(dplyr::first(mainResults_reg_group$het_var), "_",
           dplyr::first(mainResults_reg_group$depvar), ".jpeg")
  )
  
  ggplot2::ggsave(out_file, plot = p, width = 12, height = 14, units = "cm")
  
  p
}
getTable_IPV <- function(ipv_vars,
                         treatment_vars,
                         control_vars,
                         strata_vars,
                         cluster_vars,
                         followup_ipv_MRT_hh,
                         followup_ipv_MRT_hh_control_pi,
                         followup_ipv_MRT_hh_control_csh_trnsfr,
                         col_group_labels,
                         table_title,
                         footnote_text = NULL) {
  
  # Default footnote if not provided
  if (is.null(footnote_text)) {
    footnote_text <- "Notes: Results presented are OLS estimates. Columns (1) and (3) show estimates without controls (treatment + strata only for cash transfer, treatment only for PI). Columns (2) and (4) include controls for randomization strata (commune) and baseline covariates. We control for social promotion intervention. Enumerator fixed effects are included in all regressions. We estimate the regressions for the EI beneficiaries aged 18\u201349 only. Robust standard errors are shown in parentheses, clustered at the village proxy level. *** p < 0.01, ** p < 0.05, * p < 0.1."
  }
  
  # ---------------------------------------------------------------------------
  # 1. Run main analysis and extract models
  # ---------------------------------------------------------------------------
  mainResults <- map_dfr(treatment_vars, function(curr_treat_var) {
    map_dfr(ipv_vars, function(depvar) {
      getEstimateGlobalComparison(
        depvar, curr_treat_var, control_vars,
        strata_vars, cluster_vars, followup_ipv_MRT_hh
      ) %>%
        mutate(depvar = depvar, curr_treat_var = curr_treat_var)
    })
  })
  
  # Filter by treatment arm
  res_csh  <- mainResults %>% filter(curr_treat_var == "treatment_csh_trnsfr")
  res_pi   <- mainResults %>% filter(curr_treat_var == "treatment_pi")
  res_pool <- mainResults %>% filter(curr_treat_var == "treatment_pi_pool")
  
  # Model lists: one entry per outcome variable, no-controls and with-controls
  models_csh_no    <- res_csh$results_no_controls
  models_csh_with  <- res_csh$results_with_controls
  models_pi_no     <- res_pi$results_no_controls
  models_pi_with   <- res_pi$results_with_controls
  models_pool_no   <- res_pool$results_no_controls
  models_pool_with <- res_pool$results_with_controls
  
  # ---------------------------------------------------------------------------
  # 2. Extract PI interaction-term comparisons (no controls / with controls)
  # ---------------------------------------------------------------------------
  full_Psychosocial_no   <- res_pi %>% pull(full_Psychosocial_no)
  full_Capital_no        <- res_pi %>% pull(full_Capital_no)
  psychosocial_Capital_no <- res_pi %>% pull(psychosocial_Capital_no)
  
  full_Psychosocial_with   <- res_pi %>% pull(full_Psychosocial_with)
  full_Capital_with        <- res_pi %>% pull(full_Capital_with)
  psychosocial_Capital_with <- res_pi %>% pull(psychosocial_Capital_with)
  
  # ---------------------------------------------------------------------------
  # 3. Descriptive statistics (control group means and SDs)
  # ---------------------------------------------------------------------------
  mean_values_pi        <- sapply(ipv_vars, function(v) round(mean(followup_ipv_MRT_hh_control_pi[[v]],        na.rm = TRUE), 3))
  sd_values_pi          <- sapply(ipv_vars, function(v) round(sd(followup_ipv_MRT_hh_control_pi[[v]],          na.rm = TRUE), 3))
  mean_values_csh_trnsfr <- sapply(ipv_vars, function(v) round(mean(followup_ipv_MRT_hh_control_csh_trnsfr[[v]], na.rm = TRUE), 3))
  sd_values_csh_trnsfr   <- sapply(ipv_vars, function(v) round(sd(followup_ipv_MRT_hh_control_csh_trnsfr[[v]],   na.rm = TRUE), 3))
  
  # Column names driven by number of outcomes  x  2 (no controls / with controls)
  # Interleaved layout: (1)=no-ctrl outcome1, (2)=with-ctrl outcome1,
  #                     (3)=no-ctrl outcome2, (4)=with-ctrl outcome2, ...
  n_outcomes  <- length(ipv_vars)
  n_cols      <- n_outcomes * 2
  col_names   <- paste0("(", seq_len(n_cols), ")")
  
  # Helper: for outcome j, "no" column index = 2j-1, "with" column index = 2j
  idx_no   <- function(j) 2 * j - 1
  idx_with <- function(j) 2 * j
  
  # ---------------------------------------------------------------------------
  # 4. Panel 1: Tekavoul (cash transfer)
  # ---------------------------------------------------------------------------
  row_tek <- lapply(seq_len(n_outcomes), function(j) {
    list(
      no   = c("—", fmt_est_felm_regex(models_csh_no[[j]],   pat_csh),
               as.character(get_n(models_csh_no[[j]])),
               as.character(get_r2(models_csh_no[[j]]))),
      with = c("—", fmt_est_felm_regex(models_csh_with[[j]], pat_csh),
               as.character(get_n(models_csh_with[[j]])),
               as.character(get_r2(models_csh_with[[j]])))
    )
  })
  
  tek_mat <- matrix(NA_character_, nrow = 4, ncol = n_cols)
  for (j in seq_len(n_outcomes)) {
    tek_mat[, idx_no(j)]   <- row_tek[[j]]$no
    tek_mat[, idx_with(j)] <- row_tek[[j]]$with
  }
  
  tek_rows <- data.frame(
    Outcome = c("Control", "Cash Assignment", "No. Obs.", "R\u00B2"),
    as.data.frame(tek_mat, stringsAsFactors = FALSE),
    stringsAsFactors = FALSE
  )
  colnames(tek_rows) <- c("Outcome", col_names)
  
  # ---------------------------------------------------------------------------
  # 5. Panel 2: EI Inclusion (PI arms)
  # ---------------------------------------------------------------------------
  row_pi <- lapply(seq_len(n_outcomes), function(j) {
    list(
      no   = c("—",
               fmt_est_felm_regex(models_pi_no[[j]],   pat_cap),
               fmt_est_felm_regex(models_pi_no[[j]],   pat_psy),
               fmt_est_felm_regex(models_pi_no[[j]],   pat_full),
               as.character(get_n(models_pi_no[[j]])),
               as.character(get_r2(models_pi_no[[j]]))),
      with = c("—",
               fmt_est_felm_regex(models_pi_with[[j]], pat_cap),
               fmt_est_felm_regex(models_pi_with[[j]], pat_psy),
               fmt_est_felm_regex(models_pi_with[[j]], pat_full),
               as.character(get_n(models_pi_with[[j]])),
               as.character(get_r2(models_pi_with[[j]])))
    )
  })
  
  pi_mat <- matrix(NA_character_, nrow = 6, ncol = n_cols)
  for (j in seq_len(n_outcomes)) {
    pi_mat[, idx_no(j)]   <- row_pi[[j]]$no
    pi_mat[, idx_with(j)] <- row_pi[[j]]$with
  }
  
  pi_rows <- data.frame(
    Outcome = c("Control", "Capital", "Psychosocial", "Full", "No. Obs.", "R\u00B2"),
    as.data.frame(pi_mat, stringsAsFactors = FALSE),
    stringsAsFactors = FALSE
  )
  colnames(pi_rows) <- c("Outcome", col_names)
  
  # ---------------------------------------------------------------------------
  # 6. Panel 3: EI Inclusion (Pool)
  # ---------------------------------------------------------------------------
  row_pool <- lapply(seq_len(n_outcomes), function(j) {
    list(
      no   = c("—", fmt_est_felm_regex(models_pool_no[[j]],   pat_pool),
               as.character(get_n(models_pool_no[[j]])),
               as.character(get_r2(models_pool_no[[j]]))),
      with = c("—", fmt_est_felm_regex(models_pool_with[[j]], pat_pool),
               as.character(get_n(models_pool_with[[j]])),
               as.character(get_r2(models_pool_with[[j]])))
    )
  })
  
  pool_mat <- matrix(NA_character_, nrow = 4, ncol = n_cols)
  for (j in seq_len(n_outcomes)) {
    pool_mat[, idx_no(j)]   <- row_pool[[j]]$no
    pool_mat[, idx_with(j)] <- row_pool[[j]]$with
  }
  
  pool_rows <- data.frame(
    Outcome = c("Control", "Pool", "No. Obs.", "R\u00B2"),
    as.data.frame(pool_mat, stringsAsFactors = FALSE),
    stringsAsFactors = FALSE
  )
  colnames(pool_rows) <- c("Outcome", col_names)
  
  # ---------------------------------------------------------------------------
  # 7. Panel 4: Additional statistics
  # ---------------------------------------------------------------------------
  # Build a matrix: rows = stats, cols = interleaved (no/with per outcome)
  ctrl_vars_row <- rep(c("No", "Yes"), times = n_outcomes)
  
  extra_mat <- matrix(NA_character_, nrow = 8, ncol = n_cols)
  for (j in seq_len(n_outcomes)) {
    extra_mat[1, idx_no(j)]   <- "No"
    extra_mat[1, idx_with(j)] <- "Yes"
    extra_mat[2, idx_no(j)]   <- extra_mat[2, idx_with(j)] <- as.character(mean_values_csh_trnsfr[j])
    extra_mat[3, idx_no(j)]   <- extra_mat[3, idx_with(j)] <- as.character(sd_values_csh_trnsfr[j])
    extra_mat[4, idx_no(j)]   <- extra_mat[4, idx_with(j)] <- as.character(mean_values_pi[j])
    extra_mat[5, idx_no(j)]   <- extra_mat[5, idx_with(j)] <- as.character(sd_values_pi[j])
    extra_mat[6, idx_no(j)]   <- as.character(full_Psychosocial_no[j])
    extra_mat[6, idx_with(j)] <- as.character(full_Psychosocial_with[j])
    extra_mat[7, idx_no(j)]   <- as.character(full_Capital_no[j])
    extra_mat[7, idx_with(j)] <- as.character(full_Capital_with[j])
    extra_mat[8, idx_no(j)]   <- as.character(psychosocial_Capital_no[j])
    extra_mat[8, idx_with(j)] <- as.character(psychosocial_Capital_with[j])
  }
  
  extra_rows <- data.frame(
    Outcome = c(
      "Control Variables",
      "Control mean @ follow up Tekavoul",
      "Control SD @ follow up Tekavoul",
      "Control mean @ follow up PI",
      "Control SD @ follow up PI",
      "Full - Psychosocial",
      "Full - Capital",
      "Psychosocial - Capital"
    ),
    as.data.frame(extra_mat, stringsAsFactors = FALSE),
    stringsAsFactors = FALSE
  )
  colnames(extra_rows) <- c("Outcome", col_names)
  
  # ---------------------------------------------------------------------------
  # 8. Bind all panels and add Panel label column
  # ---------------------------------------------------------------------------
  final_tbl <- dplyr::bind_rows(tek_rows, pi_rows, pool_rows, extra_rows)
  
  n_tek   <- nrow(tek_rows)
  n_pi    <- nrow(pi_rows)
  n_pool  <- nrow(pool_rows)
  n_extra <- nrow(extra_rows)
  
  i_tek_start   <- 1
  i_tek_end     <- n_tek
  i_pi_start    <- i_tek_end + 1
  i_pi_end      <- i_pi_start + n_pi - 1
  i_pool_start  <- i_pi_end + 1
  i_pool_end    <- i_pool_start + n_pool - 1
  i_extra_start <- i_pool_end + 1
  i_extra_end   <- i_extra_start + n_extra - 1
  
  panel_titles <- c(
    rep("Tekavoul",                n_tek),
    rep("EI Inclusion",            n_pi),
    rep("EI Inclusion (Pool)",     n_pool),
    rep("Additional statistics",   n_extra)
  )
  
  final_tbl <- final_tbl %>%
    dplyr::mutate(Panel = panel_titles, .before = 1)
  
  # ---------------------------------------------------------------------------
  # 9. Build flextable
  # ---------------------------------------------------------------------------
  if (exists("customtab_defaults")) customtab_defaults()
  
  ft <- flextable::flextable(final_tbl)
  
  # Base header labels
  ft <- flextable::set_header_labels(
    ft,
    values = c(
      Panel   = " ",
      Outcome = "Outcome\u2020",
      stats::setNames(col_names, col_names)
    )
  )
  
  # Top header row: outcome group labels, each spanning 2 columns (no/with ctrl)
  ft <- flextable::add_header_row(
    ft,
    values    = c(" ", " ", col_group_labels),
    colwidths = c(1, 1, rep(2, length(col_group_labels)))
  )
  
  # Alignment
  ft <- flextable::align(ft, j = 1:2,             align = "left",   part = "all")
  ft <- flextable::align(ft, j = 3:(n_cols + 2),  align = "center", part = "all")
  
  # Borders
  border_thick <- officer::fp_border(color = "black", width = 2)
  border_thin  <- officer::fp_border(color = "black", width = 0.5)
  
  ft <- flextable::border_inner_h(ft, border = border_thin,  part = "body")
  ft <- flextable::hline(ft, i = i_pi_start   - 1, border = border_thick, part = "body")
  ft <- flextable::hline(ft, i = i_pool_start - 1, border = border_thick, part = "body")
  ft <- flextable::hline(ft, i = i_extra_start - 1, border = border_thick, part = "body")
  
  # Merge and bold the Panel column
  ft <- flextable::merge_v(ft, j = "Panel", part = "body")
  ft <- flextable::bold(ft,   j = "Panel", part = "body")
  
  # Caption and footnote
  ft <- flextable::set_caption(ft, caption = table_title)
  ft <- flextable::add_footer_lines(ft, values = footnote_text)
  
  # Fit to page width
  if (exists("FitFlextableToPage")) {
    ft <- FitFlextableToPage(ft)
  } else {
    ft <- flextable::autofit(ft)
  }
  
  return(ft)
}
# 
# getTable_IPV <- function(ipv_vars, 
#                          treatment_vars,
#                          control_vars,
#                          strata_vars,
#                          cluster_vars,
#                          followup_ipv_MRT_hh,
#                          followup_ipv_MRT_hh_control_pi,
#                          followup_ipv_MRT_hh_control_csh_trnsfr,
#                          col_group_labels,
#                          table_title,
#                          footnote_text = NULL) {
#   
#   # Default footnote if not provided
#   if (is.null(footnote_text)) {
#     footnote_text <- "Notes: Results presented are OLS estimates. Columns (1) and (3) show estimates without controls (treatment + strata only for cash transfer, treatment only for PI). Columns (2) and (4) include controls for randomization strata (commune) and baseline covariates. We control for social promotion intervention. Enumerator fixed effects are included in all regressions. We estimate the regressions for the EI beneficiaries aged 18–49 only. Robust standard errors are shown in parentheses, clustered at the village proxy level. *** p < 0.01, ** p < 0.05, * p < 0.1."
#   }
#   
#   # Run main analysis with comparison
#   mainResults <- map_dfr(treatment_vars, function(curr_treat_var) {
#     tempResults <- map_dfr(ipv_vars, function(depvar) {    
#       bind_rows(
#         getEstimateGlobalComparison(depvar, curr_treat_var, control_vars, strata_vars, cluster_vars, followup_ipv_MRT_hh) %>%
#           mutate(depvar = depvar, curr_treat_var = curr_treat_var)
#       )
#     })
#   })
#   
#   # Extract models WITHOUT controls
#   m11_no <- mainResults$results_no_controls[[1]]
#   m12_no <- mainResults$results_no_controls[[2]]
#   m21_no <- mainResults$results_no_controls[[3]]
#   m22_no <- mainResults$results_no_controls[[4]]
#   m31_no <- mainResults$results_no_controls[[5]]
#   m32_no <- mainResults$results_no_controls[[6]]
#   
#   # Extract models WITH controls
#   m11_with <- mainResults$results_with_controls[[1]]
#   m12_with <- mainResults$results_with_controls[[2]]
#   m21_with <- mainResults$results_with_controls[[3]]
#   m22_with <- mainResults$results_with_controls[[4]]
#   m31_with <- mainResults$results_with_controls[[5]]
#   m32_with <- mainResults$results_with_controls[[6]]
#   
#   # Extract interaction terms for PI (no controls)
#   full_Psychosocial_no <- mainResults %>% 
#     filter(curr_treat_var == "treatment_pi") %>% 
#     select(full_Psychosocial_no) %>% 
#     pull() 
#   
#   full_Capital_no <- mainResults %>% 
#     filter(curr_treat_var == "treatment_pi") %>% 
#     select(full_Capital_no) %>% 
#     pull() 
#   
#   psychosocial_Capital_no <- mainResults %>% 
#     filter(curr_treat_var == "treatment_pi") %>% 
#     select(psychosocial_Capital_no) %>% 
#     pull() 
#   
#   # Extract interaction terms for PI (with controls)
#   full_Psychosocial_with <- mainResults %>% 
#     filter(curr_treat_var == "treatment_pi") %>% 
#     select(full_Psychosocial_with) %>% 
#     pull() 
#   
#   full_Capital_with <- mainResults %>% 
#     filter(curr_treat_var == "treatment_pi") %>% 
#     select(full_Capital_with) %>% 
#     pull() 
#   
#   psychosocial_Capital_with <- mainResults %>% 
#     filter(curr_treat_var == "treatment_pi") %>% 
#     select(psychosocial_Capital_with) %>% 
#     pull() 
#   
#   # Calculate descriptive statistics
#   mean_values_pi <- c()
#   sd_values_pi <- c()
#   mean_values_csh_trnsfr <- c()
#   sd_values_csh_trnsfr <- c()
#   
#   for (depvar in ipv_vars) {
#     mean_values_pi <- c(mean_values_pi, round(mean(followup_ipv_MRT_hh_control_pi[[depvar]], na.rm = TRUE), 3))
#     mean_values_csh_trnsfr <- c(mean_values_csh_trnsfr, round(mean(followup_ipv_MRT_hh_control_csh_trnsfr[[depvar]], na.rm = TRUE), 3))
#     sd_values_pi <- c(sd_values_pi, round(sd(followup_ipv_MRT_hh_control_pi[[depvar]], na.rm = TRUE), 3))
#     sd_values_csh_trnsfr <- c(sd_values_csh_trnsfr, round(sd(followup_ipv_MRT_hh_control_csh_trnsfr[[depvar]], na.rm = TRUE), 3))
#   }
#   
#   # =========================
#   # Panel 1: Tekavoul
#   # =========================
#   tek_rows <- data.frame(
#     Outcome = c("Control", "Cash Assignment", "No. Obs.", "R²"),
#     `(1)` = c("—",
#               fmt_est_felm_regex(m11_no, pat_csh),
#               as.character(get_n(m11_no)),
#               as.character(get_r2(m11_no))),
#     `(2)` = c("—",
#               fmt_est_felm_regex(m11_with, pat_csh),
#               as.character(get_n(m11_with)),
#               as.character(get_r2(m11_with))),
#     `(3)` = c("—",
#               fmt_est_felm_regex(m12_no, pat_csh),
#               as.character(get_n(m12_no)),
#               as.character(get_r2(m12_no))),
#     `(4)` = c("—",
#               fmt_est_felm_regex(m12_with, pat_csh),
#               as.character(get_n(m12_with)),
#               as.character(get_r2(m12_with))),
#     stringsAsFactors = FALSE,
#     check.names = FALSE
#   )
#   
#   # =========================
#   # Panel 2: EI Inclusion
#   # =========================
#   pi_rows <- data.frame(
#     Outcome = c("Control", "Capital", "Psychosocial", "Full", "No. Obs.", "R²"),
#     `(1)` = c("—",
#               fmt_est_felm_regex(m21_no, pat_cap),
#               fmt_est_felm_regex(m21_no, pat_psy),
#               fmt_est_felm_regex(m21_no, pat_full),
#               as.character(get_n(m21_no)),
#               as.character(get_r2(m21_no))),
#     `(2)` = c("—",
#               fmt_est_felm_regex(m21_with, pat_cap),
#               fmt_est_felm_regex(m21_with, pat_psy),
#               fmt_est_felm_regex(m21_with, pat_full),
#               as.character(get_n(m21_with)),
#               as.character(get_r2(m21_with))),
#     `(3)` = c("—",
#               fmt_est_felm_regex(m22_no, pat_cap),
#               fmt_est_felm_regex(m22_no, pat_psy),
#               fmt_est_felm_regex(m22_no, pat_full),
#               as.character(get_n(m22_no)),
#               as.character(get_r2(m22_no))),
#     `(4)` = c("—",
#               fmt_est_felm_regex(m22_with, pat_cap),
#               fmt_est_felm_regex(m22_with, pat_psy),
#               fmt_est_felm_regex(m22_with, pat_full),
#               as.character(get_n(m22_with)),
#               as.character(get_r2(m22_with))),
#     stringsAsFactors = FALSE,
#     check.names = FALSE
#   )
#   
#   # =========================
#   # Panel 3: EI Inclusion (Pool)
#   # =========================
#   pi_pool_rows <- data.frame(
#     Outcome = c("Control", "Pool", "No. Obs.", "R²"),
#     `(1)` = c("—",
#               fmt_est_felm_regex(m31_no, pat_pool),
#               as.character(get_n(m31_no)),
#               as.character(get_r2(m31_no))),
#     `(2)` = c("—",
#               fmt_est_felm_regex(m31_with, pat_pool),
#               as.character(get_n(m31_with)),
#               as.character(get_r2(m31_with))),
#     `(3)` = c("—",
#               fmt_est_felm_regex(m32_no, pat_pool),
#               as.character(get_n(m32_no)),
#               as.character(get_r2(m32_no))),
#     `(4)` = c("—",
#               fmt_est_felm_regex(m32_with, pat_pool),
#               as.character(get_n(m32_with)),
#               as.character(get_r2(m32_with))),
#     stringsAsFactors = FALSE,
#     check.names = FALSE
#   )
#   
#   # =========================
#   # Panel 4: Additional statistics
#   # =========================
#   final_panel <- data.frame(
#     Outcome = c(
#       "Control Variables",
#       "Control mean @ follow up Tekavoul",
#       "Control SD @ follow up Tekavoul",
#       "Control mean @ follow up PI",
#       "Control SD @ follow up PI",
#       "Full - Psychosocial",
#       "Full - Capital",
#       "Psychosocial - Capital"
#     ),
#     `(1)` = c(
#       "No",
#       as.character(mean_values_csh_trnsfr[1]),
#       as.character(sd_values_csh_trnsfr[1]),
#       as.character(mean_values_pi[1]),
#       as.character(sd_values_pi[1]),
#       as.character(full_Psychosocial_no[1]),
#       as.character(full_Capital_no[1]),
#       as.character(psychosocial_Capital_no[1])
#     ),
#     `(2)` = c(
#       "Yes",
#       as.character(mean_values_csh_trnsfr[1]),
#       as.character(sd_values_csh_trnsfr[1]),
#       as.character(mean_values_pi[1]),
#       as.character(sd_values_pi[1]),
#       as.character(full_Psychosocial_with[1]),
#       as.character(full_Capital_with[1]),
#       as.character(psychosocial_Capital_with[1])
#     ),
#     `(3)` = c(
#       "No",
#       as.character(mean_values_csh_trnsfr[2]),
#       as.character(sd_values_csh_trnsfr[2]),
#       as.character(mean_values_pi[2]),
#       as.character(sd_values_pi[2]),
#       as.character(full_Psychosocial_no[2]),
#       as.character(full_Capital_no[2]),
#       as.character(psychosocial_Capital_no[2])
#     ),
#     `(4)` = c(
#       "Yes",
#       as.character(mean_values_csh_trnsfr[2]),
#       as.character(sd_values_csh_trnsfr[2]),
#       as.character(mean_values_pi[2]),
#       as.character(sd_values_pi[2]),
#       as.character(full_Psychosocial_with[2]),
#       as.character(full_Capital_with[2]),
#       as.character(psychosocial_Capital_with[2])
#     ),
#     stringsAsFactors = FALSE,
#     check.names = FALSE
#   )
#   
#   # =========================
#   # Bind everything
#   # =========================
#   final_tbl <- dplyr::bind_rows(
#     tek_rows,
#     pi_rows,
#     pi_pool_rows,
#     final_panel
#   )
#   
#   # Compute row ranges for each panel
#   n_tek   <- nrow(tek_rows)
#   n_pi    <- nrow(pi_rows)
#   n_pool  <- nrow(pi_pool_rows)
#   n_extra <- nrow(final_panel)
#   
#   i_tek_start   <- 1
#   i_tek_end     <- n_tek
#   i_pi_start    <- i_tek_end + 1
#   i_pi_end      <- i_pi_start + n_pi - 1
#   i_pool_start  <- i_pi_end + 1
#   i_pool_end    <- i_pool_start + n_pool - 1
#   i_extra_start <- i_pool_end + 1
#   i_extra_end   <- i_extra_start + n_extra - 1
#   
#   # Panel titles
#   panel_titles <- c(
#     rep("Tekavoul",                    n_tek),
#     rep("EI Inclusion",        n_pi),
#     rep("EI Inclusion (Pool)", n_pool),
#     rep("Additional statistics",       n_extra)
#   )
#   
#   # Add Panel column
#   final_tbl <- final_tbl %>%
#     dplyr::mutate(Panel = panel_titles, .before = 1)
#   
#   #------------------------------------------------------------
#   # Build flextable for Word (officedown)
#   #------------------------------------------------------------
#   if (exists("customtab_defaults")) {
#     customtab_defaults()
#   }
#   
#   ft <- flextable::flextable(final_tbl)
# 
#   # Set header labels
#   ft <- flextable::set_header_labels(
#     ft,
#     values = c(
#       Panel   = " ",
#       Outcome = "Outcome†",
#       `(1)` = "(1)",
#       `(2)` = "(2)",
#       `(3)` = "(3)",
#       `(4)` = "(4)"
#     )
#   )
#   
#   # Add header row for column grouping
#   ft <- flextable::add_header_row(
#     ft,
#     values = c(" ", " ", col_group_labels),
#     colwidths = c(1, 1, 2, 2)
#   )
#   
#   # Alignment
#   ft <- flextable::align(ft, j = 1:2, align = "left", part = "all")
#   ft <- flextable::align(ft, j = 3:6, align = "center", part = "all")
#   
#   # Borders
#   border_thick <- officer::fp_border(color = "black", width = 2)
#   border_thin  <- officer::fp_border(color = "black", width = 0.5)
#   
#   ft <- flextable::border_inner_h(ft, border = border_thin, part = "body")
#   ft <- flextable::hline(ft, i = i_pi_start - 1, border = border_thick, part = "body")
#   ft <- flextable::hline(ft, i = i_pool_start - 1, border = border_thick, part = "body")
#   ft <- flextable::hline(ft, i = i_extra_start - 1, border = border_thick, part = "body")
#   
#   # Make Panel titles merged and bold
#   ft <- flextable::merge_v(ft, j = "Panel", part = "body")
#   ft <- flextable::bold(ft, j = "Panel", part = "body")
#   
#   # Caption
#   ft <- flextable::set_caption(ft, caption = table_title)
#   
#   # Footnote
#   ft <- flextable::add_footer_lines(ft, values = footnote_text)
#   
#   # Fit to page width
#   if (exists("FitFlextableToPage")) {
#     ft <- FitFlextableToPage(ft)
#   } else {
#     ft <- flextable::autofit(ft)
#   }
#   
#   return(ft)
# }

# Modified function to estimate models with and without controls
getEstimateGlobalComparison <- function(depvar, curr_treat_var, control_vars, strata_vars, cluster_vars, followup_df){
  
  if(curr_treat_var=="treatment_csh_trnsfr"){
    
    # MODEL WITHOUT CONTROLS (only treatment + strata)
    regmodel_no_cntrls <- paste0(depvar, " ~ ", curr_treat_var, " + ", strata_vars, "| 0 | 0 |", "hhid")
    reg_df <- followup_df %>% filter(reg_hh_csh_mrt==1)
    regmodbase_no_cntrls <- felm(formula=formula(regmodel_no_cntrls), data = reg_df)
    
    # MODEL WITH CONTROLS (treatment + strata + controls)
    regmodel_with_cntrls <- paste0(depvar, " ~ ", curr_treat_var, " + ", strata_vars, " + ", 
                                   paste0(control_vars, collapse = " + "), "| 0 | 0 |", "hhid")
    regmodbase_with_cntrls <- felm(formula=formula(regmodel_with_cntrls), data = reg_df)
    
    coef_ftest_1_no = ""
    coef_ftest_2_no = ""
    coef_ftest_3_no = ""
    coef_ftest_1_with = ""
    coef_ftest_2_with = ""
    coef_ftest_3_with = ""
    
  } else if(curr_treat_var=="treatment_pi"){
    
    # MODEL WITHOUT CONTROLS (only treatment)
    regmodel_no_cntrls <- paste0(depvar, " ~ ", curr_treat_var, " + ", strata_vars, "| 0 | 0 |", "cluster")
    reg_df <- followup_df %>% filter(reg_hh_pi_mrt==1)
    regmodbase_no_cntrls <- felm(formula=formula(regmodel_no_cntrls), data = reg_df)
    
    # MODEL WITH CONTROLS (treatment + controls)
    regmodel_with_cntrls <- paste0(depvar, " ~ ", curr_treat_var, " + ", strata_vars, " + ", 
                                   paste0(control_vars, collapse = " + "), "| 0 | 0 |", "cluster")
    regmodbase_with_cntrls <- felm(formula=formula(regmodel_with_cntrls), data = reg_df)
    
    # F-tests for model WITHOUT controls
    coef_values_no <- coef(regmodbase_no_cntrls)
    
    joint_f_test_1_no <- linearHypothesis(regmodbase_no_cntrls, 
                                          c("treatment_piFull=treatment_piPsychosocial"),
                                          test = "F", singular.ok = TRUE)
    joint_f_test_2_no <- linearHypothesis(regmodbase_no_cntrls, 
                                          c("treatment_piFull=treatment_piCapital"),
                                          test = "F", singular.ok = TRUE)
    joint_f_test_3_no <- linearHypothesis(regmodbase_no_cntrls, 
                                          c("treatment_piPsychosocial = treatment_piCapital"),
                                          test = "F", singular.ok = TRUE)
    
    coef_ftest_1_no <- round(coef_values_no["treatment_piFull"] - coef_values_no["treatment_piPsychosocial"], 3)
    pval_ftest_1_no <- round(joint_f_test_1_no$`Pr(>F)`[2], 3)
    
    coef_ftest_2_no <- round(coef_values_no["treatment_piFull"] - coef_values_no["treatment_piCapital"], 3)
    pval_ftest_2_no <- round(joint_f_test_2_no$`Pr(>F)`[2], 3)
    
    coef_ftest_3_no <- round(coef_values_no["treatment_piPsychosocial"] - coef_values_no["treatment_piCapital"], 3)
    pval_ftest_3_no <- round(joint_f_test_3_no$`Pr(>F)`[2], 3)
    
    # F-tests for model WITH controls
    coef_values_with <- coef(regmodbase_with_cntrls)
    
    joint_f_test_1_with <- linearHypothesis(regmodbase_with_cntrls, 
                                            c("treatment_piFull=treatment_piPsychosocial"),
                                            test = "F", singular.ok = TRUE)
    joint_f_test_2_with <- linearHypothesis(regmodbase_with_cntrls, 
                                            c("treatment_piFull=treatment_piCapital"),
                                            test = "F", singular.ok = TRUE)
    joint_f_test_3_with <- linearHypothesis(regmodbase_with_cntrls, 
                                            c("treatment_piPsychosocial = treatment_piCapital"),
                                            test = "F", singular.ok = TRUE)
    
    coef_ftest_1_with <- round(coef_values_with["treatment_piFull"] - coef_values_with["treatment_piPsychosocial"], 3)
    pval_ftest_1_with <- round(joint_f_test_1_with$`Pr(>F)`[2], 3)
    
    coef_ftest_2_with <- round(coef_values_with["treatment_piFull"] - coef_values_with["treatment_piCapital"], 3)
    pval_ftest_2_with <- round(joint_f_test_2_with$`Pr(>F)`[2], 3)
    
    coef_ftest_3_with <- round(coef_values_with["treatment_piPsychosocial"] - coef_values_with["treatment_piCapital"], 3)
    pval_ftest_3_with <- round(joint_f_test_3_with$`Pr(>F)`[2], 3)
    
    # Add significance stars for NO controls
    for (index in 1:3) {
      pval_ftest <- get(paste0("pval_ftest_", index, "_no"))
      coef_ftest <- get(paste0("coef_ftest_", index, "_no"))
      
      if (pval_ftest <= 0.01) {
        coef_ftest <- paste0(coef_ftest, "***")
      } else if (pval_ftest <= 0.05) {
        coef_ftest <- paste0(coef_ftest, "**")
      } else if (pval_ftest <= 0.10) {
        coef_ftest <- paste0(coef_ftest, "*")
      } else {
        coef_ftest <- as.character(coef_ftest)
      }
      assign(paste0("coef_ftest_", index, "_no"), coef_ftest)
    }
    
    # Add significance stars for WITH controls
    for (index in 1:3) {
      pval_ftest <- get(paste0("pval_ftest_", index, "_with"))
      coef_ftest <- get(paste0("coef_ftest_", index, "_with"))
      
      if (pval_ftest <= 0.01) {
        coef_ftest <- paste0(coef_ftest, "***")
      } else if (pval_ftest <= 0.05) {
        coef_ftest <- paste0(coef_ftest, "**")
      } else if (pval_ftest <= 0.10) {
        coef_ftest <- paste0(coef_ftest, "*")
      } else {
        coef_ftest <- as.character(coef_ftest)
      }
      assign(paste0("coef_ftest_", index, "_with"), coef_ftest)
    }
    
  } else {
    # For pooled treatment
    
    # MODEL WITHOUT CONTROLS 
    regmodel_no_cntrls <- paste0(depvar, " ~ ", curr_treat_var, " + ", strata_vars, "| 0 | 0 |", "cluster")
    reg_df <- followup_df %>% filter(reg_hh_pi_mrt==1)
    regmodbase_no_cntrls <- felm(formula=formula(regmodel_no_cntrls), data = reg_df)
    
    # MODEL WITH CONTROLS
    regmodel_with_cntrls <- paste0(depvar, " ~ ", curr_treat_var, " + ", strata_vars, " + ", 
                                   paste0(control_vars, collapse = " + "), "| 0 | 0 |", "cluster")
    regmodbase_with_cntrls <- felm(formula=formula(regmodel_with_cntrls), data = reg_df)
    
    coef_ftest_1_no = ""
    coef_ftest_2_no = ""
    coef_ftest_3_no = ""
    coef_ftest_1_with = ""
    coef_ftest_2_with = ""
    coef_ftest_3_with = ""
  }
  
  # Return results in tibble format
  out <- tibble(
    results_no_controls = list(regmodbase_no_cntrls),
    results_with_controls = list(regmodbase_with_cntrls),
    full_Psychosocial_no = coef_ftest_1_no,
    full_Capital_no = coef_ftest_2_no,
    psychosocial_Capital_no = coef_ftest_3_no,
    full_Psychosocial_with = coef_ftest_1_with,
    full_Capital_with = coef_ftest_2_with,
    psychosocial_Capital_with = coef_ftest_3_with
  )
  
  out
}

# Modified function to estimate models with and without controls
getEstimateGlobalComparison_rob <- function(depvar, curr_treat_var, control_vars, strata_vars, cluster_vars, followup_df, fe_var){
  
  if(curr_treat_var=="treatment_csh_trnsfr"){
    
    # MODEL WITHOUT CONTROLS (only treatment + strata)
    regmodel_no_cntrls <- paste0(depvar, " ~ ", curr_treat_var, " + ", strata_vars, paste0("| ",fe_var," | 0 |", "hhid"))
    reg_df <- followup_df %>% filter(reg_hh_csh_mrt==1)
    regmodbase_no_cntrls <- felm(formula=formula(regmodel_no_cntrls), data = reg_df)
    
    # MODEL WITH CONTROLS (treatment + strata + controls)
    regmodel_with_cntrls <- paste0(depvar, " ~ ", curr_treat_var, " + ", strata_vars, " + ", paste0(control_vars, collapse = " + "), paste0("| ",fe_var," | 0 |", "hhid"))
    regmodbase_with_cntrls <- felm(formula=formula(regmodel_with_cntrls), data = reg_df)
    
    coef_ftest_1_no = ""
    coef_ftest_2_no = ""
    coef_ftest_3_no = ""
    coef_ftest_1_with = ""
    coef_ftest_2_with = ""
    coef_ftest_3_with = ""
    
  } else if(curr_treat_var=="treatment_pi"){
    
    # MODEL WITHOUT CONTROLS (only treatment)
    regmodel_no_cntrls <- paste0(depvar, " ~ ", curr_treat_var, " + ", strata_vars, paste0("| ",fe_var," | 0 |", "cluster"))
    reg_df <- followup_df %>% filter(reg_hh_pi_mrt==1)
    regmodbase_no_cntrls <- felm(formula=formula(regmodel_no_cntrls), data = reg_df)
    
    # MODEL WITH CONTROLS (treatment + controls)
    regmodel_with_cntrls <- paste0(depvar, " ~ ", curr_treat_var, " + ", strata_vars, " + ", 
                                   paste0(control_vars, collapse = " + "), paste0("| ",fe_var," | 0 |", "cluster"))
    regmodbase_with_cntrls <- felm(formula=formula(regmodel_with_cntrls), data = reg_df)
    
    # F-tests for model WITHOUT controls
    coef_values_no <- coef(regmodbase_no_cntrls)
    
    joint_f_test_1_no <- linearHypothesis(regmodbase_no_cntrls, 
                                          c("treatment_piFull=treatment_piPsychosocial"),
                                          test = "F", singular.ok = TRUE)
    joint_f_test_2_no <- linearHypothesis(regmodbase_no_cntrls, 
                                          c("treatment_piFull=treatment_piCapital"),
                                          test = "F", singular.ok = TRUE)
    joint_f_test_3_no <- linearHypothesis(regmodbase_no_cntrls, 
                                          c("treatment_piPsychosocial = treatment_piCapital"),
                                          test = "F", singular.ok = TRUE)
    
    coef_ftest_1_no <- round(coef_values_no["treatment_piFull"] - coef_values_no["treatment_piPsychosocial"], 3)
    pval_ftest_1_no <- round(joint_f_test_1_no$`Pr(>F)`[2], 3)
    
    coef_ftest_2_no <- round(coef_values_no["treatment_piFull"] - coef_values_no["treatment_piCapital"], 3)
    pval_ftest_2_no <- round(joint_f_test_2_no$`Pr(>F)`[2], 3)
    
    coef_ftest_3_no <- round(coef_values_no["treatment_piPsychosocial"] - coef_values_no["treatment_piCapital"], 3)
    pval_ftest_3_no <- round(joint_f_test_3_no$`Pr(>F)`[2], 3)
    
    # F-tests for model WITH controls
    coef_values_with <- coef(regmodbase_with_cntrls)
    
    joint_f_test_1_with <- linearHypothesis(regmodbase_with_cntrls, 
                                            c("treatment_piFull=treatment_piPsychosocial"),
                                            test = "F", singular.ok = TRUE)
    joint_f_test_2_with <- linearHypothesis(regmodbase_with_cntrls, 
                                            c("treatment_piFull=treatment_piCapital"),
                                            test = "F", singular.ok = TRUE)
    joint_f_test_3_with <- linearHypothesis(regmodbase_with_cntrls, 
                                            c("treatment_piPsychosocial = treatment_piCapital"),
                                            test = "F", singular.ok = TRUE)
    
    coef_ftest_1_with <- round(coef_values_with["treatment_piFull"] - coef_values_with["treatment_piPsychosocial"], 3)
    pval_ftest_1_with <- round(joint_f_test_1_with$`Pr(>F)`[2], 3)
    
    coef_ftest_2_with <- round(coef_values_with["treatment_piFull"] - coef_values_with["treatment_piCapital"], 3)
    pval_ftest_2_with <- round(joint_f_test_2_with$`Pr(>F)`[2], 3)
    
    coef_ftest_3_with <- round(coef_values_with["treatment_piPsychosocial"] - coef_values_with["treatment_piCapital"], 3)
    pval_ftest_3_with <- round(joint_f_test_3_with$`Pr(>F)`[2], 3)
    
    # Add significance stars for NO controls
    for (index in 1:3) {
      pval_ftest <- get(paste0("pval_ftest_", index, "_no"))
      coef_ftest <- get(paste0("coef_ftest_", index, "_no"))
      
      if (pval_ftest <= 0.01) {
        coef_ftest <- paste0(coef_ftest, "***")
      } else if (pval_ftest <= 0.05) {
        coef_ftest <- paste0(coef_ftest, "**")
      } else if (pval_ftest <= 0.10) {
        coef_ftest <- paste0(coef_ftest, "*")
      } else {
        coef_ftest <- as.character(coef_ftest)
      }
      assign(paste0("coef_ftest_", index, "_no"), coef_ftest)
    }
    
    # Add significance stars for WITH controls
    for (index in 1:3) {
      pval_ftest <- get(paste0("pval_ftest_", index, "_with"))
      coef_ftest <- get(paste0("coef_ftest_", index, "_with"))
      
      if (pval_ftest <= 0.01) {
        coef_ftest <- paste0(coef_ftest, "***")
      } else if (pval_ftest <= 0.05) {
        coef_ftest <- paste0(coef_ftest, "**")
      } else if (pval_ftest <= 0.10) {
        coef_ftest <- paste0(coef_ftest, "*")
      } else {
        coef_ftest <- as.character(coef_ftest)
      }
      assign(paste0("coef_ftest_", index, "_with"), coef_ftest)
    }
    
  } else {
    # For pooled treatment
    
    # MODEL WITHOUT CONTROLS 
    regmodel_no_cntrls <- paste0(depvar, " ~ ", curr_treat_var, " + ", strata_vars, "| 0 | 0 |", "cluster")
    reg_df <- followup_df %>% filter(reg_hh_pi_mrt==1)
    regmodbase_no_cntrls <- felm(formula=formula(regmodel_no_cntrls), data = reg_df)
    
    # MODEL WITH CONTROLS
    regmodel_with_cntrls <- paste0(depvar, " ~ ", curr_treat_var, " + ", strata_vars, " + ", 
                                   paste0(control_vars, collapse = " + "), "| 0 | 0 |", "cluster")
    regmodbase_with_cntrls <- felm(formula=formula(regmodel_with_cntrls), data = reg_df)
    
    coef_ftest_1_no = ""
    coef_ftest_2_no = ""
    coef_ftest_3_no = ""
    coef_ftest_1_with = ""
    coef_ftest_2_with = ""
    coef_ftest_3_with = ""
  }
  
  # Return results in tibble format
  out <- tibble(
    results_no_controls = list(regmodbase_no_cntrls),
    results_with_controls = list(regmodbase_with_cntrls),
    full_Psychosocial_no = coef_ftest_1_no,
    full_Capital_no = coef_ftest_2_no,
    psychosocial_Capital_no = coef_ftest_3_no,
    full_Psychosocial_with = coef_ftest_1_with,
    full_Capital_with = coef_ftest_2_with,
    psychosocial_Capital_with = coef_ftest_3_with
  )
  
  out
}

getTable1_mech<- function(outcome_vars, mainResults_mech_hh, outcome_labels, table_title){

  followup_ipv_MRT_hh_reg <- mainResults_mech_hh %>% filter(outcome %in% outcome_vars)
  
  
  ## Cash transfer regression models
  ### Extract specific models for cash transfer from the main results
  followup_ipv_MRT_hh_control_pi_csh <- followup_ipv_MRT_hh_reg %>% filter(treat_var=="treatment_csh_trnsfr")
  
  ## EI inclusion regression models
  ### Extract specific models for EI inclusion from the main results
  followup_ipv_MRT_hh_control_pi_pi <- followup_ipv_MRT_hh_reg %>% filter(treat_var=="treatment_pi")
  
  ### Extract specific models for EI inclusion (pool) from the main results
  followup_ipv_MRT_hh_control_pi_pool <- followup_ipv_MRT_hh_reg %>% filter(treat_var=="treatment_pi_pool")
  
  # list of models per outcome
  models_csh  <- followup_ipv_MRT_hh_control_pi_csh$results_base
  models_pi  <- followup_ipv_MRT_hh_control_pi_pi$results_base
  models_pool <- followup_ipv_MRT_hh_control_pi_pool$results_base
  
  # Initialize empty vectors to store descriptive statistics
  # These will store mean and standard deviation for each outcome variable
  mean_values_pi <- c()  # Vector to store mean values
  sd_values_pi <- c()   # Vector to store standard deviation values
  mean_values_csh_trnsfr <- c()  
  sd_values_csh_trnsfr <- c()   
  # Calculate descriptive statistics for each outcome variable
  # Loop through the list of outcome variables
  for (depvar in outcome_vars) {
    # Calculate and round mean, ignoring NA values
    mean_values_pi <- c(mean_values_pi,  round(mean(followup_ipv_MRT_hh_control_pi[[depvar]], na.rm = TRUE), 3))
    mean_values_csh_trnsfr <- c(mean_values_csh_trnsfr,  round(mean(followup_ipv_MRT_hh_control_csh_trnsfr[[depvar]], na.rm = TRUE), 3))
    # Calculate and round standard deviation, ignoring NA values
    sd_values_pi <- c(sd_values_pi,  round(sd(followup_ipv_MRT_hh_control_pi[[depvar]], na.rm = TRUE), 3))
    sd_values_csh_trnsfr <- c(sd_values_csh_trnsfr,  round(sd(followup_ipv_MRT_hh_control_csh_trnsfr[[depvar]], na.rm = TRUE), 3))
  }

  # Extract interaction terms for PI (no controls)
  full_Psychosocial <- followup_ipv_MRT_hh_control_pi_pi %>% 
    select(full_Psychosocial) %>% 
    pull() 
  
  full_Capital <- followup_ipv_MRT_hh_control_pi_pi %>% 
    select(full_Capital) %>% 
    pull() 
  
  psychosocial_Capital <- followup_ipv_MRT_hh_control_pi_pi %>% 
    select(psychosocial_Capital) %>% 
    pull() 
  
  #------------------------------------------------------------
  # 1. Tekavoul panel: ONLY main treatment coefficient
  #------------------------------------------------------------

  row_tek <- sapply(seq_along(outcome_vars), function(j) {
    c("—",
      fmt_est_felm_regex(models_csh[[j]], pat_csh),
      as.character(get_n(models_csh[[j]])),
      as.character(get_r2(models_csh[[j]])))
  })
  
  tek_rows <- data.frame(
    Outcome = c("Control", "Cash Assignment", "No. Obs.", "R\u00B2"),
    row_tek,
    stringsAsFactors = FALSE
  )
  
  colnames(tek_rows) <- c("Outcome", paste0("(", seq_along(outcome_vars), ")"))
  
  #------------------------------------------------------------
  # 2. PI panel: ONLY main treatment coefficient
  #------------------------------------------------------------
  row_pi <- sapply(seq_along(outcome_vars), function(j) {
    c("—",
      fmt_est_felm_regex(models_pi[[j]],   pat_cap),
      fmt_est_felm_regex(models_pi[[j]],   pat_psy),
      fmt_est_felm_regex(models_pi[[j]],   pat_full),
      as.character(get_n(models_pi[[j]])),
      as.character(get_r2(models_pi[[j]])))
  })
  
  pi_rows <- data.frame(
    Outcome = c("Control", "Capital", "Psychosocial", "Full", "No. Obs.", "R\u00B2"),
    row_pi,
    stringsAsFactors = FALSE
  )
  
  colnames(pi_rows) <- c("Outcome", paste0("(", seq_along(outcome_vars), ")"))
  
  #------------------------------------------------------------
  # 3. PI Pool panel: ONLY main treatment coefficient
  #------------------------------------------------------------
  row_pool <- sapply(seq_along(outcome_vars), function(j) {
    c("—",
      fmt_est_felm_regex(models_pool[[j]], pat_pool),
      as.character(get_n(models_pool[[j]])),
      as.character(get_r2(models_pool[[j]])))
  })
  
  pool_rows <- data.frame(
    Outcome = c("Control", "Pool", "No. Obs.", "R\u00B2"),
    row_pool,
    stringsAsFactors = FALSE
  )
  colnames(pool_rows) <- c("Outcome", paste0("(", seq_along(outcome_vars), ")"))

  #------------------------------------------------------------
  # 4. Additional statistics: control mean & SD
  #------------------------------------------------------------
  extra_rows <- data.frame(
    Outcome = c(
      "Control Variables",
      "Control mean @ follow up Tekavoul",
      "Control SD @ follow up Tekavoul",
      "Control mean @ follow up PI",
      "Control SD @ follow up PI",
      "Full - Psychosocial",
      "Full - Capital",
      "Psychosocial - Capital"
    ),
    rbind(
      rep("Yes",length(outcome_vars)),
      as.character(mean_values_csh_trnsfr),
      as.character(sd_values_csh_trnsfr),
      as.character(mean_values_pi),
      as.character(sd_values_pi),
      as.character(full_Psychosocial),
      as.character(full_Capital),
      as.character(psychosocial_Capital)
    ),
    stringsAsFactors = FALSE
  )
  
  colnames(extra_rows) <- c("Outcome", paste0("(", seq_along(outcome_vars), ")"))

  # =========================
  # Bind everything
  # =========================
  final_tbl <- dplyr::bind_rows(
    tek_rows,
    pi_rows,
    pool_rows,
    extra_rows
  )

  # ----- compute row ranges for each panel -----
  n_tek   <- nrow(tek_rows)
  n_pi    <- nrow(pi_rows)
  n_pool  <- nrow(pool_rows)
  n_extra <- nrow(extra_rows)  # now 8 rows with the new first line
  
  i_tek_start   <- 1
  i_tek_end     <- n_tek
  i_pi_start    <- i_tek_end + 1
  i_pi_end      <- i_pi_start + n_pi   - 1
  i_pool_start  <- i_pi_end  + 1
  i_pool_end    <- i_pool_start + n_pool - 1
  i_extra_start <- i_pool_end + 1
  i_extra_end   <- i_extra_start + n_extra - 1
  
  
  #------------------------------------------------------------
  # 5. Header-above with outcome labels
  #------------------------------------------------------------
  header_above <- c(
    " " = 1,
    setNames(rep(1, length(outcome_labels)), outcome_labels)
  )
  
  # Panel/block titles (Tekavoul, EI Inclusion, Pool, Additional stats)
  panel_titles <- c(
    rep("Tekavoul",                    n_tek),
    rep("EI Inclusion",        n_pi),
    rep("EI Inclusion (Pool)", n_pool),
    rep("Additional statistics",       n_extra)
  )
  
  # Add Panel column as first column (like pack_rows labels)
  final_tbl <- final_tbl %>%
    dplyr::mutate(Panel = panel_titles, .before = 1)
  
  #------------------------------------------------------------
  # 5. Build flextable for Word (officedown)
  #------------------------------------------------------------
  # Optional: apply your global flextable defaults
  customtab_defaults()
  
  ft <- flextable::flextable(final_tbl)
  
  # Header labels
  col_keys_num <- paste0("(", seq_along(outcome_vars), ")")
  names_num    <- stats::setNames(col_keys_num, col_keys_num)
  
  ft <- flextable::set_header_labels(
    ft,
    values = c(
      Panel   = " ",        # leftmost column for block titles
      Outcome = "Treatment",
      names_num
    )
  )
  
  # Add a header row with the outcome labels (like add_header_above)
  ft <- flextable::add_header_row(
    ft,
    values    = c(" ", " ", outcome_labels),
    colwidths = c(1, 1, rep(1, length(outcome_labels)))
  )
  
  # Alignment: Panel + Outcome left, coefficients centered
  ft <- flextable::align(ft, j = 1:2, align = "left",   part = "all")
  ft <- flextable::align(ft, j = 3:ncol(final_tbl), align = "center", part = "all")
  
  # Panel separation: thicker top borders at the start of each block
  border_thick <- officer::fp_border(color = "black", width = 2)
  border_thin  <- officer::fp_border(color = "black", width = 0.5)
  
  #ft <- flextable::border_remove(ft)
  ft <- flextable::border_inner_h(ft, border = border_thin, part = "body")
  
  ft <- flextable::hline(ft, i = i_pi_start-1,    border = border_thick, part = "body")
  ft <- flextable::hline(ft, i = i_pool_start,  border = border_thick, part = "body")
  ft <- flextable::hline(ft, i = i_extra_start, border = border_thick, part = "body")
  
  # Make Panel titles (Tekavoul, EI Inclusion, etc.) merged + bold
  ft <- flextable::merge_v(ft, j = "Panel", part = "body")
  ft <- flextable::bold(ft,   j = "Panel", part = "body")
  
  # Caption
  ft <- flextable::set_caption(ft, caption = table_title)
  
  # Footnote
  ft <- flextable::add_footer_lines(
    ft,
    values = "Notes: Results presented are OLS estimates that include controls for randomization strata (commune) and, where possible, baseline outcomes. We control for social promotion intervention. Enumerator fixed effects are included in all regressions. We estimate the regressions for the EI beneficiaries aged 18–49 only. Robust standard errors are shown in parentheses, clustered at the cluster proxy level. *** p < 0.01, ** p < 0.05, * p < 0.1."
  )
  
  # Fit to page width
  ft <- FitFlextableToPage(ft)
  
  return(ft)
}

getTable_IPV_with_Holm <- function(ipv_vars,
                                   treatment_vars,
                                   control_vars,
                                   strata_vars,
                                   cluster_vars,
                                   followup_ipv_MRT_hh,
                                   followup_ipv_MRT_hh_control_pi,
                                   followup_ipv_MRT_hh_control_csh_trnsfr,
                                   header_labels,
                                   table_title,
                                   footnote_text = NULL) {
  
  # ---------------------------------------------------------------------------
  # 0. Default footnote
  # ---------------------------------------------------------------------------
  if (is.null(footnote_text)) {
    footnote_text <- paste0(
      "Notes: Results presented are OLS estimates. Columns (1) and (3) show estimates without controls ",
      "(treatment + strata only for cash transfer, treatment only for PI). Columns (2) and (4) include ",
      "controls for randomization strata (commune) and baseline covariates. We control for social promotion ",
      "intervention. Enumerator fixed effects are included in all regressions. We estimate the regressions ",
      "for the EI beneficiaries aged 18\u201349 only. Robust standard errors are shown in parentheses, ",
      "clustered at the village proxy level. For the EI Inclusion intervention, we adjust for multiple ",
      "hypothesis testing using the Holm procedure across the three treatment arms (Capital, Psychosocial, ",
      "and Full vs Control). Adjusted p-values are reported in the multiple testing adjustment section. ",
      "*** p < 0.01, ** p < 0.05, * p < 0.10 (for adjusted p-values, stars indicate significance after correction)."
    )
  }
  
  # ---------------------------------------------------------------------------
  # 1. Run main regressions (with & without controls)
  # ---------------------------------------------------------------------------
  mainResults <- purrr::map_dfr(treatment_vars, function(curr_treat_var) {
    purrr::map_dfr(ipv_vars, function(depvar) {
      getEstimateGlobalComparison(
        depvar, curr_treat_var, control_vars,
        strata_vars, cluster_vars, followup_ipv_MRT_hh
      ) %>%
        dplyr::mutate(depvar = depvar, curr_treat_var = curr_treat_var)
    })
  })
  
  # Filter by treatment arm
  res_csh  <- mainResults %>% dplyr::filter(curr_treat_var == "treatment_csh_trnsfr")
  res_pi   <- mainResults %>% dplyr::filter(curr_treat_var == "treatment_pi")
  res_pool <- mainResults %>% dplyr::filter(curr_treat_var == "treatment_pi_pool")
  
  # Model lists: one entry per outcome variable, no-controls and with-controls
  models_csh_no    <- res_csh$results_no_controls
  models_csh_with  <- res_csh$results_with_controls
  models_pi_no     <- res_pi$results_no_controls
  models_pi_with   <- res_pi$results_with_controls
  models_pool_no   <- res_pool$results_no_controls
  models_pool_with <- res_pool$results_with_controls
  
  # ---------------------------------------------------------------------------
  # 2. Extract PI interaction-term comparisons (no controls / with controls)
  # ---------------------------------------------------------------------------
  full_Psychosocial_no      <- res_pi %>% dplyr::pull(full_Psychosocial_no)
  full_Capital_no           <- res_pi %>% dplyr::pull(full_Capital_no)
  psychosocial_Capital_no   <- res_pi %>% dplyr::pull(psychosocial_Capital_no)
  
  full_Psychosocial_with    <- res_pi %>% dplyr::pull(full_Psychosocial_with)
  full_Capital_with         <- res_pi %>% dplyr::pull(full_Capital_with)
  psychosocial_Capital_with <- res_pi %>% dplyr::pull(psychosocial_Capital_with)
  
  # ---------------------------------------------------------------------------
  # 3. Descriptive statistics (control group means and SDs)
  # ---------------------------------------------------------------------------
  mean_values_pi         <- sapply(ipv_vars, function(v) round(mean(followup_ipv_MRT_hh_control_pi[[v]],        na.rm = TRUE), 3))
  sd_values_pi           <- sapply(ipv_vars, function(v) round(stats::sd(followup_ipv_MRT_hh_control_pi[[v]],    na.rm = TRUE), 3))
  mean_values_csh_trnsfr <- sapply(ipv_vars, function(v) round(mean(followup_ipv_MRT_hh_control_csh_trnsfr[[v]], na.rm = TRUE), 3))
  sd_values_csh_trnsfr   <- sapply(ipv_vars, function(v) round(stats::sd(followup_ipv_MRT_hh_control_csh_trnsfr[[v]], na.rm = TRUE), 3))
  
  # ---------------------------------------------------------------------------
  # 4. Column setup
  # ---------------------------------------------------------------------------
  n_outcomes <- length(ipv_vars)
  n_cols     <- n_outcomes * 2
  col_names  <- paste0("(", seq_len(n_cols), ")")
  
  # Helper: for outcome j, "no" column index = 2j-1, "with" column index = 2j
  idx_no   <- function(j) 2 * j - 1
  idx_with <- function(j) 2 * j
  
  # ---------------------------------------------------------------------------
  # 5. Panel 1: Tekavoul (cash transfer)
  # ---------------------------------------------------------------------------
  row_tek <- lapply(seq_len(n_outcomes), function(j) {
    list(
      no   = c("—", fmt_est_felm_regex(models_csh_no[[j]],   pat_csh),
               as.character(get_n(models_csh_no[[j]])),
               as.character(get_r2(models_csh_no[[j]]))),
      with = c("—", fmt_est_felm_regex(models_csh_with[[j]], pat_csh),
               as.character(get_n(models_csh_with[[j]])),
               as.character(get_r2(models_csh_with[[j]])))
    )
  })
  
  tek_mat <- matrix(NA_character_, nrow = 4, ncol = n_cols)
  for (j in seq_len(n_outcomes)) {
    tek_mat[, idx_no(j)]   <- row_tek[[j]]$no
    tek_mat[, idx_with(j)] <- row_tek[[j]]$with
  }
  
  tek_rows <- data.frame(
    Outcome = c("Control", "Cash Assignment", "No. Obs.", "R\u00B2"),
    as.data.frame(tek_mat, stringsAsFactors = FALSE),
    stringsAsFactors = FALSE
  )
  colnames(tek_rows) <- c("Outcome", col_names)
  
  # ---------------------------------------------------------------------------
  # 6. Panel 2: EI Inclusion (PI arms)
  # ---------------------------------------------------------------------------
  row_pi <- lapply(seq_len(n_outcomes), function(j) {
    list(
      no   = c("—",
               fmt_est_felm_regex(models_pi_no[[j]],   pat_cap),
               fmt_est_felm_regex(models_pi_no[[j]],   pat_psy),
               fmt_est_felm_regex(models_pi_no[[j]],   pat_full),
               as.character(get_n(models_pi_no[[j]])),
               as.character(get_r2(models_pi_no[[j]]))),
      with = c("—",
               fmt_est_felm_regex(models_pi_with[[j]], pat_cap),
               fmt_est_felm_regex(models_pi_with[[j]], pat_psy),
               fmt_est_felm_regex(models_pi_with[[j]], pat_full),
               as.character(get_n(models_pi_with[[j]])),
               as.character(get_r2(models_pi_with[[j]])))
    )
  })
  
  pi_mat <- matrix(NA_character_, nrow = 6, ncol = n_cols)
  for (j in seq_len(n_outcomes)) {
    pi_mat[, idx_no(j)]   <- row_pi[[j]]$no
    pi_mat[, idx_with(j)] <- row_pi[[j]]$with
  }
  
  pi_rows <- data.frame(
    Outcome = c("Control", "Capital", "Psychosocial", "Full", "No. Obs.", "R\u00B2"),
    as.data.frame(pi_mat, stringsAsFactors = FALSE),
    stringsAsFactors = FALSE
  )
  colnames(pi_rows) <- c("Outcome", col_names)
  
  # ---------------------------------------------------------------------------
  # 7. Panel 3: EI Inclusion (Pool)
  # ---------------------------------------------------------------------------
  row_pool <- lapply(seq_len(n_outcomes), function(j) {
    list(
      no   = c("—", fmt_est_felm_regex(models_pool_no[[j]],   pat_pool),
               as.character(get_n(models_pool_no[[j]])),
               as.character(get_r2(models_pool_no[[j]]))),
      with = c("—", fmt_est_felm_regex(models_pool_with[[j]], pat_pool),
               as.character(get_n(models_pool_with[[j]])),
               as.character(get_r2(models_pool_with[[j]])))
    )
  })
  
  pool_mat <- matrix(NA_character_, nrow = 4, ncol = n_cols)
  for (j in seq_len(n_outcomes)) {
    pool_mat[, idx_no(j)]   <- row_pool[[j]]$no
    pool_mat[, idx_with(j)] <- row_pool[[j]]$with
  }
  
  pool_rows <- data.frame(
    Outcome = c("Control", "Pool", "No. Obs.", "R\u00B2"),
    as.data.frame(pool_mat, stringsAsFactors = FALSE),
    stringsAsFactors = FALSE
  )
  colnames(pool_rows) <- c("Outcome", col_names)
  
  # ---------------------------------------------------------------------------
  # 8. Panel 4: Multiple Testing Adjustment (Holm)
  # ---------------------------------------------------------------------------
  adjusted_p_panel <- compute_holm_rows_PI(
    mainResults,
    depvar_binary = ipv_vars[1],
    depvar_index  = ipv_vars[2]
  )
  
  if (is.null(adjusted_p_panel)) {
    stop("compute_holm_rows_PI() returned NULL \u2013 check that mainResults has treatment_pi rows for both depvars.")
  }
  
  # Remove Panel column if it exists in the result from compute_holm_rows_PI
  if ("Panel" %in% colnames(adjusted_p_panel)) {
    adjusted_p_panel <- adjusted_p_panel %>% dplyr::select(-Panel)
  }
  
  # Ensure adjusted_p_panel has the same column structure as other panels
  colnames(adjusted_p_panel) <- c("Outcome", col_names)
  
  # ---------------------------------------------------------------------------
  # 9. Panel 5: Additional statistics
  # ---------------------------------------------------------------------------
  extra_mat <- matrix(NA_character_, nrow = 8, ncol = n_cols)
  for (j in seq_len(n_outcomes)) {
    extra_mat[1, idx_no(j)]   <- "No"
    extra_mat[1, idx_with(j)] <- "Yes"
    extra_mat[2, idx_no(j)]   <- extra_mat[2, idx_with(j)] <- as.character(mean_values_csh_trnsfr[j])
    extra_mat[3, idx_no(j)]   <- extra_mat[3, idx_with(j)] <- as.character(sd_values_csh_trnsfr[j])
    extra_mat[4, idx_no(j)]   <- extra_mat[4, idx_with(j)] <- as.character(mean_values_pi[j])
    extra_mat[5, idx_no(j)]   <- extra_mat[5, idx_with(j)] <- as.character(sd_values_pi[j])
    extra_mat[6, idx_no(j)]   <- as.character(full_Psychosocial_no[j])
    extra_mat[6, idx_with(j)] <- as.character(full_Psychosocial_with[j])
    extra_mat[7, idx_no(j)]   <- as.character(full_Capital_no[j])
    extra_mat[7, idx_with(j)] <- as.character(full_Capital_with[j])
    extra_mat[8, idx_no(j)]   <- as.character(psychosocial_Capital_no[j])
    extra_mat[8, idx_with(j)] <- as.character(psychosocial_Capital_with[j])
  }
  
  extra_rows <- data.frame(
    Outcome = c(
      "Control Variables",
      "Control mean @ follow up Tekavoul",
      "Control SD @ follow up Tekavoul",
      "Control mean @ follow up PI",
      "Control SD @ follow up PI",
      "Full - Psychosocial",
      "Full - Capital",
      "Psychosocial - Capital"
    ),
    as.data.frame(extra_mat, stringsAsFactors = FALSE),
    stringsAsFactors = FALSE
  )
  colnames(extra_rows) <- c("Outcome", col_names)
  
  # ---------------------------------------------------------------------------
  # 10. Bind all panels and add Panel label column
  # ---------------------------------------------------------------------------
  final_tbl <- dplyr::bind_rows(tek_rows, pi_rows, pool_rows, adjusted_p_panel, extra_rows)
  
  n_tek   <- nrow(tek_rows)
  n_pi    <- nrow(pi_rows)
  n_pool  <- nrow(pool_rows)
  n_adj   <- nrow(adjusted_p_panel)
  n_extra <- nrow(extra_rows)
  
  i_tek_start   <- 1
  i_tek_end     <- n_tek
  i_pi_start    <- i_tek_end + 1
  i_pi_end      <- i_pi_start + n_pi - 1
  i_pool_start  <- i_pi_end + 1
  i_pool_end    <- i_pool_start + n_pool - 1
  i_adj_start   <- i_pool_end + 1
  i_adj_end     <- i_adj_start + n_adj - 1
  i_extra_start <- i_adj_end + 1
  i_extra_end   <- i_extra_start + n_extra - 1
  
  panel_titles <- c(
    rep("Tekavoul",                           n_tek),
    rep("EI Inclusion",                       n_pi),
    rep("EI Inclusion (Pool)",                n_pool),
    rep("Multiple Testing Adjustment (Holm)", n_adj),
    rep("Additional statistics",              n_extra)
  )
  
  final_tbl <- final_tbl %>%
    dplyr::mutate(Panel = panel_titles, .before = 1)
  
  # ---------------------------------------------------------------------------
  # 11. Build flextable
  # ---------------------------------------------------------------------------
  if (exists("customtab_defaults")) customtab_defaults()
  
  ft <- flextable::flextable(final_tbl)
  
  # Base header labels
  ft <- flextable::set_header_labels(
    ft,
    values = c(
      Panel   = " ",
      Outcome = "Outcome\u2020",
      stats::setNames(col_names, col_names)
    )
  )
  
  # Top header row: outcome group labels, each spanning 2 columns (no/with ctrl)
  # Check actual number of columns in final_tbl
  n_final_cols <- ncol(final_tbl)  # Should be Panel + Outcome + n_cols
  
  # The header should span: Panel(1) + Outcome(1) + header_labels (each spans 2 data cols)
  # Verify that length(header_labels) * 2 == n_cols
  if (length(header_labels) * 2 != n_cols) {
    stop(paste0("Mismatch: header_labels has ", length(header_labels), 
                " elements but should have ", n_cols/2, 
                " (one label per outcome variable)"))
  }
  
  ft <- flextable::add_header_row(
    ft,
    values    = c(" ", " ", header_labels),
    colwidths = c(1, 1, rep(2, length(header_labels)))
  )
  
  # Alignment
  ft <- flextable::align(ft, j = 1:2,             align = "left",   part = "all")
  ft <- flextable::align(ft, j = 3:(n_cols + 2),  align = "center", part = "all")
  
  # Borders
  border_thick <- officer::fp_border(color = "black", width = 2)
  border_thin  <- officer::fp_border(color = "black", width = 0.5)
  
  ft <- flextable::border_inner_h(ft, border = border_thin,  part = "body")
  ft <- flextable::hline(ft, i = i_pi_start   - 1, border = border_thick, part = "body")
  ft <- flextable::hline(ft, i = i_pool_start - 1, border = border_thick, part = "body")
  ft <- flextable::hline(ft, i = i_adj_start  - 1, border = border_thick, part = "body")
  ft <- flextable::hline(ft, i = i_extra_start - 1, border = border_thick, part = "body")
  
  # Merge and bold the Panel column
  ft <- flextable::merge_v(ft, j = "Panel", part = "body")
  ft <- flextable::bold(ft,   j = "Panel", part = "body")
  
  # Caption and footnote
  ft <- flextable::set_caption(ft, caption = table_title)
  ft <- flextable::add_footer_lines(ft, values = footnote_text)
  
  # Fit to page width
  if (exists("FitFlextableToPage")) {
    ft <- FitFlextableToPage(ft)
  } else {
    ft <- flextable::autofit(ft)
  }
  
  return(ft)
}
# getTable_IPV_with_Holm <- function(ipv_vars,
#                                    treatment_vars,
#                                    control_vars,
#                                    strata_vars,
#                                    cluster_vars,
#                                    followup_ipv_MRT_hh,
#                                    followup_ipv_MRT_hh_control_pi,
#                                    followup_ipv_MRT_hh_control_csh_trnsfr,
#                                    header_labels,   # c("Binary label", "Index label")
#                                    table_title,
#                                    footnote_text = NULL) {
#   #------------------------------------------------------------
#   # 0. Default footnote
#   #------------------------------------------------------------
#   if (is.null(footnote_text)) {
#     footnote_text <- paste0(
#       "Notes: Results presented are OLS estimates. Columns (1) and (3) show estimates without controls ",
#       "(treatment + strata only for cash transfer, treatment only for PI). Columns (2) and (4) include ",
#       "controls for randomization strata (commune) and baseline covariates. We control for social promotion ",
#       "intervention. Enumerator fixed effects are included in all regressions. We estimate the regressions ",
#       "for the EI beneficiaries aged 18–49 only. Robust standard errors are shown in parentheses, ",
#       "clustered at the village proxy level. For the EI Inclusion intervention, we adjust for multiple ",
#       "hypothesis testing using the Holm procedure across the three treatment arms (Capital, Psychosocial, ",
#       "and Full vs Control). Adjusted p-values are reported in the multiple testing adjustment section. ",
#       "*** p < 0.01, ** p < 0.05, * p < 0.10 (for adjusted p-values, stars indicate significance after correction)."
#     )
#   }
#   
#   #------------------------------------------------------------
#   # 1. Run main regressions (with & without controls)
#   #------------------------------------------------------------
#   mainResults <- purrr::map_dfr(treatment_vars, function(curr_treat_var) {
#     purrr::map_dfr(ipv_vars, function(depvar) {
#       dplyr::bind_rows(
#         getEstimateGlobalComparison(
#           depvar,
#           curr_treat_var,
#           control_vars,
#           strata_vars,
#           cluster_vars,
#           followup_ipv_MRT_hh
#         ) %>%
#           dplyr::mutate(depvar = depvar, curr_treat_var = curr_treat_var)
#       )
#     })
#   })
#   
#   # Expecting ipv_vars length = 2 and treatment_vars length = 3:
#   # 2 outcomes × 3 treatments = 6 models in each list
#   
#   # Extract models WITHOUT controls
#   m11_no <- mainResults$results_no_controls[[1]]   # Cash, outcome 1
#   m12_no <- mainResults$results_no_controls[[2]]   # Cash, outcome 2
#   m21_no <- mainResults$results_no_controls[[3]]   # PI, outcome 1
#   m22_no <- mainResults$results_no_controls[[4]]   # PI, outcome 2
#   m31_no <- mainResults$results_no_controls[[5]]   # PI pool, outcome 1
#   m32_no <- mainResults$results_no_controls[[6]]   # PI pool, outcome 2
#   
#   # Extract models WITH controls
#   m11_with <- mainResults$results_with_controls[[1]]
#   m12_with <- mainResults$results_with_controls[[2]]
#   m21_with <- mainResults$results_with_controls[[3]]
#   m22_with <- mainResults$results_with_controls[[4]]
#   m31_with <- mainResults$results_with_controls[[5]]
#   m32_with <- mainResults$results_with_controls[[6]]
#   
#   #------------------------------------------------------------
#   # 2. Interaction terms for PI (no / with controls)
#   #------------------------------------------------------------
#   # No controls
#   full_Psychosocial_no <- mainResults %>%
#     dplyr::filter(curr_treat_var == "treatment_pi") %>%
#     dplyr::pull(full_Psychosocial_no)
#   
#   full_Capital_no <- mainResults %>%
#     dplyr::filter(curr_treat_var == "treatment_pi") %>%
#     dplyr::pull(full_Capital_no)
#   
#   psychosocial_Capital_no <- mainResults %>%
#     dplyr::filter(curr_treat_var == "treatment_pi") %>%
#     dplyr::pull(psychosocial_Capital_no)
#   
#   # With controls
#   full_Psychosocial_with <- mainResults %>%
#     dplyr::filter(curr_treat_var == "treatment_pi") %>%
#     dplyr::pull(full_Psychosocial_with)
#   
#   full_Capital_with <- mainResults %>%
#     dplyr::filter(curr_treat_var == "treatment_pi") %>%
#     dplyr::pull(full_Capital_with)
#   
#   psychosocial_Capital_with <- mainResults %>%
#     dplyr::filter(curr_treat_var == "treatment_pi") %>%
#     dplyr::pull(psychosocial_Capital_with)
#   
#   #------------------------------------------------------------
#   # 3. Descriptive stats (control means & SDs)
#   #------------------------------------------------------------
#   mean_values_pi         <- c()
#   sd_values_pi           <- c()
#   mean_values_csh_trnsfr <- c()
#   sd_values_csh_trnsfr   <- c()
#   
#   for (depvar in ipv_vars) {
#     mean_values_pi <- c(
#       mean_values_pi,
#       round(mean(followup_ipv_MRT_hh_control_pi[[depvar]], na.rm = TRUE), 3)
#     )
#     mean_values_csh_trnsfr <- c(
#       mean_values_csh_trnsfr,
#       round(mean(followup_ipv_MRT_hh_control_csh_trnsfr[[depvar]], na.rm = TRUE), 3)
#     )
#     sd_values_pi <- c(
#       sd_values_pi,
#       round(stats::sd(followup_ipv_MRT_hh_control_pi[[depvar]], na.rm = TRUE), 3)
#     )
#     sd_values_csh_trnsfr <- c(
#       sd_values_csh_trnsfr,
#       round(stats::sd(followup_ipv_MRT_hh_control_csh_trnsfr[[depvar]], na.rm = TRUE), 3)
#     )
#   }
#   
#   #------------------------------------------------------------
#   # 4. Tekavoul panel
#   #------------------------------------------------------------
#   tek_rows <- tibble::tibble(
#     Panel   = "Tekavoul",
#     Outcome = c("Control", "Cash Assignment", "No. Obs.", "R\u00B2"),
#     `(1)`   = c("—",
#                 fmt_est_felm_regex(m11_no,   pat_csh),
#                 as.character(get_n(m11_no)),
#                 as.character(get_r2(m11_no))),
#     `(2)`   = c("—",
#                 fmt_est_felm_regex(m11_with, pat_csh),
#                 as.character(get_n(m11_with)),
#                 as.character(get_r2(m11_with))),
#     `(3)`   = c("—",
#                 fmt_est_felm_regex(m12_no,   pat_csh),
#                 as.character(get_n(m12_no)),
#                 as.character(get_r2(m12_no))),
#     `(4)`   = c("—",
#                 fmt_est_felm_regex(m12_with, pat_csh),
#                 as.character(get_n(m12_with)),
#                 as.character(get_r2(m12_with)))
#   )
#   
#   #------------------------------------------------------------
#   # 5. PI panel (3 arms)
#   #------------------------------------------------------------
#   pi_rows <- tibble::tibble(
#     Panel   = "EI Inclusion",
#     Outcome = c("Control", "Capital", "Psychosocial", "Full", "No. Obs.", "R\u00B2"),
#     `(1)`   = c("—",
#                 fmt_est_felm_regex(m21_no,   pat_cap),
#                 fmt_est_felm_regex(m21_no,   pat_psy),
#                 fmt_est_felm_regex(m21_no,   pat_full),
#                 as.character(get_n(m21_no)),
#                 as.character(get_r2(m21_no))),
#     `(2)`   = c("—",
#                 fmt_est_felm_regex(m21_with, pat_cap),
#                 fmt_est_felm_regex(m21_with, pat_psy),
#                 fmt_est_felm_regex(m21_with, pat_full),
#                 as.character(get_n(m21_with)),
#                 as.character(get_r2(m21_with))),
#     `(3)`   = c("—",
#                 fmt_est_felm_regex(m22_no,   pat_cap),
#                 fmt_est_felm_regex(m22_no,   pat_psy),
#                 fmt_est_felm_regex(m22_no,   pat_full),
#                 as.character(get_n(m22_no)),
#                 as.character(get_r2(m22_no))),
#     `(4)`   = c("—",
#                 fmt_est_felm_regex(m22_with, pat_cap),
#                 fmt_est_felm_regex(m22_with, pat_psy),
#                 fmt_est_felm_regex(m22_with, pat_full),
#                 as.character(get_n(m22_with)),
#                 as.character(get_r2(m22_with)))
#   )
#   
#   #------------------------------------------------------------
#   # 6. PI (pool) panel
#   #------------------------------------------------------------
#   pi_pool_rows <- tibble::tibble(
#     Panel   = "EI Inclusion (Pool)",
#     Outcome = c("Control", "Pool", "No. Obs.", "R\u00B2"),
#     `(1)`   = c("—",
#                 fmt_est_felm_regex(m31_no,   pat_pool),
#                 as.character(get_n(m31_no)),
#                 as.character(get_r2(m31_no))),
#     `(2)`   = c("—",
#                 fmt_est_felm_regex(m31_with, pat_pool),
#                 as.character(get_n(m31_with)),
#                 as.character(get_r2(m31_with))),
#     `(3)`   = c("—",
#                 fmt_est_felm_regex(m32_no,   pat_pool),
#                 as.character(get_n(m32_no)),
#                 as.character(get_r2(m32_no))),
#     `(4)`   = c("—",
#                 fmt_est_felm_regex(m32_with, pat_pool),
#                 as.character(get_n(m32_with)),
#                 as.character(get_r2(m32_with)))
#   )
#   
#   #------------------------------------------------------------
#   # 7. Extra panel: control means/SD + interaction contrasts
#   #------------------------------------------------------------
#   final_panel <- tibble::tibble(
#     Panel   = "Additional statistics",
#     Outcome = c(
#       "Control Variables",
#       "Control mean @ follow up Tekavoul",
#       "Control SD @ follow up Tekavoul",
#       "Control mean @ follow up PI",
#       "Control SD @ follow up PI",
#       "Full - Psychosocial",
#       "Full - Capital",
#       "Psychosocial - Capital"
#     ),
#     `(1)` = c(
#       "No",
#       as.character(mean_values_csh_trnsfr[1]),
#       as.character(sd_values_csh_trnsfr[1]),
#       as.character(mean_values_pi[1]),
#       as.character(sd_values_pi[1]),
#       as.character(full_Psychosocial_no[1]),
#       as.character(full_Capital_no[1]),
#       as.character(psychosocial_Capital_no[1])
#     ),
#     `(2)` = c(
#       "Yes",
#       as.character(mean_values_csh_trnsfr[1]),
#       as.character(sd_values_csh_trnsfr[1]),
#       as.character(mean_values_pi[1]),
#       as.character(sd_values_pi[1]),
#       as.character(full_Psychosocial_with[1]),
#       as.character(full_Capital_with[1]),
#       as.character(psychosocial_Capital_with[1])
#     ),
#     `(3)` = c(
#       "No",
#       as.character(mean_values_csh_trnsfr[2]),
#       as.character(sd_values_csh_trnsfr[2]),
#       as.character(mean_values_pi[2]),
#       as.character(sd_values_pi[2]),
#       as.character(full_Psychosocial_no[2]),
#       as.character(full_Capital_no[2]),
#       as.character(psychosocial_Capital_no[2])
#     ),
#     `(4)` = c(
#       "Yes",
#       as.character(mean_values_csh_trnsfr[2]),
#       as.character(sd_values_csh_trnsfr[2]),
#       as.character(mean_values_pi[2]),
#       as.character(sd_values_pi[2]),
#       as.character(full_Psychosocial_with[2]),
#       as.character(full_Capital_with[2]),
#       as.character(psychosocial_Capital_with[2])
#     )
#   )
#   
#   #------------------------------------------------------------
#   # 8. Multiple testing panel (Holm)
#   #------------------------------------------------------------
#   adjusted_p_panel <- compute_holm_rows_PI(
#     mainResults,
#     depvar_binary = ipv_vars[1],
#     depvar_index  = ipv_vars[2]
#   )
#   
#   if (is.null(adjusted_p_panel)) {
#     stop("compute_holm_rows_PI() returned NULL – check that mainResults has treatment_pi rows for both depvars.")
#   }
#   
#   #------------------------------------------------------------
#   # 9. Bind all panels + compute row ranges
#   #------------------------------------------------------------
#   final_tbl <- dplyr::bind_rows(
#     tek_rows,
#     pi_rows,
#     pi_pool_rows,
#     adjusted_p_panel,
#     final_panel
#   )
#   
#   n_tek   <- nrow(tek_rows)
#   n_pi    <- nrow(pi_rows)
#   n_pool  <- nrow(pi_pool_rows)
#   n_adj   <- nrow(adjusted_p_panel)
#   n_extra <- nrow(final_panel)
#   
#   i_tek_start   <- 1
#   i_tek_end     <- n_tek
#   i_pi_start    <- i_tek_end + 1
#   i_pi_end      <- i_pi_start + n_pi   - 1
#   i_pool_start  <- i_pi_end  + 1
#   i_pool_end    <- i_pool_start + n_pool - 1
#   i_adj_start   <- i_pool_end + 1
#   i_adj_end     <- i_adj_start + n_adj - 1
#   i_extra_start <- i_adj_end + 1
#   i_extra_end   <- i_extra_start + n_extra - 1
#   
#   # Drop Panel column for kable printing
#   final_tbl_no_panel <- final_tbl %>% dplyr::select(-Panel)
#   
#   #------------------------------------------------------------
#   # 10. Build the HTML table (kable + kableExtra)
#   #------------------------------------------------------------
#   # header_labels should be length 2 = c("Binary label", "Index label")
#   header_above <- c(
#     " " = 1,
#     stats::setNames(rep(2, 2), header_labels)  # each label spans 2 cols
#   )
#   
#   tbl <- knitr::kable(
#     final_tbl_no_panel,
#     format   = "html",
#     escape   = FALSE,
#     align    = c("l", rep("c", 4)),
#     col.names = c("Outcome\u2020", "(1)", "(2)", "(3)", "(4)"),
#     caption  = table_title
#   ) %>%
#     kableExtra::add_header_above(header_above) %>%
#     kableExtra::kable_classic(full_width = TRUE, html_font = "Arial") %>%
#     # Tekavoul
#     kableExtra::pack_rows("Tekavoul",
#                           i_tek_start, i_tek_end,
#                           bold = TRUE, italic = FALSE) %>%
#     # EI Inclusion
#     kableExtra::row_spec(i_pi_start, extra_css = "border-top: 2px solid #000;") %>%
#     kableExtra::pack_rows("EI Inclusion",
#                           i_pi_start, i_pi_end,
#                           bold = TRUE, italic = FALSE) %>%
#     # PI (Pool)
#     kableExtra::row_spec(i_pool_start, extra_css = "border-top: 2px solid #000;") %>%
#     kableExtra::pack_rows("EI Inclusion (Pool)",
#                           i_pool_start, i_pool_end,
#                           bold = TRUE, italic = FALSE) %>%
#     # Holm section
#     kableExtra::row_spec(i_adj_start, extra_css = "border-top: 2px solid #000;") %>%
#     kableExtra::pack_rows("Multiple Testing Adjustment (Holm)",
#                           i_adj_start, i_adj_end,
#                           bold = TRUE, italic = FALSE) %>%
#     # Additional statistics
#     kableExtra::row_spec(i_extra_start, extra_css = "border-top: 2px solid #000;") %>%
#     kableExtra::pack_rows("Additional statistics",
#                           i_extra_start, i_extra_end,
#                           bold = TRUE, italic = FALSE) %>%
#     kableExtra::add_footnote(
#       label    = footnote_text,
#       notation = "none"
#     )
#   
#   return(tbl)
# }

getTable_IPV_robust <- function(ipv_vars,
                                fe_var,
                                treatment_vars,
                                control_vars,
                                strata_vars,
                                cluster_vars,
                                followup_ipv_MRT_hh,
                                followup_ipv_MRT_hh_control_pi,
                                followup_ipv_MRT_hh_control_csh_trnsfr,
                                header_labels,
                                table_title,
                                footnote_text = NULL) {
  
  # ---------------------------------------------------------------------------
  # 0. Default footnote
  # ---------------------------------------------------------------------------
  if (is.null(footnote_text)) {
    footnote_text <- paste0(
      "Notes: Results presented are OLS estimates with ", fe_var,
      " fixed effects. Columns (1) and (3) show estimates without controls ",
      "(treatment + strata only for cash transfer, treatment only for PI). ",
      "Columns (2) and (4) include controls for randomization strata (commune) ",
      "and baseline covariates. We control for social promotion intervention. ",
      "Enumerator fixed effects are included in all regressions. ",
      "We estimate the regressions for the EI beneficiaries aged 18\u201349 only. ",
      "Robust standard errors are shown in parentheses, clustered at the village proxy level. ",
      "*** p < 0.01, ** p < 0.05, * p < 0.1."
    )
  }
  
  # ---------------------------------------------------------------------------
  # 1. Run main regressions (with & without controls)
  # ---------------------------------------------------------------------------
  mainResults <- purrr::map_dfr(treatment_vars, function(curr_treat_var) {
    purrr::map_dfr(ipv_vars, function(depvar) {
      getEstimateGlobalComparison_rob(
        depvar, curr_treat_var, control_vars,
        strata_vars, cluster_vars, followup_ipv_MRT_hh, fe_var
      ) %>%
        dplyr::mutate(depvar = depvar, curr_treat_var = curr_treat_var)
    })
  })
  
  # Filter by treatment arm
  res_csh  <- mainResults %>% dplyr::filter(curr_treat_var == "treatment_csh_trnsfr")
  res_pi   <- mainResults %>% dplyr::filter(curr_treat_var == "treatment_pi")
  res_pool <- mainResults %>% dplyr::filter(curr_treat_var == "treatment_pi_pool")
  
  # Model lists: one entry per outcome variable, no-controls and with-controls
  models_csh_no    <- res_csh$results_no_controls
  models_csh_with  <- res_csh$results_with_controls
  models_pi_no     <- res_pi$results_no_controls
  models_pi_with   <- res_pi$results_with_controls
  models_pool_no   <- res_pool$results_no_controls
  models_pool_with <- res_pool$results_with_controls
  
  # ---------------------------------------------------------------------------
  # 2. Extract PI interaction-term comparisons (no controls / with controls)
  # ---------------------------------------------------------------------------
  full_Psychosocial_no      <- res_pi %>% dplyr::pull(full_Psychosocial_no)
  full_Capital_no           <- res_pi %>% dplyr::pull(full_Capital_no)
  psychosocial_Capital_no   <- res_pi %>% dplyr::pull(psychosocial_Capital_no)
  
  full_Psychosocial_with    <- res_pi %>% dplyr::pull(full_Psychosocial_with)
  full_Capital_with         <- res_pi %>% dplyr::pull(full_Capital_with)
  psychosocial_Capital_with <- res_pi %>% dplyr::pull(psychosocial_Capital_with)
  
  # ---------------------------------------------------------------------------
  # 3. Descriptive statistics (control group means and SDs)
  # ---------------------------------------------------------------------------
  mean_values_pi         <- sapply(ipv_vars, function(v) round(mean(followup_ipv_MRT_hh_control_pi[[v]],        na.rm = TRUE), 3))
  sd_values_pi           <- sapply(ipv_vars, function(v) round(sd(followup_ipv_MRT_hh_control_pi[[v]],          na.rm = TRUE), 3))
  mean_values_csh_trnsfr <- sapply(ipv_vars, function(v) round(mean(followup_ipv_MRT_hh_control_csh_trnsfr[[v]], na.rm = TRUE), 3))
  sd_values_csh_trnsfr   <- sapply(ipv_vars, function(v) round(sd(followup_ipv_MRT_hh_control_csh_trnsfr[[v]],   na.rm = TRUE), 3))
  
  # ---------------------------------------------------------------------------
  # 4. Column setup
  # ---------------------------------------------------------------------------
  n_outcomes <- length(ipv_vars)
  n_cols     <- n_outcomes * 2
  col_names  <- paste0("(", seq_len(n_cols), ")")
  
  # Helper: for outcome j, "no" column index = 2j-1, "with" column index = 2j
  idx_no   <- function(j) 2 * j - 1
  idx_with <- function(j) 2 * j
  
  # ---------------------------------------------------------------------------
  # 5. Panel 1: Tekavoul (cash transfer)
  # ---------------------------------------------------------------------------
  row_tek <- lapply(seq_len(n_outcomes), function(j) {
    list(
      no   = c("—", fmt_est_felm_regex(models_csh_no[[j]],   pat_csh),
               as.character(get_n(models_csh_no[[j]])),
               as.character(get_r2(models_csh_no[[j]]))),
      with = c("—", fmt_est_felm_regex(models_csh_with[[j]], pat_csh),
               as.character(get_n(models_csh_with[[j]])),
               as.character(get_r2(models_csh_with[[j]])))
    )
  })
  
  tek_mat <- matrix(NA_character_, nrow = 4, ncol = n_cols)
  for (j in seq_len(n_outcomes)) {
    tek_mat[, idx_no(j)]   <- row_tek[[j]]$no
    tek_mat[, idx_with(j)] <- row_tek[[j]]$with
  }
  
  tek_rows <- data.frame(
    Outcome = c("Control", "Cash Assignment", "No. Obs.", "R\u00B2"),
    as.data.frame(tek_mat, stringsAsFactors = FALSE),
    stringsAsFactors = FALSE
  )
  colnames(tek_rows) <- c("Outcome", col_names)
  
  # ---------------------------------------------------------------------------
  # 6. Panel 2: EI Inclusion (PI arms)
  # ---------------------------------------------------------------------------
  row_pi <- lapply(seq_len(n_outcomes), function(j) {
    list(
      no   = c("—",
               fmt_est_felm_regex(models_pi_no[[j]],   pat_cap),
               fmt_est_felm_regex(models_pi_no[[j]],   pat_psy),
               fmt_est_felm_regex(models_pi_no[[j]],   pat_full),
               as.character(get_n(models_pi_no[[j]])),
               as.character(get_r2(models_pi_no[[j]]))),
      with = c("—",
               fmt_est_felm_regex(models_pi_with[[j]], pat_cap),
               fmt_est_felm_regex(models_pi_with[[j]], pat_psy),
               fmt_est_felm_regex(models_pi_with[[j]], pat_full),
               as.character(get_n(models_pi_with[[j]])),
               as.character(get_r2(models_pi_with[[j]])))
    )
  })
  
  pi_mat <- matrix(NA_character_, nrow = 6, ncol = n_cols)
  for (j in seq_len(n_outcomes)) {
    pi_mat[, idx_no(j)]   <- row_pi[[j]]$no
    pi_mat[, idx_with(j)] <- row_pi[[j]]$with
  }
  
  pi_rows <- data.frame(
    Outcome = c("Control", "Capital", "Psychosocial", "Full", "No. Obs.", "R\u00B2"),
    as.data.frame(pi_mat, stringsAsFactors = FALSE),
    stringsAsFactors = FALSE
  )
  colnames(pi_rows) <- c("Outcome", col_names)
  
  # ---------------------------------------------------------------------------
  # 7. Panel 3: EI Inclusion (Pool)
  # ---------------------------------------------------------------------------
  row_pool <- lapply(seq_len(n_outcomes), function(j) {
    list(
      no   = c("—", fmt_est_felm_regex(models_pool_no[[j]],   pat_pool),
               as.character(get_n(models_pool_no[[j]])),
               as.character(get_r2(models_pool_no[[j]]))),
      with = c("—", fmt_est_felm_regex(models_pool_with[[j]], pat_pool),
               as.character(get_n(models_pool_with[[j]])),
               as.character(get_r2(models_pool_with[[j]])))
    )
  })
  
  pool_mat <- matrix(NA_character_, nrow = 4, ncol = n_cols)
  for (j in seq_len(n_outcomes)) {
    pool_mat[, idx_no(j)]   <- row_pool[[j]]$no
    pool_mat[, idx_with(j)] <- row_pool[[j]]$with
  }
  
  pool_rows <- data.frame(
    Outcome = c("Control", "Pool", "No. Obs.", "R\u00B2"),
    as.data.frame(pool_mat, stringsAsFactors = FALSE),
    stringsAsFactors = FALSE
  )
  colnames(pool_rows) <- c("Outcome", col_names)
  
  # ---------------------------------------------------------------------------
  # 8. Panel 4: Additional statistics
  # ---------------------------------------------------------------------------
  extra_mat <- matrix(NA_character_, nrow = 8, ncol = n_cols)
  for (j in seq_len(n_outcomes)) {
    extra_mat[1, idx_no(j)]   <- "No"
    extra_mat[1, idx_with(j)] <- "Yes"
    extra_mat[2, idx_no(j)]   <- extra_mat[2, idx_with(j)] <- as.character(mean_values_csh_trnsfr[j])
    extra_mat[3, idx_no(j)]   <- extra_mat[3, idx_with(j)] <- as.character(sd_values_csh_trnsfr[j])
    extra_mat[4, idx_no(j)]   <- extra_mat[4, idx_with(j)] <- as.character(mean_values_pi[j])
    extra_mat[5, idx_no(j)]   <- extra_mat[5, idx_with(j)] <- as.character(sd_values_pi[j])
    extra_mat[6, idx_no(j)]   <- as.character(full_Psychosocial_no[j])
    extra_mat[6, idx_with(j)] <- as.character(full_Psychosocial_with[j])
    extra_mat[7, idx_no(j)]   <- as.character(full_Capital_no[j])
    extra_mat[7, idx_with(j)] <- as.character(full_Capital_with[j])
    extra_mat[8, idx_no(j)]   <- as.character(psychosocial_Capital_no[j])
    extra_mat[8, idx_with(j)] <- as.character(psychosocial_Capital_with[j])
  }
  
  extra_rows <- data.frame(
    Outcome = c(
      "Control Variables",
      "Control mean @ follow up Tekavoul",
      "Control SD @ follow up Tekavoul",
      "Control mean @ follow up PI",
      "Control SD @ follow up PI",
      "Full - Psychosocial",
      "Full - Capital",
      "Psychosocial - Capital"
    ),
    as.data.frame(extra_mat, stringsAsFactors = FALSE),
    stringsAsFactors = FALSE
  )
  colnames(extra_rows) <- c("Outcome", col_names)
  
  # ---------------------------------------------------------------------------
  # 9. Bind all panels and add Panel label column
  # ---------------------------------------------------------------------------
  final_tbl <- dplyr::bind_rows(tek_rows, pi_rows, pool_rows, extra_rows)
  
  n_tek   <- nrow(tek_rows)
  n_pi    <- nrow(pi_rows)
  n_pool  <- nrow(pool_rows)
  n_extra <- nrow(extra_rows)
  
  i_tek_start   <- 1
  i_tek_end     <- n_tek
  i_pi_start    <- i_tek_end + 1
  i_pi_end      <- i_pi_start + n_pi - 1
  i_pool_start  <- i_pi_end + 1
  i_pool_end    <- i_pool_start + n_pool - 1
  i_extra_start <- i_pool_end + 1
  i_extra_end   <- i_extra_start + n_extra - 1
  
  panel_titles <- c(
    rep("Tekavoul",              n_tek),
    rep("EI Inclusion",          n_pi),
    rep("EI Inclusion (Pool)",   n_pool),
    rep("Additional statistics", n_extra)
  )
  
  final_tbl <- final_tbl %>%
    dplyr::mutate(Panel = panel_titles, .before = 1)
  
  # ---------------------------------------------------------------------------
  # 10. Build flextable
  # ---------------------------------------------------------------------------
  if (exists("customtab_defaults")) customtab_defaults()
  
  ft <- flextable::flextable(final_tbl)
  
  # Base header labels
  ft <- flextable::set_header_labels(
    ft,
    values = c(
      Panel   = " ",
      Outcome = "Outcome\u2020",
      stats::setNames(col_names, col_names)
    )
  )
  
  # Top header row: outcome group labels, each spanning 2 columns (no/with ctrl)
  ft <- flextable::add_header_row(
    ft,
    values    = c(" ", " ", header_labels),
    colwidths = c(1, 1, rep(2, length(header_labels)))
  )
  
  # Alignment
  ft <- flextable::align(ft, j = 1:2,             align = "left",   part = "all")
  ft <- flextable::align(ft, j = 3:(n_cols + 2),  align = "center", part = "all")
  
  # Borders
  border_thick <- officer::fp_border(color = "black", width = 2)
  border_thin  <- officer::fp_border(color = "black", width = 0.5)
  
  ft <- flextable::border_inner_h(ft, border = border_thin,  part = "body")
  ft <- flextable::hline(ft, i = i_pi_start   - 1, border = border_thick, part = "body")
  ft <- flextable::hline(ft, i = i_pool_start - 1, border = border_thick, part = "body")
  ft <- flextable::hline(ft, i = i_extra_start - 1, border = border_thick, part = "body")
  
  # Merge and bold the Panel column
  ft <- flextable::merge_v(ft, j = "Panel", part = "body")
  ft <- flextable::bold(ft,   j = "Panel", part = "body")
  
  # Caption and footnote
  ft <- flextable::set_caption(ft, caption = table_title)
  ft <- flextable::add_footer_lines(ft, values = footnote_text)
  
  # Fit to page width
  if (exists("FitFlextableToPage")) {
    ft <- FitFlextableToPage(ft)
  } else {
    ft <- flextable::autofit(ft)
  }
  
  return(ft)
}
# getTable_IPV_robust <- function(ipv_vars,
#                                 fe_var,
#                                 treatment_vars,
#                                 control_vars,
#                                 strata_vars,
#                                 cluster_vars,
#                                 followup_ipv_MRT_hh,
#                                 followup_ipv_MRT_hh_control_pi,
#                                 followup_ipv_MRT_hh_control_csh_trnsfr,
#                                 header_labels,   # length 2: c("Binary header", "Index header")
#                                 table_title,
#                                 footnote_text = NULL) {
#   # We expect exactly two outcomes: binary & index
#   if (length(ipv_vars) != 2) {
#     stop("getTable_IPV_robust() expects ipv_vars of length 2 (binary + index).")
#   }
#   if (length(header_labels) != 2) {
#     stop("header_labels must be a character vector of length 2.")
#   }
#   
#   # Default footnote if not provided
#   if (is.null(footnote_text)) {
#     footnote_text <- paste0(
#       "Notes: Results presented are OLS estimates with ", fe_var,
#       " fixed effects. Columns (1) and (3) show estimates without controls ",
#       "(treatment + strata only for cash transfer, treatment only for PI). ",
#       "Columns (2) and (4) include controls for randomization strata (commune) ",
#       "and baseline covariates. We control for social promotion intervention. ",
#       "Enumerator fixed effects are included in all regressions. ",
#       "We estimate the regressions for the EI beneficiaries aged 18–49 only. ",
#       "Robust standard errors are shown in parentheses, clustered at the village proxy level. ",
#       "*** p < 0.01, ** p < 0.05, * p < 0.1."
#     )
#   }
#   
#   #=========================
#   # 1) Estimate all models
#   #=========================
#   mainResults <- purrr::map_dfr(treatment_vars, function(curr_treat_var) {
#     purrr::map_dfr(ipv_vars, function(depvar) {
#       getEstimateGlobalComparison_rob(depvar, curr_treat_var, control_vars, 
#                                       strata_vars, cluster_vars, followup_ipv_MRT_hh, fe_var) %>%
#         dplyr::mutate(
#           depvar        = depvar,
#           curr_treat_var = curr_treat_var
#         )
#     })
#   })
#   
#   #=========================
#   # 2) Extract models
#   #=========================
#   # WITHOUT controls
#   m11_no <- mainResults$results_no_controls[[1]]  # CSH, outcome 1
#   m12_no <- mainResults$results_no_controls[[2]]  # CSH, outcome 2
#   m21_no <- mainResults$results_no_controls[[3]]  # PI,  outcome 1
#   m22_no <- mainResults$results_no_controls[[4]]  # PI,  outcome 2
#   m31_no <- mainResults$results_no_controls[[5]]  # Pool, outcome 1
#   m32_no <- mainResults$results_no_controls[[6]]  # Pool, outcome 2
#   
#   # WITH controls
#   m11_with <- mainResults$results_with_controls[[1]]
#   m12_with <- mainResults$results_with_controls[[2]]
#   m21_with <- mainResults$results_with_controls[[3]]
#   m22_with <- mainResults$results_with_controls[[4]]
#   m31_with <- mainResults$results_with_controls[[5]]
#   m32_with <- mainResults$results_with_controls[[6]]
#   
#   #=========================
#   # 3) Interaction terms (PI)
#   #=========================
#   full_Psychosocial_no <- mainResults %>%
#     dplyr::filter(curr_treat_var == "treatment_pi") %>%
#     dplyr::pull(full_Psychosocial_no)
#   
#   full_Capital_no <- mainResults %>%
#     dplyr::filter(curr_treat_var == "treatment_pi") %>%
#     dplyr::pull(full_Capital_no)
#   
#   psychosocial_Capital_no <- mainResults %>%
#     dplyr::filter(curr_treat_var == "treatment_pi") %>%
#     dplyr::pull(psychosocial_Capital_no)
#   
#   full_Psychosocial_with <- mainResults %>%
#     dplyr::filter(curr_treat_var == "treatment_pi") %>%
#     dplyr::pull(full_Psychosocial_with)
#   
#   full_Capital_with <- mainResults %>%
#     dplyr::filter(curr_treat_var == "treatment_pi") %>%
#     dplyr::pull(full_Capital_with)
#   
#   psychosocial_Capital_with <- mainResults %>%
#     dplyr::filter(curr_treat_var == "treatment_pi") %>%
#     dplyr::pull(psychosocial_Capital_with)
#   
#   #=========================
#   # 4) Descriptive stats
#   #=========================
#   mean_values_pi         <- c()
#   sd_values_pi           <- c()
#   mean_values_csh_trnsfr <- c()
#   sd_values_csh_trnsfr   <- c()
#   
#   for (depvar in ipv_vars) {
#     mean_values_pi <- c(
#       mean_values_pi,
#       round(mean(followup_ipv_MRT_hh_control_pi[[depvar]], na.rm = TRUE), 3)
#     )
#     mean_values_csh_trnsfr <- c(
#       mean_values_csh_trnsfr,
#       round(mean(followup_ipv_MRT_hh_control_csh_trnsfr[[depvar]], na.rm = TRUE), 3)
#     )
#     sd_values_pi <- c(
#       sd_values_pi,
#       round(sd(followup_ipv_MRT_hh_control_pi[[depvar]], na.rm = TRUE), 3)
#     )
#     sd_values_csh_trnsfr <- c(
#       sd_values_csh_trnsfr,
#       round(sd(followup_ipv_MRT_hh_control_csh_trnsfr[[depvar]], na.rm = TRUE), 3)
#     )
#   }
#   
#   #=========================
#   # 5) Build panels
#   #=========================
#   ## Panel 1: Tekavoul
#   tek_rows <- tibble::tibble(
#     Panel   = "Tekavoul",
#     Outcome = c("Control", "Cash Assignment", "No. Obs.", "R\u00B2"),
#     `(1)` = c("—",
#               fmt_est_felm_regex(m11_no,   pat_csh),
#               as.character(get_n(m11_no)),
#               as.character(get_r2(m11_no))),
#     `(2)` = c("—",
#               fmt_est_felm_regex(m11_with, pat_csh),
#               as.character(get_n(m11_with)),
#               as.character(get_r2(m11_with))),
#     `(3)` = c("—",
#               fmt_est_felm_regex(m12_no,   pat_csh),
#               as.character(get_n(m12_no)),
#               as.character(get_r2(m12_no))),
#     `(4)` = c("—",
#               fmt_est_felm_regex(m12_with, pat_csh),
#               as.character(get_n(m12_with)),
#               as.character(get_r2(m12_with)))
#   )
#   
#   ## Panel 2: EI Inclusion (3 arms)
#   pi_rows <- tibble::tibble(
#     Panel   = "EI Inclusion",
#     Outcome = c("Control", "Capital", "Psychosocial", "Full", "No. Obs.", "R\u00B2"),
#     `(1)` = c("—",
#               fmt_est_felm_regex(m21_no,   pat_cap),
#               fmt_est_felm_regex(m21_no,   pat_psy),
#               fmt_est_felm_regex(m21_no,   pat_full),
#               as.character(get_n(m21_no)),
#               as.character(get_r2(m21_no))),
#     `(2)` = c("—",
#               fmt_est_felm_regex(m21_with, pat_cap),
#               fmt_est_felm_regex(m21_with, pat_psy),
#               fmt_est_felm_regex(m21_with, pat_full),
#               as.character(get_n(m21_with)),
#               as.character(get_r2(m21_with))),
#     `(3)` = c("—",
#               fmt_est_felm_regex(m22_no,   pat_cap),
#               fmt_est_felm_regex(m22_no,   pat_psy),
#               fmt_est_felm_regex(m22_no,   pat_full),
#               as.character(get_n(m22_no)),
#               as.character(get_r2(m22_no))),
#     `(4)` = c("—",
#               fmt_est_felm_regex(m22_with, pat_cap),
#               fmt_est_felm_regex(m22_with, pat_psy),
#               fmt_est_felm_regex(m22_with, pat_full),
#               as.character(get_n(m22_with)),
#               as.character(get_r2(m22_with)))
#   )
#   
#   ## Panel 3: EI Inclusion (Pool)
#   pi_pool_rows <- tibble::tibble(
#     Panel   = "EI Inclusion (Pool)",
#     Outcome = c("Control", "Pool", "No. Obs.", "R\u00B2"),
#     `(1)` = c("—",
#               fmt_est_felm_regex(m31_no,   pat_pool),
#               as.character(get_n(m31_no)),
#               as.character(get_r2(m31_no))),
#     `(2)` = c("—",
#               fmt_est_felm_regex(m31_with, pat_pool),
#               as.character(get_n(m31_with)),
#               as.character(get_r2(m31_with))),
#     `(3)` = c("—",
#               fmt_est_felm_regex(m32_no,   pat_pool),
#               as.character(get_n(m32_no)),
#               as.character(get_r2(m32_no))),
#     `(4)` = c("—",
#               fmt_est_felm_regex(m32_with, pat_pool),
#               as.character(get_n(m32_with)),
#               as.character(get_r2(m32_with)))
#   )
#   
#   ## Panel 4: Additional statistics
#   final_panel <- tibble::tibble(
#     Panel   = "Additional statistics",
#     Outcome = c(
#       "Control Variables",
#       "Control mean @ follow up Tekavoul",
#       "Control SD @ follow up Tekavoul",
#       "Control mean @ follow up PI",
#       "Control SD @ follow up PI",
#       "Full - Psychosocial",
#       "Full - Capital",
#       "Psychosocial - Capital"
#     ),
#     `(1)` = c(
#       "No",
#       as.character(mean_values_csh_trnsfr[1]),
#       as.character(sd_values_csh_trnsfr[1]),
#       as.character(mean_values_pi[1]),
#       as.character(sd_values_pi[1]),
#       as.character(full_Psychosocial_no[1]),
#       as.character(full_Capital_no[1]),
#       as.character(psychosocial_Capital_no[1])
#     ),
#     `(2)` = c(
#       "Yes",
#       as.character(mean_values_csh_trnsfr[1]),
#       as.character(sd_values_csh_trnsfr[1]),
#       as.character(mean_values_pi[1]),
#       as.character(sd_values_pi[1]),
#       as.character(full_Psychosocial_with[1]),
#       as.character(full_Capital_with[1]),
#       as.character(psychosocial_Capital_with[1])
#     ),
#     `(3)` = c(
#       "No",
#       as.character(mean_values_csh_trnsfr[2]),
#       as.character(sd_values_csh_trnsfr[2]),
#       as.character(mean_values_pi[2]),
#       as.character(sd_values_pi[2]),
#       as.character(full_Psychosocial_no[2]),
#       as.character(full_Capital_no[2]),
#       as.character(psychosocial_Capital_no[2])
#     ),
#     `(4)` = c(
#       "Yes",
#       as.character(mean_values_csh_trnsfr[2]),
#       as.character(sd_values_csh_trnsfr[2]),
#       as.character(mean_values_pi[2]),
#       as.character(sd_values_pi[2]),
#       as.character(full_Psychosocial_with[2]),
#       as.character(full_Capital_with[2]),
#       as.character(psychosocial_Capital_with[2])
#     )
#   )
#   
#   #=========================
#   # 6) Bind & index rows
#   #=========================
#   final_tbl <- dplyr::bind_rows(
#     tek_rows,
#     pi_rows,
#     pi_pool_rows,
#     final_panel
#   )
#   
#   n_tek   <- nrow(tek_rows)
#   n_pi    <- nrow(pi_rows)
#   n_pool  <- nrow(pi_pool_rows)
#   n_extra <- nrow(final_panel)
#   
#   i_tek_start   <- 1
#   i_tek_end     <- n_tek
#   i_pi_start    <- i_tek_end + 1
#   i_pi_end      <- i_pi_start + n_pi   - 1
#   i_pool_start  <- i_pi_end  + 1
#   i_pool_end    <- i_pool_start + n_pool - 1
#   i_extra_start <- i_pool_end + 1
#   i_extra_end   <- i_extra_start + n_extra - 1
#   
#   # Drop Panel for printing
#   final_tbl_no_panel <- final_tbl %>% dplyr::select(-Panel)
#   
#   # Build the header vector properly
#   header_vec <- c(
#     " " = 1,
#     stats::setNames(rep(2, length(header_labels)), header_labels)
#   )
#   
#   #=========================
#   # 7) Render kable table
#   #=========================
#   tbl <- knitr::kable(
#     final_tbl_no_panel,
#     format    = "html",
#     escape    = FALSE,
#     align     = c("l", "c", "c", "c", "c"),
#     col.names = c("Outcome\u2020", "(1)", "(2)", "(3)", "(4)"),
#     caption   = table_title
#   ) %>%
#     kableExtra::add_header_above(header_vec) %>%
#     kableExtra::kable_classic(full_width = TRUE, html_font = "Arial") %>%
#     kableExtra::pack_rows("Tekavoul",
#                           i_tek_start, i_tek_end,
#                           bold = TRUE, italic = FALSE) %>%
#     kableExtra::row_spec(i_pi_start, extra_css = "border-top: 2px solid #000;") %>%
#     kableExtra::pack_rows("EI Inclusion",
#                           i_pi_start, i_pi_end,
#                           bold = TRUE, italic = FALSE) %>%
#     kableExtra::row_spec(i_pool_start, extra_css = "border-top: 2px solid #000;") %>%
#     kableExtra::pack_rows("EI Inclusion (Pool)",
#                           i_pool_start, i_pool_end,
#                           bold = TRUE, italic = FALSE) %>%
#     kableExtra::row_spec(i_extra_start, extra_css = "border-top: 2px solid #000;") %>%
#     kableExtra::pack_rows("Additional statistics",
#                           i_extra_start, i_extra_end,
#                           bold = TRUE, italic = FALSE) %>%
#     kableExtra::add_footnote(
#       label    = footnote_text,
#       notation = "none"
#     )
#   
#   return(tbl)
# }
# 

getTable_IPV_nonlin <- function(ipv_vars_bin,
                                treatment_vars,
                                control_vars,
                                strata_vars,
                                cluster_vars,
                                followup_ipv_MRT_hh,
                                followup_ipv_MRT_hh_control_pi,
                                followup_ipv_MRT_hh_control_csh_trnsfr,
                                header_labels,
                                table_title,
                                footnote_text = NULL) {
  
  # ---------------------------------------------------------------------------
  # 0. Default footnote
  # ---------------------------------------------------------------------------
  if (is.null(footnote_text)) {
    footnote_text <- paste(
      "Notes: Results presented are nonlinear probability models (GLM with logit link).",
      "Column (1) shows estimates without controls (treatment + strata only for cash transfer, treatment only for PI).",
      "Column (2) includes controls for randomization strata (commune) and baseline covariates.",
      "We estimate the regressions for the EI beneficiaries aged 18\u201349 only.",
      "Robust standard errors are shown in parentheses, clustered at the village proxy level.",
      "*** p < 0.01, ** p < 0.05, * p < 0.1.",
      sep = " "
    )
  }
  
  # ---------------------------------------------------------------------------
  # 1. Run main regressions (with & without controls)
  # ---------------------------------------------------------------------------
  mainResults <- purrr::map_dfr(treatment_vars, function(curr_treat_var) {
    purrr::map_dfr(ipv_vars_bin, function(depvar) {
      getEstimateGlobalComparison_rob_nonlin(
        depvar, curr_treat_var, control_vars,
        strata_vars, cluster_vars, followup_ipv_MRT_hh
      ) %>%
        dplyr::mutate(depvar = depvar, curr_treat_var = curr_treat_var)
    })
  })
  
  # Filter by treatment arm
  res_csh  <- mainResults %>% dplyr::filter(curr_treat_var == "treatment_csh_trnsfr")
  res_pi   <- mainResults %>% dplyr::filter(curr_treat_var == "treatment_pi")
  res_pool <- mainResults %>% dplyr::filter(curr_treat_var == "treatment_pi_pool")
  
  # Model lists: one entry per outcome variable, no-controls and with-controls
  models_csh_no    <- res_csh$results_no_controls
  models_csh_with  <- res_csh$results_with_controls
  models_pi_no     <- res_pi$results_no_controls
  models_pi_with   <- res_pi$results_with_controls
  models_pool_no   <- res_pool$results_no_controls
  models_pool_with <- res_pool$results_with_controls
  
  # ---------------------------------------------------------------------------
  # 2. Extract PI interaction-term comparisons (no controls / with controls)
  # ---------------------------------------------------------------------------
  full_Psychosocial_no      <- res_pi %>% dplyr::pull(full_Psychosocial_no)
  full_Capital_no           <- res_pi %>% dplyr::pull(full_Capital_no)
  psychosocial_Capital_no   <- res_pi %>% dplyr::pull(psychosocial_Capital_no)
  
  full_Psychosocial_with    <- res_pi %>% dplyr::pull(full_Psychosocial_with)
  full_Capital_with         <- res_pi %>% dplyr::pull(full_Capital_with)
  psychosocial_Capital_with <- res_pi %>% dplyr::pull(psychosocial_Capital_with)
  
  # ---------------------------------------------------------------------------
  # 3. Descriptive statistics (control group means and SDs)
  # ---------------------------------------------------------------------------
  mean_values_pi         <- sapply(ipv_vars_bin, function(v) round(mean(followup_ipv_MRT_hh_control_pi[[v]],        na.rm = TRUE), 3))
  sd_values_pi           <- sapply(ipv_vars_bin, function(v) round(sd(followup_ipv_MRT_hh_control_pi[[v]],          na.rm = TRUE), 3))
  mean_values_csh_trnsfr <- sapply(ipv_vars_bin, function(v) round(mean(followup_ipv_MRT_hh_control_csh_trnsfr[[v]], na.rm = TRUE), 3))
  sd_values_csh_trnsfr   <- sapply(ipv_vars_bin, function(v) round(sd(followup_ipv_MRT_hh_control_csh_trnsfr[[v]],   na.rm = TRUE), 3))
  
  # ---------------------------------------------------------------------------
  # 4. Column setup
  # ---------------------------------------------------------------------------
  n_outcomes <- length(ipv_vars_bin)
  n_cols     <- n_outcomes * 2
  col_names  <- paste0("(", seq_len(n_cols), ")")
  
  # Helper: for outcome j, "no" column index = 2j-1, "with" column index = 2j
  idx_no   <- function(j) 2 * j - 1
  idx_with <- function(j) 2 * j
  
  # ---------------------------------------------------------------------------
  # 5. Panel 1: Tekavoul (cash transfer)
  # ---------------------------------------------------------------------------
  row_tek <- lapply(seq_len(n_outcomes), function(j) {
    list(
      no   = c("—", fmt_est_feglm_regex(models_csh_no[[j]],   pat_csh),
               as.character(get_n(models_csh_no[[j]])),
               as.character(get_pseudo_r2(models_csh_no[[j]]))),
      with = c("—", fmt_est_feglm_regex(models_csh_with[[j]], pat_csh),
               as.character(get_n(models_csh_with[[j]])),
               as.character(get_pseudo_r2(models_csh_with[[j]])))
    )
  })
  
  tek_mat <- matrix(NA_character_, nrow = 4, ncol = n_cols)
  for (j in seq_len(n_outcomes)) {
    tek_mat[, idx_no(j)]   <- row_tek[[j]]$no
    tek_mat[, idx_with(j)] <- row_tek[[j]]$with
  }
  
  tek_rows <- data.frame(
    Outcome = c("Control", "Cash Assignment", "No. Obs.", "Pseudo R\u00B2"),
    as.data.frame(tek_mat, stringsAsFactors = FALSE),
    stringsAsFactors = FALSE
  )
  colnames(tek_rows) <- c("Outcome", col_names)
  
  # ---------------------------------------------------------------------------
  # 6. Panel 2: EI Inclusion (PI arms)
  # ---------------------------------------------------------------------------
  row_pi <- lapply(seq_len(n_outcomes), function(j) {
    list(
      no   = c("—",
               fmt_est_feglm_regex(models_pi_no[[j]],   pat_cap),
               fmt_est_feglm_regex(models_pi_no[[j]],   pat_psy),
               fmt_est_feglm_regex(models_pi_no[[j]],   pat_full),
               as.character(get_n(models_pi_no[[j]])),
               as.character(get_pseudo_r2(models_pi_no[[j]]))),
      with = c("—",
               fmt_est_feglm_regex(models_pi_with[[j]], pat_cap),
               fmt_est_feglm_regex(models_pi_with[[j]], pat_psy),
               fmt_est_feglm_regex(models_pi_with[[j]], pat_full),
               as.character(get_n(models_pi_with[[j]])),
               as.character(get_pseudo_r2(models_pi_with[[j]])))
    )
  })
  
  pi_mat <- matrix(NA_character_, nrow = 6, ncol = n_cols)
  for (j in seq_len(n_outcomes)) {
    pi_mat[, idx_no(j)]   <- row_pi[[j]]$no
    pi_mat[, idx_with(j)] <- row_pi[[j]]$with
  }
  
  pi_rows <- data.frame(
    Outcome = c("Control", "Capital", "Psychosocial", "Full", "No. Obs.", "Pseudo R\u00B2"),
    as.data.frame(pi_mat, stringsAsFactors = FALSE),
    stringsAsFactors = FALSE
  )
  colnames(pi_rows) <- c("Outcome", col_names)
  
  # ---------------------------------------------------------------------------
  # 7. Panel 3: EI Inclusion (Pool)
  # ---------------------------------------------------------------------------
  row_pool <- lapply(seq_len(n_outcomes), function(j) {
    list(
      no   = c("—", fmt_est_feglm_regex(models_pool_no[[j]],   pat_pool),
               as.character(get_n(models_pool_no[[j]])),
               as.character(get_pseudo_r2(models_pool_no[[j]]))),
      with = c("—", fmt_est_feglm_regex(models_pool_with[[j]], pat_pool),
               as.character(get_n(models_pool_with[[j]])),
               as.character(get_pseudo_r2(models_pool_with[[j]])))
    )
  })
  
  pool_mat <- matrix(NA_character_, nrow = 4, ncol = n_cols)
  for (j in seq_len(n_outcomes)) {
    pool_mat[, idx_no(j)]   <- row_pool[[j]]$no
    pool_mat[, idx_with(j)] <- row_pool[[j]]$with
  }
  
  pool_rows <- data.frame(
    Outcome = c("Control", "Pool", "No. Obs.", "Pseudo R\u00B2"),
    as.data.frame(pool_mat, stringsAsFactors = FALSE),
    stringsAsFactors = FALSE
  )
  colnames(pool_rows) <- c("Outcome", col_names)
  
  # ---------------------------------------------------------------------------
  # 8. Panel 4: Additional statistics
  # ---------------------------------------------------------------------------
  extra_mat <- matrix(NA_character_, nrow = 8, ncol = n_cols)
  for (j in seq_len(n_outcomes)) {
    extra_mat[1, idx_no(j)]   <- "No"
    extra_mat[1, idx_with(j)] <- "Yes"
    extra_mat[2, idx_no(j)]   <- extra_mat[2, idx_with(j)] <- as.character(mean_values_csh_trnsfr[j])
    extra_mat[3, idx_no(j)]   <- extra_mat[3, idx_with(j)] <- as.character(sd_values_csh_trnsfr[j])
    extra_mat[4, idx_no(j)]   <- extra_mat[4, idx_with(j)] <- as.character(mean_values_pi[j])
    extra_mat[5, idx_no(j)]   <- extra_mat[5, idx_with(j)] <- as.character(sd_values_pi[j])
    extra_mat[6, idx_no(j)]   <- as.character(full_Psychosocial_no[j])
    extra_mat[6, idx_with(j)] <- as.character(full_Psychosocial_with[j])
    extra_mat[7, idx_no(j)]   <- as.character(full_Capital_no[j])
    extra_mat[7, idx_with(j)] <- as.character(full_Capital_with[j])
    extra_mat[8, idx_no(j)]   <- as.character(psychosocial_Capital_no[j])
    extra_mat[8, idx_with(j)] <- as.character(psychosocial_Capital_with[j])
  }
  
  extra_rows <- data.frame(
    Outcome = c(
      "Control Variables",
      "Control mean @ follow up Tekavoul",
      "Control SD @ follow up Tekavoul",
      "Control mean @ follow up PI",
      "Control SD @ follow up PI",
      "Full - Psychosocial",
      "Full - Capital",
      "Psychosocial - Capital"
    ),
    as.data.frame(extra_mat, stringsAsFactors = FALSE),
    stringsAsFactors = FALSE
  )
  colnames(extra_rows) <- c("Outcome", col_names)
  
  # ---------------------------------------------------------------------------
  # 9. Bind all panels and add Panel label column
  # ---------------------------------------------------------------------------
  final_tbl <- dplyr::bind_rows(tek_rows, pi_rows, pool_rows, extra_rows)
  
  n_tek   <- nrow(tek_rows)
  n_pi    <- nrow(pi_rows)
  n_pool  <- nrow(pool_rows)
  n_extra <- nrow(extra_rows)
  
  i_tek_start   <- 1
  i_tek_end     <- n_tek
  i_pi_start    <- i_tek_end + 1
  i_pi_end      <- i_pi_start + n_pi - 1
  i_pool_start  <- i_pi_end + 1
  i_pool_end    <- i_pool_start + n_pool - 1
  i_extra_start <- i_pool_end + 1
  i_extra_end   <- i_extra_start + n_extra - 1
  
  panel_titles <- c(
    rep("Tekavoul",              n_tek),
    rep("EI Inclusion",          n_pi),
    rep("EI Inclusion (Pool)",   n_pool),
    rep("Additional statistics", n_extra)
  )
  
  final_tbl <- final_tbl %>%
    dplyr::mutate(Panel = panel_titles, .before = 1)
  
  # ---------------------------------------------------------------------------
  # 10. Build flextable
  # ---------------------------------------------------------------------------
  if (exists("customtab_defaults")) customtab_defaults()
  
  ft <- flextable::flextable(final_tbl)
  
  # Base header labels
  ft <- flextable::set_header_labels(
    ft,
    values = c(
      Panel   = " ",
      Outcome = "Outcome\u2020",
      stats::setNames(col_names, col_names)
    )
  )
  
  # Top header row: outcome group labels, each spanning 2 columns (no/with ctrl)
  ft <- flextable::add_header_row(
    ft,
    values    = c(" ", " ", header_labels),
    colwidths = c(1, 1, rep(2, length(header_labels)))
  )
  
  # Alignment
  ft <- flextable::align(ft, j = 1:2,             align = "left",   part = "all")
  ft <- flextable::align(ft, j = 3:(n_cols + 2),  align = "center", part = "all")
  
  # Borders
  border_thick <- officer::fp_border(color = "black", width = 2)
  border_thin  <- officer::fp_border(color = "black", width = 0.5)
  
  ft <- flextable::border_inner_h(ft, border = border_thin,  part = "body")
  ft <- flextable::hline(ft, i = i_pi_start   - 1, border = border_thick, part = "body")
  ft <- flextable::hline(ft, i = i_pool_start - 1, border = border_thick, part = "body")
  ft <- flextable::hline(ft, i = i_extra_start - 1, border = border_thick, part = "body")
  
  # Merge and bold the Panel column
  ft <- flextable::merge_v(ft, j = "Panel", part = "body")
  ft <- flextable::bold(ft,   j = "Panel", part = "body")
  
  # Caption and footnote
  ft <- flextable::set_caption(ft, caption = table_title)
  ft <- flextable::add_footer_lines(ft, values = footnote_text)
  
  # Fit to page width
  if (exists("FitFlextableToPage")) {
    ft <- FitFlextableToPage(ft)
  } else {
    ft <- flextable::autofit(ft)
  }
  
  return(ft)
}
 

# getTable_IPV_nonlin <- function(ipv_vars_bin,
#                                 treatment_vars,
#                                 control_vars,
#                                 strata_vars,
#                                 cluster_vars,
#                                 followup_ipv_MRT_hh,
#                                 followup_ipv_MRT_hh_control_pi,
#                                 followup_ipv_MRT_hh_control_csh_trnsfr,
#                                 header_label,
#                                 table_title,
#                                 footnote_text = NULL) {
#   #---------------------------
#   # Default footnote
#   #---------------------------
#   if (is.null(footnote_text)) {
#     footnote_text <- paste(
#       "Notes: Results presented are nonlinear probability models (GLM with logit link).",
#       "Column (1) shows estimates without controls (treatment + strata only for cash transfer, treatment only for PI).",
#       "Column (2) includes controls for randomization strata (commune) and baseline covariates.",
#       "We estimate the regressions for the EI beneficiaries aged 18–49 only.",
#       "Robust standard errors are shown in parentheses, clustered at the village proxy level.",
#       "*** p < 0.01, ** p < 0.05, * p < 0.1.",
#       sep = " "
#     )
#   }
#   
#   #---------------------------
#   # 1) Main estimation
#   #---------------------------
#   mainResults <- purrr::map_dfr(treatment_vars, function(curr_treat_var) {
#     purrr::map_dfr(ipv_vars_bin, function(depvar) {
#       getEstimateGlobalComparison_rob_nonlin(depvar,curr_treat_var,control_vars,strata_vars,cluster_vars,followup_ipv_MRT_hh
#       ) %>%
#         dplyr::mutate(
#           depvar         = depvar,
#           curr_treat_var = curr_treat_var
#         )
#     })
#   })
# 
#   #---------------------------
#   # 2) Extract models
#   #   (assumes 1 depvar x 3 treatment vars = 3 models)
#   #---------------------------
#   # WITHOUT controls
#   m11_no <- mainResults$results_no_controls[[1]]  # Cash
#   m21_no <- mainResults$results_no_controls[[2]]  # PI
#   m31_no <- mainResults$results_no_controls[[3]]  # PI pool
#   
#   # WITH controls
#   m11_with <- mainResults$results_with_controls[[1]]
#   m21_with <- mainResults$results_with_controls[[2]]
#   m31_with <- mainResults$results_with_controls[[3]]
#   
#   #---------------------------
#   # 3) Extract interaction terms (PI)
#   #---------------------------
#   full_Psychosocial_no <- mainResults %>%
#     dplyr::filter(curr_treat_var == "treatment_pi") %>%
#     dplyr::pull(full_Psychosocial_no)
#   
#   full_Capital_no <- mainResults %>%
#     dplyr::filter(curr_treat_var == "treatment_pi") %>%
#     dplyr::pull(full_Capital_no)
#   
#   psychosocial_Capital_no <- mainResults %>%
#     dplyr::filter(curr_treat_var == "treatment_pi") %>%
#     dplyr::pull(psychosocial_Capital_no)
#   
#   full_Psychosocial_with <- mainResults %>%
#     dplyr::filter(curr_treat_var == "treatment_pi") %>%
#     dplyr::pull(full_Psychosocial_with)
#   
#   full_Capital_with <- mainResults %>%
#     dplyr::filter(curr_treat_var == "treatment_pi") %>%
#     dplyr::pull(full_Capital_with)
#   
#   psychosocial_Capital_with <- mainResults %>%
#     dplyr::filter(curr_treat_var == "treatment_pi") %>%
#     dplyr::pull(psychosocial_Capital_with)
#   
#   #---------------------------
#   # 4) Descriptive statistics
#   #---------------------------
#   mean_values_pi        <- c()
#   sd_values_pi          <- c()
#   mean_values_csh_trnsfr <- c()
#   sd_values_csh_trnsfr   <- c()
#   
#   for (depvar in ipv_vars_bin) {
#     mean_values_pi <- c(
#       mean_values_pi,
#       round(mean(followup_ipv_MRT_hh_control_pi[[depvar]], na.rm = TRUE), 3)
#     )
#     mean_values_csh_trnsfr <- c(
#       mean_values_csh_trnsfr,
#       round(mean(followup_ipv_MRT_hh_control_csh_trnsfr[[depvar]], na.rm = TRUE), 3)
#     )
#     
#     sd_values_pi <- c(
#       sd_values_pi,
#       round(sd(followup_ipv_MRT_hh_control_pi[[depvar]], na.rm = TRUE), 3)
#     )
#     sd_values_csh_trnsfr <- c(
#       sd_values_csh_trnsfr,
#       round(sd(followup_ipv_MRT_hh_control_csh_trnsfr[[depvar]], na.rm = TRUE), 3)
#     )
#   }
#   
#   # For the table, we use the first element (assuming one binary depvar)
#   mv_pi_1        <- as.character(mean_values_pi[1])
#   sd_pi_1        <- as.character(sd_values_pi[1])
#   mv_csh_1       <- as.character(mean_values_csh_trnsfr[1])
#   sd_csh_1       <- as.character(sd_values_csh_trnsfr[1])
#   
#   fp_no_1        <- as.character(full_Psychosocial_no[1])
#   fc_no_1        <- as.character(full_Capital_no[1])
#   pc_no_1        <- as.character(psychosocial_Capital_no[1])
#   
#   fp_with_1      <- as.character(full_Psychosocial_with[1])
#   fc_with_1      <- as.character(full_Capital_with[1])
#   pc_with_1      <- as.character(psychosocial_Capital_with[1])
#   
#   #---------------------------
#   # 5) Build panels
#   #---------------------------
#   
#   # Panel 1: Tekavoul
#   tek_rows <- tibble::tibble(
#     Panel   = "Tekavoul",
#     Outcome = c("Control", "Cash Assignment", "No. Obs.", "Pseudo R²"),
#     `(1)`   = c(
#       "—",
#       fmt_est_feglm_regex(m11_no,   pat_csh),
#       as.character(get_n(m11_no)),
#       as.character(get_pseudo_r2(m11_no))
#     ),
#     `(2)`   = c(
#       "—",
#       fmt_est_feglm_regex(m11_with, pat_csh),
#       as.character(get_n(m11_with)),
#       as.character(get_pseudo_r2(m11_with))
#     )
#   )
#   
#   # Panel 2: EI Inclusion (3 arms)
#   pi_rows <- tibble::tibble(
#     Panel   = "EI Inclusion",
#     Outcome = c("Control", "Capital", "Psychosocial", "Full", "No. Obs.", "Pseudo R²"),
#     `(1)`   = c(
#       "—",
#       fmt_est_feglm_regex(m21_no, pat_cap),
#       fmt_est_feglm_regex(m21_no, pat_psy),
#       fmt_est_feglm_regex(m21_no, pat_full),
#       as.character(get_n(m21_no)),
#       as.character(get_pseudo_r2(m21_no))
#     ),
#     `(2)`   = c(
#       "—",
#       fmt_est_feglm_regex(m21_with, pat_cap),
#       fmt_est_feglm_regex(m21_with, pat_psy),
#       fmt_est_feglm_regex(m21_with, pat_full),
#       as.character(get_n(m21_with)),
#       as.character(get_pseudo_r2(m21_with))
#     )
#   )
#   
#   # Panel 3: PI (Pool)
#   pi_pool_rows <- tibble::tibble(
#     Panel   = "EI Inclusion (Pool)",
#     Outcome = c("Control", "Pool", "No. Obs.", "Pseudo R²"),
#     `(1)`   = c(
#       "—",
#       fmt_est_feglm_regex(m31_no,   pat_pool),
#       as.character(get_n(m31_no)),
#       as.character(get_pseudo_r2(m31_no))
#     ),
#     `(2)`   = c(
#       "—",
#       fmt_est_feglm_regex(m31_with, pat_pool),
#       as.character(get_n(m31_with)),
#       as.character(get_pseudo_r2(m31_with))
#     )
#   )
#   
#   # Panel 4: Additional statistics
#   final_panel <- tibble::tibble(
#     Panel   = "Additional statistics",
#     Outcome = c(
#       "Control Variables",
#       "Control mean @ follow up Tekavoul",
#       "Control SD @ follow up Tekavoul",
#       "Control mean @ follow up PI",
#       "Control SD @ follow up PI",
#       "Full - Psychosocial",
#       "Full - Capital",
#       "Psychosocial - Capital"
#     ),
#     `(1)`   = c(
#       "No",
#       mv_csh_1,
#       sd_csh_1,
#       mv_pi_1,
#       sd_pi_1,
#       fp_no_1,
#       fc_no_1,
#       pc_no_1
#     ),
#     `(2)`   = c(
#       "Yes",
#       mv_csh_1,
#       sd_csh_1,
#       mv_pi_1,
#       sd_pi_1,
#       fp_with_1,
#       fc_with_1,
#       pc_with_1
#     )
#   )
#   
#   #---------------------------
#   # 6) Bind + indices
#   #---------------------------
#   final_tbl <- dplyr::bind_rows(
#     tek_rows,
#     pi_rows,
#     pi_pool_rows,
#     final_panel
#   )
#   
#   n_tek   <- nrow(tek_rows)
#   n_pi    <- nrow(pi_rows)
#   n_pool  <- nrow(pi_pool_rows)
#   n_extra <- nrow(final_panel)
#   
#   i_tek_start   <- 1
#   i_tek_end     <- n_tek
#   i_pi_start    <- i_tek_end + 1
#   i_pi_end      <- i_pi_start + n_pi   - 1
#   i_pool_start  <- i_pi_end  + 1
#   i_pool_end    <- i_pool_start + n_pool - 1
#   i_extra_start <- i_pool_end + 1
#   i_extra_end   <- i_extra_start + n_extra - 1
#   
#   # Drop Panel column for printing
#   final_tbl_no_panel <- final_tbl %>% dplyr::select(-Panel)
#   
#   #---------------------------
#   # 7) Build header vector safely
#   #---------------------------
#   header_vec <- c(1, 2)
#   names(header_vec) <- c(" ", header_label)
#   
#   #---------------------------
#   # 8) Render kable
#   #---------------------------
#   tbl <- knitr::kable(
#     final_tbl_no_panel,
#     format    = "html",
#     escape    = FALSE,
#     align     = c("l", "c", "c"),
#     col.names = c("Outcome\u2020", "(1)", "(2)")
#   ) %>%
#     kableExtra::add_header_above(header_vec) %>%
#     kableExtra::kable_classic(full_width = TRUE, html_font = "Arial") %>%
#     # Panels
#     kableExtra::pack_rows("Tekavoul",
#                           i_tek_start, i_tek_end,
#                           bold = TRUE, italic = FALSE) %>%
#     kableExtra::row_spec(i_pi_start, extra_css = "border-top: 2px solid #000;") %>%
#     kableExtra::pack_rows("EI Inclusion",
#                           i_pi_start, i_pi_end,
#                           bold = TRUE, italic = FALSE) %>%
#     kableExtra::row_spec(i_pool_start, extra_css = "border-top: 2px solid #000;") %>%
#     kableExtra::pack_rows("EI Inclusion (Pool)",
#                           i_pool_start, i_pool_end,
#                           bold = TRUE, italic = FALSE) %>%
#     kableExtra::row_spec(i_extra_start, extra_css = "border-top: 2px solid #000;") %>%
#     kableExtra::pack_rows("Additional statistics",
#                           i_extra_start, i_extra_end,
#                           bold = TRUE, italic = FALSE) %>%
#     kableExtra::add_footnote(
#       label    = footnote_text,
#       notation = "none"
#     )
#   
#   return(tbl)
# }
getTable_IPV_spill <- function(ipv_vars,
                               binary_header,
                               index_header,
                               table_title,
                               treatment_vars,
                               control_vars,
                               strata_vars,
                               cluster_vars,
                               followup_ipv_MRT_hh_spill,
                               followup_ipv_MRT_hh_spill_control_pi,
                               footnote_text = NULL) {
  
  # ---------------------------------------------------------------------------
  # 0. Default footnote
  # ---------------------------------------------------------------------------
  if (is.null(footnote_text)) {
    footnote_text <- paste(
      "Notes: Results presented are OLS estimates.",
      "Columns (1) and (3) show estimates without controls (treatment + strata only for cash transfer, treatment only for PI).",
      "Columns (2) and (4) include controls for randomization strata (commune) and baseline covariates.",
      "We control for social promotion intervention. Enumerator fixed effects are included in all regressions.",
      "We estimate the regressions for the EI beneficiaries aged 18\u201349 only.",
      "Robust standard errors are shown in parentheses, clustered at the village proxy level.",
      "*** p < 0.01, ** p < 0.05, * p < 0.1.",
      sep = " "
    )
  }
  
  # Combine the two header parameters into header_labels vector
  header_labels <- c(binary_header, index_header)
  
  # ---------------------------------------------------------------------------
  # 1. Run main regressions (with & without controls)
  # ---------------------------------------------------------------------------
  mainResults <- purrr::map_dfr(treatment_vars, function(curr_treat_var) {
    purrr::map_dfr(ipv_vars, function(depvar) {
      getEstimateGlobal_spill(
        depvar, curr_treat_var, control_vars,
        strata_vars, cluster_vars, followup_ipv_MRT_hh_spill
      ) %>%
        dplyr::mutate(depvar = depvar, curr_treat_var = curr_treat_var)
    })
  })
  
  # Filter by treatment arm
  res_pi   <- mainResults %>% dplyr::filter(curr_treat_var == "treatment_pi_spill")
  res_pool <- mainResults %>% dplyr::filter(curr_treat_var == "treatment_pi_pool_spill")
  
  # Model lists: one entry per outcome variable, no-controls and with-controls
  models_pi_no     <- res_pi$results_no_controls
  models_pi_with   <- res_pi$results_with_controls
  models_pool_no   <- res_pool$results_no_controls
  models_pool_with <- res_pool$results_with_controls
  
  # ---------------------------------------------------------------------------
  # 2. Extract PI interaction-term comparisons (no controls / with controls)
  # ---------------------------------------------------------------------------
  full_Psychosocial_no      <- res_pi %>% dplyr::pull(full_Psychosocial_no)
  full_Capital_no           <- res_pi %>% dplyr::pull(full_Capital_no)
  psychosocial_Capital_no   <- res_pi %>% dplyr::pull(psychosocial_Capital_no)
  
  full_Psychosocial_with    <- res_pi %>% dplyr::pull(full_Psychosocial_with)
  full_Capital_with         <- res_pi %>% dplyr::pull(full_Capital_with)
  psychosocial_Capital_with <- res_pi %>% dplyr::pull(psychosocial_Capital_with)
  
  # ---------------------------------------------------------------------------
  # 3. Descriptive statistics (control group means and SDs)
  # ---------------------------------------------------------------------------
  mean_values_pi <- sapply(ipv_vars, function(v) round(mean(followup_ipv_MRT_hh_spill_control_pi[[v]], na.rm = TRUE), 3))
  sd_values_pi   <- sapply(ipv_vars, function(v) round(sd(followup_ipv_MRT_hh_spill_control_pi[[v]],   na.rm = TRUE), 3))
  
  # ---------------------------------------------------------------------------
  # 4. Column setup
  # ---------------------------------------------------------------------------
  n_outcomes <- length(ipv_vars)
  n_cols     <- n_outcomes * 2
  col_names  <- paste0("(", seq_len(n_cols), ")")
  
  # Helper: for outcome j, "no" column index = 2j-1, "with" column index = 2j
  idx_no   <- function(j) 2 * j - 1
  idx_with <- function(j) 2 * j
  
  # ---------------------------------------------------------------------------
  # 5. Panel 1: EI Inclusion (PI arms)
  # ---------------------------------------------------------------------------
  row_pi <- lapply(seq_len(n_outcomes), function(j) {
    list(
      no   = c("—",
               fmt_est_felm_regex(models_pi_no[[j]],   patspill_cap),
               fmt_est_felm_regex(models_pi_no[[j]],   patspill_psy),
               fmt_est_felm_regex(models_pi_no[[j]],   patspill_full),
               as.character(get_n(models_pi_no[[j]])),
               as.character(get_r2(models_pi_no[[j]]))),
      with = c("—",
               fmt_est_felm_regex(models_pi_with[[j]], patspill_cap),
               fmt_est_felm_regex(models_pi_with[[j]], patspill_psy),
               fmt_est_felm_regex(models_pi_with[[j]], patspill_full),
               as.character(get_n(models_pi_with[[j]])),
               as.character(get_r2(models_pi_with[[j]])))
    )
  })
  
  pi_mat <- matrix(NA_character_, nrow = 6, ncol = n_cols)
  for (j in seq_len(n_outcomes)) {
    pi_mat[, idx_no(j)]   <- row_pi[[j]]$no
    pi_mat[, idx_with(j)] <- row_pi[[j]]$with
  }
  
  pi_rows <- data.frame(
    Outcome = c("Control", "Capital", "Psychosocial", "Full", "No. Obs.", "R\u00B2"),
    as.data.frame(pi_mat, stringsAsFactors = FALSE),
    stringsAsFactors = FALSE
  )
  colnames(pi_rows) <- c("Outcome", col_names)
  
  # ---------------------------------------------------------------------------
  # 6. Panel 2: EI Inclusion (Pool)
  # ---------------------------------------------------------------------------
  row_pool <- lapply(seq_len(n_outcomes), function(j) {
    list(
      no   = c("—", fmt_est_felm_regex(models_pool_no[[j]],   patspill_pool),
               as.character(get_n(models_pool_no[[j]])),
               as.character(get_r2(models_pool_no[[j]]))),
      with = c("—", fmt_est_felm_regex(models_pool_with[[j]], patspill_pool),
               as.character(get_n(models_pool_with[[j]])),
               as.character(get_r2(models_pool_with[[j]])))
    )
  })
  
  pool_mat <- matrix(NA_character_, nrow = 4, ncol = n_cols)
  for (j in seq_len(n_outcomes)) {
    pool_mat[, idx_no(j)]   <- row_pool[[j]]$no
    pool_mat[, idx_with(j)] <- row_pool[[j]]$with
  }
  
  pool_rows <- data.frame(
    Outcome = c("Control", "Pool", "No. Obs.", "R\u00B2"),
    as.data.frame(pool_mat, stringsAsFactors = FALSE),
    stringsAsFactors = FALSE
  )
  colnames(pool_rows) <- c("Outcome", col_names)
  
  # ---------------------------------------------------------------------------
  # 7. Panel 3: Additional statistics
  # ---------------------------------------------------------------------------
  extra_mat <- matrix(NA_character_, nrow = 6, ncol = n_cols)
  for (j in seq_len(n_outcomes)) {
    extra_mat[1, idx_no(j)]   <- "No"
    extra_mat[1, idx_with(j)] <- "Yes"
    extra_mat[2, idx_no(j)]   <- extra_mat[2, idx_with(j)] <- as.character(mean_values_pi[j])
    extra_mat[3, idx_no(j)]   <- extra_mat[3, idx_with(j)] <- as.character(sd_values_pi[j])
    extra_mat[4, idx_no(j)]   <- as.character(full_Psychosocial_no[j])
    extra_mat[4, idx_with(j)] <- as.character(full_Psychosocial_with[j])
    extra_mat[5, idx_no(j)]   <- as.character(full_Capital_no[j])
    extra_mat[5, idx_with(j)] <- as.character(full_Capital_with[j])
    extra_mat[6, idx_no(j)]   <- as.character(psychosocial_Capital_no[j])
    extra_mat[6, idx_with(j)] <- as.character(psychosocial_Capital_with[j])
  }
  
  extra_rows <- data.frame(
    Outcome = c(
      "Control Variables",
      "Control mean @ follow up PI",
      "Control SD @ follow up PI",
      "Full - Psychosocial",
      "Full - Capital",
      "Psychosocial - Capital"
    ),
    as.data.frame(extra_mat, stringsAsFactors = FALSE),
    stringsAsFactors = FALSE
  )
  colnames(extra_rows) <- c("Outcome", col_names)
  
  # ---------------------------------------------------------------------------
  # 8. Bind all panels and add Panel label column
  # ---------------------------------------------------------------------------
  final_tbl <- dplyr::bind_rows(pi_rows, pool_rows, extra_rows)
  
  n_pi    <- nrow(pi_rows)
  n_pool  <- nrow(pool_rows)
  n_extra <- nrow(extra_rows)
  
  i_pi_start    <- 1
  i_pi_end      <- n_pi
  i_pool_start  <- i_pi_end + 1
  i_pool_end    <- i_pool_start + n_pool - 1
  i_extra_start <- i_pool_end + 1
  i_extra_end   <- i_extra_start + n_extra - 1
  
  panel_titles <- c(
    rep("EI Inclusion",          n_pi),
    rep("EI Inclusion (Pool)",   n_pool),
    rep("Additional statistics", n_extra)
  )
  
  final_tbl <- final_tbl %>%
    dplyr::mutate(Panel = panel_titles, .before = 1)
  
  # ---------------------------------------------------------------------------
  # 9. Build flextable
  # ---------------------------------------------------------------------------
  if (exists("customtab_defaults")) customtab_defaults()
  
  ft <- flextable::flextable(final_tbl)
  
  # Base header labels
  ft <- flextable::set_header_labels(
    ft,
    values = c(
      Panel   = " ",
      Outcome = "Outcome\u2020",
      stats::setNames(col_names, col_names)
    )
  )
  
  # Top header row: outcome group labels, each spanning 2 columns (no/with ctrl)
  ft <- flextable::add_header_row(
    ft,
    values    = c(" ", " ", header_labels),
    colwidths = c(1, 1, rep(2, length(header_labels)))
  )
  
  # Alignment
  ft <- flextable::align(ft, j = 1:2,             align = "left",   part = "all")
  ft <- flextable::align(ft, j = 3:(n_cols + 2),  align = "center", part = "all")
  
  # Borders
  border_thick <- officer::fp_border(color = "black", width = 2)
  border_thin  <- officer::fp_border(color = "black", width = 0.5)
  
  ft <- flextable::border_inner_h(ft, border = border_thin,  part = "body")
  ft <- flextable::hline(ft, i = i_pool_start - 1, border = border_thick, part = "body")
  ft <- flextable::hline(ft, i = i_extra_start - 1, border = border_thick, part = "body")
  
  # Merge and bold the Panel column
  ft <- flextable::merge_v(ft, j = "Panel", part = "body")
  ft <- flextable::bold(ft,   j = "Panel", part = "body")
  
  # Caption and footnote
  ft <- flextable::set_caption(ft, caption = table_title)
  ft <- flextable::add_footer_lines(ft, values = footnote_text)
  
  # Fit to page width
  if (exists("FitFlextableToPage")) {
    ft <- FitFlextableToPage(ft)
  } else {
    ft <- flextable::autofit(ft)
  }
  
  return(ft)
}
############################################################
# Generic spillover IPV table (OLS + kable)
############################################################



# getTable_IPV_spill <- function(ipv_vars,
#                                binary_header,
#                                index_header,
#                                table_title,
#                                treatment_vars,
#                                control_vars,
#                                strata_vars,
#                                cluster_vars,
#                                followup_ipv_MRT_hh_spill,
#                                followup_ipv_MRT_hh_spill_control_pi,
#                                footnote_text = NULL) {
#   #---------------------------
#   # Default footnote
#   #---------------------------
#   if (is.null(footnote_text)) {
#     footnote_text <- paste(
#       "Notes: Results presented are OLS estimates.",
#       "Columns (1) and (3) show estimates without controls (treatment + strata only for cash transfer, treatment only for PI).",
#       "Columns (2) and (4) include controls for randomization strata (commune) and baseline covariates.",
#       "We control for social promotion intervention. Enumerator fixed effects are included in all regressions.",
#       "We estimate the regressions for the EI beneficiaries aged 18–49 only.",
#       "Robust standard errors are shown in parentheses, clustered at the village proxy level.",
#       "*** p < 0.01, ** p < 0.05, * p < 0.1.",
#       sep = " "
#     )
#   }
#  
#   #---------------------------
#   # 1) Main estimation
#   #---------------------------
#   mainResults <- purrr::map_dfr(treatment_vars, function(curr_treat_var) {
#     purrr::map_dfr(ipv_vars, function(depvar) {
#       getEstimateGlobal_spill(depvar,curr_treat_var,control_vars,strata_vars,
#                               cluster_vars,followup_ipv_MRT_hh_spill
#       ) %>%
#         dplyr::mutate(
#           depvar         = depvar,
#           curr_treat_var = curr_treat_var
#         )
#     })
#   })
#   
#   #---------------------------
#   # 2) Extract models WITHOUT / WITH controls
#   #   Order assumed:
#   #   1: PI, binary
#   #   2: PI, index
#   #   3: PI pool, binary
#   #   4: PI pool, index
#   #---------------------------
#   m21_no   <- mainResults$results_no_controls[[1]]
#   m22_no   <- mainResults$results_no_controls[[2]]
#   m31_no   <- mainResults$results_no_controls[[3]]
#   m32_no   <- mainResults$results_no_controls[[4]]
#   
#   m21_with <- mainResults$results_with_controls[[1]]
#   m22_with <- mainResults$results_with_controls[[2]]
#   m31_with <- mainResults$results_with_controls[[3]]
#   m32_with <- mainResults$results_with_controls[[4]]
# 
#   #---------------------------
#   # 3) Interaction terms (PI)
#   #---------------------------
#   full_Psychosocial_no <- mainResults %>%
#     dplyr::filter(curr_treat_var == "treatment_pi_spill") %>%
#     dplyr::pull(full_Psychosocial_no)
#   
#   full_Capital_no <- mainResults %>%
#     dplyr::filter(curr_treat_var == "treatment_pi_spill") %>%
#     dplyr::pull(full_Capital_no)
#   
#   psychosocial_Capital_no <- mainResults %>%
#     dplyr::filter(curr_treat_var == "treatment_pi_spill") %>%
#     dplyr::pull(psychosocial_Capital_no)
#   
#   full_Psychosocial_with <- mainResults %>%
#     dplyr::filter(curr_treat_var == "treatment_pi_spill") %>%
#     dplyr::pull(full_Psychosocial_with)
#   
#   full_Capital_with <- mainResults %>%
#     dplyr::filter(curr_treat_var == "treatment_pi_spill") %>%
#     dplyr::pull(full_Capital_with)
#   
#   psychosocial_Capital_with <- mainResults %>%
#     dplyr::filter(curr_treat_var == "treatment_pi_spill") %>%
#     dplyr::pull(psychosocial_Capital_with)
#   
#   #---------------------------
#   # 4) Descriptive statistics
#   #---------------------------
#   mean_values_pi <- c()
#   sd_values_pi   <- c()
#   
#   for (depvar in ipv_vars) {
#     mean_values_pi <- c(
#       mean_values_pi,
#       round(mean(followup_ipv_MRT_hh_spill_control_pi[[depvar]], na.rm = TRUE), 3)
#     )
#     sd_values_pi <- c(
#       sd_values_pi,
#       round(sd(followup_ipv_MRT_hh_spill_control_pi[[depvar]], na.rm = TRUE), 3)
#     )
#   }
#   
#   #---------------------------
#   # 5) Panels
#   #---------------------------
#   
#   # Panel 1: EI Inclusion (3 arms)
#   pi_rows <- tibble::tibble(
#     Panel   = "EI Inclusion",
#     Outcome = c("Control", "Capital", "Psychosocial", "Full", "No. Obs.", "R\u00B2"),
#     `(1)`   = c(
#       "—",
#       fmt_est_felm_regex(m21_no,   patspill_cap),
#       fmt_est_felm_regex(m21_no,   patspill_psy),
#       fmt_est_felm_regex(m21_no,   patspill_full),
#       as.character(get_n(m21_no)),
#       as.character(get_r2(m21_no))
#     ),
#     `(2)`   = c(
#       "—",
#       fmt_est_felm_regex(m21_with, patspill_cap),
#       fmt_est_felm_regex(m21_with, patspill_psy),
#       fmt_est_felm_regex(m21_with, patspill_full),
#       as.character(get_n(m21_with)),
#       as.character(get_r2(m21_with))
#     ),
#     `(3)`   = c(
#       "—",
#       fmt_est_felm_regex(m22_no,   patspill_cap),
#       fmt_est_felm_regex(m22_no,   patspill_psy),
#       fmt_est_felm_regex(m22_no,   patspill_full),
#       as.character(get_n(m22_no)),
#       as.character(get_r2(m22_no))
#     ),
#     `(4)`   = c(
#       "—",
#       fmt_est_felm_regex(m22_with, patspill_cap),
#       fmt_est_felm_regex(m22_with, patspill_psy),
#       fmt_est_felm_regex(m22_with, patspill_full),
#       as.character(get_n(m22_with)),
#       as.character(get_r2(m22_with))
#     )
#   )
#   
#   # Panel 2: EI Inclusion (Pool)
#   pi_pool_rows <- tibble::tibble(
#     Panel   = "EI Inclusion (Pool)",
#     Outcome = c("Control", "Pool", "No. Obs.", "R\u00B2"),
#     `(1)`   = c(
#       "—",
#       fmt_est_felm_regex(m31_no, patspill_pool),
#       as.character(get_n(m31_no)),
#       as.character(get_r2(m31_no))
#     ),
#     `(2)`   = c(
#       "—",
#       fmt_est_felm_regex(m31_with, patspill_pool),
#       as.character(get_n(m31_with)),
#       as.character(get_r2(m31_with))
#     ),
#     `(3)`   = c(
#       "—",
#       fmt_est_felm_regex(m32_no, patspill_pool),
#       as.character(get_n(m32_no)),
#       as.character(get_r2(m32_no))
#     ),
#     `(4)`   = c(
#       "—",
#       fmt_est_felm_regex(m32_with, patspill_pool),
#       as.character(get_n(m32_with)),
#       as.character(get_r2(m32_with))
#     )
#   )
#   
#   # Panel 3: Additional statistics
#   final_panel <- tibble::tibble(
#     Panel   = "Additional statistics",
#     Outcome = c(
#       "Control Variables",
#       "Control mean @ follow up PI",
#       "Control SD @ follow up PI",
#       "Full - Psychosocial",
#       "Full - Capital",
#       "Psychosocial - Capital"
#     ),
#     `(1)` = c(
#       "No",
#       as.character(mean_values_pi[1]),
#       as.character(sd_values_pi[1]),
#       as.character(full_Psychosocial_no[1]),
#       as.character(full_Capital_no[1]),
#       as.character(psychosocial_Capital_no[1])
#     ),
#     `(2)` = c(
#       "Yes",
#       as.character(mean_values_pi[1]),
#       as.character(sd_values_pi[1]),
#       as.character(full_Psychosocial_with[1]),
#       as.character(full_Capital_with[1]),
#       as.character(psychosocial_Capital_with[1])
#     ),
#     `(3)` = c(
#       "No",
#       as.character(mean_values_pi[2]),
#       as.character(sd_values_pi[2]),
#       as.character(full_Psychosocial_no[2]),
#       as.character(full_Capital_no[2]),
#       as.character(psychosocial_Capital_no[2])
#     ),
#     `(4)` = c(
#       "Yes",
#       as.character(mean_values_pi[2]),
#       as.character(sd_values_pi[2]),
#       as.character(full_Psychosocial_with[2]),
#       as.character(full_Capital_with[2]),
#       as.character(psychosocial_Capital_with[2])
#     )
#   )
#   
#   #---------------------------
#   # 6) Bind + row indices
#   #---------------------------
#   final_tbl <- dplyr::bind_rows(
#     pi_rows,
#     pi_pool_rows,
#     final_panel
#   )
#   
#   n_pi    <- nrow(pi_rows)
#   n_pool  <- nrow(pi_pool_rows)
#   n_extra <- nrow(final_panel)
#   
#   i_pi_start    <- 1
#   i_pi_end      <- n_pi
#   i_pool_start  <- i_pi_end + 1
#   i_pool_end    <- i_pool_start + n_pool - 1
#   i_extra_start <- i_pool_end + 1
#   i_extra_end   <- i_extra_start + n_extra - 1
#   
#   final_tbl_no_panel <- final_tbl %>% dplyr::select(-Panel)
#   
#   #---------------------------
#   # 7) Header (grouping)
#   #---------------------------
#   header_vec <- c(" " = 1, binary_header = 2, index_header = 2)
#   
#   #---------------------------
#   # 8) Render kable (caption = table_title, no 'Table X:')
#   #---------------------------
#   tbl <- knitr::kable(
#     final_tbl_no_panel,
#     format    = "html",
#     escape    = FALSE,
#     align     = c("l", "c", "c", "c", "c"),
#     col.names = c("Outcome\u2020", "(1)", "(2)", "(3)", "(4)"),
#     caption   = table_title
#   ) %>%
#     kableExtra::add_header_above(header_vec) %>%
#     kableExtra::kable_classic(full_width = TRUE, html_font = "Arial") %>%
#     kableExtra::pack_rows("EI Inclusion",
#                           i_pi_start, i_pi_end,
#                           bold = TRUE, italic = FALSE) %>%
#     kableExtra::row_spec(i_pool_start, extra_css = "border-top: 2px solid #000;") %>%
#     kableExtra::pack_rows("EI Inclusion (Pool)",
#                           i_pool_start, i_pool_end,
#                           bold = TRUE, italic = FALSE) %>%
#     kableExtra::row_spec(i_extra_start, extra_css = "border-top: 2px solid #000;") %>%
#     kableExtra::pack_rows("Additional statistics",
#                           i_extra_start, i_extra_end,
#                           bold = TRUE, italic = FALSE) %>%
#     kableExtra::add_footnote(
#       label    = footnote_text,
#       notation = "none"
#     )
#   
#   return(tbl)
# }

############################################################################
######### Function for computing regression ################################
############################################################################

# Function to get global estimates for continuous variables paste0(control_vars, collapse = " + "),
getEstimateGlobal <- function(depvar, curr_treat_var, control_vars, strata_vars, cluster_vars, followup_df){
  

  if(curr_treat_var=="treatment_csh_trnsfr"){
    
    # Create formula for base model with full controls
    regmodel_cntrls <- paste0(depvar, " ~ ", curr_treat_var, " + ", strata_vars, " + ", paste0(control_vars, collapse = " + "), "| 0 | 0 |", "hhid")
    
    ## browser()
    reg_df <- followup_df %>% filter(reg_hh_csh_mrt==1)
    # Estimate both models using felm
    regmodbase_cntrls <- felm(formula=formula(regmodel_cntrls), data = reg_df)
    
    coef_ftest_1 = ""
    coef_ftest_2 = ""
    coef_ftest_3 = ""
    
  }else if(curr_treat_var=="treatment_pi"){
    
    # Create formula for base model with full controls
    regmodel_cntrls <- paste0(depvar, " ~ ", curr_treat_var, " + ", strata_vars, " + ", paste0(control_vars, collapse = " + "), "| 0 | 0 |", "cluster")
    
    reg_df <- followup_df %>% 
      filter(reg_hh_pi_mrt==1)

    # Estimate both models using felm
    regmodbase_cntrls <- felm(formula=formula(regmodel_cntrls), data = reg_df)
    # Estimate both models using felm
    #regmodbase_cntrls <- fixest::feglm(fml=formula(regmodel_cntrls), data = reg_df, 
    #                                   cluster = cluster_vars, family = "binomial")
    
    # Extract coefficients from the model
    coef_values <- coef(regmodbase_cntrls)
    
    # Perform joint F-test with clustered standard errors
    joint_f_test_1 <- linearHypothesis(regmodbase_cntrls, 
                                       c("treatment_piFull=treatment_piPsychosocial"),
                                       test = "F", singular.ok = TRUE)
    
    joint_f_test_2<- linearHypothesis(regmodbase_cntrls, 
                                      c("treatment_piFull=treatment_piCapital"),
                                      test = "F", singular.ok = TRUE)
    
    joint_f_test_3 <- linearHypothesis(regmodbase_cntrls, 
                                       c("treatment_piPsychosocial = treatment_piCapital"),
                                       test = "F", singular.ok = TRUE)
    
    # Get and format p-value with significance stars
    coef_ftest_1 <- round(coef_values["treatment_piFull"] - coef_values["treatment_piPsychosocial"],3)
    pval_ftest_1 <- round(joint_f_test_1$`Pr(>F)`[2],3)
    
    coef_ftest_2 <- round(coef_values["treatment_piFull"] - coef_values["treatment_piCapital"],3)
    pval_ftest_2 <- round(joint_f_test_2$`Pr(>F)`[2],3)
    
    coef_ftest_3 <- round(coef_values["treatment_piPsychosocial"] - coef_values["treatment_piCapital"],3)
    pval_ftest_3 <- round(joint_f_test_3$`Pr(>F)`[2],3)
    
    # Get and format p-value with significance stars
    for (index in 1:3) {
      # Retrieve values dynamically
      pval_ftest <- get(paste0("pval_ftest_", index))
      coef_ftest <- get(paste0("coef_ftest_", index))
      
      # Append significance stars based on p-value
      if (pval_ftest < 0.01) {
        coef_ftest <- paste0(coef_ftest, "***")
      } else if (pval_ftest < 0.05) {
        coef_ftest <- paste0(coef_ftest, "**")
      } else if (pval_ftest < 0.1) {
        coef_ftest <- paste0(coef_ftest, "*")
      } else {
        coef_ftest <- as.character(coef_ftest)
      }
      
      # Assign the modified value back to the global variable
      assign(paste0("coef_ftest_", index), coef_ftest)
    }
    
  }else{
    
    # Create formula for base model with full controls
    regmodel_cntrls <- paste0(depvar, " ~ ", curr_treat_var, " + ", strata_vars, " + ", paste0(control_vars, collapse = " + "), "| 0 | 0 |", "cluster")
    
    # for the case of the pool treatment 
    reg_df <- followup_df %>% 
      filter(reg_hh_pi_mrt==1)
    
    # Estimate both models using felm
    regmodbase_cntrls <- felm(formula=formula(regmodel_cntrls), data = reg_df)
    # Estimate both models using felm
    #regmodbase_cntrls <- fixest::feglm(fml=formula(regmodel_cntrls), data = reg_df, 
    #                                  cluster = cluster_vars, family = "binomial")

    
    coef_ftest_1 = ""
    coef_ftest_2 = ""
    coef_ftest_3 = ""
  }


  # Return results in tibble format
  out <- tibble(
    results_base = list(regmodbase_cntrls),
    full_Psychosocial = coef_ftest_1,
    full_Capital = coef_ftest_2,
    psychosocial_Capital = coef_ftest_3
  )
  
  out
}

# Function to get global estimates for continuous variables adding surveyor FE
getEstimateGlobal_rob <- function(depvar, curr_treat_var, control_vars, strata_vars, cluster_vars, followup_df, fe_var="0"){
  
  
  if(curr_treat_var=="treatment_csh_trnsfr"){
    
    # Create formula for base model with full controls
    regmodel_cntrls <- paste0(depvar, " ~ ", curr_treat_var, " + ", strata_vars, " + ", paste0(control_vars, collapse = " + "), paste0("| ",fe_var," | 0 |", "hhid"))
    ## browser()
    reg_df <- followup_df %>% filter(reg_hh_csh_mrt==1)
    # Estimate both models using felm
    regmodbase_cntrls <- felm(formula=formula(regmodel_cntrls), data = reg_df)
    #regmodbase_cntrls <- fixest::feglm(fml=formula(regmodel_cntrls), data = reg_df, cluster = cluster_vars)
    
    coef_ftest_1 = ""
    coef_ftest_2 = ""
    coef_ftest_3 = ""
    
  }else if(curr_treat_var=="treatment_pi"){
    
    # Create formula for base model with full controls
    regmodel_cntrls <- paste0(depvar, " ~ ", curr_treat_var, " + ", strata_vars, " + ", paste0(control_vars, collapse = " + "), paste0("| ",fe_var," | 0 |", "cluster"))
    
    reg_df <- followup_df %>% 
      filter(reg_hh_pi_mrt==1)
    
    # Estimate both models using felm
    regmodbase_cntrls <- felm(formula=formula(regmodel_cntrls), data = reg_df)
    # Estimate both models using felm
    #regmodbase_cntrls <- fixest::feglm(fml=formula(regmodel_cntrls), data = reg_df, 
    #                                   cluster = cluster_vars, family = "binomial")
    
    # Extract coefficients from the model
    coef_values <- coef(regmodbase_cntrls)
    
    # Perform joint F-test with clustered standard errors
    joint_f_test_1 <- linearHypothesis(regmodbase_cntrls, 
                                       c("treatment_piFull=treatment_piPsychosocial"),
                                       test = "F", singular.ok = TRUE)
    
    joint_f_test_2<- linearHypothesis(regmodbase_cntrls, 
                                      c("treatment_piFull=treatment_piCapital"),
                                      test = "F", singular.ok = TRUE)
    
    joint_f_test_3 <- linearHypothesis(regmodbase_cntrls, 
                                       c("treatment_piPsychosocial = treatment_piCapital"),
                                       test = "F", singular.ok = TRUE)
    
    # Get and format p-value with significance stars
    coef_ftest_1 <- round(coef_values["treatment_piFull"] - coef_values["treatment_piPsychosocial"],3)
    pval_ftest_1 <- round(joint_f_test_1$`Pr(>F)`[2],3)
    
    coef_ftest_2 <- round(coef_values["treatment_piFull"] - coef_values["treatment_piCapital"],3)
    pval_ftest_2 <- round(joint_f_test_2$`Pr(>F)`[2],3)
    
    coef_ftest_3 <- round(coef_values["treatment_piPsychosocial"] - coef_values["treatment_piCapital"],3)
    pval_ftest_3 <- round(joint_f_test_3$`Pr(>F)`[2],3)
    
    # Get and format p-value with significance stars
    for (index in 1:3) {
      # Retrieve values dynamically
      pval_ftest <- get(paste0("pval_ftest_", index))
      coef_ftest <- get(paste0("coef_ftest_", index))
      
      # Append significance stars based on p-value
      if (pval_ftest < 0.01) {
        coef_ftest <- paste0(coef_ftest, "***")
      } else if (pval_ftest < 0.05) {
        coef_ftest <- paste0(coef_ftest, "**")
      } else if (pval_ftest < 0.1) {
        coef_ftest <- paste0(coef_ftest, "*")
      } else {
        coef_ftest <- as.character(coef_ftest)
      }
      
      # Assign the modified value back to the global variable
      assign(paste0("coef_ftest_", index), coef_ftest)
    }
    
  }else{
    
    # Create formula for base model with full controls
    regmodel_cntrls <- paste0(depvar, " ~ ", curr_treat_var, " + ", strata_vars, " + ", paste0(control_vars, collapse = " + "), "| 0 | 0 |", "cluster")
    
    # for the case of the pool treatment 
    reg_df <- followup_df %>% 
      filter(reg_hh_pi_mrt==1)
    
    # Estimate both models using felm
    regmodbase_cntrls <- felm(formula=formula(regmodel_cntrls), data = reg_df)
    # Estimate both models using felm
    #regmodbase_cntrls <- fixest::feglm(fml=formula(regmodel_cntrls), data = reg_df, 
    #                                  cluster = cluster_vars, family = "binomial")
    
    
    coef_ftest_1 = ""
    coef_ftest_2 = ""
    coef_ftest_3 = ""
  }
  
  
  # Return results in tibble format
  out <- tibble(
    results_base = list(regmodbase_cntrls),
    full_Psychosocial = coef_ftest_1,
    full_Capital = coef_ftest_2,
    psychosocial_Capital = coef_ftest_3
  )
  
  out
}


# Function to get global estimates for continuous variables adding surveyor FE
getEstimateGlobalComparison_rob_nonlin <- function(depvar, curr_treat_var, control_vars, strata_vars, cluster_vars, followup_df){
  
    if(curr_treat_var=="treatment_csh_trnsfr"){
      
      # MODEL WITHOUT CONTROLS (only treatment + strata)
      regmodel_no_cntrls <- paste0(depvar, " ~ ", curr_treat_var)
      reg_df <- followup_df %>% filter(reg_hh_csh_mrt==1)
      regmodbase_no_cntrls <- fixest::feglm(fml=formula(regmodel_no_cntrls), family = "binomial",data = reg_df, cluster = "hhid")

      # MODEL WITH CONTROLS (treatment + strata + controls)
      regmodel_with_cntrls <- paste0(depvar, " ~ ", curr_treat_var, " + ", strata_vars, " + ", 
                                     paste0(control_vars, collapse = " + "))
      regmodbase_with_cntrls <- fixest::feglm(fml=formula(regmodel_with_cntrls), family = "binomial",data = reg_df, cluster = "hhid")
      
      coef_ftest_1_no = ""
      coef_ftest_2_no = ""
      coef_ftest_3_no = ""
      coef_ftest_1_with = ""
      coef_ftest_2_with = ""
      coef_ftest_3_with = ""
      
    } else if(curr_treat_var=="treatment_pi"){
      
      # MODEL WITHOUT CONTROLS (only treatment)
      regmodel_no_cntrls <- paste0(depvar, " ~ ", curr_treat_var)
      reg_df <- followup_df %>% filter(reg_hh_pi_mrt==1)
      regmodbase_no_cntrls <- fixest::feglm(fml=formula(regmodel_no_cntrls), family = "binomial",data = reg_df, cluster = "cluster")
      
      # MODEL WITH CONTROLS (treatment + controls)
      regmodel_with_cntrls <- paste0(depvar, " ~ ", curr_treat_var, " + ", strata_vars, " + ", 
                                     paste0(control_vars, collapse = " + "))
      regmodbase_with_cntrls <- fixest::feglm(fml=formula(regmodel_with_cntrls), family = "binomial",data = reg_df, cluster = "cluster")
      
      # F-tests for model WITHOUT controls
      coef_values_no <- coef(regmodbase_with_cntrls)
      
      joint_f_test_1_no <- linearHypothesis(regmodbase_with_cntrls, 
                                            c("treatment_piFull=treatment_piPsychosocial"),
                                            test = "F", singular.ok = TRUE)
      joint_f_test_2_no <- linearHypothesis(regmodbase_with_cntrls, 
                                            c("treatment_piFull=treatment_piCapital"),
                                            test = "F", singular.ok = TRUE)
      joint_f_test_3_no <- linearHypothesis(regmodbase_with_cntrls, 
                                            c("treatment_piPsychosocial = treatment_piCapital"),
                                            test = "F", singular.ok = TRUE)
      
      coef_ftest_1_no <- round(coef_values_no["treatment_piFull"] - coef_values_no["treatment_piPsychosocial"], 3)
      pval_ftest_1_no <- round(joint_f_test_1_no$`Pr(>F)`[2], 3)
      
      coef_ftest_2_no <- round(coef_values_no["treatment_piFull"] - coef_values_no["treatment_piCapital"], 3)
      pval_ftest_2_no <- round(joint_f_test_2_no$`Pr(>F)`[2], 3)
      
      coef_ftest_3_no <- round(coef_values_no["treatment_piPsychosocial"] - coef_values_no["treatment_piCapital"], 3)
      pval_ftest_3_no <- round(joint_f_test_3_no$`Pr(>F)`[2], 3)
      
      # F-tests for model WITH controls
      coef_values_with <- coef(regmodbase_with_cntrls)
      
      joint_f_test_1_with <- linearHypothesis(regmodbase_with_cntrls, 
                                              c("treatment_piFull=treatment_piPsychosocial"),
                                              test = "F", singular.ok = TRUE)
      joint_f_test_2_with <- linearHypothesis(regmodbase_with_cntrls, 
                                              c("treatment_piFull=treatment_piCapital"),
                                              test = "F", singular.ok = TRUE)
      joint_f_test_3_with <- linearHypothesis(regmodbase_with_cntrls, 
                                              c("treatment_piPsychosocial = treatment_piCapital"),
                                              test = "F", singular.ok = TRUE)
      
      coef_ftest_1_with <- round(coef_values_with["treatment_piFull"] - coef_values_with["treatment_piPsychosocial"], 3)
      pval_ftest_1_with <- round(joint_f_test_1_with$`Pr(>F)`[2], 3)
      
      coef_ftest_2_with <- round(coef_values_with["treatment_piFull"] - coef_values_with["treatment_piCapital"], 3)
      pval_ftest_2_with <- round(joint_f_test_2_with$`Pr(>F)`[2], 3)
      
      coef_ftest_3_with <- round(coef_values_with["treatment_piPsychosocial"] - coef_values_with["treatment_piCapital"], 3)
      pval_ftest_3_with <- round(joint_f_test_3_with$`Pr(>F)`[2], 3)
      
      # Add significance stars for NO controls
      for (index in 1:3) {
        pval_ftest <- get(paste0("pval_ftest_", index, "_no"))
        coef_ftest <- get(paste0("coef_ftest_", index, "_no"))
        
        if (pval_ftest <= 0.01) {
          coef_ftest <- paste0(coef_ftest, "***")
        } else if (pval_ftest <= 0.05) {
          coef_ftest <- paste0(coef_ftest, "**")
        } else if (pval_ftest <= 0.10) {
          coef_ftest <- paste0(coef_ftest, "*")
        } else {
          coef_ftest <- as.character(coef_ftest)
        }
        assign(paste0("coef_ftest_", index, "_no"), coef_ftest)
      }
      
      # Add significance stars for WITH controls
      for (index in 1:3) {
        pval_ftest <- get(paste0("pval_ftest_", index, "_with"))
        coef_ftest <- get(paste0("coef_ftest_", index, "_with"))
        
        if (pval_ftest <= 0.01) {
          coef_ftest <- paste0(coef_ftest, "***")
        } else if (pval_ftest <= 0.05) {
          coef_ftest <- paste0(coef_ftest, "**")
        } else if (pval_ftest <= 0.10) {
          coef_ftest <- paste0(coef_ftest, "*")
        } else {
          coef_ftest <- as.character(coef_ftest)
        }
        assign(paste0("coef_ftest_", index, "_with"), coef_ftest)
      }
      
    } else {
      # For pooled treatment
      
      # MODEL WITHOUT CONTROLS 
      regmodel_no_cntrls <- paste0(depvar, " ~ ", curr_treat_var)
      reg_df <- followup_df %>% filter(reg_hh_pi_mrt==1)
      regmodbase_no_cntrls <-  fixest::feglm(fml=formula(regmodel_no_cntrls), family = "binomial",data = reg_df, cluster = "cluster")
      
      # MODEL WITH CONTROLS
      regmodel_with_cntrls <- paste0(depvar, " ~ ", curr_treat_var, " + ", strata_vars, " + ", 
                                     paste0(control_vars, collapse = " + "))
      regmodbase_with_cntrls <- fixest::feglm(fml=formula(regmodel_with_cntrls), family = "binomial",data = reg_df, cluster = "cluster")
      
      coef_ftest_1_no = ""
      coef_ftest_2_no = ""
      coef_ftest_3_no = ""
      coef_ftest_1_with = ""
      coef_ftest_2_with = ""
      coef_ftest_3_with = ""
    }
    
    # Return results in tibble format
    out <- tibble(
      results_no_controls = list(regmodbase_no_cntrls),
      results_with_controls = list(regmodbase_with_cntrls),
      full_Psychosocial_no = coef_ftest_1_no,
      full_Capital_no = coef_ftest_2_no,
      psychosocial_Capital_no = coef_ftest_3_no,
      full_Psychosocial_with = coef_ftest_1_with,
      full_Capital_with = coef_ftest_2_with,
      psychosocial_Capital_with = coef_ftest_3_with
    )
    
    out
  }


# Function to get global estimates for continuous variables paste0(control_vars, collapse = " + "),
getEstimateGlobal_spill <- function(depvar, curr_treat_var, control_vars, strata_vars, cluster_vars, followup_df_spill){

  if(curr_treat_var=="treatment_pi_spill"){
    
   
    # MODEL WITHOUT CONTROLS (only treatment)
    regmodel_no_cntrls <- paste0(depvar, " ~ ", curr_treat_var, " + ", strata_vars, "| 0 | 0 |", "cluster")
    reg_df <- followup_df_spill %>% filter(reg_hh_pi_spl_mrt==1)
    regmodbase_no_cntrls <- felm(formula=formula(regmodel_no_cntrls), data = reg_df)

    # MODEL WITH CONTROLS (treatment + controls)
    regmodel_with_cntrls <- paste0(depvar, " ~ ", curr_treat_var, " + ", strata_vars, " + ", 
                                   paste0(control_vars, collapse = " + "), "| 0 | 0 |", "cluster")
    regmodbase_with_cntrls <- felm(formula=formula(regmodel_with_cntrls), data = reg_df)
    
    # F-tests for model WITHOUT controls
    coef_values_no <- coef(regmodbase_no_cntrls)
    
    joint_f_test_1_no <- linearHypothesis(regmodbase_no_cntrls, 
                                          c("treatment_pi_spillFull=treatment_pi_spillPsychosocial"),
                                          test = "F", singular.ok = TRUE)
    joint_f_test_2_no <- linearHypothesis(regmodbase_no_cntrls, 
                                          c("treatment_pi_spillFull=treatment_pi_spillCapital"),
                                          test = "F", singular.ok = TRUE)
    joint_f_test_3_no <- linearHypothesis(regmodbase_no_cntrls, 
                                          c("treatment_pi_spillPsychosocial = treatment_pi_spillCapital"),
                                          test = "F", singular.ok = TRUE)
 
    coef_ftest_1_no <- round(coef_values_no["treatment_pi_spillFull"] - coef_values_no["treatment_pi_spillPsychosocial"], 3)
    pval_ftest_1_no <- round(joint_f_test_1_no$`Pr(>F)`[2], 3)
    
    coef_ftest_2_no <- round(coef_values_no["treatment_pi_spillFull"] - coef_values_no["treatment_pi_spillCapital"], 3)
    pval_ftest_2_no <- round(joint_f_test_2_no$`Pr(>F)`[2], 3)
    
    coef_ftest_3_no <- round(coef_values_no["treatment_pi_spillPsychosocial"] - coef_values_no["treatment_pi_spillCapital"], 3)
    pval_ftest_3_no <- round(joint_f_test_3_no$`Pr(>F)`[2], 3)
    
    # F-tests for model WITH controls
    coef_values_with <- coef(regmodbase_with_cntrls)
    
    joint_f_test_1_with <- linearHypothesis(regmodbase_with_cntrls, 
                                            c("treatment_pi_spillFull=treatment_pi_spillPsychosocial"),
                                            test = "F", singular.ok = TRUE)
    joint_f_test_2_with <- linearHypothesis(regmodbase_with_cntrls, 
                                            c("treatment_pi_spillFull=treatment_pi_spillCapital"),
                                            test = "F", singular.ok = TRUE)
    joint_f_test_3_with <- linearHypothesis(regmodbase_with_cntrls, 
                                            c("treatment_pi_spillPsychosocial = treatment_pi_spillCapital"),
                                            test = "F", singular.ok = TRUE)
    
    coef_ftest_1_with <- round(coef_values_with["treatment_pi_spillFull"] - coef_values_with["treatment_pi_spillPsychosocial"], 3)
    pval_ftest_1_with <- round(joint_f_test_1_with$`Pr(>F)`[2], 3)
    
    coef_ftest_2_with <- round(coef_values_with["treatment_pi_spillFull"] - coef_values_with["treatment_pi_spillCapital"], 3)
    pval_ftest_2_with <- round(joint_f_test_2_with$`Pr(>F)`[2], 3)
    
    coef_ftest_3_with <- round(coef_values_with["treatment_pi_spillPsychosocial"] - coef_values_with["treatment_pi_spillCapital"], 3)
    pval_ftest_3_with <- round(joint_f_test_3_with$`Pr(>F)`[2], 3)
    
    # Add significance stars for NO controls
    for (index in 1:3) {
      pval_ftest <- get(paste0("pval_ftest_", index, "_no"))
      coef_ftest <- get(paste0("coef_ftest_", index, "_no"))
      
      if (pval_ftest <= 0.01) {
        coef_ftest <- paste0(coef_ftest, "***")
      } else if (pval_ftest <= 0.05) {
        coef_ftest <- paste0(coef_ftest, "**")
      } else if (pval_ftest <= 0.10) {
        coef_ftest <- paste0(coef_ftest, "*")
      } else {
        coef_ftest <- as.character(coef_ftest)
      }
      assign(paste0("coef_ftest_", index, "_no"), coef_ftest)
    }
    
    # Add significance stars for WITH controls
    for (index in 1:3) {
      pval_ftest <- get(paste0("pval_ftest_", index, "_with"))
      coef_ftest <- get(paste0("coef_ftest_", index, "_with"))
      
      if (pval_ftest <= 0.01) {
        coef_ftest <- paste0(coef_ftest, "***")
      } else if (pval_ftest <= 0.05) {
        coef_ftest <- paste0(coef_ftest, "**")
      } else if (pval_ftest <= 0.10) {
        coef_ftest <- paste0(coef_ftest, "*")
      } else {
        coef_ftest <- as.character(coef_ftest)
      }
      assign(paste0("coef_ftest_", index, "_with"), coef_ftest)
    }
    
  } else {
    # For pooled treatment
    
    # MODEL WITHOUT CONTROLS 
    regmodel_no_cntrls <- paste0(depvar, " ~ ", curr_treat_var, " + ", strata_vars, "| 0 | 0 |", "cluster")
    reg_df <- followup_df_spill %>% filter(reg_hh_pi_spl_mrt==1)
    regmodbase_no_cntrls <- felm(formula=formula(regmodel_no_cntrls), data = reg_df)
    
    # MODEL WITH CONTROLS
    regmodel_with_cntrls <- paste0(depvar, " ~ ", curr_treat_var, " + ", strata_vars, " + ", 
                                   paste0(control_vars, collapse = " + "), "| 0 | 0 |", "cluster")
    regmodbase_with_cntrls <- felm(formula=formula(regmodel_with_cntrls), data = reg_df)
    
    coef_ftest_1_no = ""
    coef_ftest_2_no = ""
    coef_ftest_3_no = ""
    coef_ftest_1_with = ""
    coef_ftest_2_with = ""
    coef_ftest_3_with = ""
  }
  
  # Return results in tibble format
  out <- tibble(
    results_no_controls = list(regmodbase_no_cntrls),
    results_with_controls = list(regmodbase_with_cntrls),
    full_Psychosocial_no = coef_ftest_1_no,
    full_Capital_no = coef_ftest_2_no,
    psychosocial_Capital_no = coef_ftest_3_no,
    full_Psychosocial_with = coef_ftest_1_with,
    full_Capital_with = coef_ftest_2_with,
    psychosocial_Capital_with = coef_ftest_3_with
  )
  out
}

getEstimate_mechanism <- function(depvar, curr_treat_var, control_vars, strata_vars, cluster_vars, followup){
  
  if(curr_treat_var=="treatment_csh_trnsfr"){
    
    regmodel_cntrls <- paste0(depvar, " ~ ", curr_treat_var, " + ", strata_vars, " + ", paste0(control_vars, collapse = " + "), "| 0 | 0 |", " hhid")

    reg_df <- followup %>% 
      filter(reg_hh_csh_mrt==1)

    # Estimate models using felm
    regmodbase_cntrls <- felm(formula(regmodel_cntrls), data = reg_df, keepModel = T, na.action = na.omit) 
    
    coef_ftest_1 = ""
    
    coef_ftest_2 = ""
    
    coef_ftest_3 = ""
    

  }else if(curr_treat_var=="treatment_pi"){
    
    regmodel_cntrls <- paste0(depvar, " ~ ", curr_treat_var, " + ", strata_vars, " + ", paste0(control_vars, collapse = " + "), "| 0 | 0 |", " cluster")
    
    reg_df <- followup %>% 
      filter(reg_hh_pi_mrt==1)

    # Estimate both models using felm
    regmodbase_cntrls <- felm(formula=formula(regmodel_cntrls), data = reg_df)
    
    # Extract coefficients from the model
    coef_values <- coef(regmodbase_cntrls)

    
    # Perform joint F-test with clustered standard errors
    joint_f_test_1 <- linearHypothesis(regmodbase_cntrls, 
                                       c("treatment_piFull=treatment_piPsychosocial"),
                                       test = "F", singular.ok = TRUE)
    
    joint_f_test_2<- linearHypothesis(regmodbase_cntrls, 
                                      c("treatment_piFull=treatment_piCapital"),
                                      test = "F", singular.ok = TRUE)
    
    joint_f_test_3 <- linearHypothesis(regmodbase_cntrls, 
                                       c("treatment_piPsychosocial = treatment_piCapital"),
                                       test = "F", singular.ok = TRUE)
    
    # Get and format p-value with significance stars
    coef_ftest_1 <- round(coef_values["treatment_piFull"] - coef_values["treatment_piPsychosocial"],3)
    pval_ftest_1 <- round(joint_f_test_1$`Pr(>F)`[2],3)
    
    coef_ftest_2 <- round(coef_values["treatment_piFull"] - coef_values["treatment_piCapital"],3)
    pval_ftest_2 <- round(joint_f_test_2$`Pr(>F)`[2],3)
    
    coef_ftest_3 <- round(coef_values["treatment_piPsychosocial"] - coef_values["treatment_piCapital"],3)
    pval_ftest_3 <- round(joint_f_test_3$`Pr(>F)`[2],3)
    
    # Get and format p-value with significance stars
    for (index in 1:3) {
      # Retrieve values dynamically
      pval_ftest <- get(paste0("pval_ftest_", index))
      coef_ftest <- get(paste0("coef_ftest_", index))
      
      # Append significance stars based on p-value
      if (pval_ftest < 0.01) {
        coef_ftest <- paste0(coef_ftest, "***")
      } else if (pval_ftest < 0.05) {
        coef_ftest <- paste0(coef_ftest, "**")
      } else if (pval_ftest < 0.1) {
        coef_ftest <- paste0(coef_ftest, "*")
      } else {
        coef_ftest <- as.character(coef_ftest)
      }
      
      # Assign the modified value back to the global variable
      assign(paste0("coef_ftest_", index), coef_ftest)
    
    }
    
    }else{
      # Create formula for base model with full controls
      regmodel_cntrls <- paste0(depvar, " ~ ", curr_treat_var, " + ", strata_vars, " + ", paste0(control_vars, collapse = " + "), "| 0 | 0 |", " cluster")
      
      # for the case of the pool treatment 
      reg_df <- followup %>% 
        filter(reg_hh_pi_mrt==1)
      
      # Estimate both models using felm
      regmodbase_cntrls <- felm(formula=formula(regmodel_cntrls), data = reg_df)
      
      # Extract coefficients from the model
      coef_values <- coef(regmodbase_cntrls)
      ### browser()
      # Perform joint F-test with clustered standard errors
      coef_ftest_1 = ""
      
      coef_ftest_2 = ""
      
      coef_ftest_3 = ""
      
    }

  # Return results in tibble format
  out <- tibble(
    results_base = list(regmodbase_cntrls),
    full_Psychosocial = coef_ftest_1,
    full_Capital = coef_ftest_2,
    psychosocial_Capital = coef_ftest_3,
  )
  
  out

  
}


getEstimateGlobal_het <- function(depvar, curr_treat_var, het_var, control_vars, strata_vars, cluster_vars, followup){
  

  if(curr_treat_var=="treatment_csh_trnsfr"){

    regmodel_cntrls <- paste0(depvar, " ~ ", het_var, " * ", curr_treat_var, " + ", strata_vars, " + ", paste0(control_vars, collapse = " + "), "| 0 | 0 |", " hhid")
    
    reg_df <- followup %>% 
      filter(reg_hh_csh_mrt==1)

    # Estimate both models using felm
    regmodbase_cntrls <- getEstimatedf_het_csh(formula=formula(regmodel_cntrls), depvar, het_var, curr_treat_var, reg_df)
    
  }else{
    
    regmodel_cntrls <- paste0(depvar, " ~ ", het_var, " * ", curr_treat_var, " + ", strata_vars, " + ", paste0(control_vars, collapse = " + "), "| 0 | 0 |", " cluster")
    
    reg_df <- followup %>% filter(reg_hh_pi_mrt==1)

    # Estimate both models using felm
    regmodbase_cntrls <- getEstimatedf_het_pi(formula=formula(regmodel_cntrls), depvar, het_var, curr_treat_var, reg_df)
    
  }
  regmodbase_cntrls
}





getEstimatedf_het_csh <- function(formula, depvar, het_var, curr_treat_var, dfcurr, R = 1000) {
 
  # Fit the model using fixed effects
  mod <- felm(formula, data = dfcurr, keepModel = T, na.action = na.omit)
  

  # Extract the variance-covariance matrix
  vcmod <- vcov(mod)
  vcmod[is.na(vcmod)] <- 0
  
  # Define alpha values for confidence intervals
  alpha <- c(0.025, 0.05, 0.95, 0.975)
  
  # Extract coefficients and handle NAs
  mat_coef <- coef(mod)
  mat_coef[is.na(mat_coef)] <- 0
  
  # Simulate coefficients using multivariate normal distribution
  modU <- MASS::mvrnorm(R, mu = mat_coef, Sigma = vcmod)
  
  # Define matrix for coefficient selection
  matSelect <- matrix(0, nrow = 2, ncol = length(coef(mod)))
  colnames(matSelect) <- names(coef(mod))
  
  
  # Define crossed variables for interaction terms
  crosvar1 <- paste0(curr_treat_var,"Cash Assignment", sep = "")
  crosvar2 <- paste0(het_var,"1:",curr_treat_var,"Cash Assignment", sep = "")
  
  # ## browser()
  # Compute the mean nbr of conflict
  #medianhetvar <- median(mod$model %>% pull(sym(het_var)), na.rm = TRUE)
  
  # Populate the matrix for coefficient selection
  matSelect[, crosvar1] <- c(1, 1)
  matSelect[, crosvar2] <- c(0, 1)
  
  # Calculate the coefficients
  coefs <- as.numeric(matSelect %*% mat_coef)
  modU_CI <- t(matSelect %*% t(modU))
  
  # Function to access the coefficients
  CI <- t(apply(modU_CI, 2, quantile, probs = alpha, na.rm = TRUE))
  colnames(CI) <- paste0("CI", parse_number(colnames(CI)))
  
  # Extract relevant informations
  vect_temp_1 <- mod$model %>% 
    pull(sym(het_var))
  
  vect_temp_2 <- mod$model %>% 
    filter(treatment_csh_trnsfr == "Cash Assignment") %>% 
    pull(sym(het_var))
  
  # Create a tibble with results
  out <- tibble(
    name="cash",
    estimate = c("No", "Yes"),
    typeTreat = c("Cash Assignment"),
    pe = coefs,
    N = nrow(mod$model),
    Ntype = c(as.numeric(table(vect_temp_1))[1], as.numeric(table(vect_temp_1))[2]),
    Ntype_treat = c(as.numeric(table(vect_temp_2))[1], as.numeric(table(vect_temp_2))[2]),
    #medianhetvar = medianhetvar,
    r.squared = summary(mod)$r.squared
  )
  out <- bind_cols(out, as.tibble(CI))
  
  # Return the tibble with results
  out
}

# Function to estimate the model with squared exposition variable
getEstimatedf_het_pi <- function(formula, depvar, het_var, curr_treat_var, dfcurr, R = 1000) {

  # Fit the model using fixed effects
  mod <- felm(formula, data = dfcurr, keepModel = T, na.action = na.omit)
  
  # Extract the variance-covariance matrix
  vcmod <- vcov(mod)
  vcmod[is.na(vcmod)] <- 0
  
  # Define alpha values for confidence intervals
  alpha <- c(0.025, 0.05, 0.95, 0.975)
  
  # Extract coefficients and handle NAs
  mat_coef <- coef(mod)
  mat_coef[is.na(mat_coef)] <- 0
  
  # Simulate coefficients using multivariate normal distribution
  modU <- MASS::mvrnorm(R, mu = mat_coef, Sigma = vcmod)
  
  if(curr_treat_var=="treatment_pi"){
  # Define matrix for coefficient selection
  matSelect <- matrix(0, nrow = 6, ncol = length(coef(mod)))
  colnames(matSelect) <- names(coef(mod))
  
  
  # Define crossed variables for interaction terms
  crosvar1 <- paste0(curr_treat_var,"Capital", sep = "")
  crosvar2 <- paste0(curr_treat_var,"Psychosocial", sep = "")
  crosvar3 <- paste0(curr_treat_var,"Full", sep = "")
  
  crosvar4 <- paste0(het_var,"1:",curr_treat_var,"Capital", sep = "")
  crosvar5 <- paste0(het_var,"1:",curr_treat_var,"Psychosocial", sep = "")
  crosvar6 <- paste0(het_var,"1:",curr_treat_var,"Full", sep = "")

  # Populate the matrix for coefficient selection
  matSelect[, crosvar1] <- c(1, 1, 0, 0, 0, 0)
  matSelect[, crosvar2] <- c(0, 0, 1, 1, 0, 0)
  matSelect[, crosvar3] <- c(0, 0, 0, 0, 1, 1)
  
  matSelect[, crosvar4] <- c(0, 1, 0, 0, 0, 0)
  matSelect[, crosvar5] <- c(0, 0, 0, 1, 0, 0)
  matSelect[, crosvar6] <- c(0, 0, 0, 0, 0, 1)
  
  # Calculate the coefficients
  coefs <- as.numeric(matSelect %*% mat_coef)
  modU_CI <- t(matSelect %*% t(modU))
  
  # Function to access the coefficients
  CI <- t(apply(modU_CI, 2, quantile, probs = alpha, na.rm = TRUE))
  colnames(CI) <- paste0("CI", parse_number(colnames(CI)))
  
  # Extract relevant informations
  vect_temp_1 <- mod$model %>% 
    pull(sym(het_var))
  
  vect_temp_2 <- mod$model %>% 
    filter(treatment_pi == "Capital") %>% 
    pull(sym(het_var))
  
  vect_temp_3 <- mod$model %>% 
    filter(treatment_pi == "Psychosocial") %>% 
    pull(sym(het_var))
  
  vect_temp_4 <- mod$model %>% 
    filter(treatment_pi == "Full") %>% 
    pull(sym(het_var))
  
  # Create a tibble with results
  out <- tibble(
    name="pi",
    estimate = c("No", "Yes","No", "Yes","No", "Yes"),
    typeTreat = c("Capital","Capital", "Psychosocial","Psychosocial","Full","Full"),
    pe = coefs,
    N = nrow(mod$model),
    Ntype = c(as.numeric(table(vect_temp_1))[1], as.numeric(table(vect_temp_1))[2],
              as.numeric(table(vect_temp_1))[1], as.numeric(table(vect_temp_1))[2],
              as.numeric(table(vect_temp_1))[1], as.numeric(table(vect_temp_1))[2]),
    Ntype_treat = c(as.numeric(table(vect_temp_2))[1], as.numeric(table(vect_temp_2))[2],
                    as.numeric(table(vect_temp_3))[1], as.numeric(table(vect_temp_3))[2],
                    as.numeric(table(vect_temp_4))[1], as.numeric(table(vect_temp_4))[2]),
    r.squared = summary(mod)$r.squared
  )
  }else{
    # Define matrix for coefficient selection
    matSelect <- matrix(0, nrow = 2, ncol = length(coef(mod)))
    colnames(matSelect) <- names(coef(mod))
    
    
    # Define crossed variables for interaction terms
    crosvar1 <- paste0(curr_treat_var,"Pool", sep = "")
    crosvar2 <- paste0(het_var,"1:",curr_treat_var,"Pool", sep = "")
    
    # Populate the matrix for coefficient selection
    matSelect[, crosvar1] <- c(1, 1)
    matSelect[, crosvar2] <- c(0, 1)
    
    # Calculate the coefficients
    coefs <- as.numeric(matSelect %*% mat_coef)
    modU_CI <- t(matSelect %*% t(modU))
    
    # Function to access the coefficients
    CI <- t(apply(modU_CI, 2, quantile, probs = alpha, na.rm = TRUE))
    colnames(CI) <- paste0("CI", parse_number(colnames(CI)))
    
    # Extract relevant informations
    vect_temp_1 <- mod$model %>% 
      pull(sym(het_var))
    
    vect_temp_2 <- mod$model %>% 
      filter(treatment_pi_pool == "Pool") %>% 
      pull(sym(het_var))
    
    # Create a tibble with results
    out <- tibble(
      name="pi",
      estimate = c("No", "Yes"),
      typeTreat = c("Pool","Pool"),
      pe = coefs,
      N = nrow(mod$model),
      Ntype = c(as.numeric(table(vect_temp_1))[1], as.numeric(table(vect_temp_1))[2]),
      Ntype_treat = c(as.numeric(table(vect_temp_2))[1], as.numeric(table(vect_temp_2))[2]),
      r.squared = summary(mod)$r.squared
    )
  }
    
  out <- bind_cols(out, as.tibble(CI))
  
  # Return the tibble with results
  out
}

# Function to estimate the ML for each outcome index variable
getEstimateML_IPV <- function(curr_ipv, listOutcomes, treatment_vars, followup_MRT_hh_cntrl, model_methods, fitControl) {
  #)## browser()
  # Prepare data subset
  train_data <- followup_MRT_hh_cntrl %>%
    dplyr::select(-c(setdiff(listOutcomes, curr_ipv), hhid,  all_of(treatment_vars), ends_with("_bl"))) %>% 
    filter(if_all(all_of(curr_ipv), ~ !is.na(.)))
  
  
  # Remove constant factor columns (with < 2 levels)
  train_data <- train_data %>%
    dplyr::select(where(~ !is.factor(.) || nlevels(.) > 1))
  
  
  # Perform KNN imputation (default k = 10)
  # It returns a data.frame with imputed values
  #train_data_imptd <- kNN(train_data, k = 5, imp_var = FALSE, impNA = TRUE)  # Don't keep extra columns showing where imputations happened
  #train_data_imptd <- train_data
  
  # Detect and register all available cores (for Parallel Processing)
  # registerDoMC(cores = num_cores)
  # ## browser()
  # Define the control using a selection function
  ## Linear model
  # control_lm <- rfeControl(functions=lmFuncs, 
  #                          method="cv", 
  #                          number=2,  
  #                          returnResamp = "final",
  #                          saveDetails = FALSE, 
  #                          verbose = TRUE,
  #                          allowParallel=TRUE)
  # 
  # ## Random Forest
  # control_rf <- rfeControl(functions=rfFuncs, 
  #                          method="cv", 
  #                          number=2,  
  #                          returnResamp = "final",
  #                          saveDetails = FALSE, 
  #                          verbose = TRUE,
  #                          allowParallel=TRUE)
  # 
  # ## Bagging
  # control_treebag <- rfeControl(functions=treebagFuncs, 
  #                               method="cv", 
  #                               number=2,  
  #                               returnResamp = "final",
  #                               saveDetails = FALSE, 
  #                               verbose = TRUE,
  #                               allowParallel=TRUE)
  # 
  ## XgbTree : Extreme Gradient Boosting (trees); excellent for structured/tabular data.
  
  ## Tuning parameters that must be determined at each iteration.
  # control_allm <- rfeControl(functions=caretFuncs, 
  #                            method="cv", 
  #                            number=2,  
  #                            returnResamp = "final",
  #                            saveDetails = FALSE, 
  #                            verbose = TRUE,
  #                            allowParallel=TRUE)
  
  # Run the RFE algorithm
  ## Linear model
  # results_rfe_lm <- rfe(
  #   train_data_imptd %>% dplyr::select(-!!sym(curr_ipv)), 
  #   train_data_imptd %>% pull(!!sym(curr_ipv)), 
  #   sizes=c(2:100), 
  #   na.action = na.omit,
  #   metric = "RMSE",
  #   rfeControl=control_lm)
  # 
  ## Random Forest
  # results_rfe_rf <- rfe(
  #   train_data_imptd %>% dplyr::select(-!!sym(curr_ipv)), 
  #   train_data_imptd %>% pull(!!sym(curr_ipv)), 
  #   sizes=c(2:100), 
  #   na.action = na.omit,
  #   metric = "RMSE",
  #   rfeControl=control_rf)
  # 
  ## Bagging
  # results_rfe_treebag <- rfe(
  #   train_data_imptd %>% dplyr::select(-!!sym(curr_ipv)), 
  #   train_data_imptd %>% pull(!!sym(curr_ipv)), 
  #   sizes=c(2:100), 
  #   na.action = na.omit,
  #   metric = "RMSE",
  #   rfeControl=control_treebag)
  
  
  ## Tuning parameters that must be determined at each iteration.
  # results_rfe_allm <- rfe(
  #   train_data_imptd %>% dplyr::select(-!!sym(curr_ipv)), 
  #   train_data_imptd %>% pull(!!sym(curr_ipv)), 
  #   sizes=c(2:100), 
  #   na.action = na.omit,
  #   metric = "RMSE",
  #   rfeControl=control_allm)
  
  # list the chosen features
  ## Linear model
  # predictors(results_rfe_lm)
  # varImp(results_rfe_lm$fit, top=20)
  # lst_feature_lm  <- results_rfe_lm$optVariables
  # 
  ## Random Forest
  # predictors(results_rfe_rf)
  # varImp(results_rfe_rf$fit, top=20)
  # lst_feature_rf  <- results_rfe_rf$optVariables
  # 
  ## Bagging
  # predictors(results_rfe_treebag)
  # varImp(results_rfe_treebag$fit)
  # lst_feature_treebag  <- varImp(results_rfe_treebag$fit) %>%
    # as.data.frame() %>%
    # rownames_to_column(var = "col_names") %>%
    # filter(Overall!=0) %>%
    # arrange(desc(Overall)) %>%
    # pull(col_names)
  
  ## Tuning parameters that must be determined at each iteration.
  # predictors(results_rfe_allm)
  # varImp(results_rfe_allm$fit, top=20)
  # lst_feature_allm  <- results_rfe_allm$optVariables
  # 
  # List of different feature sets to test - contains four different pre-defined sets of features
  #lst_features = list(lst_feature_lm, lst_feature_rf, lst_feature_treebag, lst_feature_allm)
  #lst_features = list(lst_feature_lm, lst_feature_rf, lst_feature_treebag)
  
  # This creates a data frame of results by mapping over each feature set
  # df_results <- purrr::map_dfr(lst_features, function(feature_set) {
  #   
  #   # Create a data subset with only the current feature set and target variable
  #   data_subset <- train_data %>% dplyr::select(all_of(feature_set), !!sym(curr_ipv))
  #   
    # Create a model formula: target ~ all predictors
    model_formula <- reformulate(".", response = curr_ipv)
    
    # Train each model type on the current feature set
    model_list <- purrr::map(model_methods, function(method) {
      # Train the model with cross-validation (fitControl) and RMSE optimization
      train(model_formula, data = train_data, method = method, trControl = fitControl, metric = "RMSE", maximize=FALSE,na.action = na.omit, verbose = TRUE)
    }) %>% set_names(model_methods)

    # Construct a results tibble with performance metrics for each model
    tibble(
      #List_feature = paste(feature_set, collapse = ", "),  # Concatenate feature names
      Model_names = names(model_list),                          # Model method name
      # Model = list(model_list), # The model
      curr_ipv = curr_ipv,
      RMSE = purrr::map_dbl(model_list, ~ mean(.x$results$RMSE, na.rm = TRUE)),    # Mean RMSE
      MAE = purrr::map_dbl(model_list, ~ mean(.x$results$MAE, na.rm = TRUE)),      # Mean MAE
      Rsquared = purrr::map_dbl(model_list, ~ mean(.x$results$Rsquared, na.rm = TRUE))  # Mean R²
    )
  # }, .progress = TRUE)
  

}
  



# Function to estimate the ML for each outcome index variable
getEstimatePred_IPV <- function(curr_ipv, listOutcomes, treatment_vars,  
                                vars_to_keep_ml, method, fitControl, followup_MRT_hh_cntrl, followup_MRT_hh_imptd) {
 

  # Prepare data subset
  train_data <- followup_MRT_hh_cntrl %>%
    dplyr::select(-c(setdiff(listOutcomes, curr_ipv), hhid, all_of(treatment_vars), ends_with("_bl")))

  ## To have the sames features in the dataset
  poss_features = names(train_data)

  # Create a model formula: target ~ all predictors
  model_formula <- reformulate(".", response = curr_ipv)
  
  # Prediction dataset of baseline IPV
  ## Baseline
  pred_data_baseline <- followup_MRT_hh_imptd %>%
    dplyr::select(hhid, ends_with("_bl"), all_of(vars_to_keep_ml), !!sym(curr_ipv))%>%
    rename_with(~ gsub("_bl$", "", .x), ends_with("_bl")) %>% 
    select(hhid, poss_features)

  ## Followup
  pred_data_followup <- followup_MRT_hh_imptd %>%
    dplyr::select(-c(all_of(treatment_vars), ends_with("_bl"))) %>% 
    select(hhid, poss_features)
  
  # Train the model with cross-validation (fitControl) and RMSE optimization
  model_trained <- train(model_formula, data = train_data, method = method, trControl = fitControl, metric = "RMSE", maximize=FALSE,na.action = na.omit, verbose = TRUE)
  

  # Construct a results tibble with performance metrics for each model
  bind_rows(
    tibble(
      hhid = pred_data_baseline$hhid %>% as.vector(),
      curr_ipv = curr_ipv,
      type="baseline",
      feature = paste(varImp(model_trained$finalModel) %>%
                             as.data.frame() %>% 
                             rownames_to_column(var = "col_names") %>% 
                             filter(Overall!=0) %>% 
                             arrange(desc(Overall)) %>% 
                             pull(col_names), collapse = ", "),  # Concatenate feature names
      RMSE = mean(model_trained$results$RMSE, na.rm = TRUE),    # Best RMSE
      MAE = mean(model_trained$results$MAE, na.rm = TRUE),      # Best MAE
      Rsquared = mean(model_trained$results$Rsquared, na.rm = TRUE),  # Best R²
      importance = paste(varImp(model_trained$finalModel) %>%
                    as.data.frame() %>%
                    rownames_to_column(var = "col_names") %>%
                    filter(Overall!=0) %>%
                    arrange(desc(Overall)) %>%
                    pull(Overall), collapse = ", "),
      predicted_val = predict(model_trained, newdata = pred_data_baseline)
  ),
  tibble(
    hhid = pred_data_followup$hhid %>% as.vector(),
    curr_ipv = curr_ipv,
    type="followup",
    feature = paste(varImp(model_trained$finalModel) %>%
                      as.data.frame() %>% 
                      rownames_to_column(var = "col_names") %>% 
                      filter(Overall!=0) %>% 
                      arrange(desc(Overall)) %>% 
                      pull(col_names), collapse = ", "),  # Concatenate feature names
    RMSE = mean(model_trained$results$RMSE, na.rm = TRUE),    # Best RMSE
    MAE = mean(model_trained$results$MAE, na.rm = TRUE),      # Best MAE
    Rsquared = mean(model_trained$results$Rsquared, na.rm = TRUE),  # Best R²
    importance = paste(varImp(model_trained$finalModel) %>%
                         as.data.frame() %>%
                         rownames_to_column(var = "col_names") %>%
                         filter(Overall!=0) %>%
                         arrange(desc(Overall)) %>%
                         pull(Overall), collapse = ", "),
    predicted_val = predict(model_trained, newdata = pred_data_followup)
  )
  )
  # }, .progress = TRUE)
  
  
}


################################## Het by baseline pred IPV #########################################################

# compute_group_summary <- function(group_var, treat_var, followup) {
#   
#   # Step 1: Summary by group and treatment
#   df_grouped_two <- data %>%
#     group_by(!!sym(group_var), !!sym(treat_var)) %>%
#     summarise(
#       n_group = n(),
#       CI0.5 = mean(CI0.5, na.rm = TRUE),
#       CI2.5 = mean(CI2.5, na.rm = TRUE),
#       CI97.5 = mean(CI97.5, na.rm = TRUE),
#       CI99.5 = mean(CI99.5, na.rm = TRUE)
#     )
#   
#   # Step 2: Summary by group only (aggregated)
#   df_grouped_one <- df_grouped_two %>%
#     group_by(!!sym(group_var)) %>%
#     summarise(
#       n_tot = sum(n_group),
#       CI0.5 = mean(CI0.5, na.rm = TRUE),
#       CI2.5 = mean(CI2.5, na.rm = TRUE),
#       CI97.5 = mean(CI97.5, na.rm = TRUE),
#       CI99.5 = mean(CI99.5, na.rm = TRUE)
#     ) 
#   
#   # Return the joined output: two summaries in one
#   out <- inner_join( )
#   
#   (
#     grouped_by_treatment = df_grouped_two,
#     aggregated_by_group = df_grouped_one
#   )
# }


# getEstimateGlobal_CF <- function(depvar, treat_var, followup){
# 
#   if(treat_var=="treatment_csh_trnsfr"){
# 
#     reg_df <- followup %>%
#       filter(reg_hh_csh_mrt==1) 
#     
#     # Extract clustering variable (cluster) for clustered standard errors
#     clusters <- reg_df %>%  
#       dplyr::select(hhid) %>% 
#       pull()
#     
#     
#   }else{
#     reg_df <- followup %>%
#       filter(reg_hh_pi_mrt==1) 
#     
#     # Extract clustering variable (cluster) for clustered standard errors
#     clusters <- reg_df %>%  
#       dplyr::select(cluster) %>% 
#       pull()
#   }
# 
#   # Y: Extract outcome variable (IPV severity index) from the filtered dataset
#   Y <- reg_df %>%  
#     dplyr::select(!!sym(depvar)) %>% 
#     pull() # Outcome variable
#   
#   # W: Extract treatment variable from the filtered dataset
#   W <- reg_df %>%  
#     dplyr::select(!!sym(treat_var))%>% 
#     pull()   # Treatment var
#   
#   # X: Create matrix of covariates by removing other variables
#   X <- reg_df %>%  
#     dplyr::select(-c(!!sym(treat_var),!!sym(depvar))) %>% 
#     as.matrix()  # Covariates matrix
#   
#   # Fit the causal forest
#   cf_model <- causal_forest(X=X, Y=Y, W=W, clusters = clusters, num.trees = 4000,
#                             mtry = 700, honesty = TRUE, # 1/3 of variables mtry = 700, 
#                             min.node.size = 10,
#                             tune.parameters = c("sample.fraction", "mtry", "min.node.size", 
#                                                "honesty.fraction", "honesty.prune.leaves", 
#                                                "alpha", "imbalance.penalty"),
#                             tune.num.trees = 500,
#                             tune.num.reps = 500,
#                             tune.num.draws = 1000)
#   
#   
#   # Estimate the average treatment effect (ATE)
#   ATE = average_treatment_effect(cf_model) # Compute the ATE
#   pval = 2 * pnorm(-abs(ATE[1]/ATE[2])) # Compute the p-value
#   
#   # Print the results of the ATE
#   print(sprintf("The estimated ATE is %s, the standard error is %s and the pvalue is %s. The outcome is var is %s and the treatment variable %s.", 
#                 round(ATE[1],2),round(ATE[2],2), round(pval,2), depvar, treat_var))
#   
#   # Predict individual treatment effects
#   cf_predictions <- predict(cf_model, X, estimate.variance = TRUE)
#   
#   # Create a tibble with results
#   out <- tibble(
#     hhid = reg_df$hhid,
#     estimated_ate_value = ATE[1],
#     estimated_ate_std = ATE[2],
#     estimated_ate_pvalue = pval,
#     predicted_val = cf_predictions$predictions,
#     predicted_std = cf_predictions$variance.estimates
#   )
#   
#   out
# 
# }

getEstimateGlobal_CF_median <- function(depvar, treat_var, followup, n_forests = 5, num_trees = 2000) {
  
  if (treat_var == "treatment_csh_trnsfr") {
    reg_df <- followup %>% filter(reg_hh_csh_mrt == 1)
    clusters <- reg_df %>% pull(hhid)
  } else {
    reg_df <- followup %>% filter(reg_hh_pi_mrt == 1)
    clusters <- reg_df %>% pull(cluster)
  }
  
  Y <- reg_df %>% dplyr::select(!!sym(depvar)) %>% pull()
  W <- reg_df %>% dplyr::select(!!sym(treat_var)) %>% pull()
  X <- reg_df %>% dplyr::select(-c(!!sym(treat_var), !!sym(depvar))) %>% as.matrix()
  
  # Store predictions for each forest
  pred_matrix <- matrix(NA, nrow = nrow(X), ncol = n_forests)
  ate_list <- numeric(n_forests)

  for (i in 1:n_forests) {
    
    cf_model <- causal_forest(
      X = X, Y = Y, W = W, clusters = clusters,
      num.trees = num_trees,
      mtry = min(ncol(X), 100),
      honesty = TRUE,
      min.node.size = 10,
      tune.parameters = c("sample.fraction", "mtry", "min.node.size", 
                          "honesty.fraction", "honesty.prune.leaves", 
                          "alpha", "imbalance.penalty"),
      tune.num.trees = i*100,
      tune.num.reps = i*100,
      tune.num.draws = i*200
    )
    
    ate <- average_treatment_effect(cf_model)[1]
    ate_list[i] <- ate
    
    preds <- predict(cf_model, X, estimate.variance = FALSE)
    pred_matrix[, i] <- preds$predictions
  }
  
  # Compute median predictions across forests
  pred_median <- apply(pred_matrix, 1, median)
  
  # ATE summary across forests
  ate_median <- median(ate_list)
  ate_sd <- sd(ate_list)
  pval <- 2 * pnorm(-abs(ate_median / ate_sd))
  
  print(sprintf(
    "Median ATE from %d forests is %s, SD is %s, and p-value is %s. Outcome: %s, Treatment: %s.",
    n_forests, round(ate_median, 2), round(ate_sd, 2), round(pval, 2), depvar, treat_var
  ))
  
  # Output tibble
  out <- tibble(
    hhid = reg_df$hhid,
    estimated_ate_value = ate_median,
    estimated_ate_std = ate_sd,
    estimated_ate_pvalue = pval,
    predicted_val = pred_median
  )
  
  return(out)
}

getEstimateGlobal_ML_het <- function(depvar, curr_treat_var, het_var, control_vars, strata_vars, cluster_vars, followup){
  

  if(curr_treat_var=="treatment_csh_trnsfr"){
    
    regmodel_cntrls <- paste0(depvar, " ~ ", het_var, " * ", curr_treat_var, " + ", strata_vars, " + ", paste0(control_vars, collapse = " + "), "| 0 | 0 |", " hhid")

    reg_df <- followup %>%
      # filter(!!sym(curr_treat_var)!="None") %>% 
      filter(reg_hh_csh_mrt==1) %>% 
      distinct(hhid,cluster,hhh_fem_bl, hhh_poly, hhh_edu, .keep_all = TRUE)
    
    # Estimate both models using felm
    reg_results <- getEstimatedf_ML_het_csh(formula=formula(regmodel_cntrls), depvar, het_var, curr_treat_var, strata_vars, reg_df)
    
  }else{
    regmodel_cntrls <- paste0(depvar, " ~ ", het_var, " * ", curr_treat_var, " + ", strata_vars, " + ", paste0(control_vars, collapse = " + "), "| 0 | 0 |", " cluster")
    
    reg_df <- followup %>%
      filter(reg_hh_pi_mrt==1) %>% 
      distinct(hhid,cluster,hhh_fem_bl, hhh_poly, hhh_edu, .keep_all = TRUE)

    if(het_var=="decile_baseline_ipv"){
      
    # Estimate both models using felm
      reg_results <- getEstimatedf_ML_het_pi_dec(formula=formula(regmodel_cntrls), depvar, het_var, curr_treat_var, strata_vars, reg_df)
    }else if(het_var=="quartile_baseline_ipv"){

      # Estimate both models using felm
      reg_results <- getEstimatedf_ML_het_pi_quan(formula=formula(regmodel_cntrls), depvar, het_var, curr_treat_var, strata_vars, reg_df)
    }else{

      # Estimate both models using felm
      reg_results <- getEstimatedf_ML_het_pi_med(formula=formula(regmodel_cntrls), depvar, het_var, curr_treat_var, strata_vars, reg_df)
    }
    
  }
  reg_results
}

# Function to estimate the model with squared exposition variable
getEstimatedf_ML_het_pi_med <- function(formula, depvar, het_var, curr_treat_var, dfcurr, R = 1000) {

  # Fit the model using fixed effects
  mod <- felm(formula, data = dfcurr, keepModel = T, na.action = na.omit)
  
  # Extract the variance-covariance matrix
  vcmod <- vcov(mod)
  vcmod[is.na(vcmod)] <- 0
  
  # Define alpha values for confidence intervals
  alpha <- c(0.025, 0.05, 0.95, 0.975)
  
  # Extract coefficients and handle NAs
  mat_coef <- coef(mod)
  mat_coef[is.na(mat_coef)] <- 0
  
  # Simulate coefficients using multivariate normal distribution
  modU <- MASS::mvrnorm(R, mu = mat_coef, Sigma = vcmod)
  
  if(curr_treat_var=="treatment_pi"){
    # Define matrix for coefficient selection
    matSelect <- matrix(0, nrow = 6, ncol = length(coef(mod)))
    colnames(matSelect) <- names(coef(mod))
    
    # Define crossed variables for interaction terms
    crosvar11 <- paste0(curr_treat_var,"Capital", sep = "")
    crosvar21 <- paste0(curr_treat_var,"Psychosocial", sep = "")
    crosvar31 <- paste0(curr_treat_var,"Full", sep = "")
    
    crosvar12 <- paste0(het_var,"2:",curr_treat_var,"Capital", sep = "")
    crosvar22 <- paste0(het_var,"2:",curr_treat_var,"Psychosocial", sep = "")
    crosvar32 <- paste0(het_var,"2:",curr_treat_var,"Full", sep = "")    
    
    # Populate the matrix for coefficient selection
    matSelect[, crosvar11] <- c(1, 0, 0, 1, 0, 0)
    matSelect[, crosvar21] <- c(0, 1, 0, 0, 1, 0)
    matSelect[, crosvar31] <- c(0, 0, 1, 0, 0, 1)
    
    matSelect[, crosvar12] <- c(0, 0, 0, 1, 0, 0)
    matSelect[, crosvar22] <- c(0, 0, 0, 0, 1, 0)
    matSelect[, crosvar32] <- c(0, 0, 0, 0, 0, 1)
   
    # Calculate the coefficients
    coefs <- as.numeric(matSelect %*% mat_coef)
    modU_CI <- t(matSelect %*% t(modU))
    
    # Function to access the coefficients
    CI <- t(apply(modU_CI, 2, quantile, probs = alpha, na.rm = TRUE))
    colnames(CI) <- paste0("CI", parse_number(colnames(CI)))
    
    # Extract relevant informations
    vect_temp_1 <- mod$model %>% 
      pull(sym(het_var))
    
    vect_temp_2 <- mod$model %>% 
      filter(treatment_pi == "Capital") %>% 
      pull(sym(het_var))
    
    vect_temp_3 <- mod$model %>% 
      filter(treatment_pi == "Psychosocial") %>% 
      pull(sym(het_var))
    
    vect_temp_4 <- mod$model %>% 
      filter(treatment_pi == "Full") %>% 
      pull(sym(het_var))
    
    # Create a tibble with results
    out <- tibble(
      name="pi",
      estimate_quant = rep(as.character(1:2), each = 3),
      typeTreat = rep(c("Capital","Psychosocial","Full"), 2),
      pe = coefs,
      N = nrow(mod$model),
      Ntype = rep(c(as.numeric(table(vect_temp_1))[1], as.numeric(table(vect_temp_1))[2]), each = 3),
      
      Ntype_treat = c(as.numeric(table(vect_temp_2))[1], as.numeric(table(vect_temp_3))[1], as.numeric(table(vect_temp_4))[1],
                      as.numeric(table(vect_temp_2))[2], as.numeric(table(vect_temp_3))[2], as.numeric(table(vect_temp_4))[2]),
      
      r.squared = summary(mod)$r.squared
    )
  }else{
    # Define matrix for coefficient selection
    matSelect <- matrix(0, nrow = 2, ncol = length(coef(mod)))
    colnames(matSelect) <- names(coef(mod))
    
    # Define crossed variables for interaction terms
    crosvar1 <- paste0(curr_treat_var,"Pool", sep = "")
    crosvar2 <- paste0(het_var,"2:",curr_treat_var,"Pool", sep = "")

    
    # Populate the matrix for coefficient selection
    matSelect[, crosvar1] <- c(1, 1)
    matSelect[, crosvar2] <- c(0, 1)

    
    # Calculate the coefficients
    coefs <- as.numeric(matSelect %*% mat_coef)
    modU_CI <- t(matSelect %*% t(modU))
    
    # Function to access the coefficients
    CI <- t(apply(modU_CI, 2, quantile, probs = alpha, na.rm = TRUE))
    colnames(CI) <- paste0("CI", parse_number(colnames(CI)))
    
    # Extract relevant informations
    vect_temp_1 <- mod$model %>% 
      pull(sym(het_var))
    
    vect_temp_2 <- mod$model %>% 
      filter(treatment_pi_pool == "Pool") %>% 
      pull(sym(het_var))
    
    # Create a tibble with results
    out <- tibble(
      name="pi",
      estimate_quant = as.character(1:2),
      typeTreat = "Pool",
      pe = coefs,
      N = nrow(mod$model),
      Ntype = c(as.numeric(table(vect_temp_1))[1], as.numeric(table(vect_temp_1))[2]),
      Ntype_treat = c(as.numeric(table(vect_temp_2))[1], as.numeric(table(vect_temp_2))[2]),
      r.squared = summary(mod)$r.squared
    )
  }
  
  
  out <- bind_cols(out, as.tibble(CI))
  
  # Return the tibble with results
  out
}


getEstimatedf_ML_het_csh <- function(formula, depvar, het_var, curr_treat_var, strata_vars, dfcurr, R = 1000) {

  # Fit the model using fixed effects
  mod <- felm(formula, data = dfcurr, keepModel = T, na.action = na.omit)


  # Extract the variance-covariance matrix
  vcmod <- vcov(mod)
  vcmod[is.na(vcmod)] <- 0
  
  # Define alpha values for confidence intervals
  alpha <- c(0.025, 0.05, 0.95, 0.975)
  
  # Extract coefficients and handle NAs
  mat_coef <- coef(mod)
  mat_coef[is.na(mat_coef)] <- 0
  
  # Simulate coefficients using multivariate normal distribution
  modU <- MASS::mvrnorm(R, mu = mat_coef, Sigma = vcmod)
  
  if(het_var=="decile_baseline_ipv"){
    
    # Define matrix for coefficient selection
    matSelect <- matrix(0, nrow = 10, ncol = length(coef(mod)))
    colnames(matSelect) <- names(coef(mod))
    
    
    # Define crossed variables for interaction terms
    crosvar1 <- paste0(curr_treat_var,"Cash Assignment", sep = "")
    crosvar2 <- paste0(het_var,"2:",curr_treat_var,"Cash Assignment", sep = "")
    crosvar3 <- paste0(het_var,"3:",curr_treat_var,"Cash Assignment", sep = "")
    crosvar4 <- paste0(het_var,"4:",curr_treat_var,"Cash Assignment", sep = "")
    crosvar5 <- paste0(het_var,"5:",curr_treat_var,"Cash Assignment", sep = "")
    crosvar6 <- paste0(het_var,"6:",curr_treat_var,"Cash Assignment", sep = "")
    crosvar7 <- paste0(het_var,"7:",curr_treat_var,"Cash Assignment", sep = "")
    crosvar8 <- paste0(het_var,"8:",curr_treat_var,"Cash Assignment", sep = "")
    crosvar9 <- paste0(het_var,"9:",curr_treat_var,"Cash Assignment", sep = "")
    crosvar10 <- paste0(het_var,"10:",curr_treat_var,"Cash Assignment", sep = "")
    
    
    # Populate the matrix for coefficient selection
    matSelect[, crosvar1] <-  c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
    matSelect[, crosvar2] <-  c(0, 1, 0, 0, 0, 0, 0, 0, 0, 0)
    matSelect[, crosvar3] <-  c(0, 0, 1, 0, 0, 0, 0, 0, 0, 0)
    matSelect[, crosvar4] <-  c(0, 0, 0, 1, 0, 0, 0, 0, 0, 0)
    matSelect[, crosvar5] <-  c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0)
    matSelect[, crosvar6] <-  c(0, 0, 0, 0, 0, 1, 0, 0, 0, 0)
    matSelect[, crosvar7] <-  c(0, 0, 0, 0, 0, 0, 1, 0, 0, 0)
    matSelect[, crosvar8] <-  c(0, 0, 0, 0, 0, 0, 0, 1, 0, 0)
    matSelect[, crosvar9] <-  c(0, 0, 0, 0, 0, 0, 0, 0, 1, 0)
    matSelect[, crosvar10] <-  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 1)
    
    
  }else if(het_var=="quartile_baseline_ipv"){
    
    # Define matrix for coefficient selection
    matSelect <- matrix(0, nrow = 5, ncol = length(coef(mod)))
    colnames(matSelect) <- names(coef(mod))
    
    
    # Define crossed variables for interaction terms
    crosvar1 <- paste0(curr_treat_var,"Cash Assignment", sep = "")
    crosvar2 <- paste0(het_var,"2:",curr_treat_var,"Cash Assignment", sep = "")
    crosvar3 <- paste0(het_var,"3:",curr_treat_var,"Cash Assignment", sep = "")
    crosvar4 <- paste0(het_var,"4:",curr_treat_var,"Cash Assignment", sep = "")
    crosvar5 <- paste0(het_var,"5:",curr_treat_var,"Cash Assignment", sep = "")
    
    
    # Populate the matrix for coefficient selection
    matSelect[, crosvar1] <-  c(1, 1, 1, 1, 1)
    matSelect[, crosvar2] <-  c(0, 1, 0, 0, 0)
    matSelect[, crosvar3] <-  c(0, 0, 1, 0, 0)
    matSelect[, crosvar4] <-  c(0, 0, 0, 1, 0)
    matSelect[, crosvar5] <-  c(0, 0, 0, 0, 1)
  }else{

    # Define matrix for coefficient selection
    matSelect <- matrix(0, nrow = 2, ncol = length(coef(mod)))
    colnames(matSelect) <- names(coef(mod))
    
    
    # Define crossed variables for interaction terms
    crosvar1 <- paste0(curr_treat_var,"Cash Assignment", sep = "")
    crosvar2 <- paste0(het_var,"2:",curr_treat_var,"Cash Assignment", sep = "")
    
    
    # Populate the matrix for coefficient selection
    matSelect[, crosvar1] <-  c(1, 1)
    matSelect[, crosvar2] <-  c(0, 1)
  }
  
    
  # Calculate the coefficients
  coefs <- as.numeric(matSelect %*% mat_coef)
  modU_CI <- t(matSelect %*% t(modU))
  
  # Function to access the coefficients
  CI <- t(apply(modU_CI, 2, quantile, probs = alpha, na.rm = TRUE))
  colnames(CI) <- paste0("CI", parse_number(colnames(CI)))
  
  # Extract relevant informations
  vect_temp_1 <- mod$model %>% 
    pull(sym(het_var))
  
  vect_temp_2 <- mod$model %>% 
    filter(treatment_csh_trnsfr == "Cash Assignment") %>% 
    pull(sym(het_var))
  
  # Create a tibble with results
  if(het_var=="decile_baseline_ipv"){
    
    out <- tibble(
      name="cash",
      estimate_quant = as.character(1:10),
      typeTreat = c("Cash Assignment"),
      pe = coefs,
      N = nrow(mod$model),
      Ntype = c(as.numeric(table(vect_temp_1))[1], as.numeric(table(vect_temp_1))[2], 
                as.numeric(table(vect_temp_1))[3], as.numeric(table(vect_temp_1))[4], 
                as.numeric(table(vect_temp_1))[5], as.numeric(table(vect_temp_1))[6],
                as.numeric(table(vect_temp_1))[7], as.numeric(table(vect_temp_1))[8],
                as.numeric(table(vect_temp_1))[9], as.numeric(table(vect_temp_1))[10]),
      Ntype_treat = c(as.numeric(table(vect_temp_2))[1], as.numeric(table(vect_temp_2))[2], 
                      as.numeric(table(vect_temp_2))[3], as.numeric(table(vect_temp_2))[4], 
                      as.numeric(table(vect_temp_2))[5], as.numeric(table(vect_temp_2))[6],
                      as.numeric(table(vect_temp_2))[7], as.numeric(table(vect_temp_2))[8],
                      as.numeric(table(vect_temp_2))[9], as.numeric(table(vect_temp_2))[10]),
      #medianhetvar = medianhetvar,
      r.squared = summary(mod)$r.squared
    )
    
  }else if(het_var=="quartile_baseline_ipv"){
    
    out <- tibble(
      name="cash",
      estimate_quant = as.character(1:5),
      typeTreat = c("Cash Assignment"),
      pe = coefs,
      N = nrow(mod$model),
      Ntype = c(as.numeric(table(vect_temp_1))[1], as.numeric(table(vect_temp_1))[2], 
                as.numeric(table(vect_temp_1))[3], as.numeric(table(vect_temp_1))[4], 
                as.numeric(table(vect_temp_1))[5]),
      Ntype_treat = c(as.numeric(table(vect_temp_2))[1], as.numeric(table(vect_temp_2))[2], 
                      as.numeric(table(vect_temp_2))[3], as.numeric(table(vect_temp_2))[4], 
                      as.numeric(table(vect_temp_2))[5]),
      r.squared = summary(mod)$r.squared
    )
  }else{
    
    out <- tibble(
      name="cash",
      estimate_quant = as.character(1:2),
      typeTreat = c("Cash Assignment"),
      pe = coefs,
      N = nrow(mod$model),
      Ntype = c(as.numeric(table(vect_temp_1))[1], as.numeric(table(vect_temp_1))[2]),
      Ntype_treat = c(as.numeric(table(vect_temp_2))[1], as.numeric(table(vect_temp_2))[2]),
      r.squared = summary(mod)$r.squared
    )
  }
  

  out <- bind_cols(out, as.tibble(CI))
  
  # Return the tibble with results
  out
}

# Function to estimate the model with squared exposition variable
getEstimatedf_ML_het_pi_quan <- function(formula, depvar, het_var, curr_treat_var, strata_vars, dfcurr, R = 1000) {

  # Fit the model using fixed effects
  mod <- felm(formula, data = dfcurr, keepModel = T, na.action = na.omit)
  
  # Extract the variance-covariance matrix
  vcmod <- vcov(mod)
  vcmod[is.na(vcmod)] <- 0
  
  # Define alpha values for confidence intervals
  alpha <- c(0.025, 0.05, 0.95, 0.975)
  
  # Extract coefficients and handle NAs
  mat_coef <- coef(mod)
  mat_coef[is.na(mat_coef)] <- 0
  
  # Simulate coefficients using multivariate normal distribution
  modU <- MASS::mvrnorm(R, mu = mat_coef, Sigma = vcmod)

  if(curr_treat_var=="treatment_pi"){
    # Define matrix for coefficient selection
    matSelect <- matrix(0, nrow = 15, ncol = length(coef(mod)))
    colnames(matSelect) <- names(coef(mod))
    
    # Define crossed variables for interaction terms
    crosvar11 <- paste0(curr_treat_var,"Capital", sep = "")
    crosvar21 <- paste0(curr_treat_var,"Psychosocial", sep = "")
    crosvar31 <- paste0(curr_treat_var,"Full", sep = "")
    
    crosvar12 <- paste0(het_var,"2:",curr_treat_var,"Capital", sep = "")
    crosvar22 <- paste0(het_var,"2:",curr_treat_var,"Psychosocial", sep = "")
    crosvar32 <- paste0(het_var,"2:",curr_treat_var,"Full", sep = "")    
    
    crosvar13 <- paste0(het_var,"3:",curr_treat_var,"Capital", sep = "")
    crosvar23 <- paste0(het_var,"3:",curr_treat_var,"Psychosocial", sep = "")
    crosvar33 <- paste0(het_var,"3:",curr_treat_var,"Full", sep = "")
    
    crosvar14 <- paste0(het_var,"4:",curr_treat_var,"Capital", sep = "")
    crosvar24 <- paste0(het_var,"4:",curr_treat_var,"Psychosocial", sep = "")
    crosvar34 <- paste0(het_var,"4:",curr_treat_var,"Full", sep = "")    
    
    crosvar15 <- paste0(het_var,"5:",curr_treat_var,"Capital", sep = "")
    crosvar25 <- paste0(het_var,"5:",curr_treat_var,"Psychosocial", sep = "")
    crosvar35 <- paste0(het_var,"5:",curr_treat_var,"Full", sep = "")    
    
    # Populate the matrix for coefficient selection
    matSelect[, crosvar11] <- c(1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0)
    matSelect[, crosvar21] <- c(0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0)
    matSelect[, crosvar31] <- c(0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1)
    
    matSelect[, crosvar12] <- c(0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    matSelect[, crosvar22] <- c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    matSelect[, crosvar32] <- c(0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    
    matSelect[, crosvar13] <- c(0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0)
    matSelect[, crosvar23] <- c(0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0)
    matSelect[, crosvar33] <- c(0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0)
    
    matSelect[, crosvar14] <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0)
    matSelect[, crosvar24] <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0)
    matSelect[, crosvar34] <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0)
    
    matSelect[, crosvar15] <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0)
    matSelect[, crosvar25] <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0)
    matSelect[, crosvar35] <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1)
 
    
    # Calculate the coefficients
    coefs <- as.numeric(matSelect %*% mat_coef)
    modU_CI <- t(matSelect %*% t(modU))
    
    # Function to access the coefficients
    CI <- t(apply(modU_CI, 2, quantile, probs = alpha, na.rm = TRUE))
    colnames(CI) <- paste0("CI", parse_number(colnames(CI)))
    
    # Extract relevant informations
    vect_temp_1 <- mod$model %>% 
      pull(sym(het_var))
    
    vect_temp_2 <- mod$model %>% 
      filter(treatment_pi == "Capital") %>% 
      pull(sym(het_var))
    
    vect_temp_3 <- mod$model %>% 
      filter(treatment_pi == "Psychosocial") %>% 
      pull(sym(het_var))
    
    vect_temp_4 <- mod$model %>% 
      filter(treatment_pi == "Full") %>% 
      pull(sym(het_var))
    
    # Create a tibble with results
    out <- tibble(
      name="pi",
      estimate_quant = rep(as.character(1:5), each = 3),
      typeTreat = rep(c("Capital","Psychosocial","Full"), 5),
      pe = coefs,
      N = nrow(mod$model),
      Ntype = rep(c(as.numeric(table(vect_temp_1))[1], as.numeric(table(vect_temp_1))[2],
                    as.numeric(table(vect_temp_1))[3], as.numeric(table(vect_temp_1))[4],
                    as.numeric(table(vect_temp_1))[5]), each = 3),
      
      Ntype_treat = c(as.numeric(table(vect_temp_2))[1], as.numeric(table(vect_temp_3))[1], as.numeric(table(vect_temp_4))[1],
                      as.numeric(table(vect_temp_2))[2], as.numeric(table(vect_temp_3))[2], as.numeric(table(vect_temp_4))[2],
                      as.numeric(table(vect_temp_2))[3], as.numeric(table(vect_temp_3))[3], as.numeric(table(vect_temp_4))[3],
                      as.numeric(table(vect_temp_2))[4], as.numeric(table(vect_temp_3))[4], as.numeric(table(vect_temp_4))[4],
                      as.numeric(table(vect_temp_2))[5], as.numeric(table(vect_temp_3))[5], as.numeric(table(vect_temp_4))[5]),
      
      r.squared = summary(mod)$r.squared
    )
  }else{
    # Define matrix for coefficient selection
    matSelect <- matrix(0, nrow = 5, ncol = length(coef(mod)))
    colnames(matSelect) <- names(coef(mod))

    # Define crossed variables for interaction terms
    crosvar1 <- paste0(curr_treat_var,"Pool", sep = "")
    crosvar2 <- paste0(het_var,"2:",curr_treat_var,"Pool", sep = "")
    crosvar3 <- paste0(het_var,"3:",curr_treat_var,"Pool", sep = "")
    crosvar4 <- paste0(het_var,"4:",curr_treat_var,"Pool", sep = "")    
    crosvar5 <- paste0(het_var,"5:",curr_treat_var,"Pool", sep = "")
    
    # Populate the matrix for coefficient selection
    matSelect[, crosvar1] <- c(1, 1, 1, 1, 1)
    matSelect[, crosvar2] <- c(0, 1, 0, 0, 0)
    matSelect[, crosvar3] <- c(0, 0, 1, 0, 0)
    matSelect[, crosvar4] <- c(0, 0, 0, 1, 0)
    matSelect[, crosvar5] <- c(0, 0, 0, 0, 1)
    
    # Calculate the coefficients
    coefs <- as.numeric(matSelect %*% mat_coef)
    modU_CI <- t(matSelect %*% t(modU))
    
    # Function to access the coefficients
    CI <- t(apply(modU_CI, 2, quantile, probs = alpha, na.rm = TRUE))
    colnames(CI) <- paste0("CI", parse_number(colnames(CI)))
    
    # Extract relevant informations
    vect_temp_1 <- mod$model %>% 
      pull(sym(het_var))
    
    vect_temp_2 <- mod$model %>% 
      filter(treatment_pi_pool == "Pool") %>% 
      pull(sym(het_var))
    
    # Create a tibble with results
    out <- tibble(
      name="pi",
      estimate_quant = as.character(1:5),
      typeTreat = "Pool",
      pe = coefs,
      N = nrow(mod$model),
      Ntype = rep(c(as.numeric(table(vect_temp_1))[1], as.numeric(table(vect_temp_1))[2],
                    as.numeric(table(vect_temp_1))[3], as.numeric(table(vect_temp_1))[4],
                    as.numeric(table(vect_temp_1))[5])),
      Ntype_treat = c(as.numeric(table(vect_temp_2))[1], as.numeric(table(vect_temp_2))[2], 
                      as.numeric(table(vect_temp_2))[3], as.numeric(table(vect_temp_2))[4], 
                      as.numeric(table(vect_temp_2))[5]),
      r.squared = summary(mod)$r.squared
    )
  }
  
  
  out <- bind_cols(out, as.tibble(CI))
  
  # Return the tibble with results
  out
}



# Function to estimate the model with squared exposition variable
getEstimatedf_ML_het_pi_dec <- function(formula, depvar, het_var, curr_treat_var, strata_vars, dfcurr, R = 1000) {
  
  # Fit the model using fixed effects
  mod <- felm(formula, data = dfcurr, keepModel = T, na.action = na.omit)
  
  # Extract the variance-covariance matrix
  vcmod <- vcov(mod)
  vcmod[is.na(vcmod)] <- 0
  
  # Define alpha values for confidence intervals
  alpha <- c(0.025, 0.05, 0.95, 0.975)
  
  # Extract coefficients and handle NAs
  mat_coef <- coef(mod)
  mat_coef[is.na(mat_coef)] <- 0
  
  # Simulate coefficients using multivariate normal distribution
  modU <- MASS::mvrnorm(R, mu = mat_coef, Sigma = vcmod)

  if(curr_treat_var=="treatment_pi"){
    # Define matrix for coefficient selection
    matSelect <- matrix(0, nrow = 30, ncol = length(coef(mod)))
    colnames(matSelect) <- names(coef(mod))
    
    
    # Define crossed variables for interaction terms
    crosvar11 <- paste0(curr_treat_var,"Capital", sep = "")
    crosvar21 <- paste0(curr_treat_var,"Psychosocial", sep = "")
    crosvar31 <- paste0(curr_treat_var,"Full", sep = "")
    
    crosvar12 <- paste0(het_var,"2:",curr_treat_var,"Capital", sep = "")
    crosvar22 <- paste0(het_var,"2:",curr_treat_var,"Psychosocial", sep = "")
    crosvar32 <- paste0(het_var,"2:",curr_treat_var,"Full", sep = "")    
    
    crosvar13 <- paste0(het_var,"3:",curr_treat_var,"Capital", sep = "")
    crosvar23 <- paste0(het_var,"3:",curr_treat_var,"Psychosocial", sep = "")
    crosvar33 <- paste0(het_var,"3:",curr_treat_var,"Full", sep = "")
    
    crosvar14 <- paste0(het_var,"4:",curr_treat_var,"Capital", sep = "")
    crosvar24 <- paste0(het_var,"4:",curr_treat_var,"Psychosocial", sep = "")
    crosvar34 <- paste0(het_var,"4:",curr_treat_var,"Full", sep = "")    
    
    crosvar15 <- paste0(het_var,"5:",curr_treat_var,"Capital", sep = "")
    crosvar25 <- paste0(het_var,"5:",curr_treat_var,"Psychosocial", sep = "")
    crosvar35 <- paste0(het_var,"5:",curr_treat_var,"Full", sep = "")    
    
    crosvar16 <- paste0(het_var,"6:",curr_treat_var,"Capital", sep = "")
    crosvar26 <- paste0(het_var,"6:",curr_treat_var,"Psychosocial", sep = "")
    crosvar36 <- paste0(het_var,"6:",curr_treat_var,"Full", sep = "")
    
    crosvar17 <- paste0(het_var,"7:",curr_treat_var,"Capital", sep = "")
    crosvar27 <- paste0(het_var,"7:",curr_treat_var,"Psychosocial", sep = "")
    crosvar37 <- paste0(het_var,"7:",curr_treat_var,"Full", sep = "")
    
    crosvar18 <- paste0(het_var,"8:",curr_treat_var,"Capital", sep = "")
    crosvar28 <- paste0(het_var,"8:",curr_treat_var,"Psychosocial", sep = "")
    crosvar38 <- paste0(het_var,"8:",curr_treat_var,"Full", sep = "")    
    
    crosvar19 <- paste0(het_var,"9:",curr_treat_var,"Capital", sep = "")
    crosvar29 <- paste0(het_var,"9:",curr_treat_var,"Psychosocial", sep = "")
    crosvar39 <- paste0(het_var,"9:",curr_treat_var,"Full", sep = "")
    
    crosvar110 <- paste0(het_var,"10:",curr_treat_var,"Capital", sep = "")
    crosvar210 <- paste0(het_var,"10:",curr_treat_var,"Psychosocial", sep = "")
    crosvar310 <- paste0(het_var,"10:",curr_treat_var,"Full", sep = "")    
    
    
    # Populate the matrix for coefficient selection
    matSelect[, crosvar11] <- c(1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0)
    matSelect[, crosvar21] <- c(0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0)
    matSelect[, crosvar31] <- c(0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1)
    
    matSelect[, crosvar12] <- c(0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    matSelect[, crosvar22] <- c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    matSelect[, crosvar32] <- c(0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    
    matSelect[, crosvar13] <- c(0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    matSelect[, crosvar23] <- c(0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    matSelect[, crosvar33] <- c(0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    
    matSelect[, crosvar14] <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    matSelect[, crosvar24] <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    matSelect[, crosvar34] <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    
    matSelect[, crosvar15] <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    matSelect[, crosvar25] <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    matSelect[, crosvar35] <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    
    matSelect[, crosvar16] <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    matSelect[, crosvar26] <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    matSelect[, crosvar36] <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    
    matSelect[, crosvar17] <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    matSelect[, crosvar27] <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    matSelect[, crosvar37] <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    
    matSelect[, crosvar18] <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0)
    matSelect[, crosvar28] <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0)
    matSelect[, crosvar38] <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0)
    
    matSelect[, crosvar19] <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0)
    matSelect[, crosvar29] <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0)
    matSelect[, crosvar39] <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0)
    
    matSelect[, crosvar110] <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0)
    matSelect[, crosvar210] <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0)
    matSelect[, crosvar310] <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1)
    
    # Calculate the coefficients
    coefs <- as.numeric(matSelect %*% mat_coef)
    modU_CI <- t(matSelect %*% t(modU))
    
    # Function to access the coefficients
    CI <- t(apply(modU_CI, 2, quantile, probs = alpha, na.rm = TRUE))
    colnames(CI) <- paste0("CI", parse_number(colnames(CI)))
    
    # Extract relevant informations
    vect_temp_1 <- mod$model %>% 
      pull(sym(het_var))
    
    vect_temp_2 <- mod$model %>% 
      filter(treatment_pi == "Capital") %>% 
      pull(sym(het_var))
    
    vect_temp_3 <- mod$model %>% 
      filter(treatment_pi == "Psychosocial") %>% 
      pull(sym(het_var))
    
    vect_temp_4 <- mod$model %>% 
      filter(treatment_pi == "Full") %>% 
      pull(sym(het_var))
    
    # Create a tibble with results
    out <- tibble(
      name="pi",
      estimate_quant = rep(as.character(1:10), each = 3),
      typeTreat = rep(c("Capital","Psychosocial","Full"), 10),
      pe = coefs,
      N = nrow(mod$model),
      Ntype = rep(c(as.numeric(table(vect_temp_1))[1], as.numeric(table(vect_temp_1))[2],
                    as.numeric(table(vect_temp_1))[3], as.numeric(table(vect_temp_1))[4],
                    as.numeric(table(vect_temp_1))[5], as.numeric(table(vect_temp_1))[6],
                    as.numeric(table(vect_temp_1))[7], as.numeric(table(vect_temp_1))[8],
                    as.numeric(table(vect_temp_1))[9], as.numeric(table(vect_temp_1))[10]), each = 3),
      
      Ntype_treat = c(as.numeric(table(vect_temp_2))[1], as.numeric(table(vect_temp_3))[1], as.numeric(table(vect_temp_4))[1],
                      as.numeric(table(vect_temp_2))[2], as.numeric(table(vect_temp_3))[2], as.numeric(table(vect_temp_4))[2],
                      as.numeric(table(vect_temp_2))[3], as.numeric(table(vect_temp_3))[3], as.numeric(table(vect_temp_4))[3],
                      as.numeric(table(vect_temp_2))[4], as.numeric(table(vect_temp_3))[4], as.numeric(table(vect_temp_4))[4],
                      as.numeric(table(vect_temp_2))[5], as.numeric(table(vect_temp_3))[5], as.numeric(table(vect_temp_4))[5],
                      as.numeric(table(vect_temp_2))[6], as.numeric(table(vect_temp_3))[6], as.numeric(table(vect_temp_4))[6],
                      as.numeric(table(vect_temp_2))[7], as.numeric(table(vect_temp_3))[7], as.numeric(table(vect_temp_4))[7],
                      as.numeric(table(vect_temp_2))[8], as.numeric(table(vect_temp_3))[8], as.numeric(table(vect_temp_4))[8],
                      as.numeric(table(vect_temp_2))[9], as.numeric(table(vect_temp_3))[9], as.numeric(table(vect_temp_4))[9],
                      as.numeric(table(vect_temp_2))[10], as.numeric(table(vect_temp_3))[10], as.numeric(table(vect_temp_4))[10]),
      
      r.squared = summary(mod)$r.squared
    )
  }else{
    # Define matrix for coefficient selection
    matSelect <- matrix(0, nrow = 10, ncol = length(coef(mod)))
    colnames(matSelect) <- names(coef(mod))
    

    # Define crossed variables for interaction terms
    crosvar1 <- paste0(curr_treat_var,"Pool", sep = "")
    
    crosvar2 <- paste0(het_var,"2:",curr_treat_var,"Pool", sep = "")
    crosvar3 <- paste0(het_var,"3:",curr_treat_var,"Pool", sep = "")
    crosvar4 <- paste0(het_var,"4:",curr_treat_var,"Pool", sep = "")    
    
    crosvar5 <- paste0(het_var,"5:",curr_treat_var,"Pool", sep = "")
    crosvar6 <- paste0(het_var,"6:",curr_treat_var,"Pool", sep = "")
    crosvar7 <- paste0(het_var,"7:",curr_treat_var,"Pool", sep = "")
    
    crosvar8 <- paste0(het_var,"8:",curr_treat_var,"Pool", sep = "")
    crosvar9 <- paste0(het_var,"9:",curr_treat_var,"Pool", sep = "")
    crosvar10 <- paste0(het_var,"10:",curr_treat_var,"Pool", sep = "")    
    
    
    # Populate the matrix for coefficient selection
    matSelect[, crosvar1] <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
    matSelect[, crosvar2] <- c(0, 1, 0, 0, 0, 0, 0, 0, 0, 0)
    matSelect[, crosvar3] <- c(0, 0, 1, 0, 0, 0, 0, 0, 0, 0)
    matSelect[, crosvar4] <- c(0, 0, 0, 1, 0, 0, 0, 0, 0, 0)
    matSelect[, crosvar5] <- c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0)
    matSelect[, crosvar6] <- c(0, 0, 0, 0, 0, 1, 0, 0, 0, 0)
    matSelect[, crosvar7] <- c(0, 0, 0, 0, 0, 0, 1, 0, 0, 0)
    matSelect[, crosvar8] <- c(0, 0, 0, 0, 0, 0, 0, 1, 0, 0)
    matSelect[, crosvar9] <- c(0, 0, 0, 0, 0, 0, 0, 0, 1, 0)
    matSelect[, crosvar10] <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 1)
    
    
    
    # Calculate the coefficients
    coefs <- as.numeric(matSelect %*% mat_coef)
    modU_CI <- t(matSelect %*% t(modU))
    
    # Function to access the coefficients
    CI <- t(apply(modU_CI, 2, quantile, probs = alpha, na.rm = TRUE))
    colnames(CI) <- paste0("CI", parse_number(colnames(CI)))
    
    # Extract relevant informations
    vect_temp_1 <- mod$model %>% 
      pull(sym(het_var))
    
    vect_temp_2 <- mod$model %>% 
      filter(treatment_pi_pool == "Pool") %>% 
      pull(sym(het_var))
    
    # Create a tibble with results
    out <- tibble(
      name="pi",
      estimate_quant = as.character(1:10),
      typeTreat = "Pool",
      pe = coefs,
      N = nrow(mod$model),
      Ntype = rep(c(as.numeric(table(vect_temp_1))[1], as.numeric(table(vect_temp_1))[2],
                    as.numeric(table(vect_temp_1))[3], as.numeric(table(vect_temp_1))[4],
                    as.numeric(table(vect_temp_1))[5], as.numeric(table(vect_temp_1))[6],
                    as.numeric(table(vect_temp_1))[7], as.numeric(table(vect_temp_1))[8],
                    as.numeric(table(vect_temp_1))[9], as.numeric(table(vect_temp_1))[10])),
      Ntype_treat = c(as.numeric(table(vect_temp_2))[1], as.numeric(table(vect_temp_2))[2], as.numeric(table(vect_temp_2))[3],
                      as.numeric(table(vect_temp_2))[4], as.numeric(table(vect_temp_2))[5], as.numeric(table(vect_temp_2))[6],
                      as.numeric(table(vect_temp_2))[7], as.numeric(table(vect_temp_2))[8], as.numeric(table(vect_temp_2))[9],
                      as.numeric(table(vect_temp_2))[10]),
      r.squared = summary(mod)$r.squared
    )
  }
  
  
  out <- bind_cols(out, as.tibble(CI))
  
  # Return the tibble with results
  out
}
