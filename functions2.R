############################################################################
######### Function for computing Mechanisms tables #########################
############################################################################



getTable7_het <- function(empowerment, mainResults_mech_hh){
  
  mainResults_mech_ben_curr <- mainResults_mech_ben
  
  
  ## Cash transfer regression models
  ### Extract specific models for cash transfer from the main results
  mainResults_mech_ben_curr_csh <- mainResults_mech_ben_curr %>% filter(treat_var=="treatment_csh_trnsfr")
  
  m10 <- mainResults_mech_ben_curr_csh$results_base[[1]]  # First cash transfer model
  m11 <- mainResults_mech_ben_curr_csh$results_base[[2]]  # Second cash transfer model
  m12 <- mainResults_mech_ben_curr_csh$results_base[[3]]  #  cash transfer model
  m13 <- mainResults_mech_ben_curr_csh$results_base[[4]]  #  cash transfer model
  m14 <- mainResults_mech_ben_curr_csh$results_base[[5]]  #  cash transfer model
  m15 <- mainResults_mech_ben_curr_csh$results_base[[6]]  #  cash transfer model
  m16 <- mainResults_mech_ben_curr_csh$results_base[[7]]  # Second cash transfer model
  m17 <- mainResults_mech_ben_curr_csh$results_base[[8]]  #  cash transfer model
  m18 <- mainResults_mech_ben_curr_csh$results_base[[9]]  #  cash transfer model
  m19 <- mainResults_mech_ben_curr_csh$results_base[[10]]  #  cash transfer model
  m110 <- mainResults_mech_ben_curr_csh$results_base[[11]]  #  cash transfer model
  m111 <- mainResults_mech_ben_curr_csh$results_base[[12]]  #  cash transfer model
  m112 <- mainResults_mech_ben_curr_csh$results_base[[13]]  #  cash transfer model
  m113 <- mainResults_mech_ben_curr_csh$results_base[[14]]  #  cash transfer model
  m114 <- mainResults_mech_ben_curr_csh$results_base[[15]]  #  cash transfer model
  m115 <- mainResults_mech_ben_curr_csh$results_base[[16]]  #  cash transfer model
  m116 <- mainResults_mech_ben_curr_csh$results_base[[17]]  #  cash transfer model
  m117 <- mainResults_mech_ben_curr_csh$results_base[[18]]  #  cash transfer model
  
  ## Productive inclusion regression models
  ### Extract specific models for productive inclusion from the main results
  mainResults_mech_ben_curr_pi <- mainResults_mech_ben_curr %>% filter(treat_var=="treatment_pi")
  
  m20 <- mainResults_mech_ben_curr_pi$results_base[[1]]  # First productive inclusion model
  m21 <- mainResults_mech_ben_curr_pi$results_base[[2]]  # Second productive inclusion model
  m22 <- mainResults_mech_ben_curr_pi$results_base[[3]]  #  productive inclusion model
  m23 <- mainResults_mech_ben_curr_pi$results_base[[4]]  #  productive inclusion model
  m24 <- mainResults_mech_ben_curr_pi$results_base[[5]]  #  productive inclusion model
  m25 <- mainResults_mech_ben_curr_pi$results_base[[6]]  #  productive inclusion model
  m26 <- mainResults_mech_ben_curr_pi$results_base[[7]]  #  productive inclusion model
  m27 <- mainResults_mech_ben_curr_pi$results_base[[8]]  #  productive inclusion model
  m28 <- mainResults_mech_ben_curr_pi$results_base[[9]]  #  productive inclusion model
  m29 <- mainResults_mech_ben_curr_pi$results_base[[10]]  #  productive inclusion model
  m210 <- mainResults_mech_ben_curr_pi$results_base[[11]]  #  productive inclusion model
  m211 <- mainResults_mech_ben_curr_pi$results_base[[12]]  #  productive inclusion model
  m212 <- mainResults_mech_ben_curr_pi$results_base[[13]]  #  productive inclusion model
  m213 <- mainResults_mech_ben_curr_pi$results_base[[14]]  #  productive inclusion model
  m214 <- mainResults_mech_ben_curr_pi$results_base[[15]]  #  productive inclusion model
  m215 <- mainResults_mech_ben_curr_pi$results_base[[16]]  #  productive inclusion model
  m216 <- mainResults_mech_ben_curr_pi$results_base[[17]]  #  productive inclusion model
  m217 <- mainResults_mech_ben_curr_pi$results_base[[18]]  #  productive inclusion model
  
  ### Extract specific models for productive inclusion (pool) from the main results
  mainResults_mech_ben_curr_pool <- mainResults_mech_ben_curr %>% filter(treat_var=="treatment_pi_pool")
  
  m30 <- mainResults_mech_ben_curr_pool$results_base[[1]]  # First pool productive inclusion model
  m31 <- mainResults_mech_ben_curr_pool$results_base[[2]]  # Second pool productive inclusion model
  m32 <- mainResults_mech_ben_curr_pool$results_base[[3]]  #  pool productive inclusion model
  m33 <- mainResults_mech_ben_curr_pool$results_base[[4]]  #  pool productive inclusion model
  m34 <- mainResults_mech_ben_curr_pool$results_base[[5]]  #  pool productive inclusion model
  m35 <- mainResults_mech_ben_curr_pool$results_base[[6]]  #  pool productive inclusion model
  m36 <- mainResults_mech_ben_curr_pool$results_base[[7]]  #  pool productive inclusion model
  m37 <- mainResults_mech_ben_curr_pool$results_base[[8]]  #  pool productive inclusion model
  m38 <- mainResults_mech_ben_curr_pool$results_base[[9]]  #  pool productive inclusion model
  m39 <- mainResults_mech_ben_curr_pool$results_base[[10]]  #  pool productive inclusion model
  m310 <- mainResults_mech_ben_curr_pool$results_base[[11]]  #  pool productive inclusion model
  m311 <- mainResults_mech_ben_curr_pool$results_base[[12]]  #  pool productive inclusion model
  m312 <- mainResults_mech_ben_curr_pool$results_base[[13]]  #  pool productive inclusion model
  m313 <- mainResults_mech_ben_curr_pool$results_base[[14]]  #  pool productive inclusion model
  m314 <- mainResults_mech_ben_curr_pool$results_base[[15]]  #  pool productive inclusion model
  m315 <- mainResults_mech_ben_curr_pool$results_base[[16]]  #  pool productive inclusion model
  m316 <- mainResults_mech_ben_curr_pool$results_base[[17]]  #  pool productive inclusion model
  m317 <- mainResults_mech_ben_curr_pool$results_base[[18]]  #  pool productive inclusion model
  
  # Initialize empty vectors to store descriptive statistics
  # These will store mean and standard deviation for each outcome variable
  mean_values <- c()  # Vector to store mean values
  sd_values <- c()   # Vector to store standard deviation values
  
  # Calculate descriptive statistics for each outcome variable
  # Loop through the list of outcome variables
  for (depvar in empowerment) {
    # Calculate and round mean, ignoring NA values
    mean_values <- c(mean_values,  round(mean(followup_MRT_hh_control[[depvar]], na.rm = TRUE), 3))
    
    # Calculate and round standard deviation, ignoring NA values
    sd_values <- c(sd_values,  round(sd(followup_MRT_hh_control[[depvar]], na.rm = TRUE), 3))
  }
  
  # Create lists of models for easier processing
  models_pi <- list(m20, m21, m22, m23, m24, m25, m26, m27, m28, m29, m210, m211, m212, m213, m214, m215, m216, m217)  # Productive inclusion models
  models_pi_pool <- list(m30, m31, m32, m33, m34, m35, m36, m37, m38, m39, m310, m311, m312, m313, m314, m315, m316, m317)  # Productive inclusion models pool
  models_csh_trnsfr <- list(m10, m11, m12, m13, m14, m15, m16, m17, m18, m19, m110, m111, m112, m113, m114, m115, m116, m117)  # Cash transfer models
  
  
  
  
  # Create regression tables for Productive Inclusion models
  tbl_list_pi <- map(models_pi, ~ tbl_regression(.x, 
                                                 exponentiate = FALSE,  # Keep coefficients as-is 
                                                 estimate_fun = ~ style_number(.x, digits = 2),
                                                 include=c(                                 # Retain main and interaction terms
                                                   "treatment_pi"))  %>% 
                       add_significance_stars(pattern = "{estimate}{stars} \n ({std.error})",
                                              hide_se = TRUE) %>%
                       add_glance_table(include = c(nobs, r.squared)))
  
  tbl_list_pi_pool <- map(models_pi_pool, ~ tbl_regression(.x, 
                                                           exponentiate = FALSE,  # Keep coefficients as-is 
                                                           estimate_fun = ~ style_number(.x, digits = 2),
                                                           include=c(                                 # Keep main pooled treatment and its interaction
                                                             "treatment_pi_pool" 
                                                             # paste0(curr_het_var, ":treatment_pi_pool")
                                                                    ))  %>% 
                            add_significance_stars(pattern = "{estimate}{stars} \n ({std.error})",
                                                   hide_se = TRUE)%>%
                            add_glance_table(include = c(nobs, r.squared)))
  
  # Create regression tables for Cash Transfer models
  tbl_list_csh_trnsfr <- map(models_csh_trnsfr, ~ tbl_regression(.x, 
                                                                 exponentiate = FALSE,  # Keep coefficients as-is
                                                                 estimate_fun = ~ style_number(.x, digits = 2),
                                                                 include= c(                                 # Coefficients to retain
                                                                   "treatment_csh_trnsfr" 
                                                                   #  paste0(curr_het_var, ":treatment_csh_trnsfr")
                                                                 ))  %>% 
                               add_significance_stars(pattern = "{estimate}{stars} \n ({std.error})",
                                                      hide_se = TRUE)%>%
                               add_glance_table(include = c(nobs, r.squared)))
  
  # Modify headers for the Productive Inclusion and Cash Transfer models
  
  tbl_list_pi[[1]] <- tbl_list_pi[[1]] %>%
    modify_header(estimate = "Can Decide to\n Earn Alone (1-4)")
  
  tbl_list_csh_trnsfr[[1]] <- tbl_list_csh_trnsfr[[1]] %>%
    modify_header(estimate = "Can Decide to\n Earn Alone (1-4)")
  
  tbl_list_pi_pool[[1]] <- tbl_list_pi_pool[[1]] %>%
    modify_header(estimate = "Can Decide to\n Earn Alone (1-4)")
  
  ### 
  tbl_list_pi[[2]] <- tbl_list_pi[[2]] %>%
    modify_header(estimate = "Wage\n earnings\n (yearly, USD)")
  
  tbl_list_csh_trnsfr[[2]] <- tbl_list_csh_trnsfr[[2]] %>%
    modify_header(estimate = "Wage\n earnings\n (yearly, USD)")
  
  tbl_list_pi_pool[[2]] <- tbl_list_pi_pool[[2]] %>%
    modify_header(estimate = "Wage\n earnings\n (yearly, USD)")
  
  ### partner
  tbl_list_pi[[3]] <- tbl_list_pi[[3]] %>%
    modify_header(estimate = "Benef. controls\n crop\n revenue (0,1)")
  
  tbl_list_csh_trnsfr[[3]] <- tbl_list_csh_trnsfr[[3]] %>%
    modify_header(estimate = "Benef. controls\n crop\n revenue (0,1)")
  
  tbl_list_pi_pool[[3]] <- tbl_list_pi_pool[[3]] %>%
    modify_header(estimate = "Benef. controls\n crop\n revenue (0,1)")
  
  
  ## Business rev
  ### hh
  tbl_list_pi[[4]] <- tbl_list_pi[[4]] %>%
    modify_header(estimate = "No. of\n beneficiary\n businesses")
  
  tbl_list_csh_trnsfr[[4]] <- tbl_list_csh_trnsfr[[4]] %>%
    modify_header(estimate = "No. of\n beneficiary\n businesses")
  
  tbl_list_pi_pool[[4]] <- tbl_list_pi_pool[[4]] %>%
    modify_header(estimate = "No. of\n beneficiary\n businesses")
  
  ### ben
  tbl_list_pi[[5]] <- tbl_list_pi[[5]] %>%
    modify_header(estimate = "Beneficiary\n has a\n business (0,1)")
  
  tbl_list_csh_trnsfr[[5]] <- tbl_list_csh_trnsfr[[5]] %>%
    modify_header(estimate =  "Beneficiary\n has a\n business (0,1)")
  
  tbl_list_pi_pool[[5]] <- tbl_list_pi_pool[[5]] %>%
    modify_header(estimate =  "Beneficiary\n has a\n business (0,1)")
  
  ### partner
  tbl_list_pi[[6]] <- tbl_list_pi[[6]] %>%
    modify_header(estimate = "No. of months benef worked last year")
  
  tbl_list_csh_trnsfr[[6]] <- tbl_list_csh_trnsfr[[6]] %>%
    modify_header(estimate = "No. of months benef worked last year")
  
  tbl_list_pi_pool[[6]] <- tbl_list_pi_pool[[6]] %>%
    modify_header(estimate = "No. of months benef worked last year")
  
  ## Wage rev
  ### hh
  tbl_list_pi[[7]] <- tbl_list_pi[[7]] %>%
    modify_header(estimate = "Entrepreneurial\n business types\n (yearly)")
  
  tbl_list_csh_trnsfr[[7]] <- tbl_list_csh_trnsfr[[7]] %>%
    modify_header(estimate = "Entrepreneurial\n business types\n (yearly)")
  
  tbl_list_pi_pool[[7]] <- tbl_list_pi_pool[[7]] %>%
    modify_header(estimate = "Entrepreneurial\n business types\n (yearly)")
  
  ### ben
  tbl_list_pi[[8]] <- tbl_list_pi[[8]] %>%
    modify_header(estimate = "Beneficiary\n launched a\n business (0,1)")
  
  tbl_list_csh_trnsfr[[8]] <- tbl_list_csh_trnsfr[[8]] %>%
    modify_header(estimate = "Beneficiary\n launched a\n business (0,1)")
  
  tbl_list_pi_pool[[8]] <- tbl_list_pi_pool[[8]] %>%
    modify_header(estimate = "Beneficiary\n launched a\n business (0,1)")
  
  ### part
  tbl_list_pi[[9]] <- tbl_list_pi[[9]] %>%
    modify_header(estimate = "Beneficiary\n abandoned a\n business (0,1)")
  
  tbl_list_csh_trnsfr[[9]] <- tbl_list_csh_trnsfr[[9]] %>%
    modify_header(estimate = "Beneficiary\n abandoned a\n business (0,1)")
  
  tbl_list_pi_pool[[9]] <- tbl_list_pi_pool[[9]] %>%
    modify_header(estimate = "Beneficiary\n abandoned a\n business (0,1)")
  
  ## Livestock
  ### hh
  tbl_list_pi[[10]] <- tbl_list_pi[[10]] %>%
    modify_header(estimate = "Personnal\n savings")
  
  tbl_list_csh_trnsfr[[10]] <- tbl_list_csh_trnsfr[[10]] %>%
    modify_header(estimate = "Personnal\n savings")
  
  tbl_list_pi_pool[[10]] <- tbl_list_pi_pool[[10]] %>%
    modify_header(estimate = "Personnal\n savings")
  
  ### ben
  tbl_list_pi[[11]] <- tbl_list_pi[[11]] %>%
    modify_header(estimate = "Business\n revenues\n (yearly, USD)")
  
  tbl_list_csh_trnsfr[[11]] <- tbl_list_csh_trnsfr[[11]] %>%
    modify_header(estimate = "Business\n revenues\n (yearly, USD)")
  
  tbl_list_pi_pool[[11]] <- tbl_list_pi_pool[[11]] %>%
    modify_header(estimate = "Business\n revenues\n (yearly, USD)")
  
  ### prt
  tbl_list_pi[[12]] <- tbl_list_pi[[12]] %>%
    modify_header(estimate = "Business\n profits\n (yearly, USD)")
  
  tbl_list_csh_trnsfr[[12]] <- tbl_list_csh_trnsfr[[12]] %>%
    modify_header(estimate = "Business\n profits\n (yearly, USD)")
  
  tbl_list_pi_pool[[12]] <- tbl_list_pi_pool[[12]] %>%
    modify_header(estimate = "Business\n profits\n (yearly, USD)")
  
  tbl_list_pi[[13]] <- tbl_list_pi[[13]] %>%
    modify_header(estimate = "Business\n asset\n value (USD)")
  
  tbl_list_csh_trnsfr[[13]] <- tbl_list_csh_trnsfr[[13]] %>%
    modify_header(estimate = "Business\n asset\n value (USD)")
  
  tbl_list_pi_pool[[13]] <- tbl_list_pi_pool[[13]] %>%
    modify_header(estimate = "Business\n asset\n value (USD)")
  
  tbl_list_pi[[14]] <- tbl_list_pi[[14]] %>%
    modify_header(estimate = "Beneficiary\n investments\n (yearly, USD)")
  
  tbl_list_csh_trnsfr[[14]] <- tbl_list_csh_trnsfr[[14]] %>%
    modify_header(estimate = "Beneficiary\n investments\n (yearly, USD)")
  
  tbl_list_pi_pool[[14]] <- tbl_list_pi_pool[[14]] %>%
    modify_header(estimate = "Beneficiary\n investments\n (yearly, USD)")
  
  tbl_list_pi[[15]] <- tbl_list_pi[[15]] %>%
    modify_header(estimate = "Business revenue\n (beneficiary,\n monthly, USD)")
  
  tbl_list_csh_trnsfr[[15]] <- tbl_list_csh_trnsfr[[15]] %>%
    modify_header(estimate = "Business revenue\n (beneficiary,\n monthly, USD)")
  
  tbl_list_pi_pool[[15]] <- tbl_list_pi_pool[[15]] %>%
    modify_header(estimate = "Business revenue\n (beneficiary,\n monthly, USD)")
  
  tbl_list_pi[[16]] <- tbl_list_pi[[16]] %>%
    modify_header(estimate = "Benef. owns\n livestock\n (0,1)")
  
  tbl_list_csh_trnsfr[[16]] <- tbl_list_csh_trnsfr[[16]] %>%
    modify_header(estimate = "Benef. owns\n livestock\n (0,1)")
  
  tbl_list_pi_pool[[16]] <- tbl_list_pi_pool[[16]] %>%
    modify_header(estimate = "Benef. owns\n livestock\n (0,1)")
  
  tbl_list_pi[[17]] <- tbl_list_pi[[17]] %>%
    modify_header(estimate = "Benef. controls\n livestock\n revenue (0,1)")
  
  tbl_list_csh_trnsfr[[17]] <- tbl_list_csh_trnsfr[[17]] %>%
    modify_header(estimate = "Benef. controls\n livestock\n revenue (0,1)")
  
  tbl_list_pi_pool[[17]] <- tbl_list_pi_pool[[17]] %>%
    modify_header(estimate = "Benef. controls\n livestock\n revenue (0,1)")
  
  tbl_list_pi[[18]] <- tbl_list_pi[[18]] %>%
    modify_header(estimate = "Benef. traveled\n for work\n (0,1)")
  
  tbl_list_csh_trnsfr[[18]] <- tbl_list_csh_trnsfr[[18]] %>%
    modify_header(estimate = "Benef. traveled\n for work\n (0,1)")
  
  tbl_list_pi_pool[[18]] <- tbl_list_pi_pool[[18]] %>%
    modify_header(estimate = "Benef. traveled\n for work\n (0,1)")
  
  
  # Merge the stacked tables into a single table with column headers
  ## Cash transfert
  tbl_csh_trnsfr <- tbl_merge(
    tbls = list(tbl_list_csh_trnsfr[[1]],# First model
                tbl_list_csh_trnsfr[[2]], # Second model
                tbl_list_csh_trnsfr[[3]], 
                tbl_list_csh_trnsfr[[4]],
                tbl_list_csh_trnsfr[[5]], 
                tbl_list_csh_trnsfr[[6]], 
                tbl_list_csh_trnsfr[[7]], 
                tbl_list_csh_trnsfr[[8]],
                tbl_list_csh_trnsfr[[9]],
                tbl_list_csh_trnsfr[[10]],
                tbl_list_csh_trnsfr[[11]],
                tbl_list_csh_trnsfr[[12]],
                tbl_list_csh_trnsfr[[13]],
                tbl_list_csh_trnsfr[[14]],
                tbl_list_csh_trnsfr[[15]],
                tbl_list_csh_trnsfr[[16]],
                tbl_list_csh_trnsfr[[17]],
                tbl_list_csh_trnsfr[[18]]
    ), 
    tab_spanner = c("(1)","(2)","(3)","(4)","(5)","(6)", "(7)","(8)","(9)","(10)","(11)","(12)","(13)","(14)","(15)","(16)","(17)","(18)")
  ) 
  
  ## Productive inclusion
  tbl_pi <- tbl_merge(
    tbls = list(tbl_list_pi[[1]],# First model
                tbl_list_pi[[2]], # Second model
                tbl_list_pi[[3]], 
                tbl_list_pi[[4]],
                tbl_list_pi[[5]], 
                tbl_list_pi[[6]], 
                tbl_list_pi[[7]], 
                tbl_list_pi[[8]],
                tbl_list_pi[[9]],
                tbl_list_pi[[10]],
                tbl_list_pi[[11]],
                tbl_list_pi[[12]], 
                tbl_list_pi[[13]],
                tbl_list_pi[[14]],
                tbl_list_pi[[15]],
                tbl_list_pi[[16]],
                tbl_list_pi[[17]],
                tbl_list_pi[[18]]
    ), 
    tab_spanner = c("(1)","(2)","(3)","(4)","(5)","(6)", "(7)","(8)","(9)","(10)","(11)","(12)","(13)","(14)","(15)","(16)","(17)","(18)")
  )
  
  ## Pool Productive inclusion
  tbl_pi_pool <- tbl_merge(
    tbls = list(tbl_list_pi_pool[[1]],# First model
                tbl_list_pi_pool[[2]], # Second model
                tbl_list_pi_pool[[3]], 
                tbl_list_pi_pool[[4]],
                tbl_list_pi_pool[[5]], 
                tbl_list_pi_pool[[6]], 
                tbl_list_pi_pool[[7]], 
                tbl_list_pi_pool[[8]],
                tbl_list_pi_pool[[9]],
                tbl_list_pi_pool[[10]],
                tbl_list_pi_pool[[11]],
                tbl_list_pi_pool[[12]], 
                tbl_list_pi_pool[[13]],
                tbl_list_pi_pool[[14]],
                tbl_list_pi_pool[[15]],
                tbl_list_pi_pool[[16]],
                tbl_list_pi_pool[[17]],
                tbl_list_pi_pool[[18]]
    ),  
    tab_spanner = c("(1)","(2)","(3)","(4)","(5)","(6)", "(7)","(8)","(9)","(10)","(11)","(12)","(13)","(14)","(15)","(16)","(17)","(18)")
  )
  
  # Create a list of stacked tables combining Cash Transfer and Productive Inclusion models
  # Each list element combines the corresponding models from both treatment types
  tbl <- tbl_stack(list(tbl_csh_trnsfr,  tbl_pi, tbl_pi_pool)) #tbl_pi,
  
  # Modify table body to customize variable labels
  tbl <- tbl %>%
    modify_table_body(~ .x %>%
                        mutate(label = case_when(
                          # Rename specific treatment variables
                          term_1 == "treatment_piCapital" ~ "Capital",
                          term_1 == "treatment_piPsychosocial" ~ "Psychosocial",
                          term_1 == "treatment_piFull" ~ "Full",
                          term_1 == "treatment_pi_poolPool" ~ "Pool",
                          
                          # Label cash transfer and productive inclusion treatments
                          variable == "treatment_csh_trnsfr" & label=="Tekavoul program IE treatment group" ~ "Tekavoul",
                          
                          variable == "treatment_pi" & label=="PI IE treatment group" ~ "PI",
                          variable == "treatment_pi_pool" & label=="treatment_pi_pool" ~ "PI (Pool)",
                          TRUE ~ label  # Keep original labels for other variables
                        )))
  
  
  additional_rows <- tribble(
    ~tbl_id1, ~variable, ~var_label, ~label,
    ~estimate_1, ~estimate_2, ~estimate_3, ~estimate_4, ~estimate_5,
    ~estimate_6, ~estimate_7, ~estimate_8, ~estimate_9, ~estimate_10,
    ~estimate_11, ~estimate_12,~estimate_13, ~estimate_14,~estimate_15,
    ~estimate_16, ~estimate_17,~estimate_18, 
    ~stars_1, ~stars_2, ~stars_3, ~stars_4, ~stars_5,
    ~stars_6, ~stars_7, ~stars_8, ~stars_9, ~stars_10,~stars_11, ~stars_12,
    ~stars_13,~stars_14, ~stars_15,~stars_16,~stars_17,~stars_18,
    
    # Row 1: Control mean
    4, "Control mean @ follow up", "Control mean @ follow up", "Control mean @ follow up",
    mean_values[1], mean_values[2], mean_values[3], mean_values[4], mean_values[5],
    mean_values[6], mean_values[7], mean_values[8], mean_values[9], mean_values[10], 
    mean_values[11], mean_values[12],mean_values[13], mean_values[14],mean_values[15],
    mean_values[16], mean_values[17], mean_values[18], 
    "", "", "", "", "", "", "", "", "", "",
    "", "", "", "", "","", "","",
    
    # Row 2: Control SD
    4, "Control SD @ follow up", "Control SD @ follow up", "Control SD @ follow up",
    sd_values[1], sd_values[2], sd_values[3], sd_values[4], sd_values[5],
    sd_values[6], sd_values[7], sd_values[8], sd_values[9], sd_values[10],
    sd_values[11], sd_values[12],sd_values[13], sd_values[14],
    sd_values[15], sd_values[16],sd_values[17], sd_values[18],
    "", "", "", "", "", "", "", "", "", "",
    "", "", "", "", "","", "","" 
  )
  # Append the additional rows to the table body
  tbl$table_body <- bind_rows(tbl$table_body, additional_rows)
  
  # Title for each index
  tbl_ipv_title = paste0("**Table : effect of the programs by treatment**")
  
  # Final table formatting
  tbl <- tbl %>%
    # Set table caption
    modify_caption(tbl_ipv_title) %>%
    # Modify header for the label column
    modify_header(label = "**Outcome**") %>%
    # Remove all existing footnotes
    modify_footnote(everything() ~ NA) %>%
    modify_footnote(all_stat_cols() ~ NA) %>%
    # Add custom footnote with methodological details
    modify_footnote(label = "Notes: Results presented are OLS estimates that include controls for randomization strata (commune) and, where possible, baseline outcomes.  We control for social promotion intervention. Enumerator fixed effects are included in all regression. We estimate the regressions for the productive beneficiaries aged 18-49 only. Robust standard errors are shown in parentheses, clustered at the village proxy level. *** p < 0.01, ** p < 0.05, * p < 0.1.")
  
  # Print the final table
  tbl
  
}



getTable3_het_socialnorm <- function(socialnorm, reg_results){
  
  mainResults_mech_ben_curr <- reg_results
  
  
  ## Cash transfer regression models
  ### Extract specific models for cash transfer from the main results
  mainResults_mech_ben_curr_csh <- mainResults_mech_ben_curr %>% filter(treat_var=="treatment_csh_trnsfr")
  
  m10 <- mainResults_mech_ben_curr_csh$results_base[[1]]  # First cash transfer model
  m11 <- mainResults_mech_ben_curr_csh$results_base[[2]]  # Second cash transfer model
  m12 <- mainResults_mech_ben_curr_csh$results_base[[3]]  #  cash transfer model
  m13 <- mainResults_mech_ben_curr_csh$results_base[[4]]  #  cash transfer model
  m14 <- mainResults_mech_ben_curr_csh$results_base[[5]]  #  cash transfer model
  m15 <- mainResults_mech_ben_curr_csh$results_base[[6]]  #  cash transfer model
  m16 <- mainResults_mech_ben_curr_csh$results_base[[7]]  # Second cash transfer model
  m17 <- mainResults_mech_ben_curr_csh$results_base[[8]]  #  cash transfer model
  m18 <- mainResults_mech_ben_curr_csh$results_base[[9]]  #  cash transfer model
  m19 <- mainResults_mech_ben_curr_csh$results_base[[10]]  #  cash transfer model
  m110 <- mainResults_mech_ben_curr_csh$results_base[[11]]  #  cash transfer model
  
  
  ## Productive inclusion regression models
  ### Extract specific models for productive inclusion from the main results
  mainResults_mech_ben_curr_pi <- mainResults_mech_ben_curr %>% filter(treat_var=="treatment_pi")
  
  m20 <- mainResults_mech_ben_curr_pi$results_base[[1]]  # First productive inclusion model
  m21 <- mainResults_mech_ben_curr_pi$results_base[[2]]  # Second productive inclusion model
  m22 <- mainResults_mech_ben_curr_pi$results_base[[3]]  #  productive inclusion model
  m23 <- mainResults_mech_ben_curr_pi$results_base[[4]]  #  productive inclusion model
  m24 <- mainResults_mech_ben_curr_pi$results_base[[5]]  #  productive inclusion model
  m25 <- mainResults_mech_ben_curr_pi$results_base[[6]]  #  productive inclusion model
  m26 <- mainResults_mech_ben_curr_pi$results_base[[7]]  #  productive inclusion model
  m27 <- mainResults_mech_ben_curr_pi$results_base[[8]]  #  productive inclusion model
  m28 <- mainResults_mech_ben_curr_pi$results_base[[9]]  #  productive inclusion model
  m29 <- mainResults_mech_ben_curr_pi$results_base[[10]]  #  productive inclusion model
  m210 <- mainResults_mech_ben_curr_pi$results_base[[11]]  #  productive inclusion model
  
  
  ### Extract specific models for productive inclusion (pool) from the main results
  mainResults_mech_ben_curr_pool <- mainResults_mech_ben_curr %>% filter(treat_var=="treatment_pi_pool")
  
  m30 <- mainResults_mech_ben_curr_pool$results_base[[1]]  # First pool productive inclusion model
  m31 <- mainResults_mech_ben_curr_pool$results_base[[2]]  # Second pool productive inclusion model
  m32 <- mainResults_mech_ben_curr_pool$results_base[[3]]  #  pool productive inclusion model
  m33 <- mainResults_mech_ben_curr_pool$results_base[[4]]  #  pool productive inclusion model
  m34 <- mainResults_mech_ben_curr_pool$results_base[[5]]  #  pool productive inclusion model
  m35 <- mainResults_mech_ben_curr_pool$results_base[[6]]  #  pool productive inclusion model
  m36 <- mainResults_mech_ben_curr_pool$results_base[[7]]  #  pool productive inclusion model
  m37 <- mainResults_mech_ben_curr_pool$results_base[[8]]  #  pool productive inclusion model
  m38 <- mainResults_mech_ben_curr_pool$results_base[[9]]  #  pool productive inclusion model
  m39 <- mainResults_mech_ben_curr_pool$results_base[[10]]  #  pool productive inclusion model
  m310 <- mainResults_mech_ben_curr_pool$results_base[[11]]  #  pool productive inclusion model
  
  # Initialize empty vectors to store descriptive statistics
  # These will store mean and standard deviation for each outcome variable
  mean_values <- c()  # Vector to store mean values
  sd_values <- c()   # Vector to store standard deviation values
  
  # Calculate descriptive statistics for each outcome variable
  # Loop through the list of outcome variables
  for (depvar in socialnorm) {
    # Calculate and round mean, ignoring NA values
    mean_values <- c(mean_values,  round(mean(followup_MRT_hh_control[[depvar]], na.rm = TRUE), 3))
    
    # Calculate and round standard deviation, ignoring NA values
    sd_values <- c(sd_values,  round(sd(followup_MRT_hh_control[[depvar]], na.rm = TRUE), 3))
  }
  
  # Create lists of models for easier processing
  models_pi <- list(m20, m21, m22, m23, m24, m25, m26, m27, m28, m29, m210)  # Productive inclusion models
  models_pi_pool <- list(m30, m31, m32, m33, m34, m35, m36, m37, m38, m39, m310)  # Productive inclusion models pool
  models_csh_trnsfr <- list(m10, m11, m12, m13, m14, m15, m16, m17, m18, m19, m110)  # Cash transfer models
  
  
  
  
  # Create regression tables for Productive Inclusion models
  tbl_list_pi <- map(models_pi, ~ tbl_regression(.x, 
                                                 exponentiate = FALSE,  # Keep coefficients as-is 
                                                 estimate_fun = ~ style_number(.x, digits = 2),
                                                 include=c(                                 # Retain main and interaction terms
                                                   "treatment_pi"))  %>% 
                       add_significance_stars(pattern = "{estimate}{stars} \n ({std.error})",
                                              hide_se = TRUE) %>%
                       add_glance_table(include = c(nobs, r.squared)))
  
  tbl_list_pi_pool <- map(models_pi_pool, ~ tbl_regression(.x, 
                                                           exponentiate = FALSE,  # Keep coefficients as-is 
                                                           estimate_fun = ~ style_number(.x, digits = 2),
                                                           include=c(                                 # Keep main pooled treatment and its interaction
                                                             "treatment_pi_pool"))  %>% 
                            add_significance_stars(pattern = "{estimate}{stars} \n ({std.error})",
                                                   hide_se = TRUE)%>%
                            add_glance_table(include = c(nobs, r.squared)))
  
  # Create regression tables for Cash Transfer models
  tbl_list_csh_trnsfr <- map(models_csh_trnsfr, ~ tbl_regression(.x, 
                                                                 exponentiate = FALSE,  # Keep coefficients as-is
                                                                 estimate_fun = ~ style_number(.x, digits = 2),
                                                                 include= c(                                 # Coefficients to retain
                                                                   "treatment_csh_trnsfr"
                                                                 ))  %>% 
                               add_significance_stars(pattern = "{estimate}{stars} \n ({std.error})",
                                                      hide_se = TRUE)%>%
                               add_glance_table(include = c(nobs, r.squared)))
  
  # Modify headers for the Productive Inclusion and Cash Transfer models
  
  tbl_list_pi[[1]] <- tbl_list_pi[[1]] %>%
    modify_header(estimate = "Social\n norms\n index")
  
  tbl_list_csh_trnsfr[[1]] <- tbl_list_csh_trnsfr[[1]] %>%
    modify_header(estimate = "Social\n norms\n index")
  
  tbl_list_pi_pool[[1]] <- tbl_list_pi_pool[[1]] %>%
    modify_header(estimate = "Social\n norms\n index")
  
  ### 
  tbl_list_pi[[2]] <- tbl_list_pi[[2]] %>%
    modify_header(estimate = "Descriptive\n norms\n index")
  
  tbl_list_csh_trnsfr[[2]] <- tbl_list_csh_trnsfr[[2]] %>%
    modify_header(estimate = "Descriptive\n norms\n index")
  
  tbl_list_pi_pool[[2]] <- tbl_list_pi_pool[[2]] %>%
    modify_header(estimate = "Descriptive\n norms\n index")
  
  
  tbl_list_pi[[3]] <- tbl_list_pi[[3]] %>%
    modify_header(estimate = "Know women\n travel freely\n (0-10)")
  
  tbl_list_csh_trnsfr[[3]] <- tbl_list_csh_trnsfr[[3]] %>%
    modify_header(estimate = "Know women\n travel freely\n (0-10)")
  
  tbl_list_pi_pool[[3]] <- tbl_list_pi_pool[[3]] %>%
    modify_header(estimate = "Know women\n travel freely\n (0-10)")
  
  ### hh
  tbl_list_pi[[4]] <- tbl_list_pi[[4]] %>%
    modify_header(estimate = "Know\n women\n with loans (0-10)")
  
  tbl_list_csh_trnsfr[[4]] <- tbl_list_csh_trnsfr[[4]] %>%
    modify_header(estimate = "Know\n women\n with loans (0-10)")
  
  tbl_list_pi_pool[[4]] <- tbl_list_pi_pool[[4]] %>%
    modify_header(estimate = "Know\n women\n with loans (0-10)")
  
  ### 
  tbl_list_pi[[5]] <- tbl_list_pi[[5]] %>%
    modify_header(estimate = "Know women\n who started\n activities (0-10)")
  
  tbl_list_csh_trnsfr[[5]] <- tbl_list_csh_trnsfr[[5]] %>%
    modify_header(estimate = "Know women\n who started\n activities (0-10)")
  
  tbl_list_pi_pool[[5]] <- tbl_list_pi_pool[[5]] %>%
    modify_header(estimate = "Know women\n who started\n activities (0-10)")
  
  ### 
  tbl_list_pi[[6]] <- tbl_list_pi[[6]] %>%
    modify_header(estimate = "Know women\n travel freely \n (0-10)")
  
  tbl_list_csh_trnsfr[[6]] <- tbl_list_csh_trnsfr[[6]] %>%
    modify_header(estimate = "Know women\n travel freely \n (0-10)")
  
  tbl_list_pi_pool[[6]] <- tbl_list_pi_pool[[6]] %>%
    modify_header(estimate = "Know women\n travel freely \n (0-10)")
  
  ### hh
  tbl_list_pi[[7]] <- tbl_list_pi[[7]] %>%
    modify_header(estimate = "Prescriptive\n norms\n index")
  
  tbl_list_csh_trnsfr[[7]] <- tbl_list_csh_trnsfr[[7]] %>%
    modify_header(estimate = "Prescriptive\n norms\n index")
  
  tbl_list_pi_pool[[7]] <- tbl_list_pi_pool[[7]] %>%
    modify_header(estimate = "Prescriptive\n norms\n index")
  
  ### ben
  tbl_list_pi[[8]] <- tbl_list_pi[[8]] %>%
    modify_header(estimate = "No. men who think\n women shd travel\n freely (0-10)")
  
  tbl_list_csh_trnsfr[[8]] <- tbl_list_csh_trnsfr[[8]] %>%
    modify_header(estimate = "No. men who think\n women shd travel\n freely (0-10)")
  
  tbl_list_pi_pool[[8]] <- tbl_list_pi_pool[[8]] %>%
    modify_header(estimate = "No. men who think\n women shd travel\n freely (0-10)")
  
  ### part
  tbl_list_pi[[9]] <- tbl_list_pi[[9]] %>%
    modify_header(estimate = "No. men who think\n women shd have\n own work (0-10)")
  
  tbl_list_csh_trnsfr[[9]] <- tbl_list_csh_trnsfr[[9]] %>%
    modify_header(estimate = "No. men who think\n women shd have\n own work (0-10)")
  
  tbl_list_pi_pool[[9]] <- tbl_list_pi_pool[[9]] %>%
    modify_header(estimate = "No. men who think\n women shd have\n own work (0-10)")
  
  ## Livestock
  ### hh
  tbl_list_pi[[10]] <- tbl_list_pi[[10]] %>%
    modify_header(estimate = "No. women who think\n women shd travel\n freely (0-10)")
  
  tbl_list_csh_trnsfr[[10]] <- tbl_list_csh_trnsfr[[10]] %>%
    modify_header(estimate = "No. women who think\n women shd travel\n freely (0-10)")
  
  tbl_list_pi_pool[[10]] <- tbl_list_pi_pool[[10]] %>%
    modify_header(estimate = "No. women who think\n women shd travel\n freely (0-10)")
  
  ### ben
  tbl_list_pi[[11]] <- tbl_list_pi[[11]] %>%
    modify_header(estimate = "No. women who think\n women shd have\n own work (0-10)")
  
  tbl_list_csh_trnsfr[[11]] <- tbl_list_csh_trnsfr[[11]] %>%
    modify_header(estimate = "No. women who think\n women shd have\n own work (0-10)")
  
  tbl_list_pi_pool[[11]] <- tbl_list_pi_pool[[11]] %>%
    modify_header(estimate = "No. women who think\n women shd have\n own work (0-10)")
  
  # Merge the stacked tables into a single table with column headers
  ## Cash transfert
  tbl_csh_trnsfr <- tbl_merge(
    tbls = list(tbl_list_csh_trnsfr[[1]],# First model
                tbl_list_csh_trnsfr[[2]], # Second model
                tbl_list_csh_trnsfr[[3]], 
                tbl_list_csh_trnsfr[[4]],
                tbl_list_csh_trnsfr[[5]], 
                tbl_list_csh_trnsfr[[6]], 
                tbl_list_csh_trnsfr[[7]], 
                tbl_list_csh_trnsfr[[8]],
                tbl_list_csh_trnsfr[[9]],
                tbl_list_csh_trnsfr[[10]],
                tbl_list_csh_trnsfr[[11]]
    ), 
    tab_spanner = c("(1)","(2)","(3)","(4)","(5)","(6)", "(7)","(8)","(9)","(10)","(11)")
  ) 
  
  ## Productive inclusion
  tbl_pi <- tbl_merge(
    tbls = list(tbl_list_pi[[1]],# First model
                tbl_list_pi[[2]], # Second model
                tbl_list_pi[[3]], 
                tbl_list_pi[[4]],
                tbl_list_pi[[5]], 
                tbl_list_pi[[6]], 
                tbl_list_pi[[7]], 
                tbl_list_pi[[8]],
                tbl_list_pi[[9]],
                tbl_list_pi[[10]],
                tbl_list_pi[[11]]
    ), 
    tab_spanner = c("(1)","(2)","(3)","(4)","(5)","(6)", "(7)","(8)","(9)","(10)","(11)")
  )
  
  ## Pool Productive inclusion
  tbl_pi_pool <- tbl_merge(
    tbls = list(tbl_list_pi_pool[[1]],# First model
                tbl_list_pi_pool[[2]], # Second model
                tbl_list_pi_pool[[3]], 
                tbl_list_pi_pool[[4]],
                tbl_list_pi_pool[[5]], 
                tbl_list_pi_pool[[6]], 
                tbl_list_pi_pool[[7]], 
                tbl_list_pi_pool[[8]],
                tbl_list_pi_pool[[9]],
                tbl_list_pi_pool[[10]],
                tbl_list_pi_pool[[11]]
    ),  
    tab_spanner = c("(1)","(2)","(3)","(4)","(5)","(6)", "(7)","(8)","(9)","(10)","(11)")
  )
  
  # Create a list of stacked tables combining Cash Transfer and Productive Inclusion models
  # Each list element combines the corresponding models from both treatment types
  tbl <- tbl_stack(list(tbl_csh_trnsfr,  tbl_pi, tbl_pi_pool)) #tbl_pi,
  
  # Modify table body to customize variable labels
  tbl <- tbl %>%
    modify_table_body(~ .x %>%
                        mutate(label = case_when(
                          # Rename specific treatment variables
                          term_1 == "treatment_piCapital" ~ "Capital",
                          term_1 == "treatment_piPsychosocial" ~ "Psychosocial",
                          term_1 == "treatment_piFull" ~ "Full",
                          term_1 == "treatment_pi_poolPool" ~ "Pool",
                          # Label cash transfer and productive inclusion treatments
                          variable == "treatment_csh_trnsfr" & label=="Tekavoul program IE treatment group" ~ "Tekavoul",
                          
                          variable == "treatment_pi" & label=="PI IE treatment group" ~ "PI",
                          variable == "treatment_pi_pool" & label=="treatment_pi_pool" ~ "PI (Pool)",
                          TRUE ~ label  # Keep original labels for other variables
                        )))
  
  
  additional_rows <- tribble(
    ~tbl_id1, ~variable, ~var_label, ~label,
    ~estimate_1, ~estimate_2, ~estimate_3, ~estimate_4, ~estimate_5,
    ~estimate_6, ~estimate_7, ~estimate_8, ~estimate_9, ~estimate_10,
    ~estimate_11, 
    ~stars_1, ~stars_2, ~stars_3, ~stars_4, ~stars_5,
    ~stars_6, ~stars_7, ~stars_8, ~stars_9, ~stars_10,~stars_11, 
    
    # Row 1: Control mean
    4, "Control mean @ follow up", "Control mean @ follow up", "Control mean @ follow up",
    mean_values[1], mean_values[2], mean_values[3], mean_values[4], mean_values[5],
    mean_values[6], mean_values[7], mean_values[8], mean_values[9], mean_values[10], 
    mean_values[11], 
    "", "", "", "", "", "", "", "", "", "",
    "", 
    
    # Row 2: Control SD
    4, "Control SD @ follow up", "Control SD @ follow up", "Control SD @ follow up",
    sd_values[1], sd_values[2], sd_values[3], sd_values[4], sd_values[5],
    sd_values[6], sd_values[7], sd_values[8], sd_values[9], sd_values[10],
    sd_values[11], 
    "", "", "", "", "", "", "", "", "", "",
    "" 
  )
  # Append the additional rows to the table body
  tbl$table_body <- bind_rows(tbl$table_body, additional_rows)
  
  # Title for each index
  tbl_ipv_title = paste0("**Table : effect of the programs by treatment**")
  
  # Final table formatting
  tbl <- tbl %>%
    # Set table caption
    modify_caption(tbl_ipv_title) %>%
    # Modify header for the label column
    modify_header(label = "**Outcome**") %>%
    # Remove all existing footnotes
    modify_footnote(everything() ~ NA) %>%
    modify_footnote(all_stat_cols() ~ NA) %>%
    # Add custom footnote with methodological details
    modify_footnote(label = "Notes: Results presented are OLS estimates that include controls for randomization strata (commune) and, where possible, baseline outcomes.  We control for social promotion intervention. Enumerator fixed effects are included in all regression. We estimate the regressions for the productive beneficiaries aged 18-49 only. Robust standard errors are shown in parentheses, clustered at the village proxy level. *** p < 0.01, ** p < 0.05, * p < 0.1.")
  
  # Print the final table
  tbl
  
}



getTable3_het_gender <- function(gender_var, reg_results){
  
  mainResults_mech_ben_curr <- reg_results
  
  
  ## Cash transfer regression models
  ### Extract specific models for cash transfer from the main results
  mainResults_mech_ben_curr_csh <- mainResults_mech_ben_curr %>% filter(treat_var=="treatment_csh_trnsfr")
  
  m10 <- mainResults_mech_ben_curr_csh$results_base[[1]]  # First cash transfer model
  m11 <- mainResults_mech_ben_curr_csh$results_base[[2]]  # Second cash transfer model
  m12 <- mainResults_mech_ben_curr_csh$results_base[[3]]  #  cash transfer model
  m13 <- mainResults_mech_ben_curr_csh$results_base[[4]]  #  cash transfer model
  m14 <- mainResults_mech_ben_curr_csh$results_base[[5]]  #  cash transfer model
  m15 <- mainResults_mech_ben_curr_csh$results_base[[6]]  #  cash transfer model
  
  
  ## Productive inclusion regression models
  ### Extract specific models for productive inclusion from the main results
  mainResults_mech_ben_curr_pi <- mainResults_mech_ben_curr %>% filter(treat_var=="treatment_pi")
  
  m20 <- mainResults_mech_ben_curr_pi$results_base[[1]]  # First productive inclusion model
  m21 <- mainResults_mech_ben_curr_pi$results_base[[2]]  # Second productive inclusion model
  m22 <- mainResults_mech_ben_curr_pi$results_base[[3]]  #  productive inclusion model
  m23 <- mainResults_mech_ben_curr_pi$results_base[[4]]  #  productive inclusion model
  m24 <- mainResults_mech_ben_curr_pi$results_base[[5]]  #  productive inclusion model
  m25 <- mainResults_mech_ben_curr_pi$results_base[[6]]  #  productive inclusion model
  
  
  ### Extract specific models for productive inclusion (pool) from the main results
  mainResults_mech_ben_curr_pool <- mainResults_mech_ben_curr %>% filter(treat_var=="treatment_pi_pool")
  
  m30 <- mainResults_mech_ben_curr_pool$results_base[[1]]  # First pool productive inclusion model
  m31 <- mainResults_mech_ben_curr_pool$results_base[[2]]  # Second pool productive inclusion model
  m32 <- mainResults_mech_ben_curr_pool$results_base[[3]]  #  pool productive inclusion model
  m33 <- mainResults_mech_ben_curr_pool$results_base[[4]]  #  pool productive inclusion model
  m34 <- mainResults_mech_ben_curr_pool$results_base[[5]]  #  pool productive inclusion model
  m35 <- mainResults_mech_ben_curr_pool$results_base[[6]]  #  pool productive inclusion model
  
  # Initialize empty vectors to store descriptive statistics
  # These will store mean and standard deviation for each outcome variable
  mean_values <- c()  # Vector to store mean values
  sd_values <- c()   # Vector to store standard deviation values
  
  # Calculate descriptive statistics for each outcome variable
  # Loop through the list of outcome variables
  for (depvar in gender_var) {
    # Calculate and round mean, ignoring NA values
    mean_values <- c(mean_values,  round(mean(followup_MRT_hh_control[[depvar]], na.rm = TRUE), 3))
    
    # Calculate and round standard deviation, ignoring NA values
    sd_values <- c(sd_values,  round(sd(followup_MRT_hh_control[[depvar]], na.rm = TRUE), 3))
  }
  
  # Create lists of models for easier processing
  models_pi <- list(m20, m21, m22, m23, m24, m25)  # Productive inclusion models
  models_pi_pool <- list(m30, m31, m32, m33, m34, m35)  # Productive inclusion models pool
  models_csh_trnsfr <- list(m10, m11, m12, m13, m14, m15)  # Cash transfer models
  
  
  
  
  # Create regression tables for Productive Inclusion models
  tbl_list_pi <- map(models_pi, ~ tbl_regression(.x, 
                                                 exponentiate = FALSE,  # Keep coefficients as-is 
                                                 estimate_fun = ~ style_number(.x, digits = 2),
                                                 include=c(                                 # Retain main and interaction terms
                                                   "treatment_pi"))  %>% 
                       add_significance_stars(pattern = "{estimate}{stars} \n ({std.error})",
                                              hide_se = TRUE) %>%
                       add_glance_table(include = c(nobs, r.squared)))
  
  tbl_list_pi_pool <- map(models_pi_pool, ~ tbl_regression(.x, 
                                                           exponentiate = FALSE,  # Keep coefficients as-is 
                                                           estimate_fun = ~ style_number(.x, digits = 2),
                                                           include=c(                                 # Keep main pooled treatment and its interaction
                                                             "treatment_pi_pool"))  %>% 
                            add_significance_stars(pattern = "{estimate}{stars} \n ({std.error})",
                                                   hide_se = TRUE)%>%
                            add_glance_table(include = c(nobs, r.squared)))
  
  # Create regression tables for Cash Transfer models
  tbl_list_csh_trnsfr <- map(models_csh_trnsfr, ~ tbl_regression(.x, 
                                                                 exponentiate = FALSE,  # Keep coefficients as-is
                                                                 estimate_fun = ~ style_number(.x, digits = 2),
                                                                 include= c(                                 # Coefficients to retain
                                                                   "treatment_csh_trnsfr" 
                                                                   #  paste0(curr_het_var, ":treatment_csh_trnsfr")
                                                                 ))  %>% 
                               add_significance_stars(pattern = "{estimate}{stars} \n ({std.error})",
                                                      hide_se = TRUE)%>%
                               add_glance_table(include = c(nobs, r.squared)))
  
  # Modify headers for the Productive Inclusion and Cash Transfer models
  
  tbl_list_pi[[1]] <- tbl_list_pi[[1]] %>%
    modify_header(estimate = "Violence\n perceptions\n index")
  
  tbl_list_csh_trnsfr[[1]] <- tbl_list_csh_trnsfr[[1]] %>%
    modify_header(estimate = "Violence\n perceptions\n index")
  
  tbl_list_pi_pool[[1]] <- tbl_list_pi_pool[[1]] %>%
    modify_header(estimate = "Violence\n perceptions\n index")
  
  ### 
  tbl_list_pi[[2]] <- tbl_list_pi[[2]] %>%
    modify_header(estimate = "Food violence\n is OK\n (0,1)")
  
  tbl_list_csh_trnsfr[[2]] <- tbl_list_csh_trnsfr[[2]] %>%
    modify_header(estimate = "Food violence\n is OK\n (0,1)")
  
  tbl_list_pi_pool[[2]] <- tbl_list_pi_pool[[2]] %>%
    modify_header(estimate = "Food violence\n is OK\n (0,1)")
  
  tbl_list_pi[[3]] <- tbl_list_pi[[3]] %>%
    modify_header(estimate = "Children violence\n is OK\n (0,1)")
  
  tbl_list_csh_trnsfr[[3]] <- tbl_list_csh_trnsfr[[3]] %>%
    modify_header(estimate = "Children violence\n is OK\n (0,1)")
  
  tbl_list_pi_pool[[3]] <- tbl_list_pi_pool[[3]] %>%
    modify_header(estimate = "Children violence\n is OK\n (0,1)")
  
  
  tbl_list_pi[[4]] <- tbl_list_pi[[4]] %>%
    modify_header(estimate = "Should \n tolerate\n violence (1-4)")
  
  tbl_list_csh_trnsfr[[4]] <- tbl_list_csh_trnsfr[[4]] %>%
    modify_header(estimate = "Should \n tolerate\n violence (1-4)")
  
  tbl_list_pi_pool[[4]] <- tbl_list_pi_pool[[4]] %>%
    modify_header(estimate = "Should \n tolerate\n violence (1-4)")
  
  
  tbl_list_pi[[5]] <- tbl_list_pi[[5]] %>%
    modify_header(estimate = "Only\n men should\n work (1-4)")
  
  tbl_list_csh_trnsfr[[5]] <- tbl_list_csh_trnsfr[[5]] %>%
    modify_header(estimate = "Only\n men should\n work (1-4)")
  
  tbl_list_pi_pool[[5]] <- tbl_list_pi_pool[[5]] %>%
    modify_header(estimate = "Only\n men should\n work (1-4)")
  
  
  tbl_list_pi[[6]] <- tbl_list_pi[[6]] %>%
    modify_header(estimate = "Should\n school\n girls (1-4)")
  
  tbl_list_csh_trnsfr[[6]] <- tbl_list_csh_trnsfr[[6]] %>%
    modify_header(estimate = "Should\n school\n girls (1-4)")
  
  tbl_list_pi_pool[[6]] <- tbl_list_pi_pool[[6]] %>%
    modify_header(estimate = "Should\n school\n girls (1-4)")
  
  
  # Merge the stacked tables into a single table with column headers
  ## Cash transfert
  tbl_csh_trnsfr <- tbl_merge(
    tbls = list(tbl_list_csh_trnsfr[[1]],# First model
                tbl_list_csh_trnsfr[[2]], # Second model
                tbl_list_csh_trnsfr[[3]], 
                tbl_list_csh_trnsfr[[4]],
                tbl_list_csh_trnsfr[[5]], 
                tbl_list_csh_trnsfr[[6]]
    ), 
    tab_spanner = c("(1)","(2)","(3)","(4)","(5)","(6)")
  ) 
  
  ## Productive inclusion
  tbl_pi <- tbl_merge(
    tbls = list(tbl_list_pi[[1]],# First model
                tbl_list_pi[[2]], # Second model
                tbl_list_pi[[3]], 
                tbl_list_pi[[4]],
                tbl_list_pi[[5]], 
                tbl_list_pi[[6]]
    ), 
    tab_spanner = c("(1)","(2)","(3)","(4)","(5)","(6)")
  )
  
  ## Pool Productive inclusion
  tbl_pi_pool <- tbl_merge(
    tbls = list(tbl_list_pi_pool[[1]],# First model
                tbl_list_pi_pool[[2]], # Second model
                tbl_list_pi_pool[[3]], 
                tbl_list_pi_pool[[4]],
                tbl_list_pi_pool[[5]], 
                tbl_list_pi_pool[[6]]
    ),  
    tab_spanner = c("(1)","(2)","(3)","(4)","(5)","(6)")
  )
  
  # Create a list of stacked tables combining Cash Transfer and Productive Inclusion models
  # Each list element combines the corresponding models from both treatment types
  tbl <- tbl_stack(list(tbl_csh_trnsfr,  tbl_pi, tbl_pi_pool)) #tbl_pi,
  
  # Modify table body to customize variable labels
  tbl <- tbl %>%
    modify_table_body(~ .x %>%
                        mutate(label = case_when(
                          # Rename specific treatment variables
                          term_1 == "treatment_piCapital" ~ "Capital",
                          term_1 == "treatment_piPsychosocial" ~ "Psychosocial",
                          term_1 == "treatment_piFull" ~ "Full",
                          term_1 == "treatment_pi_poolPool" ~ "Pool",
                          
                          # Label cash transfer and productive inclusion treatments
                          variable == "treatment_csh_trnsfr" & label=="Tekavoul program IE treatment group" ~ "Tekavoul",
                          
                          variable == "treatment_pi" & label=="PI IE treatment group" ~ "PI",
                          variable == "treatment_pi_pool" & label=="treatment_pi_pool" ~ "PI (Pool)",

                          TRUE ~ label  # Keep original labels for other variables
                        )))
  
  
  additional_rows <- tribble(
    ~tbl_id1, ~variable, ~var_label, ~label,
    ~estimate_1, ~estimate_2, ~estimate_3, ~estimate_4, ~estimate_5,
    ~estimate_6,  
    ~stars_1, ~stars_2, ~stars_3, ~stars_4, ~stars_5,
    ~stars_6, 
    
    # Row 1: Control mean
    4, "Control mean @ follow up", "Control mean @ follow up", "Control mean @ follow up",
    mean_values[1], mean_values[2], mean_values[3], mean_values[4], mean_values[5],
    mean_values[6], 
    "", "", "", "", "", "", 
    
    # Row 2: Control SD
    4, "Control SD @ follow up", "Control SD @ follow up", "Control SD @ follow up",
    sd_values[1], sd_values[2], sd_values[3], sd_values[4], sd_values[5],
    sd_values[6], 
    "", "", "", "", "", ""
  )
  # browser()
  # Append the additional rows to the table body
  tbl$table_body <- bind_rows(tbl$table_body, additional_rows)
  
  # Title for each index
  tbl_ipv_title = paste0("**Table : effect of the programs by treatment**")
  
  # Final table formatting
  tbl <- tbl %>%
    # Set table caption
    modify_caption(tbl_ipv_title) %>%
    # Modify header for the label column
    modify_header(label = "**Outcome**") %>%
    # Remove all existing footnotes
    modify_footnote(everything() ~ NA) %>%
    modify_footnote(all_stat_cols() ~ NA) %>%
    # Add custom footnote with methodological details
    modify_footnote(label = "Notes: Results presented are OLS estimates that include controls for randomization strata (commune) and, where possible, baseline outcomes.  We control for social promotion intervention. Enumerator fixed effects are included in all regression. We estimate the regressions for the productive beneficiaries aged 18-49 only. Robust standard errors are shown in parentheses, clustered at the village proxy level. *** p < 0.01, ** p < 0.05, * p < 0.1.")
  
  # Print the final table
  tbl
  
}


getTable3_het_violence <- function(violence_var, reg_results){
  
  mainResults_mech_ben_curr <- reg_results
  
  
  ## Cash transfer regression models
  ### Extract specific models for cash transfer from the main results
  mainResults_mech_ben_curr_csh <- mainResults_mech_ben_curr %>% filter(treat_var=="treatment_csh_trnsfr")
  
  m10 <- mainResults_mech_ben_curr_csh$results_base[[1]]  # First cash transfer model
  m11 <- mainResults_mech_ben_curr_csh$results_base[[2]]  # Second cash transfer model
  m12 <- mainResults_mech_ben_curr_csh$results_base[[3]]  #  cash transfer model
  m13 <- mainResults_mech_ben_curr_csh$results_base[[4]]  #  cash transfer model
  
  
  ## Productive inclusion regression models
  ### Extract specific models for productive inclusion from the main results
  mainResults_mech_ben_curr_pi <- mainResults_mech_ben_curr %>% filter(treat_var=="treatment_pi")
  
  m20 <- mainResults_mech_ben_curr_pi$results_base[[1]]  # First productive inclusion model
  m21 <- mainResults_mech_ben_curr_pi$results_base[[2]]  # Second productive inclusion model
  m22 <- mainResults_mech_ben_curr_pi$results_base[[3]]  #  productive inclusion model
  m23 <- mainResults_mech_ben_curr_pi$results_base[[4]]  #  productive inclusion model
  
  
  ### Extract specific models for productive inclusion (pool) from the main results
  mainResults_mech_ben_curr_pool <- mainResults_mech_ben_curr %>% filter(treat_var=="treatment_pi_pool")
  
  m30 <- mainResults_mech_ben_curr_pool$results_base[[1]]  # First pool productive inclusion model
  m31 <- mainResults_mech_ben_curr_pool$results_base[[2]]  # Second pool productive inclusion model
  m32 <- mainResults_mech_ben_curr_pool$results_base[[3]]  #  pool productive inclusion model
  m33 <- mainResults_mech_ben_curr_pool$results_base[[4]]  #  pool productive inclusion model
  
  # Initialize empty vectors to store descriptive statistics
  # These will store mean and standard deviation for each outcome variable
  mean_values <- c()  # Vector to store mean values
  sd_values <- c()   # Vector to store standard deviation values
  
  # Calculate descriptive statistics for each outcome variable
  # Loop through the list of outcome variables
  for (depvar in violence_var) {
    # Calculate and round mean, ignoring NA values
    mean_values <- c(mean_values,  round(mean(followup_MRT_hh_control[[depvar]], na.rm = TRUE), 3))
    
    # Calculate and round standard deviation, ignoring NA values
    sd_values <- c(sd_values,  round(sd(followup_MRT_hh_control[[depvar]], na.rm = TRUE), 3))
  }
  
  # Create lists of models for easier processing
  models_pi <- list(m20, m21, m22, m23)  # Productive inclusion models
  models_pi_pool <- list(m30, m31, m32, m33)  # Productive inclusion models pool
  models_csh_trnsfr <- list(m10, m11, m12, m13)  # Cash transfer models
  
  
  
  
  # Create regression tables for Productive Inclusion models
  tbl_list_pi <- map(models_pi, ~ tbl_regression(.x, 
                                                 exponentiate = FALSE,  # Keep coefficients as-is 
                                                 estimate_fun = ~ style_number(.x, digits = 2),
                                                 include=c(                                 # Retain main and interaction terms
                                                   "treatment_pi"))  %>% 
                       add_significance_stars(pattern = "{estimate}{stars} \n ({std.error})",
                                              hide_se = TRUE) %>%
                       add_glance_table(include = c(nobs, r.squared)))
  
  tbl_list_pi_pool <- map(models_pi_pool, ~ tbl_regression(.x, 
                                                           exponentiate = FALSE,  # Keep coefficients as-is 
                                                           estimate_fun = ~ style_number(.x, digits = 2),
                                                           include=c(                                 # Keep main pooled treatment and its interaction
                                                             "treatment_pi_pool"))  %>% 
                            add_significance_stars(pattern = "{estimate}{stars} \n ({std.error})",
                                                   hide_se = TRUE)%>%
                            add_glance_table(include = c(nobs, r.squared)))
  
  # Create regression tables for Cash Transfer models
  tbl_list_csh_trnsfr <- map(models_csh_trnsfr, ~ tbl_regression(.x, 
                                                                 exponentiate = FALSE,  # Keep coefficients as-is
                                                                 estimate_fun = ~ style_number(.x, digits = 2),
                                                                 include= c(                                 # Coefficients to retain
                                                                   "treatment_csh_trnsfr"
                                                                 ))  %>% 
                               add_significance_stars(pattern = "{estimate}{stars} \n ({std.error})",
                                                      hide_se = TRUE)%>%
                               add_glance_table(include = c(nobs, r.squared)))
  
  # Modify headers for the Productive Inclusion and Cash Transfer models
  tbl_list_pi[[1]] <- tbl_list_pi[[1]] %>%
    modify_header(estimate = "Gender\n attitudes\n index")
  
  tbl_list_csh_trnsfr[[1]] <- tbl_list_csh_trnsfr[[1]] %>%
    modify_header(estimate = "Gender\n attitudes\n index")
  
  tbl_list_pi_pool[[1]] <- tbl_list_pi_pool[[1]] %>%
    modify_header(estimate = "Gender\n attitudes\n index")
  
  ### 
  tbl_list_pi[[2]] <- tbl_list_pi[[2]] %>%
    modify_header(estimate = "Know women\n with HH-tension\n (0-10)")
  
  tbl_list_csh_trnsfr[[2]] <- tbl_list_csh_trnsfr[[2]] %>%
    modify_header(estimate = "Know women\n with HH-tension\n (0-10)")
  
  tbl_list_pi_pool[[2]] <- tbl_list_pi_pool[[2]] %>%
    modify_header(estimate = "Know women\n with HH-tension\n (0-10)")
  
  tbl_list_pi[[3]] <- tbl_list_pi[[3]] %>%
    modify_header(estimate = "Women beaten\n for burning\n food (1-4)")
  
  tbl_list_csh_trnsfr[[3]] <- tbl_list_csh_trnsfr[[3]] %>%
    modify_header(estimate = "Women beaten\n for burning\n food (1-4)")
  
  tbl_list_pi_pool[[3]] <- tbl_list_pi_pool[[3]] %>%
    modify_header(estimate = "Women beaten\n for burning\n food (1-4)")
  
  
  tbl_list_pi[[4]] <- tbl_list_pi[[4]] %>%
    modify_header(estimate = "Women beaten\n for neglecting\n children (1-4)")
  
  tbl_list_csh_trnsfr[[4]] <- tbl_list_csh_trnsfr[[4]] %>%
    modify_header(estimate = "Women beaten\n for neglecting\n children (1-4)")
  
  tbl_list_pi_pool[[4]] <- tbl_list_pi_pool[[4]] %>%
    modify_header(estimate = "Women beaten\n for neglecting\n children (1-4)")
  
  
  # Merge the stacked tables into a single table with column headers
  ## Cash transfert
  tbl_csh_trnsfr <- tbl_merge(
    tbls = list(tbl_list_csh_trnsfr[[1]],# First model
                tbl_list_csh_trnsfr[[2]], # Second model
                tbl_list_csh_trnsfr[[3]], 
                tbl_list_csh_trnsfr[[4]]
    ), 
    tab_spanner = c("(1)","(2)","(3)","(4)")
  ) 
  
  ## Productive inclusion
  tbl_pi <- tbl_merge(
    tbls = list(tbl_list_pi[[1]],# First model
                tbl_list_pi[[2]], # Second model
                tbl_list_pi[[3]], 
                tbl_list_pi[[4]]
    ), 
    tab_spanner = c("(1)","(2)","(3)","(4)")
  )
  
  ## Pool Productive inclusion
  tbl_pi_pool <- tbl_merge(
    tbls = list(tbl_list_pi_pool[[1]],# First model
                tbl_list_pi_pool[[2]], # Second model
                tbl_list_pi_pool[[3]], 
                tbl_list_pi_pool[[4]]
    ),  
    tab_spanner = c("(1)","(2)","(3)","(4)")
  )
  
  # Create a list of stacked tables combining Cash Transfer and Productive Inclusion models
  # Each list element combines the corresponding models from both treatment types
  tbl <- tbl_stack(list(tbl_csh_trnsfr,  tbl_pi, tbl_pi_pool)) #tbl_pi,
  
  # Modify table body to customize variable labels
  tbl <- tbl %>%
    modify_table_body(~ .x %>%
                        mutate(label = case_when(
                          # Rename specific treatment variables
                          term_1 == "treatment_piCapital" ~ "Capital",
                          term_1 == "treatment_piPsychosocial" ~ "Psychosocial",
                          term_1 == "treatment_piFull" ~ "Full",
                          term_1 == "treatment_pi_poolPool" ~ "Pool",
                          
                          # Label cash transfer and productive inclusion treatments
                          variable == "treatment_csh_trnsfr" & label=="Tekavoul program IE treatment group" ~ "Tekavoul",
                          
                          variable == "treatment_pi" & label=="PI IE treatment group" ~ "PI",
                          variable == "treatment_pi_pool" & label=="treatment_pi_pool" ~ "PI (Pool)",
                          TRUE ~ label  # Keep original labels for other variables
                        )))
  
  
  additional_rows <- tribble(
    ~tbl_id1, ~variable, ~var_label, ~label,
    ~estimate_1, ~estimate_2, ~estimate_3, ~estimate_4,  
    ~stars_1, ~stars_2, ~stars_3, ~stars_4, 
    
    # Row 1: Control mean
    4, "Control mean @ follow up", "Control mean @ follow up", "Control mean @ follow up",
    mean_values[1], mean_values[2], mean_values[3], mean_values[4],
    "", "", "", "",
    
    # Row 2: Control SD
    4, "Control SD @ follow up", "Control SD @ follow up", "Control SD @ follow up",
    sd_values[1], sd_values[2], sd_values[3], sd_values[4], 
    "", "", "", "" 
  )

  # Append the additional rows to the table body
  tbl$table_body <- bind_rows(tbl$table_body, additional_rows)
  
  # Title for each index
  tbl_ipv_title = paste0("**Table : effect of the programs by treatment**")
  
  # Final table formatting
  tbl <- tbl %>%
    # Set table caption
    modify_caption(tbl_ipv_title) %>%
    # Modify header for the label column
    modify_header(label = "**Outcome**") %>%
    # Remove all existing footnotes
    modify_footnote(everything() ~ NA) %>%
    modify_footnote(all_stat_cols() ~ NA) %>%
    # Add custom footnote with methodological details
    modify_footnote(label = "Notes: Results presented are OLS estimates that include controls for randomization strata (commune) and, where possible, baseline outcomes.  We control for social promotion intervention. Enumerator fixed effects are included in all regression. We estimate the regressions for the productive beneficiaries aged 18-49 only. Robust standard errors are shown in parentheses, clustered at the village proxy level. *** p < 0.01, ** p < 0.05, * p < 0.1.")
  
  # Print the final table
  tbl
  
}

getTable3_het_private <- function(priv_expenditures, reg_results){
  
  mainResults_mech_ben_curr <- reg_results 
  
  
  ## Cash transfer regression models
  ### Extract specific models for cash transfer from the main results
  mainResults_mech_ben_curr_csh <- mainResults_mech_ben_curr %>% filter(treat_var=="treatment_csh_trnsfr")
  
  m10 <- mainResults_mech_ben_curr_csh$results_base[[1]]  # First cash transfer model
  m11 <- mainResults_mech_ben_curr_csh$results_base[[2]]  # Second cash transfer model
  m12 <- mainResults_mech_ben_curr_csh$results_base[[3]]  #  cash transfer model
  
  
  
  ## Productive inclusion regression models
  ### Extract specific models for productive inclusion from the main results
  mainResults_mech_ben_curr_pi <- mainResults_mech_ben_curr %>% filter(treat_var=="treatment_pi")
  
  m20 <- mainResults_mech_ben_curr_pi$results_base[[1]]  # First productive inclusion model
  m21 <- mainResults_mech_ben_curr_pi$results_base[[2]]  # Second productive inclusion model
  m22 <- mainResults_mech_ben_curr_pi$results_base[[3]]  #  productive inclusion model

  
  ### Extract specific models for productive inclusion (pool) from the main results
  mainResults_mech_ben_curr_pool <- mainResults_mech_ben_curr %>% filter(treat_var=="treatment_pi_pool")
  
  m30 <- mainResults_mech_ben_curr_pool$results_base[[1]]  # First pool productive inclusion model
  m31 <- mainResults_mech_ben_curr_pool$results_base[[2]]  # Second pool productive inclusion model
  m32 <- mainResults_mech_ben_curr_pool$results_base[[3]]  #  pool productive inclusion model

  
  # Initialize empty vectors to store descriptive statistics
  # These will store mean and standard deviation for each outcome variable
  mean_values <- c()  # Vector to store mean values
  sd_values <- c()   # Vector to store standard deviation values
  
  # Calculate descriptive statistics for each outcome variable
  # Loop through the list of outcome variables
  for (depvar in priv_expenditures) {
    # Calculate and round mean, ignoring NA values
    mean_values <- c(mean_values,  round(mean(followup_MRT_hh_control[[depvar]], na.rm = TRUE), 3))
    
    # Calculate and round standard deviation, ignoring NA values
    sd_values <- c(sd_values,  round(sd(followup_MRT_hh_control[[depvar]], na.rm = TRUE), 3))
  }
  
  # Create lists of models for easier processing
  models_pi <- list(m20, m21, m22)  # Productive inclusion models
  models_pi_pool <- list(m30, m31, m32)  # Productive inclusion models pool
  models_csh_trnsfr <- list(m10, m11, m12)  # Cash transfer models
  
  
  
  
  # Create regression tables for Productive Inclusion models
  tbl_list_pi <- map(models_pi, ~ tbl_regression(.x, 
                                                 exponentiate = FALSE,  # Keep coefficients as-is 
                                                 estimate_fun = ~ style_number(.x, digits = 2),
                                                 include=c(                                 # Retain main and interaction terms
                                                   "treatment_pi"))  %>% 
                       add_significance_stars(pattern = "{estimate}{stars} \n ({std.error})",
                                              hide_se = TRUE) %>%
                       add_glance_table(include = c(nobs, r.squared)))
  
  tbl_list_pi_pool <- map(models_pi_pool, ~ tbl_regression(.x, 
                                                           exponentiate = FALSE,  # Keep coefficients as-is 
                                                           estimate_fun = ~ style_number(.x, digits = 2),
                                                           include=c(                                 # Keep main pooled treatment and its interaction
                                                             "treatment_pi_pool"))  %>% 
                            add_significance_stars(pattern = "{estimate}{stars} \n ({std.error})",
                                                   hide_se = TRUE)%>%
                            add_glance_table(include = c(nobs, r.squared)))
  
  # Create regression tables for Cash Transfer models
  tbl_list_csh_trnsfr <- map(models_csh_trnsfr, ~ tbl_regression(.x, 
                                                                 exponentiate = FALSE,  # Keep coefficients as-is
                                                                 estimate_fun = ~ style_number(.x, digits = 2),
                                                                 include= c(                                 # Coefficients to retain
                                                                   "treatment_csh_trnsfr" 
                                                                   #  paste0(curr_het_var, ":treatment_csh_trnsfr")
                                                                 ))  %>% 
                               add_significance_stars(pattern = "{estimate}{stars} \n ({std.error})",
                                                      hide_se = TRUE)%>%
                               add_glance_table(include = c(nobs, r.squared)))
  
  # Modify headers for the Productive Inclusion and Cash Transfer models 
  
  tbl_list_pi[[1]] <- tbl_list_pi[[1]] %>%
    modify_header(estimate = "Partner private \n goods \n consumption (yearly)")
  
  tbl_list_csh_trnsfr[[1]] <- tbl_list_csh_trnsfr[[1]] %>%
    modify_header(estimate = "Partner private \n goods \n consumption (yearly)")
  
  tbl_list_pi_pool[[1]] <- tbl_list_pi_pool[[1]] %>%
    modify_header(estimate = "Partner private \n goods \n consumption (yearly)")
  
  ### 
  tbl_list_pi[[2]] <- tbl_list_pi[[2]] %>%
    modify_header(estimate = "Benef. private \n goods \n consumption (yearly)")
  
  tbl_list_csh_trnsfr[[2]] <- tbl_list_csh_trnsfr[[2]] %>%
    modify_header(estimate = "Benef. private \n goods \n consumption (yearly)")
  
  tbl_list_pi_pool[[2]] <- tbl_list_pi_pool[[2]] %>%
    modify_header(estimate = "Benef. private \n goods \n consumption (yearly)")
  
  tbl_list_pi[[3]] <- tbl_list_pi[[3]] %>%
    modify_header(estimate = "Other \n goods \n consumption (yearly)")
  
  tbl_list_csh_trnsfr[[3]] <- tbl_list_csh_trnsfr[[3]] %>%
    modify_header(estimate = "Other \n goods \n consumption (yearly)")
  
  tbl_list_pi_pool[[3]] <- tbl_list_pi_pool[[3]] %>%
    modify_header(estimate = "Other \n goods \n consumption (yearly)")
  
  
  # Merge the stacked tables into a single table with column headers
  ## Cash transfert
  tbl_csh_trnsfr <- tbl_merge(
    tbls = list(tbl_list_csh_trnsfr[[1]],# First model
                tbl_list_csh_trnsfr[[2]], # Second model
                tbl_list_csh_trnsfr[[3]]
    ), 
    tab_spanner = c("(1)","(2)","(3)")
  ) 
  
  ## Productive inclusion
  tbl_pi <- tbl_merge(
    tbls = list(tbl_list_pi[[1]],# First model
                tbl_list_pi[[2]], # Second model
                tbl_list_pi[[3]]
    ), 
    tab_spanner = c("(1)","(2)","(3)")
  )
  
  ## Pool Productive inclusion
  tbl_pi_pool <- tbl_merge(
    tbls = list(tbl_list_pi_pool[[1]],# First model
                tbl_list_pi_pool[[2]], # Second model
                tbl_list_pi_pool[[3]]
    ),  
    tab_spanner = c("(1)","(2)","(3)")
  )

  # Create a list of stacked tables combining Cash Transfer and Productive Inclusion models
  # Each list element combines the corresponding models from both treatment types
  tbl <- tbl_stack(list(tbl_csh_trnsfr,  tbl_pi, tbl_pi_pool)) #tbl_pi,
  
  # Modify table body to customize variable labels
  tbl <- tbl %>%
    modify_table_body(~ .x %>%
                        mutate(label = case_when(
                          # Rename specific treatment variables
                          term_1 == "treatment_piCapital" ~ "Capital",
                          term_1 == "treatment_piPsychosocial" ~ "Psychosocial",
                          term_1 == "treatment_piFull" ~ "Full",
                          term_1 == "treatment_pi_poolPool" ~ "Pool",
                          
                          # Label cash transfer and productive inclusion treatments
                          variable == "treatment_csh_trnsfr" & label=="Tekavoul program IE treatment group" ~ "Tekavoul",
                          
                          variable == "treatment_pi" & label=="PI IE treatment group" ~ "PI",
                          variable == "treatment_pi_pool" & label=="treatment_pi_pool" ~ "PI (Pool)",
                          
                          TRUE ~ label  # Keep original labels for other variables
                        )))
  
  
  additional_rows <- tribble(
    ~tbl_id1, ~variable, ~var_label, ~label,
    ~estimate_1, ~estimate_2, ~estimate_3, 
    ~stars_1, ~stars_2, ~stars_3, 
    
    # Row 1: Control mean
    4, "Control mean @ follow up", "Control mean @ follow up", "Control mean @ follow up",
    mean_values[1], mean_values[2], mean_values[3], 
    "", "", "",
    
    # Row 2: Control SD
    4, "Control SD @ follow up", "Control SD @ follow up", "Control SD @ follow up",
    sd_values[1], sd_values[2], sd_values[3], 
    "", "", ""
  )

  # Append the additional rows to the table body
  tbl$table_body <- bind_rows(tbl$table_body, additional_rows)
  
  # Title for each index
  tbl_ipv_title = paste0("**Table : effect of the programs by treatment**")
  
  # Final table formatting
  tbl <- tbl %>%
    # Set table caption
    modify_caption(tbl_ipv_title) %>%
    # Modify header for the label column
    modify_header(label = "**Outcome**") %>%
    # Remove all existing footnotes
    modify_footnote(everything() ~ NA) %>%
    modify_footnote(all_stat_cols() ~ NA) %>%
    # Add custom footnote with methodological details
    modify_footnote(label = "Notes: Results presented are OLS estimates that include controls for randomization strata (commune) and, where possible, baseline outcomes.  We control for social promotion intervention. Enumerator fixed effects are included in all regression. We estimate the regressions for the productive beneficiaries aged 18-49 only. Robust standard errors are shown in parentheses, clustered at the village proxy level. *** p < 0.01, ** p < 0.05, * p < 0.1.")
  
  # Print the final table
  tbl
  
  
}

getTable3_het_intra <- function(intra_var, reg_results){
  
  mainResults_mech_ben_curr <- reg_results 
  
  
  ## Cash transfer regression models
  ### Extract specific models for cash transfer from the main results
  mainResults_mech_ben_curr_csh <- mainResults_mech_ben_curr %>% filter(treat_var=="treatment_csh_trnsfr")
  
  m10 <- mainResults_mech_ben_curr_csh$results_base[[1]]  # First cash transfer model
  m11 <- mainResults_mech_ben_curr_csh$results_base[[2]]  # Second cash transfer model
  m12 <- mainResults_mech_ben_curr_csh$results_base[[3]]  #  cash transfer model
  m13 <- mainResults_mech_ben_curr_csh$results_base[[4]]  #  cash transfer model
  m14 <- mainResults_mech_ben_curr_csh$results_base[[5]]  #  cash transfer model
  m15 <- mainResults_mech_ben_curr_csh$results_base[[6]]  #  cash transfer model
  m16 <- mainResults_mech_ben_curr_csh$results_base[[7]]  # Second cash transfer model
  m17 <- mainResults_mech_ben_curr_csh$results_base[[8]]  #  cash transfer model
  m18 <- mainResults_mech_ben_curr_csh$results_base[[9]]  #  cash transfer model
  
  
  
  ## Productive inclusion regression models
  ### Extract specific models for productive inclusion from the main results
  mainResults_mech_ben_curr_pi <- mainResults_mech_ben_curr %>% filter(treat_var=="treatment_pi")
  
  m20 <- mainResults_mech_ben_curr_pi$results_base[[1]]  # First productive inclusion model
  m21 <- mainResults_mech_ben_curr_pi$results_base[[2]]  # Second productive inclusion model
  m22 <- mainResults_mech_ben_curr_pi$results_base[[3]]  #  productive inclusion model
  m23 <- mainResults_mech_ben_curr_pi$results_base[[4]]  #  productive inclusion model
  m24 <- mainResults_mech_ben_curr_pi$results_base[[5]]  #  productive inclusion model
  m25 <- mainResults_mech_ben_curr_pi$results_base[[6]]  #  productive inclusion model
  m26 <- mainResults_mech_ben_curr_pi$results_base[[7]]  #  productive inclusion model
  m27 <- mainResults_mech_ben_curr_pi$results_base[[8]]  #  productive inclusion model
  m28 <- mainResults_mech_ben_curr_pi$results_base[[9]]  #  productive inclusion model
  
  ### Extract specific models for productive inclusion (pool) from the main results
  mainResults_mech_ben_curr_pool <- mainResults_mech_ben_curr %>% filter(treat_var=="treatment_pi_pool")
  
  m30 <- mainResults_mech_ben_curr_pool$results_base[[1]]  # First pool productive inclusion model
  m31 <- mainResults_mech_ben_curr_pool$results_base[[2]]  # Second pool productive inclusion model
  m32 <- mainResults_mech_ben_curr_pool$results_base[[3]]  #  pool productive inclusion model
  m33 <- mainResults_mech_ben_curr_pool$results_base[[4]]  #  pool productive inclusion model
  m34 <- mainResults_mech_ben_curr_pool$results_base[[5]]  #  pool productive inclusion model
  m35 <- mainResults_mech_ben_curr_pool$results_base[[6]]  #  pool productive inclusion model
  m36 <- mainResults_mech_ben_curr_pool$results_base[[7]]  #  pool productive inclusion model
  m37 <- mainResults_mech_ben_curr_pool$results_base[[8]]  #  pool productive inclusion model
  m38 <- mainResults_mech_ben_curr_pool$results_base[[9]]  #  pool productive inclusion model
  
  # Initialize empty vectors to store descriptive statistics
  # These will store mean and standard deviation for each outcome variable
  mean_values <- c()  # Vector to store mean values
  sd_values <- c()   # Vector to store standard deviation values
  
  # Calculate descriptive statistics for each outcome variable
  # Loop through the list of outcome variables
  for (depvar in intra_var) {
    # Calculate and round mean, ignoring NA values
    mean_values <- c(mean_values,  round(mean(followup_MRT_hh_control[[depvar]], na.rm = TRUE), 3))
    
    # Calculate and round standard deviation, ignoring NA values
    sd_values <- c(sd_values,  round(sd(followup_MRT_hh_control[[depvar]], na.rm = TRUE), 3))
  }
  
  # Create lists of models for easier processing
  models_pi <- list(m20, m21, m22, m23, m24, m25, m26, m27, m28)  # Productive inclusion models
  models_pi_pool <- list(m30, m31, m32, m33, m34, m35, m36, m37, m38)  # Productive inclusion models pool
  models_csh_trnsfr <- list(m10, m11, m12, m13, m14, m15, m16, m17, m18)  # Cash transfer models
  
  
  
  
  # Create regression tables for Productive Inclusion models
  tbl_list_pi <- map(models_pi, ~ tbl_regression(.x, 
                                                 exponentiate = FALSE,  # Keep coefficients as-is 
                                                 estimate_fun = ~ style_number(.x, digits = 2),
                                                 include=c(                                 # Retain main and interaction terms
                                                   "treatment_pi"))  %>% 
                       add_significance_stars(pattern = "{estimate}{stars} \n ({std.error})",
                                              hide_se = TRUE) %>%
                       add_glance_table(include = c(nobs, r.squared)))
  
  tbl_list_pi_pool <- map(models_pi_pool, ~ tbl_regression(.x, 
                                                           exponentiate = FALSE,  # Keep coefficients as-is 
                                                           estimate_fun = ~ style_number(.x, digits = 2),
                                                           include=c(                                 # Keep main pooled treatment and its interaction
                                                             "treatment_pi_pool"))  %>% 
                            add_significance_stars(pattern = "{estimate}{stars} \n ({std.error})",
                                                   hide_se = TRUE)%>%
                            add_glance_table(include = c(nobs, r.squared)))
  
  # Create regression tables for Cash Transfer models
  tbl_list_csh_trnsfr <- map(models_csh_trnsfr, ~ tbl_regression(.x, 
                                                                 exponentiate = FALSE,  # Keep coefficients as-is
                                                                 estimate_fun = ~ style_number(.x, digits = 2),
                                                                 include= c(                                 # Coefficients to retain
                                                                   "treatment_csh_trnsfr" 
                                                                   #  paste0(curr_het_var, ":treatment_csh_trnsfr")
                                                                 ))  %>% 
                               add_significance_stars(pattern = "{estimate}{stars} \n ({std.error})",
                                                      hide_se = TRUE)%>%
                               add_glance_table(include = c(nobs, r.squared)))
  
  # Modify headers for the Productive Inclusion and Cash Transfer models
  
  tbl_list_pi[[1]] <- tbl_list_pi[[1]] %>%
    modify_header(estimate = "Intra-household \n dynamics \n index")
  
  tbl_list_csh_trnsfr[[1]] <- tbl_list_csh_trnsfr[[1]] %>%
    modify_header(estimate = "Intra-household \n dynamics \n index")
  
  tbl_list_pi_pool[[1]] <- tbl_list_pi_pool[[1]] %>%
    modify_header(estimate = "Intra-household \n dynamics \n index")
  
  ### 
  tbl_list_pi[[2]] <- tbl_list_pi[[2]] %>%
    modify_header(estimate = "Partner \n dynamics \n index")
  
  tbl_list_csh_trnsfr[[2]] <- tbl_list_csh_trnsfr[[2]] %>%
    modify_header(estimate = "Partner \n dynamics \n index")
  
  tbl_list_pi_pool[[2]] <- tbl_list_pi_pool[[2]] %>%
    modify_header(estimate = "Partner \n dynamics \n index")
  
  tbl_list_pi[[3]] <- tbl_list_pi[[3]] %>%
    modify_header(estimate = "Household \n dynamics \n index")
  
  tbl_list_csh_trnsfr[[3]] <- tbl_list_csh_trnsfr[[3]] %>%
    modify_header(estimate = "Household \n dynamics \n index")
  
  tbl_list_pi_pool[[3]] <- tbl_list_pi_pool[[3]] %>%
    modify_header(estimate = "Household \n dynamics \n index")
  
  
  tbl_list_pi[[4]] <- tbl_list_pi[[4]] %>%
    modify_header(estimate = "Comfortable \n disagreeing with \n partner (1-4)")
  
  tbl_list_csh_trnsfr[[4]] <- tbl_list_csh_trnsfr[[4]] %>%
    modify_header(estimate = "Comfortable \n disagreeing with \n partner (1-4)")
  
  tbl_list_pi_pool[[4]] <- tbl_list_pi_pool[[4]] %>%
    modify_header(estimate = "Comfortable \n disagreeing with \n partner (1-4)")
  
  
  ### partner
  tbl_list_pi[[5]] <- tbl_list_pi[[5]] %>%
    modify_header(estimate = "Trusts\n partner\n (1-4)")
  
  tbl_list_csh_trnsfr[[5]] <- tbl_list_csh_trnsfr[[5]] %>%
    modify_header(estimate = "Trusts\n partner\n (1-4)")
  
  tbl_list_pi_pool[[5]] <- tbl_list_pi_pool[[5]] %>%
    modify_header(estimate = "Trusts\n partner\n (1-4)")
  
  
  tbl_list_pi[[6]] <- tbl_list_pi[[6]] %>%
    modify_header(estimate = "Partner\n inclusiveness\n (1-4)")
  
  tbl_list_csh_trnsfr[[6]] <- tbl_list_csh_trnsfr[[6]] %>%
    modify_header(estimate = "Partner\n inclusiveness\n (1-4)")
  
  tbl_list_pi_pool[[6]] <- tbl_list_pi_pool[[6]] %>%
    modify_header(estimate = "Partner\n inclusiveness\n (1-4)")
  
  
  tbl_list_pi[[7]] <- tbl_list_pi[[7]] %>%
    modify_header(estimate = "Household\n inclusiveness\n (1-4)")
  
  tbl_list_csh_trnsfr[[7]] <- tbl_list_csh_trnsfr[[7]] %>%
    modify_header(estimate = "Household\n inclusiveness\n (1-4)")
  
  tbl_list_pi_pool[[7]] <- tbl_list_pi_pool[[7]] %>%
    modify_header(estimate = "Household\n inclusiveness\n (1-4)")
  
  
  tbl_list_pi[[8]] <- tbl_list_pi[[8]] %>%
    modify_header(estimate = "Household\n tensions\n frequent (1-4)")
  
  tbl_list_csh_trnsfr[[8]] <- tbl_list_csh_trnsfr[[8]] %>%
    modify_header(estimate = "Household\n tensions\n frequent (1-4)")
  
  tbl_list_pi_pool[[8]] <- tbl_list_pi_pool[[8]] %>%
    modify_header(estimate = "Household\n tensions\n frequent (1-4)")
  
  
  tbl_list_pi[[9]] <- tbl_list_pi[[9]] %>%
    modify_header(estimate = "Relationship \n satisfaction \n (1-10)")
  
  tbl_list_csh_trnsfr[[9]] <- tbl_list_csh_trnsfr[[9]] %>%
    modify_header(estimate =  "Relationship \n satisfaction \n (1-10)")
  
  tbl_list_pi_pool[[9]] <- tbl_list_pi_pool[[9]] %>%
    modify_header(estimate = "Relationship \n satisfaction \n (1-10)")
  
  # Merge the stacked tables into a single table with column headers
  ## Cash transfert
  tbl_csh_trnsfr <- tbl_merge(
    tbls = list(tbl_list_csh_trnsfr[[1]],# First model
                tbl_list_csh_trnsfr[[2]], # Second model
                tbl_list_csh_trnsfr[[3]], 
                tbl_list_csh_trnsfr[[4]],
                tbl_list_csh_trnsfr[[5]], 
                tbl_list_csh_trnsfr[[6]], 
                tbl_list_csh_trnsfr[[7]], 
                tbl_list_csh_trnsfr[[8]],
                tbl_list_csh_trnsfr[[9]]
    ), 
    tab_spanner = c("(1)","(2)","(3)","(4)","(5)","(6)", "(7)","(8)","(9)")
  ) 
  
  ## Productive inclusion
  tbl_pi <- tbl_merge(
    tbls = list(tbl_list_pi[[1]],# First model
                tbl_list_pi[[2]], # Second model
                tbl_list_pi[[3]], 
                tbl_list_pi[[4]],
                tbl_list_pi[[5]], 
                tbl_list_pi[[6]], 
                tbl_list_pi[[7]], 
                tbl_list_pi[[8]],
                tbl_list_pi[[9]]
    ), 
    tab_spanner = c("(1)","(2)","(3)","(4)","(5)","(6)", "(7)","(8)","(9)")
  )
  
  ## Pool Productive inclusion
  tbl_pi_pool <- tbl_merge(
    tbls = list(tbl_list_pi_pool[[1]],# First model
                tbl_list_pi_pool[[2]], # Second model
                tbl_list_pi_pool[[3]], 
                tbl_list_pi_pool[[4]],
                tbl_list_pi_pool[[5]], 
                tbl_list_pi_pool[[6]], 
                tbl_list_pi_pool[[7]], 
                tbl_list_pi_pool[[8]],
                tbl_list_pi_pool[[9]]
    ),  
    tab_spanner = c("(1)","(2)","(3)","(4)","(5)","(6)", "(7)","(8)","(9)")
  )
  
  # Create a list of stacked tables combining Cash Transfer and Productive Inclusion models
  # Each list element combines the corresponding models from both treatment types
  tbl <- tbl_stack(list(tbl_csh_trnsfr,  tbl_pi, tbl_pi_pool)) #tbl_pi,
  
  # Modify table body to customize variable labels
  tbl <- tbl %>%
    modify_table_body(~ .x %>%
                        mutate(label = case_when(
                          # Rename specific treatment variables
                          term_1 == "treatment_piCapital" ~ "Capital",
                          term_1 == "treatment_piPsychosocial" ~ "Psychosocial",
                          term_1 == "treatment_piFull" ~ "Full",
                          term_1 == "treatment_pi_poolPool" ~ "Pool",
                          
                          # Label cash transfer and productive inclusion treatments
                          variable == "treatment_csh_trnsfr" & label=="Tekavoul program IE treatment group" ~ "Tekavoul",
                          
                          variable == "treatment_pi" & label=="PI IE treatment group" ~ "PI",
                          variable == "treatment_pi_pool" & label=="treatment_pi_pool" ~ "PI (Pool)",

                          TRUE ~ label  # Keep original labels for other variables
                        )))
  
  
  additional_rows <- tribble(
    ~tbl_id1, ~variable, ~var_label, ~label,
    ~estimate_1, ~estimate_2, ~estimate_3, ~estimate_4, ~estimate_5,
    ~estimate_6, ~estimate_7, ~estimate_8, ~estimate_9, 
    ~stars_1, ~stars_2, ~stars_3, ~stars_4, ~stars_5,
    ~stars_6, ~stars_7, ~stars_8, ~stars_9, 
    
    # Row 1: Control mean
    4, "Control mean @ follow up", "Control mean @ follow up", "Control mean @ follow up",
    mean_values[1], mean_values[2], mean_values[3], mean_values[4], mean_values[5],
    mean_values[6], mean_values[7], mean_values[8], mean_values[9], 
    "", "", "", "", "", "", "", "", "", 
    
    # Row 2: Control SD
    4, "Control SD @ follow up", "Control SD @ follow up", "Control SD @ follow up",
    sd_values[1], sd_values[2], sd_values[3], sd_values[4], sd_values[5],
    sd_values[6], sd_values[7], sd_values[8], sd_values[9],
    "", "", "", "", "", "", "", "", "" 
  )
  # browser()
  # Append the additional rows to the table body
  tbl$table_body <- bind_rows(tbl$table_body, additional_rows)
  
  # Title for each index
  tbl_ipv_title = paste0("**Table : effect of the programs by treatment**")
  
  # Final table formatting
  tbl <- tbl %>%
    # Set table caption
    modify_caption(tbl_ipv_title) %>%
    # Modify header for the label column
    modify_header(label = "**Outcome**") %>%
    # Remove all existing footnotes
    modify_footnote(everything() ~ NA) %>%
    modify_footnote(all_stat_cols() ~ NA) %>%
    # Add custom footnote with methodological details
    modify_footnote(label = "Notes: Results presented are OLS estimates that include controls for randomization strata (commune) and, where possible, baseline outcomes.  We control for social promotion intervention. Enumerator fixed effects are included in all regression. We estimate the regressions for the productive beneficiaries aged 18-49 only. Robust standard errors are shown in parentheses, clustered at the village proxy level. *** p < 0.01, ** p < 0.05, * p < 0.1.")
  
  # Print the final table
  tbl
  
}


getTable5_het <- function(time_use_vars, mainResults_mech_hh){
  
  mainResults_mech_ben_curr <- mainResults_mech_ben 
  
  
  ## Cash transfer regression models
  ### Extract specific models for cash transfer from the main results
  mainResults_mech_ben_curr_csh <- mainResults_mech_ben_curr %>% filter(treat_var=="treatment_csh_trnsfr")
  
  m10 <- mainResults_mech_ben_curr_csh$results_base[[1]]  # First cash transfer model
  m11 <- mainResults_mech_ben_curr_csh$results_base[[2]]  # Second cash transfer model
  m12 <- mainResults_mech_ben_curr_csh$results_base[[3]]  #  cash transfer model
  m13 <- mainResults_mech_ben_curr_csh$results_base[[4]]  #  cash transfer model
  
  ## Productive inclusion regression models
  ### Extract specific models for productive inclusion from the main results
  mainResults_mech_ben_curr_pi <- mainResults_mech_ben_curr %>% filter(treat_var=="treatment_pi")
  
  m20 <- mainResults_mech_ben_curr_pi$results_base[[1]]  # First productive inclusion model
  m21 <- mainResults_mech_ben_curr_pi$results_base[[2]]  # Second productive inclusion model
  m22 <- mainResults_mech_ben_curr_pi$results_base[[3]]  #  productive inclusion model
  m23 <- mainResults_mech_ben_curr_pi$results_base[[4]]  #  productive inclusion model
  
  ### Extract specific models for productive inclusion (pool) from the main results
  mainResults_mech_ben_curr_pool <- mainResults_mech_ben_curr %>% filter(treat_var=="treatment_pi_pool")
  
  m30 <- mainResults_mech_ben_curr_pool$results_base[[1]]  # First pool productive inclusion model
  m31 <- mainResults_mech_ben_curr_pool$results_base[[2]]  # Second pool productive inclusion model
  m32 <- mainResults_mech_ben_curr_pool$results_base[[3]]  #  pool productive inclusion model
  m33 <- mainResults_mech_ben_curr_pool$results_base[[4]]  #  pool productive inclusion model

  
  # Initialize empty vectors to store descriptive statistics
  # These will store mean and standard deviation for each outcome variable
  mean_values <- c()  # Vector to store mean values
  sd_values <- c()   # Vector to store standard deviation values
  
  # Calculate descriptive statistics for each outcome variable
  # Loop through the list of outcome variables
  for (depvar in time_use_vars) {
    # Calculate and round mean, ignoring NA values
    mean_values <- c(mean_values,  round(mean(followup_MRT_hh_control[[depvar]], na.rm = TRUE), 3))
    
    # Calculate and round standard deviation, ignoring NA values
    sd_values <- c(sd_values,  round(sd(followup_MRT_hh_control[[depvar]], na.rm = TRUE), 3))
  }
  
  # Create lists of models for easier processing
  models_pi <- list(m20, m21, m22, m23)  # Productive inclusion models
  models_pi_pool <- list(m30, m31, m32, m33)  # Productive inclusion models pool
  models_csh_trnsfr <- list(m10, m11, m12, m13)  # Cash transfer models
  
  
  
  
  # Create regression tables for Productive Inclusion models
  tbl_list_pi <- map(models_pi, ~ tbl_regression(.x, 
                                                 exponentiate = FALSE,  # Keep coefficients as-is 
                                                 estimate_fun = ~ style_number(.x, digits = 2),
                                                 include=c(                                 # Retain main and interaction terms
                                                   "treatment_pi"))  %>% 
                       add_significance_stars(pattern = "{estimate}{stars} \n ({std.error})",
                                              hide_se = TRUE) %>%
                       add_glance_table(include = c(nobs, r.squared)))
  
  tbl_list_pi_pool <- map(models_pi_pool, ~ tbl_regression(.x, 
                                                           exponentiate = FALSE,  # Keep coefficients as-is 
                                                           estimate_fun = ~ style_number(.x, digits = 2),
                                                           include=c(                                 # Keep main pooled treatment and its interaction
                                                             "treatment_pi_pool"))  %>% 
                            add_significance_stars(pattern = "{estimate}{stars} \n ({std.error})",
                                                   hide_se = TRUE)%>%
                            add_glance_table(include = c(nobs, r.squared)))
  
  # Create regression tables for Cash Transfer models
  tbl_list_csh_trnsfr <- map(models_csh_trnsfr, ~ tbl_regression(.x, 
                                                                 exponentiate = FALSE,  # Keep coefficients as-is
                                                                 estimate_fun = ~ style_number(.x, digits = 2),
                                                                 include= c(                                 # Coefficients to retain
                                                                   "treatment_csh_trnsfr" 
                                                                   #  paste0(curr_het_var, ":treatment_csh_trnsfr")
                                                                 ))  %>% 
                               add_significance_stars(pattern = "{estimate}{stars} \n ({std.error})",
                                                      hide_se = TRUE)%>%
                               add_glance_table(include = c(nobs, r.squared)))
  
  # Modify headers for the Productive Inclusion and Cash Transfer models
  ## Tot Mins in household chores activities
  tbl_list_pi[[1]] <- tbl_list_pi[[1]] %>%
    modify_header(estimate = "Tot Mins in \n household \n chores (last 7 days)")
  
  tbl_list_csh_trnsfr[[1]] <- tbl_list_csh_trnsfr[[1]] %>%
    modify_header(estimate = "Tot Mins in \n household \n chores (last 7 days)")
  
  tbl_list_pi_pool[[1]] <- tbl_list_pi_pool[[1]] %>%
    modify_header(estimate = "Tot Mins in \n household \n chores (last 7 days)")
  
  ## Tot Mins in market income activities
  tbl_list_pi[[2]] <- tbl_list_pi[[2]] %>%
    modify_header(estimate = "Tot Mins in \n market income \n activities (last 7 days)")
  
  tbl_list_csh_trnsfr[[2]] <- tbl_list_csh_trnsfr[[2]] %>%
    modify_header(estimate = "Tot Mins in \n market income \n activities (last 7 days)")
  
  tbl_list_pi_pool[[2]] <- tbl_list_pi_pool[[2]] %>%
    modify_header(estimate = "Tot Mins in \n market income \n activities (last 7 days)")
  
  ## Tot Mins in leisure activities
  tbl_list_pi[[3]] <- tbl_list_pi[[3]] %>%
    modify_header(estimate = "Tot Mins in \n leisure \n activities (last 7 days)")
  
  tbl_list_csh_trnsfr[[3]] <- tbl_list_csh_trnsfr[[3]] %>%
    modify_header(estimate = "Tot Mins in \n leisure \n activities (last 7 days)")
  
  tbl_list_pi_pool[[3]] <- tbl_list_pi_pool[[3]] %>%
    modify_header(estimate = "Tot Mins in \n leisure \n activities (last 7 days)")
  
  ## Tot Mins in other activities
  tbl_list_pi[[4]] <- tbl_list_pi[[4]] %>%
    modify_header(estimate = "Tot Mins in \n other \n activities (last 7 days)")
  
  tbl_list_csh_trnsfr[[4]] <- tbl_list_csh_trnsfr[[4]] %>%
    modify_header(estimate = "Tot Mins in \n other \n activities (last 7 days)")
  
  tbl_list_pi_pool[[4]] <- tbl_list_pi_pool[[4]] %>%
    modify_header(estimate = "Tot Mins in \n other \n activities (last 7 days)")  
  
  
  # Merge the stacked tables into a single table with column headers
  ## Cash transfert
  tbl_csh_trnsfr <- tbl_merge(
    tbls = list(tbl_list_csh_trnsfr[[1]],# First model
                tbl_list_csh_trnsfr[[2]], # Second model
                tbl_list_csh_trnsfr[[3]],
                tbl_list_csh_trnsfr[[4]]
    ), 
    tab_spanner = c("(1)","(2)","(3)","(4)")
  ) 
  
  ## Productive inclusion
  tbl_pi <- tbl_merge(
    tbls = list(tbl_list_pi[[1]],# First model
                tbl_list_pi[[2]], # Second model
                tbl_list_pi[[3]],
                tbl_list_pi[[4]]
    ), 
    tab_spanner = c("(1)","(2)","(3)","(4)")
  )
  
  ## Pool Productive inclusion
  tbl_pi_pool <- tbl_merge(
    tbls = list(tbl_list_pi_pool[[1]],# First model
                tbl_list_pi_pool[[2]], # Second model
                tbl_list_pi_pool[[3]],
                tbl_list_pi_pool[[4]]
    ),  
    tab_spanner = c("(1)","(2)","(3)","(4)")
  )
  
  # Create a list of stacked tables combining Cash Transfer and Productive Inclusion models
  # Each list element combines the corresponding models from both treatment types
  tbl <- tbl_stack(list(tbl_csh_trnsfr,  tbl_pi, tbl_pi_pool)) #tbl_pi,
  
  # Modify table body to customize variable labels
  tbl <- tbl %>%
    modify_table_body(~ .x %>%
                        mutate(label = case_when(
                          # Rename specific treatment variables
                          term_1 == "treatment_piCapital" ~ "Capital",
                          term_1 == "treatment_piPsychosocial" ~ "Psychosocial",
                          term_1 == "treatment_piFull" ~ "Full",
                          term_1 == "treatment_pi_poolPool" ~ "Pool",
                          
                          # Label cash transfer and productive inclusion treatments
                          variable == "treatment_csh_trnsfr" & label=="Tekavoul program IE treatment group" ~ "Tekavoul",
                          
                          variable == "treatment_pi" & label=="PI IE treatment group" ~ "PI",
                          variable == "treatment_pi_pool" & label=="treatment_pi_pool" ~ "PI (Pool)",

                          TRUE ~ label  # Keep original labels for other variables
                        )))
  
  
  additional_rows <- tribble(
    ~tbl_id1, ~variable, ~var_label, ~label,
    ~estimate_1, ~estimate_2, ~estimate_3, ~estimate_4,
    ~stars_1, ~stars_2, ~stars_3, ~stars_4, 
    
    # Row 1: Control mean
    4, "Control mean @ follow up", "Control mean @ follow up", "Control mean @ follow up",
    mean_values[1], mean_values[2], mean_values[3], mean_values[4], 
    "", "", "", "",
    
    # Row 2: Control SD
    4, "Control SD @ follow up", "Control SD @ follow up", "Control SD @ follow up",
    sd_values[1], sd_values[2], sd_values[3], sd_values[4],
    "", "", "", ""
  )
  # browser()
  # Append the additional rows to the table body
  tbl$table_body <- bind_rows(tbl$table_body, additional_rows)
  
  # Title for each index
  tbl_ipv_title = paste0("**Table : effect of the programs by treatment**")
  
  # Final table formatting
  tbl <- tbl %>%
    # Set table caption
    modify_caption(tbl_ipv_title) %>%
    # Modify header for the label column
    modify_header(label = "**Outcome**") %>%
    # Remove all existing footnotes
    modify_footnote(everything() ~ NA) %>%
    modify_footnote(all_stat_cols() ~ NA) %>%
    # Add custom footnote with methodological details
    modify_footnote(label = "Notes: Results presented are OLS estimates that include controls for randomization strata (commune) and, where possible, baseline outcomes.  We control for social promotion intervention. Enumerator fixed effects are included in all regression. We estimate the regressions for the productive beneficiaries aged 18-49 only. Robust standard errors are shown in parentheses, clustered at the village proxy level. *** p < 0.01, ** p < 0.05, * p < 0.1.")
  
  # Print the final table
  tbl
  
}


getTable6_het <- function(dec_making, mainResults_mech_hh){
  
  mainResults_mech_ben_curr <- mainResults_mech_ben
  
  
  ## Cash transfer regression models
  ### Extract specific models for cash transfer from the main results
  mainResults_mech_ben_curr_csh <- mainResults_mech_ben_curr %>% filter(treat_var=="treatment_csh_trnsfr")
  
  m10 <- mainResults_mech_ben_curr_csh$results_base[[1]]  # First cash transfer model
  m11 <- mainResults_mech_ben_curr_csh$results_base[[2]]  # Second cash transfer model
  m12 <- mainResults_mech_ben_curr_csh$results_base[[3]]  #  cash transfer model
  m13 <- mainResults_mech_ben_curr_csh$results_base[[4]]  #  cash transfer model
  m14 <- mainResults_mech_ben_curr_csh$results_base[[5]]  #  cash transfer model
  m15 <- mainResults_mech_ben_curr_csh$results_base[[6]]  #  cash transfer model
  m16 <- mainResults_mech_ben_curr_csh$results_base[[7]]  # Second cash transfer model
  m17 <- mainResults_mech_ben_curr_csh$results_base[[8]]  #  cash transfer model
  m18 <- mainResults_mech_ben_curr_csh$results_base[[9]]  #  cash transfer model
  m19 <- mainResults_mech_ben_curr_csh$results_base[[10]]  #  cash transfer model
  m110 <- mainResults_mech_ben_curr_csh$results_base[[11]]  #  cash transfer model
  m111 <- mainResults_mech_ben_curr_csh$results_base[[12]]  #  cash transfer model
  m112 <- mainResults_mech_ben_curr_csh$results_base[[13]]  #  cash transfer model
  m113 <- mainResults_mech_ben_curr_csh$results_base[[14]]  #  cash transfer model
  m114 <- mainResults_mech_ben_curr_csh$results_base[[15]]  #  cash transfer model
  m115 <- mainResults_mech_ben_curr_csh$results_base[[16]]  #  cash transfer model
  m116 <- mainResults_mech_ben_curr_csh$results_base[[17]]  #  cash transfer model
  m117 <- mainResults_mech_ben_curr_csh$results_base[[18]]  #  cash transfer model
  m118 <- mainResults_mech_ben_curr_csh$results_base[[19]]  #  cash transfer model
  
  ## Productive inclusion regression models
  ### Extract specific models for productive inclusion from the main results
  mainResults_mech_ben_curr_pi <- mainResults_mech_ben_curr %>% filter(treat_var=="treatment_pi")
  
  m20 <- mainResults_mech_ben_curr_pi$results_base[[1]]  # First productive inclusion model
  m21 <- mainResults_mech_ben_curr_pi$results_base[[2]]  # Second productive inclusion model
  m22 <- mainResults_mech_ben_curr_pi$results_base[[3]]  #  productive inclusion model
  m23 <- mainResults_mech_ben_curr_pi$results_base[[4]]  #  productive inclusion model
  m24 <- mainResults_mech_ben_curr_pi$results_base[[5]]  #  productive inclusion model
  m25 <- mainResults_mech_ben_curr_pi$results_base[[6]]  #  productive inclusion model
  m26 <- mainResults_mech_ben_curr_pi$results_base[[7]]  #  productive inclusion model
  m27 <- mainResults_mech_ben_curr_pi$results_base[[8]]  #  productive inclusion model
  m28 <- mainResults_mech_ben_curr_pi$results_base[[9]]  #  productive inclusion model
  m29 <- mainResults_mech_ben_curr_pi$results_base[[10]]  #  productive inclusion model
  m210 <- mainResults_mech_ben_curr_pi$results_base[[11]]  #  productive inclusion model
  m211 <- mainResults_mech_ben_curr_pi$results_base[[12]]  #  productive inclusion model
  m212 <- mainResults_mech_ben_curr_pi$results_base[[13]]  #  productive inclusion model
  m213 <- mainResults_mech_ben_curr_pi$results_base[[14]]  #  productive inclusion model
  m214 <- mainResults_mech_ben_curr_pi$results_base[[15]]  #  productive inclusion model
  m215 <- mainResults_mech_ben_curr_pi$results_base[[16]]  #  productive inclusion model
  m216 <- mainResults_mech_ben_curr_pi$results_base[[17]]  #  productive inclusion model
  m217 <- mainResults_mech_ben_curr_pi$results_base[[18]]  #  productive inclusion model
  m218 <- mainResults_mech_ben_curr_pi$results_base[[19]]  #  productive inclusion model
  
  ### Extract specific models for productive inclusion (pool) from the main results
  mainResults_mech_ben_curr_pool <- mainResults_mech_ben_curr %>% filter(treat_var=="treatment_pi_pool")
  
  m30 <- mainResults_mech_ben_curr_pool$results_base[[1]]  # First pool productive inclusion model
  m31 <- mainResults_mech_ben_curr_pool$results_base[[2]]  # Second pool productive inclusion model
  m32 <- mainResults_mech_ben_curr_pool$results_base[[3]]  #  pool productive inclusion model
  m33 <- mainResults_mech_ben_curr_pool$results_base[[4]]  #  pool productive inclusion model
  m34 <- mainResults_mech_ben_curr_pool$results_base[[5]]  #  pool productive inclusion model
  m35 <- mainResults_mech_ben_curr_pool$results_base[[6]]  #  pool productive inclusion model
  m36 <- mainResults_mech_ben_curr_pool$results_base[[7]]  #  pool productive inclusion model
  m37 <- mainResults_mech_ben_curr_pool$results_base[[8]]  #  pool productive inclusion model
  m38 <- mainResults_mech_ben_curr_pool$results_base[[9]]  #  pool productive inclusion model
  m39 <- mainResults_mech_ben_curr_pool$results_base[[10]]  #  pool productive inclusion model
  m310 <- mainResults_mech_ben_curr_pool$results_base[[11]]  #  pool productive inclusion model
  m311 <- mainResults_mech_ben_curr_pool$results_base[[12]]  #  pool productive inclusion model
  m312 <- mainResults_mech_ben_curr_pool$results_base[[13]]  #  pool productive inclusion model
  m313 <- mainResults_mech_ben_curr_pool$results_base[[14]]  #  pool productive inclusion model
  m314 <- mainResults_mech_ben_curr_pool$results_base[[15]]  #  pool productive inclusion model
  m315 <- mainResults_mech_ben_curr_pool$results_base[[16]]  #  pool productive inclusion model
  m316 <- mainResults_mech_ben_curr_pool$results_base[[17]]  #  pool productive inclusion model
  m317 <- mainResults_mech_ben_curr_pool$results_base[[18]]  #  pool productive inclusion model
  m318 <- mainResults_mech_ben_curr_pool$results_base[[19]]  #  pool productive inclusion model
  
  # Initialize empty vectors to store descriptive statistics
  # These will store mean and standard deviation for each outcome variable
  mean_values <- c()  # Vector to store mean values
  sd_values <- c()   # Vector to store standard deviation values
  
  # Calculate descriptive statistics for each outcome variable
  # Loop through the list of outcome variables
  for (depvar in time_use_int) {
    # Calculate and round mean, ignoring NA values
    mean_values <- c(mean_values,  round(mean(followup_MRT_hh_control[[depvar]], na.rm = TRUE), 3))
    
    # Calculate and round standard deviation, ignoring NA values
    sd_values <- c(sd_values,  round(sd(followup_MRT_hh_control[[depvar]], na.rm = TRUE), 3))
  }
  
  # Create lists of models for easier processing
  models_pi <- list(m20, m21, m22, m23, m24, m25, m26, m27, m28, m29, m210, m211, m212, m213, m214, m215, m216, m217, m218)  # Productive inclusion models
  models_pi_pool <- list(m30, m31, m32, m33, m34, m35, m36, m37, m38, m39, m310, m311, m312, m313, m314, m315, m316, m317, m318)  # Productive inclusion models pool
  models_csh_trnsfr <- list(m10, m11, m12, m13, m14, m15, m16, m17, m18, m19, m110, m111, m112, m113, m114, m115, m116, m117, m118)  # Cash transfer models
  
  
  
  
  # Create regression tables for Productive Inclusion models
  tbl_list_pi <- map(models_pi, ~ tbl_regression(.x, 
                                                 exponentiate = FALSE,  # Keep coefficients as-is 
                                                 estimate_fun = ~ style_number(.x, digits = 2),
                                                 include=c(                                 # Retain main and interaction terms
                                                   "treatment_pi"))  %>% 
                       add_significance_stars(pattern = "{estimate}{stars} \n ({std.error})",
                                              hide_se = TRUE) %>%
                       add_glance_table(include = c(nobs, r.squared)))
  
  tbl_list_pi_pool <- map(models_pi_pool, ~ tbl_regression(.x, 
                                                           exponentiate = FALSE,  # Keep coefficients as-is 
                                                           estimate_fun = ~ style_number(.x, digits = 2),
                                                           include=c(                                 # Keep main pooled treatment and its interaction
                                                             "treatment_pi_pool"))  %>% 
                            add_significance_stars(pattern = "{estimate}{stars} \n ({std.error})",
                                                   hide_se = TRUE)%>%
                            add_glance_table(include = c(nobs, r.squared)))
  
  # Create regression tables for Cash Transfer models
  tbl_list_csh_trnsfr <- map(models_csh_trnsfr, ~ tbl_regression(.x, 
                                                                 exponentiate = FALSE,  # Keep coefficients as-is
                                                                 estimate_fun = ~ style_number(.x, digits = 2),
                                                                 include= c(                                 # Coefficients to retain
                                                                   "treatment_csh_trnsfr" 
                                                                   #  paste0(curr_het_var, ":treatment_csh_trnsfr")
                                                                 ))  %>% 
                               add_significance_stars(pattern = "{estimate}{stars} \n ({std.error})",
                                                      hide_se = TRUE)%>%
                               add_glance_table(include = c(nobs, r.squared)))
  
  # Modify headers for the Productive Inclusion and Cash Transfer models
  
  tbl_list_pi[[1]] <- tbl_list_pi[[1]] %>%
    modify_header(estimate = "Mins in \n off-farm \n business")
  
  tbl_list_csh_trnsfr[[1]] <- tbl_list_csh_trnsfr[[1]] %>%
    modify_header(estimate = "Mins in \n off-farm \n business")
  
  tbl_list_pi_pool[[1]] <- tbl_list_pi_pool[[1]] %>%
    modify_header(estimate = "Mins in \n off-farm \n business")
  
  ### 
  tbl_list_pi[[2]] <- tbl_list_pi[[2]] %>%
    modify_header(estimate = "Mins spent \n retrieving \n water")
  
  tbl_list_csh_trnsfr[[2]] <- tbl_list_csh_trnsfr[[2]] %>%
    modify_header(estimate = "Mins spent \n retrieving \n water")
  
  tbl_list_pi_pool[[2]] <- tbl_list_pi_pool[[2]] %>%
    modify_header(estimate = "Mins spent \n retrieving \n water")
  
  ### partner
  tbl_list_pi[[3]] <- tbl_list_pi[[3]] %>%
    modify_header(estimate = "Mins spent \n cooking")
  
  tbl_list_csh_trnsfr[[3]] <- tbl_list_csh_trnsfr[[3]] %>%
    modify_header(estimate = "Mins spent \n cooking")
  
  tbl_list_pi_pool[[3]] <- tbl_list_pi_pool[[3]] %>%
    modify_header(estimate = "Mins spent \n cooking")
  
  
  ## Business rev
  ### hh
  tbl_list_pi[[4]] <- tbl_list_pi[[4]] %>%
    modify_header(estimate = "Mins spent \n agriculture")
  
  tbl_list_csh_trnsfr[[4]] <- tbl_list_csh_trnsfr[[4]] %>%
    modify_header(estimate = "Mins spent \n agriculture")
  
  tbl_list_pi_pool[[4]] <- tbl_list_pi_pool[[4]] %>%
    modify_header(estimate = "Mins spent \n agriculture")
  
  ### ben
  tbl_list_pi[[5]] <- tbl_list_pi[[5]] %>%
    modify_header(estimate = "Mins spent \n gathering \n firewood")
  
  tbl_list_csh_trnsfr[[5]] <- tbl_list_csh_trnsfr[[5]] %>%
    modify_header(estimate =  "Mins spent \n gathering \n firewood")
  
  tbl_list_pi_pool[[5]] <- tbl_list_pi_pool[[5]] %>%
    modify_header(estimate =  "Mins spent \n gathering \n firewood")
  
  ### partner
  tbl_list_pi[[6]] <- tbl_list_pi[[6]] %>%
    modify_header(estimate = "Mins spent \n cleaning")
  
  tbl_list_csh_trnsfr[[6]] <- tbl_list_csh_trnsfr[[6]] %>%
    modify_header(estimate = "Mins spent \n cleaning")
  
  tbl_list_pi_pool[[6]] <- tbl_list_pi_pool[[6]] %>%
    modify_header(estimate = "Mins spent \n cleaning")
  
  ## Wage rev
  ### hh
  tbl_list_pi[[7]] <- tbl_list_pi[[7]] %>%
    modify_header(estimate = "Mins \n studying for \n Koranic school")
  
  tbl_list_csh_trnsfr[[7]] <- tbl_list_csh_trnsfr[[7]] %>%
    modify_header(estimate = "Mins \n studying for \n Koranic school")
  
  tbl_list_pi_pool[[7]] <- tbl_list_pi_pool[[7]] %>%
    modify_header(estimate = "Mins \n studying for \n Koranic school")
  
  ### ben
  tbl_list_pi[[8]] <- tbl_list_pi[[8]] %>%
    modify_header(estimate = "Mins spent \n doing \n laundry")
  
  tbl_list_csh_trnsfr[[8]] <- tbl_list_csh_trnsfr[[8]] %>%
    modify_header(estimate = "Mins spent \n doing \n laundry")
  
  tbl_list_pi_pool[[8]] <- tbl_list_pi_pool[[8]] %>%
    modify_header(estimate = "Mins spent \n doing \n laundry")
  
  ### part
  tbl_list_pi[[9]] <- tbl_list_pi[[9]] %>%
    modify_header(estimate = "Mins in \n livestock")
  
  tbl_list_csh_trnsfr[[9]] <- tbl_list_csh_trnsfr[[9]] %>%
    modify_header(estimate = "Mins in \n livestock")
  
  tbl_list_pi_pool[[9]] <- tbl_list_pi_pool[[9]] %>%
    modify_header(estimate = "Mins in \n livestock")
  
  ## Livestock
  ### hh
  tbl_list_pi[[10]] <- tbl_list_pi[[10]] %>%
    modify_header(estimate = "Mins \n studying for \n traditional school")
  
  tbl_list_csh_trnsfr[[10]] <- tbl_list_csh_trnsfr[[10]] %>%
    modify_header(estimate = "Mins \n studying for \n traditional school")
  
  tbl_list_pi_pool[[10]] <- tbl_list_pi_pool[[10]] %>%
    modify_header(estimate = "Mins \n studying for \n traditional school")
  
  ### ben
  tbl_list_pi[[11]] <- tbl_list_pi[[11]] %>%
    modify_header(estimate = "Mins spent \n shopping")
  
  tbl_list_csh_trnsfr[[11]] <- tbl_list_csh_trnsfr[[11]] %>%
    modify_header(estimate = "Mins spent \n shopping")
  
  tbl_list_pi_pool[[11]] <- tbl_list_pi_pool[[11]] %>%
    modify_header(estimate = "Mins spent \n shopping")
  
  ### prt
  tbl_list_pi[[12]] <- tbl_list_pi[[12]] %>%
    modify_header(estimate = "Mins spent \n child care")
  
  tbl_list_csh_trnsfr[[12]] <- tbl_list_csh_trnsfr[[12]] %>%
    modify_header(estimate = "Mins spent \n child care")
  
  tbl_list_pi_pool[[12]] <- tbl_list_pi_pool[[12]] %>%
    modify_header(estimate = "Mins spent \n child care")
  
  tbl_list_pi[[13]] <- tbl_list_pi[[13]] %>%
    modify_header(estimate = "Mins \n helping handicapped \n relatives")
  
  tbl_list_csh_trnsfr[[13]] <- tbl_list_csh_trnsfr[[13]] %>%
    modify_header(estimate = "Mins \n helping handicapped \n relatives")
  
  tbl_list_pi_pool[[13]] <- tbl_list_pi_pool[[13]] %>%
    modify_header(estimate = "Mins \n helping handicapped \n relatives")
  
  tbl_list_pi[[14]] <- tbl_list_pi[[14]] %>%
    modify_header(estimate = "Mins spent \n with friends")
  
  tbl_list_csh_trnsfr[[14]] <- tbl_list_csh_trnsfr[[14]] %>%
    modify_header(estimate = "Mins spent \n with friends")
  
  tbl_list_pi_pool[[14]] <- tbl_list_pi_pool[[14]] %>%
    modify_header(estimate = "Mins spent \n with friends")
  
  tbl_list_pi[[15]] <- tbl_list_pi[[15]] %>%
    modify_header(estimate = "Mins spent \n listening radio")
  
  tbl_list_csh_trnsfr[[15]] <- tbl_list_csh_trnsfr[[15]] %>%
    modify_header(estimate = "Mins spent \n listening radio")
  
  tbl_list_pi_pool[[15]] <- tbl_list_pi_pool[[15]] %>%
    modify_header(estimate = "Mins spent \n listening radio")
  
  tbl_list_pi[[16]] <- tbl_list_pi[[16]] %>%
    modify_header(estimate = "Mins spent \n resting")
  
  tbl_list_csh_trnsfr[[16]] <- tbl_list_csh_trnsfr[[16]] %>%
    modify_header(estimate = "Mins spent \n resting")
  
  tbl_list_pi_pool[[16]] <- tbl_list_pi_pool[[16]] %>%
    modify_header(estimate = "Mins spent \n resting")
  
  tbl_list_pi[[17]] <- tbl_list_pi[[17]] %>%
    modify_header(estimate = "Mins \n studying for \n traditional school")
  
  tbl_list_csh_trnsfr[[17]] <- tbl_list_csh_trnsfr[[17]] %>%
    modify_header(estimate = "Mins \n studying for \n traditional school")
  
  tbl_list_pi_pool[[17]] <- tbl_list_pi_pool[[17]] %>%
    modify_header(estimate = "Mins \n studying for \n traditional school")
  
  tbl_list_pi[[18]] <- tbl_list_pi[[18]] %>%
    modify_header(estimate = "Mins spent \n shopping")
  
  tbl_list_csh_trnsfr[[18]] <- tbl_list_csh_trnsfr[[18]] %>%
    modify_header(estimate = "Mins spent \n shopping")
  
  tbl_list_pi_pool[[18]] <- tbl_list_pi_pool[[18]] %>%
    modify_header(estimate = "Mins spent \n shopping")
  
  tbl_list_pi[[19]] <- tbl_list_pi[[19]] %>%
    modify_header(estimate = "Mins spent \n praying")
  
  tbl_list_csh_trnsfr[[19]] <- tbl_list_csh_trnsfr[[19]] %>%
    modify_header(estimate = "Mins spent \n praying")
  
  tbl_list_pi_pool[[19]] <- tbl_list_pi_pool[[19]] %>%
    modify_header(estimate = "Mins spent \n praying")
  
  # Merge the stacked tables into a single table with column headers
  ## Cash transfert
  tbl_csh_trnsfr <- tbl_merge(
    tbls = list(tbl_list_csh_trnsfr[[1]],# First model
                tbl_list_csh_trnsfr[[2]], # Second model
                tbl_list_csh_trnsfr[[3]], 
                tbl_list_csh_trnsfr[[4]],
                tbl_list_csh_trnsfr[[5]], 
                tbl_list_csh_trnsfr[[6]], 
                tbl_list_csh_trnsfr[[7]], 
                tbl_list_csh_trnsfr[[8]],
                tbl_list_csh_trnsfr[[9]],
                tbl_list_csh_trnsfr[[10]],
                tbl_list_csh_trnsfr[[11]],
                tbl_list_csh_trnsfr[[12]],
                tbl_list_csh_trnsfr[[13]],
                tbl_list_csh_trnsfr[[14]],
                tbl_list_csh_trnsfr[[15]],
                tbl_list_csh_trnsfr[[16]],
                tbl_list_csh_trnsfr[[17]],
                tbl_list_csh_trnsfr[[18]],
                tbl_list_csh_trnsfr[[19]]
    ), 
    tab_spanner = c("(1)","(2)","(3)","(4)","(5)","(6)", "(7)","(8)","(9)","(10)","(11)","(12)","(13)","(14)","(15)","(16)","(17)","(18)","(19)")
  ) 
  
  ## Productive inclusion
  tbl_pi <- tbl_merge(
    tbls = list(tbl_list_pi[[1]],# First model
                tbl_list_pi[[2]], # Second model
                tbl_list_pi[[3]], 
                tbl_list_pi[[4]],
                tbl_list_pi[[5]], 
                tbl_list_pi[[6]], 
                tbl_list_pi[[7]], 
                tbl_list_pi[[8]],
                tbl_list_pi[[9]],
                tbl_list_pi[[10]],
                tbl_list_pi[[11]],
                tbl_list_pi[[12]], 
                tbl_list_pi[[13]],
                tbl_list_pi[[14]],
                tbl_list_pi[[15]],
                tbl_list_pi[[16]],
                tbl_list_pi[[17]],
                tbl_list_pi[[18]],
                tbl_list_pi[[19]]
    ), 
    tab_spanner = c("(1)","(2)","(3)","(4)","(5)","(6)", "(7)","(8)","(9)","(10)","(11)","(12)","(13)","(14)","(15)","(16)","(17)","(18)","(19)")
  )
  
  ## Pool Productive inclusion
  tbl_pi_pool <- tbl_merge(
    tbls = list(tbl_list_pi_pool[[1]],# First model
                tbl_list_pi_pool[[2]], # Second model
                tbl_list_pi_pool[[3]], 
                tbl_list_pi_pool[[4]],
                tbl_list_pi_pool[[5]], 
                tbl_list_pi_pool[[6]], 
                tbl_list_pi_pool[[7]], 
                tbl_list_pi_pool[[8]],
                tbl_list_pi_pool[[9]],
                tbl_list_pi_pool[[10]],
                tbl_list_pi_pool[[11]],
                tbl_list_pi_pool[[12]], 
                tbl_list_pi_pool[[13]],
                tbl_list_pi_pool[[14]],
                tbl_list_pi_pool[[15]],
                tbl_list_pi_pool[[16]],
                tbl_list_pi_pool[[17]],
                tbl_list_pi_pool[[18]],
                tbl_list_pi_pool[[19]]  
    ),  
    tab_spanner = c("(1)","(2)","(3)","(4)","(5)","(6)", "(7)","(8)","(9)","(10)","(11)","(12)","(13)","(14)","(15)","(16)","(17)","(18)","(19)")
  )
  
  # Create a list of stacked tables combining Cash Transfer and Productive Inclusion models
  # Each list element combines the corresponding models from both treatment types
  tbl <- tbl_stack(list(tbl_csh_trnsfr,  tbl_pi, tbl_pi_pool)) #tbl_pi,
  
  # Modify table body to customize variable labels
  tbl <- tbl %>%
    modify_table_body(~ .x %>%
                        mutate(label = case_when(
                          # Rename specific treatment variables
                          term_1 == "treatment_piCapital" ~ "Capital",
                          term_1 == "treatment_piPsychosocial" ~ "Psychosocial",
                          term_1 == "treatment_piFull" ~ "Full",
                          term_1 == "treatment_pi_poolPool" ~ "Pool",
                          
                          # Label cash transfer and productive inclusion treatments
                          variable == "treatment_csh_trnsfr" & label=="Tekavoul program IE treatment group" ~ "Tekavoul",
                          
                          variable == "treatment_pi" & label=="PI IE treatment group" ~ "PI",
                          variable == "treatment_pi_pool" & label=="treatment_pi_pool" ~ "PI (Pool)",

                          TRUE ~ label  # Keep original labels for other variables
                        )))
  
  
  additional_rows <- tribble(
    ~tbl_id1, ~variable, ~var_label, ~label,
    ~estimate_1, ~estimate_2, ~estimate_3, ~estimate_4, ~estimate_5,
    ~estimate_6, ~estimate_7, ~estimate_8, ~estimate_9, ~estimate_10,
    ~estimate_11, ~estimate_12,~estimate_13, ~estimate_14,~estimate_15,
    ~estimate_16, ~estimate_17,~estimate_18, ~estimate_19,
    ~stars_1, ~stars_2, ~stars_3, ~stars_4, ~stars_5,
    ~stars_6, ~stars_7, ~stars_8, ~stars_9, ~stars_10,~stars_11, ~stars_12,
    ~stars_13,~stars_14, ~stars_15,~stars_16,~stars_17,~stars_18,~stars_19,
    
    # Row 1: Control mean
    4, "Control mean @ follow up", "Control mean @ follow up", "Control mean @ follow up",
    mean_values[1], mean_values[2], mean_values[3], mean_values[4], mean_values[5],
    mean_values[6], mean_values[7], mean_values[8], mean_values[9], mean_values[10], 
    mean_values[11], mean_values[12],mean_values[13], mean_values[14],mean_values[15],
    mean_values[16], mean_values[17], mean_values[18], mean_values[19],
    "", "", "", "", "", "", "", "", "", "",
    "", "", "", "", "","", "","", "",
    
    # Row 2: Control SD
    4, "Control SD @ follow up", "Control SD @ follow up", "Control SD @ follow up",
    sd_values[1], sd_values[2], sd_values[3], sd_values[4], sd_values[5],
    sd_values[6], sd_values[7], sd_values[8], sd_values[9], sd_values[10],
    sd_values[11], sd_values[12],sd_values[13], sd_values[14],
    sd_values[15], sd_values[16],sd_values[17], sd_values[18],sd_values[19], 
    "", "", "", "", "", "", "", "", "", "",
    "", "", "", "", "","", "","", ""
  )
  # Append the additional rows to the table body
  tbl$table_body <- bind_rows(tbl$table_body, additional_rows)
  
  # Title for each index
  tbl_ipv_title = paste0("**Table : effect of the programs by treatment**")
  
  # Final table formatting
  tbl <- tbl %>%
    # Set table caption
    modify_caption(tbl_ipv_title) %>%
    # Modify header for the label column
    modify_header(label = "**Outcome**") %>%
    # Remove all existing footnotes
    modify_footnote(everything() ~ NA) %>%
    modify_footnote(all_stat_cols() ~ NA) %>%
    # Add custom footnote with methodological details
    modify_footnote(label = "Notes: Results presented are OLS estimates that include controls for randomization strata (commune) and, where possible, baseline outcomes.  We control for social promotion intervention. Enumerator fixed effects are included in all regression. We estimate the regressions for the productive beneficiaries aged 18-49 only. Robust standard errors are shown in parentheses, clustered at the village proxy level. *** p < 0.01, ** p < 0.05, * p < 0.1.")
  
  # Print the final table
  tbl
  
}





getTable4_het <- function(dec_making, mainResults_mech_hh){
  
  mainResults_mech_ben_curr <- mainResults_mech_ben
  
  
  ## Cash transfer regression models
  ### Extract specific models for cash transfer from the main results
  mainResults_mech_ben_curr_csh <- mainResults_mech_ben_curr %>% filter(treat_var=="treatment_csh_trnsfr")
  
  m10 <- mainResults_mech_ben_curr_csh$results_base[[1]]  # First cash transfer model
  m11 <- mainResults_mech_ben_curr_csh$results_base[[2]]  # Second cash transfer model
  m12 <- mainResults_mech_ben_curr_csh$results_base[[3]]  #  cash transfer model
  m13 <- mainResults_mech_ben_curr_csh$results_base[[4]]  #  cash transfer model
  m14 <- mainResults_mech_ben_curr_csh$results_base[[5]]  #  cash transfer model
  m15 <- mainResults_mech_ben_curr_csh$results_base[[6]]  #  cash transfer model
  m16 <- mainResults_mech_ben_curr_csh$results_base[[7]]  # Second cash transfer model
  m17 <- mainResults_mech_ben_curr_csh$results_base[[8]]  #  cash transfer model
  m18 <- mainResults_mech_ben_curr_csh$results_base[[9]]  #  cash transfer model
  m19 <- mainResults_mech_ben_curr_csh$results_base[[10]]  #  cash transfer model
  m110 <- mainResults_mech_ben_curr_csh$results_base[[11]]  #  cash transfer model
  m111 <- mainResults_mech_ben_curr_csh$results_base[[12]]  #  cash transfer model
  m112 <- mainResults_mech_ben_curr_csh$results_base[[13]]  #  cash transfer model
  m113 <- mainResults_mech_ben_curr_csh$results_base[[14]]  #  cash transfer model
  m114 <- mainResults_mech_ben_curr_csh$results_base[[15]]  #  cash transfer model
  m115 <- mainResults_mech_ben_curr_csh$results_base[[16]]  #  cash transfer model
  m116 <- mainResults_mech_ben_curr_csh$results_base[[17]]  #  cash transfer model
  
  ## Productive inclusion regression models
  ### Extract specific models for productive inclusion from the main results
  mainResults_mech_ben_curr_pi <- mainResults_mech_ben_curr %>% filter(treat_var=="treatment_pi")
  
  m20 <- mainResults_mech_ben_curr_pi$results_base[[1]]  # First productive inclusion model
  m21 <- mainResults_mech_ben_curr_pi$results_base[[2]]  # Second productive inclusion model
  m22 <- mainResults_mech_ben_curr_pi$results_base[[3]]  #  productive inclusion model
  m23 <- mainResults_mech_ben_curr_pi$results_base[[4]]  #  productive inclusion model
  m24 <- mainResults_mech_ben_curr_pi$results_base[[5]]  #  productive inclusion model
  m25 <- mainResults_mech_ben_curr_pi$results_base[[6]]  #  productive inclusion model
  m26 <- mainResults_mech_ben_curr_pi$results_base[[7]]  #  productive inclusion model
  m27 <- mainResults_mech_ben_curr_pi$results_base[[8]]  #  productive inclusion model
  m28 <- mainResults_mech_ben_curr_pi$results_base[[9]]  #  productive inclusion model
  m29 <- mainResults_mech_ben_curr_pi$results_base[[10]]  #  productive inclusion model
  m210 <- mainResults_mech_ben_curr_pi$results_base[[11]]  #  productive inclusion model
  m211 <- mainResults_mech_ben_curr_pi$results_base[[12]]  #  productive inclusion model
  m212 <- mainResults_mech_ben_curr_pi$results_base[[13]]  #  productive inclusion model
  m213 <- mainResults_mech_ben_curr_pi$results_base[[14]]  #  productive inclusion model
  m214 <- mainResults_mech_ben_curr_pi$results_base[[15]]  #  productive inclusion model
  m215 <- mainResults_mech_ben_curr_pi$results_base[[16]]  #  productive inclusion model
  m216 <- mainResults_mech_ben_curr_pi$results_base[[17]]  #  productive inclusion model
  
  
  ### Extract specific models for productive inclusion (pool) from the main results
  mainResults_mech_ben_curr_pool <- mainResults_mech_ben_curr %>% filter(treat_var=="treatment_pi_pool")
  
  m30 <- mainResults_mech_ben_curr_pool$results_base[[1]]  # First pool productive inclusion model
  m31 <- mainResults_mech_ben_curr_pool$results_base[[2]]  # Second pool productive inclusion model
  m32 <- mainResults_mech_ben_curr_pool$results_base[[3]]  #  pool productive inclusion model
  m33 <- mainResults_mech_ben_curr_pool$results_base[[4]]  #  pool productive inclusion model
  m34 <- mainResults_mech_ben_curr_pool$results_base[[5]]  #  pool productive inclusion model
  m35 <- mainResults_mech_ben_curr_pool$results_base[[6]]  #  pool productive inclusion model
  m36 <- mainResults_mech_ben_curr_pool$results_base[[7]]  #  pool productive inclusion model
  m37 <- mainResults_mech_ben_curr_pool$results_base[[8]]  #  pool productive inclusion model
  m38 <- mainResults_mech_ben_curr_pool$results_base[[9]]  #  pool productive inclusion model
  m39 <- mainResults_mech_ben_curr_pool$results_base[[10]]  #  pool productive inclusion model
  m310 <- mainResults_mech_ben_curr_pool$results_base[[11]]  #  pool productive inclusion model
  m311 <- mainResults_mech_ben_curr_pool$results_base[[12]]  #  pool productive inclusion model
  m312 <- mainResults_mech_ben_curr_pool$results_base[[13]]  #  pool productive inclusion model
  m313 <- mainResults_mech_ben_curr_pool$results_base[[14]]  #  pool productive inclusion model
  m314 <- mainResults_mech_ben_curr_pool$results_base[[15]]  #  pool productive inclusion model
  m315 <- mainResults_mech_ben_curr_pool$results_base[[16]]  #  pool productive inclusion model
  m316 <- mainResults_mech_ben_curr_pool$results_base[[17]]  #  pool productive inclusion model
  
  # Initialize empty vectors to store descriptive statistics
  # These will store mean and standard deviation for each outcome variable
  mean_values <- c()  # Vector to store mean values
  sd_values <- c()   # Vector to store standard deviation values
  
  # Calculate descriptive statistics for each outcome variable
  # Loop through the list of outcome variables
  for (depvar in dec_making) {
    # Calculate and round mean, ignoring NA values
    mean_values <- c(mean_values,  round(mean(followup_MRT_hh_control[[depvar]], na.rm = TRUE), 3))
    
    # Calculate and round standard deviation, ignoring NA values
    sd_values <- c(sd_values,  round(sd(followup_MRT_hh_control[[depvar]], na.rm = TRUE), 3))
  }
  
  # Create lists of models for easier processing
  models_pi <- list(m20, m21, m22, m23, m24, m25, m26, m27, m28, m29, m210, m211, m212, m213, m214, m215, m216)  # Productive inclusion models
  models_pi_pool <- list(m30, m31, m32, m33, m34, m35, m36, m37, m38, m39, m310, m311, m312, m313, m314, m315, m316)  # Productive inclusion models pool
  models_csh_trnsfr <- list(m10, m11, m12, m13, m14, m15, m16, m17, m18, m19, m110, m111, m112, m113, m114, m115, m116)  # Cash transfer models
  
  
  
  
  # Create regression tables for Productive Inclusion models
  tbl_list_pi <- map(models_pi, ~ tbl_regression(.x, 
                                                 exponentiate = FALSE,  # Keep coefficients as-is 
                                                 estimate_fun = ~ style_number(.x, digits = 2),
                                                 include=c(                                 # Retain main and interaction terms
                                                   "treatment_pi"))  %>% 
                       add_significance_stars(pattern = "{estimate}{stars} \n ({std.error})",
                                              hide_se = TRUE) %>%
                       add_glance_table(include = c(nobs, r.squared)))
  
  tbl_list_pi_pool <- map(models_pi_pool, ~ tbl_regression(.x, 
                                                           exponentiate = FALSE,  # Keep coefficients as-is 
                                                           estimate_fun = ~ style_number(.x, digits = 2),
                                                           include=c(                                 # Keep main pooled treatment and its interaction
                                                             "treatment_pi_pool"))  %>% 
                            add_significance_stars(pattern = "{estimate}{stars} \n ({std.error})",
                                                   hide_se = TRUE)%>%
                            add_glance_table(include = c(nobs, r.squared)))
  
  # Create regression tables for Cash Transfer models
  tbl_list_csh_trnsfr <- map(models_csh_trnsfr, ~ tbl_regression(.x, 
                                                                 exponentiate = FALSE,  # Keep coefficients as-is
                                                                 estimate_fun = ~ style_number(.x, digits = 2),
                                                                 include= c(                                 # Coefficients to retain
                                                                   "treatment_csh_trnsfr"
                                                                   
                                                                 ))  %>% 
                               add_significance_stars(pattern = "{estimate}{stars} \n ({std.error})",
                                                      hide_se = TRUE)%>%
                               add_glance_table(include = c(nobs, r.squared)))
  
  # Modify headers for the Productive Inclusion and Cash Transfer models
  ### hh
  tbl_list_pi[[1]] <- tbl_list_pi[[1]] %>%
    modify_header(estimate = "Control over \n household \n resources index")
  
  tbl_list_csh_trnsfr[[1]] <- tbl_list_csh_trnsfr[[1]] %>%
    modify_header(estimate = "Control over \n household \n resources index")
  
  tbl_list_pi_pool[[1]] <- tbl_list_pi_pool[[1]] %>%
    modify_header(estimate = "Control over \n household \n resources index")
  
  ### ben
  tbl_list_pi[[2]] <- tbl_list_pi[[2]] %>%
    modify_header(estimate = "Control over \n earnings \n index")
  
  tbl_list_csh_trnsfr[[2]] <- tbl_list_csh_trnsfr[[2]] %>%
    modify_header(estimate = "Control over \n earnings \n index")
  
  tbl_list_pi_pool[[2]] <- tbl_list_pi_pool[[2]] %>%
    modify_header(estimate = "Control over \n earnings \n index")
  
  ### partner
  tbl_list_pi[[3]] <- tbl_list_pi[[3]] %>%
    modify_header(estimate = "Own \n earnings \n influence (1-3)")
  
  tbl_list_csh_trnsfr[[3]] <- tbl_list_csh_trnsfr[[3]] %>%
    modify_header(estimate = "Own \n earnings \n influence (1-3)")
  
  tbl_list_pi_pool[[3]] <- tbl_list_pi_pool[[3]] %>%
    modify_header(estimate = "Own \n earnings \n influence (1-3)")
  
  
  ## Business rev
  ### hh
  tbl_list_pi[[4]] <- tbl_list_pi[[4]] %>%
    modify_header(estimate = "Can Decide \n to Earn \n Alone (1-3)")
  
  tbl_list_csh_trnsfr[[4]] <- tbl_list_csh_trnsfr[[4]] %>%
    modify_header(estimate = "Can Decide \n to Earn \n Alone (1-3)")
  
  tbl_list_pi_pool[[4]] <- tbl_list_pi_pool[[4]] %>%
    modify_header(estimate = "Can Decide \n to Earn \n Alone (1-3)")
  
  ### ben
  tbl_list_pi[[5]] <- tbl_list_pi[[5]] %>%
    modify_header(estimate = "Agriculture \n influence (1-3)")
  
  tbl_list_csh_trnsfr[[5]] <- tbl_list_csh_trnsfr[[5]] %>%
    modify_header(estimate =  "Agriculture \n influence (1-3)")
  
  tbl_list_pi_pool[[5]] <- tbl_list_pi_pool[[5]] %>%
    modify_header(estimate =  "Agriculture \n influence (1-3)")
  
  ### partner
  tbl_list_pi[[6]] <- tbl_list_pi[[6]] %>%
    modify_header(estimate = "Livestock \n influence (1-3)")
  
  tbl_list_csh_trnsfr[[6]] <- tbl_list_csh_trnsfr[[6]] %>%
    modify_header(estimate = "Livestock \n influence (1-3)")
  
  tbl_list_pi_pool[[6]] <- tbl_list_pi_pool[[6]] %>%
    modify_header(estimate = "Livestock \n influence (1-3)")
  
  ## Wage rev
  ### hh
  tbl_list_pi[[7]] <- tbl_list_pi[[7]] %>%
    modify_header(estimate = "Off-farm \n business \n influence (1-3)")
  
  tbl_list_csh_trnsfr[[7]] <- tbl_list_csh_trnsfr[[7]] %>%
    modify_header(estimate = "Off-farm \n business \n influence (1-3)")
  
  tbl_list_pi_pool[[7]] <- tbl_list_pi_pool[[7]] %>%
    modify_header(estimate = "Off-farm \n business \n influence (1-3)")
  
  ### ben
  tbl_list_pi[[8]] <- tbl_list_pi[[8]] %>%
    modify_header(estimate = "Daily \n spending \n influence (1-3)")
  
  tbl_list_csh_trnsfr[[8]] <- tbl_list_csh_trnsfr[[8]] %>%
    modify_header(estimate = "Daily \n spending \n influence (1-3)")
  
  tbl_list_pi_pool[[8]] <- tbl_list_pi_pool[[8]] %>%
    modify_header(estimate = "Daily \n spending \n influence (1-3)")
  
  ### part
  tbl_list_pi[[9]] <- tbl_list_pi[[9]] %>%
    modify_header(estimate = "Can Decide \n to Spend \n Alone (1-3)")
  
  tbl_list_csh_trnsfr[[9]] <- tbl_list_csh_trnsfr[[9]] %>%
    modify_header(estimate = "Can Decide \n to Spend \n Alone (1-3)")
  
  tbl_list_pi_pool[[9]] <- tbl_list_pi_pool[[9]] %>%
    modify_header(estimate = "Can Decide \n to Spend \n Alone (1-3)")
  
  ## Livestock
  ### hh
  tbl_list_pi[[10]] <- tbl_list_pi[[10]] %>%
    modify_header(estimate = "Large \n purchases \n influence (1-3)")
  
  tbl_list_csh_trnsfr[[10]] <- tbl_list_csh_trnsfr[[10]] %>%
    modify_header(estimate = "Large \n purchases \n influence (1-3)")
  
  tbl_list_pi_pool[[10]] <- tbl_list_pi_pool[[10]] %>%
    modify_header(estimate = "Large \n purchases \n influence (1-3)")
  
  ### ben
  tbl_list_pi[[11]] <- tbl_list_pi[[11]] %>%
    modify_header(estimate = "Can Decide to \n Spend Large \n Amounts Alone (1-3)")
  
  tbl_list_csh_trnsfr[[11]] <- tbl_list_csh_trnsfr[[11]] %>%
    modify_header(estimate = "Can Decide to \n Spend Large \n Amounts Alone (1-3)")
  
  tbl_list_pi_pool[[11]] <- tbl_list_pi_pool[[11]] %>%
    modify_header(estimate = "Can Decide to \n Spend Large \n Amounts Alone (1-3)")
  
  ### prt
  tbl_list_pi[[12]] <- tbl_list_pi[[12]] %>%
    modify_header(estimate = "Family \n planning \n influence (1-3)")
  
  tbl_list_csh_trnsfr[[12]] <- tbl_list_csh_trnsfr[[12]] %>%
    modify_header(estimate = "Family \n planning \n influence (1-3)")
  
  tbl_list_pi_pool[[12]] <- tbl_list_pi_pool[[12]] %>%
    modify_header(estimate = "Family \n planning \n influence (1-3)")
  
  tbl_list_pi[[13]] <- tbl_list_pi[[13]] %>%
    modify_header(estimate = "Can Make \n Fertility \n Choices Alone (1-3)")
  
  tbl_list_csh_trnsfr[[13]] <- tbl_list_csh_trnsfr[[13]] %>%
    modify_header(estimate = "Can Make \n Fertility \n Choices Alone (1-3)")
  
  tbl_list_pi_pool[[13]] <- tbl_list_pi_pool[[13]] %>%
    modify_header(estimate = "Can Make \n Fertility \n Choices Alone (1-3)")
  
  tbl_list_pi[[14]] <- tbl_list_pi[[14]] %>%
    modify_header(estimate = "Own \n healthcare \n influence (1-3)")
  
  tbl_list_csh_trnsfr[[14]] <- tbl_list_csh_trnsfr[[14]] %>%
    modify_header(estimate = "Own \n healthcare \n influence (1-3)")
  
  tbl_list_pi_pool[[14]] <- tbl_list_pi_pool[[14]] %>%
    modify_header(estimate = "Own \n healthcare \n influence (1-3)")
  
  tbl_list_pi[[15]] <- tbl_list_pi[[15]] %>%
    modify_header(estimate = "Can Decide \n about Self-Care \n Alone (1-3)")
  
  tbl_list_csh_trnsfr[[15]] <- tbl_list_csh_trnsfr[[15]] %>%
    modify_header(estimate = "Can Decide \n about Self-Care \n Alone (1-3)")
  
  tbl_list_pi_pool[[15]] <- tbl_list_pi_pool[[15]] %>%
    modify_header(estimate = "Can Decide \n about Self-Care \n Alone (1-3)")
  
  tbl_list_pi[[16]] <- tbl_list_pi[[16]] %>%
    modify_header(estimate = "Partner’s \n earnings \n influence (1-3)")
  
  tbl_list_csh_trnsfr[[16]] <- tbl_list_csh_trnsfr[[16]] %>%
    modify_header(estimate = "Partner’s \n earnings \n influence (1-3)")
  
  tbl_list_pi_pool[[16]] <- tbl_list_pi_pool[[16]] %>%
    modify_header(estimate = "Partner’s \n earnings \n influence (1-3)")
  
  tbl_list_pi[[17]] <- tbl_list_pi[[17]] %>%
    modify_header(estimate = "Child \n education \n influence (1-3)")
  
  tbl_list_csh_trnsfr[[17]] <- tbl_list_csh_trnsfr[[17]] %>%
    modify_header(estimate = "Child \n education \n influence (1-3)")
  
  tbl_list_pi_pool[[17]] <- tbl_list_pi_pool[[17]] %>%
    modify_header(estimate = "Child \n education \n influence (1-3)")
  
  # Merge the stacked tables into a single table with column headers
  ## Cash transfert
  tbl_csh_trnsfr <- tbl_merge(
    tbls = list(tbl_list_csh_trnsfr[[1]],# First model
                tbl_list_csh_trnsfr[[2]], # Second model
                tbl_list_csh_trnsfr[[3]], 
                tbl_list_csh_trnsfr[[4]],
                tbl_list_csh_trnsfr[[5]], 
                tbl_list_csh_trnsfr[[6]], 
                tbl_list_csh_trnsfr[[7]], 
                tbl_list_csh_trnsfr[[8]],
                tbl_list_csh_trnsfr[[9]],
                tbl_list_csh_trnsfr[[10]],
                tbl_list_csh_trnsfr[[11]],
                tbl_list_csh_trnsfr[[12]],
                tbl_list_csh_trnsfr[[13]],
                tbl_list_csh_trnsfr[[14]],
                tbl_list_csh_trnsfr[[15]],
                tbl_list_csh_trnsfr[[16]],
                tbl_list_csh_trnsfr[[17]]
    ), 
    tab_spanner = c("(1)","(2)","(3)","(4)","(5)","(6)", "(7)","(8)","(9)","(10)","(11)","(12)","(13)","(14)","(15)","(16)","(17)")
    
  ) 
  
  ## Productive inclusion
  tbl_pi <- tbl_merge(
    tbls = list(tbl_list_pi[[1]],# First model
                tbl_list_pi[[2]], # Second model
                tbl_list_pi[[3]], 
                tbl_list_pi[[4]],
                tbl_list_pi[[5]], 
                tbl_list_pi[[6]], 
                tbl_list_pi[[7]], 
                tbl_list_pi[[8]],
                tbl_list_pi[[9]],
                tbl_list_pi[[10]],
                tbl_list_pi[[11]],
                tbl_list_pi[[12]], 
                tbl_list_pi[[13]],
                tbl_list_pi[[14]],
                tbl_list_pi[[15]],
                tbl_list_pi[[16]],
                tbl_list_pi[[17]]
    ), 
    tab_spanner = c("(1)","(2)","(3)","(4)","(5)","(6)", "(7)","(8)","(9)","(10)","(11)","(12)","(13)","(14)","(15)","(16)","(17)")
  )
  
  ## Pool Productive inclusion
  tbl_pi_pool <- tbl_merge(
    tbls = list(tbl_list_pi_pool[[1]],# First model
                tbl_list_pi_pool[[2]], # Second model
                tbl_list_pi_pool[[3]], 
                tbl_list_pi_pool[[4]],
                tbl_list_pi_pool[[5]], 
                tbl_list_pi_pool[[6]], 
                tbl_list_pi_pool[[7]], 
                tbl_list_pi_pool[[8]],
                tbl_list_pi_pool[[9]],
                tbl_list_pi_pool[[10]],
                tbl_list_pi_pool[[11]],
                tbl_list_pi_pool[[12]], 
                tbl_list_pi_pool[[13]],
                tbl_list_pi_pool[[14]],
                tbl_list_pi_pool[[15]],
                tbl_list_pi_pool[[16]],
                tbl_list_pi_pool[[17]] 
    ),  
    tab_spanner = c("(1)","(2)","(3)","(4)","(5)","(6)", "(7)","(8)","(9)","(10)","(11)","(12)","(13)","(14)","(15)","(16)","(17)")
  )
  
  # Create a list of stacked tables combining Cash Transfer and Productive Inclusion models
  # Each list element combines the corresponding models from both treatment types
  tbl <- tbl_stack(list(tbl_csh_trnsfr,  tbl_pi, tbl_pi_pool)) #tbl_pi,
  
  # Modify table body to customize variable labels
  tbl <- tbl %>%
    modify_table_body(~ .x %>%
                        mutate(label = case_when(
                          # Rename specific treatment variables
                          term_1 == "treatment_piCapital" ~ "Capital",
                          term_1 == "treatment_piPsychosocial" ~ "Psychosocial",
                          term_1 == "treatment_piFull" ~ "Full",
                          term_1 == "treatment_pi_poolPool" ~ "Pool",
                          
                          # Label cash transfer and productive inclusion treatments
                          variable == "treatment_csh_trnsfr" & label=="Tekavoul program IE treatment group" ~ "Tekavoul",
                          
                          variable == "treatment_pi" & label=="PI IE treatment group" ~ "PI",
                          variable == "treatment_pi_pool" & label=="treatment_pi_pool" ~ "PI (Pool)",
 
                          TRUE ~ label  # Keep original labels for other variables
                        )))
  
  
  additional_rows <- tribble(
    ~tbl_id1, ~variable, ~var_label, ~label,
    ~estimate_1, ~estimate_2, ~estimate_3, ~estimate_4, ~estimate_5,
    ~estimate_6, ~estimate_7, ~estimate_8, ~estimate_9, ~estimate_10,
    ~estimate_11, ~estimate_12,~estimate_13, ~estimate_14,~estimate_15,
    ~estimate_16, ~estimate_17,
    ~stars_1, ~stars_2, ~stars_3, ~stars_4, ~stars_5,
    ~stars_6, ~stars_7, ~stars_8, ~stars_9, ~stars_10,~stars_11, ~stars_12,
    ~stars_13,~stars_14, ~stars_15,~stars_16,~stars_17,
    
    # Row 1: Control mean
    4, "Control mean @ follow up", "Control mean @ follow up", "Control mean @ follow up",
    mean_values[1], mean_values[2], mean_values[3], mean_values[4], mean_values[5],
    mean_values[6], mean_values[7], mean_values[8], mean_values[9], mean_values[10], 
    mean_values[11], mean_values[12],mean_values[13], mean_values[14],mean_values[15],
    mean_values[16], mean_values[17],
    "", "", "", "", "", "", "", "", "", "",
    "", "", "", "", "","", "",
    
    # Row 2: Control SD
    4, "Control SD @ follow up", "Control SD @ follow up", "Control SD @ follow up",
    sd_values[1], sd_values[2], sd_values[3], sd_values[4], sd_values[5],
    sd_values[6], sd_values[7], sd_values[8], sd_values[9], sd_values[10],
    sd_values[11], sd_values[12],sd_values[13], sd_values[14],
    sd_values[15], sd_values[16],sd_values[17], 
    "", "", "", "", "", "", "", "", "", "",
    "", "", "", "", "","", ""
  )
  # browser()
  # Append the additional rows to the table body
  tbl$table_body <- bind_rows(tbl$table_body, additional_rows)
  
  # Title for each index
  tbl_ipv_title = paste0("**Table : effect of the programs by treatment**")
  
  # Final table formatting
  tbl <- tbl %>%
    # Set table caption
    modify_caption(tbl_ipv_title) %>%
    # Modify header for the label column
    modify_header(label = "**Outcome**") %>%
    # Remove all existing footnotes
    modify_footnote(everything() ~ NA) %>%
    modify_footnote(all_stat_cols() ~ NA) %>%
    # Add custom footnote with methodological details
    modify_footnote(label = "Notes: Results presented are OLS estimates that include controls for randomization strata (commune) and, where possible, baseline outcomes.  We control for social promotion intervention. Enumerator fixed effects are included in all regression. We estimate the regressions for the productive beneficiaries aged 18-49 only. Robust standard errors are shown in parentheses, clustered at the village proxy level. *** p < 0.01, ** p < 0.05, * p < 0.1.")
  
  # Print the final table
  tbl
  
}


getTable3_het <- function(intra_hh_vars, mainResults_mech_hh){
  
  
  mainResults_mech_ben_curr <- mainResults_mech_ben
  
  
  ## Cash transfer regression models
  ### Extract specific models for cash transfer from the main results
  mainResults_mech_ben_curr_csh <- mainResults_mech_ben_curr %>% filter(treat_var=="treatment_csh_trnsfr")
  
  m10 <- mainResults_mech_ben_curr_csh$results_base[[1]]  # First cash transfer model
  m11 <- mainResults_mech_ben_curr_csh$results_base[[2]]  # Second cash transfer model
  m12 <- mainResults_mech_ben_curr_csh$results_base[[3]]  #  cash transfer model
  
  ## Productive inclusion regression models
  ### Extract specific models for productive inclusion from the main results
  mainResults_mech_ben_curr_pi <- mainResults_mech_ben_curr %>% filter(treat_var=="treatment_pi")
  
  m20 <- mainResults_mech_ben_curr_pi$results_base[[1]]  # First productive inclusion model
  m21 <- mainResults_mech_ben_curr_pi$results_base[[2]]  # Second productive inclusion model
  m22 <- mainResults_mech_ben_curr_pi$results_base[[3]]  #  productive inclusion model
  
  ### Extract specific models for productive inclusion (pool) from the main results
  mainResults_mech_ben_curr_pool <- mainResults_mech_ben_curr %>% filter(treat_var=="treatment_pi_pool")
  
  m30 <- mainResults_mech_ben_curr_pool$results_base[[1]]  # First pool productive inclusion model
  m31 <- mainResults_mech_ben_curr_pool$results_base[[2]]  # Second pool productive inclusion model
  m32 <- mainResults_mech_ben_curr_pool$results_base[[3]]  #  pool productive inclusion model

  
  # Initialize empty vectors to store descriptive statistics
  # These will store mean and standard deviation for each outcome variable
  mean_values <- c()  # Vector to store mean values
  sd_values <- c()   # Vector to store standard deviation values
  
  # Calculate descriptive statistics for each outcome variable
  # Loop through the list of outcome variables
  for (depvar in intra_hh_vars) {
    # Calculate and round mean, ignoring NA values
    mean_values <- c(mean_values,  round(mean(followup_MRT_hh_control[[depvar]], na.rm = TRUE), 3))
    
    # Calculate and round standard deviation, ignoring NA values
    sd_values <- c(sd_values,  round(sd(followup_MRT_hh_control[[depvar]], na.rm = TRUE), 3))
  }
  
  # Create lists of models for easier processing
  models_pi <- list(m20, m21, m22)  # Productive inclusion models
  models_pi_pool <- list(m30, m31, m32)  # Productive inclusion models pool
  models_csh_trnsfr <- list(m10, m11, m12)  # Cash transfer models
  
  
  
  
  # Create regression tables for Productive Inclusion models
  tbl_list_pi <- map(models_pi, ~ tbl_regression(.x, 
                                                 exponentiate = FALSE,  # Keep coefficients as-is 
                                                 estimate_fun = ~ style_number(.x, digits = 2),
                                                 include=c(                                 # Retain main and interaction terms
                                                   "treatment_pi"))  %>% 
                       add_significance_stars(pattern = "{estimate}{stars} \n ({std.error})",
                                              hide_se = TRUE) %>%
                       add_glance_table(include = c(nobs, r.squared)))
  
  tbl_list_pi_pool <- map(models_pi_pool, ~ tbl_regression(.x, 
                                                           exponentiate = FALSE,  # Keep coefficients as-is 
                                                           estimate_fun = ~ style_number(.x, digits = 2),
                                                           include=c(                                 # Keep main pooled treatment and its interaction
                                                             "treatment_pi_pool"))  %>% 
                            add_significance_stars(pattern = "{estimate}{stars} \n ({std.error})",
                                                   hide_se = TRUE)%>%
                            add_glance_table(include = c(nobs, r.squared)))
  
  # Create regression tables for Cash Transfer models
  tbl_list_csh_trnsfr <- map(models_csh_trnsfr, ~ tbl_regression(.x, 
                                                                 exponentiate = FALSE,  # Keep coefficients as-is
                                                                 estimate_fun = ~ style_number(.x, digits = 2),
                                                                 include= c(                                 # Coefficients to retain
                                                                   "treatment_csh_trnsfr"
                                                                   #  paste0(curr_het_var, ":treatment_csh_trnsfr")
                                                                 ))  %>% 
                               add_significance_stars(pattern = "{estimate}{stars} \n ({std.error})",
                                                      hide_se = TRUE)%>%
                               add_glance_table(include = c(nobs, r.squared)))
  
  # Modify headers for the Productive Inclusion and Cash Transfer models
  
  ## Intra-household dynamics index
  tbl_list_pi[[1]] <- tbl_list_pi[[1]] %>%
    modify_header(estimate = "Intra-household \n dynamics index")
  
  tbl_list_csh_trnsfr[[1]] <- tbl_list_csh_trnsfr[[1]] %>%
    modify_header(estimate = "Intra-household \n dynamics index")
  
  tbl_list_pi_pool[[1]] <- tbl_list_pi_pool[[1]] %>%
    modify_header(estimate = "Intra-household \n dynamics index")
  
  ### Violence perceptions index
  tbl_list_pi[[2]] <- tbl_list_pi[[2]] %>%
    modify_header(estimate = "Violence \n perceptions index")
  
  tbl_list_csh_trnsfr[[2]] <- tbl_list_csh_trnsfr[[2]] %>%
    modify_header(estimate = "Violence \n perceptions index")
  
  tbl_list_pi_pool[[2]] <- tbl_list_pi_pool[[2]] %>%
    modify_header(estimate = "Violence \n perceptions index")
  
  ### Gender attitudes index
  tbl_list_pi[[3]] <- tbl_list_pi[[3]] %>%
    modify_header(estimate = "Gender \n attitudes index")
  
  tbl_list_csh_trnsfr[[3]] <- tbl_list_csh_trnsfr[[3]] %>%
    modify_header(estimate = "Gender \n attitudes index")
  
  tbl_list_pi_pool[[3]] <- tbl_list_pi_pool[[3]] %>%
    modify_header(estimate = "Gender \n attitudes index")

  # Merge the stacked tables into a single table with column headers
  ## Cash transfert
  tbl_csh_trnsfr <- tbl_merge(
    tbls = list(tbl_list_csh_trnsfr[[1]],# First model
                tbl_list_csh_trnsfr[[2]], # Second model
                tbl_list_csh_trnsfr[[3]]
    ), 
    tab_spanner = c("(1)","(2)","(3)")
  ) 
  
  ## Productive inclusion
  tbl_pi <- tbl_merge(
    tbls = list(tbl_list_pi[[1]],# First model
                tbl_list_pi[[2]], # Second model
                tbl_list_pi[[3]]
                
    ),  
    tab_spanner = c("(1)","(2)","(3)")
  )
  
  ## Pool Productive inclusion
  tbl_pi_pool <- tbl_merge(
    tbls = list(tbl_list_pi_pool[[1]],# First model
                tbl_list_pi_pool[[2]], # Second model
                tbl_list_pi_pool[[3]]
                
    ),  
    tab_spanner = c("(1)","(2)","(3)")
  )
  
  # Create a list of stacked tables combining Cash Transfer and Productive Inclusion models
  # Each list element combines the corresponding models from both treatment types
  tbl <- tbl_stack(list(tbl_csh_trnsfr,  tbl_pi, tbl_pi_pool)) #tbl_pi,
  
  # Modify table body to customize variable labels
  tbl <- tbl %>%
    modify_table_body(~ .x %>%
                        mutate(label = case_when(
                          # Rename specific treatment variables
                          term_1 == "treatment_piCapital" ~ "Capital",
                          term_1 == "treatment_piPsychosocial" ~ "Psychosocial",
                          term_1 == "treatment_piFull" ~ "Full",
                          term_1 == "treatment_pi_poolPool" ~ "Pool",
                          
                          # Label cash transfer and productive inclusion treatments
                          variable == "treatment_csh_trnsfr" & label=="Tekavoul program IE treatment group" ~ "Tekavoul",
                          
                          variable == "treatment_pi" & label=="PI IE treatment group" ~ "PI",
                          variable == "treatment_pi_pool" & label=="treatment_pi_pool" ~ "PI (Pool)",

                          TRUE ~ label  # Keep original labels for other variables
                        )))
  
  
  additional_rows <- tribble(
    ~tbl_id1, ~variable, ~var_label, ~label,
    ~estimate_1, ~estimate_2, ~estimate_3, 
    ~stars_1, ~stars_2, ~stars_3, 
    
    
    # Row 1: Control mean mean_values[4], mean_values[5], 
    4, "Control mean @ follow up", "Control mean @ follow up", "Control mean @ follow up",
    mean_values[1], mean_values[2], mean_values[3], 
    "", "", "", 
    # Row 2: Control SD
    4, "Control SD @ follow up", "Control SD @ follow up", "Control SD @ follow up",
    sd_values[1], sd_values[2], sd_values[3], 
    "", "", "" 
  )
  # browser()
  # Append the additional rows to the table body
  tbl$table_body <- bind_rows(tbl$table_body, additional_rows)
  
  # Title for each index
  tbl_ipv_title = paste0("**Table : effect of the programs by treatment**")
  
  # Final table formatting
  tbl <- tbl %>%
    # Set table caption
    modify_caption(tbl_ipv_title) %>%
    # Modify header for the label column
    modify_header(label = "**Outcome**") %>%
    # Remove all existing footnotes
    modify_footnote(everything() ~ NA) %>%
    modify_footnote(all_stat_cols() ~ NA) %>%
    # Add custom footnote with methodological details
    modify_footnote(label = "Notes: Results presented are OLS estimates that include controls for randomization strata (commune) and, where possible, baseline outcomes.  We control for social promotion intervention. Enumerator fixed effects are included in all regression. We estimate the regressions for the productive beneficiaries aged 18-49 only. Robust standard errors are shown in parentheses, clustered at the village proxy level. *** p < 0.01, ** p < 0.05, * p < 0.1.")
  
  # Print the final table
  tbl
}

getTable2_het <- function(revenue_consum_vars_ben, mainResults_mech_ben){
  
  mainResults_mech_ben_curr <- mainResults_mech_ben 
  
  
  ## Cash transfer regression models
  ### Extract specific models for cash transfer from the main results
  mainResults_mech_ben_curr_csh <- mainResults_mech_ben_curr %>% filter(treat_var=="treatment_csh_trnsfr")
  
  m10 <- mainResults_mech_ben_curr_csh$results_base[[1]]  # First cash transfer model
  m11 <- mainResults_mech_ben_curr_csh$results_base[[2]]  # Second cash transfer model
  m12 <- mainResults_mech_ben_curr_csh$results_base[[3]]  #  cash transfer model
  m13 <- mainResults_mech_ben_curr_csh$results_base[[4]]  #  cash transfer model
  m14 <- mainResults_mech_ben_curr_csh$results_base[[5]]  #  cash transfer model
  m15 <- mainResults_mech_ben_curr_csh$results_base[[6]]  #  cash transfer model
  m16 <- mainResults_mech_ben_curr_csh$results_base[[7]]  # Second cash transfer model
  m17 <- mainResults_mech_ben_curr_csh$results_base[[8]]  #  cash transfer model
  m18 <- mainResults_mech_ben_curr_csh$results_base[[9]]  #  cash transfer model
  m19 <- mainResults_mech_ben_curr_csh$results_base[[10]]  #  cash transfer model
  m110 <- mainResults_mech_ben_curr_csh$results_base[[11]]  #  cash transfer model
  m111 <- mainResults_mech_ben_curr_csh$results_base[[12]]  #  cash transfer model
  
  ## Productive inclusion regression models
  ### Extract specific models for productive inclusion from the main results
  mainResults_mech_ben_curr_pi <- mainResults_mech_ben_curr %>% filter(treat_var=="treatment_pi")
  
  m20 <- mainResults_mech_ben_curr_pi$results_base[[1]]  # First productive inclusion model
  m21 <- mainResults_mech_ben_curr_pi$results_base[[2]]  # Second productive inclusion model
  m22 <- mainResults_mech_ben_curr_pi$results_base[[3]]  #  productive inclusion model
  m23 <- mainResults_mech_ben_curr_pi$results_base[[4]]  #  productive inclusion model
  m24 <- mainResults_mech_ben_curr_pi$results_base[[5]]  #  productive inclusion model
  m25 <- mainResults_mech_ben_curr_pi$results_base[[6]]  #  productive inclusion model
  m26 <- mainResults_mech_ben_curr_pi$results_base[[7]]  #  productive inclusion model
  m27 <- mainResults_mech_ben_curr_pi$results_base[[8]]  #  productive inclusion model
  m28 <- mainResults_mech_ben_curr_pi$results_base[[9]]  #  productive inclusion model
  m29 <- mainResults_mech_ben_curr_pi$results_base[[10]]  #  productive inclusion model
  m210 <- mainResults_mech_ben_curr_pi$results_base[[11]]  #  productive inclusion model
  m211 <- mainResults_mech_ben_curr_pi$results_base[[12]]  #  productive inclusion model
  
  ### Extract specific models for productive inclusion (pool) from the main results
  mainResults_mech_ben_curr_pool <- mainResults_mech_ben_curr %>% filter(treat_var=="treatment_pi_pool")
  
  m30 <- mainResults_mech_ben_curr_pool$results_base[[1]]  # First pool productive inclusion model
  m31 <- mainResults_mech_ben_curr_pool$results_base[[2]]  # Second pool productive inclusion model
  m32 <- mainResults_mech_ben_curr_pool$results_base[[3]]  #  pool productive inclusion model
  m33 <- mainResults_mech_ben_curr_pool$results_base[[4]]  #  pool productive inclusion model
  m34 <- mainResults_mech_ben_curr_pool$results_base[[5]]  #  pool productive inclusion model
  m35 <- mainResults_mech_ben_curr_pool$results_base[[6]]  #  pool productive inclusion model
  m36 <- mainResults_mech_ben_curr_pool$results_base[[7]]  #  pool productive inclusion model
  m37 <- mainResults_mech_ben_curr_pool$results_base[[8]]  #  pool productive inclusion model
  m38 <- mainResults_mech_ben_curr_pool$results_base[[9]]  #  pool productive inclusion model
  m39 <- mainResults_mech_ben_curr_pool$results_base[[10]]  #  pool productive inclusion model
  m310 <- mainResults_mech_ben_curr_pool$results_base[[11]]  #  pool productive inclusion model
  m311 <- mainResults_mech_ben_curr_pool$results_base[[12]]  #  pool productive inclusion model
  
  
  # Initialize empty vectors to store descriptive statistics
  # These will store mean and standard deviation for each outcome variable
  mean_values <- c()  # Vector to store mean values
  sd_values <- c()   # Vector to store standard deviation values
  
  # Calculate descriptive statistics for each outcome variable
  # Loop through the list of outcome variables
  for (depvar in revenue_consum_vars_ben) {
    # Calculate and round mean, ignoring NA values
    mean_values <- c(mean_values,  round(mean(followup_MRT_hh_control[[depvar]], na.rm = TRUE), 3))
    
    # Calculate and round standard deviation, ignoring NA values
    sd_values <- c(sd_values,  round(sd(followup_MRT_hh_control[[depvar]], na.rm = TRUE), 3))
  }
  
  # Create lists of models for easier processing
  models_pi <- list(m20, m21, m22, m23, m24, m25, m26, m27, m28, m29, m210, m211)  # Productive inclusion models
  models_pi_pool <- list(m30, m31, m32, m33, m34, m35, m36, m37, m38, m39, m310, m311)  # Productive inclusion models pool
  models_csh_trnsfr <- list(m10, m11, m12, m13, m14, m15, m16, m17, m18, m19, m110, m111)  # Cash transfer models
  
  
  
  
  # Create regression tables for Productive Inclusion models
  tbl_list_pi <- map(models_pi, ~ tbl_regression(.x, 
                                                 exponentiate = FALSE,  # Keep coefficients as-is 
                                                 estimate_fun = ~ style_number(.x, digits = 2),
                                                 include=c(                                 # Retain main and interaction terms
                                                   "treatment_pi"))  %>% 
                       add_significance_stars(pattern = "{estimate}{stars} \n ({std.error})",
                                              hide_se = TRUE) %>%
                       add_glance_table(include = c(nobs, r.squared)))
  
  tbl_list_pi_pool <- map(models_pi_pool, ~ tbl_regression(.x, 
                                                           exponentiate = FALSE,  # Keep coefficients as-is 
                                                           estimate_fun = ~ style_number(.x, digits = 2),
                                                           include=c(                                 # Keep main pooled treatment and its interaction
                                                             "treatment_pi_pool"))  %>% 
                            add_significance_stars(pattern = "{estimate}{stars} \n ({std.error})",
                                                   hide_se = TRUE)%>%
                            add_glance_table(include = c(nobs, r.squared)))
  
  # Create regression tables for Cash Transfer models
  tbl_list_csh_trnsfr <- map(models_csh_trnsfr, ~ tbl_regression(.x, 
                                                                 exponentiate = FALSE,  # Keep coefficients as-is
                                                                 estimate_fun = ~ style_number(.x, digits = 2),
                                                                 include= c(                                 # Coefficients to retain
                                                                   "treatment_csh_trnsfr"
                                                                 ))  %>% 
                               add_significance_stars(pattern = "{estimate}{stars} \n ({std.error})",
                                                      hide_se = TRUE)%>%
                               add_glance_table(include = c(nobs, r.squared)))
  
  # Modify headers for the Productive Inclusion and Cash Transfer models
  
  ## Total revenue
  ### hh
  tbl_list_pi[[1]] <- tbl_list_pi[[1]] %>%
    modify_header(estimate = "hh.")
  
  tbl_list_csh_trnsfr[[1]] <- tbl_list_csh_trnsfr[[1]] %>%
    modify_header(estimate = "hh.")
  
  tbl_list_pi_pool[[1]] <- tbl_list_pi_pool[[1]] %>%
    modify_header(estimate = "hh.")
  
  ### ben
  tbl_list_pi[[2]] <- tbl_list_pi[[2]] %>%
    modify_header(estimate = "ben.")
  
  tbl_list_csh_trnsfr[[2]] <- tbl_list_csh_trnsfr[[2]] %>%
    modify_header(estimate = "ben.")
  
  tbl_list_pi_pool[[2]] <- tbl_list_pi_pool[[2]] %>%
    modify_header(estimate = "ben.")
  ### partner
  tbl_list_pi[[3]] <- tbl_list_pi[[3]] %>%
    modify_header(estimate = "prt.")
  
  tbl_list_csh_trnsfr[[3]] <- tbl_list_csh_trnsfr[[3]] %>%
    modify_header(estimate = "prt.")
  
  tbl_list_pi_pool[[3]] <- tbl_list_pi_pool[[3]] %>%
    modify_header(estimate = "prt.")
  
  
  ## Business rev
  ### hh
  tbl_list_pi[[4]] <- tbl_list_pi[[4]] %>%
    modify_header(estimate = "hh.")
  
  tbl_list_csh_trnsfr[[4]] <- tbl_list_csh_trnsfr[[4]] %>%
    modify_header(estimate = "hh.")
  
  tbl_list_pi_pool[[4]] <- tbl_list_pi_pool[[4]] %>%
    modify_header(estimate = "hh.")
  
  ### ben
  tbl_list_pi[[5]] <- tbl_list_pi[[5]] %>%
    modify_header(estimate = "ben.")
  
  tbl_list_csh_trnsfr[[5]] <- tbl_list_csh_trnsfr[[5]] %>%
    modify_header(estimate = "ben.")
  
  tbl_list_pi_pool[[5]] <- tbl_list_pi_pool[[5]] %>%
    modify_header(estimate = "ben.")
  
  ### partner
  tbl_list_pi[[6]] <- tbl_list_pi[[6]] %>%
    modify_header(estimate = "prt.")
  
  tbl_list_csh_trnsfr[[6]] <- tbl_list_csh_trnsfr[[6]] %>%
    modify_header(estimate = "prt.")
  
  tbl_list_pi_pool[[6]] <- tbl_list_pi_pool[[6]] %>%
    modify_header(estimate = "prt.")
  
  ## Wage rev
  ### hh
  tbl_list_pi[[7]] <- tbl_list_pi[[7]] %>%
    modify_header(estimate = "hh.")
  
  tbl_list_csh_trnsfr[[7]] <- tbl_list_csh_trnsfr[[7]] %>%
    modify_header(estimate = "hh.")
  
  tbl_list_pi_pool[[7]] <- tbl_list_pi_pool[[7]] %>%
    modify_header(estimate = "hh.")
  
  ### ben
  tbl_list_pi[[8]] <- tbl_list_pi[[8]] %>%
    modify_header(estimate = "ben.")
  
  tbl_list_csh_trnsfr[[8]] <- tbl_list_csh_trnsfr[[8]] %>%
    modify_header(estimate = "ben.")
  
  tbl_list_pi_pool[[8]] <- tbl_list_pi_pool[[8]] %>%
    modify_header(estimate = "ben.")
  
  ### part
  tbl_list_pi[[9]] <- tbl_list_pi[[9]] %>%
    modify_header(estimate = "prt.")
  
  tbl_list_csh_trnsfr[[9]] <- tbl_list_csh_trnsfr[[9]] %>%
    modify_header(estimate = "prt.")
  
  tbl_list_pi_pool[[9]] <- tbl_list_pi_pool[[9]] %>%
    modify_header(estimate = "prt.")
  
  ## Livestock
  ### hh
  tbl_list_pi[[10]] <- tbl_list_pi[[10]] %>%
    modify_header(estimate = "hh.")
  
  tbl_list_csh_trnsfr[[10]] <- tbl_list_csh_trnsfr[[10]] %>%
    modify_header(estimate = "hh.")
  
  tbl_list_pi_pool[[10]] <- tbl_list_pi_pool[[10]] %>%
    modify_header(estimate = "hh.")
  
  ### ben
  tbl_list_pi[[11]] <- tbl_list_pi[[11]] %>%
    modify_header(estimate = "ben.")
  
  tbl_list_csh_trnsfr[[11]] <- tbl_list_csh_trnsfr[[11]] %>%
    modify_header(estimate = "ben.")
  
  tbl_list_pi_pool[[11]] <- tbl_list_pi_pool[[11]] %>%
    modify_header(estimate = "ben.")
  
  ### prt
  tbl_list_pi[[12]] <- tbl_list_pi[[12]] %>%
    modify_header(estimate = "prt.")
  
  tbl_list_csh_trnsfr[[12]] <- tbl_list_csh_trnsfr[[12]] %>%
    modify_header(estimate = "prt.")
  
  tbl_list_pi_pool[[12]] <- tbl_list_pi_pool[[12]] %>%
    modify_header(estimate = "prt.")
  
  
  
  # Merge the stacked tables into a single table with column headers
  ## Cash transfert
  tbl_csh_trnsfr <- tbl_merge(
    tbls = list(tbl_list_csh_trnsfr[[1]],# First model
                tbl_list_csh_trnsfr[[2]], # Second model
                tbl_list_csh_trnsfr[[3]], 
                tbl_list_csh_trnsfr[[4]],
                tbl_list_csh_trnsfr[[5]], 
                tbl_list_csh_trnsfr[[6]], 
                tbl_list_csh_trnsfr[[7]], 
                tbl_list_csh_trnsfr[[8]],
                tbl_list_csh_trnsfr[[9]],
                tbl_list_csh_trnsfr[[10]],
                tbl_list_csh_trnsfr[[11]],
                tbl_list_csh_trnsfr[[12]] 
    ), 
    tab_spanner = c("Total \n revenue \n (yearly, USD)","","","Business \n revenues \n (yearly, USD)","","", "Wage \n earnings \n (yearly, USD)","","","Livestock \n revenue \n (yearly, USD)","","")
  ) 
  
  ## Productive inclusion
  tbl_pi <- tbl_merge(
    tbls = list(tbl_list_pi[[1]],# First model
                tbl_list_pi[[2]], # Second model
                tbl_list_pi[[3]], 
                tbl_list_pi[[4]],
                tbl_list_pi[[5]], 
                tbl_list_pi[[6]], 
                tbl_list_pi[[7]], 
                tbl_list_pi[[8]],
                tbl_list_pi[[9]],
                tbl_list_pi[[10]],
                tbl_list_pi[[11]],
                tbl_list_pi[[12]] 
    ),  
    tab_spanner = c("Total \n revenue \n (yearly, USD)","","","Business \n revenues \n (yearly, USD)","","", "Wage \n earnings \n (yearly, USD)","","","Livestock \n revenue \n (yearly, USD)","","")
  )
  
  ## Pool Productive inclusion
  tbl_pi_pool <- tbl_merge(
    tbls = list(tbl_list_pi_pool[[1]],# First model
                tbl_list_pi_pool[[2]], # Second model
                tbl_list_pi_pool[[3]], 
                tbl_list_pi_pool[[4]],
                tbl_list_pi_pool[[5]], 
                tbl_list_pi_pool[[6]], 
                tbl_list_pi_pool[[7]], 
                tbl_list_pi_pool[[8]],
                tbl_list_pi_pool[[9]],
                tbl_list_pi_pool[[10]],
                tbl_list_pi_pool[[11]],
                tbl_list_pi_pool[[12]] 
    ),  
    tab_spanner = c("Total \n revenue \n (yearly, USD)","","","Business \n revenues \n (yearly, USD)","","", "Wage \n earnings \n (yearly, USD)","","","Livestock \n revenue \n (yearly, USD)","","")
  )
  
  # Create a list of stacked tables combining Cash Transfer and Productive Inclusion models
  # Each list element combines the corresponding models from both treatment types
  tbl <- tbl_stack(list(tbl_csh_trnsfr,  tbl_pi, tbl_pi_pool)) #tbl_pi,
  
  # Modify table body to customize variable labels
  tbl <- tbl %>%
    modify_table_body(~ .x %>%
                        mutate(label = case_when(
                          # Rename specific treatment variables
                          term_1 == "treatment_piCapital" ~ "Capital",
                          term_1 == "treatment_piPsychosocial" ~ "Psychosocial",
                          term_1 == "treatment_piFull" ~ "Full",
                          term_1 == "treatment_pi_poolPool" ~ "Pool",
                          
                          # Label cash transfer and productive inclusion treatments
                          variable == "treatment_csh_trnsfr" & label=="Tekavoul program IE treatment group" ~ "Tekavoul",
                          
                          variable == "treatment_pi" & label=="PI IE treatment group" ~ "PI",
                          variable == "treatment_pi_pool" & label=="treatment_pi_pool" ~ "PI (Pool)",
                          TRUE ~ label  # Keep original labels for other variables
                        )))
  
  
  additional_rows <- tribble(
    ~tbl_id1, ~variable, ~var_label, ~label,
    ~estimate_1, ~estimate_2, ~estimate_3, ~estimate_4, ~estimate_5,
    ~estimate_6, ~estimate_7, ~estimate_8, ~estimate_9, ~estimate_10,
    ~estimate_11, ~estimate_12,
    ~stars_1, ~stars_2, ~stars_3, ~stars_4, ~stars_5,
    ~stars_6, ~stars_7, ~stars_8, ~stars_9, ~stars_10,~stars_11, ~stars_12,
    
    
    # Row 1: Control mean
    4, "Control mean @ follow up", "Control mean @ follow up", "Control mean @ follow up",
    mean_values[1], mean_values[2], mean_values[3], mean_values[4], mean_values[5],
    mean_values[6], mean_values[7], mean_values[8], mean_values[9], mean_values[10], 
    mean_values[11], mean_values[12],
    "", "", "", "", "",
    "", "", "", "", "","", "",
    
    # Row 2: Control SD
    4, "Control SD @ follow up", "Control SD @ follow up", "Control SD @ follow up",
    sd_values[1], sd_values[2], sd_values[3], sd_values[4], sd_values[5],
    sd_values[6], sd_values[7], sd_values[8], sd_values[9], sd_values[10],
    sd_values[11], sd_values[12],
    "", "", "", "", "","", "",
    "", "", "", "", ""
    
  )
  # Append the additional rows to the table body
  tbl$table_body <- bind_rows(tbl$table_body, additional_rows)
  
  # Title for each index
  tbl_ipv_title = paste0("**Table : effect of the programs by treatment**")
  
  # Final table formatting
  tbl <- tbl %>%
    # Set table caption
    modify_caption(tbl_ipv_title) %>%
    # Modify header for the label column
    modify_header(label = "**Outcome**") %>%
    # Remove all existing footnotes
    modify_footnote(everything() ~ NA) %>%
    modify_footnote(all_stat_cols() ~ NA) %>%
    # Add custom footnote with methodological details
    modify_footnote(label = "Notes: Results presented are OLS estimates that include controls for randomization strata (commune) and, where possible, baseline outcomes.  We control for social promotion intervention. Enumerator fixed effects are included in all regression. We estimate the regressions for the productive beneficiaries aged 18-49 only. Robust standard errors are shown in parentheses, clustered at the village proxy level. *** p < 0.01, ** p < 0.05, * p < 0.1.")
  
  # Print the final table
  tbl
  
}


getTable1_het <- function(revenue_consum_vars, mainResults_mech_hh){
  
  
  mainResults_mech_hh_curr <- mainResults_mech_hh 
  
  
  ## Cash transfer regression models
  ### Extract specific models for cash transfer from the main results
  mainResults_mech_hh_curr_csh <- mainResults_mech_hh_curr %>% filter(treat_var=="treatment_csh_trnsfr")
  
  m10 <- mainResults_mech_hh_curr_csh$results_base[[1]]  # First cash transfer model
  m11 <- mainResults_mech_hh_curr_csh$results_base[[2]]  # Second cash transfer model
  m12 <- mainResults_mech_hh_curr_csh$results_base[[3]]  #  cash transfer model
  m13 <- mainResults_mech_hh_curr_csh$results_base[[4]]  #  cash transfer model
  m14 <- mainResults_mech_hh_curr_csh$results_base[[5]]  #  cash transfer model
  m15 <- mainResults_mech_hh_curr_csh$results_base[[6]]  #  cash transfer model
  m16 <- mainResults_mech_hh_curr_csh$results_base[[7]]  # Second cash transfer model
  m17 <- mainResults_mech_hh_curr_csh$results_base[[8]]  #  cash transfer model
  m18 <- mainResults_mech_hh_curr_csh$results_base[[9]]  #  cash transfer model
  m19 <- mainResults_mech_hh_curr_csh$results_base[[10]]  #  cash transfer model
  m110 <- mainResults_mech_hh_curr_csh$results_base[[11]]  # First cash transfer model
  m111 <- mainResults_mech_hh_curr_csh$results_base[[12]]  # Second cash transfer model
  m112 <- mainResults_mech_hh_curr_csh$results_base[[13]]  #  cash transfer model
  m113 <- mainResults_mech_hh_curr_csh$results_base[[14]]  #  cash transfer model
  m114 <- mainResults_mech_hh_curr_csh$results_base[[15]]  #  cash transfer model
  m115 <- mainResults_mech_hh_curr_csh$results_base[[16]]  #  cash transfer model
  m116 <- mainResults_mech_hh_curr_csh$results_base[[17]]  # Second cash transfer model
  m117 <- mainResults_mech_hh_curr_csh$results_base[[18]]  #  cash transfer model
  m118 <- mainResults_mech_hh_curr_csh$results_base[[19]]  #  cash transfer model
  m119 <- mainResults_mech_hh_curr_csh$results_base[[20]]  #  cash transfer model 
  m120 <- mainResults_mech_hh_curr_csh$results_base[[21]]  #  cash transfer model  
  ## Productive inclusion regression models
  ### Extract specific models for productive inclusion from the main results
  mainResults_mech_hh_curr_pi <- mainResults_mech_hh_curr %>% filter(treat_var=="treatment_pi")
  
  m20 <- mainResults_mech_hh_curr_pi$results_base[[1]]  # First productive inclusion model
  m21 <- mainResults_mech_hh_curr_pi$results_base[[2]]  # Second productive inclusion model
  m22 <- mainResults_mech_hh_curr_pi$results_base[[3]]  #  productive inclusion model
  m23 <- mainResults_mech_hh_curr_pi$results_base[[4]]  #  productive inclusion model
  m24 <- mainResults_mech_hh_curr_pi$results_base[[5]]  #  productive inclusion model
  m25 <- mainResults_mech_hh_curr_pi$results_base[[6]]  #  productive inclusion model
  m26 <- mainResults_mech_hh_curr_pi$results_base[[7]]  #  productive inclusion model
  m27 <- mainResults_mech_hh_curr_pi$results_base[[8]]  #  productive inclusion model
  m28 <- mainResults_mech_hh_curr_pi$results_base[[9]]  #  productive inclusion model
  m29 <- mainResults_mech_hh_curr_pi$results_base[[10]]  #  productive inclusion model
  m210 <- mainResults_mech_hh_curr_pi$results_base[[11]]  # First productive inclusion model
  m211 <- mainResults_mech_hh_curr_pi$results_base[[12]]  # Second productive inclusion model
  m212 <- mainResults_mech_hh_curr_pi$results_base[[13]]  #  productive inclusion model
  m213 <- mainResults_mech_hh_curr_pi$results_base[[14]]  #  productive inclusion model
  m214 <- mainResults_mech_hh_curr_pi$results_base[[15]]  #  productive inclusion model
  m215 <- mainResults_mech_hh_curr_pi$results_base[[16]]  #  productive inclusion model
  m216 <- mainResults_mech_hh_curr_pi$results_base[[17]]  #  productive inclusion model
  m217 <- mainResults_mech_hh_curr_pi$results_base[[18]]  #  productive inclusion model
  m218 <- mainResults_mech_hh_curr_pi$results_base[[19]]  #  productive inclusion model
  m219 <- mainResults_mech_hh_curr_pi$results_base[[20]]  #  productive inclusion model  
  m220 <- mainResults_mech_hh_curr_pi$results_base[[21]]  #  productive inclusion model  
  
  ### Extract specific models for productive inclusion (pool) from the main results
  mainResults_mech_hh_curr_pool <- mainResults_mech_hh_curr %>% filter(treat_var=="treatment_pi_pool")
  
  m30 <- mainResults_mech_hh_curr_pool$results_base[[1]]  # First pool productive inclusion model
  m31 <- mainResults_mech_hh_curr_pool$results_base[[2]]  # Second pool productive inclusion model
  m32 <- mainResults_mech_hh_curr_pool$results_base[[3]]  #  pool productive inclusion model
  m33 <- mainResults_mech_hh_curr_pool$results_base[[4]]  #  pool productive inclusion model
  m34 <- mainResults_mech_hh_curr_pool$results_base[[5]]  #  pool productive inclusion model
  m35 <- mainResults_mech_hh_curr_pool$results_base[[6]]  #  pool productive inclusion model
  m36 <- mainResults_mech_hh_curr_pool$results_base[[7]]  #  pool productive inclusion model
  m37 <- mainResults_mech_hh_curr_pool$results_base[[8]]  #  pool productive inclusion model
  m38 <- mainResults_mech_hh_curr_pool$results_base[[9]]  #  pool productive inclusion model
  m39 <- mainResults_mech_hh_curr_pool$results_base[[10]]  #  pool productive inclusion model
  m310 <- mainResults_mech_hh_curr_pool$results_base[[11]]  # First pool productive inclusion model
  m311 <- mainResults_mech_hh_curr_pool$results_base[[12]]  # Second pool productive inclusion model
  m312 <- mainResults_mech_hh_curr_pool$results_base[[13]]  #  pool productive inclusion model
  m313 <- mainResults_mech_hh_curr_pool$results_base[[14]]  #  pool productive inclusion model
  m314 <- mainResults_mech_hh_curr_pool$results_base[[15]]  #  pool productive inclusion model
  m315 <- mainResults_mech_hh_curr_pool$results_base[[16]]  #  pool productive inclusion model
  m316 <- mainResults_mech_hh_curr_pool$results_base[[17]]  #  pool productive inclusion model
  m317 <- mainResults_mech_hh_curr_pool$results_base[[18]]  #  pool productive inclusion model
  m318 <- mainResults_mech_hh_curr_pool$results_base[[19]]  #  pool productive inclusion model
  m319 <- mainResults_mech_hh_curr_pool$results_base[[20]]  #  pool productive inclusion model  
  m320 <- mainResults_mech_hh_curr_pool$results_base[[21]]  #  pool productive inclusion model  
  

  # Initialize empty vectors to store descriptive statistics
  # These will store mean and standard deviation for each outcome variable
  mean_values <- c()  # Vector to store mean values
  sd_values <- c()   # Vector to store standard deviation values

  # Calculate descriptive statistics for each outcome variable
  # Loop through the list of outcome variables
  for (depvar in revenue_consum_vars) {
    # Calculate and round mean, ignoring NA values
    mean_values <- c(mean_values,  round(mean(followup_MRT_hh_control[[depvar]], na.rm = TRUE), 3))
    
    # Calculate and round standard deviation, ignoring NA values
    sd_values <- c(sd_values,  round(sd(followup_MRT_hh_control[[depvar]], na.rm = TRUE), 3))
  }
  
  # Create lists of models for easier processing
  models_pi <- list(m20, m21, m22, m23, m24, m25, m26, m27, m28, m29, m210, m211, 
                    m212, m213, m214, m215, m216, m217, m218, m219, m220)  # Productive inclusion models
  models_pi_pool <- list(m30, m31, m32, m33, m34, m35, m36, m37, m38, m39, m310, 
                         m311, m312, m313, m314, m315, m316, m317, m318, m319, m320)  # Productive inclusion models pool
  models_csh_trnsfr <- list(m10, m11, m12, m13, m14, m15, m16, m17, m18, m19, m110, 
                            m111, m112, m113, m114, m115, m116, m117, m118, m119, m120)  # Cash transfer models
  
  
  
  
  # Create regression tables for Productive Inclusion models
  tbl_list_pi <- map(models_pi, ~ tbl_regression(.x, 
                                                 exponentiate = FALSE,  # Keep coefficients as-is 
                                                 estimate_fun = ~ style_number(.x, digits = 2),
                                                 include=c(                                 # Retain main and interaction terms
                                                   "treatment_pi"))  %>% 
                       add_significance_stars(pattern = "{estimate}{stars} \n ({std.error})",
                                              hide_se = TRUE) %>%
                       add_glance_table(include = c(nobs, r.squared)))
  
  tbl_list_pi_pool <- map(models_pi_pool, ~ tbl_regression(.x, 
                                                           exponentiate = FALSE,  # Keep coefficients as-is 
                                                           estimate_fun = ~ style_number(.x, digits = 2),
                                                           include=c(                                 # Keep main pooled treatment and its interaction
                                                             "treatment_pi_pool"))  %>% 
                            add_significance_stars(pattern = "{estimate}{stars} \n ({std.error})",
                                                   hide_se = TRUE)%>%
                            add_glance_table(include = c(nobs, r.squared)))
  
  # Create regression tables for Cash Transfer models
  tbl_list_csh_trnsfr <- map(models_csh_trnsfr, ~ tbl_regression(.x, 
                                                                 exponentiate = FALSE,  # Keep coefficients as-is
                                                                 estimate_fun = ~ style_number(.x, digits = 2),
                                                                 include= c(                                 # Coefficients to retain
                                                                   "treatment_csh_trnsfr" 
                                                                   #  paste0(curr_het_var, ":treatment_csh_trnsfr")
                                                                 ))  %>% 
                               add_significance_stars(pattern = "{estimate}{stars} \n ({std.error})",
                                                      hide_se = TRUE)%>%
                               add_glance_table(include = c(nobs, r.squared)))
  
  # Modify headers for the Productive Inclusion and Cash Transfer models
  
  ## Gross consumption
  tbl_list_pi[[1]] <- tbl_list_pi[[1]] %>%
    modify_header(estimate = "Gross \n consumption\n (daily, USD/capita)")
  
  tbl_list_csh_trnsfr[[1]] <- tbl_list_csh_trnsfr[[1]] %>%
    modify_header(estimate = "Gross \n consumption\n (daily, USD/capita)")
  
  tbl_list_pi_pool[[1]] <- tbl_list_pi_pool[[1]] %>%
    modify_header(estimate = "Gross \n consumption\n (daily, USD/capita)")
  
  ## Food consumption
  tbl_list_pi[[2]] <- tbl_list_pi[[2]] %>%
    modify_header(estimate = "Food\n consumption\n (daily, USD/adult eq.)")
  
  tbl_list_csh_trnsfr[[2]] <- tbl_list_csh_trnsfr[[2]] %>%
    modify_header(estimate = "Food\n consumption\n (daily, USD/adult eq.)")
  
  tbl_list_pi_pool[[2]] <- tbl_list_pi_pool[[2]] %>%
    modify_header(estimate = "Food\n consumption\n (daily, USD/adult eq.)")
  
  
  ## Non food consumption
  tbl_list_pi[[3]] <- tbl_list_pi[[3]] %>%
    modify_header(estimate = "Non Food\n consumption\n (daily, USD/adult eq.)")
  
  tbl_list_csh_trnsfr[[3]] <- tbl_list_csh_trnsfr[[3]] %>%
    modify_header(estimate = "Non Food\n consumption\n (daily, USD/adult eq.)")
  
  tbl_list_pi_pool[[3]] <- tbl_list_pi_pool[[3]] %>%
    modify_header(estimate = "Non Food\n consumption\n (daily, USD/adult eq.)")
  
  ## Eat out expenditures
  tbl_list_pi[[4]] <- tbl_list_pi[[4]] %>%
    modify_header(estimate = "Eating out\n expenditure\n (daily, USD/adult eq.)")
  
  tbl_list_csh_trnsfr[[4]] <- tbl_list_csh_trnsfr[[4]] %>%
    modify_header(estimate = "Eating out\n expenditure\n (daily, USD/adult eq.)")
  
  tbl_list_pi_pool[[4]] <- tbl_list_pi_pool[[4]] %>%
    modify_header(estimate = "Eating out\n expenditure\n (daily, USD/adult eq.)")
  
  ## Education expenditures
  tbl_list_pi[[5]] <- tbl_list_pi[[5]] %>%
    modify_header(estimate = "Education\n expenditure\n (daily, USD/adult eq.)")
  
  tbl_list_csh_trnsfr[[5]] <- tbl_list_csh_trnsfr[[5]] %>%
    modify_header(estimate = "Education\n expenditure\n (daily, USD/adult eq.)")
  
  tbl_list_pi_pool[[5]] <- tbl_list_pi_pool[[5]] %>%
    modify_header(estimate = "Education\n expenditure\n (daily, USD/adult eq.)")
  
  ## Health expenditures
  tbl_list_pi[[6]] <- tbl_list_pi[[6]] %>%
    modify_header(estimate = "Health\n expenditure\n (daily, USD/adult eq.)")
  
  tbl_list_csh_trnsfr[[6]] <- tbl_list_csh_trnsfr[[6]] %>%
    modify_header(estimate = "Health\n expenditure\n (daily, USD/adult eq.)")
  
  tbl_list_pi_pool[[6]] <- tbl_list_pi_pool[[6]] %>%
    modify_header(estimate = "Health\n expenditure\n (daily, USD/adult eq.)")
  
  ## Celebration expenditures
  tbl_list_pi[[7]] <- tbl_list_pi[[7]] %>%
    modify_header(estimate = "Celebration\n expenditure\n (daily, USD/adult eq.)")
  
  tbl_list_csh_trnsfr[[7]] <- tbl_list_csh_trnsfr[[7]] %>%
    modify_header(estimate = "Celebration\n expenditure\n (daily, USD/adult eq.)")
  
  tbl_list_pi_pool[[7]] <- tbl_list_pi_pool[[7]] %>%
    modify_header(estimate = "Celebration\n expenditure\n (daily, USD/adult eq.)")  
  
  ## Tontine
  tbl_list_pi[[8]] <- tbl_list_pi[[8]] %>%
    modify_header(estimate ="Takes part in\n tontine/AVEC\n (0,1)")
  
  tbl_list_csh_trnsfr[[8]] <- tbl_list_csh_trnsfr[[8]] %>%
    modify_header(estimate = "Takes part in\n tontine/AVEC\n (0,1)")
  
  tbl_list_pi_pool[[8]] <- tbl_list_pi_pool[[8]] %>%
    modify_header(estimate = "Takes part in\n tontine/AVEC\n (0,1)")  
  
  ## "Total\n debt\n (yearly, USD)"	
  tbl_list_pi[[9]] <- tbl_list_pi[[9]] %>%
    modify_header(estimate ="Total\n debt\n (yearly, USD)")
  
  tbl_list_csh_trnsfr[[9]] <- tbl_list_csh_trnsfr[[9]] %>%
    modify_header(estimate = "Total\n debt\n (yearly, USD)")
  
  tbl_list_pi_pool[[9]] <- tbl_list_pi_pool[[9]] %>%
    modify_header(estimate = "Total\n debt\n (yearly, USD)")    
  
  ## "Total\n borrowed\n (yearly, USD)"
  tbl_list_pi[[10]] <- tbl_list_pi[[10]] %>%
    modify_header(estimate ="Total\n borrowed\n (yearly, USD)")
  
  tbl_list_csh_trnsfr[[10]] <- tbl_list_csh_trnsfr[[10]] %>%
    modify_header(estimate = "Total\n borrowed\n (yearly, USD)")
  
  tbl_list_pi_pool[[10]] <- tbl_list_pi_pool[[10]] %>%
    modify_header(estimate = "Total\n borrowed\n (yearly, USD)")   
  
  ## "Household\n Gross transfers\n (yearly, USD)"
  tbl_list_pi[[11]] <- tbl_list_pi[[11]] %>%
    modify_header(estimate ="Household\n Gross transfers\n (yearly, USD)")
  
  tbl_list_csh_trnsfr[[11]] <- tbl_list_csh_trnsfr[[11]] %>%
    modify_header(estimate = "Household\n Gross transfers\n (yearly, USD)")
  
  tbl_list_pi_pool[[11]] <- tbl_list_pi_pool[[11]] %>%
    modify_header(estimate = "Household\n Gross transfers\n (yearly, USD)")   
  
  ##   "Total\n savings\n (3 months, USD)"
  tbl_list_pi[[12]] <- tbl_list_pi[[12]] %>%
    modify_header(estimate =  "Total\n savings\n (3 months, USD)")
  
  tbl_list_csh_trnsfr[[12]] <- tbl_list_csh_trnsfr[[12]] %>%
    modify_header(estimate =   "Total\n savings\n (3 months, USD)")
  
  tbl_list_pi_pool[[12]] <- tbl_list_pi_pool[[12]] %>%
    modify_header(estimate =   "Total\n savings\n (3 months, USD)")  
  
  
  ## Food security
  tbl_list_pi[[13]] <- tbl_list_pi[[13]] %>%
    modify_header(estimate = "Food \n security \n (FIES)")
  
  tbl_list_csh_trnsfr[[13]] <- tbl_list_csh_trnsfr[[13]] %>%
    modify_header(estimate = "Food \n security \n (FIES)")
  
  tbl_list_pi_pool[[13]] <- tbl_list_pi_pool[[13]] %>%
    modify_header(estimate = "Food \n security \n (FIES)")
  
  ## Dietary diversity
  tbl_list_pi[[14]] <- tbl_list_pi[[14]] %>%
    modify_header(estimate = "Dietary \n diversity \n (FCS)")
  
  tbl_list_csh_trnsfr[[14]] <- tbl_list_csh_trnsfr[[14]] %>%
    modify_header(estimate = "Dietary \n diversity \n (FCS)")
  
  tbl_list_pi_pool[[14]] <- tbl_list_pi_pool[[14]] %>%
    modify_header(estimate = "Dietary \n diversity \n (FCS)")
  
  ## Total revenue
  tbl_list_pi[[15]] <- tbl_list_pi[[15]] %>%
    modify_header(estimate = "Total \n revenue \n (yearly, USD)")
  
  tbl_list_csh_trnsfr[[15]] <- tbl_list_csh_trnsfr[[15]] %>%
    modify_header(estimate = "Total \n revenue \n (yearly, USD)")
  
  tbl_list_pi_pool[[15]] <- tbl_list_pi_pool[[15]] %>%
    modify_header(estimate = "Total \n revenue \n (yearly, USD)")
  
  ## Business rev
  tbl_list_pi[[16]] <- tbl_list_pi[[16]] %>%
    modify_header(estimate = "Business \n revenues \n (yearly, USD)")
  
  tbl_list_csh_trnsfr[[16]] <- tbl_list_csh_trnsfr[[16]] %>%
    modify_header(estimate = "Business \n revenues \n (yearly, USD)")
  
  tbl_list_pi_pool[[16]] <- tbl_list_pi_pool[[16]] %>%
    modify_header(estimate = "Business \n revenues \n (yearly, USD)")
  
  ## Wage rev
  tbl_list_pi[[17]] <- tbl_list_pi[[17]] %>%
    modify_header(estimate = "Wage \n earnings \n (yearly, USD)")
  
  tbl_list_csh_trnsfr[[17]] <- tbl_list_csh_trnsfr[[17]] %>%
    modify_header(estimate = "Wage \n earnings \n (yearly, USD)")
  
  tbl_list_pi_pool[[17]] <- tbl_list_pi_pool[[17]] %>%
    modify_header(estimate = "Wage \n earnings \n (yearly, USD)")
  
  ## Livestock
  tbl_list_pi[[18]] <- tbl_list_pi[[18]] %>%
    modify_header(estimate = "Livestock \n revenue \n (yearly, USD)")
  
  tbl_list_csh_trnsfr[[18]] <- tbl_list_csh_trnsfr[[18]] %>%
    modify_header(estimate = "Livestock \n revenue \n (yearly, USD)")
  
  tbl_list_pi_pool[[18]] <- tbl_list_pi_pool[[18]] %>%
    modify_header(estimate = "Livestock \n revenue \n (yearly, USD)")
  
  ## Income sources
  tbl_list_pi[[19]] <- tbl_list_pi[[19]] %>%
    modify_header(estimate = "Count of \n income sources \n (yearly)")
  
  tbl_list_csh_trnsfr[[19]] <- tbl_list_csh_trnsfr[[19]] %>%
    modify_header(estimate = "Count of \n income sources \n (yearly)")
  
  tbl_list_pi_pool[[19]] <- tbl_list_pi_pool[[19]] %>%
    modify_header(estimate = "Count of \n income sources \n (yearly)")
  
  ## Income diversification
  tbl_list_pi[[20]] <- tbl_list_pi[[20]] %>%
    modify_header(estimate = "No. \n of income \n sources\n (Household)")
  
  tbl_list_csh_trnsfr[[20]] <- tbl_list_csh_trnsfr[[20]] %>%
    modify_header(estimate = "No. \n of income \n sources\n (Household)")
  
  tbl_list_pi_pool[[20]] <- tbl_list_pi_pool[[20]] %>%
    modify_header(estimate = "No. \n of income \n sources\n (Household)")
  
  
  ## Wage types
  tbl_list_pi[[21]] <- tbl_list_pi[[21]] %>%
    modify_header(estimate = "Wage \n types \n (Household)")
  
  tbl_list_csh_trnsfr[[21]] <- tbl_list_csh_trnsfr[[21]] %>%
    modify_header(estimate = "Wage \n types \n (Household)")
  
  tbl_list_pi_pool[[21]] <- tbl_list_pi_pool[[21]] %>%
    modify_header(estimate = "Wage \n types \n (Household)")
  
  
  
  # Merge the stacked tables into a single table with column headers
  ## Cash transfert
  tbl_csh_trnsfr <- tbl_merge(
    tbls = list(tbl_list_csh_trnsfr[[1]],# First model
                tbl_list_csh_trnsfr[[2]], # Second model
                tbl_list_csh_trnsfr[[3]], 
                tbl_list_csh_trnsfr[[4]],
                tbl_list_csh_trnsfr[[5]], 
                tbl_list_csh_trnsfr[[6]], 
                tbl_list_csh_trnsfr[[7]], 
                tbl_list_csh_trnsfr[[8]],
                tbl_list_csh_trnsfr[[9]],
                tbl_list_csh_trnsfr[[10]],
                tbl_list_csh_trnsfr[[11]],# First model
                tbl_list_csh_trnsfr[[12]], # Second model
                tbl_list_csh_trnsfr[[13]], 
                tbl_list_csh_trnsfr[[14]],
                tbl_list_csh_trnsfr[[15]], 
                tbl_list_csh_trnsfr[[16]], 
                tbl_list_csh_trnsfr[[17]], 
                tbl_list_csh_trnsfr[[18]],
                tbl_list_csh_trnsfr[[19]],
                tbl_list_csh_trnsfr[[20]], 
                tbl_list_csh_trnsfr[[21]]
    ),  
    tab_spanner = c("(1)","(2)","(3)","(4)","(5)","(6)", "(7)","(8)","(9)","(10)",
                    "(11)","(12)","(13)","(14)","(15)","(16)", "(17)","(18)","(19)",
                    "(20)","(21)")
  )
  
  ## Productive inclusion
  tbl_pi <- tbl_merge(
    tbls = list(tbl_list_pi[[1]],# First model
                tbl_list_pi[[2]], # Second model
                tbl_list_pi[[3]], 
                tbl_list_pi[[4]],
                tbl_list_pi[[5]], 
                tbl_list_pi[[6]], 
                tbl_list_pi[[7]], 
                tbl_list_pi[[8]],
                tbl_list_pi[[9]],
                tbl_list_pi[[10]], 
                tbl_list_pi[[11]],# First model
                tbl_list_pi[[12]], # Second model
                tbl_list_pi[[13]], 
                tbl_list_pi[[14]],
                tbl_list_pi[[15]], 
                tbl_list_pi[[16]], 
                tbl_list_pi[[17]], 
                tbl_list_pi[[18]],
                tbl_list_pi[[19]],
                tbl_list_pi[[20]], 
                tbl_list_pi[[21]] 
    ),  
    tab_spanner = c("(1)","(2)","(3)","(4)","(5)","(6)", "(7)","(8)","(9)","(10)",
                    "(11)","(12)","(13)","(14)","(15)","(16)", "(17)","(18)","(19)",
                    "(20)","(21)")
  )
  
  ## Pool Productive inclusion
  tbl_pi_pool <- tbl_merge(
    tbls = list(tbl_list_pi_pool[[1]],# First model
                tbl_list_pi_pool[[2]], # Second model
                tbl_list_pi_pool[[3]], 
                tbl_list_pi_pool[[4]],
                tbl_list_pi_pool[[5]], 
                tbl_list_pi_pool[[6]], 
                tbl_list_pi_pool[[7]], 
                tbl_list_pi_pool[[8]],
                tbl_list_pi_pool[[9]],
                tbl_list_pi_pool[[10]],
                tbl_list_pi_pool[[11]],# First model
                tbl_list_pi_pool[[12]], # Second model
                tbl_list_pi_pool[[13]], 
                tbl_list_pi_pool[[14]],
                tbl_list_pi_pool[[15]], 
                tbl_list_pi_pool[[16]], 
                tbl_list_pi_pool[[17]], 
                tbl_list_pi_pool[[18]],
                tbl_list_pi_pool[[19]],
                tbl_list_pi_pool[[20]],
                tbl_list_pi_pool[[21]]
    ),  
    tab_spanner = c("(1)","(2)","(3)","(4)","(5)","(6)", "(7)","(8)","(9)","(10)",
                    "(11)","(12)","(13)","(14)","(15)","(16)", "(17)","(18)","(19)",
                    "(20)","(21)")
  )
  
  # Create a list of stacked tables combining Cash Transfer and Productive Inclusion models
  # Each list element combines the corresponding models from both treatment types
  tbl <- tbl_stack(list(tbl_csh_trnsfr,  tbl_pi, tbl_pi_pool)) #tbl_pi,
  
  # Modify table body to customize variable labels
  tbl <- tbl %>%
    modify_table_body(~ .x %>%
                        mutate(label = case_when(
                          # Rename specific treatment variables
                          term_1 == "treatment_piCapital" ~ "Capital",
                          term_1 == "treatment_piPsychosocial" ~ "Psychosocial",
                          term_1 == "treatment_piFull" ~ "Full",
                          term_1 == "treatment_pi_poolPool" ~ "Pool",
                          
                          # Label cash transfer and productive inclusion treatments
                          variable == "treatment_csh_trnsfr" & label=="Tekavoul program IE treatment group" ~ "Tekavoul",
                          variable == "treatment_pi" & label=="PI IE treatment group" ~ "PI",
                          variable == "treatment_pi_pool" & label=="treatment_pi_pool" ~ "PI (Pool)",
                          
                          TRUE ~ label  # Keep original labels for other variables
                        )))
  
  
  additional_rows <- tribble(
    ~tbl_id1, ~variable, ~var_label, ~label,
    ~estimate_1, ~estimate_2, ~estimate_3, ~estimate_4, ~estimate_5,
    ~estimate_6, ~estimate_7, ~estimate_8, ~estimate_9, ~estimate_10,
    ~estimate_11, ~estimate_12, ~estimate_13, ~estimate_14, ~estimate_15,
    ~estimate_16, ~estimate_17, ~estimate_18, ~estimate_19, ~estimate_20, ~estimate_21,
    ~stars_1, ~stars_2, ~stars_3, ~stars_4, ~stars_5,
    ~stars_6, ~stars_7, ~stars_8, ~stars_9, ~stars_10,
    ~stars_11, ~stars_12, ~stars_13, ~stars_14, ~stars_15,
    ~stars_16, ~stars_17, ~stars_18, ~stars_19, ~stars_20, ~stars_21,
    
    
    # Row 1: Control mean
    4, "Control mean @ follow up", "Control mean @ follow up", "Control mean @ follow up",
    mean_values[1], mean_values[2], mean_values[3], mean_values[4], mean_values[5],
    mean_values[6], mean_values[7], mean_values[8], mean_values[9], mean_values[10],
    mean_values[11], mean_values[12], mean_values[13], mean_values[14], mean_values[15],
    mean_values[16], mean_values[17], mean_values[18], mean_values[19], mean_values[20],
    mean_values[21],
    "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
    
    
    # Row 2: Control SD
    4, "Control SD @ follow up", "Control SD @ follow up", "Control SD @ follow up",
    sd_values[1], sd_values[2], sd_values[3], sd_values[4], sd_values[5],
    sd_values[6], sd_values[7], sd_values[8], sd_values[9], sd_values[10],
    sd_values[11], sd_values[12], sd_values[13], sd_values[14], sd_values[15],
    sd_values[16], sd_values[17], sd_values[18], sd_values[19], sd_values[20],
    sd_values[21],
    "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", ""
    
 
  )
  
  # browser()
  # Append the additional rows to the table body
  tbl$table_body <- bind_rows(tbl$table_body, additional_rows)
  
  # Title for each index
  tbl_ipv_title = paste0("**Table : effect of the programs by type of treatment**")
  
  # Final table formatting
  tbl <- tbl %>%
    # Set table caption
    modify_caption(tbl_ipv_title) %>%
    # Modify header for the label column
    modify_header(label = "**Outcome**") %>%
    # Remove all existing footnotes
    modify_footnote(everything() ~ NA) %>%
    modify_footnote(all_stat_cols() ~ NA) %>%
    # Add custom footnote with methodological details
    modify_footnote(label = "Notes: Results presented are OLS estimates that include controls for randomization strata (commune) and, where possible, baseline outcomes.  We control for social promotion intervention. Enumerator fixed effects are included in all regression. We estimate the regressions for the productive beneficiaries aged 18-49 only. Robust standard errors are shown in parentheses, clustered at the village proxy level. *** p < 0.01, ** p < 0.05, * p < 0.1.")
  
  # Print the final table
  tbl
  
}



############################################################################
######### Function for computing regression ################################
############################################################################

# Function to get global estimates for continuous variables
getEstimateGlobal <- function(depvar, curr_treat_var, control_vars, strata_vars, cluster_vars, followup_df){
  
  
  if(curr_treat_var=="treatment_csh_trnsfr"){
    
    # Create formula for base model with full controls
    regmodel_cntrls <- paste0(depvar, " ~ ", curr_treat_var, " + ", paste0(control_vars, collapse = " + "), "| 0 | 0 |", "hhid")
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
    regmodel_cntrls <- paste0(depvar, " ~ ", curr_treat_var, " + ", paste0(control_vars, collapse = " + "), "| 0 | 0 |", "village")
    
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
    regmodel_cntrls <- paste0(depvar, " ~ ", curr_treat_var, " + ", paste0(control_vars, collapse = " + "), "| 0 | 0 |", "village")
    
    # for the case of the pool treatment 
    reg_df <- followup_df %>% 
      filter(reg_hh_pi_mrt==1)
    
    # Estimate both models using felm
    regmodbase_cntrls <- felm(formula=formula(regmodel_cntrls), data = reg_df)
    
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


getEstimate_mechanism <- function(depvar, curr_treat_var, control_vars, strata_vars, cluster_vars, followup){
  
  
  if(curr_treat_var=="treatment_csh_trnsfr"){
    
    regmodel_cntrls <- paste0(depvar, " ~ ", curr_treat_var, " + ", paste0(control_vars, collapse = " + "), "| 0 | 0 |", " hhid")
    
    reg_df <- followup %>% 
      filter(reg_hh_csh_mrt==1)
    
    # Estimate models using felm
    regmodbase_cntrls <- felm(formula(regmodel_cntrls), data = reg_df, keepModel = T, na.action = na.omit) 
    # Extract coefficients from the model
    coef_values <- coef(regmodbase_cntrls)
    
  }else if(curr_treat_var=="treatment_pi"){
    
    regmodel_cntrls <- paste0(depvar, " ~ ", curr_treat_var, " + ", paste0(control_vars, collapse = " + "), "| 0 | 0 |", " village")
    
    reg_df <- followup %>% 
      filter(reg_hh_pi_mrt==1)
    
    # Estimate both models using felm
    regmodbase_cntrls <- felm(formula=formula(regmodel_cntrls), data = reg_df)
    
    # Extract coefficients from the model
    coef_values <- coef(regmodbase_cntrls)
    
  }else{
    # Create formula for base model with full controls
    regmodel_cntrls <- paste0(depvar, " ~ ", curr_treat_var, " + ", paste0(control_vars, collapse = " + "), "| 0 | 0 |", " village")
    
    # for the case of the pool treatment 
    reg_df <- followup %>% 
      filter(reg_hh_pi_mrt==1)
    
    # Estimate both models using felm
    regmodbase_cntrls <- felm(formula=formula(regmodel_cntrls), data = reg_df)
    
    # Extract coefficients from the model
    coef_values <- coef(regmodbase_cntrls)
    
  }
  
  # Return results in tibble format
  out <- tibble(
    results_base = list(regmodbase_cntrls)
  )
  
  out
  
  
}


getEstimateGlobal_het <- function(depvar, curr_treat_var, control_vars, strata_vars, cluster_vars, followup){
  
  
  if(curr_treat_var=="treatment_csh_trnsfr"){
    
    regmodel_cntrls <- paste0(depvar, " ~ ", het_var, " * ", curr_treat_var, " + ", paste0(control_vars, collapse = " + "), "| 0 | 0 |", " hhid")
    
    reg_df <- followup %>% 
      filter(reg_hh_csh_mrt==1)
    
    # Estimate both models using felm
    regmodbase_cntrls <- getEstimatedf_het_csh(formula=formula(regmodel_cntrls), depvar, het_var, curr_treat_var, reg_df)
    
  }else{
    
    regmodel_cntrls <- paste0(depvar, " ~ ", het_var, " * ", curr_treat_var, " + ", paste0(control_vars, collapse = " + "), "| 0 | 0 |", " village")
    
    reg_df <- followup %>% filter(reg_hh_pi_mrt==1)
    
    # Estimate both models using felm
    regmodbase_cntrls <- getEstimatedf_het_pi(formula=formula(regmodel_cntrls), depvar, het_var, curr_treat_var, reg_df)
    
  }
  regmodbase_cntrls
}





getEstimatedf_het_csh <- function(formula, depvar, het_var, curr_treat_var, dfcurr, R = 1000) {
  
  # Fit the model using fixed effects
  mod <- felm(formula, data = dfcurr, keepModel = T, na.action = na.omit)
  ### browser()
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
getEstimatedf_het_pi <- function(formula, depvar, curr_treat_var, dfcurr, R = 1000) {
  
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

getEstimateGlobal_CF <- function(depvar, treat_var, followup){
  
  if(treat_var=="treatment_csh_trnsfr"){
    
    reg_df <- followup %>%
      filter(reg_hh_csh_mrt==1) 
    
    # Extract clustering variable (village) for clustered standard errors
    clusters <- reg_df %>%  
      dplyr::select(hhid) %>% 
      pull()
    
    
  }else{
    reg_df <- followup %>%
      filter(reg_hh_pi_mrt==1) 
    
    # Extract clustering variable (village) for clustered standard errors
    clusters <- reg_df %>%  
      dplyr::select(village) %>% 
      pull()
  }
  
  # Y: Extract outcome variable (IPV severity index) from the filtered dataset
  Y <- reg_df %>%  
    dplyr::select(!!sym(depvar)) %>% 
    pull() # Outcome variable
  
  # W: Extract treatment variable from the filtered dataset
  W <- reg_df %>%  
    dplyr::select(!!sym(treat_var))%>% 
    pull()   # Treatment var
  
  # X: Create matrix of covariates by removing other variables
  X <- reg_df %>%  
    dplyr::select(-c(!!sym(treat_var),!!sym(depvar))) %>% 
    as.matrix()  # Covariates matrix
  
  # Fit the causal forest
  cf_model <- causal_forest(X=X, Y=Y, W=W, clusters = clusters, num.trees = 4000,
                            mtry = 700, honesty = TRUE, # 1/3 of variables mtry = 700, 
                            min.node.size = 10,
                            tune.parameters = c("sample.fraction", "mtry", "min.node.size", 
                                                "honesty.fraction", "honesty.prune.leaves", 
                                                "alpha", "imbalance.penalty"),
                            tune.num.trees = 500,
                            tune.num.reps = 500,
                            tune.num.draws = 1000)
  
  
  # Estimate the average treatment effect (ATE)
  ATE = average_treatment_effect(cf_model) # Compute the ATE
  pval = 2 * pnorm(-abs(ATE[1]/ATE[2])) # Compute the p-value
  
  # Print the results of the ATE
  print(sprintf("The estimated ATE is %s, the standard error is %s and the pvalue is %s. The outcome is var is %s and the treatment variable %s.", 
                round(ATE[1],2),round(ATE[2],2), round(pval,2), depvar, treat_var))
  
  # Predict individual treatment effects
  cf_predictions <- predict(cf_model, X, estimate.variance = TRUE)
  
  # Create a tibble with results
  out <- tibble(
    hhid = reg_df$hhid,
    estimated_ate_value = ATE[1],
    estimated_ate_std = ATE[2],
    estimated_ate_pvalue = pval,
    predicted_val = cf_predictions$predictions,
    predicted_std = cf_predictions$variance.estimates
  )
  
  out
  
}


getEstimateGlobal_ML_het <- function(depvar, curr_treat_var, het_var, control_vars, strata_vars, cluster_vars, followup){
  
  # Create formula for base model with full controls
  # regmodel_cntrls <- paste0(depvar, " ~ ", het_var, " * ", curr_treat_var, " + ", paste0(control_vars, collapse = " + "), " + ", paste0(strata_vars, collapse = " + "), "| 0 | 0 |", cluster_vars)
  
  if(curr_treat_var=="treatment_csh_trnsfr"){
    
    regmodel_cntrls <- paste0(depvar, " ~ ", het_var, " * ", curr_treat_var, " + ", paste0(control_vars, collapse = " + "), "| 0 | 0 |", " hhid")
    
    reg_df <- followup %>%
      # filter(!!sym(curr_treat_var)!="None") %>% 
      filter(reg_hh_csh_mrt==1) %>% 
      distinct(hhid,village,hhh_fem_bl, hhh_poly, hhh_edu, .keep_all = TRUE)
    
    # Estimate both models using felm
    reg_results <- getEstimatedf_ML_het_csh(formula=formula(regmodel_cntrls), depvar, het_var, curr_treat_var, reg_df)
    
  }else{
    regmodel_cntrls <- paste0(depvar, " ~ ", het_var, " * ", curr_treat_var, " + ", paste0(control_vars, collapse = " + "), "| 0 | 0 |", " village")
    
    reg_df <- followup %>%
      filter(reg_hh_pi_mrt==1) %>% 
      distinct(hhid,village,hhh_fem_bl, hhh_poly, hhh_edu, .keep_all = TRUE)
    
    if(het_var=="decile_baseline_ipv"){
      
      # Estimate both models using felm
      reg_results <- getEstimatedf_ML_het_pi_dec(formula=formula(regmodel_cntrls), depvar, het_var, curr_treat_var, reg_df)
    }else if(het_var=="quartile_baseline_ipv"){
      
      # Estimate both models using felm
      reg_results <- getEstimatedf_ML_het_pi_quan(formula=formula(regmodel_cntrls), depvar, het_var, curr_treat_var, reg_df)
    }else{
      
      # Estimate both models using felm
      reg_results <- getEstimatedf_ML_het_pi_med(formula=formula(regmodel_cntrls), depvar, het_var, curr_treat_var, reg_df)
    }
    
  }
  reg_results
}

# Function to estimate the model with squared exposition variable
getEstimatedf_ML_het_pi_med <- function(formula, depvar, curr_treat_var, dfcurr, R = 1000) {
  
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


getEstimatedf_ML_het_csh <- function(formula, depvar, curr_treat_var, dfcurr, R = 1000) {
  
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
getEstimatedf_ML_het_pi_quan <- function(formula, depvar, curr_treat_var, dfcurr, R = 1000) {
  
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
getEstimatedf_ML_het_pi_dec <- function(formula, depvar, het_var, curr_treat_var, dfcurr, R = 1000) {
  
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
