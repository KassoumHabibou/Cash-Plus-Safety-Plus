
############################################################################
######### Function for computing regression ################################
############################################################################

# Function to get global estimates for continuous variables
getEstimateGlobal <- function(depvar, curr_treat_var, control_vars, strata_vars, cluster_vars, followup_df){
  
  # Create formula for base model with full controls
  regmodel_cntrls <- paste0(depvar, " ~ ", curr_treat_var, " + ", paste0(control_vars, collapse = " + "), " + ", paste0(strata_vars, collapse = " + "), "| surveyor | 0 |", cluster_vars)
 
  if(curr_treat_var=="treatment_csh_trnsfr"){
    reg_df <- followup_df %>% filter(reg_hh_csh_mrt==1)
    
    # Estimate both models using felm
    regmodbase_cntrls <- felm(formula=formula(regmodel_cntrls), data = reg_df)
    
    coef_ftest_1 = ""
    coef_ftest_2 = ""
    coef_ftest_3 = ""
    
  }else if(curr_treat_var=="treatment_pi"){
    
    reg_df <- followup_df %>% 
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


getEstimateGlobal_het <- function(depvar, curr_treat_var, het_var, control_vars, strata_vars, cluster_vars, followup){
  
  # Create formula for base model with full controls
  regmodel_cntrls <- paste0(depvar, " ~ ", het_var, " * ", curr_treat_var, " + ", paste0(control_vars, collapse = " + "), " + ", paste0(strata_vars, collapse = " + "), "| surveyor | 0 |", cluster_vars)
  
  
  if(curr_treat_var=="treatment_csh_trnsfr"){
   
    reg_df <- followup %>% 
      filter(reg_hh_csh_mrt==1)
    
    # Estimate both models using felm
    regmodbase_cntrls <- getEstimatedf_het_csh(formula=formula(regmodel_cntrls), depvar, het_var, curr_treat_var, reg_df)
    
  }else{
    reg_df <- followup %>% filter(reg_hh_pi_mrt==1)
    
    # Estimate both models using felm
    regmodbase_cntrls <- getEstimatedf_het_pi(formula=formula(regmodel_cntrls), depvar, het_var, curr_treat_var, reg_df)
    
  }
  regmodbase_cntrls
}

getEstimatedf_het_csh <- function(formula, depvar, het_var, curr_treat_var, dfcurr, R = 1000) {
  
  # Fit the model using fixed effects
  mod <- felm(formula, data = dfcurr, keepModel = T, na.action = na.omit)
  #browser()
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
  
  # browser()
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


# Function to estimate global effects with interaction term of exposure and employment variable
getEstimateGlobalemp <- function(depvar, empvar, expvar, df1, df2, type = 1) {
  
  # Define baseline and sibling models based on the 'type' argument
  if (type == 1) {
    basemodelEmp <- formula(paste0(depvar, " ~ ", expvar, " * ", empvar,
                                   " + kidtwin_rec + KIDSEX + KIDBORD + KIDCURAGE + ETHNICITYNG + EDYRTOTAL + HUSEDYRS + motherAge + motherAgesqr + womwrkAgri + huswrkAgri + HHMEMTOTAL + HHKIDLT5 + WEALTHQ + religion_rec | interview_month + KIDBIRTHMO + KIDBIRTHYR + YEAR + CLUSTERNO | 0 | CLUSTERNO"))
    
    basemodelEmp_s <- formula(paste0(depvar, " ~ ", expvar, ":", empvar,
                                     " + ", expvar, " + kidtwin_rec + KIDSEX + KIDBORD + KIDCURAGE | interview_month + KIDBIRTHMO + KIDBIRTHYR + IDHSPID | 0 | CLUSTERNO"))
  } else {
    basemodelEmp <- formula(paste0(depvar, " ~ ", expvar, " * ", empvar,
                                   " + kidtwin_rec + KIDSEX + KIDBORD + KIDCURAGE + ETHNICITYNG + EDYRTOTAL + HUSEDYRS + motherAge + motherAgesqr + womwrkAgri + huswrkAgri + HHMEMTOTAL + HHKIDLT5 + WEALTHQ + religion_rec | interview_month + KIDBIRTHMO + KIDBIRTHYR + YEAR + CLUSTERNO | 0 | CLUSTERNO"))
    
    basemodelEmp_s <- formula(paste0(depvar, " ~ ", expvar, ":", empvar,
                                     " + ", expvar, " + kidtwin_rec + KIDSEX + KIDBORD + KIDCURAGE | interview_month + KIDBIRTHMO + KIDBIRTHYR + IDHSPID | 0 | CLUSTERNO"))
  }
  
  # Bind the results of baseline and sibling models
  temp_res <- bind_rows(
    getEstimatedf(basemodelEmp, empvar, expvar, dfcurr = df1) %>% mutate(name = "baseline"),
    getEstimatedf(basemodelEmp_s, empvar, expvar, dfcurr = df2) %>% mutate(name = "sibling")
  )
  
  # Return the combined results
  temp_res
}

# Function to estimate the global model with interaction terms involving URBAN variable
getEstimateGlobal_urban <- function(depvar, empvar, expvar, df1, df2) {
  
  # Define the baseline model formula
  basemodelEmp <- formula(paste0(depvar, "~", expvar, "*", empvar, "*URBAN + kidtwin_rec +
                                 KIDSEX + KIDBORD + KIDCURAGE + EDYRTOTAL + ETHNICITYNG + 
                                 HUSEDYRS + motherAge + motherAgesqr + womwrkAgri + 
                                 huswrkAgri + HHMEMTOTAL + HHKIDLT5 + WEALTHQ  + 
                                 religion_rec| KIDBIRTHMO + KIDBIRTHYR + interview_month  + 
                                 YEAR + CLUSTERNO | 0 | CLUSTERNO"))
  
  # Define the sibling model formula
  #basemodelEmp_s <- formula(paste0(depvar, "~", expvar, ":", empvar, ":URBAN + ", expvar, ":", empvar, "+", expvar, ":URBAN + ", expvar, " + kidtwin_rec + KIDSEX + KIDCURAGE | KIDBIRTHMO + KIDBIRTHYR + interview_month  + IDHSPID | 0 | CLUSTERNO"))
  basemodelEmp_s <- formula(paste0(depvar, "~",expvar, ":", empvar, ":URBAN + ",expvar, ":URBAN + ", 
                                   expvar, ":", empvar, " + " , expvar, " + kidtwin_rec + 
                                   KIDSEX + KIDBORD + KIDCURAGE | KIDBIRTHMO + 
                                   KIDBIRTHYR + interview_month  + IDHSPID | 0 | CLUSTERNO"))
  
  # Combine results of the baseline and sibling models
  temp_res <- bind_rows(
    getEstimatedf_urban(basemodelEmp, empvar, expvar, dfcurr = df1) %>% mutate(name = "baseline"),
    getEstimatedf_urban(basemodelEmp_s, empvar, expvar, dfcurr = df2) %>% mutate(name = "sibling")
  )
  
  # Return the combined results
  temp_res
  
  
}

# Function to estimate the model with interaction terms involving URBAN variable
getEstimatedf_urban <- function(formula, empvar, expvar, dfcurr, R = 1000) {
  
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
  matSelect <- matrix(0, nrow = 4, ncol = length(coef(mod)))
  colnames(matSelect) <- names(coef(mod))
  
  # Define crossed variables for interaction terms
  crosvar1 <- paste0(expvar, ":URBAN1", sep = "")
  crosvar2 <- paste0(expvar, ":", empvar, "1", sep = "")
  crosvar3 <- paste0(expvar, ":", empvar, "1:URBAN1", sep = "")
  
  # Populate the matrix for coefficient selection
  matSelect[, expvar] <- c(1, 1, 1, 1)
  matSelect[, crosvar1] <- c(0, 1, 0, 1)
  matSelect[, crosvar2] <- c(0, 0, 1, 1)
  matSelect[, crosvar3] <- c(0, 0, 0, 1)
  
  # Calculate the coefficients
  coefs <- as.numeric(matSelect %*% mat_coef)
  modU_CI <- t(matSelect %*% t(modU))
  
  # Function to access the coefficients
  CI <- t(apply(modU_CI, 2, quantile, probs = alpha, na.rm = TRUE))
  colnames(CI) <- paste0("CI", parse_number(colnames(CI)))
  
  # Extract relevant informations
  vect_temp <- mod$model %>% pull(empvar)
  vect_temp_2 <- mod$model %>% filter(numberEventpostBHR > 0) %>% pull(empvar)
  
  # Create a tibble with results
  out <- tibble(
    empvar = empvar,
    estimate = c("No", "No", "Yes", "Yes"),
    typeEffect = c("Rural", "Urban", "Rural", "Urban"),
    pe = coefs,
    N = nrow(mod$model),
    Ntype = c(as.numeric(table(vect_temp))[1], as.numeric(table(vect_temp))[1], as.numeric(table(vect_temp))[2], as.numeric(table(vect_temp))[2]),
    Ntype_treat = c(as.numeric(table(vect_temp_2))[1], as.numeric(table(vect_temp_2))[1], as.numeric(table(vect_temp_2))[2], as.numeric(table(vect_temp_2))[2]),
    r.squared = summary(mod)$r.squared
  )
  out <- bind_cols(out, as.tibble(CI))
  
  # Return the tibble with results
  out
}




# Function to estimate the global model with interaction terms involving the religion variable
getEstimateGlobal_religion <- function(depvar, empvar, expvar, df1, df2) {
  
  # Define the baseline model formula
  basemodelEmp <- formula(paste0(depvar, "~", expvar, "*", empvar, " + kidtwin_rec + KIDSEX + KIDBORD + KIDCURAGE + EDYRTOTAL + ETHNICITYNG + HUSEDYRS + motherAge + motherAgesqr + womwrkAgri + huswrkAgri + HHMEMTOTAL + HHKIDLT5 + WEALTHQ| KIDBIRTHMO + KIDBIRTHYR + interview_month  + YEAR + CLUSTERNO | 0 | CLUSTERNO"))
  
  # Define the sibling model formula
  basemodelEmp_s <- formula(paste0(depvar, "~", expvar, ":", empvar, " + ", expvar, " + kidtwin_rec + KIDSEX + KIDBORD + KIDCURAGE | KIDBIRTHMO + KIDBIRTHYR + interview_month  + IDHSPID | 0 | CLUSTERNO"))
  
  # Combine results of the baseline and sibling models
  resultsbasemodelEmp <- felm(basemodelEmp, data = df1)
  resultsbasemodelEmp_s <- felm(basemodelEmp_s, data = df2)
  
  
  out <- tibble(
    model = "Model",
    Results_base = list(resultsbasemodelEmp),
    Results_sibling = list(resultsbasemodelEmp_s)
  )
  
  out
}

# Function to estimate the model with interaction terms involving the religion variable
getEstimatedf_religion <- function(formula, empvar, expvar, dfcurr, R = 1000) {
  
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
  matSelect <- matrix(0, nrow = 4, ncol = length(coef(mod)))
  colnames(matSelect) <- names(coef(mod))
  
  # Define crossed variables for interaction terms
  crosvar1 <- paste0(expvar, ":religion_recbinary1", sep = "")
  crosvar2 <- paste0(expvar, ":", empvar, "1", sep = "")
  crosvar3 <- paste0(expvar, ":", empvar, "1:religion_recbinary1", sep = "")
  
  # Populate the matrix for coefficient selection
  matSelect[, expvar] <- c(1, 1, 1, 1)
  matSelect[, crosvar1] <- c(0, 1, 0, 1)
  matSelect[, crosvar2] <- c(0, 0, 1, 1)
  matSelect[, crosvar3] <- c(0, 0, 0, 1)
  
  # Calculate the coefficients
  coefs <- as.numeric(matSelect %*% mat_coef)
  modU_CI <- t(matSelect %*% t(modU))
  
  # Function to access the coefficients
  CI <- t(apply(modU_CI, 2, quantile, probs = alpha, na.rm = TRUE))
  colnames(CI) <- paste0("CI", parse_number(colnames(CI)))
  
  # Extract relevant informations
  vect_temp <- mod$model %>% pull(empvar)
  vect_temp_2 <- mod$model %>% filter(numberEventpostBHR > 0) %>% pull(empvar)
  
  # Create a tibble with results
  out <- tibble(
    empvar = empvar,
    estimate = c("No", "No", "Yes", "Yes"),
    typeEffect = c("Christian", "Muslim", "Christian", "Muslim"),
    pe = coefs,
    N = nrow(mod$model),
    Ntype = c(as.numeric(table(vect_temp))[1], as.numeric(table(vect_temp))[1], as.numeric(table(vect_temp))[2], as.numeric(table(vect_temp))[2]),
    Ntype_treat = c(as.numeric(table(vect_temp_2))[1], as.numeric(table(vect_temp_2))[1], as.numeric(table(vect_temp_2))[2], as.numeric(table(vect_temp_2))[2]),
    r.squared = summary(mod)$r.squared
  )
  out <- bind_cols(out, as.tibble(CI))
  
  # Return the tibble with results
  out
}


getEstimateGlobal_athome <- function(depvar, empvar, expvar, df1, df2) {
  
  # Define the baseline model formula
  basemodelEmp <- formula(paste0(depvar, "~", expvar, "*", empvar, " + kidtwin_rec + KIDSEX + KIDBORD + KIDCURAGE + EDYRTOTAL + ETHNICITYNG + HUSEDYRS + motherAge + motherAgesqr + womwrkAgri + huswrkAgri + HHMEMTOTAL + HHKIDLT5 + WEALTHQ + religion_rec| KIDBIRTHMO + KIDBIRTHYR + interview_month  + YEAR + CLUSTERNO | 0 | CLUSTERNO"))
  
  # Define the sibling model formula
  basemodelEmp_s <- formula(paste0(depvar, "~", expvar, ":", empvar, " + ", expvar, " + kidtwin_rec + KIDSEX + KIDBORD + KIDCURAGE | KIDBIRTHMO + KIDBIRTHYR + interview_month  + IDHSPID | 0 | CLUSTERNO"))
  
  
  resultsbasemodelEmp <- felm(basemodelEmp, data = df1)
  resultsbasemodelEmp_s <- felm(basemodelEmp_s, data = df2)
  
  
  out <- tibble(
    model = "Model",
    Results_base = list(resultsbasemodelEmp),
    Results_sibling = list(resultsbasemodelEmp_s)
  )
  
  out
}


# Function to estimate global effects with interaction between exposure, employment, and heterogeneity
# Parameters:
#   - depvar: dependent variable
#   - empvar: employment variable
#   - expvar: exposure variable
#   - df1: data frame for the first group
#   - df2: data frame for the second group (sibling)
#   - type: type of model (1 or 2)
getEstimateGlobalemp_het <- function(depvar, empvar, expvar, df1, df2) {
  
  # Define baseline and sibling models based on the type
  
  basemodelEmp <- formula(paste0(depvar, "~ ", expvar, "*", empvar, "  + kidtwin_rec + KIDSEX + KIDBORD + KIDCURAGE  + ETHNICITYNG + EDYRTOTAL + HUSEDYRS + motherAge + motherAgesqr + womwrkAgri + huswrkAgri + HHMEMTOTAL + HHKIDLT5 | KIDBIRTHMO + KIDBIRTHYR + interview_month  + YEAR + CLUSTERNO | 0 | CLUSTERNO"))
  
  basemodelEmp_s <- formula(paste0(depvar, "~ ", expvar, ":", empvar, " + ", expvar, "  + kidtwin_rec + KIDSEX + KIDBORD + KIDCURAGE | KIDBIRTHMO + KIDBIRTHYR + interview_month  + IDHSPID | 0 | CLUSTERNO"))
  
  
  # # Bind results from the two models
  temp_res <- bind_rows(
    getEstimatedf(basemodelEmp, empvar, expvar, dfcurr = df1) %>% mutate(name = "baseline"),
    getEstimatedf(basemodelEmp_s, empvar, expvar, dfcurr = df2) %>% mutate(name = "sibling")
  )
  
  # # Return the results
  temp_res
  
}
# Function to estimate global effects on mortality
getEstimateGlobalemort <- function(depvar, empvar, expvar, df1, df2) {
  
  # Define the baseline model formula
  basemodelEmp <- formula(paste0(depvar,"~ ",expvar,"*",empvar," + kidtwin_rec + KIDSEX + KIDBORD  + ETHNICITYNG + EDYRTOTAL + HUSEDYRS + motherAge + motherAgesqr + womwrkAgri + huswrkAgri + HHMEMTOTAL + HHKIDLT5 | KIDBIRTHMO + KIDBIRTHYR + interview_month  + YEAR + CLUSTERNO | 0 | CLUSTERNO"))
  
  
  basemodelEmp_s <- formula(paste0(depvar, "~ ", expvar, ":", empvar, " + ", expvar, "  + kidtwin_rec + KIDSEX + KIDBORD | KIDBIRTHMO + KIDBIRTHYR + interview_month  + IDHSPID | 0 | CLUSTERNO"))
  
  
  # # Bind results from the two models
  temp_res <- bind_rows(
    getEstimatedf(basemodelEmp, empvar, expvar, dfcurr = df1) %>% mutate(name = "baseline"),
    getEstimatedf(basemodelEmp_s, empvar, expvar, dfcurr = df2) %>% mutate(name = "sibling")
  )
  
  #  Return the results
  temp_res
}



# Function to estimate employment selection effects
getEstimateGlobalempsel <- function(depvar, empvar, expvar, df1, df2){
  # Create formula for baseline model with employment interaction
  basemodelEmp <- formula(paste0(depvar, "~", expvar, "*", empvar,
                                 " + kidtwin_rec + KIDBORD + KIDCURAGE + EDYRTOTAL + ETHNICITYNG + HUSEDYRS + ",
                                 "motherAge + motherAgesqr + womwrkAgri + huswrkAgri + HHMEMTOTAL + HHKIDLT5 + ",
                                 "WEALTHQ + religion_rec| ",
                                 "KIDBIRTHMO + KIDBIRTHYR + interview_month + YEAR + CLUSTERNO | 0 | CLUSTERNO"))
  
  # Create formula for sibling fixed effects model
  basemodelEmp_s <- formula(paste0(depvar, "~", expvar, ":", empvar, " + ", expvar,
                                   " + kidtwin_rec + KIDBORD + KIDCURAGE | ",
                                   "KIDBIRTHMO + KIDBIRTHYR + interview_month + IDHSPID | 0 | CLUSTERNO"))
  
  # Combine baseline and sibling model results
  temp_res <- bind_rows(
    getEstimatedf(basemodelEmp, empvar, expvar, dfcurr = df1) %>% 
      mutate(name = "baseline"),
    getEstimatedf(basemodelEmp_s, empvar, expvar, dfcurr = df2) %>% 
      mutate(name = "sibling")
  )
  
  temp_res
}

# Function to estimate drought interaction effects
getEstimateGlobaldrought <- function(depvar, empvar, expvar, df1, df2){
  # Create formula for baseline drought model
  basemodeldrought <- formula(paste0(depvar, "~", expvar, "*", empvar,
                                     " + kidtwin_rec + KIDBORD + KIDCURAGE + KIDSEX + EDYRTOTAL + ETHNICITYNG + ",
                                     "HUSEDYRS + motherAge + motherAgesqr + womwrkAgri + huswrkAgri + HHMEMTOTAL + ",
                                     "HHKIDLT5 + WEALTHQ + religion_rec + drought_indicator| ",
                                     "KIDBIRTHMO + KIDBIRTHYR + interview_month + YEAR + CLUSTERNO | 0 | CLUSTERNO"))
  
  # Create formula for sibling fixed effects drought model
  basemodeldrought_s <- formula(paste0(depvar, "~", expvar, ":", empvar, " + ", expvar,
                                       " + kidtwin_rec + KIDBORD + KIDSEX + KIDCURAGE + drought_indicator | ",
                                       "KIDBIRTHMO + KIDBIRTHYR + interview_month + IDHSPID | 0 | CLUSTERNO"))
  
  # Combine baseline and sibling model results
  temp_res <- bind_rows(
    getEstimatedf(basemodeldrought, empvar, expvar, dfcurr = df1) %>% 
      mutate(name = "baseline"),
    getEstimatedf(basemodeldrought_s, empvar, expvar, dfcurr = df2) %>% 
      mutate(name = "sibling")
  )
  
  temp_res
}

# Function to estimate regional employment effects
getEstimateGlobalempreg <- function(depvar, empvar, expvar, df1){
  # Create formula for regional employment model
  basemodelEmpreg <- formula(paste0(depvar, "~ ", expvar, "*", empvar,
                                    " + womwrkAgri + huswrkAgri + motherAge + motherAgesqr + ETHNICITYNG + ",
                                    "HHMEMTOTAL + EDUCLVL + WEALTHQ + religion_rec | ",
                                    "YEAR + CLUSTERNO | 0 | CLUSTERNO"))
  
  
  # Get estimates for baseline model only
  temp_res <- bind_rows(
    getEstimatedf(basemodelEmpreg, empvar, expvar, df1) %>% 
      mutate(name = "baseline1")
  )
  
  temp_res
}

# Function to estimate the effect of women's autonomy on a given variable
getEstimateGlobalWomenAut <- function(empvar, expvar, df3) {
  
  # Define the baseline model formula for women's autonomy
  basemodelEmp <- formula(paste0(empvar,"~ ",expvar," + womwrkAgri + huswrkAgri + motherAge + ETHNICITYNG + motherAgesqr + HHMEMTOTAL + HHKIDLT5 + WEALTHQ  + religion_rec | YEAR + stateNameCorrected "))
  basemodelEmplm <- formula(paste0(empvar,"~ ",expvar," + womwrkAgri + huswrkAgri + motherAge + ETHNICITYNG + motherAgesqr + HHMEMTOTAL + HHKIDLT5 + WEALTHQ  + religion_rec | YEAR + stateNameCorrected | 0 | stateNameCorrected"))
  
  
  # Fit fixed-effects GLM using fixest package
  mod1 <- fixest::feglm(basemodelEmp, data = df3, cluster = "stateNameCorrected", family = "binomial")
  mod2 <- felm(basemodelEmplm, data = df3)
  
  # Return a tibble with the results
  out <- tibble(
    empvar = empvar,
    model = c("baseModelLM","baseModelL"),
    Results_base = list(mod2, mod1)
  )
  
  out
}



# Function to make a placebo test of the correlation of women's autonomy and conflict
getEstimateWomenAutPlace <- function(empvar, expvar, df3) {
  
  # Define the baseline model formula for women's autonomy  + religion_rec 
  basemodelEmp <- formula(paste0(empvar,"~ ",expvar," + womwrkAgri + huswrkAgri + motherAge + ETHNICITYNG + motherAgesqr + HHMEMTOTAL + HHKIDLT5 + WEALTHQ  + religion_rec "))
  
  # Bind the results from the baseline model
  temp_res <- bind_rows(
    getEstimateAut(basemodelEmp, empvar, expvar, dfcurr = df3) %>% mutate(name="Baseline"),
  )
  
  # Return the results
  temp_res
}

# Function to estimate the effect of a variable on migration
getEstimateMigration <- function(empvar, expvar, df3) {
  
  # Define the baseline model formula for migration
  basemodelEmp <- formula(paste0("migrant_binary ~ ",empvar,"*",expvar," + womwrkAgri + huswrkAgri + motherAge + ETHNICITYNG + motherAgesqr + HHMEMTOTAL + HHKIDLT5 + WEALTHQ  + religion_rec | YEAR + stateNameCorrected "))
  
  # Bind the results from the baseline model
  temp_res <- bind_rows(
    getEstimateAut(basemodelEmp, empvar, expvar, dfcurr = df3) %>% mutate(name="Baseline"),
  )
  
  # Return the results
  temp_res
}

# Function to estimate the effect using fixed-effects GLM
getEstimateAut <- function(formula, empvar, expvar, dfcurr, R=1000) {
  
  # Fit fixed-effects GLM using fixest package
  mod <- fixest::feglm(formula, data = dfcurr, cluster = "CLUSTERNO", family = "binomial")
  mod <- felm(formula, data = dfcurr)
  # Return a tibble with the results
  out <- tibble(
    empvar = empvar,
    model = c("baseModel"),
    Results_base = list(mod)
  )
  
  out
}

# Function to estimate global effects with and without the specified exposure variable in prepost conflict periods
getEstimateGlobalprepost <- function(depvar, empvar, df1, df2){
  
  # Define baseline model formula including the exposure variable
  basemodelEmp1 <- formula(paste0(depvar, " ~ numberEventprepost*", empvar, " + kidtwin_rec + KIDSEX + KIDBORD + KIDCURAGE + ETHNICITYNG + womwrkAgri + huswrkAgri + HHMEMTOTAL + HHKIDLT5 + EDYRTOTAL + HUSEDYRS + motherAge + motherAgesqr + WEALTHQ  + religion_rec | KIDBIRTHMO + KIDBIRTHYR + interview_month  + YEAR + CLUSTERNO | 0 | CLUSTERNO"))
  
  # Define sibling model formula including the exposure variable
  basemodelEmp_s1 <- formula(paste0(depvar, " ~ numberEventprepost:", empvar, " + numberEventprepost + kidtwin_rec + KIDSEX + KIDBORD + KIDCURAGE | KIDBIRTHMO + KIDBIRTHYR + interview_month  + IDHSPID | 0 | CLUSTERNO"))
  
  # Bind the results of estimating the specified models
  temp_res <- bind_rows(
    
    getEstimatedf(basemodelEmp1, empvar, expvar = "numberEventprepost", dfcurr = df1) %>% mutate(name = "baseline", typeEffect = "with"),
    getEstimatedf(basemodelEmp_s1, empvar, expvar = "numberEventprepost", dfcurr = df2) %>% mutate(name = "sibling", typeEffect = "with")
  )
  
  # Return the combined results
  temp_res
}

# Function to get estimates for a global model with interaction terms involving binary order variable
getEstimateGlobalorder <- function(depvar, empvar, expvar, df1, df2){
  # Define the baseline model formula
  basemodelEmp <- formula(paste0(depvar, "~", expvar, "*", empvar, "*orderbinary + KIDSEX  + KIDCURAGE  + EDYRTOTAL + ETHNICITYNG + HUSEDYRS + motherAge + motherAgesqr + womwrkAgri + huswrkAgri + HHMEMTOTAL + HHKIDLT5 + WEALTHQ  + religion_rec | KIDBIRTHMO + KIDBIRTHYR + interview_month  + YEAR + CLUSTERNO | 0 | CLUSTERNO"))
  
  # Define the sibling model formula
  basemodelEmp_s <- formula(paste0(depvar, "~", expvar, ":", empvar, ":orderbinary + ", expvar, ":", empvar, "+", expvar, ":orderbinary + ", expvar, " + kidtwin_rec + KIDSEX  + KIDCURAGE| KIDBIRTHMO + KIDBIRTHYR + interview_month  + IDHSPID | 0 | CLUSTERNO"))
  
  # Combine results for baseline and sibling models
  temp_res <- bind_rows(
    getEstimatedforder(basemodelEmp, empvar, expvar, dfcurr = df1) %>% mutate(name = "baseline"),
    getEstimatedforder(basemodelEmp_s, empvar, expvar, dfcurr = df2) %>% mutate(name = "sibling") 
  )
  
  temp_res
}

# Function to get estimates using fixed effects regression with interaction terms
getEstimatedforder <- function(formula, empvar, expvar, dfcurr, R = 1000){
  # Fit fixed effects regression model
  mod <- felm(formula, data = dfcurr, keepModel = T, na.action = na.omit)
  vcmod <- vcov(mod)
  
  # Generate random draws from the multivariate normal distribution
  modU <- MASS::mvrnorm(R, mu = coef(mod), Sigma = vcmod)
  
  # Define alpha levels for confidence intervals
  alpha <- c(.025, .05, .95, .975)
  
  # Create a matrix to select coefficients for interaction terms
  matSelect <- matrix(0, nrow = 4, ncol = length(coef(mod)))
  colnames(matSelect) <- names(coef(mod))
  
  # Define variable names for interaction terms
  crosvar1 <- paste0(expvar, ":orderbinary1", sep = "")
  crosvar2 <- paste0(expvar, ":", empvar, "1", sep = "")
  crosvar3 <- paste0(expvar, ":", empvar, "1:orderbinary1", sep = "")
  
  # Assign values to the matrix to select coefficients
  matSelect[, expvar] <- c(1, 1, 1, 1)
  matSelect[, crosvar1] <- c(0, 1, 0, 1)
  matSelect[, crosvar2] <- c(0, 0, 1, 1)
  matSelect[, crosvar3] <- c(0, 0, 0, 1)
  
  # Calculate coefficients, confidence intervals, and other statistics
  coefs <- as.numeric(matSelect %*% coef(mod))
  modU_CI <- t(matSelect %*% t(modU))
  
  # Compute confidence intervals
  CI <- t(apply(modU_CI, 2, quantile, probs = alpha, na.rm = TRUE))
  colnames(CI) <- paste0("CI", parse_number(colnames(CI)))
  
  # Extract relevant variables from the model
  vect_temp <- mod$model
  vect_temp <- vect_temp %>% pull(empvar)
  vect_temp_2 <- mod$model %>% filter(numberEventpostBHR > 0) %>% pull(empvar)
  
  # Create a tibble with results
  out <- tibble(
    empvar = empvar,
    estimate = c("No", "No", "Yes", "Yes"),
    typeEffect = c("2 and more", "1", "2 and more", "1"),
    pe = coefs,
    N = nrow(mod$model),
    Ntype = c(as.numeric(table(vect_temp))[1], as.numeric(table(vect_temp))[1], as.numeric(table(vect_temp))[2], as.numeric(table(vect_temp))[2]),
    Ntype_treat = c(as.numeric(table(vect_temp_2))[1], as.numeric(table(vect_temp_2))[1], as.numeric(table(vect_temp_2))[2], as.numeric(table(vect_temp_2))[2]),
    r.squared = summary(mod)$r.squared
  )
  out <- bind_cols(out, as.tibble(CI))
  
  out
}

# Function to get estimates for a global model with interaction terms involving age groups
getEstimateGlobalageg <- function(depvar, empvar, expvar, df1, df2){
  # Define the baseline model formula
  basemodelEmp <- formula(paste0(depvar, "~", expvar, "*", empvar, "*agegroup + KIDBORD + KIDSEX  + KIDCURAGE + EDYRTOTAL + ETHNICITYNG + HUSEDYRS + motherAge + motherAgesqr + womwrkAgri + huswrkAgri + HHMEMTOTAL + HHKIDLT5 + WEALTHQ  + religion_rec | KIDBIRTHMO + KIDBIRTHYR + interview_month  + YEAR + CLUSTERNO | 0 | CLUSTERNO"))
  
  # Define the sibling model formula
  basemodelEmp_s <- formula(paste0(depvar, "~", expvar, ":", empvar, ":agegroup + ", expvar, ":", empvar, "+", expvar, ":agegroup + ", expvar, " + kidtwin_rec  + KIDBORD + KIDSEX| KIDBIRTHMO + KIDBIRTHYR + interview_month  + IDHSPID | 0 | CLUSTERNO"))
  
  # Combine results for baseline and sibling models
  temp_res <- bind_rows(
    getEstimatedfageg(basemodelEmp, empvar, expvar, dfcurr = df1) %>% mutate(name = "baseline"),
    getEstimatedfageg(basemodelEmp_s, empvar, expvar, dfcurr = df2) %>% mutate(name = "sibling") 
  )
  
  temp_res
}



# Function to get estimates using fixed effects regression with interaction terms involving age groups
getEstimatedfageg <- function(formula, empvar, expvar, dfcurr, R = 1000){
  
  # Fit fixed effects regression model
  mod <- felm(formula, data = dfcurr, keepModel = T, na.action = na.omit)
  vcmod <- vcov(mod)
  
  # Define alpha levels for confidence intervals
  alpha <- c(.025, .05, .95, .975)
  
  # Generate random draws from the multivariate normal distribution
  modU <- MASS::mvrnorm(R, mu = coef(mod), Sigma = vcmod)
  
  # Create a matrix to select coefficients for interaction terms
  matSelect <- matrix(0, nrow = 4, ncol = length(coef(mod)))
  colnames(matSelect) <- names(coef(mod))
  
  # Define variable names for interaction terms
  crosvar1 <- paste0(expvar, ":agegroup1", sep = "")
  crosvar2 <- paste0(expvar, ":", empvar, "1", sep = "")
  crosvar3 <- paste0(expvar, ":", empvar, "1:agegroup1", sep = "")
  
  
  # Assign values to the matrix to select coefficients
  matSelect[, expvar] <- c(1, 1, 1, 1)
  matSelect[, crosvar1] <- c(0, 1, 0, 1)
  matSelect[, crosvar2] <- c(0, 0, 1, 1)
  matSelect[, crosvar3] <- c(0, 0, 0, 1)
  
  
  # Calculate coefficients, confidence intervals, and other statistics
  coefs <- as.numeric(matSelect %*% coef(mod))
  modU_CI <- t(matSelect %*% t(modU))
  
  # Compute confidence intervals
  CI <- t(apply(modU_CI, 2, quantile, probs = alpha, na.rm = TRUE))
  colnames(CI) <- paste0("CI", parse_number(colnames(CI)))
  
  # Extract relevant variables from the model
  vect_temp <- mod$model
  vect_temp <- vect_temp %>% pull(empvar)
  vect_temp_2 <- mod$model %>% filter(numberEventpostBHR > 0) %>% pull(empvar)
  
  # Create a tibble with results
  out <- tibble(
    empvar = empvar,
    estimate = c("No", "No", "Yes", "Yes"),
    typeEffect = c("2-5", "0-1", "2-5", "0-1"),
    pe = coefs,
    N = nrow(mod$model),
    Ntype = c(as.numeric(table(vect_temp))[1], as.numeric(table(vect_temp))[1], 
              as.numeric(table(vect_temp))[2], as.numeric(table(vect_temp))[2]),
    Ntype_treat = c(as.numeric(table(vect_temp_2))[1], as.numeric(table(vect_temp_2))[1], 
                    as.numeric(table(vect_temp_2))[2], as.numeric(table(vect_temp_2))[2]),
    r.squared = summary(mod)$r.squared
  )
  out <- bind_cols(out, as.tibble(CI))
  
  out
}


# Function to get estimates for a global model with interaction terms involving age groups
getEstimateGlobalexposed <- function(depvar, empvar, df1, df2){
  # Define the baseline model formula
  basemodelEmp <- formula(paste0(depvar, "~ exposed_group*",empvar," + kidtwin_rec + 
                                 KIDBORD + KIDSEX + KIDCURAGE + EDYRTOTAL + ETHNICITYNG + 
                                 HUSEDYRS + motherAge + motherAgesqr + womwrkAgri + 
                                 huswrkAgri + HHMEMTOTAL + HHKIDLT5 + WEALTHQ  + 
                                 religion_rec | KIDBIRTHMO + KIDBIRTHYR + interview_month  + 
                                 YEAR + CLUSTERNO | 0 | CLUSTERNO"))
  
  # Define the sibling model formula
  basemodelEmp_s <- formula(paste0(depvar, "~ exposed_group*",empvar," + kidtwin_rec + 
                                   KIDBORD + KIDSEX + KIDCURAGE | KIDBIRTHMO + KIDBIRTHYR + 
                                   interview_month  + IDHSPID | 0 | CLUSTERNO"))
  
  # Combine results for baseline and sibling models
  temp_res <- bind_rows(
    getEstimatedfexposed(basemodelEmp, empvar, dfcurr = df1) %>% mutate(name = "baseline"),
    getEstimatedfexposed(basemodelEmp_s, empvar, dfcurr = df2) %>% mutate(name = "sibling") 
  )
  
  temp_res
}



# Function to get estimates using fixed effects regression with interaction terms involving age groups
getEstimatedfexposed <- function(formula, empvar, dfcurr, R = 1000){
  
  # Fit fixed effects regression model
  mod <- felm(formula, data = dfcurr, keepModel = T, na.action = na.omit)
  
  vcmod <- vcov(mod)
  vcmod[is.na(vcmod)] <- 0
  
  # Extract coefficients and handle NAs
  mat_coef <- coef(mod)
  mat_coef[is.na(mat_coef)] <- 0
  
  # Define alpha levels for confidence intervals
  alpha <- c(.025, .05, .95, .975)
  
  # Generate random draws from the multivariate normal distribution
  modU <- MASS::mvrnorm(R, mu = mat_coef, Sigma = vcmod)
  
  # Create a matrix to select coefficients for interaction terms
  matSelect <- matrix(0, nrow = 4, ncol = length(mat_coef))
  colnames(matSelect) <- names(mat_coef)
  
  # Define variable names for interaction terms
  crosvar1 <- paste0("exposed_group1", sep = "")
  crosvar2 <- paste0("exposed_group1:",empvar,"1",  sep = "")
  crosvar3 <- paste0("exposed_group2", sep = "")
  crosvar4 <- paste0("exposed_group2:",empvar,"1", sep = "")
  
  # Assign values to the matrix to select coefficients
  matSelect[, crosvar1] <- c(1, 1, 0, 0)
  matSelect[, crosvar2] <- c(0, 1, 0, 0)
  matSelect[, crosvar3] <- c(0, 0, 1, 1)
  matSelect[, crosvar4] <- c(0, 0, 0, 1)
  
  # Calculate coefficients, confidence intervals, and other statistics
  coefs <- as.numeric(matSelect %*% mat_coef)
  modU_CI <- t(matSelect %*% t(modU))
  
  # Compute confidence intervals
  CI <- t(apply(modU_CI, 2, quantile, probs = alpha, na.rm = TRUE))
  colnames(CI) <- paste0("CI", parse_number(colnames(CI)))
  
  # Extract relevant variables from the model
  vect_temp <- mod$model
  vect_temp <- vect_temp %>% pull(empvar)
  vect_temp_2 <- mod$model %>% filter(exposed_group==1 | exposed_group==2) %>% 
    pull(empvar)
  
  # Create a tibble with results
  out <- tibble(
    empvar = empvar,
    estimate = c("No", "Yes", "No", "Yes"),
    typeEffect = c("1", "1", "2", "2"),
    pe = coefs,
    N = nrow(mod$model),
    Ntype = c(as.numeric(table(vect_temp))[1], as.numeric(table(vect_temp))[2], 
              as.numeric(table(vect_temp))[1], as.numeric(table(vect_temp))[2]),
    Ntype_treat = c(as.numeric(table(vect_temp_2))[1], as.numeric(table(vect_temp_2))[2], 
                    as.numeric(table(vect_temp_2))[1], as.numeric(table(vect_temp_2))[2]),
    r.squared = summary(mod)$r.squared
  )
  out <- bind_cols(out, as.tibble(CI))
  
  out
}






getEstimatepostbif <- function(formula, empvar,dfcurr, R=1000){
  
  mod <- felm(formula, data = dfcurr,keepModel = T, na.action=na.omit)
  vcmod <- vcov(mod)
  
  alpha <- c(.025, .05, .95, .975)
  modU <- MASS::mvrnorm(R, mu = coef(mod), Sigma = vcmod)
  
  matSelect <- matrix(0, nrow = 4, ncol = length(coef(mod)))
  colnames(matSelect) <- names(coef(mod))
  
  crosvar1 <- paste0("numberEventpost:",empvar,"1",sep="")
  crosvar2 <- paste0("numberEventTotal:",empvar,"1",sep="")
  
  matSelect[, "numberEventpost"] <- c(1,1,0,0)
  matSelect[, crosvar1] <- c(0,1,0,0)
  matSelect[, "numberEventTotal"] <- c(0,0,1,1)
  matSelect[, crosvar2] <- c(0,0,0,1)
  
  
  coefs <- as.numeric(matSelect %*% coef(mod))
  modU_CI <- t(matSelect %*% t(modU))
  
  # fonction pour acceder au truc: pull(x)
  
  
  CI <- t(apply(modU_CI, 2, quantile, probs = alpha, na.rm = TRUE))
  colnames(CI) <- paste0("CI", parse_number(colnames(CI)))
  
  #select the right column
  vect_temp <- mod$model
  vect_temp <- vect_temp %>% pull(empvar)
  vect_temp_2 <- mod$model %>% filter(numberEventpostBHR > 0) %>% pull(empvar)
  
  out <- tibble(
    empvar = empvar,
    estimate = c("No", "Yes","No","Yes"),
    typeEffect=c("Post","Post","Pre","Pre"),
    pe = coefs,
    N = nrow(mod$model),
    Ntype = c(as.numeric(table(vect_temp))[1],as.numeric(table(vect_temp))[1], 
              as.numeric(table(vect_temp))[2], as.numeric(table(vect_temp))[2]),
    Ntype_treat = c(as.numeric(table(vect_temp_2))[1],as.numeric(table(vect_temp_2))[1], as.numeric(table(vect_temp_2))[2], as.numeric(table(vect_temp_2))[2]),
    r.squared = summary(mod)$r.squared
  )
  out <- bind_cols(out, as.tibble(CI))
  
  out
}

# Function to estimate coefficients and confidence intervals for a given formula and data
getEstimatedf <- function(formula, empvar, expvar, dfcurr, R = 1000) {
  
  # Fit the fixed effects model using the formula and input dataframe
  mod <- felm(formula, data = dfcurr, keepModel = TRUE, na.action = na.omit)
  
  # Extract the variance-covariance matrix of model coefficients
  vcmod <- vcov(mod)
  
  # Define quantiles for confidence intervals
  alpha <- c(.025, .05, .95, .975)
  
  # Simulate coefficients from a multivariate normal distribution
  modU <- MASS::mvrnorm(R, mu = coef(mod), Sigma = vcmod)
  
  # Create a matrix to select coefficients of interest
  matSelect <- matrix(0, nrow = 2, ncol = length(coef(mod)))
  colnames(matSelect) <- names(coef(mod))
  
  # Define the interaction term and the variable of interest
  crosvar1 <- paste0(expvar, ":", empvar, "1", sep = "")
  
  # Set values in the matrix for the interaction term and variable of interest
  matSelect[, expvar] <- c(1, 1)
  matSelect[, crosvar1] <- c(0, 1)
  
  # Calculate the simulated coefficients and their confidence intervals
  coefs <- as.numeric(matSelect %*% coef(mod))
  modU_CI <- t(matSelect %*% t(modU))
  
  # Compute confidence intervals for the coefficients
  CI <- t(apply(modU_CI, 2, quantile, probs = alpha, na.rm = TRUE))
  colnames(CI) <- paste0("CI", parse_number(colnames(CI)))
  
  # Extract variables related to the number of events and treatments
  vect_temp <- mod$model
  vect_temp <- vect_temp %>% pull(empvar)
  
  vect_temp_2 <- mod$model %>% filter(!!as.name(expvar) > 0) %>% pull(empvar)
  
  # Create a tibble containing results
  out <- tibble(
    empvar = empvar,
    estimate = c("No", "Yes"),
    pe = coefs,
    N = nrow(mod$model),
    Ntype = as.numeric(table(vect_temp)),
    Ntype_treat = as.numeric(table(vect_temp_2)),
    r.squared = summary(mod)$r.squared
  )
  
  # Bind confidence intervals to the output
  out <- bind_cols(out, as.tibble(CI))
  
  # Return the result
  out
}

# Function to estimate confidence intervals for logistic regression effects
getEstimatedf_logit <- function(formula, empvar, expvar, dfcurr, R=1000){
  # Fit the model using the felm function
  mod <- felm(formula, data = dfcurr, keepModel = TRUE, na.action=na.omit)
  vcmod <- vcov(mod)  # Extract variance-covariance matrix
  
  alpha <- c(.025, .05, .95, .975)  # Define confidence interval levels
  modU <- MASS::mvrnorm(R, mu = coef(mod), Sigma = vcmod)  # Generate Monte Carlo simulations
  
  # Create a matrix to extract relevant coefficients
  matSelect <- matrix(0, nrow = 2, ncol = length(coef(mod)))
  colnames(matSelect) <- names(coef(mod))
  
  # Define the interaction term for explanatory and employment variables
  crosvar1 <- paste0(expvar,":",empvar,"1",sep="")
  
  # Assign values to extract coefficients
  matSelect[, expvar] <- c(1,1)
  matSelect[, crosvar1] <- c(0,1)
  
  # Compute point estimates and confidence intervals
  coefs <- as.numeric(matSelect %*% coef(mod))
  modU_CI <- t(matSelect %*% t(modU))
  
  CI <- t(apply(modU_CI, 2, quantile, probs = alpha, na.rm = TRUE))  # Compute confidence intervals
  colnames(CI) <- paste0("CI", parse_number(colnames(CI)))  # Rename confidence interval columns
  
  # Extract sample size and employment variable distribution
  vect_temp <- mod$model %>% pull(empvar)
  
  # Create a tibble with estimation results
  out <- tibble(
    empvar = empvar,
    #depvariable = depvar,  # (Commented out, could be included if needed)
    estimate = c("No", "Yes"),  # Labels for employment effect
    pe = coefs,  # Point estimates
    N = nrow(mod$model),  # Sample size
    Ntype = as.numeric(table(vect_temp)),  # Frequency of employment variable categories
    r.squared = summary(mod)$r.squared  # Model fit statistic
  )
  
  out <- bind_cols(out, as.tibble(CI))  # Append confidence intervals to results
  
  return(out)
}

# Function to get estimates for a global model with interaction terms involving sex variable

getEstimateGlobalsex <- function(depvar, empvar, expvar, df1, df2){
  # Define the baseline model formula
  basemodelEmp <- formula(paste0(depvar, "~", expvar, "*", empvar, "*KIDSEX + kidtwin_rec + KIDBORD + KIDCURAGE + EDYRTOTAL + ETHNICITYNG + HUSEDYRS + motherAge + motherAgesqr + womwrkAgri + huswrkAgri + HHMEMTOTAL + HHKIDLT5 + WEALTHQ  + religion_rec | KIDBIRTHMO + KIDBIRTHYR + interview_month  + YEAR + CLUSTERNO | 0 | CLUSTERNO"))
  
  # Define the sibling model formula
  basemodelEmp_s <- formula(paste0(depvar, "~", expvar, ":", empvar, ":KIDSEX + ", expvar, ":", empvar, "+", expvar, ":KIDSEX + ", expvar, " + kidtwin_rec + KIDSEX + KIDBORD + KIDCURAGE | KIDBIRTHMO + KIDBIRTHYR + interview_month  + IDHSPID | 0 | CLUSTERNO"))
  
  
  # Combine results for baseline and sibling models
  temp_res <- bind_rows(
    getEstimatedfsex(basemodelEmp, empvar, expvar, dfcurr = df1) %>% mutate(name = "baseline"),
    getEstimatedfsex(basemodelEmp_s, empvar, expvar, dfcurr = df2) %>% mutate(name = "sibling") 
  )
  
  temp_res
}

# Function to get estimates using fixed effects regression with interaction terms involving sex variable
getEstimatedfsex <- function(formula, empvar, expvar, dfcurr, R = 1000){
  
  # Fit fixed effects regression model
  mod <- felm(formula, data = dfcurr, keepModel = T, na.action = na.omit)
  vcmod <- vcov(mod)
  
  # Define alpha levels for confidence intervals
  alpha <- c(.025, .05, .95, .975)
  
  # Generate random draws from the multivariate normal distribution
  modU <- MASS::mvrnorm(R, mu = coef(mod), Sigma = vcmod)
  
  # Create a matrix to select coefficients for interaction terms
  matSelect <- matrix(0, nrow = 4, ncol = length(coef(mod)))
  colnames(matSelect) <- names(coef(mod))
  
  # Define variable names for interaction terms
  crosvar1 <- paste0(expvar, ":KIDSEX1", sep = "")
  crosvar2 <- paste0(expvar, ":", empvar, "1", sep = "")
  crosvar3 <- paste0(expvar, ":", empvar, "1:KIDSEX1", sep = "")
  
  
  # Assign values to the matrix to select coefficients
  matSelect[, expvar] <- c(1, 1, 1, 1)
  matSelect[, crosvar1] <- c(0, 1, 0, 1)
  matSelect[, crosvar2] <- c(0, 0, 1, 1)
  matSelect[, crosvar3] <- c(0, 0, 0, 1)
  
  
  # Calculate coefficients, confidence intervals, and other statistics
  coefs <- as.numeric(matSelect %*% coef(mod))
  modU_CI <- t(matSelect %*% t(modU))
  
  # Compute confidence intervals
  CI <- t(apply(modU_CI, 2, quantile, probs = alpha, na.rm = TRUE))
  colnames(CI) <- paste0("CI", parse_number(colnames(CI)))
  
  # Extract relevant variables from the model
  vect_temp <- mod$model
  vect_temp <- vect_temp %>% pull(empvar)
  
  vect_temp_2 <- mod$model %>% filter(numberEventpostBHR > 0) %>% pull(empvar)
  
  # Create a tibble with results
  out <- tibble(
    empvar = empvar,
    estimate = c("No", "No", "Yes", "Yes"),
    typeEffect = c("female", "male", "female", "male"),
    pe = coefs,
    N = nrow(mod$model),
    Ntype = c(as.numeric(table(vect_temp))[1], as.numeric(table(vect_temp))[1], 
              as.numeric(table(vect_temp))[2], as.numeric(table(vect_temp))[2]),
    Ntype_treat = c(as.numeric(table(vect_temp_2))[1], as.numeric(table(vect_temp_2))[1], as.numeric(table(vect_temp_2))[2], as.numeric(table(vect_temp_2))[2]),
    r.squared = summary(mod)$r.squared
  )
  out <- bind_cols(out, as.tibble(CI))
  
  out
}

# Function to estimate the effect of an explanatory variable on a binary dependent variable
getEstimatebinary <- function(depvar, empvar, expvar, df1, df2){
  
  # Define the base model with employment variable interaction and additional covariates
  basemodelEmp <-
    formula(paste0(depvar,"~ ",expvar,"*",empvar," + kidtwin_rec + KIDSEX + 
                   KIDBORD + ETHNICITYNG + EDYRTOTAL + HUSEDYRS  + motherAge + 
                   motherAgesqr + womwrkAgri + huswrkAgri + HHMEMTOTAL + HHKIDLT5 + 
                   WEALTHQ  + religion_rec + KIDCURAGE | KIDBIRTHMO + KIDBIRTHYR + 
                   interview_month  + YEAR + CLUSTERNO | 0 | CLUSTERNO"))
  
  # Define a simplified base model with fewer covariates
  basemodelEmp_s <-
    formula(paste0(depvar,"~",expvar,":",empvar," + ",expvar,"  + kidtwin_rec  + 
                   KIDBORD + KIDSEX  + KIDCURAGE | KIDBIRTHMO + KIDBIRTHYR + 
                   interview_month  + IDHSPID | 0 | CLUSTERNO"))
  
  # Fit the models using the felm function from the lfe package
  modbasemodelTot <- felm(basemodelEmp, data = df1)
  modbasemodelTot_s <- felm(basemodelEmp_s, data = df2)
  
  # Store results in a tibble for easier handling
  out <- tibble(
    model = c("baseModel", "baseModelS"),
    Results_base = list(modbasemodelTot, modbasemodelTot_s)
  )
  
  return(out)
}

# Function to estimate confidence intervals for the effect of an explanatory variable
getEstimatedfbinary <- function(formula, empvar, expvar, dfcurr, R=1000){
  # Fit the model with employment and explanatory variable
  mod <- felm(formula, data = dfcurr, keepModel = TRUE, na.action=na.omit)
  vcmod <- vcov(mod)  # Extract variance-covariance matrix of the model
  
  alpha <- c(.025, .05, .95, .975)  # Confidence interval levels
  modU <- MASS::mvrnorm(R, mu = coef(mod), Sigma = vcmod)  # Generate Monte Carlo simulations
  
  # Create a matrix to extract relevant coefficients for interaction effects
  matSelect <- matrix(0, nrow = 2, ncol = length(coef(mod)))
  colnames(matSelect) <- names(coef(mod))
  
  # Define the interaction terms for the explanatory and employment variables
  crosvar1 <- paste0(expvar,"1",sep="")
  crosvar2 <- paste0(expvar,"1:",empvar,"1",sep="")
  matSelect[, crosvar1] <- c(1,1)
  matSelect[, crosvar2] <- c(0,1)
  
  # Compute point estimates and confidence intervals
  coefs <- as.numeric(matSelect %*% coef(mod))
  modU_CI <- t(matSelect %*% t(modU))
  CI <- t(apply(modU_CI, 2, quantile, probs = alpha, na.rm = TRUE))
  colnames(CI) <- paste0("CI", parse_number(colnames(CI)))
  
  # Extract sample size and employment variable distribution
  vect_temp <- mod$model %>% pull(empvar)
  
  # Create a tibble with estimation results
  out <- tibble(
    empvar = empvar,
    estimate = c("No","Yes"),  # Labels for the employment variable effect
    typeEffect = c("without","with"),  # Indicating interaction effect presence
    pe = coefs,  # Point estimates
    N = nrow(mod$model),  # Sample size
    Ntype = as.numeric(table(vect_temp)),  # Frequency of employment variable categories
    r.squared = summary(mod)$r.squared  # Model fit statistic
  )
  
  out <- bind_cols(out, as.tibble(CI))  # Append confidence intervals to results
  
  return(out)
}

############################################## Other functions ####################


# Function to estimate fixed effects model for a base outcome variable
getEstimateGlobalext <- function(depvar, expvar, df1, df2){
  # This function estimates a fixed effects model for a specified dependent variable (depvar)
  # and explanatory variable (expvar) using two datasets (df1, df2)
  
  # Create formula for baseline model with comprehensive controls
  # Uses felm for fixed effects linear model
  basemodelTot <-
    formula(paste0(depvar,"~ ",expvar,"+ kidtwin_rec + KIDSEX + KIDBORD + KIDCURAGE + ETHNICITYNG + EDYRTOTAL + HUSEDYRS + womwrkAgri + huswrkAgri +  MotherAge  + MotherAgesqr + HHEADSEXHH + HHMEMTOTAL + HHKIDLT5 + WEALTHQ + religion_rec| interview_month + KIDBIRTHMO + KIDBIRTHYR + YEAR + DHSID | 0 | DHSID"))
  
  # Create simpler formula for sibling model with fewer controls and fixed effects
  basemodelTot_s <-
    formula(paste0(depvar,"~ ",expvar," + kidtwin_rec + KIDSEX + KIDBORD + KIDCURAGE | KIDBIRTHMO + interview_month + KIDBIRTHYR + IDHSPID + DHSID | 0 | DHSID"))
  
  # Run fixed effects linear models for both datasets
  # felm syntax: outcome ~ covariates | fixed effects | IV | cluster
  modbasemodelTot <- felm(basemodelTot, data = df1)
  modbasemodelTot_s <- felm(basemodelTot_s, data = df2)
  
  # Return results in a tibble format
  out <- tibble(
    model = c("baseModel","baseModelS"),
    Results_base = list(modbasemodelTot, modbasemodelTot_s)
  )
  
  out
}

# Function to estimate fixed effects model with empowerment variables
getEstimateGlobalempext <- function(depvar, empvar, expvar, df1){
  # This function estimates a model including both exposure and empowerment variables
  # Only uses the main dataset (df1)
  
  # Create formula incorporating both exposure and empowerment variables
  basemodelTot <-
    formula(paste0(depvar,"~ ",expvar,"+",empvar,"+ kidtwin_rec + KIDSEX + KIDBORD + KIDCURAGE + ETHNICITYNG + EDYRTOTAL + HUSEDYRS + womwrkAgri + huswrkAgri +  MotherAge  + MotherAgesqr + HHEADSEXHH + HHMEMTOTAL + HHKIDLT5 + WEALTHQ + religion_rec| interview_month + KIDBIRTHMO + KIDBIRTHYR + YEAR + DHSID| 0 | DHSID"))
  
  # Run the fixed effects model
  modbasemodelTot <- felm(basemodelTot, data = df1)
  
  # Return results in a tibble
  out <- tibble(
    model = c("baseModel"),
    Results_base = list(modbasemodelTot)
  )
  
  out
}

# Function to estimate difference-in-differences models
getEstimateGlobalDiffinDiff <- function(depvar, expvar1, expvar2, df1, df2){
  # This function estimates a difference-in-differences model with interaction terms
  # between two explanatory variables (expvar1 and expvar2)
  
  # Create formula with interaction term for baseline model
  basemodelTot <-
    formula(paste0(depvar,"~ ",expvar1,"*",expvar2,"+ kidtwin_rec + KIDSEX + KIDBORD + KIDCURAGE + ETHNICITYNG + EDYRTOTAL + HUSEDYRS + womwrkAgri + huswrkAgri +  MotherAge  + MotherAgesqr + HHEADSEXHH + HHMEMTOTAL + HHKIDLT5 + WEALTHQ + religion_rec| interview_month + KIDBIRTHMO + YEAR| 0 | DHSID"))
  
  # Create simpler formula with interaction term for sibling model
  basemodelTot_s <-
    formula(paste0(depvar,"~ ",expvar1,"*",expvar2,"+ kidtwin_rec + KIDSEX + KIDBORD + KIDCURAGE | interview_month + KIDBIRTHMO + IDHSPID| 0 | DHSID"))
  
  # Run models for both datasets
  modbasemodelTot <- felm(basemodelTot, data = df1)
  modbasemodelTot_s <- felm(basemodelTot_s, data = df2)
  
  # Return results
  out <- tibble(
    model = c("baseModel","baseModelS"),
    Results_base = list(modbasemodelTot, modbasemodelTot_s)
  )
  
  out
}

# Function to estimate difference-in-differences models with empowerment variables
getEstimateGlobalempDiffinDiff <- function(depvar, empvar, expvar1, expvar2, df1, df2){
  # This function estimates a diff-in-diff model that includes an empowerment variable
  # Note: df2 is passed as a parameter but not used in this function
  
  # Create formula with interaction term and empowerment variable
  basemodelEmp2 <-
    formula(paste0(depvar,"~ ",expvar1,"*",expvar2,"+",empvar," + kidtwin_rec + KIDSEX + KIDBORD + KIDCURAGE + ETHNICITYNG + EDYRTOTAL + HUSEDYRS + womwrkAgri + huswrkAgri +  MotherAge  + MotherAgesqr + HHEADSEXHH + HHMEMTOTAL + HHKIDLT5 + WEALTHQ + religion_rec| interview_month + KIDBIRTHMO + YEAR| 0 | DHSID"))
  
  # Run the model for baseline dataset only
  modbasemodelEmp2 <- felm(basemodelEmp2, data = df1)
  #modbasemodelEmp3 <- felm(basemodelEmp3, data = df2) # Commented out in original code
  
  # Return results
  out <- tibble(
    model = "Model",
    empvar = empvar,
    Results_basec = list(modbasemodelEmp2)
    #Results_sibling = list(modbasemodelEmp3) # Commented out in original code
  )
  
  out
}

# Function to estimate logistic fixed effects models with empowerment interactions
NanGlobalwithemp <- function(depvar, empvar, expvar, df1, df2){
  # This function estimates a logistic fixed effects model with interaction between
  # an exposure variable and an empowerment variable
  
  # Create formula with interaction between exposure and empowerment for baseline model
  basemodelEmp2 <-
    formula(paste0(depvar,"~ ",expvar,"*",empvar, " + womwrkAgri + huswrkAgri + motherAge + HHEADSEXHH  + ETHNICITYNG +motherAgesqr+ HHMEMTOTAL + HHKIDLT5 + WEALTHQ  + religion_rec  + kidtwin_rec + KIDSEX + KIDBORD + KIDCURAGE| YEAR + KIDBIRTHYR + interview_month + KIDBIRTHMO ")) 
  
  # Create simpler formula for sibling model
  basemodelEmp_s <- formula(paste0(depvar, "~", expvar, ":", empvar, " + ", expvar," + kidtwin_rec + KIDSEX + KIDBORD + KIDCURAGE | YEAR + KIDBIRTHYR + interview_month + KIDBIRTHMO "))
  
  # Run logistic fixed effects models using feglm from fixest package
  # Note the use of binomial family for logistic regression and clustering by CLUSTERNO
  modbasemodelEmp <- fixest::feglm(basemodelEmp2, data = df1, family="binomial", cluster = "CLUSTERNO")
  modbasemodelEmp2 <- fixest::feglm(basemodelEmp_s, data = df2, family="binomial", cluster = "CLUSTERNO")
  
  # Return results
  out <- tibble(
    model = "Model",
    Results_basec = list(modbasemodelEmp),
    Results_sibling = list(modbasemodelEmp2)
  )
  
  out
}

# Function to estimate models with two explanatory variables without interaction
getEstimateGlobalwithemp2 <- function(depvar, expvar1, expvar2, df1, df2){
  # This function estimates models with two explanatory variables (without interaction)
  # for both baseline and sibling datasets
  
  # Create formula for baseline model with full controls
  basemodelEmp2 <-
    formula(paste0(depvar,"~ ",expvar1,"+",expvar2," + kidtwin_rec + KIDSEX + KIDBORD + KIDCURAGE + ETHNICITYNG + EDYRTOTAL + HUSEDYRS + womwrkAgri + huswrkAgri +  MotherAge  + MotherAgesqr + HHEADSEXHH + HHMEMTOTAL + HHKIDLT5 + WEALTHQ + religion_rec| KIDBIRTHMO + KIDBIRTHYR + YEAR + DHSID | 0 | DHSID"))
  
  # Create simpler formula for sibling model
  basemodelEmp3 <-
    formula(paste0(depvar,"~ ",expvar1,"+",expvar2,"+ kidtwin_rec + KIDSEX + KIDBORD + KIDCURAGE| IDHSPID + KIDBIRTHMO + KIDBIRTHYR | 0 | DHSID"))
  
  # Run models for both datasets
  modbasemodelEmp2 <- felm(basemodelEmp2, data = df1)
  modbasemodelEmp3 <- felm(basemodelEmp3, data = df2)
  
  # Return results
  out <- tibble(
    model = "Model",
    Results_basec = list(modbasemodelEmp2),
    Results_sibling = list(modbasemodelEmp3)
  )
  
  out
}

# Function to create descriptive statistics tables stratified by conflict and empowerment
get_table <- function(curr_empvar, df1, df2){
  # This function creates descriptive statistics tables for anthropometric outcomes
  # stratified by conflict status and a specified empowerment variable
  
  # Create table for baseline dataset (df1)
  t1 <- df1 %>% 
    drop_na() %>% 
    # Select relevant anthropometric outcomes and stratification variables
    dplyr::select(!!as.name(curr_empvar), HWWAZWHO, HWHAZWHO, HWWHZWHO, conflict_affected) %>% 
    
    # Create stratified table by conflict status
    tbl_strata(
      strata = conflict_affected,
      .tbl_fun =
        ~ .x %>% 
        # Generate summary statistics by empowerment variable
        tbl_summary(
          by = !!as.name(curr_empvar),  # Group by current empowerment variable
          type = c(c("HWWAZWHO", "HWHAZWHO", "HWWHZWHO")~"continuous"),
          statistic = list(
            all_categorical() ~ "{n} ({p}%)",
            all_continuous() ~ "{mean} ({sd})"
          ),
          missing_text = "(Missing)",
          digits = all_continuous() ~ c(3,3)
        ) %>% 
        # Add statistical difference tests
        add_difference() %>% 
        modify_column_hide(ci) %>%
        modify_header(label ~ "**Baseline**"),
      .header = "**{strata}**, N = {n}") 
  
  # Create similar table for sibling dataset (df2)
  t2 <- df2 %>%
    dplyr::select(!!as.name(curr_empvar), HWWAZWHO, HWHAZWHO, HWWHZWHO, conflict_affected) %>%
    
    tbl_strata(
      strata = conflict_affected,
      .tbl_fun =
        ~ .x %>% 
        tbl_summary(
          by = !!as.name(curr_empvar),
          type = c(c("HWWAZWHO", "HWHAZWHO", "HWWHZWHO")~"continuous"),
          statistic = list(
            all_categorical() ~ "{n} ({p}%)",
            all_continuous() ~ "{mean} ({sd})"
          ),
          missing_text = "(Missing)",
          digits = all_continuous() ~ c(3,3)
        ) %>% 
        add_difference() %>% 
        modify_column_hide(ci) %>%
        modify_header(label ~ "**Sibling**"),
      .header = "**{strata}**, N = {n}") 
  
  # Combine tables and save as LaTeX file
  tbl_stack(list(t1,t2), quiet = TRUE, group_header=c("Baseline","Sibling")) %>% 
    as_gt() %>%
    gt::gtsave(paste0('table_',curr_empvar,'.tex'), path = here::here())
}

# Function to estimate models with three-way interactions between exposure and empowerment variables
getEstimateGlobalemp2 <- function(depvar, empvar, expvar1, expvar2, df1, df2, type=1){
  # This function creates and estimates models with three-way interactions (expvar1*expvar2*empvar)
  # for both baseline and sibling datasets with two different formula types
  
  if(type==1){
    # Type 1: Uses interview_month, KIDBIRTHMO, and YEAR for fixed effects in baseline model
    basemodelEmp <-
      formula(paste0(depvar,"~ ",expvar1,"*",expvar2,"*",empvar,"  + kidtwin_rec + KIDSEX + KIDBORD + KIDCURAGE  + ETHNICITYNG+ EDYRTOTAL + HUSEDYRS + MotherAge + MotherAgesqr + womwrkAgri + huswrkAgri + HHEADSEXHH + HHMEMTOTAL + HHKIDLT5 + WEALTHQ  + religion_rec| interview_month + KIDBIRTHMO + YEAR| 0 | DHSID"))
    
    # Sibling model with simpler controls and IDHSPID as fixed effect
    basemodelEmp_s <- # Comment indicates alternate formulation was considered
      formula(paste0(depvar,"~" ,expvar1,"*",expvar2,"*",empvar,"   + kidtwin_rec + KIDSEX + KIDBORD + KIDCURAGE | interview_month + KIDBIRTHMO + IDHSPID | 0 | DHSID"))
  }
  else {
    # Type 2: Alternative model specifications with slightly different fixed effects
    basemodelEmp <-
      formula(paste0(depvar,"~ ",expvar1,"*",expvar2,"*",empvar," + kidtwin_rec + KIDSEX + KIDBORD + KIDCURAGE+ ETHNICITYNG+ EDYRTOTAL + HUSEDYRS  + MotherAge + MotherAgesqr + womwrkAgri + huswrkAgri + HHEADSEXHH + HHMEMTOTAL + HHKIDLT5 + WEALTHQ  + religion_rec|interview_month + KIDBIRTHMO + YEAR| 0 | DHSID"))
    
    # Sibling model includes YEAR along with IDHSPID for fixed effects
    basemodelEmp_s <- # Comment indicates alternate formulation was considered
      formula(paste0(depvar,"~ " ,expvar1,"*",expvar2,"*",empvar," +  kidtwin_rec + KIDSEX + KIDBORD + KIDCURAGE | interview_month + KIDBIRTHMO + YEAR + IDHSPID | 0 | DHSID"))
  }
  
  # Run models and combine results using a helper function getEstimatedf
  # Adds a name column to identify which dataset was used
  temp_res <- bind_rows(
    getEstimatedf(basemodelEmp, empvar, expvar1, expvar2, dfcurr = df1) %>% mutate(name="baseline"),
    getEstimatedf(basemodelEmp_s, empvar, expvar1, expvar2, dfcurr = df2) %>% mutate(name="sibling") 
  )
  
  temp_res
}

# Function to estimate models examining interactions between poverty and empowerment
getEstimateGlobalemp_poverty <- function(depvar, empvar, expvar, df1, df2, type=1){
  # This function creates models to examine interactions between poverty indicators
  # and empowerment variables (note: WEALTHQ control is omitted since likely related to poverty measure)
  
  if(type==1){
    # Type 1: Uses birth month/year, survey year, and DHS ID as fixed effects
    basemodelEmp <-
      formula(paste0(depvar,"~ ",expvar,"*",empvar,"  + kidtwin_rec + KIDSEX + KIDBORD + KIDCURAGE  + ETHNICITYNG + EDYRTOTAL + HUSEDYRS + MotherAge + MotherAgesqr + womwrkAgri + huswrkAgri + HHEADSEXHH + HHMEMTOTAL + HHKIDLT5  + religion_rec| KIDBIRTHMO + KIDBIRTHYR + YEAR + DHSID | 0 | DHSID"))
    
    # Sibling model uses interaction term with ':' notation and simpler fixed effects
    basemodelEmp_s <-
      formula(paste0(depvar,"~ ",expvar,":",empvar," + ",expvar,"  + kidtwin_rec + KIDSEX + KIDBORD + KIDCURAGE | KIDBIRTHMO + KIDBIRTHYR + IDHSPID | 0 | DHSID"))
  }
  else {
    # Type 2: Similar to type 1 but with slightly different structure
    basemodelEmp <-
      formula(paste0(depvar,"~ ",expvar,"*",empvar," + kidtwin_rec + KIDSEX + KIDBORD + KIDCURAGE+ ETHNICITYNG + EDYRTOTAL + HUSEDYRS  + MotherAge + MotherAgesqr + womwrkAgri + huswrkAgri + HHEADSEXHH + HHMEMTOTAL + HHKIDLT5  + religion_rec| KIDBIRTHMO + KIDBIRTHYR + YEAR + DHSID | 0 | DHSID"))
    
    # Sibling model similar to type 1
    basemodelEmp_s <-
      formula(paste0(depvar,"~ ",expvar,":",empvar," + ",expvar," + kidtwin_rec + KIDSEX + KIDBORD + KIDCURAGE | KIDBIRTHMO + KIDBIRTHYR + IDHSPID | 0 | DHSID"))
  }
  
  # Run models and combine results
  temp_res <- bind_rows(
    getEstimatedf(basemodelEmp, empvar, expvar, dfcurr = df1) %>% mutate(name="baseline"),
    getEstimatedf(basemodelEmp_s, empvar, expvar, dfcurr = df2) %>% mutate(name="sibling") 
  )
  
  temp_res
}

# Function for mortality analysis models - note KIDCURAGE is omitted (likely for mortality outcomes)
getEstimateGlobalemort <- function(depvar, empvar, expvar, df1, df2){
  # This function creates models for analyzing mortality outcomes
  # KIDCURAGE is excluded from controls (likely because for mortality outcomes, current age may not be relevant)
  
  # Full baseline model with comprehensive controls
  basemodelEmp <-
    formula(paste0(depvar,"~ ",expvar,"*",empvar," + kidtwin_rec + KIDSEX + KIDBORD + ETHNICITYNG+ EDYRTOTAL + HUSEDYRS  + MotherAge + MotherAgesqr + womwrkAgri + huswrkAgri + HHEADSEXHH + HHMEMTOTAL + HHKIDLT5 + WEALTHQ  + religion_rec| KIDBIRTHMO + KIDBIRTHYR + YEAR + DHSID | 0 | DHSID"))
  
  # Simpler sibling model with fewer controls, using ':' for interaction
  basemodelEmp_s <-
    formula(paste0(depvar,"~ ",expvar,":",empvar," + ",expvar," + kidtwin_rec + KIDSEX + KIDBORD | KIDBIRTHMO + KIDBIRTHYR + IDHSPID | 0 | DHSID"))
  
  # Run models and combine results
  temp_res <- bind_rows(
    getEstimatedf(basemodelEmp, empvar, expvar, dfcurr = df1) %>% mutate(name="baseline"),
    getEstimatedf(basemodelEmp_s, empvar, expvar, dfcurr = df2) %>% mutate(name="sibling") 
  )
  
  temp_res
}

# Function for selection models (possibly examining selection effects)
getEstimateGlobalempsel <- function(depvar, empvar, expvar, df1, df2){
  # This function creates models for analyzing selection effects
  # Note: KIDSEX is omitted from the controls, likely for testing selection effects
  
  # Baseline model without KIDSEX control
  basemodelEmp <-
    formula(paste0(depvar,"~",expvar,"*",empvar," + kidtwin_rec + KIDBORD + KIDCURAGE  + EDYRTOTAL + ETHNICITYNG + HUSEDYRS + MotherAge + MotherAgesqr + womwrkAgri + huswrkAgri + HHEADSEXHH + HHMEMTOTAL + HHKIDLT5 + WEALTHQ  + religion_rec| KIDBIRTHMO + KIDBIRTHYR + YEAR + DHSID | 0 | DHSID"))
  
  # Sibling model without KIDSEX, using ':' for interaction
  basemodelEmp_s <-
    formula(paste0(depvar,"~",expvar,":",empvar," + ",expvar,"  + kidtwin_rec  + KIDBORD  + KIDCURAGE | KIDBIRTHMO + KIDBIRTHYR + IDHSPID | 0 | DHSID"))
  
  # Run models and combine results
  temp_res <- bind_rows(
    getEstimatedf(basemodelEmp, empvar, expvar, dfcurr = df1) %>% mutate(name="baseline"),
    getEstimatedf(basemodelEmp_s, empvar, expvar, dfcurr = df2) %>% mutate(name="sibling") 
  )
  
  temp_res
}

# Function including drought indicators in the models
getEstimateGlobaldrought <- function(depvar, empvar, expvar, df1, df2){
  # This function creates models that include drought indicators as controls
  # Useful for examining interactions between environmental shocks, empowerment, and outcomes
  
  # Baseline model with drought_indicator control
  basemodeldrought <-
    formula(paste0(depvar,"~",expvar,"*",empvar," + kidtwin_rec + KIDBORD + KIDCURAGE + KIDSEX + EDYRTOTAL + ETHNICITYNG + HUSEDYRS + MotherAge + MotherAgesqr + womwrkAgri + huswrkAgri + HHEADSEXHH + HHMEMTOTAL + HHKIDLT5 + WEALTHQ  + religion_rec + drought_indicator| KIDBIRTHMO + KIDBIRTHYR + YEAR + DHSID | 0 | DHSID"))
  
  # Sibling model with drought_indicator
  basemodeldrought_s <-
    formula(paste0(depvar,"~",expvar,":",empvar," + ",expvar,"  + kidtwin_rec  + KIDBORD + KIDSEX  + KIDCURAGE + drought_indicator | KIDBIRTHMO + KIDBIRTHYR + IDHSPID | 0 | DHSID"))
  
  # Run models and combine results
  temp_res <- bind_rows(
    getEstimatedf(basemodeldrought, empvar, expvar, dfcurr = df1) %>% mutate(name="baseline"),
    getEstimatedf(basemodeldrought_s, empvar, expvar, dfcurr = df2) %>% mutate(name="sibling") 
  )
  
  temp_res
}

# Function for region-level analysis with empowerment variables
getEstimateGlobalempreg <- function(depvar, empvar, expvar, df1){
  # This function creates models for regional-level analysis
  # Note: Only uses baseline dataset (df1) and has different controls
  # Uses EDUCLVL instead of EDYRTOTAL/HUSEDYRS, suggests region-level aggregated data
  
  # Regional model with different control set and only YEAR as fixed effect
  basemodelEmpreg <-
    formula(paste0(depvar,"~ ",expvar,"*",empvar," + womwrkAgri + huswrkAgri + ETHNICITYNG + HHEADSEXHH + HHMEMTOTAL + MotherAge + MotherAgesqr + EDUCLVL + WEALTHQ  + religion_rec | YEAR | 0 | DHSID"))
  
  # Run model on baseline data only
  temp_res <- bind_rows(
    getEstimatedf(basemodelEmpreg, empvar, expvar, df1) %>% mutate(name="baseline1")
    # Note the empty second argument to bind_rows, suggesting this function originally had more models
  )
  
  temp_res
}

# Function to estimate the effect of an exposure variable (expvar) on an outcome variable (empvar)
# for all women, controlling for various factors.  This is a baseline model.
getEstimateGlobalWomenAut <- function(empvar, expvar, df3) {
  
  # Define the fixed effects model formula.
  # Includes the exposure variable (expvar), woman's agricultural work (womwrkAgri), 
  # husband's agricultural work (huswrkAgri), mother's age (MotherAge and MotherAgesqr),
  # ethnicity (ETHNICITYNG), household size (HHMEMTOTAL), number of children < 5 (HHKIDLT5),
  # wealth (WEALTHQ), and religion (religion_rec).
  # Fixed effects are included for YEAR, and standard errors are clustered at the DHSID level.
  basemodelEmp <-
    formula(paste0(empvar, "~ ", expvar, " + womwrkAgri + huswrkAgri + MotherAge + ETHNICITYNG + MotherAgesqr+ HHMEMTOTAL + HHKIDLT5 + WEALTHQ  + religion_rec | YEAR | 0 | DHSID"))
  
  # Call the helper function getEstimateAut to fit the model and format results.
  # Adds a "Baseline" label to the results.
  temp_res <- bind_rows(
    getEstimateAut(basemodelEmp, empvar, expvar, dfcurr = df3) %>% mutate(name = "Baseline"),
  )
  
  temp_res
}


# Helper function to fit a fixed effects model using felm.
getEstimateAut <- function(formula, empvar, expvar, dfcurr, R = 1000) {
  # Fit the fixed effects model using felm.  na.action = na.omit handles missing data.
  mod <- felm(formula, data = dfcurr, na.action = na.omit)
  
  # Store the model results in a tibble.
  out <- tibble(
    empvar = empvar,
    model = c("baseModel"),
    Results_base = list(mod) # Store the model object in a list column.
  )
  
  out
}


# Function to estimate the effect of an exposure variable (empvar) on a dependent variable (depvar)
# before and after an event, comparing models with and without sibling fixed effects.
getEstimateGlobalprepost <- function(depvar, empvar, df1, df2) {
  
  # Define the fixed effects model formulas for pre- and post-event periods,
  # with and without sibling fixed effects.  These formulas include interactions
  # between the exposure variable and event indicators (numberEventprepost, numberEventpostBHR),
  # along with various control variables and fixed effects.
  basemodelEmp1 <-
    formula(paste0(depvar, "~ numberEventprepost*", empvar, " + kidtwin_rec + KIDSEX + KIDBORD + KIDCURAGE + ETHNICITYNG + womwrkAgri + huswrkAgri + HHEADSEXHH + HHMEMTOTAL + HHKIDLT5 + EDYRTOTAL + HUSEDYRS + MotherAge + MotherAgesqr + WEALTHQ  + religion_rec | KIDBIRTHMO + KIDBIRTHYR + YEAR + DHSID | 0 | DHSID"))
  
  basemodelEmp_s1 <-
    formula(paste0(depvar, "~ numberEventprepost:", empvar, " + numberEventprepost + kidtwin_rec + KIDSEX + KIDBORD + KIDCURAGE | KIDBIRTHMO + KIDBIRTHYR + IDHSPID | 0 | DHSID"))
  
  basemodelEmp2 <-
    formula(paste0(depvar, "~ numberEventpostBHR*", empvar, " + kidtwin_rec + KIDSEX + KIDBORD + KIDCURAGE + ETHNICITYNG + HHEADSEXHH + HHMEMTOTAL + HHKIDLT5 + EDYRTOTAL + HUSEDYRS + MotherAge + MotherAgesqr + WEALTHQ  + womwrkAgri + huswrkAgri  + religion_rec | KIDBIRTHMO + KIDBIRTHYR + YEAR + DHSID | 0 | DHSID"))
  
  basemodelEmp_s2 <-
    formula(paste0(depvar, "~ numberEventpostBHR:", empvar, " + numberEventpostBHR + KIDCURAGE + kidtwin_rec + KIDSEX + KIDBORD | KIDBIRTHMO + KIDBIRTHYR + IDHSPID | 0 | DHSID"))
  
  # Fit the models and combine the results into a single tibble.
  temp_res <- bind_rows(
    getEstimatedf(basemodelEmp2, empvar, expvar = "numberEventpostBHR", dfcurr = df1) %>% mutate(name = "baseline", typeEffect = "without"),
    getEstimatedf(basemodelEmp_s2, empvar, expvar = "numberEventpostBHR", dfcurr = df2) %>% mutate(name = "sibling", typeEffect = "without"),
    getEstimatedf(basemodelEmp1, empvar, expvar = "numberEventprepost", dfcurr = df1) %>% mutate(name = "baseline", typeEffect = "with"),
    getEstimatedf(basemodelEmp_s1, empvar, expvar = "numberEventprepost", dfcurr = df2) %>% mutate(name = "sibling", typeEffect = "with")
  )
  
  temp_res
}


# Function to estimate the effect of an exposure variable (empvar) interacted with birth order (orderbinary).
# Compares models with and without sibling fixed effects.
getEstimateGlobalorder <- function(depvar, empvar, expvar, df1, df2) {
  
  # Define the fixed effects model formulas, including interactions between
  # the exposure variable, birth order, and various control variables.
  basemodelEmp <-
    formula(paste0(depvar, "~", expvar, "*", empvar, "*orderbinary + kidtwin_rec  + KIDCURAGE + EDYRTOTAL + ETHNICITYNG + HUSEDYRS + MotherAge + MotherAgesqr + womwrkAgri + huswrkAgri + HHEADSEXHH + HHMEMTOTAL + HHKIDLT5 + WEALTHQ  + religion_rec| KIDBIRTHMO + KIDBIRTHYR + YEAR + DHSID | 0 | DHSID"))
  
  basemodelEmp_s <-
    formula(paste0(depvar, "~", expvar, ":", empvar, ":orderbinary + ", expvar, ":", empvar, "+", expvar, ":orderbinary + ", expvar, " + kidtwin_rec + KIDSEX + KIDCURAGE | KIDBIRTHMO + KIDBIRTHYR + IDHSPID | 0 | DHSID"))
  
  # Fit the models and combine the results.
  temp_res <- bind_rows(
    getEstimatedforder(basemodelEmp, empvar, expvar, dfcurr = df1) %>% mutate(name = "baseline"),
    getEstimatedforder(basemodelEmp_s, empvar, expvar, dfcurr = df2) %>% mutate(name = "sibling"))
  
  temp_res
}

# Helper function to fit the model, calculate clustered standard errors, and extract coefficients
# for specific combinations of exposure variable, outcome variable, and order/age group.
getEstimatedforder <- function(formula, empvar, expvar, dfcurr, R = 1000) {
  # Fit the fixed effects model.  keepModel = TRUE is essential for later access to the model data.
  mod <- felm(formula, data = dfcurr, keepModel = TRUE, na.action = na.omit)
  vcmod <- vcov(mod) # Get the variance-covariance matrix of the estimated coefficients.
  
  alpha <- c(.025, .05, .95, .975) # Define the quantiles for the confidence intervals.
  modU <- MASS::mvrnorm(R, mu = coef(mod), Sigma = vcmod) # Simulate a distribution of coefficients
  # based on the variance-covariance matrix.
  # This is used for calculating robust standard errors.
  
  matSelect <- matrix(0, nrow = 4, ncol = length(coef(mod))) # Create a selection matrix.
  colnames(matSelect) <- names(coef(mod)) # Name the columns to match the model coefficients.
  
  # Define the names of the interaction terms.  These are crucial for selecting the 
  # correct coefficients from the model.  The "1" likely refers to a specific level
  # of the orderbinary or agegroup variable.
  crosvar1 <- paste0(expvar, ":orderbinary1", sep = "")
  crosvar2 <- paste0(expvar, ":", empvar, "1", sep = "")
  crosvar3 <- paste0(expvar, ":", empvar, "1:orderbinary1", sep = "")
  
  # Fill the selection matrix.  Each row corresponds to a specific combination
  # of the main effect and interaction terms.  A "1" indicates that the term should
  # be included in the calculation of the combined coefficient.
  matSelect[, expvar] <- c(1, 1, 1, 1) # Main effect of expvar.
  matSelect[, crosvar1] <- c(0, 1, 0, 1) # Interaction of expvar and orderbinary1.
  matSelect[, crosvar2] <- c(0, 0, 1, 1) # Interaction of expvar and empvar1.
  matSelect[, crosvar3] <- c(0, 0, 0, 1) # Three-way interaction.
  
  coefs <- as.numeric(matSelect %*% coef(mod)) # Calculate the combined coefficients.
  modU_CI <- t(matSelect %*% t(modU)) # Calculate the distribution of the combined coefficients
  # from the simulated coefficients.
  
  CI <- t(apply(modU_CI, 2, quantile, probs = alpha, na.rm = TRUE)) # Calculate the confidence intervals
  # for the combined coefficients.
  colnames(CI) <- paste0("CI", parse_number(colnames(CI))) # Name the confidence interval columns.
  
  vect_temp <- mod$model %>% pull(empvar) # Extract the outcome variable from the model data.
  # This is used for calculating the number of observations
  # for each group.
  
  # Create a tibble to store the results.
  out <- tibble(
    empvar = empvar,
    estimate = c("No", "No", "Yes", "Yes"), # Labels for the different combinations of effects.
    typeEffect = c("2 and more", "1", "2 and more", "1"), # Labels for the order groups.
    pe = coefs, # Point estimates of the combined coefficients.
    N = nrow(mod$model), # Total number of observations in the model.
    Ntype = c(as.numeric(table(vect_temp)), as.numeric(table(vect_temp))), # Number of observations in each group.
    r.squared = summary(mod)$r.squared # R-squared of the model.
  )
  
  out <- bind_cols(out, as.tibble(CI)) # Add the confidence intervals to the results tibble.
  
  out
}


# Function to estimate the effect of an exposure variable (empvar) interacted with age group (agegroup).
# Compares models with and without sibling fixed effects.
getEstimateGlobalageg <- function(depvar, empvar, expvar, df1, df2) {
  # Define the fixed effects model formulas, including interactions between
  # the exposure variable, age group, and various control variables.
  basemodelEmp <-
    formula(paste0(depvar, "~", expvar, "*", empvar, "*agegroup + kidtwin_rec + EDYRTOTAL + ETHNICITYNG + HUSEDYRS + MotherAge + MotherAgesqr + womwrkAgri + huswrkAgri + HHEADSEXHH + HHMEMTOTAL + HHKIDLT5 + WEALTHQ  + religion_rec| KIDBIRTHMO + KIDBIRTHYR + YEAR + DHSID | 0 | DHSID"))
  
  basemodelEmp_s <-
    formula(paste0(depvar, "~", expvar, ":", empvar, ":agegroup + ", expvar, ":", empvar, "+", expvar, ":agegroup + ", expvar, " + kidtwin_rec + KIDSEX | KIDBIRTHMO + KIDBIRTHYR + IDHSPID | 0 | DHSID"))
  
  # Fit the models and combine the results.
  temp_res <- bind_rows(
    getEstimatedfageg(basemodelEmp, empvar, expvar, dfcurr = df1) %>% mutate(name = "baseline"),
    getEstimatedfageg(basemodelEmp_s, empvar, expvar, dfcurr = df2) %>% mutate(name = "sibling"))
  
  temp_res
}


# Helper function for getEstimateGlobalageg, very similar to getEstimatedforder.
getEstimatedfageg <- function(formula, empvar, expvar, dfcurr, R = 1000) {
  mod <- felm(formula, data = dfcurr, keepModel = TRUE, na.action = na.omit)
  vcmod <- vcov(mod)
  
  alpha <- c(.025, .05, .95, .975)
  modU <- MASS::mvrnorm(R, mu = coef(mod), Sigma = vcmod)
  
  matSelect <- matrix(0, nrow = 4, ncol = length(coef(mod)))
  colnames(matSelect) <- names(coef(mod))
  
  crosvar1 <- paste0(expvar, ":agegroup1", sep = "")
  crosvar2 <- paste0(expvar, ":", empvar, "1", sep = "")
  crosvar3 <- paste0(expvar, ":", empvar, "1:agegroup1", sep = "")
  
  matSelect[, expvar] <- c(1, 1, 1, 1)
  matSelect[, crosvar1] <- c(0, 1, 0, 1)
  matSelect[, crosvar2] <- c(0, 0, 1, 1)
  matSelect[, crosvar3] <- c(0, 0, 0, 1)
  
  coefs <- as.numeric(matSelect %*% coef(mod))
  modU_CI <- t(matSelect %*% t(modU))
  
  CI <- t(apply(modU_CI, 2, quantile, probs = alpha, na.rm = TRUE))
  colnames(CI) <- paste0("CI", parse_number(colnames(CI)))
  
  vect_temp <- mod$model %>% pull(empvar)
  
  out <- tibble(
    empvar = empvar,
    estimate = c("No", "No", "Yes", "Yes"),
    typeEffect = c("2-5", "0-1", "2-5", "0-1"),
    pe = coefs,
    N = nrow(mod$model),
    Ntype = c(as.numeric(table(vect_temp)), as.numeric(table(vect_temp))),
    r.squared = summary(mod)$r.squared
  )
  out <- bind_cols(out, as.tibble(CI))
  
  out
}
# Function to estimate the effect of an exposure variable (empvar) on a dependent variable,
# distinguishing between pre- and post-intervention periods (numberEventpost, numberEventTotal).
getEstimatepostbif <- function(formula, empvar, dfcurr, R = 1000) {
  
  # Fit the fixed effects model. keepModel = TRUE is crucial for accessing model data later.
  mod <- felm(formula, data = dfcurr, keepModel = TRUE, na.action = na.omit)
  vcmod <- vcov(mod) # Get the variance-covariance matrix of the coefficients.
  
  alpha <- c(.025, .05, .95, .975) # Define quantiles for confidence intervals.
  modU <- MASS::mvrnorm(R, mu = coef(mod), Sigma = vcmod) # Simulate coefficient draws for robust SEs.
  
  matSelect <- matrix(0, nrow = 4, ncol = length(coef(mod))) # Create a selection matrix.
  colnames(matSelect) <- names(coef(mod)) # Name columns to match coefficients.
  
  # Define interaction term names.  "1" likely indicates a specific level of the variable.
  crosvar1 <- paste0("numberEventpost:", empvar, "1", sep = "")
  crosvar2 <- paste0("numberEventTotal:", empvar, "1", sep = "")
  
  # Fill the selection matrix. Each row selects a combination of main and interaction effects.
  matSelect[, "numberEventpost"] <- c(1, 1, 0, 0) # Select the main effect of numberEventpost.
  matSelect[, crosvar1] <- c(0, 1, 0, 0)        # Select the interaction of numberEventpost and empvar1.
  matSelect[, "numberEventTotal"] <- c(0, 0, 1, 1) # Select the main effect of numberEventTotal.
  matSelect[, crosvar2] <- c(0, 0, 0, 1)        # Select the interaction of numberEventTotal and empvar1.
  
  coefs <- as.numeric(matSelect %*% coef(mod)) # Calculate combined coefficients.
  modU_CI <- t(matSelect %*% t(modU)) # Calculate distribution of combined coefficients.
  
  CI <- t(apply(modU_CI, 2, quantile, probs = alpha, na.rm = TRUE)) # Calculate confidence intervals.
  colnames(CI) <- paste0("CI", parse_number(colnames(CI))) # Name CI columns.
  
  vect_temp <- mod$model %>% pull(empvar) # Extract the outcome variable for calculating N by group.
  
  # Create a tibble to store the results.
  out <- tibble(
    empvar = empvar,
    estimate = c("No", "Yes", "No", "Yes"), # Labels for pre/post and interaction effects.
    typeEffect = c("Post", "Post", "Pre", "Pre"), # Labels for pre/post periods.
    pe = coefs, # Point estimates of combined coefficients.
    N = nrow(mod$model), # Total N.
    Ntype = c(as.numeric(table(vect_temp)), as.numeric(table(vect_temp))), # N by group.
    r.squared = summary(mod)$r.squared # Model R-squared.
  )
  out <- bind_cols(out, as.tibble(CI)) # Add CIs to the tibble.
  
  out
}


# Function to estimate effects, likely for a difference-in-differences or similar design.
getEstimatedf <- function(formula, empvar, expvar1, expvar2, dfcurr, R = 1000) {
  mod <- felm(formula, data = dfcurr, keepModel = TRUE, na.action = na.omit)
  vcmod <- vcov(mod)
  
  coef_mat <- coef(mod)
  coef_mat[is.na(coef_mat)] <- 0 # Replace any NA coefficients with 0.  This is important
  # for the matrix multiplication to work correctly, as NA values
  # would propagate through the multiplication.
  
  alpha <- c(.025, .05, .95, .975)
  modU <- MASS::mvrnorm(R, mu = coef_mat, Sigma = vcmod)
  
  matSelect <- matrix(0, nrow = 2, ncol = length(coef_mat))
  colnames(matSelect) <- names(coef(mod))
  
  crosvar1 <- paste0(expvar1, "1:", expvar2, "1:", empvar, "1", sep = "")
  crosvar2 <- paste0(expvar1, "1:", expvar2, "1", sep = "")
  
  matSelect[, crosvar2] <- c(1, 1) # Select the interaction of expvar1 and expvar2.
  matSelect[, crosvar1] <- c(0, 1) # Select the three-way interaction.
  
  coefs <- as.numeric(matSelect %*% coef_mat)
  modU_CI <- t(matSelect %*% t(modU))
  
  CI <- t(apply(modU_CI, 2, quantile, probs = alpha, na.rm = TRUE))
  colnames(CI) <- paste0("CI", parse_number(colnames(CI)))
  
  vect_temp <- mod$model %>% pull(empvar) # All observations.
  vect_temp_2 <- mod$model %>% filter(bornpostBH == 1 & bh_treat_lga == 1) %>% pull(empvar) # Treated group.
  
  out <- tibble(
    empvar = empvar,
    estimate = c("No", "Yes"), # Labels for the effects being estimated.
    pe = coefs, # Point estimates.
    N = nrow(mod$model), # Total N.
    Ntype = as.numeric(table(vect_temp)), # N in each group (likely treatment/control).
    Ntype_treat = as.numeric(table(vect_temp_2)), # N in the treated group.
    r.squared = summary(mod)$r.squared # Model R-squared.
  )
  out <- bind_cols(out, as.tibble(CI))
  
  out
}
# Helper function to fit a fixed effects model (likely a logit model due to the name) and 
# calculate clustered standard errors.
getEstimatedf_logit <- function(formula, empvar, expvar, dfcurr, R = 1000) {
  # Fit the fixed effects model. keepModel=TRUE is crucial for accessing model data later.
  mod <- felm(formula, data = dfcurr, keepModel = TRUE, na.action = na.omit)
  vcmod <- vcov(mod) # Get the variance-covariance matrix of the coefficients.
  
  alpha <- c(.025, .05, .95, .975) # Define quantiles for confidence intervals.
  modU <- MASS::mvrnorm(R, mu = coef(mod), Sigma = vcmod) # Simulate coefficient draws for robust SEs.
  
  matSelect <- matrix(0, nrow = 2, ncol = length(coef(mod))) # Create a selection matrix.
  colnames(matSelect) <- names(coef(mod)) # Name columns to match coefficients.
  
  crosvar1 <- paste0(expvar, ":", empvar, "1", sep = "") # Define the interaction term name.
  
  matSelect[, expvar] <- c(1, 1) # Select the main effect of expvar.
  matSelect[, crosvar1] <- c(0, 1) # Select the interaction of expvar and empvar1.
  
  coefs <- as.numeric(matSelect %*% coef(mod)) # Calculate combined coefficients.
  modU_CI <- t(matSelect %*% t(modU)) # Calculate distribution of combined coefficients.
  
  CI <- t(apply(modU_CI, 2, quantile, probs = alpha, na.rm = TRUE)) # Calculate confidence intervals.
  colnames(CI) <- paste0("CI", parse_number(colnames(CI))) # Name CI columns.
  
  vect_temp <- mod$model %>% pull(empvar) # Extract the outcome variable for calculating N by group.
  
  # Create a tibble to store the results.
  out <- tibble(
    empvar = empvar,
    estimate = c("No", "Yes"), # Labels for main effect and interaction.
    pe = coefs, # Point estimates of combined coefficients.
    N = nrow(mod$model), # Total N.
    Ntype = as.numeric(table(vect_temp)), # N by group.
    r.squared = summary(mod)$r.squared # Model R-squared.
  )
  out <- bind_cols(out, as.tibble(CI)) # Add CIs to the tibble.
  
  out
}


# Function to estimate the effect of an exposure variable (expvar) interacted with sex (KIDSEX).
# Compares models with and without sibling fixed effects.
getEstimateGlobalsex <- function(depvar, empvar, expvar, df1, df2) {
  
  # Define the fixed effects model formulas, including interactions between
  # the exposure variable, sex, and various control variables.
  basemodelEmp <-
    formula(paste0(depvar, "~", expvar, "*", empvar, "*KIDSEX + kidtwin_rec + KIDBORD + KIDCURAGE + EDYRTOTAL + ETHNICITYNG + HUSEDYRS + MotherAge + MotherAgesqr + womwrkAgri + huswrkAgri + HHEADSEXHH + HHMEMTOTAL + HHKIDLT5 + WEALTHQ  + religion_rec| KIDBIRTHMO + KIDBIRTHYR + YEAR + DHSID | 0 | DHSID"))
  
  basemodelEmp_s <-
    formula(paste0(depvar, "~", expvar, ":", empvar, ":KIDSEX + ", expvar, ":", empvar, "+", expvar, ":KIDSEX + ", expvar, " + kidtwin_rec + KIDSEX + KIDBORD + KIDCURAGE | KIDBIRTHMO + KIDBIRTHYR + IDHSPID | 0 | DHSID"))
  
  # Fit the models and combine the results.
  temp_res <- bind_rows(
    getEstimatedfsex(basemodelEmp, empvar, expvar, dfcurr = df1) %>% mutate(name = "baseline"),
    getEstimatedfsex(basemodelEmp_s, empvar, expvar, dfcurr = df2) %>% mutate(name = "sibling"))
  
  temp_res
}


# Helper function for getEstimateGlobalsex, very similar to getEstimatedforder/getEstimatedfageg.
getEstimatedfsex <- function(formula, empvar, expvar, dfcurr, R = 1000) {
  mod <- felm(formula, data = dfcurr, keepModel = TRUE, na.action = na.omit)
  vcmod <- vcov(mod)
  
  alpha <- c(.025, .05, .95, .975)
  modU <- MASS::mvrnorm(R, mu = coef(mod), Sigma = vcmod)
  
  matSelect <- matrix(0, nrow = 4, ncol = length(coef(mod)))
  colnames(matSelect) <- names(coef(mod))
  
  crosvar1 <- paste0(expvar, ":KIDSEX1", sep = "")
  crosvar2 <- paste0(expvar, ":", empvar, "1", sep = "")
  crosvar3 <- paste0(expvar, ":", empvar, "1:KIDSEX1", sep = "")
  
  matSelect[, expvar] <- c(1, 1, 1, 1)
  matSelect[, crosvar1] <- c(0, 1, 0, 1)
  matSelect[, crosvar2] <- c(0, 0, 1, 1)
  matSelect[, crosvar3] <- c(0, 0, 0, 1)
  
  coefs <- as.numeric(matSelect %*% coef(mod))
  modU_CI <- t(matSelect %*% t(modU))
  
  CI <- t(apply(modU_CI, 2, quantile, probs = alpha, na.rm = TRUE))
  colnames(CI) <- paste0("CI", parse_number(colnames(CI)))
  
  vect_temp <- mod$model %>% pull(empvar)
  
  out <- tibble(
    empvar = empvar,
    estimate = c("No", "No", "Yes", "Yes"),
    typeEffect = c("female", "male", "female", "male"),
    pe = coefs,
    N = nrow(mod$model),
    Ntype = c(as.numeric(table(vect_temp)), as.numeric(table(vect_temp))),
    r.squared = summary(mod)$r.squared
  )
  out <- bind_cols(out, as.tibble(CI))
  
  out
}
# Function to estimate the effect of an exposure variable (expvar) interacted with a binary variable (empvar).
# Compares models with and without sibling fixed effects.
getEstimatebinary <- function(depvar, empvar, expvar, df1, df2) {
  
  # Define the fixed effects model formulas, including interactions between
  # the exposure variable, the binary variable, and various control variables.
  basemodelEmp <-
    formula(paste0(depvar, "~ ", expvar, "*", empvar, " + kidtwin_rec + KIDSEX + KIDBORD + ETHNICITYNG+ EDYRTOTAL + HUSEDYRS  + MotherAge + MotherAgesqr + womwrkAgri + huswrkAgri + HHEADSEXHH + HHMEMTOTAL + HHKIDLT5 + WEALTHQ  + religion_rec + KIDCURAGE| KIDBIRTHMO + KIDBIRTHYR + YEAR + DHSID | 0 | DHSID"))
  basemodelEmp_s <-
    formula(paste0(depvar, "~", expvar, ":", empvar, " + ", expvar, "  + kidtwin_rec  + KIDBORD + KIDSEX  + KIDCURAGE | KIDBIRTHMO + KIDBIRTHYR + IDHSPID | 0 | DHSID"))
  
  # Fit the models.
  modbasemodelTot <- felm(basemodelEmp, data = df1)
  modbasemodelTot_s <- felm(basemodelEmp_s, data = df2)
  
  # Store the model results in a tibble.
  out <- tibble(
    model = c("baseModel", "baseModelS"),
    Results_base = list(modbasemodelTot, modbasemodelTot_s) # Store the model objects.
  )
  
  out
}


# Helper function to fit the model, calculate clustered standard errors, and extract coefficients.
getEstimatedfbinary <- function(formula, empvar, expvar, dfcurr, R = 1000) {
  # Fit the fixed effects model. keepModel=TRUE is essential for accessing model data later.
  mod <- felm(formula, data = dfcurr, keepModel = TRUE, na.action = na.omit)
  vcmod <- vcov(mod) # Get the variance-covariance matrix.
  
  alpha <- c(.025, .05, .95, .975) # Quantiles for confidence intervals.
  modU <- MASS::mvrnorm(R, mu = coef(mod), Sigma = vcmod) # Simulate coefficient draws.
  
  matSelect <- matrix(0, nrow = 2, ncol = length(coef(mod))) # Selection matrix.
  colnames(matSelect) <- names(coef(mod)) # Name columns.
  
  crosvar1 <- paste0(expvar, "1", sep = "") # Interaction term name.
  crosvar2 <- paste0(expvar, "1:", empvar, "1", sep = "") #Interaction term name.
  
  matSelect[, crosvar1] <- c(1, 1) # Select main effect of expvar.
  matSelect[, crosvar2] <- c(0, 1) # Select interaction of expvar and empvar.
  
  coefs <- as.numeric(matSelect %*% coef(mod)) # Calculate combined coefficients.
  modU_CI <- t(matSelect %*% t(modU)) # Distribution of combined coefficients.
  
  CI <- t(apply(modU_CI, 2, quantile, probs = alpha, na.rm = TRUE)) # Confidence intervals.
  colnames(CI) <- paste0("CI", parse_number(colnames(CI))) # Name CI columns.
  
  vect_temp <- mod$model %>% pull(empvar) # Extract the binary variable.
  
  out <- tibble(
    empvar = empvar,
    estimate = c("No", "Yes"), # Labels for effects.
    typeEffect = c("without", "with"), # Labels for interaction.
    pe = coefs, # Point estimates.
    N = nrow(mod$model), # Total N.
    Ntype = as.numeric(table(vect_temp)), # N by group (levels of empvar).
    r.squared = summary(mod)$r.squared # R-squared.
  )
  out <- bind_cols(out, as.tibble(CI)) # Add CIs.
  
  out
}

#### SPECIFIC TIME FUNCTION ######################


# Function to estimate the effect of early life events (numberEvent9_6, numberEvent6_3, numberEvent3_0) 
# on a dependent variable (depvar).  Compares models with and without sibling fixed effects.
getEstimateSpecific <- function(depvar, df1, df2) {
  
  # Define the fixed effects model formulas. These include controls for various
  # socioeconomic and demographic factors.
  basemodelTot <-
    formula(paste0(depvar, "~ numberEvent9_6 + numberEvent6_3 + numberEvent3_0 + kidtwin_rec + KIDSEX +    KIDBORD +  + ageSqrt + MotherAge + MotherAgesqr + husjob_rec + HHEADSEXHH + HHMEMTOTAL + HHKIDLT5 + WEALTHQ  + wkcurrjob_rec + EDUCLVL + religion_rec | KIDBIRTHYR + YEAR + DHSID | 0 | DHSID"))
  
  basemodelTot_s <-
    formula(paste0(depvar, "~ numberEvent9_6 + numberEvent6_3 + numberEvent3_0 + kidtwin_rec + KIDSEX +    KIDBORD +  + ageSqrt  + MotherAge + MotherAgesqr  | KIDBIRTHYR + IDHSPID | 0 | DHSID"))
  
  # Fit the models.
  modbasemodelTot <- felm(basemodelTot, data = df1)
  modbasemodelTot_s <- felm(basemodelTot_s, data = df2)
  
  # Store the model results in a tibble.
  out <- tibble(
    model = c("baseModel", "baseModelS"),
    Results_base = list(modbasemodelTot, modbasemodelTot_s) # Store the model objects.
  )
  
  out
}


# Function to estimate the effect of early life events and an empowerment variable (empvar) 
# on a dependent variable (depvar).  This version includes the empowerment variable as a 
# *separate* predictor, not interacted with the early life events.
getEstimateSpecificwithemp <- function(depvar, empvar, df1) {
  
  # Define the fixed effects model formula.
  basemodelEmp1 <-
    formula(paste0(depvar, "~ numberEvent9_6 + numberEvent6_3 + numberEvent3_0 + ", empvar, " + kidtwin_rec + KIDSEX +    KIDBORD +  + ageSqrt + husjob_rec + HHEADSEXHH + HHMEMTOTAL + HHKIDLT5  + MotherAge + MotherAgesqr + WEALTHQ  + wkcurrjob_rec + religion_rec | KIDBIRTHYR + YEAR + DHSID | 0 | DHSID"))
  
  # Fit the model.
  modbasemodelEmp1 <- felm(basemodelEmp1, data = df1)
  
  # Store the model results.
  out <- tibble(
    model = "baseModel",
    empvar = empvar,
    Results_base = list(modbasemodelEmp1)
  )
  
  out
}


# Function to estimate the effect of early life events interacted with an empowerment variable (empvar).
# Compares models with and without sibling fixed effects.
getEstimateSpecificemp <- function(depvar, empvar, df1, df2) {
  
  # Define the fixed effects model formulas, including interactions between
  # the empowerment variable and the early life event variables.
  basemodelEmp <-
    formula(paste0(depvar, "~ ", empvar, "*(numberEvent9_6 + numberEvent6_3 + numberEvent3_0) + kidtwin_rec + KIDSEX + kidfirstborn + husjob_rec + HHEADSEXHH + HHMEMTOTAL + HHKIDLT5  + MotherAge + MotherAgesqr + WEALTHQ  + wkcurrjob_rec  + religion_rec | KIDBIRTHYR + YEAR + DHSID | 0 | DHSID", sep = ""))
  
  basemodelEmp_s <-
    formula(paste0(depvar, "~ ", empvar, ":numberEvent9_6 + numberEvent9_6 + ", empvar, ":numberEvent6_3 + numberEvent6_3 + ", empvar, ":numberEvent3_0 + numberEvent3_0 + kidtwin_rec + KIDSEX + kidfirstborn +  MotherAge + MotherAgesqr | KIDBIRTHYR + IDHSPID | 0 | DHSID", sep = ""))
  
  # Fit the models and combine the results.
  temp_res <- bind_rows(
    getEstimatedfsp(basemodelEmp, empvar, dfcurr = df1) %>% mutate(name = "baseline"),
    getEstimatedfsp(basemodelEmp_s, empvar, dfcurr = df2) %>% mutate(name = "sibling"))
  
  temp_res
}


# Helper function to fit the model, calculate clustered standard errors, and extract coefficients
# for the interaction terms.
getEstimatedfsp <- function(formula, empvar, dfcurr, R = 1000) {
  
  mod <- felm(formula, data = dfcurr, keepModel = TRUE, na.action = na.omit)
  vcmod <- vcov(mod)
  
  alpha <- c(.025, .05, .95, .975)
  modU <- MASS::mvrnorm(R, mu = coef(mod), Sigma = vcmod)
  
  matSelect <- matrix(0, nrow = 6, ncol = length(coef(mod))) # Selection matrix.
  colnames(matSelect) <- names(coef(mod))
  
  # Define interaction term names.
  crosvar1 <- paste0(empvar, "1:", "numberEvent9_6", sep = "")
  crosvar2 <- paste0(empvar, "1:", "numberEvent6_3", sep = "")
  crosvar3 <- paste0(empvar, "1:", "numberEvent3_0", sep = "")
  
  # Fill the selection matrix. Each row selects a combination of main and interaction effects.
  matSelect[, "numberEvent9_6"] <- c(1, 1, 0, 0, 0, 0) # Main effect of numberEvent9_6 and its interaction
  matSelect[, "numberEvent6_3"] <- c(0, 0, 1, 1, 0, 0) # Main effect of numberEvent6_3 and its interaction
  matSelect[, "numberEvent3_0"] <- c(0, 0, 0, 0, 1, 1) # Main effect of numberEvent3_0 and its interaction
  matSelect[, crosvar1] <- c(0, 1, 0, 0, 0, 0) # Interaction of empvar and numberEvent9_6
  matSelect[, crosvar2] <- c(0, 0, 0, 1, 0, 0) # Interaction of empvar and numberEvent6_3
  matSelect[, crosvar3] <- c(0, 0, 0, 0, 0, 1) # Interaction of empvar and numberEvent3_0
  
  coefs <- as.numeric(matSelect %*% coef(mod)) # Calculate combined coefficients.
  modU_CI <- t(matSelect %*% t(modU)) # Distribution of combined coefficients.
  
  CI <- t(apply(modU_CI, 2, quantile, probs = alpha, na.rm = TRUE)) # Confidence intervals.
  colnames(CI) <- paste0("CI", parse_number(colnames(CI))) # Name CI columns.
  
  vect_temp <- mod$model %>% pull(empvar) # Extract the empowerment variable.
  
  out <- tibble(
    empVariable = empvar,
    typeEffect = c("exposureInt9_6", "exposureInt9_6", "exposureInt6_3", "exposureInt6_3",
                   "exposureInt3_0", "exposureInt3_0"), # Labels for effects.
    estimate = c("No", "Yes", "No", "Yes", "No", "Yes"), # Labels for main and interaction effects.
    pe = coefs, # Point estimates.
    N = nrow(mod$model), # Total N.
    Ntype = c(as.numeric(table(vect_temp)), as.numeric(table(vect_temp)),as.numeric(table(vect_temp))), # N by group.
    r.squared = summary(mod)$r.squared # R-squared.
  )
  out <- bind_cols(out, as.tibble(CI)) # Add CIs.
  
  out
}
# Function to estimate the effect of an exposure variable (expvar) interacted with an empowerment index (empvar).
# Compares models with and without sibling fixed effects.  The `type` argument allows for slight 
# variations in the control variables included in the model.
getEstimate_mca <- function(depvar, empvar = "emp_index", expvar, df1, df2, type = 1) {
  
  # Define the fixed effects model formulas.
  if (type == 1) {
    basemodelEmp <-
      formula(paste0(depvar, "~ ", expvar, "*", empvar, "  + kidtwin_rec + KIDSEX + KIDBORD + KIDCURAGE  + ETHNICITYNG+ EDYRTOTAL + HUSEDYRS + MotherAge + MotherAgesqr + womwrkAgri + huswrkAgri + HHEADSEXHH + HHMEMTOTAL + HHKIDLT5 + WEALTHQ  + religion_rec| KIDBIRTHMO + KIDBIRTHYR + YEAR + DHSID | 0 | DHSID"))
    
    basemodelEmp_s <-
      formula(paste0(depvar, "~ ", expvar, ":", empvar, " + ", expvar, "  + kidtwin_rec + KIDSEX + KIDBORD + KIDCURAGE | KIDBIRTHMO + KIDBIRTHYR + IDHSPID | 0 | DHSID"))
  } else { # type != 1
    basemodelEmp <-
      formula(paste0(depvar, "~ ", expvar, "*", empvar, " + kidtwin_rec + KIDSEX + KIDBORD + KIDCURAGE+ ETHNICITYNG+ EDYRTOTAL + HUSEDYRS  + MotherAge + MotherAgesqr + womwrkAgri + huswrkAgri + HHEADSEXHH + HHMEMTOTAL + HHKIDLT5 + WEALTHQ  + religion_rec| KIDBIRTHMO + KIDBIRTHYR + YEAR + DHSID | 0 | DHSID"))
    
    basemodelEmp_s <-
      formula(paste0(depvar, "~ ", expvar, ":", empvar, " + ", expvar, " + kidtwin_rec + KIDSEX + KIDBORD + KIDCURAGE | KIDBIRTHMO + KIDBIRTHYR + IDHSPID | 0 | DHSID"))
  }
  
  # Fit the models and combine the results.
  temp_res <- bind_rows(
    getEstimatedf(basemodelEmp, empvar, expvar, dfcurr = df1) %>% mutate(name = "baseline"),
    getEstimatedf(basemodelEmp_s, empvar, expvar, dfcurr = df2) %>% mutate(name = "sibling"))
  
  temp_res
}



# Function to estimate a model, likely a non-linear model given the use of `feglm`, with interactions.
# Compares models with and without sibling fixed effects.
NaNGlobal <- function(depvar, expvar1, expvar2, df1, df2) {
  
  # Define the model formulas, including interactions.
  basemodelTot <-
    formula(paste0(depvar, "~ ", expvar1, "*", expvar2, "+ kidtwin_rec + KIDSEX + KIDBORD + KIDCURAGE + ETHNICITYNG + EDYRTOTAL + HUSEDYRS + womwrkAgri + huswrkAgri +  MotherAge  + MotherAgesqr + HHEADSEXHH + HHMEMTOTAL + HHKIDLT5 + WEALTHQ + religion_rec| KIDBIRTHMO + YEAR | 0 | DHSID"))
  
  basemodelTot_s <-
    formula(paste0(depvar, "~ ", expvar1, "*", expvar2, "+ kidtwin_rec + KIDSEX + KIDBORD + KIDCURAGE | IDHSPID + KIDBIRTHMO + YEAR| 0 | DHSID"))
  
  # Fit the models using feglm (likely for a non-linear model).
  modbasemodelTot <- feglm(basemodelTot, data = df1)
  modbasemodelTot_s <- feglm(basemodelTot_s, data = df2)
  
  # Store the model results.
  out <- tibble(
    model = c("baseModel", "baseModelS"),
    Results_base = list(modbasemodelTot, modbasemodelTot_s)
  )
  
  out
}




