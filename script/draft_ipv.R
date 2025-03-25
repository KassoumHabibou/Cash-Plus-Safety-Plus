# ################################################################################
######################## Cleaning the IPV data #################################
################################################################################

# Create a sample variable
ipv_df <- ipv_df %>%
  mutate(sample = 1 %>% structure(label = "Is in the Maghama Pilot sample [yes=1]") , 
         hhid = as.numeric(hhid)) # %>% # Convert hhid to numeric
# filter(consent == 1) # Handling survey completion and consent


# Changing in the columns names all containing "vip" to "ipv"
ipv_df <- ipv_df %>%
  rename_with(~ gsub("vip", "ipv", .x))
names(ipv_df)

# Recoding "Don't Know" responses (97 or -97) as NA
ipv_df <- ipv_df %>%
  mutate(across(all_of(c("mr1", "mr2", "mr3", "mr4", "mr5", "mr7")), ~ifelse(. %in% c(97, -97), NA, .)))

# Recode binary variables (2 -> 0)
ipv_df <- ipv_df %>%
  mutate(across(all_of(c("mr1", "mr3", "mr4", "mr5", "mr7", "mr8", "mr10")), ~ifelse(. == 2, 0, .)),
         across(starts_with("f_ipv_contra_type_"), as.factor))


# Create new variables with labels
ipv_df <- ipv_df %>%
  mutate(
    f_ipv_husb = mr1 %>% structure(label = "Woman lives with husband [yes=1]"),
    f_ipv_agemarr = mr2 %>% structure(label = "Age at marriage with current husband"),
    f_ipv_polyg = mr3 %>% structure(label = "Husband is polygamous [yes=1]"),
    f_ipv_marrbefore = mr4 %>% structure(label = "Married before current marriage [yes=1]"),
    f_ipv_everpreg = mr5 %>% structure(label = "Ever pregnant [yes=1]"),
    f_ipv_preg_nb = mr6 %>% structure(label = "Number of pregnancies") ,
    f_ipv_contra_ever = mr7%>% structure(label = "Ever used any contraception methods [yes=1]"),
    f_ipv_contra_now = mr8%>% structure(label = "Currently using a contraception method [yes=1]"),
    f_ipv_contra_type = mr9b%>% structure(label = "Type of contraception currently used"),
    f_ipv_husb_knows = mr10%>% structure(label = "Husband knows about the current contraception method [yes=1]")
    
    # Contraception
    # f_ipv_contra_type_1 = f_ipv_contra_type_1 %>% structure(label = "Contraception currently used: Sterilization"), 
    # f_ipv_contra_type_2 = f_ipv_contra_type_2 %>% structure(label = "Contraception currently used: IUD"),
    # f_ipv_contra_type_3 = f_ipv_contra_type_3%>% structure(label = "Contraception currently used: Injectables"), 
    # f_ipv_contra_type_4 = f_ipv_contra_type_4%>% structure(label = "Contraception currently used: Implant"),
    # f_ipv_contra_type_5 = f_ipv_contra_type_5%>% structure(label = "Contraception currently used: Pill"), 
    # f_ipv_contra_type_6 = f_ipv_contra_type_6%>% structure(label = "Contraception currently used: Emergency pill"),
    # f_ipv_contra_type_7 = f_ipv_contra_type_7%>% structure(label = "Contraception currently used: LAM (Lactational Amenorrhea Method)")
    
  )

# Table like in stata
for(column in c("mr1","mr2", "mr3", "mr4", "mr5", "mr6", "mr7", "mr8", "mr10")){
  ipv_df %>%
    group_by(sample, .data[[column]]) %>% 
    summarise(count = n(), .groups = "drop") %>%
    mutate(percent = count / sum(count) * 100) %>%
    print() 
}




# List of variables
econ_ipv_vars <- c("ipv1", "ipv2", "ipv3", "ipv5", "ipv6", "ipv7")
emotional_ipv_vars <- paste0("ipv",8:11, rep(c("a", "b", "c"), each = 4))
physical_ipv_vars <- paste0("ipv", 12:17, rep(c("a", "b", "c"), each = 6))
sexual_ipv_vars <- paste0("ipv", 18:20, rep(c("a", "b", "c"), each = 3))
other_ipv_vars <- paste0("ipv", c(22, 23, 25), rep(c("a", "b", "c"), each = 3))

# Apply recoding function
ipv_df <- recode_and_generate(ipv_df, econ_ipv_vars)
ipv_df <- recode_and_generate(ipv_df, emotional_ipv_vars)
ipv_df <- recode_and_generate(ipv_df, physical_ipv_vars)
ipv_df <- recode_and_generate(ipv_df, sexual_ipv_vars)
ipv_df <- recode_and_generate(ipv_df, other_ipv_vars)

# Function to recode variables
# ipv_df <- ipv_df %>%
#   mutate(across(econ_ipv_vars, 
#                 ~ case_when(. == 2 ~ 0,
#                             . == 97 ~ NA_real_,
#                             TRUE ~ .),
#                 .names = "{.col}_rec0")) %>%
#   mutate(across(econ_ipv_vars, 
#                 ~ case_when(. == 2 ~ 0,
#                             . == 97 ~ NA_real_,
#                             TRUE ~ 1),
#                 .names = "{.col}_rec1"))
# 
# # For values 8 to 20
# df <- df %>%
#   mutate(across(emotional_ipv_vars, 
#                 ~ case_when(. == 2 ~ 0,
#                             . == 97 ~ NA_real_,
#                             TRUE ~ .),
#                 .names = "{.col}_rec0")) %>%
#   mutate(across(emotional_ipv_vars, 
#                 ~ case_when(. == 2 ~ 0,
#                             . == 97 ~ NA_real_,
#                             TRUE ~ 1),
#                 .names = "{.col}_rec1"))
# ipv_df <- ipv_df %>%
#   mutate(across(all_of(c(econ_ipv_vars,
#                          econ_ipv_vars,
#                          physical_ipv_vars,
#                          sexual_ipv_vars,
#                          other_ipv_vars)),
#                 ~ as.numeric(.))) %>% 
#   mutate(across(all_of(c(econ_ipv_vars,
#                   econ_ipv_vars,
#                   physical_ipv_vars, 
#                   sexual_ipv_vars, 
#                   other_ipv_vars)), 
#                 ~ case_when(. == 2 ~ 0,
#                             . == 97 ~ NA, 
#                             TRUE ~ .))) %>%
#   mutate(across(all_of(c(econ_ipv_vars,
#                   econ_ipv_vars,
#                   physical_ipv_vars, 
#                   sexual_ipv_vars, 
#                   other_ipv_vars)), 
#                 ~ case_when(. == 2 ~ 0,
#                             . == 97 ~ NA, 
#                             is.na(.) ~ 0,
#                             TRUE ~ .),
#                 .names = "{.col}_rec0")) %>%
#   mutate(across(all_of(c(econ_ipv_vars,
#                   econ_ipv_vars,
#                   physical_ipv_vars,
#                   sexual_ipv_vars,
#                   other_ipv_vars)),
#                 case_when(. == 2 ~ 0,
#                           . == 97 ~ NA,
#                           is.na(.) ~ 1,
#                           TRUE ~ .),
#                 .names = "{.col}_rec1"))
# Define index labels as a factor
index_labels <- c("No", "Yes")

######################## Controlling behavior ##################################


# Create the control_ipv_12m variable
ipv_df <- ipv_df %>%
  mutate(control_ipv_12m = case_when(
    ipv1 == 1 | ipv2 == 1 | ipv3 == 1 | ipv5 == 1 | ipv6 == 1 | ipv7 == 1 ~ 1,
    ipv1 == 0 & ipv2 == 0 & ipv3 == 0 & ipv5 == 0 & ipv6 == 0 & ipv7 == 0 ~ 0,
    TRUE ~ NA
  ) %>% structure(label = "Husband's controlling behaviour Index (last 12 months)")) 

# Create the control_ipv_r0_12m variable (Refusal = 0)
ipv_df <- ipv_df %>%
  mutate(control_ipv_r0_12m = case_when(
    ipv1_rec0 == 1 | ipv2_rec0 == 1 | ipv3_rec0 == 1 | ipv5_rec0 == 1 | ipv6_rec0 == 1 | ipv7_rec0 == 1 ~ 1,
    ipv1_rec0 == 0 & ipv2_rec0 == 0 & ipv3_rec0 == 0 & ipv5_rec0 == 0 & ipv6_rec0 == 0 & ipv7_rec0 == 0 ~ 0,
    TRUE ~ NA
  )%>% structure(label = "Husband's controlling behaviour Index (Refusal = 0)")) 

# Create the control_ipv_r1_12m variable (Refusal = 1)
ipv_df <- ipv_df %>%
  mutate(control_ipv_r1_12m = case_when(
    ipv1_rec1 == 1 | ipv2_rec1 == 1 | ipv3_rec1 == 1 | ipv5_rec1 == 1 | ipv6_rec1 == 1 | ipv7_rec1 == 1 ~ 1,
    ipv1_rec1 == 0 & ipv2_rec1 == 0 & ipv3_rec1 == 0 & ipv5_rec1 == 0 & ipv6_rec1 == 0 & ipv7_rec1 == 0 ~ 0,
    TRUE ~ NA
  )%>% structure(label = "Husband's controlling behaviour Index (12m - Refusal = 1)")) 

# Table like in stata
for(column in c("control_ipv_12m","control_ipv_r0_12m", "control_ipv_r1_12m")){
  ipv_df %>%
    group_by(sample, .data[[column]]) %>% 
    summarise(count = n(), .groups = "drop") %>%
    mutate(percent = count / sum(count) * 100) %>%
    print() 
}

######################## Emotional violence ##################################


# Emotional violence index (Ever)
ipv_df <- ipv_df %>%
  mutate(emo_ipv_ever = case_when(
    ipv8a == 1 | ipv9a == 1 | ipv10a == 1 | ipv11a == 1 ~ 1,
    ipv8a == 0 & ipv9a == 0 & ipv10a == 0 & ipv11a == 0 ~ 0,
    TRUE ~ NA
  )%>% structure(label = "Emotional violence Index (ever)"))

# Emotional violence index (Last 12 months)
ipv_df <- ipv_df %>%
  mutate(emo_ipv_12m = case_when(
    ipv8b == 1 | ipv9b == 1 | ipv10b == 1 | ipv11b == 1 ~ 1,
    ipv8b == 0 & ipv9b == 0 & ipv10b == 0 & ipv11b == 0 ~ 0,
    TRUE ~ NA
  )%>% structure(label = "Emotional violence Index (last 12 months)"),
  emo_ipv_12m = ifelse(emo_ipv_ever== 0 & is.na(emo_ipv_12m),0, emo_ipv_12m)) # Ensure it stays 0 if no past violence was reported)

# Emotional violence index (Ever - Refusal = 0)
ipv_df <- ipv_df %>%
  mutate(emo_ipv_r0_ever = case_when(
    ipv8a_rec0 == 1 | ipv9a_rec0 == 1 | ipv10a_rec0 == 1 | ipv11a_rec0 == 1 ~ 1,
    ipv8a_rec0 == 0 & ipv9a_rec0 == 0 & ipv10a_rec0 == 0 & ipv11a_rec0 == 0 ~ 0,
    TRUE ~ NA
  )%>% structure(label = "Emotional violence Index (ever - Refusal = 0)"))

# Emotional violence index (12m - Refusal = 0)
ipv_df <- ipv_df %>%
  mutate(emo_ipv_r0_12m = case_when(
    ipv8b_rec0 == 1 | ipv9b_rec0 == 1 | ipv10b_rec0 == 1 | ipv11b_rec0 == 1 ~ 1,
    ipv8b_rec0 == 0 & ipv9b_rec0 == 0 & ipv10b_rec0 == 0 & ipv11b_rec0 == 0 ~ 0,
    TRUE ~ NA
  )%>% structure(label = "Emotional violence Index (12m - Refusal = 0)"),
  emo_ipv_r0_12m = ifelse(emo_ipv_r0_ever == 0 & is.na(emo_ipv_r0_12m),0, emo_ipv_r0_12m))

# Emotional violence index (Ever - Refusal = 1)
ipv_df <- ipv_df %>%
  mutate(emo_ipv_r1_ever = case_when(
    ipv8a_rec1 == 1 | ipv9a_rec1 == 1 | ipv10a_rec1 == 1 | ipv11a_rec1 == 1 ~ 1,
    ipv8a_rec1 == 0 & ipv9a_rec1 == 0 & ipv10a_rec1 == 0 & ipv11a_rec1 == 0 ~ 0,
    TRUE ~ NA
  )%>% structure(label = "Emotional violence Index (ever - Refusal = 1)"))

# Emotional violence index (12m - Refusal = 1)
ipv_df <- ipv_df %>%
  mutate(emo_ipv_r1_12m = case_when(
    ipv8b_rec1 == 1 | ipv9b_rec1 == 1 | ipv10b_rec1 == 1 | ipv11b_rec1 == 1 ~ 1,
    ipv8b_rec1 == 0 & ipv9b_rec1 == 0 & ipv10b_rec1 == 0 & ipv11b_rec1 == 0 ~ 0,
    TRUE ~ NA
  )%>% structure(label = "Emotional violence Index (12m - Refusal = 1)")) %>% 
  mutate(emo_ipv_r1_12m = ifelse(emo_ipv_r1_ever == 0  & is.na(emo_ipv_r1_12m),0, emo_ipv_r1_12m))

# Table like in stata
for(column in c("emo_ipv_ever","emo_ipv_12m","emo_ipv_r0_ever","emo_ipv_r0_12m","emo_ipv_r1_ever","emo_ipv_r1_12m")){
  ipv_df %>%
    group_by(sample, .data[[column]]) %>% 
    summarise(count = n(), .groups = "drop") %>%
    mutate(percent = count / sum(count) * 100) %>%
    print() 
}

# Create frequency variables and set to 0 under specified conditions
ipv_df <- ipv_df %>%
  mutate(
    freq8 = ipv8c,
    freq9 = ipv9c,
    freq10 = ipv10c,
    freq11 = ipv11c
  ) %>%
  mutate(
    freq8 = ifelse(ipv8b == 0 | ipv8a == 0, 0, freq8),
    freq9 = ifelse(ipv9b == 0 | ipv9a == 0, 0, freq9),
    freq10 = ifelse(ipv10b == 0 | ipv10a == 0, 0, freq10),
    freq11 = ifelse(ipv11b == 0 | ipv11a == 0, 0, freq11)
  )

# Generate summary tables for each frequency variable
freq_vars <- c("freq8", "freq9", "freq10", "freq11")

# Calculate emotional severity index (mean across rows)
ipv_df <- ipv_df %>%
  rowwise() %>%
  mutate(
    # Calculate mean, ignoring NA values unless all values are NA
    emo_severity_12m = mean(c_across(all_of(freq_vars)), na.rm = TRUE)%>% 
      structure(label = "Emotional violence Severity Index (last 12 months)")
  ) %>%
  ungroup()

# Calculate z-scores 
ipv_df <- ipv_df %>%
  mutate(emo_severity_12m_z = scale(emo_severity_12m, center = TRUE, scale = TRUE)%>% 
           structure(label = "Emotional violence Severity Z-score (last 12 months)"))


# Summary statistics for non-zero values
cat("\nSummary statistics for emo_severity_12m > 0:\n")
ipv_df %>%
  filter(emo_severity_12m > 0) %>%
  summarize(
    count = n(),
    mean = mean(emo_severity_12m),
    median = median(emo_severity_12m),
    min = min(emo_severity_12m),
    max = max(emo_severity_12m),
    q1 = quantile(emo_severity_12m, 0.25),
    q3 = quantile(emo_severity_12m, 0.75)
  ) %>%
  print()

# Number of women with score above 0
women_above_zero <- sum(ipv_df$emo_severity_12m > 0, na.rm = TRUE)
cat("Number of women with emo_severity_12m > 0:", women_above_zero, "\n")

# Drop temporary frequency variables
ipv_df <- ipv_df %>%
  select(-all_of(freq_vars))

######################## Physical violence #####################################

# Create and label 'phy_ipv_ever'
ipv_df <- ipv_df %>%
  mutate(phy_ipv_ever = case_when(
    ipv12a == 1 | ipv13a == 1 | ipv14a == 1 | ipv15a == 1 | ipv16a == 1 | ipv17a == 1 ~ 1,
    ipv12a == 0 & ipv13a == 0 & ipv14a == 0 & ipv15a == 0 & ipv16a == 0 & ipv17a == 0 ~ 0,
    TRUE ~ NA
  )%>% structure(label = "Physical violence Index (ever)"))  # Optional label for the variable


# Create and label 'phy_ipv_12m'
ipv_df <- ipv_df %>%
  mutate(phy_ipv_12m = case_when(
    ipv12b == 1 | ipv13b == 1 | ipv14b == 1 | ipv15b == 1 | ipv16b == 1 | ipv17b == 1 ~ 1,
    ipv12b == 0 & ipv13b == 0 & ipv14b == 0 & ipv15b == 0 & ipv16b == 0 & ipv17b == 0 ~ 0,
    TRUE ~ NA
  )%>% structure(label = "Physical violence Index (last 12 months)"),
  phy_ipv_12m = ifelse(phy_ipv_ever == 0 & is.na(phy_ipv_12m),0, phy_ipv_12m)) 

# Create and label 'phy_ipv_r0_ever'
ipv_df <- ipv_df %>%
  mutate(phy_ipv_r0_ever = case_when(
    ipv12a_rec0 == 1 | ipv13a_rec0 == 1 | ipv14a_rec0 == 1 | ipv15a_rec0 == 1 | ipv16a_rec0 == 1 | ipv17a_rec0 == 1 ~ 1,
    ipv12a_rec0 == 0 & ipv13a_rec0 == 0 & ipv14a_rec0 == 0 & ipv15a_rec0 == 0 & ipv16a_rec0 == 0 & ipv17a_rec0 == 0 ~ 0,
    TRUE ~ NA
  )%>% structure(label = "Physical violence Index (ever - Refusal = 0)"))  # Optional label for the variable

# Create and label 'phy_ipv_r0_12m'
ipv_df <- ipv_df %>%
  mutate(phy_ipv_r0_12m = case_when(
    ipv12b_rec0 == 1 | ipv13b_rec0 == 1 | ipv14b_rec0 == 1 | ipv15b_rec0 == 1 | ipv16b_rec0 == 1 | ipv17b_rec0 == 1 ~ 1,
    ipv12b_rec0 == 0 & ipv13b_rec0 == 0 & ipv14b_rec0 == 0 & ipv15b_rec0 == 0 & ipv16b_rec0 == 0 & ipv17b_rec0 == 0 ~ 0,
    TRUE ~ NA
  )%>% structure(label = "Physical violence Index (12m - Refusal = 0)"),
  phy_ipv_r0_12m = ifelse(phy_ipv_r0_ever == 0 & is.na(phy_ipv_r0_12m),0, phy_ipv_r0_12m))   # Optional label for the variable

# Create and label 'phy_ipv_r1_ever'
ipv_df <- ipv_df %>%
  mutate(phy_ipv_r1_ever = case_when(
    ipv12a_rec1 == 1 | ipv13a_rec1 == 1 | ipv14a_rec1 == 1 | ipv15a_rec1 == 1 | ipv16a_rec1 == 1 | ipv17a_rec1 == 1 ~ 1,
    ipv12a_rec1 == 0 & ipv13a_rec1 == 0 & ipv14a_rec1 == 0 & ipv15a_rec1 == 0 & ipv16a_rec1 == 0 & ipv17a_rec1 == 0 ~ 0,
    TRUE ~ NA
  )%>% structure(label = "Physical violence Index (ever - Refusal = 1)"))  # Optional label for the variable

# Create and label 'phy_ipv_r1_12m'
ipv_df <- ipv_df %>%
  mutate(phy_ipv_r1_12m = case_when(
    ipv12b_rec1 == 1 | ipv13b_rec1 == 1 | ipv14b_rec1 == 1 | ipv15b_rec1 == 1 | ipv16b_rec1 == 1 | ipv17b_rec1 == 1 ~ 1,
    ipv12b_rec1 == 0 & ipv13b_rec1 == 0 & ipv14b_rec1 == 0 & ipv15b_rec1 == 0 & ipv16b_rec1 == 0 & ipv17b_rec1 == 0 ~ 0,
    TRUE ~ NA
  )%>% structure(label = "Physical violence Index (12m - Refusal = 1)"), # Optional label for the variable
  phy_ipv_r1_12m = ifelse(phy_ipv_r1_ever == 0 & is.na(phy_ipv_r1_12m),0, phy_ipv_r1_12m))  

# Table like in stata
for(column in c("phy_ipv_ever","phy_ipv_12m","phy_ipv_r0_ever","phy_ipv_r0_12m","phy_ipv_r1_ever","phy_ipv_r1_12m")){
  ipv_df %>%
    group_by(sample, .data[[column]]) %>% 
    summarise(count = n(), .groups = "drop") %>%
    mutate(percent = count / sum(count) * 100) %>%
    print() 
}


# Create frequency variables and set to 0 under specified conditions
ipv_df <- ipv_df %>%
  mutate(
    freq12 = ipv12c,
    freq13 = ipv13c,
    freq14 = ipv14c,
    freq15 = ipv15c,
    freq16 = ipv16c,
    freq17 = ipv17c
  ) %>%
  mutate(
    freq12 = ifelse(ipv12b == 0 | ipv12a == 0, 0, freq12),
    freq13 = ifelse(ipv13b == 0 | ipv13a == 0, 0, freq13),
    freq14 = ifelse(ipv14b == 0 | ipv14a == 0, 0, freq14),
    freq15 = ifelse(ipv15b == 0 | ipv15a == 0, 0, freq15),
    freq16 = ifelse(ipv16b == 0 | ipv16a == 0, 0, freq16),
    freq17 = ifelse(ipv17b == 0 | ipv17a == 0, 0, freq17)
  )

# Generate summary tables for each frequency variable
freq_vars <- c("freq12", "freq13", "freq14", "freq15", "freq16", "freq17")

# Calculate emotional severity index (mean across rows)
ipv_df <- ipv_df %>%
  rowwise() %>%
  mutate(
    # Calculate mean, ignoring NA values unless all values are NA
    phy_severity_12m = mean(c_across(all_of(freq_vars)), na.rm = TRUE)%>% 
      structure(label = "Physical violence Severity Index (last 12 months)")
  ) %>%
  ungroup()

# Calculate z-scores 
ipv_df <- ipv_df %>%
  mutate(phy_severity_12m_z = scale(phy_severity_12m, center = TRUE, scale = TRUE)%>% 
           structure(label = "Physical violence Severity Z-score (last 12 months)"))


# Summary statistics for non-zero values
cat("\nSummary statistics for phy_severity_12m_z > 0:\n")
ipv_df %>%
  filter(phy_severity_12m > 0) %>%
  summarize(
    count = n(),
    mean = mean(phy_severity_12m),
    median = median(phy_severity_12m),
    min = min(phy_severity_12m),
    max = max(phy_severity_12m),
    q1 = quantile(phy_severity_12m, 0.25),
    q3 = quantile(phy_severity_12m, 0.75)
  ) %>%
  print()

# Number of women with score above 0
women_above_zero <- sum(ipv_df$phy_severity_12m > 0, na.rm = TRUE)
cat("Number of women with phy_severity_12m > 0:", women_above_zero, "\n")

# Drop temporary frequency variables
ipv_df <- ipv_df %>%
  select(-all_of(freq_vars))

######################## Sexual violence #######################################

# Create and label 'sex_ipv_ever'
ipv_df <- ipv_df %>%
  mutate(sex_ipv_ever = case_when(
    ipv18a == 1 | ipv19a == 1 | ipv20a == 1 ~ 1,
    ipv18a == 0 & ipv19a == 0 & ipv20a == 0 ~ 0,
    TRUE ~ NA
  )%>% structure(label = "Sexual violence Index (ever)"))  # Optional label for the variable


# Create and label 'sex_ipv_12m'
ipv_df <- ipv_df %>%
  mutate(sex_ipv_12m = case_when(
    ipv18b == 1 | ipv19b == 1 | ipv20b == 1 ~ 1,
    ipv18b == 0 & ipv19b == 0 & ipv20b == 0 ~ 0,
    TRUE ~ NA
  )%>% structure(label = "Sexual violence Index (last 12 months)"),
  sex_ipv_12m = ifelse( sex_ipv_ever == 0 & is.na(sex_ipv_12m),0, sex_ipv_12m)) # Optional label for the variable


# Create and label 'sex_ipv_r0_ever'
ipv_df <- ipv_df %>%
  mutate(sex_ipv_r0_ever = case_when(
    ipv18a_rec0 == 1 | ipv19a_rec0 == 1 | ipv20a_rec0 == 1 ~ 1,
    ipv18a_rec0 == 0 & ipv19a_rec0 == 0 & ipv20a_rec0 == 0 ~ 0,
    TRUE ~ NA
  )%>% structure(label = "Sexual violence Index (ever)"))  # Optional label for the variable

# Create and label 'sex_ipv_r0_12m'
ipv_df <- ipv_df %>%
  mutate(sex_ipv_r0_12m = case_when(
    ipv18b_rec0 == 1 | ipv19b_rec0 == 1 | ipv20b_rec0 == 1 ~ 1,
    ipv18b_rec0 == 0 & ipv19b_rec0 == 0 & ipv20b_rec0 == 0 ~ 0,
    TRUE ~ NA
  )%>% structure(label = "Sexual violence Index (last 12 months)"),
  phy_ipv_r1_12m = ifelse(sex_ipv_r0_ever == 0 & is.na(phy_ipv_r1_12m),0,phy_ipv_r1_12m))  # Optional label for the variable

# Create and label 'sex_ipv_r1_ever'
ipv_df <- ipv_df %>%
  mutate(sex_ipv_r1_ever = case_when(
    ipv18a_rec1 == 1 | ipv19a_rec1 == 1 | ipv20a_rec1 == 1 ~ 1,
    ipv18a_rec1 == 0 & ipv19a_rec1 == 0 & ipv20a_rec1 == 0 ~ 0,
    TRUE ~ NA
  )%>% structure(label = "Sexual violence Index (ever - Refusal = 0)"))  # Optional label for the variable

# Create and label 'sex_ipv_r1_12m'
ipv_df <- ipv_df %>%
  mutate(sex_ipv_r1_12m = case_when(
    ipv18b_rec1 == 1 | ipv19b_rec1 == 1 | ipv20b_rec1 == 1 ~ 1,
    ipv18b_rec1 == 0 & ipv19b_rec1 == 0 & ipv20b_rec1 == 0 ~ 0,
    TRUE ~ NA
  )%>% structure(label = "Sexual violence Index (12m - Refusal = 1)"),
  sex_ipv_r1_12m = ifelse(sex_ipv_r1_ever == 0 & is.na(sex_ipv_r1_12m),0,sex_ipv_r1_12m))  # Optional label for the variable

# Table like in stata
for(column in c("sex_ipv_ever", "sex_ipv_12m","sex_ipv_r0_ever","sex_ipv_r0_12m","sex_ipv_r1_ever","sex_ipv_r1_12m")){
  ipv_df %>%
    group_by(sample, .data[[column]]) %>% 
    summarise(count = n(), .groups = "drop") %>%
    mutate(percent = count / sum(count) * 100) %>%
    print() 
}

# Create frequency variables and set to 0 under specified conditions
ipv_df <- ipv_df %>%
  mutate(
    freq18 = ipv18c,
    freq19 = ipv19c,
    freq20 = ipv20c
  ) %>%
  mutate(
    freq18 = ifelse(ipv18b == 0 | ipv18a == 0, 0, freq18),
    freq19 = ifelse(ipv19b == 0 | ipv19a == 0, 0, freq19),
    freq20 = ifelse(ipv20b == 0 | ipv20a == 0, 0, freq20)
  )

# Generate summary tables for each frequency variable
freq_vars <- c("freq18", "freq19", "freq20")

# Calculate emotional severity index (mean across rows)
ipv_df <- ipv_df %>%
  rowwise() %>%
  mutate(
    # Calculate mean, ignoring NA values unless all values are NA
    sex_severity_12m = mean(c_across(all_of(freq_vars)), na.rm = TRUE)%>% 
      structure(label = "Sexual violence Severity Index (last 12 months)")
  ) %>%
  ungroup()

# Calculate z-scores 
ipv_df <- ipv_df %>%
  mutate(sex_severity_12m_z = scale(sex_severity_12m, center = TRUE, scale = TRUE)%>% 
           structure(label = "Sexual violence Severity Z-score (last 12 months)"))


# Summary statistics for non-zero values
cat("\nSummary statistics for sex_severity_12m_z > 0:\n")
ipv_df %>%
  filter(sex_severity_12m > 0) %>%
  summarize(
    count = n(),
    mean = mean(sex_severity_12m),
    median = median(sex_severity_12m),
    min = min(sex_severity_12m),
    max = max(sex_severity_12m),
    q1 = quantile(sex_severity_12m, 0.25),
    q3 = quantile(sex_severity_12m, 0.75)
  ) %>%
  print()

# Number of women with score above 0
women_above_zero <- sum(ipv_df$sex_severity_12m > 0, na.rm = TRUE)
cat("Number of women with sex_severity_12m > 0:", women_above_zero, "\n")

# Drop temporary frequency variables
ipv_df <- ipv_df %>%
  select(-all_of(freq_vars))

######################## Economic coercion #######################################

# Create and label 'eco_ipv_ever'
ipv_df <- ipv_df %>%
  mutate(eco_ipv_ever = case_when(
    ipv22a == 1 | ipv23a == 1 | ipv25a == 1 ~ 1,
    ipv22a == 0 & ipv23a == 0 & ipv25a == 0 ~ 0,
    TRUE ~ NA
  )%>% structure(label = "Economic violence Index (ever)"))  # Optional label for the variable



# Create and label 'eco_ipv_12m'
ipv_df <- ipv_df %>%
  mutate(eco_ipv_12m = case_when(
    ipv22b == 1 | ipv23b == 1 | ipv25b == 1 ~ 1,
    ipv22b == 0 & ipv23b == 0 & ipv25b == 0 ~ 0,
    TRUE ~ NA
  )%>% structure(label = "Economic violence Index (last 12 months)"),
  eco_ipv_12m = ifelse(eco_ipv_ever == 0 & is.na(eco_ipv_12m),0,eco_ipv_12m))  # Optional label for the variable


# Create and label 'eco_ipv1'
ipv_df <- ipv_df %>%
  mutate(eco_ipv1 = ifelse(ipv22b == 1, 1, ifelse(ipv22b == 0 | (eco_ipv_ever == "No" & is.na(eco_ipv_12m)), 0, NA))%>% structure(label = "Prend l'argent"))

# Create and label 'eco_ipv2'
ipv_df <- ipv_df %>%
  mutate(eco_ipv2 = if_else(ipv23b == 1, 1, ifelse(ipv23b == 0 | (eco_ipv_ever == "No" & is.na(eco_ipv_12m)), 0, NA))%>% structure(label = "Refuse de donner l'argent")) 

# Create and label 'eco_ipv3'
ipv_df <- ipv_df %>%
  mutate(eco_ipv3 = if_else(ipv25b == 1, 1, if_else(ipv25b == 0 | (eco_ipv_ever == "No" & is.na(eco_ipv_12m)), 0, NA))%>% structure(label = "Oblige à donner tout l'argent")) 

# Create and label 'eco_ipv_r0_ever'
ipv_df <- ipv_df %>%
  mutate(eco_ipv_r0_ever = case_when(
    ipv22a_rec0 == 1 | ipv23a_rec0 == 1 | ipv25a_rec0 == 1 ~ 1,
    ipv22a_rec0 == 0 & ipv23a_rec0 == 0 & ipv25a_rec0 == 0 ~ 0,
    TRUE ~ NA
  )%>% structure(label = "Economic violence Index (ever - Refusal = 0)"))  # Optional label for the variable

# Create and label 'eco_ipv_r0_12m'
ipv_df <- ipv_df %>%
  mutate(eco_ipv_r0_12m = case_when(
    ipv22b_rec0 == 1 | ipv23b_rec0 == 1 | ipv25b_rec0 == 1 ~ 1,
    ipv22b_rec0 == 0 & ipv23b_rec0 == 0 & ipv25b_rec0 == 0 ~ 0,
    TRUE ~ NA
  )%>% structure(label = "Economic violence Index (12m - Refusal = 0)"))  # Optional label for the variable

# Create and label 'eco_ipv_r1_ever'
ipv_df <- ipv_df %>%
  mutate(eco_ipv_r1_ever = case_when(
    ipv22a_rec1 == 1 | ipv23a_rec1 == 1 | ipv25a_rec1 == 1 ~ 1,
    ipv22a_rec1 == 0 & ipv23a_rec1 == 0 & ipv25a_rec1 == 0 ~ 0,
    TRUE ~ NA
  )%>% structure(label = "Economic violence Index (ever - Refusal = 1)"))  # Optional label for the variable

# Create and label 'eco_ipv_r1_12m'
ipv_df <- ipv_df %>%
  mutate(eco_ipv_r1_12m = case_when(
    ipv22b_rec1 == 1 | ipv23b_rec1 == 1 | ipv25b_rec1 == 1 ~ 1,
    ipv22b_rec1 == 0 & ipv23b_rec1 == 0 & ipv25b_rec1 == 0 ~ 0,
    TRUE ~ NA
  )%>% structure(label = "Economic violence Index (12m - Refusal = 1)"),
  eco_ipv_r1_12m = ifelse(eco_ipv_r1_ever == 0 & is.na(eco_ipv_r1_12m),0,eco_ipv_r1_12m))  # Optional label for the variable

# Table like in stata
for(column in c("eco_ipv_ever","eco_ipv_12m","eco_ipv_r0_ever","eco_ipv_r0_12m","eco_ipv_r1_ever","eco_ipv_r1_12m")){
  ipv_df %>%
    group_by(sample, .data[[column]]) %>% 
    summarise(count = n(), .groups = "drop") %>%
    mutate(percent = count / sum(count) * 100) %>%
    print() 
}


# Create frequency variables and set to 0 under specified conditions
ipv_df <- ipv_df %>%
  mutate(
    freq22 = ipv22c,
    freq23 = ipv23c,
    freq25 = ipv25c
  ) %>%
  mutate(
    freq22 = ifelse(ipv22b == 0 | ipv22a == 0, 0, freq22),
    freq23 = ifelse(ipv23b == 0 | ipv23a == 0, 0, freq23),
    freq25 = ifelse(ipv25b == 0 | ipv25a == 0, 0, freq25)
  )

# Generate summary tables for each frequency variable
freq_vars <- c("freq22", "freq23", "freq25")

# Calculate emotional severity index (mean across rows)
ipv_df <- ipv_df %>%
  rowwise() %>%
  mutate(
    # Calculate mean, ignoring NA values unless all values are NA
    eco_severity_12m = mean(c_across(all_of(freq_vars)), na.rm = TRUE)%>% 
      structure(label = "Economic violence Severity Index (last 12 months)")
  ) %>%
  ungroup()

# Calculate z-scores 
ipv_df <- ipv_df %>%
  mutate(eco_severity_12m_z = scale(eco_severity_12m, center = TRUE, scale = TRUE)%>% 
           structure(label = "Economic violence Severity Z-score (last 12 months)"))


# Summary statistics for non-zero values
cat("\nSummary statistics for eco_severity_12m_z > 0:\n")
ipv_df %>%
  filter(eco_severity_12m > 0) %>%
  summarize(
    count = n(),
    mean = mean(eco_severity_12m),
    median = median(eco_severity_12m),
    min = min(eco_severity_12m),
    max = max(eco_severity_12m),
    q1 = quantile(eco_severity_12m, 0.25),
    q3 = quantile(eco_severity_12m, 0.75)
  ) %>%
  print()

# Number of women with score above 0
women_above_zero <- sum(ipv_df$eco_severity_12m > 0, na.rm = TRUE)
cat("Number of women with eco_severity_12m > 0:", women_above_zero, "\n")

# Drop temporary frequency variables
ipv_df <- ipv_df %>%
  select(-all_of(freq_vars))
######################## Any type of violence #######################################


# 1. Index for any type of violence in the last 12 months
ipv_df <- ipv_df %>%
  mutate(
    ipv_all_12m = case_when(
      emo_ipv_12m == 1 | phy_ipv_12m == 1 | sex_ipv_12m == 1 | eco_ipv_12m == 1 ~ 1,
      is.na(emo_ipv_12m) & is.na(phy_ipv_12m) & is.na(sex_ipv_12m) & is.na(eco_ipv_12m) ~ NA,
      TRUE ~ 0
    )%>% structure(label = "Suffered at least one type of violence incl. economic (last 12 months)"),
    ipv_all2_12m = case_when(
      emo_ipv_12m == 1 | phy_ipv_12m == 1 | sex_ipv_12m == 1 ~ 1,
      is.na(emo_ipv_12m) & is.na(phy_ipv_12m) & is.na(sex_ipv_12m) ~ NA,
      TRUE ~ 0
    )%>% structure(label = "Suffered at least one type of violence (last 12 months)"),
    ipv_all_r0_12m = case_when(
      emo_ipv_r0_12m == 1 | phy_ipv_r0_12m == 1 | sex_ipv_r0_12m == 1 | eco_ipv_r0_12m == 1 ~ 1,
      is.na(emo_ipv_r0_12m) & is.na(phy_ipv_r0_12m) & is.na(sex_ipv_r0_12m) & is.na(eco_ipv_r0_12m) ~ NA,
      TRUE ~ 0
    )%>% structure(label = "Suffered at least one type of violence (12m - Refusal = 0)"),
    ipv_all_r1_12m = case_when(
      emo_ipv_r1_12m == 1 | phy_ipv_r1_12m == 1 | sex_ipv_r1_12m == 1 | eco_ipv_r1_12m == 1 ~ 1,
      is.na(emo_ipv_r1_12m) & is.na(phy_ipv_r1_12m) & is.na(sex_ipv_r1_12m) & is.na(eco_ipv_r1_12m) ~ NA,
      TRUE ~ 0
    )%>% structure(label = "Suffered at least one type of violence (12m - Refusal = 1)")
  )

# 2. Reported violence (Talked to someone about it)
ipv_df <- ipv_df %>%
  mutate(
    ipv21 = ifelse(ipv21 == 2, 0, ipv21), # Recode 2 to 0
    ipv21_new = case_when(
      ipv21 == 96 ~ NA,  # Don't know
      ipv21 == 97 ~ NA,  # Refuse to answer
      TRUE ~ ipv21
    )
  )

# Generate dummy variables for `ipv21`
ipv_df <- ipv_df %>%
  mutate(
    ipv21_1 = ifelse(ipv21 == 1, 1, 0),
    ipv21_2 = ifelse(ipv21 == 0, 1, 0),
    ipv21_3 = ifelse(ipv21 == 96, 1, 0),
    ipv21_4 = ifelse(ipv21 == 97, 1, 0)
  )

# Summary tables (similar to Stata's tab1)
ipv_df %>%
  group_by(sample, ipv_all_12m) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(percent = count / sum(count) * 100) %>%
  print()

ipv_df %>%
  group_by(sample, ipv_all2_12m) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(percent = count / sum(count) * 100) %>%
  print()

ipv_df %>%
  group_by(sample, ipv21_new) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(percent = count / sum(count) * 100) %>%
  print()
