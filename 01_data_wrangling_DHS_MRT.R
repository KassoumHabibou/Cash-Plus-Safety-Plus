
################################################################################
######### DHS Mauritania 2019 - 2021############################################
################################################################################


# Data wrangling

######################## Importing library and external files ##################
### List of required packages
required_packages <- c("tidyverse", "dplyr","haven","readr", "ipumsr",
                       "fastDummies","plyr","labelled","survey","writexl")

### Check if packages are installed
missing_packages <- setdiff(required_packages, installed.packages()[,"Package"])

### Install missing packages
if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

### Load all packages
lapply(setdiff(required_packages,"plyr"), library, character.only = TRUE)

# Remove all objects
rm(list = ls())

# Define a function to recode and create new variables
recode_and_generate <- function(df, columns_list) {
  df <- df %>%
    mutate(across(all_of(columns_list), ~ ifelse(. == 2, 0, .))) %>%   # Recode 2 -> 0
    mutate(across(all_of(columns_list), ~ ifelse(. == 97, NA, .))) %>% # Recode 97 -> NA
    mutate(across(all_of(columns_list), list(rec0 = ~ replace_na(., 0)))) %>%   # recode missing as 0
    mutate(across(all_of(columns_list), list(rec1 = ~ replace_na(., 1)))) # recode missing as 1
  return(df)
}

################################################################################
#                        Note

#- Missing values in columns in the baseline are replaced by cluster level average
# value of the variable
################################################################################

######################## Loading the datasets ##################################
# Read the stata file containing the global dataframe into allrounds_MRT_hh
# Importing DHS data for comparison purpose.

dhs_df <- readRDS("input/data/MRIR71FL.rds")

# All variables for identification of hh.
var_int = c('caseid', 'v000',	'v001','v002','v003','v004','v005','v009','v010','v011','v012',
            'v020','v024','v025','v101','v106','v107','v113','v115','v131','v133','v136','v137','v138','v150',
            'v151','v152','v155', "v190", "v191", "v190a", 'v191a')

# Create a vector of IPV module variable names based on the provided list
ipv_variables <- c(
  "d005",   # Weight for Domestic Violence (6 decimals)
  "d101a",  # Partner jealous if respondent talks with other men
  "d101b",  # Partner accuses respondent of unfaithfulness
  "d101c",  # Partner does not permit respondent to meet female friends
  "d101d",  # Partner tries to limit respondent's contact with family
  "d101e",  # Partner insists on knowing where respondent is
  "d101f",  # Partner doesn't trust respondent with money
  "d101g",  # Partner threatens to respondent out from home
  "d101h",  # NA - CS control issue
  "d101i",  # NA - CS control issue
  "d101j",  # NA - CS control issue
  "d102",   # Number of control issues answered 'Yes' (D101x = 1)
  "d103a",  # Ever been humiliated by husband/partner
  "d103b",  # Ever been threatened with harm by husband/partner
  "d103c",  # Ever been insulted or made to feel bad by husband/partner
  "d103d",  # NA - Ever CS emotional abuse by husband/partner
  "d103e",  # NA - Ever CS emotional abuse by husband/partner
  "d103f",  # NA - Ever CS emotional abuse by husband/partner
  "d104",   # Experienced any emotional violence (D103x series)
  "d105a",  # Ever been pushed, shook or had something thrown by husband/partner
  "d105b",  # Ever been slapped by husband/partner
  "d105c",  # Ever been punched with fist or hit by something harmful by husband/partner
  "d105d",  # Ever been kicked or dragged by husband/partner
  "d105e",  # Ever been strangled or burnt by husband/partner
  "d105f",  # Ever been threatened with knife/gun or other weapon by husband/partner
  "d105g",  # NA - Ever CS physical violence by husband/partner
  "d105h",  # Ever been physically forced into unwanted sex by husband/partner
  "d105i",  # NA - Ever been forced into other unwanted sexual acts by husband/partner
  "d105j",  # Ever had arm twisted or hair pulled by husband/partner
  "d105k",  # Ever been physically forced to perform sexual acts respondent didn't want to
  "d105l",  # NA - Ever CS physical violence by husband/partner
  "d105m",  # NA - Ever CS physical violence by husband/partner
  "d105n",  # NA - Ever CS physical violence by husband/partner
  "d106",   # Experienced any less severe violence (D105A-C,J) by husband/partner
  "d107",   # Experienced any severe violence (D105D-F) by husband/partner
  "d108",   # Experienced any sexual violence (D105H-I,K) by husband/partner
  "d109",   # NA - Timing of first event (from D105 series) in years after marriage
  "d110a",  # Ever had bruises because of husband/partner's actions
  "d110b",  # Ever had eye injuries, sprains, dislocations or burns because of husband/partner's actions
  "d110c",  # NA - Ever went to health facility because of husband/partner's actions
  "d110d",  # Ever had wounds, broken bones, broken teeth or other serious injury because of husband/partner's actions
  "d110e",  # NA - CS because of husband/partner's actions
  "d110f",  # NA - CS because of husband/partner's actions
  "d110g",  # NA - CS because of husband/partner's actions
  "d110h",  # NA - CS because of husband/partner's actions
  "d111"    # Experienced any of listed husband/partner's actions (D110x series)
)

# Alternative methods for subsetting the variables by category:

# Control/controlling behavior variables
control_vars <- c("d101a", "d101b", "d101c", "d101d", "d101e", "d101f", "d101g", "d102")

# Emotional violence variables  
emotional_vars <- c("d103a", "d103b", "d103c", "d104")

# Physical and sexual violence variables
violence_vars <- c("d105a", "d105b", "d105c", "d105d", "d105e", "d105f", 
                   "d105h", "d105j", "d105k", "d106", "d107", "d108")


# Weight variable
weight_var <- "d005"


# Define controls variables for the regression 
control_vars = c("hhh_fem_bl", 
                 "mem_n_bl",
                 "same_cb",
                 "hhh_poly",
                 "hhh_age",
                 "age_gap",
                 "hhh_edu",
                 "hhh_lit",
                 "mem_n",
                 "hou_room_bl",
                 "hou_hea_min_bl",
                 "hou_mar_min_bl",
                 "hou_wat_min_bl")


## By professional practitioner
# dhs_df$v002 <- lbl_clean(dhs_df$v002)
# dhs_df$v005 <- lbl_clean(dhs_df$v005)
# dhs_df$v012 <- lbl_clean(dhs_df$v012)
# dhs_df$v020 <- lbl_clean(dhs_df$v020)
# dhs_df$v024 <- lbl_clean(dhs_df$v024)
# dhs_df$v002 <- lbl_clean(dhs_df$v002)
# dhs_df$v002 <- lbl_clean(dhs_df$v002)
# dhs_df$v002 <- lbl_clean(dhs_df$v002)
# dhs_df$v002 <- lbl_clean(dhs_df$v002)
# dhs_df$v002 <- lbl_clean(dhs_df$v002)
# dhs_df$v002 <- lbl_clean(dhs_df$v002)


# Convert all variables in var_vec to numeric type

dhs_df <- dhs_df  %>% 
  dplyr::mutate(across(c(var_int, ipv_variables, weight_var), lbl_clean))

dhs_df <- dhs_df %>% 
  dplyr::select(all_of(var_int), all_of(ipv_variables),  all_of(weight_var)) %>% 
  rename(
    hhid = caseid,
    hh_nbr = v002,
    wmn_smpl_wght = v005,
    resp_age = v012,
    ever_married_sample = v020,
    region = v024,
    residence_type = v025,
    region_alt = v101,
    highest_edu_level = v106,
    time_to_water = v115,
    edu_years_single = v133,
    hh_members_count = v136,
    eligible_women_count = v138,
    relation_to_head = v150,
    hhh_sex = v151,
    hhh_age = v152,
    literacy = v155,
    wealth_ndx_cmbnd = v190, 
    wealth_ndx_fctr_scr = v191,
    wealth_ndx_urbrural = v190a, 
    wealth_ndx_fctr_scr_urbrural = v191a,
    wealth_index_combined = v190,
    wealth_index_score_combined = v191,
    wealth_index_urban_rural = v190a,
    wealth_index_score_urban_rural = v191a) %>%
  
  mutate(
    hhid = hhid %>% structure(label = "Household ID"),
    hh_nbr = hh_nbr %>% structure(label = "Household number"),
    wmn_smpl_wght = wmn_smpl_wght %>% structure(label = "Women's sample weight"),
    resp_age = resp_age %>% structure(label = "Respondent's current age"),
    ever_married_sample = ever_married_sample %>% structure(label = "Ever-married sample"),
    region = region %>% structure(label = "Region"),
    region_alt = region_alt %>% structure(label = "Region (alternative variable)"),
    highest_edu_level = highest_edu_level %>% structure(label = "Highest educational level attained"),
    time_to_water = time_to_water %>% structure(label = "Time to reach water source"),
    edu_years_single = edu_years_single %>% structure(label = "Education in single years"),
    hh_members_count = hh_members_count %>% structure(label = "Number of household members"),
    relation_to_head = structure(relation_to_head, label = "Relationship to household head"),
    literacy = literacy %>% structure(label = "Literacy"),
    wealth_index_combined = wealth_index_combined %>% structure(label = "Wealth index combined"),
    wealth_index_score_combined = wealth_index_score_combined %>% structure(label = "Wealth index factor score combined (5 decimals)"),
    wealth_index_urban_rural = wealth_index_urban_rural %>% structure(label = "Wealth index for urban/rural"),
    wealth_index_score_urban_rural = wealth_index_score_urban_rural %>% structure(label = "Wealth index factor score for urban/rural (5 decimals)")
  ) 


# Additionnal cleaning
dhs_df$region <- lbl_clean(dhs_df$region)
dhs_df$region <- labelled::unlabelled(dhs_df$region)

dhs_df$residence_type <- lbl_clean(dhs_df$residence_type)
dhs_df$residence_type <- labelled::unlabelled(dhs_df$residence_type)

dhs_df <- dhs_df %>% 
  mutate(
    wealth_quint = ntile(wealth_index_combined, 5))

## Filtering
dhs_df <- dhs_df %>% 
  filter(region %in% c("assaba","guidimagha")) %>% 
  filter(residence_type =="rural") %>% 
  filter(wealth_quint <= 2 )

# Create IPV indicators based on the correct domestic violence variables
ipv_data <- dhs_df %>%
  mutate(
    # Controlling behavior (jealousy, accusations, limiting contact, insisting on knowing whereabouts)
    controlling_behavior = case_when(
      d101a == 1 | d101b == 1 | d101d == 1 | d101e == 1 ~ 1,
      d101a == 0 & d101b == 0 & d101d == 0 & d101e == 0 ~ 0,
      TRUE ~ 0
    ),
    
    
    # Emotional violence (humiliated, threatened, insulted)
    emotional_violence = case_when(
      d103a == 1 | d103b == 1 | d103c == 1 ~ 1,
      d103a == 0 & d103b == 0 & d103c == 0 ~ 0,
      TRUE ~ 0
    ),
    
    
    # Physical violence (pushed, slapped, punched, kicked, strangled, threatened with weapon, hair pulled)
    physical_violence = case_when(
      d105a == 1 | d105b == 1 | d105c == 1 | d105d == 1 | 
        d105e == 1 | d105f == 1 | d105j == 1 ~ 1,
      d105a == 0 & d105b == 0 & d105c == 0 & d105d == 0 & 
        d105e == 0 & d105f == 0 & d105j == 0 ~ 0,
      TRUE ~ 0
    ),
    
    # Sexual violence (forced sex, forced sexual acts)
    sexual_violence = case_when(
      d105h == 1 | d105k == 1 ~ 1,
      d105h == 0 & d105k == 0 ~ 0,
      TRUE ~ 0
    ),
    
    sexual_or_physical_violence = case_when(
      d105h == 1 | d105k == 1 | d105a == 1 | d105b == 1 | d105c == 1 | d105d == 1 | 
        d105e == 1 | d105f == 1 | d105j == 1 ~ 1,
      d105h == 0 & d105k == 0 & d105a == 0 & d105b == 0 & d105c == 0 & d105d == 0 & 
        d105e == 0 & d105f == 0 & d105j == 0 ~ 0,
      TRUE ~ 0
    )
  ) %>% 
  mutate(
    any_violence = case_when(
      emotional_violence == 1 | physical_violence == 1 | sexual_violence == 1  ~ 1,
      emotional_violence == 0 & physical_violence == 0 & sexual_violence == 0 ~ 0,
      TRUE ~ 0
    )
    
  ) %>% filter(!is.na(d005)) # Remove observations with Na in weight

# Set up survey design with appropriate weights
survey_design <- svydesign(
  ids = ~1,
  weights = ~d005,  # domestic violence weight (d005)
  data = ipv_data
)

# Calculate IPV rates by type
ipv_rates <- data.frame(
  IPV_Type = c("Controlling Behavior", "Emotional Violence", "Physical Violence", 
               "Sexual Violence", "Sexual or Physical Violence", "Any type of violence"),
  Rate = c(
    svymean(~controlling_behavior, survey_design, na.rm = TRUE)[1],
    svymean(~emotional_violence, survey_design, na.rm = TRUE)[1],
    svymean(~physical_violence, survey_design, na.rm = TRUE)[1],
    svymean(~sexual_violence, survey_design, na.rm = TRUE)[1],
    svymean(~sexual_or_physical_violence, survey_design, na.rm = TRUE)[1],
    svymean(~any_violence, survey_design, na.rm = TRUE)[1]
  ),
  Standard_Error = c(
    SE(svymean(~controlling_behavior, survey_design, na.rm = TRUE)),
    SE(svymean(~emotional_violence, survey_design, na.rm = TRUE)),
    SE(svymean(~physical_violence, survey_design, na.rm = TRUE)),
    SE(svymean(~sexual_violence, survey_design, na.rm = TRUE)),
    SE(svymean(~sexual_or_physical_violence, survey_design, na.rm = TRUE)),
    SE(svymean(~any_violence, survey_design, na.rm = TRUE))
  )
)

# Display results
print("IPV Rates among Poorest Men in Guidimakha and Assaba Regions:")
print(ipv_rates)


# Define output file name
file_name <- paste0("output/ipv_rates_DHS_estimates.xlsx")

# Write the table to Excel
write_xlsx(table, path = file_name)


write_rds(dhs_df, "output/data/ipv_rates_DHS_estimates.rds")

# Remove all objects
rm(list = ls())