################################################################################
######### IPV in Mauritania ####################################################
################################################################################

# Data wrangling

######################## Importing library and external files ##################
### List of required packages
required_packages <- c("tidyverse", "dplyr","haven","readr","fastDummies","plyr",
                       "hms","lubridate","tidyr","labelled","stringr","FactoMineR")

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


################################################################################
#                        Note

#- Missing values in columns in the baseline are replaced by cluster level average
# value of the variable
################################################################################

######################## Loading the datasets ##################################
# Read the stata file containing the global dataframe into allrounds_MRT_hh
allrounds_MRT_hh <-  read_dta("input/data/allrounds_MRT_hh.dta")
baseline_MRT_hh <- read_dta("input/data/baseline_MRT_hh.dta")
members_fl <- read_dta("input/data/members_long_fl.dta")
members_bl <- read_dta("input/data/members_long_bl.dta")
roster <- read_dta("input/data/roster.dta")
nonfood <- read_dta("input/data/nonfood.dta")

## Importing the geospatial coordinates
coordinates_data <-  read_dta("input/coordinates/coordinates.dta")

## Data on IPV
ipv_df_all <- read_dta("output/data/ipv_index_all.dta")

# Load ACV/Germe wide file
acv_germe_df <- read_dta("input/data/administratif_data/data_Final/ACVGerme_Final.dta")

# Film screening data
fiche_film_df <- haven::read_dta("input/data/administratif_data/data_Final/Fiche_Film_Final.dta")

######################## Cleaning the data #####################################
# Selecting the columns
## Phase column contains information on the survey rounds
## The balance table was performed with the baseline data
## the line follow up in the balance table is the table for attrition (which is tiny regarding the size)
## to be consider as a household of interest, the recipient household member should be age between 18 and 49 (pben_age_bl >= 18 & pben_age_bl <= 49) during the program inplementation

#                   select(hhid,treatment,treat_dum,strata,cluster,p_region,p_commune,p_village,region,commune,
#                          village,phase, consent, reg_hh_mrt, soc_prmtn,cash_trsfr,same_cb,benef_pos,chef_pos,
#                          hhh_fem,pben_fem,hhh_age,pben_age,hhh_poly,hhh_handicap,
#                          pben_poly,pben_handicap,pben_relation,equiv_n,mem_n,adult_n,
#                          adult_fem_s,mem_n,equiv_n,depend_ratio,extend_ratio,baby_n,
#                          earlychild_n,olderchild_n, adult_fem_s, hhh_fem, sleep_days_chef,
#                          sleep_days_ben, earlychild_n, olderchild_n, workage1_n, workage2_n, workage3_n, 
#                          workage4_n, workage5_n, elder_n, workageo_n, workageo_mal_n, 
#                          workageo_fem_n, earlychild_s, olderchild_s, workage1_s, 
#                          workage2_s, workage3_s, workage4_s, workage5_s, elder_s, 
#                          workageo_s, workageo_mal_s, workageo_fem_s,hhh_edu_bl, 
#                          pben_edu_bl,pben_prim_bl,hhh_lit_bl,pben_lit_bl,mem_n, mes_coach_d, 
#                          mes_avec_d, mes_germe_d, mes_video_d, mes_acv_d, mes_bourse_d,
#                          contains("got"), ends_with("_bl"), ctrans_d, hhh_fem_bl, mem_n_bl, 
#                          save_share_d, # Share of saving own by partner
#                          pben_handicap_bl, pben_poly_bl, hhh_fem_bl, pben_edu_bl,
#                          hhh_age_bl, pben_age_bl,
#                          pben_relation, # Economic beneficiary relationship to household head
#                          ctrl_earn_index_tr_bl, #B.10.5 Control Earnings Z-index at bl, 
#                          ctrl_hh_index_tr_bl, # B.10.6 Control HH Z-index at bl
#                          intrahh_vars_index_tr_bl, # C.2.3.A Intra-HH Dynamics Z-index at Baseline
#                          rev_sum_bohh_wemp_98_tr_bl, # beneficiary share of household total rev 98 wins baseline
#                          same_cb,pben_fem,hhh_poly,hhh_prim_bl,pben_prim_bl,
#                          mem_n,hou_room_bl,hou_hea_min_bl,hou_mar_min_bl,hou_wat_min_bl
#                          # dom_relation_index_tr_bl, not baseline level data collected
#                          # gender_attitudes_index_tr_bl  not baseline level data collected
#                          ) %>% 
# members2 <- members2 %>% 
#   select(hhid, spouse_in, spouses, hh_pos2) 


allrounds_MRT_hh <- allrounds_MRT_hh %>% 
  rename(treatment_pi = treatment,
          treat_dum_pi = treat_dum,
          reg_hh_pi_mrt = reg_hh_mrt,
          mes_ctrans_d = ctrans_d) %>% 
  mutate(treatment_pi = treatment_pi %>%
            structure(label = "PI IE treatment group"),
         reg_hh_pi_mrt = reg_hh_pi_mrt %>%
           structure(label = "Filtering condition for PI IE")
         )



# Subselecting the variables needed in the baseline
baseline_MRT_hh <- baseline_MRT_hh %>% 
  select(hhid,treatment,treat_dum,strata,cluster,p_region,p_commune,p_village,region,commune,
         village,reg_hh_mrt,consent, soc_prmtn,cash_trsfr,same_cb,benef_pos,chef_pos,
         hhh_fem,pben_fem,hhh_age,pben_age,hhh_poly,hhh_handicap,
         pben_poly,pben_handicap,pben_relation,equiv_n,mem_n,adult_n,
         adult_fem_s,mem_n,equiv_n,depend_ratio,extend_ratio,baby_n,
         earlychild_n,olderchild_n, adult_fem_s, hhh_fem, sleep_days_chef,
         sleep_days_ben, earlychild_n, olderchild_n, workage1_n, workage2_n, workage3_n, 
         workage4_n, workage5_n, elder_n, workageo_n, workageo_mal_n, 
         workageo_fem_n, earlychild_s, olderchild_s, workage1_s, 
         pben_relation, # Economic beneficiary relationship to household head
         workage2_s, workage3_s, workage4_s, workage5_s, elder_s, 
         workageo_s, workageo_mal_s, workageo_fem_s,hhh_edu,
         pben_edu,hhh_prim,pben_prim,hhh_lit,pben_lit,
         mem_n,hou_room,hou_hea_min,hou_mar_min,hou_wat_min,
         hhh_edu, pben_edu, hhh_prim, pben_prim, hhh_lit, ctrans_d
         )%>% 
  rename(treatment_pi = treatment,
         treat_dum_pi = treat_dum,
         reg_hh_pi_mrt = reg_hh_mrt,
         mes_ctrans_d = ctrans_d) 

# Adding information on the presence of husband in the household age, sex,

members_bl <- members_bl %>% 
  filter(is_benef==1) %>% 
  select(hhid, spouse_in, spouses) %>% 
  rename(spouse_in_bl = spouse_in,
         spouses_bl = spouses
  )

members_fl <- members_fl %>% 
  filter(is_benef==1) %>% 
  select(hhid, spouse_in, spouses) %>% 
  rename(spouse_in_fl = spouse_in,
         spouses_fl = spouses
  )

# Merging to get beneficiary information on hh presence
allrounds_MRT_hh <- allrounds_MRT_hh %>% 
  left_join(members_bl, by=c("hhid" = "hhid")) %>% 
  left_join(members_fl, by=c("hhid" = "hhid"))



#Merging to get the indexes of the baseline
baseline_MRT_hh <- baseline_MRT_hh %>% 
  left_join(allrounds_MRT_hh %>% 
              filter(phase==2) %>% 
              select(hhid,
                     ctrl_earn_index_tr_bl, #B.10.5 Control Earnings Z-index at bl, 
                     ctrl_hh_index_tr_bl, # B.10.6 Control HH Z-index at bl
                     intrahh_vars_index_tr_bl, # C.2.3.A Intra-HH Dynamics Z-index at Baseline
                     rev_sum_bohh_wemp_98_tr_bl # beneficiary share of household total rev 98 wins baseline
                     ), by = "hhid")

# Include hh lat and long : ,hhh_health_index,pben_health_index
## D:\Dropbox\Sahel_analysis\Data\MRT\Baseline\03_Intermediate_anon\coordinates.dta
## https://cran.r-project.org/web/packages/RCT/RCT.pdf
## https://ehsanx.github.io/psw/balance.html


## Selecting the UNIT level data
coordinates_data <- coordinates_data %>% 
  select(hhid, ru_lat_bl_tr, ru_lon_bl_tr)

## Merging the coordinates and hh level with baseline
baseline_MRT_hh <- baseline_MRT_hh %>% 
  left_join(coordinates_data, by="hhid")

allrounds_MRT_hh <- allrounds_MRT_hh %>% 
  left_join(coordinates_data, by="hhid")

## Creating treatment dummy variables for cash transfer analysis

### For the cash transfer
### for those that should be considered in the cash_transfer regression, either they
### are recipients of the cash transfer and they are assigned to the control group for the pi
### or they are assigned to the control group for the cash transfer
### total of 1502 for the cash transfer and the remaining are excluded as they are assigned to the pi

# Create indicator for households to be included in cash transfer regression analysis
allrounds_MRT_hh <- allrounds_MRT_hh %>% 
  mutate(reg_hh_csh_mrt = 
           case_when(
             cash_trsfr==0 & treatment_pi==0 & pben_age_bl <= 50 & pben_age_bl >= 18~ 1,  # Control group for cash transfer
             cash_trsfr==1 & treatment_pi==0 & pben_age_bl <= 50 & pben_age_bl >= 18~ 1,  # Cash transfer recipients in pi control group
             .default = 0                       # All other cases
           ) %>%
           structure(label = "Filtering condition for Tekavoul IE"))



# Create treatment indicator for cash transfer (1 = received transfer, 0 = control)
allrounds_MRT_hh <- allrounds_MRT_hh %>% 
  mutate(treatment_csh_trnsfr = 
           ifelse(reg_hh_csh_mrt==1 & cash_trsfr==1, 1,     # Treatment group
                  ifelse(reg_hh_csh_mrt==1 & cash_trsfr==0, 0, NA)),  # Control group, NA for excluded,
         treatment_csh_trnsfr = factor(treatment_csh_trnsfr, levels = c(0,1),
                                        labels = c("Control","Cash Assignment")) %>%
           structure(label = "Tekavoul program IE treatment group"))

# Create indicator for households to be included in cash transfer regression analysis
baseline_MRT_hh <- baseline_MRT_hh %>% 
  mutate(reg_hh_csh_mrt = 
           case_when(
             cash_trsfr==0 & treatment_pi==0 & pben_age <= 50 & pben_age >= 18~ 1,  # Control group for cash transfer
             cash_trsfr==1 & treatment_pi==0 & pben_age <= 50 & pben_age >= 18~ 1,  # Cash transfer recipients in pi control group
             .default = 0                       # All other cases
           ) %>%
           structure(label = "Filtering condition for Tekavoul IE"))



# Create treatment indicator for cash transfer (1 = received transfer, 0 = control)
baseline_MRT_hh <- baseline_MRT_hh %>% 
  mutate(treatment_csh_trnsfr = 
           ifelse(reg_hh_csh_mrt==1 & cash_trsfr==1, 1,     # Treatment group
                  ifelse(reg_hh_csh_mrt==1 & cash_trsfr==0, 0, NA)),  # Control group, NA for excluded,
         treatment_csh_trnsfr = factor(treatment_csh_trnsfr, levels = c(0,1),
                                       labels = c("Control","Cash Assignment")) %>%
           structure(label = "Tekavoul program IE treatment group"))

########################### Spill over effects #################################
# Create regression indicator for baseline households
allrounds_MRT_hh <- allrounds_MRT_hh %>% 
  mutate(reg_hh_pi_spl_mrt = 
           case_when(
             cash_trsfr==0 ~ 1,  # Don't receive the CT
             .default = 0                       # All other cases
           )%>%
           structure(label = "Filtering condition for spillover effect evaluation"))

# Create treatment indicator for baseline households
allrounds_MRT_hh <- allrounds_MRT_hh %>% 
  mutate(treatment_pi_spill= 
           case_when(
             reg_hh_pi_spl_mrt==1 & treatment_pi==0 & pben_age_bl <= 50 & pben_age_bl >= 18 ~ 0,  # Control group for spill
             reg_hh_pi_spl_mrt==1 & treatment_pi== 1 & pben_age_bl <= 50 & pben_age_bl >= 18 ~ 1,  
             reg_hh_pi_spl_mrt==1 & treatment_pi== 2 & pben_age_bl <= 50 & pben_age_bl >= 18 ~ 2, 
             reg_hh_pi_spl_mrt==1 & treatment_pi== 3 & pben_age_bl <= 50 & pben_age_bl >= 18 ~ 3,               
             .default = NA                       # All other cases
           )) %>% 
  mutate(
    treatment_pi_spill = factor(treatment_pi_spill, levels = c(0,1,2,3),
                                  labels = c("Control","Capital","Psychosocial","Full")) %>%
      structure(label = "Spillover effect impact evaluation"))

### For the pi
allrounds_MRT_hh <- allrounds_MRT_hh %>% 
  mutate(treatment_pi_1 = ifelse(treatment_pi==0,1,ifelse(treatment_pi==1,0,NA)),
         treatment_pi_2 = ifelse(treatment_pi==0,1,ifelse(treatment_pi==2,0,NA)),
         treatment_pi_3 = ifelse(treatment_pi==0,1,ifelse(treatment_pi==3,0,NA)),
         treatment_pi_4 = ifelse(treatment_pi==1,1,ifelse(treatment_pi==2,0,NA)),
         treatment_pi_5 = ifelse(treatment_pi==1,1,ifelse(treatment_pi==3,0,NA)),
         treatment_pi_6 = ifelse(treatment_pi==2,1,ifelse(treatment_pi==3,0,NA)),
         treatment_pi_pool = ifelse(treatment_pi==0,0,1))

baseline_MRT_hh <- baseline_MRT_hh %>% 
  mutate(treatment_pi_1 = ifelse(treatment_pi==0,1,ifelse(treatment_pi==1,0,NA)),
         treatment_pi_2 = ifelse(treatment_pi==0,1,ifelse(treatment_pi==2,0,NA)),
         treatment_pi_3 = ifelse(treatment_pi==0,1,ifelse(treatment_pi==3,0,NA)),
         treatment_pi_4 = ifelse(treatment_pi==1,1,ifelse(treatment_pi==2,0,NA)),
         treatment_pi_5 = ifelse(treatment_pi==1,1,ifelse(treatment_pi==3,0,NA)),
         treatment_pi_6 = ifelse(treatment_pi==2,1,ifelse(treatment_pi==3,0,NA)),
         treatment_pi_pool = ifelse(treatment_pi==0,0,1))

#### For the pool Economic inclusion program

allrounds_MRT_hh <- allrounds_MRT_hh %>% 
  mutate(treatment_pi_pool = factor(treatment_pi_pool, levels = c(0,1),
                                    labels = c("Control","Pool")))

baseline_MRT_hh <- baseline_MRT_hh %>% 
  mutate(treatment_pi_pool = factor(treatment_pi_pool, levels = c(0,1),
                                    labels = c("Control","Pool")))

################################################################################
######################## Cleaning the IPV data #################################
################################################################################
 
# get proportions (41 percent = 1755 of the total sample of women completed the IPV module)
ipv_df_all %>% 
  filter(!is.na(ipv_completed)) %>%  
  mutate(ipv_completed = as.factor(ipv_completed)) %>% 
  count(ipv_completed) %>%
  mutate(proportion = n / sum(n))

# Other specify in the IPV all

ipv_df_all <- ipv_df_all %>% 
  mutate(status_noconsent = as.character(status_noconsent),
         status_noconsent_o = as.character(status_noconsent_o), 
         status_explain = as.character(status_explain)
  ) 


# Your reclassify_status function
reclassify_status <- function(status_noconsent_o, status_explain) {
  
  # Combine both text fields for more comprehensive matching
  combined_text <- tolower(paste(
    ifelse(is.na(status_noconsent_o), "", status_noconsent_o),
    ifelse(is.na(status_explain), "", status_explain),
    sep = " "
  ))
  
  # Category 6: Age over 50 years
  if (str_detect(combined_text, "âgée? de plus de 50 ans|agée? de plus de 50 ans|très vieille|âge dépasser")) {
    return("6")
  }
  
  # Check for specific ages mentioned (51-99 years)
  age_match <- str_extract(combined_text, "\\d{2,}\\s*ans")
  if (!is.na(age_match)) {
    age <- as.numeric(str_extract(age_match, "\\d+"))
    if (!is.na(age) && age > 50) {
      return("6")
    }
  }
  
  # Category 5: Widow (veuve)
  if (str_detect(combined_text, "\\bveuve\\b")) {
    return("5")
  }
  
  # Category 7: Deceased
  if (str_detect(combined_text, "décédée?|décè[sd]|décès")) {
    return("7")
  }
  
  # Category 4: Not living in household / Absent / Away
  if (str_detect(combined_text, "en voyage|absente?|déplacement|rendez[-\\s]vous|n'habite pas|mariée? dans un autre")) {
    return("4")
  }
  
  # Category 3: Is not available
  if (str_detect(combined_text, "malade|muette|suppléante|mit")) {
    return("3")
  }
  
  # Category 8: divorced or separated
  if (str_detect(combined_text, "divorcée?|séparée?")) {
    return("8")
  }
  
  # Category 9: single
  if (str_detect(combined_text, "célibataire|jamais mariée?|jamais mari[èe]e|n'a jamais été mariée|ne s'est jamais mariée")) {
    return("9")
  }
  
  # Category 2: Refusal (if explicitly mentioned)
  if (str_detect(combined_text, "refus")) {
    return("2")
  }
  
  # Category 1: Household issues
  if (str_detect(combined_text, "ménage.*vide|doublon")) {
    return("1")
  }
  
  # If no clear match, keep as "Autre"
  return("-777")
}

# Apply reclassification to the dataset
# This code reclassifies the "Autre (préciser)" category (-777) in the 
# status_noconsent variable into more specific categories based on the 
# text descriptions in status_noconsent_o and status_explain columns.

# Reclassify status_noconsent from -777 to specific categories
ipv_df_all <- ipv_df_all %>%
  mutate(
    status_noconsent_new = case_when(
      # Only reclassify if current value is -777 (Autre/Other)
      status_noconsent == "-777" ~ mapply(
        reclassify_status,  # Custom function that analyzes text descriptions
        status_noconsent_o, 
        status_explain
      ),
      # Keep original value for all non-Autre cases
      TRUE ~ status_noconsent
    )
  ) %>% 
  # Clear text fields for successfully reclassified cases
  # (Keep text only for cases that remain as "Other")
  mutate(
    status_noconsent_o = ifelse(status_noconsent_new != "-777", "", status_noconsent_o),
    status_explain = ifelse(status_noconsent_new != "-777", "", status_explain)
  )

# Add English labels for the reclassified variable
ipv_df_all <- ipv_df_all %>%
  mutate(
    status_noconsent_new_label = case_when(
      status_noconsent_new == "-777" ~ "Other",
      status_noconsent_new == "1" ~ "Household not found",
      status_noconsent_new == "2" ~ "Household refusal",
      status_noconsent_new == "3" ~ "Benef. not available during entire survey period (sick)",
      status_noconsent_new == "4" ~ "Benef. does not live in the household (travelling)",
      status_noconsent_new == "5" ~ "Benef. is a widow",
      status_noconsent_new == "6" ~ "Benef. is over 50 years old",
      status_noconsent_new == "7" ~ "Benef. is deceased",
      status_noconsent_new == "8" ~ "Benef. is divorced/separated",
      status_noconsent_new == "9" ~ "Benef. is single/never married",
      TRUE ~ "Benef. participate"
    )
  )

  
# Selecting the intimate partner violence (IPV) variables of interest
ipv_df_consent <- ipv_df_all %>% 
  # Remove any rows where the IPV completion status is missing
  filter(!is.na(ipv_completed)) %>% 
  # Keep only rows where the IPV survey was completed (value equals 1)
  filter(ipv_completed==1) %>% 
  # Perform a left join with household data from the MRT follow-up survey
  left_join(allrounds_MRT_hh %>% 
              # Phase 2
              filter(phase==2) %>%  
              # Select only the household ID and regional household program indicators
              select(hhid, reg_hh_pi_mrt, reg_hh_csh_mrt, treatment_csh_trnsfr, treatment_pi_pool, 
                     married_ben_bl, workageo_mal_n_bl, married_ben, workageo_mal_n, elder_n_bl,
                     elder_n, spouse_in_bl, spouses_bl, spouse_in_fl, spouses_fl, treatment_pi_spill) %>% 
              # Convert household ID to character type to ensure consistent joining
              mutate(hhid=as.character(hhid)), 
            # Specify that joining should be done using household ID as the key
            by="hhid") %>% 
  # Filtering to keep the individual of interest
  filter(reg_hh_pi_mrt==1 | reg_hh_csh_mrt==1)

lst_hh = ipv_df_consent %>% select(hhid) %>% pull()


# 73.2 percent = 869 of women are currently leaving with their partner at followup
ipv_df_consent %>% 
  filter(!is.na(spouse_in_fl)) %>%  
  mutate(spouse_in_fl = as.factor(spouse_in_fl)) %>% 
  count(spouse_in_fl) %>%
  mutate(proportion = n / sum(n))

ipv_df_consent <- ipv_df_consent %>% 
  # Keep only married women leaving with their husband at followup
  filter(spouse_in_fl==1) #spouse_in_bl==1 &  & 

# For the cash
ipv_df_consent_cash <- ipv_df_consent %>% 
  filter(reg_hh_csh_mrt==1)

# For the PI
ipv_df_consent_pi <- ipv_df_consent %>% 
  filter(reg_hh_pi_mrt==1)

################################################################################
######################## Controlling behavior ##################################
################################################################################

# Define the variables to include in the IPV severity index
scale_vars = c("ipv1", "ipv2", "ipv3", "ipv5", "ipv6", "ipv7")

# Set the name of the new index variable to be created
curr_ipv = "control_ipv_sev_index"
lab_curr_ipv = "Controlling behavior severity index"

curr_ipv_m0 = "control_ipv_sev_index_m0"
lab_curr_ipv_m0 = "Controlling behavior severity index (missing replaced by 0)"

curr_ipv_m1 = "control_ipv_sev_index_m1"
lab_curr_ipv_m1 = "Controlling behavior severity index (missing replaced by 1)"


######################### Tekavoul (Cash Transfer) ####################################

####### No missing values imputed
# Create the IPV severity index as the mean of selected items, handling missing values
ipv_df_consent_cash <- ipv_df_consent_cash %>%
  # Compute the row-wise mean of the selected IPV variables
  mutate(!!sym(curr_ipv) := apply(select(., scale_vars), 1, sum, na.rm = TRUE)) %>%
  mutate(!!sym(curr_ipv) := !!sym(curr_ipv)/length(scale_vars)) %>%
  # Replace values with NA if all selected variables are NA for a given row
  mutate(!!sym(curr_ipv) := ifelse(rowSums(!is.na(select(., all_of(scale_vars)))) == 0, NA, !!sym(curr_ipv)))

# Transfort to NA the index values that are missing in the regular indicator
ipv_df_consent_cash <- ipv_df_consent_cash %>%
  mutate(!!sym(curr_ipv) := ifelse(is.na(!!sym(paste0(substr(curr_ipv, 1, nchar(curr_ipv) - 9), "12m"))),NA, !!sym(curr_ipv))) 

# Calculate the global mean of the index among the control groups
mean_glob = mean(ipv_df_consent_cash %>% 
                   filter(treatment_csh_trnsfr == "Control") %>% 
                   select(!!sym(curr_ipv)) %>% 
                   pull(), na.rm = TRUE)

# Calculate the global standard deviation of the index among the control groups
std_glob = sd(ipv_df_consent_cash %>% 
                filter(treatment_csh_trnsfr == "Control") %>% 
                select(!!sym(curr_ipv)) %>% 
                pull(), na.rm = TRUE)

# Standardize the IPV index using the global mean and standard deviation, and add a label
ipv_df_consent_cash <- ipv_df_consent_cash %>%
  mutate(!!sym(curr_ipv) := ifelse(is.na(!!sym(curr_ipv)),NA, ((!!sym(curr_ipv) - mean_glob) / std_glob)) %>%
           structure(label = lab_curr_ipv)) %>%
  # Rename an existing variable (to make sure the source name is correct)
  rename(control_ipv_sev_12m = control_ipv_12m) %>% 
  mutate(control_ipv_sev_12m_m0 = ifelse(is.na(control_ipv_sev_12m),0,control_ipv_sev_12m),
         control_ipv_sev_12m_m1 = ifelse(is.na(control_ipv_sev_12m),1,control_ipv_sev_12m)
         )


####### Missing values imputed replaced by 0 ####################################

# Create the IPV severity index as the mean of selected items, handling missing values
ipv_df_consent_cash <- ipv_df_consent_cash %>%
  # Compute the row-wise mean of the selected IPV variables
  mutate(!!sym(curr_ipv_m0) := apply(select(., scale_vars), 1, function(x) {
    x[is.na(x)] <- 0  # Replace NA with 0
    sum(x)             # Compute sum
  })) %>%
  mutate(!!sym(curr_ipv_m0) := !!sym(curr_ipv_m0)/length(scale_vars)) %>%
  # Replace values with NA if all selected variables are NA for a given row
  mutate(!!sym(curr_ipv_m0) := ifelse(rowSums(!is.na(select(., all_of(scale_vars)))) == 0, NA, !!sym(curr_ipv_m0)))

# Transfort to NA the index values that are missing in the regular indicator
ipv_df_consent_cash <- ipv_df_consent_cash %>%
  mutate(!!sym(curr_ipv_m0) := ifelse(is.na(!!sym(curr_ipv)),NA, !!sym(curr_ipv_m0))) 

# Calculate the global mean of the index among the control groups
mean_glob_m0 = mean(ipv_df_consent_cash %>% 
                   filter(treatment_csh_trnsfr == "Control") %>% 
                   select(!!sym(curr_ipv_m0)) %>% 
                   pull(), na.rm = TRUE)

# Calculate the global standard deviation of the index among the control groups
std_glob_m0 = sd(ipv_df_consent_cash %>% 
                filter(treatment_csh_trnsfr == "Control") %>% 
                select(!!sym(curr_ipv_m0)) %>% 
                pull(), na.rm = TRUE)

# Standardize the IPV index using the global mean and standard deviation, and add a label
ipv_df_consent_cash <- ipv_df_consent_cash %>%
  mutate(!!sym(curr_ipv_m0) := ifelse(is.na(!!sym(curr_ipv_m0)),NA, ((!!sym(curr_ipv_m0) - mean_glob_m0) / std_glob_m0)) %>%
           structure(label = lab_curr_ipv_m0)) 

####### Missing values imputed replaced by 1 ####################################

# Create the IPV severity index as the mean of selected items, handling missing values
ipv_df_consent_cash <- ipv_df_consent_cash %>%
  # Compute the row-wise mean of the selected IPV variables
  mutate(!!sym(curr_ipv_m1) := apply(select(., scale_vars), 1, function(x) {
    x[is.na(x)] <- 1  # Replace NA with 0
    sum(x)             # Compute sum
  })) %>%
  mutate(!!sym(curr_ipv_m1) := !!sym(curr_ipv_m1)/length(scale_vars)) %>%
  # Replace values with NA if all selected variables are NA for a given row
  mutate(!!sym(curr_ipv_m1) := ifelse(rowSums(!is.na(select(., all_of(scale_vars)))) == 0, NA, !!sym(curr_ipv_m1)))

# Transfort to NA the index values that are missing in the regular indicator
ipv_df_consent_cash <- ipv_df_consent_cash %>%
  mutate(!!sym(curr_ipv_m1) := ifelse(is.na(!!sym(curr_ipv)),NA, !!sym(curr_ipv_m1))) 

# Calculate the global mean of the index among the control groups
mean_glob_m1 = mean(ipv_df_consent_cash %>% 
                      filter(treatment_csh_trnsfr == "Control") %>% 
                      select(!!sym(curr_ipv_m1)) %>% 
                      pull(), na.rm = TRUE)

# Calculate the global standard deviation of the index among the control groups
std_glob_m1 = sd(ipv_df_consent_cash %>% 
                   filter(treatment_csh_trnsfr == "Control") %>% 
                   select(!!sym(curr_ipv_m1)) %>% 
                   pull(), na.rm = TRUE)

# Standardize the IPV index using the global mean and standard deviation, and add a label
ipv_df_consent_cash <- ipv_df_consent_cash %>%
  mutate(!!sym(curr_ipv_m1) := ifelse(is.na(!!sym(curr_ipv_m1)),NA, ((!!sym(curr_ipv_m1) - mean_glob_m1) / std_glob_m1)) %>%
           structure(label = lab_curr_ipv_m1)) 

# Comparison table
ipv_df_consent_cash %>%
  select(!!sym(curr_ipv), !!sym(curr_ipv_m0), !!sym(curr_ipv_m1)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  group_by(variable) %>%
  summarise(
    n = n(),
    mean = mean(value, na.rm = TRUE),
    median = median(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    min = min(value, na.rm = TRUE),
    max = max(value, na.rm = TRUE),
    q25 = quantile(value, 0.25, na.rm = TRUE),
    q75 = quantile(value, 0.75, na.rm = TRUE)
  )

######################### Economic Inclusion ####################################
# Create the IPV severity index as the mean of selected items, handling missing values
ipv_df_consent_pi <- ipv_df_consent_pi %>%
  # Compute the row-wise mean of the selected IPV variables
  mutate(!!sym(curr_ipv) := apply(select(., scale_vars), 1, sum, na.rm = TRUE)) %>%
  mutate(!!sym(curr_ipv) := !!sym(curr_ipv)/length(scale_vars)) %>%
  # Replace values with NA if all selected variables are NA for a given row
  mutate(!!sym(curr_ipv) := ifelse(rowSums(!is.na(select(., all_of(scale_vars)))) == 0, NA, !!sym(curr_ipv)))

# Transfort to NA the index values that are missing in the regular indicator
ipv_df_consent_pi <- ipv_df_consent_pi %>%
  mutate(!!sym(curr_ipv) := ifelse(is.na(!!sym(paste0(substr(curr_ipv, 1, nchar(curr_ipv) - 9), "12m"))),NA, !!sym(curr_ipv))) 

# Calculate the global mean of the index among the control groups
mean_glob = mean(ipv_df_consent_pi %>% 
                   filter(treatment_pi_pool == "Control") %>% 
                   select(!!sym(curr_ipv)) %>% 
                   pull(), na.rm = TRUE)

# Calculate the global standard deviation of the index among the control groups
std_glob = sd(ipv_df_consent_pi %>% 
                filter(treatment_pi_pool == "Control") %>% 
                select(!!sym(curr_ipv)) %>% 
                pull(), na.rm = TRUE)

# Standardize the IPV index using the global mean and standard deviation, and add a label
ipv_df_consent_pi <- ipv_df_consent_pi %>%
  mutate(!!sym(curr_ipv) := ifelse(is.na(!!sym(curr_ipv)),NA, ((!!sym(curr_ipv) - mean_glob) / std_glob)) %>%
           structure(label = lab_curr_ipv)) %>%
  # Rename an existing variable (to make sure the source name is correct)
  rename(control_ipv_sev_12m = control_ipv_12m) %>% 
  mutate(control_ipv_sev_12m_m0 = ifelse(is.na(control_ipv_sev_12m),0,control_ipv_sev_12m),
         control_ipv_sev_12m_m1 = ifelse(is.na(control_ipv_sev_12m),1,control_ipv_sev_12m)
  )

####### Missing values imputed replaced by 0 ####################################

# Create the IPV severity index as the mean of selected items, handling missing values
ipv_df_consent_pi <- ipv_df_consent_pi %>%
  # Compute the row-wise mean of the selected IPV variables
  mutate(!!sym(curr_ipv_m0) := apply(select(., scale_vars), 1, function(x) {
    x[is.na(x)] <- 0  # Replace NA with 0
    sum(x)             # Compute sum
  })) %>%
  mutate(!!sym(curr_ipv_m0) := !!sym(curr_ipv_m0)/length(scale_vars)) %>%
  # Replace values with NA if all selected variables are NA for a given row
  mutate(!!sym(curr_ipv_m0) := ifelse(rowSums(!is.na(select(., all_of(scale_vars)))) == 0, NA, !!sym(curr_ipv_m0)))

# Transfort to NA the index values that are missing in the regular indicator
ipv_df_consent_pi <- ipv_df_consent_pi %>%
  mutate(!!sym(curr_ipv_m0) := ifelse(is.na(!!sym(curr_ipv)),NA, !!sym(curr_ipv_m0))) 

# Calculate the global mean of the index among the control groups
mean_glob_m0 = mean(ipv_df_consent_pi %>% 
                      filter(treatment_pi_pool == "Control") %>% 
                      select(!!sym(curr_ipv_m0)) %>% 
                      pull(), na.rm = TRUE)

# Calculate the global standard deviation of the index among the control groups
std_glob_m0 = sd(ipv_df_consent_pi %>% 
                   filter(treatment_pi_pool == "Control") %>% 
                   select(!!sym(curr_ipv_m0)) %>% 
                   pull(), na.rm = TRUE)

# Standardize the IPV index using the global mean and standard deviation, and add a label
ipv_df_consent_pi <- ipv_df_consent_pi %>%
  mutate(!!sym(curr_ipv_m0) := ifelse(is.na(!!sym(curr_ipv_m0)),NA, ((!!sym(curr_ipv_m0) - mean_glob_m0) / std_glob_m0)) %>%
           structure(label = lab_curr_ipv_m0)) 

####### Missing values imputed replaced by 1 ####################################

# Create the IPV severity index as the mean of selected items, handling missing values
ipv_df_consent_pi <- ipv_df_consent_pi %>%
  # Compute the row-wise mean of the selected IPV variables
  mutate(!!sym(curr_ipv_m1) := apply(select(., scale_vars), 1, function(x) {
    x[is.na(x)] <- 1  # Replace NA with 1
    sum(x)             # Compute sum
  })) %>%
  mutate(!!sym(curr_ipv_m1) := !!sym(curr_ipv_m1)/length(scale_vars)) %>%
  # Replace values with NA if all selected variables are NA for a given row
  mutate(!!sym(curr_ipv_m1) := ifelse(rowSums(!is.na(select(., all_of(scale_vars)))) == 0, NA, !!sym(curr_ipv_m1)))

# Transfort to NA the index values that are missing in the regular indicator
ipv_df_consent_pi <- ipv_df_consent_pi %>%
  mutate(!!sym(curr_ipv_m1) := ifelse(is.na(!!sym(curr_ipv)),NA, !!sym(curr_ipv_m1))) 

# Calculate the global mean of the index among the control groups
mean_glob_m1 = mean(ipv_df_consent_pi %>% 
                      filter(treatment_pi_pool == "Control") %>% 
                      select(!!sym(curr_ipv_m1)) %>% 
                      pull(), na.rm = TRUE)

# Calculate the global standard deviation of the index among the control groups
std_glob_m1 = sd(ipv_df_consent_pi %>% 
                   filter(treatment_pi_pool == "Control") %>% 
                   select(!!sym(curr_ipv_m1)) %>% 
                   pull(), na.rm = TRUE)

# Standardize the IPV index using the global mean and standard deviation, and add a label
ipv_df_consent_pi <- ipv_df_consent_pi %>%
  mutate(!!sym(curr_ipv_m1) := ifelse(is.na(!!sym(curr_ipv_m1)),NA, ((!!sym(curr_ipv_m1) - mean_glob_m1) / std_glob_m1)) %>%
           structure(label = lab_curr_ipv_m1)) 

# Comparison table
ipv_df_consent_pi %>%
  select(!!sym(curr_ipv), !!sym(curr_ipv_m0), !!sym(curr_ipv_m1)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  group_by(variable) %>%
  summarise(
    n = n(),
    mean = mean(value, na.rm = TRUE),
    median = median(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    min = min(value, na.rm = TRUE),
    max = max(value, na.rm = TRUE),
    q25 = quantile(value, 0.25, na.rm = TRUE),
    q75 = quantile(value, 0.75, na.rm = TRUE)
  )

# Process the dataframe
ipv_df_consent_cash <- ipv_df_consent_cash %>%
  # 1. Replace NA in 'c' variables with 0 IF the 'b' variable is 0
  mutate(across(all_of(scale_vars), ~ {
    # Identify the corresponding 'b' column for the current 'c' column
    b_col_name <- sub("c", "b", cur_column())
    # Logical check: if 'b' is 0 and 'c' is NA, make 'c' 0
    ifelse(get(b_col_name) == 0 & is.na(.x), 0, .x)
  }))

################################################################################
######################## Emotional violence ####################################
################################################################################

# Define the variables to include in the IPV severity index
scale_vars = c("ipv8","ipv9","ipv10","ipv11")

# Set the name of the new index variable to be created
curr_ipv = "emo_ipv_sev_index"
lab_curr_ipv = "Emo. IPV severity index"

curr_ipv_m0 = "emo_ipv_sev_index_m0"
lab_curr_ipv_m0 = "Emo. IPV severity index (missing replaced by 0)"

curr_ipv_m1 = "emo_ipv_sev_index_m1"
lab_curr_ipv_m1 = "Emo. IPV severity index (missing replaced by 1)"

# Automatisation de la mutation basée sur scale_vars
ipv_df_consent_cash <- ipv_df_consent_cash %>%
  mutate(across(
    .cols = all_of(paste0(scale_vars, "c")),
    .fns = ~ ifelse(get(paste0(cur_column() %>% str_remove("c$"), "b")) == 0, 0, .),
    .names = "{.col}"
  ))

# Automatisation de la mutation basée sur scale_vars
ipv_df_consent_pi <- ipv_df_consent_pi %>%
  mutate(across(
    .cols = all_of(paste0(scale_vars, "c")),
    .fns = ~ ifelse(get(paste0(cur_column() %>% str_remove("c$"), "b")) == 0, 0, .),
    .names = "{.col}"
  ))

######################### Tekavoul (Cash Transfer) ####################################
# Create the IPV severity index as the mean of selected items, handling missing values
ipv_df_consent_cash <- ipv_df_consent_cash %>%
  # Compute the row-wise mean of the selected IPV variables
  mutate(!!sym(curr_ipv) := apply(select(., paste0(scale_vars,"c")), 1, sum, na.rm = TRUE)) %>%
  mutate(!!sym(curr_ipv) := !!sym(curr_ipv)/length(scale_vars)) %>%
  # Replace values with 0 if any of the selected variables are 0 for a given row
  mutate(!!sym(curr_ipv) := ifelse(rowSums(select(., all_of(c(paste0(scale_vars, "a"), 
                                                              paste0(scale_vars, "b")))) == 0, na.rm = TRUE) > 0 & is.na(!!sym(curr_ipv)), 0,!!sym(curr_ipv)))

# Transfort to NA the index values that are missing in the regular indicator
ipv_df_consent_cash <- ipv_df_consent_cash %>%
  mutate(!!sym(curr_ipv) := ifelse(is.na(!!sym(paste0(substr(curr_ipv, 1, nchar(curr_ipv) - 9), "12m"))),NA, !!sym(curr_ipv))) 
           
# Calculate the global mean of the index among the control groups
mean_glob = mean(ipv_df_consent_cash %>% 
                   filter(treatment_csh_trnsfr == "Control") %>% 
                   select(!!sym(curr_ipv)) %>% 
                   pull(), na.rm = TRUE)

# Calculate the global standard deviation of the index among the control groups
std_glob = sd(ipv_df_consent_cash %>% 
                filter(treatment_csh_trnsfr == "Control") %>% 
                select(!!sym(curr_ipv)) %>% 
                pull(), na.rm = TRUE)

# Standardize the IPV index using the global mean and standard deviation, and add a label
ipv_df_consent_cash <- ipv_df_consent_cash %>%
  mutate(!!sym(curr_ipv) := ifelse(is.na(!!sym(curr_ipv)),NA, ((!!sym(curr_ipv) - mean_glob) / std_glob)) %>%
           structure(label = lab_curr_ipv)) %>%
  # Rename an existing variable (to make sure the source name is correct)
  rename(emo_ipv_sev_12m = emo_ipv_12m)%>% 
  mutate(emo_ipv_sev_12m_m0 = ifelse(is.na(emo_ipv_sev_12m),0,emo_ipv_sev_12m),
         emo_ipv_sev_12m_m1 = ifelse(is.na(emo_ipv_sev_12m),1,emo_ipv_sev_12m)
  )


####### Missing values imputed replaced by 0 ####################################

# Create the IPV severity index as the mean of selected items, handling missing values
ipv_df_consent_cash <- ipv_df_consent_cash %>%
  # Compute the row-wise mean of the selected IPV variables   
  mutate(!!sym(curr_ipv_m0) := apply(select(., paste0(scale_vars,"c")), 1, function(x) {
    x[is.na(x)] <- 0  # Replace NA with 0
    sum(x)             # Compute sum
  })) %>%
  mutate(!!sym(curr_ipv_m0) := !!sym(curr_ipv_m0)/length(scale_vars)) %>%
  # Replace values with 0 if any of the selected variables are 0 for a given row
  mutate(!!sym(curr_ipv_m0) := ifelse(rowSums(select(., all_of(c(paste0(scale_vars, "a"), 
                                                              paste0(scale_vars, "b")))) == 0, na.rm = TRUE) > 0 & is.na(!!sym(curr_ipv_m0)), 0,!!sym(curr_ipv_m0)))

# Transfort to NA the index values that are missing in the regular indicator
ipv_df_consent_cash <- ipv_df_consent_cash %>%
  mutate(!!sym(curr_ipv_m0) := ifelse(is.na(!!sym(curr_ipv)),NA, !!sym(curr_ipv_m0))) 

# Calculate the global mean of the index among the control groups
mean_glob_m0 = mean(ipv_df_consent_cash %>% 
                      filter(treatment_csh_trnsfr == "Control") %>% 
                      select(!!sym(curr_ipv_m0)) %>% 
                      pull(), na.rm = TRUE)

# Calculate the global standard deviation of the index among the control groups
std_glob_m0 = sd(ipv_df_consent_cash %>% 
                   filter(treatment_csh_trnsfr == "Control") %>% 
                   select(!!sym(curr_ipv_m0)) %>% 
                   pull(), na.rm = TRUE)

# Standardize the IPV index using the global mean and standard deviation, and add a label
ipv_df_consent_cash <- ipv_df_consent_cash %>%
  mutate(!!sym(curr_ipv_m0) := ifelse(is.na(!!sym(curr_ipv_m0)),NA, ((!!sym(curr_ipv_m0) - mean_glob_m0) / std_glob_m0)) %>%
           structure(label = lab_curr_ipv_m0)) 

####### Missing values imputed replaced by 1 ####################################

# Create the IPV severity index as the mean of selected items, handling missing values
ipv_df_consent_cash <- ipv_df_consent_cash %>%
  # Compute the row-wise mean of the selected IPV variables
  mutate(!!sym(curr_ipv_m1) := apply(select(., paste0(scale_vars,"c")), 1, function(x) {
    x[is.na(x)] <- 1  # Replace NA with 0
    sum(x)             # Compute sum
  })) %>%
  mutate(!!sym(curr_ipv_m1) := !!sym(curr_ipv_m1)/length(scale_vars)) %>%
  # Replace values with 0 if any of the selected variables are 0 for a given row
  mutate(!!sym(curr_ipv_m1) := ifelse(rowSums(select(., all_of(c(paste0(scale_vars, "a"), 
                                                              paste0(scale_vars, "b")))) == 0, na.rm = TRUE) > 0 & is.na(!!sym(curr_ipv_m1)), 0,!!sym(curr_ipv_m1)))

# Transfort to NA the index values that are missing in the regular indicator
ipv_df_consent_cash <- ipv_df_consent_cash %>%
  mutate(!!sym(curr_ipv_m1) := ifelse(is.na(!!sym(curr_ipv)),NA, !!sym(curr_ipv_m1))) 

# Calculate the global mean of the index among the control groups
mean_glob_m1 = mean(ipv_df_consent_cash %>% 
                      filter(treatment_csh_trnsfr == "Control") %>% 
                      select(!!sym(curr_ipv_m1)) %>% 
                      pull(), na.rm = TRUE)

# Calculate the global standard deviation of the index among the control groups
std_glob_m1 = sd(ipv_df_consent_cash %>% 
                   filter(treatment_csh_trnsfr == "Control") %>% 
                   select(!!sym(curr_ipv_m1)) %>% 
                   pull(), na.rm = TRUE)

# Standardize the IPV index using the global mean and standard deviation, and add a label
ipv_df_consent_cash <- ipv_df_consent_cash %>%
  mutate(!!sym(curr_ipv_m1) := ifelse(is.na(!!sym(curr_ipv_m1)),NA, ((!!sym(curr_ipv_m1) - mean_glob_m1) / std_glob_m1)) %>%
           structure(label = lab_curr_ipv_m1)) 

# Comparison table
ipv_df_consent_cash %>%
  select(!!sym(curr_ipv), !!sym(curr_ipv_m0), !!sym(curr_ipv_m1)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  group_by(variable) %>%
  summarise(
    n = n(),
    mean = mean(value, na.rm = TRUE),
    median = median(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    min = min(value, na.rm = TRUE),
    max = max(value, na.rm = TRUE),
    q25 = quantile(value, 0.25, na.rm = TRUE),
    q75 = quantile(value, 0.75, na.rm = TRUE)
  )

######################### Economic Inclusion ####################################


# Create the IPV severity index as the mean of selected items, handling missing values
ipv_df_consent_pi <- ipv_df_consent_pi %>%
  # Compute the row-wise mean of the selected IPV variables
  mutate(!!sym(curr_ipv) := apply(select(., paste0(scale_vars,"c")), 1, sum, na.rm = TRUE)) %>%
  mutate(!!sym(curr_ipv) := !!sym(curr_ipv)/length(scale_vars)) %>%
  # Replace values with 0 if any of the selected variables are 0 for a given row
  mutate(!!sym(curr_ipv) := ifelse(rowSums(select(., all_of(c(paste0(scale_vars, "a"), 
                                                                 paste0(scale_vars, "b")))) == 0, na.rm = TRUE) > 0 & is.na(!!sym(curr_ipv)), 0,!!sym(curr_ipv)))

# Transfort to NA the index values that are missing in the regular indicator
ipv_df_consent_pi <- ipv_df_consent_pi %>%
  mutate(!!sym(curr_ipv) := ifelse(is.na(!!sym(paste0(substr(curr_ipv, 1, nchar(curr_ipv) - 9), "12m"))),NA, !!sym(curr_ipv))) 

# Calculate the global mean of the index among the control groups
mean_glob = mean(ipv_df_consent_pi %>% 
                   filter(treatment_pi_pool == "Control") %>% 
                   select(!!sym(curr_ipv)) %>% 
                   pull(), na.rm = TRUE)

# Calculate the global standard deviation of the index among the control groups
std_glob = sd(ipv_df_consent_pi %>% 
                filter(treatment_pi_pool == "Control") %>% 
                select(!!sym(curr_ipv)) %>% 
                pull(), na.rm = TRUE)

# Standardize the IPV index using the global mean and standard deviation, and add a label
ipv_df_consent_pi <- ipv_df_consent_pi %>%
  mutate(!!sym(curr_ipv) := ifelse(is.na(!!sym(curr_ipv)), NA, ((!!sym(curr_ipv) - mean_glob) / std_glob)) %>%
           structure(label = lab_curr_ipv)) %>%
  # Rename an existing variable (to make sure the source name is correct)
  rename(emo_ipv_sev_12m = emo_ipv_12m)%>% 
  mutate(emo_ipv_sev_12m_m0 = ifelse(is.na(emo_ipv_sev_12m),0,emo_ipv_sev_12m),
         emo_ipv_sev_12m_m1 = ifelse(is.na(emo_ipv_sev_12m),1,emo_ipv_sev_12m)
  )

####### Missing values imputed replaced by 0 ####################################

# Create the IPV severity index as the mean of selected items, handling missing values
ipv_df_consent_pi <- ipv_df_consent_pi %>%
  # Compute the row-wise mean of the selected IPV variables
  mutate(!!sym(curr_ipv_m0) := apply(select(., paste0(scale_vars,"c")), 1, function(x) {
    x[is.na(x)] <- 0  # Replace NA with 0
    sum(x)             # Compute sum
  })) %>%
  mutate(!!sym(curr_ipv_m0) := !!sym(curr_ipv_m0)/length(scale_vars)) %>%
  # Replace values with 0 if any of the selected variables are 0 for a given row
  mutate(!!sym(curr_ipv_m0) := ifelse(rowSums(select(., all_of(c(paste0(scale_vars, "a"), 
                                                                 paste0(scale_vars, "b")))) == 0, na.rm = TRUE) > 0 & is.na(!!sym(curr_ipv_m0)), 0,!!sym(curr_ipv_m0)))

# Calculate the global mean of the index among the control groups
mean_glob_m0 = mean(ipv_df_consent_pi %>% 
                      filter(treatment_pi_pool == "Control") %>% 
                      select(!!sym(curr_ipv_m0)) %>% 
                      pull(), na.rm = TRUE)

# Calculate the global standard deviation of the index among the control groups
std_glob_m0 = sd(ipv_df_consent_pi %>% 
                   filter(treatment_pi_pool == "Control") %>% 
                   select(!!sym(curr_ipv_m0)) %>% 
                   pull(), na.rm = TRUE)

# Standardize the IPV index using the global mean and standard deviation, and add a label
ipv_df_consent_pi <- ipv_df_consent_pi %>%
  mutate(!!sym(curr_ipv_m0) := ifelse(is.na(!!sym(curr_ipv_m0)),NA, ((!!sym(curr_ipv_m0) - mean_glob_m0) / std_glob_m0)) %>%
           structure(label = lab_curr_ipv_m0)) 

####### Missing values imputed replaced by 1 ####################################

# Create the IPV severity index as the mean of selected items, handling missing values
ipv_df_consent_pi <- ipv_df_consent_pi %>%
  # Compute the row-wise mean of the selected IPV variables
  mutate(!!sym(curr_ipv_m1) := apply(select(., paste0(scale_vars,"c")), 1, function(x) {
    x[is.na(x)] <- 1  # Replace NA with 1
    sum(x)             # Compute sum
  })) %>%
  mutate(!!sym(curr_ipv_m1) := !!sym(curr_ipv_m1)/length(scale_vars)) %>%
  # Replace values with 0 if any of the selected variables are 0 for a given row
  mutate(!!sym(curr_ipv_m1) := ifelse(rowSums(select(., all_of(c(paste0(scale_vars, "a"), 
                                                                 paste0(scale_vars, "b")))) == 0, na.rm = TRUE) > 0 & is.na(!!sym(curr_ipv_m1)), 0,!!sym(curr_ipv_m1)))

# Calculate the global mean of the index among the control groups
mean_glob_m1 = mean(ipv_df_consent_pi %>% 
                      filter(treatment_pi_pool == "Control") %>% 
                      select(!!sym(curr_ipv_m1)) %>% 
                      pull(), na.rm = TRUE)

# Calculate the global standard deviation of the index among the control groups
std_glob_m1 = sd(ipv_df_consent_pi %>% 
                   filter(treatment_pi_pool == "Control") %>% 
                   select(!!sym(curr_ipv_m1)) %>% 
                   pull(), na.rm = TRUE)

# Standardize the IPV index using the global mean and standard deviation, and add a label
ipv_df_consent_pi <- ipv_df_consent_pi %>%
  mutate(!!sym(curr_ipv_m1) := ifelse(is.na(!!sym(curr_ipv_m1)),NA, ((!!sym(curr_ipv_m1) - mean_glob_m1) / std_glob_m1)) %>%
           structure(label = lab_curr_ipv_m1)) 

# Comparison table
ipv_df_consent_pi %>%
  select(!!sym(curr_ipv), !!sym(curr_ipv_m0), !!sym(curr_ipv_m1)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  group_by(variable) %>%
  summarise(
    n = n(),
    mean = mean(value, na.rm = TRUE),
    median = median(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    min = min(value, na.rm = TRUE),
    max = max(value, na.rm = TRUE),
    q25 = quantile(value, 0.25, na.rm = TRUE),
    q75 = quantile(value, 0.75, na.rm = TRUE)
  )
################################################################################
######################## Physical violence #####################################
################################################################################

# Define the variables to include in the IPV severity index
scale_vars = c("ipv12","ipv13","ipv14","ipv15","ipv16","ipv17")

# Set the name of the new index variable to be created
curr_ipv = "phy_ipv_sev_index"
lab_curr_ipv = "Phy. IPV severity index"

curr_ipv_m0 = "phy_ipv_sev_index_m0"
lab_curr_ipv_m0 = "Phy. IPV severity index (missing replaced by 0)"

curr_ipv_m1 = "phy_ipv_sev_index_m1"
lab_curr_ipv_m1 = "Phy. IPV severity index (missing replaced by 1)"

# Automatisation de la mutation basée sur scale_vars
ipv_df_consent_cash <- ipv_df_consent_cash %>%
  mutate(across(
    .cols = all_of(paste0(scale_vars, "c")),
    .fns = ~ ifelse(get(paste0(cur_column() %>% str_remove("c$"), "b")) == 0, 0, .),
    .names = "{.col}"
  ))

# Automatisation de la mutation basée sur scale_vars
ipv_df_consent_pi <- ipv_df_consent_pi %>%
  mutate(across(
    .cols = all_of(paste0(scale_vars, "c")),
    .fns = ~ ifelse(get(paste0(cur_column() %>% str_remove("c$"), "b")) == 0, 0, .),
    .names = "{.col}"
  ))

######################### Tekavoul (Cash Transfer) ####################################
# Create the IPV severity index as the mean of selected items, handling missing values
ipv_df_consent_cash <- ipv_df_consent_cash %>%
  # Compute the row-wise mean of the selected IPV variables
  mutate(!!sym(curr_ipv) := apply(select(., paste0(scale_vars,"c")), 1, sum, na.rm = TRUE)) %>%
  mutate(!!sym(curr_ipv) := !!sym(curr_ipv)/length(scale_vars)) %>%
  # Replace values with 0 if any of the selected variables are 0 for a given row
  mutate(!!sym(curr_ipv) := ifelse(rowSums(select(., all_of(c(paste0(scale_vars, "a"), 
                                                                 paste0(scale_vars, "b")))) == 0, na.rm = TRUE) > 0 & is.na(!!sym(curr_ipv)), 0,!!sym(curr_ipv)))

# Transfort to NA the index values that are missing in the regular indicator
ipv_df_consent_cash <- ipv_df_consent_cash %>%
  mutate(!!sym(curr_ipv) := ifelse(is.na(!!sym(paste0(substr(curr_ipv, 1, nchar(curr_ipv) - 9), "12m"))),NA, !!sym(curr_ipv))) 

# Calculate the global mean of the index among the control groups
mean_glob = mean(ipv_df_consent_cash %>% 
                   filter(treatment_csh_trnsfr == "Control") %>% 
                   select(!!sym(curr_ipv)) %>% 
                   pull(), na.rm = TRUE)

# Calculate the global standard deviation of the index among the control groups
std_glob = sd(ipv_df_consent_cash %>% 
                filter(treatment_csh_trnsfr == "Control") %>% 
                select(!!sym(curr_ipv)) %>% 
                pull(), na.rm = TRUE)

# Standardize the IPV index using the global mean and standard deviation, and add a label
ipv_df_consent_cash <- ipv_df_consent_cash %>%
  mutate(!!sym(curr_ipv) := ifelse(is.na(!!sym(curr_ipv)),NA, ((!!sym(curr_ipv) - mean_glob) / std_glob)) %>%
           structure(label = lab_curr_ipv)) %>%
  # Rename an existing variable (to make sure the source name is correct)
  rename(phy_ipv_sev_12m = phy_ipv_12m)%>% 
  mutate(phy_ipv_sev_12m_m0 = ifelse(is.na(phy_ipv_sev_12m),0,phy_ipv_sev_12m),
         phy_ipv_sev_12m_m1 = ifelse(is.na(phy_ipv_sev_12m),1,phy_ipv_sev_12m)
  )


####### Missing values imputed replaced by 0 ####################################

# Create the IPV severity index as the mean of selected items, handling missing values
ipv_df_consent_cash <- ipv_df_consent_cash %>%
  # Compute the row-wise mean of the selected IPV variables
  mutate(!!sym(curr_ipv_m0) := apply(select(., paste0(scale_vars,"c")), 1, function(x) {
    x[is.na(x)] <- 0  # Replace NA with 0
    sum(x)             # Compute sum
  })) %>%
  mutate(!!sym(curr_ipv_m0) := !!sym(curr_ipv_m0)/length(scale_vars)) %>%
  # Replace values with 0 if any of the selected variables are 0 for a given row
  mutate(!!sym(curr_ipv_m0) := ifelse(rowSums(select(., all_of(c(paste0(scale_vars, "a"), 
                                                                 paste0(scale_vars, "b")))) == 0, na.rm = TRUE) > 0 & is.na(!!sym(curr_ipv_m0)), 0,!!sym(curr_ipv_m0)))

# Calculate the global mean of the index among the control groups
mean_glob_m0 = mean(ipv_df_consent_cash %>% 
                      filter(treatment_csh_trnsfr == "Control") %>% 
                      select(!!sym(curr_ipv_m0)) %>% 
                      pull(), na.rm = TRUE)

# Calculate the global standard deviation of the index among the control groups
std_glob_m0 = sd(ipv_df_consent_cash %>% 
                   filter(treatment_csh_trnsfr == "Control") %>% 
                   select(!!sym(curr_ipv_m0)) %>% 
                   pull(), na.rm = TRUE)

# Standardize the IPV index using the global mean and standard deviation, and add a label
ipv_df_consent_cash <- ipv_df_consent_cash %>%
  mutate(!!sym(curr_ipv_m0) := ifelse(is.na(!!sym(curr_ipv_m0)),NA, ((!!sym(curr_ipv_m0) - mean_glob_m0) / std_glob_m0)) %>%
           structure(label = lab_curr_ipv_m0)) 

####### Missing values imputed replaced by 1 ####################################

# Create the IPV severity index as the mean of selected items, handling missing values
ipv_df_consent_cash <- ipv_df_consent_cash %>%
  # Compute the row-wise mean of the selected IPV variables
  mutate(!!sym(curr_ipv_m1) := apply(select(., paste0(scale_vars,"c")), 1, function(x) {
    x[is.na(x)] <- 1  # Replace NA with 0
    sum(x)             # Compute sum
  })) %>%
  mutate(!!sym(curr_ipv_m1) := !!sym(curr_ipv_m1)/length(scale_vars)) %>%
  # Replace values with 0 if any of the selected variables are 0 for a given row
  mutate(!!sym(curr_ipv_m1) := ifelse(rowSums(select(., all_of(c(paste0(scale_vars, "a"), 
                                                                 paste0(scale_vars, "b")))) == 0, na.rm = TRUE) > 0 & is.na(!!sym(curr_ipv_m1)), 0,!!sym(curr_ipv_m1)))

# Calculate the global mean of the index among the control groups
mean_glob_m1 = mean(ipv_df_consent_cash %>% 
                      filter(treatment_csh_trnsfr == "Control") %>% 
                      select(!!sym(curr_ipv_m1)) %>% 
                      pull(), na.rm = TRUE)

# Calculate the global standard deviation of the index among the control groups
std_glob_m1 = sd(ipv_df_consent_cash %>% 
                   filter(treatment_csh_trnsfr == "Control") %>% 
                   select(!!sym(curr_ipv_m1)) %>% 
                   pull(), na.rm = TRUE)

# Standardize the IPV index using the global mean and standard deviation, and add a label
ipv_df_consent_cash <- ipv_df_consent_cash %>%
  mutate(!!sym(curr_ipv_m0) := ifelse(is.na(!!sym(curr_ipv_m0)),NA, ((!!sym(curr_ipv_m0) - mean_glob_m0) / std_glob_m0)) %>%
           structure(label = lab_curr_ipv_m1)) 

# Comparison table
ipv_df_consent_cash %>%
  select(!!sym(curr_ipv), !!sym(curr_ipv_m0), !!sym(curr_ipv_m1)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  group_by(variable) %>%
  summarise(
    n = n(),
    mean = mean(value, na.rm = TRUE),
    median = median(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    min = min(value, na.rm = TRUE),
    max = max(value, na.rm = TRUE),
    q25 = quantile(value, 0.25, na.rm = TRUE),
    q75 = quantile(value, 0.75, na.rm = TRUE)
  )

######################### Economic Inclusion ####################################
# Create the IPV severity index as the mean of selected items, handling missing values
ipv_df_consent_pi <- ipv_df_consent_pi %>%
  # Compute the row-wise mean of the selected IPV variables
  mutate(!!sym(curr_ipv) := apply(select(., paste0(scale_vars,"c")), 1, sum, na.rm = TRUE)) %>%
  mutate(!!sym(curr_ipv) := !!sym(curr_ipv)/length(scale_vars)) %>%
  # Replace values with 0 if any of the selected variables are 0 for a given row
  mutate(!!sym(curr_ipv) := ifelse(rowSums(select(., all_of(c(paste0(scale_vars, "a"), 
                                                                 paste0(scale_vars, "b")))) == 0, na.rm = TRUE) > 0 & is.na(!!sym(curr_ipv)), 0,!!sym(curr_ipv)))

# Transfort to NA the index values that are missing in the regular indicator
ipv_df_consent_pi <- ipv_df_consent_pi %>%
  mutate(!!sym(curr_ipv) := ifelse(is.na(!!sym(paste0(substr(curr_ipv, 1, nchar(curr_ipv) - 9), "12m"))),NA, !!sym(curr_ipv))) 

# Calculate the global mean of the index among the control groups
mean_glob = mean(ipv_df_consent_pi %>% 
                   filter(treatment_pi_pool == "Control") %>% 
                   select(!!sym(curr_ipv)) %>% 
                   pull(), na.rm = TRUE)

# Calculate the global standard deviation of the index among the control groups
std_glob = sd(ipv_df_consent_pi %>% 
                filter(treatment_pi_pool == "Control") %>% 
                select(!!sym(curr_ipv)) %>% 
                pull(), na.rm = TRUE)

# Standardize the IPV index using the global mean and standard deviation, and add a label
ipv_df_consent_pi <- ipv_df_consent_pi %>%
  mutate(!!sym(curr_ipv) := ifelse(is.na(!!sym(curr_ipv)),NA, ((!!sym(curr_ipv) - mean_glob) / std_glob)) %>%
           structure(label = lab_curr_ipv)) %>%
  # Rename an existing variable (to make sure the source name is correct)
  rename(phy_ipv_sev_12m = phy_ipv_12m)%>% 
  mutate(phy_ipv_sev_12m_m0 = ifelse(is.na(phy_ipv_sev_12m),0,phy_ipv_sev_12m),
         phy_ipv_sev_12m_m1 = ifelse(is.na(phy_ipv_sev_12m),1,phy_ipv_sev_12m)
  )

####### Missing values imputed replaced by 0 ####################################

# Create the IPV severity index as the mean of selected items, handling missing values
ipv_df_consent_pi <- ipv_df_consent_pi %>%
  # Compute the row-wise mean of the selected IPV variables
  mutate(!!sym(curr_ipv_m0) := apply(select(., paste0(scale_vars,"c")), 1, function(x) {
    x[is.na(x)] <- 0  # Replace NA with 0
    sum(x)             # Compute sum
  })) %>%
  mutate(!!sym(curr_ipv_m0) := !!sym(curr_ipv_m0)/length(scale_vars)) %>%
  # Replace values with 0 if any of the selected variables are 0 for a given row
  mutate(!!sym(curr_ipv_m0) := ifelse(rowSums(select(., all_of(c(paste0(scale_vars, "a"), 
                                                                 paste0(scale_vars, "b")))) == 0, na.rm = TRUE) > 0 & is.na(!!sym(curr_ipv_m0)), 0,!!sym(curr_ipv_m0)))

# Calculate the global mean of the index among the control groups
mean_glob_m0 = mean(ipv_df_consent_pi %>% 
                      filter(treatment_pi_pool == "Control") %>% 
                      select(!!sym(curr_ipv_m0)) %>% 
                      pull(), na.rm = TRUE)

# Calculate the global standard deviation of the index among the control groups
std_glob_m0 = sd(ipv_df_consent_pi %>% 
                   filter(treatment_pi_pool == "Control") %>% 
                   select(!!sym(curr_ipv_m0)) %>% 
                   pull(), na.rm = TRUE)

# Standardize the IPV index using the global mean and standard deviation, and add a label
ipv_df_consent_pi <- ipv_df_consent_pi %>%
  mutate(!!sym(curr_ipv_m0) := ifelse(is.na(!!sym(curr_ipv_m0)),NA, ((!!sym(curr_ipv_m0) - mean_glob_m0) / std_glob_m0)) %>%
           structure(label = lab_curr_ipv_m0)) 

####### Missing values imputed replaced by 1 ####################################

# Create the IPV severity index as the mean of selected items, handling missing values
ipv_df_consent_pi <- ipv_df_consent_pi %>%
  # Compute the row-wise mean of the selected IPV variables
  mutate(!!sym(curr_ipv_m1) := apply(select(., paste0(scale_vars,"c")), 1, function(x) {
    x[is.na(x)] <- 1  # Replace NA with 1
    sum(x)             # Compute sum
  })) %>%
  mutate(!!sym(curr_ipv_m1) := !!sym(curr_ipv_m1)/length(scale_vars)) %>%
  # Replace values with 0 if any of the selected variables are 0 for a given row
  mutate(!!sym(curr_ipv_m1) := ifelse(rowSums(select(., all_of(c(paste0(scale_vars, "a"), 
                                                                 paste0(scale_vars, "b")))) == 0, na.rm = TRUE) > 0 & is.na(!!sym(curr_ipv_m1)), 0,!!sym(curr_ipv_m1)))

# Calculate the global mean of the index among the control groups
mean_glob_m1 = mean(ipv_df_consent_pi %>% 
                      filter(treatment_pi_pool == "Control") %>% 
                      select(!!sym(curr_ipv_m1)) %>% 
                      pull(), na.rm = TRUE)

# Calculate the global standard deviation of the index among the control groups
std_glob_m1 = sd(ipv_df_consent_pi %>% 
                   filter(treatment_pi_pool == "Control") %>% 
                   select(!!sym(curr_ipv_m1)) %>% 
                   pull(), na.rm = TRUE)

# Standardize the IPV index using the global mean and standard deviation, and add a label
ipv_df_consent_pi <- ipv_df_consent_pi %>%
  mutate(!!sym(curr_ipv_m1) := ifelse(is.na(!!sym(curr_ipv_m1)),NA, ((!!sym(curr_ipv_m1) - mean_glob_m1) / std_glob_m1)) %>%
           structure(label = lab_curr_ipv_m1)) 

# Comparison table
ipv_df_consent_pi %>%
  select(!!sym(curr_ipv), !!sym(curr_ipv_m0), !!sym(curr_ipv_m1)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  group_by(variable) %>%
  summarise(
    n = n(),
    mean = mean(value, na.rm = TRUE),
    median = median(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    min = min(value, na.rm = TRUE),
    max = max(value, na.rm = TRUE),
    q25 = quantile(value, 0.25, na.rm = TRUE),
    q75 = quantile(value, 0.75, na.rm = TRUE)
  )
################################################################################
######################## Sexual violence #######################################
################################################################################

# Define the variables to include in the IPV severity index
scale_vars = c("ipv18","ipv19","ipv20")

# Set the name of the new index variable to be created
curr_ipv = "sex_ipv_sev_index"
lab_curr_ipv = "Sex. IPV severity index"

curr_ipv_m0 = "sex_ipv_sev_index_m0"
lab_curr_ipv_m0 = "Sex. IPV severity index (missing replaced by 0)"

curr_ipv_m1 = "sex_ipv_sev_index_m1"
lab_curr_ipv_m1 = "Sex. IPV severity index (missing replaced by 1)"

# Automatisation de la mutation basée sur scale_vars
ipv_df_consent_cash <- ipv_df_consent_cash %>%
  mutate(across(
    .cols = all_of(paste0(scale_vars, "c")),
    .fns = ~ ifelse(get(paste0(cur_column() %>% str_remove("c$"), "b")) == 0, 0, .),
    .names = "{.col}"
  ))

# Automatisation de la mutation basée sur scale_vars
ipv_df_consent_pi <- ipv_df_consent_pi %>%
  mutate(across(
    .cols = all_of(paste0(scale_vars, "c")),
    .fns = ~ ifelse(get(paste0(cur_column() %>% str_remove("c$"), "b")) == 0, 0, .),
    .names = "{.col}"
  ))


######################### Tekavoul (Cash Transfer) ####################################
# Create the IPV severity index as the mean of selected items, handling missing values
ipv_df_consent_cash <- ipv_df_consent_cash %>%
  # Compute the row-wise mean of the selected IPV variables
  mutate(!!sym(curr_ipv) := apply(select(., paste0(scale_vars,"c")), 1, sum, na.rm = TRUE)) %>%
  mutate(!!sym(curr_ipv) := !!sym(curr_ipv)/length(scale_vars)) %>%
  # Replace values with 0 if any of the selected variables are 0 for a given row
  mutate(!!sym(curr_ipv) := ifelse(rowSums(select(., all_of(c(paste0(scale_vars, "a"), 
                                                              paste0(scale_vars, "b")))) == 0, na.rm = TRUE) > 0 & is.na(!!sym(curr_ipv)), 0,!!sym(curr_ipv)))

# Transfort to NA the index values that are missing in the regular indicator
ipv_df_consent_cash <- ipv_df_consent_cash %>%
  mutate(!!sym(curr_ipv) := ifelse(is.na(!!sym(paste0(substr(curr_ipv, 1, nchar(curr_ipv) - 9), "12m"))),NA, !!sym(curr_ipv))) 

# Calculate the global mean of the index among the control groups
mean_glob = mean(ipv_df_consent_cash %>% 
                   filter(treatment_csh_trnsfr == "Control") %>% 
                   select(!!sym(curr_ipv)) %>% 
                   pull(), na.rm = TRUE)

# Calculate the global standard deviation of the index among the control groups
std_glob = sd(ipv_df_consent_cash %>% 
                filter(treatment_csh_trnsfr == "Control") %>% 
                select(!!sym(curr_ipv)) %>% 
                pull(), na.rm = TRUE)

# Standardize the IPV index using the global mean and standard deviation, and add a label
ipv_df_consent_cash <- ipv_df_consent_cash %>%
  mutate(!!sym(curr_ipv) := ifelse(is.na(!!sym(curr_ipv)),NA, ((!!sym(curr_ipv) - mean_glob) / std_glob)) %>%
           structure(label = lab_curr_ipv)) %>%
  # Rename an existing variable (to make sure the source name is correct)
  rename(sex_ipv_sev_12m = sex_ipv_12m)%>% 
  mutate(sex_ipv_sev_12m_m0 = ifelse(is.na(sex_ipv_sev_12m),0,sex_ipv_sev_12m),
         sex_ipv_sev_12m_m1 = ifelse(is.na(sex_ipv_sev_12m),1,sex_ipv_sev_12m)
  )


####### Missing values imputed replaced by 0 ####################################
# Create the IPV severity index as the mean of selected items, handling missing values
ipv_df_consent_cash <- ipv_df_consent_cash %>%
  # Compute the row-wise mean of the selected IPV variables
  mutate(!!sym(curr_ipv_m0) := apply(select(., paste0(scale_vars,"c")), 1, function(x) {
    x[is.na(x)] <- 0  # Replace NA with 0
    sum(x)             # Compute sum
  })) %>%
  mutate(!!sym(curr_ipv_m0) := !!sym(curr_ipv_m0)/length(scale_vars)) %>%
  # Replace values with 0 if any of the selected variables are 0 for a given row
  mutate(!!sym(curr_ipv_m0) := ifelse(rowSums(select(., all_of(c(paste0(scale_vars, "a"), 
                                                                 paste0(scale_vars, "b")))) == 0, na.rm = TRUE) > 0 & is.na(!!sym(curr_ipv_m0)), 0,!!sym(curr_ipv_m0)))

# Calculate the global mean of the index among the control groups
mean_glob_m0 = mean(ipv_df_consent_cash %>% 
                      filter(treatment_csh_trnsfr == "Control") %>% 
                      select(!!sym(curr_ipv_m0)) %>% 
                      pull(), na.rm = TRUE)

# Calculate the global standard deviation of the index among the control groups
std_glob_m0 = sd(ipv_df_consent_cash %>% 
                   filter(treatment_csh_trnsfr == "Control") %>% 
                   select(!!sym(curr_ipv_m0)) %>% 
                   pull(), na.rm = TRUE)

# Standardize the IPV index using the global mean and standard deviation, and add a label
ipv_df_consent_cash <- ipv_df_consent_cash %>%
  mutate(!!sym(curr_ipv_m0) := ifelse(is.na(!!sym(curr_ipv_m0)),NA, ((!!sym(curr_ipv_m0) - mean_glob_m0) / std_glob_m0)) %>%
           structure(label = lab_curr_ipv_m0)) 

####### Missing values imputed replaced by 1 ####################################

# Create the IPV severity index as the mean of selected items, handling missing values
ipv_df_consent_cash <- ipv_df_consent_cash %>%
  # Compute the row-wise mean of the selected IPV variables
  mutate(!!sym(curr_ipv_m1) := apply(select(., paste0(scale_vars,"c")), 1, function(x) {
    x[is.na(x)] <- 1  # Replace NA with 1
    sum(x)             # Compute sum
  })) %>%
  mutate(!!sym(curr_ipv_m1) := !!sym(curr_ipv_m1)/length(scale_vars)) %>%
  # Replace values with 0 if any of the selected variables are 0 for a given row
  mutate(!!sym(curr_ipv_m1) := ifelse(rowSums(select(., all_of(c(paste0(scale_vars, "a"), 
                                                                 paste0(scale_vars, "b")))) == 0, na.rm = TRUE) > 0 & is.na(!!sym(curr_ipv_m1)), 0,!!sym(curr_ipv_m1)))

# Calculate the global mean of the index among the control groups
mean_glob_m1 = mean(ipv_df_consent_cash %>% 
                      filter(treatment_csh_trnsfr == "Control") %>% 
                      select(!!sym(curr_ipv_m1)) %>% 
                      pull(), na.rm = TRUE)

# Calculate the global standard deviation of the index among the control groups
std_glob_m1 = sd(ipv_df_consent_cash %>% 
                   filter(treatment_csh_trnsfr == "Control") %>% 
                   select(!!sym(curr_ipv_m1)) %>% 
                   pull(), na.rm = TRUE)

# Standardize the IPV index using the global mean and standard deviation, and add a label
ipv_df_consent_cash <- ipv_df_consent_cash %>%
  mutate(!!sym(curr_ipv_m1) := ifelse(is.na(!!sym(curr_ipv_m1)),NA, ((!!sym(curr_ipv_m1) - mean_glob_m1) / std_glob_m1)) %>%
           structure(label = lab_curr_ipv_m1)) 

# Comparison table
ipv_df_consent_cash %>%
  select(!!sym(curr_ipv), !!sym(curr_ipv_m0), !!sym(curr_ipv_m1)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  group_by(variable) %>%
  summarise(
    n = n(),
    mean = mean(value, na.rm = TRUE),
    median = median(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    min = min(value, na.rm = TRUE),
    max = max(value, na.rm = TRUE),
    q25 = quantile(value, 0.25, na.rm = TRUE),
    q75 = quantile(value, 0.75, na.rm = TRUE)
  )

######################### Economic Inclusion ####################################
# Create the IPV severity index as the mean of selected items, handling missing values
ipv_df_consent_pi <- ipv_df_consent_pi %>%
  # Compute the row-wise mean of the selected IPV variables
  mutate(!!sym(curr_ipv) := apply(select(., paste0(scale_vars,"c")), 1, sum, na.rm = TRUE)) %>%
  mutate(!!sym(curr_ipv) := !!sym(curr_ipv)/length(scale_vars)) %>%
  # Replace values with 0 if any of the selected variables are 0 for a given row
  mutate(!!sym(curr_ipv) := ifelse(rowSums(select(., all_of(c(paste0(scale_vars, "a"), paste0(scale_vars, "b")))) == 0, na.rm = TRUE) > 0 & is.na(!!sym(curr_ipv)), 0,!!sym(curr_ipv)))

# Transfort to NA the index values that are missing in the regular indicator
ipv_df_consent_pi <- ipv_df_consent_pi %>%
  mutate(!!sym(curr_ipv) := ifelse(is.na(!!sym(paste0(substr(curr_ipv, 1, nchar(curr_ipv) - 9), "12m"))),NA, !!sym(curr_ipv))) 

# Calculate the global mean of the index among the control groups
mean_glob = mean(ipv_df_consent_pi %>% 
                   filter(treatment_pi_pool == "Control") %>% 
                   select(!!sym(curr_ipv)) %>% 
                   pull(), na.rm = TRUE)

# Calculate the global standard deviation of the index among the control groups
std_glob = sd(ipv_df_consent_pi %>% 
                filter(treatment_pi_pool == "Control") %>% 
                select(!!sym(curr_ipv)) %>% 
                pull(), na.rm = TRUE)

# Standardize the IPV index using the global mean and standard deviation, and add a label
ipv_df_consent_pi <- ipv_df_consent_pi %>%
  mutate(!!sym(curr_ipv) := ifelse(is.na(!!sym(curr_ipv)),NA, ((!!sym(curr_ipv) - mean_glob) / std_glob)) %>%
           structure(label = lab_curr_ipv)) %>%
  # Rename an existing variable (to make sure the source name is correct)
  rename(sex_ipv_sev_12m = sex_ipv_12m)%>% 
  mutate(sex_ipv_sev_12m_m0 = ifelse(is.na(sex_ipv_sev_12m),0,sex_ipv_sev_12m),
         sex_ipv_sev_12m_m1 = ifelse(is.na(sex_ipv_sev_12m),1,sex_ipv_sev_12m)
  )

####### Missing values imputed replaced by 0 ####################################

# Create the IPV severity index as the mean of selected items, handling missing values
ipv_df_consent_pi <- ipv_df_consent_pi %>%
  # Compute the row-wise mean of the selected IPV variables
  mutate(!!sym(curr_ipv_m0) := apply(select(., paste0(scale_vars,"c")), 1, function(x) {
    x[is.na(x)] <- 0  # Replace NA with 0
    sum(x)             # Compute sum
  })) %>%
  mutate(!!sym(curr_ipv_m0) := !!sym(curr_ipv_m0)/length(scale_vars)) %>%
  # Replace values with 0 if any of the selected variables are 0 for a given row
  mutate(!!sym(curr_ipv_m0) := ifelse(rowSums(select(., all_of(c(paste0(scale_vars, "a"), 
                                                                 paste0(scale_vars, "b")))) == 0, na.rm = TRUE) > 0 & is.na(!!sym(curr_ipv_m0)), 0,!!sym(curr_ipv_m0)))

# Calculate the global mean of the index among the control groups
mean_glob_m0 = mean(ipv_df_consent_pi %>% 
                      filter(treatment_pi_pool == "Control") %>% 
                      select(!!sym(curr_ipv_m0)) %>% 
                      pull(), na.rm = TRUE)

# Calculate the global standard deviation of the index among the control groups
std_glob_m0 = sd(ipv_df_consent_pi %>% 
                   filter(treatment_pi_pool == "Control") %>% 
                   select(!!sym(curr_ipv_m0)) %>% 
                   pull(), na.rm = TRUE)

# Standardize the IPV index using the global mean and standard deviation, and add a label
ipv_df_consent_pi <- ipv_df_consent_pi %>%
  mutate(!!sym(curr_ipv_m0) := ifelse(is.na(!!sym(curr_ipv_m0)),NA, ((!!sym(curr_ipv_m0) - mean_glob_m0) / std_glob_m0)) %>%
           structure(label = lab_curr_ipv_m0)) 

####### Missing values imputed replaced by 1 ####################################

# Create the IPV severity index as the mean of selected items, handling missing values
ipv_df_consent_pi <- ipv_df_consent_pi %>%
  # Compute the row-wise mean of the selected IPV variables
  mutate(!!sym(curr_ipv_m1) := apply(select(., paste0(scale_vars,"c")), 1, function(x) {
    x[is.na(x)] <- 1  # Replace NA with 1
    sum(x)             # Compute sum
  })) %>%
  mutate(!!sym(curr_ipv_m1) := !!sym(curr_ipv_m1)/length(scale_vars)) %>%
  # Replace values with 0 if any of the selected variables are 0 for a given row
  mutate(!!sym(curr_ipv_m1) := ifelse(rowSums(select(., all_of(c(paste0(scale_vars, "a"), 
                                                                 paste0(scale_vars, "b")))) == 0, na.rm = TRUE) > 0 & is.na(!!sym(curr_ipv_m1)), 0,!!sym(curr_ipv_m1)))

# Calculate the global mean of the index among the control groups
mean_glob_m1 = mean(ipv_df_consent_pi %>% 
                      filter(treatment_pi_pool == "Control") %>% 
                      select(!!sym(curr_ipv_m1)) %>% 
                      pull(), na.rm = TRUE)

# Calculate the global standard deviation of the index among the control groups
std_glob_m1 = sd(ipv_df_consent_pi %>% 
                   filter(treatment_pi_pool == "Control") %>% 
                   select(!!sym(curr_ipv_m1)) %>% 
                   pull(), na.rm = TRUE)

# Standardize the IPV index using the global mean and standard deviation, and add a label
ipv_df_consent_pi <- ipv_df_consent_pi %>%
  mutate(!!sym(curr_ipv_m1) := ifelse(is.na(!!sym(curr_ipv_m1)),NA, ((!!sym(curr_ipv_m1) - mean_glob_m1) / std_glob_m1)) %>%
           structure(label = lab_curr_ipv_m1)) 

# Comparison table
ipv_df_consent_pi %>%
  select(!!sym(curr_ipv), !!sym(curr_ipv_m0), !!sym(curr_ipv_m1)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  group_by(variable) %>%
  summarise(
    n = n(),
    mean = mean(value, na.rm = TRUE),
    median = median(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    min = min(value, na.rm = TRUE),
    max = max(value, na.rm = TRUE),
    q25 = quantile(value, 0.25, na.rm = TRUE),
    q75 = quantile(value, 0.75, na.rm = TRUE)
  )

##################################################################################
######################## Sexual or Physical violence ############################
##################################################################################

# Define the variables to include in the IPV severity index
scale_vars = c("ipv12","ipv13","ipv14","ipv15","ipv16","ipv17", # Physical 
                             "ipv18","ipv19","ipv20") # Sexual

# Set the name of the new index variable to be created
curr_ipv = "sex_phy_ipv_sev_index"
lab_curr_ipv = "Phy. and sex. IPV severity index"

curr_ipv_m0 = "sex_phy_ipv_sev_index_m0"
lab_curr_ipv_m0 = "Phy. and sex. IPV severity index (missing replaced by 0)"

curr_ipv_m1 = "sex_phy_ipv_sev_index_m1"
lab_curr_ipv_m1 = "Phy. and sex. IPV severity index (missing replaced by 1)"

# Automatisation de la mutation basée sur scale_vars
ipv_df_consent_cash <- ipv_df_consent_cash %>%
  mutate(across(
    .cols = all_of(paste0(scale_vars, "c")),
    .fns = ~ ifelse(get(paste0(cur_column() %>% str_remove("c$"), "b")) == 0, 0, .),
    .names = "{.col}"
  ))

# Automatisation de la mutation basée sur scale_vars
ipv_df_consent_pi <- ipv_df_consent_pi %>%
  mutate(across(
    .cols = all_of(paste0(scale_vars, "c")),
    .fns = ~ ifelse(get(paste0(cur_column() %>% str_remove("c$"), "b")) == 0, 0, .),
    .names = "{.col}"
  ))

######################### Tekavoul (Cash Transfer) ####################################

ipv_df_consent_cash <- ipv_df_consent_cash %>%
  # Compute the row-wise mean of the selected IPV variables
  mutate(sex_phy_ipv_sev_12m = ifelse(sex_ipv_sev_12m==1  |  phy_ipv_sev_12m==1, 1, 
                                      ifelse( sex_ipv_sev_12m==0  |  phy_ipv_sev_12m==0, 0, NA))) %>% 
  mutate(sex_phy_ipv_sev_12m_m0 = ifelse(is.na(sex_phy_ipv_sev_12m),0,sex_phy_ipv_sev_12m),
         sex_phy_ipv_sev_12m_m1 = ifelse(is.na(sex_phy_ipv_sev_12m),1,sex_phy_ipv_sev_12m)
  )

# Create the IPV severity index as the mean of selected items, handling missing values
ipv_df_consent_cash <- ipv_df_consent_cash %>%
  # Compute the row-wise mean of the selected IPV variables
  mutate(!!sym(curr_ipv) := apply(select(., paste0(scale_vars,"c")), 1, sum, na.rm = TRUE)) %>%
  mutate(!!sym(curr_ipv) := !!sym(curr_ipv)/length(scale_vars)) %>%
  # Replace values with 0 if any of the selected variables are 0 for a given row
  mutate(!!sym(curr_ipv) := ifelse(rowSums(select(., all_of(c(paste0(scale_vars, "a"), 
                                                              paste0(scale_vars, "b")))) == 0, na.rm = TRUE) > 0 & is.na(!!sym(curr_ipv)), 0,!!sym(curr_ipv)))

# Transfort to NA the index values that are missing in the regular indicator
ipv_df_consent_cash <- ipv_df_consent_cash %>%
  mutate(!!sym(curr_ipv) := ifelse(is.na(!!sym(paste0(substr(curr_ipv, 1, nchar(curr_ipv) - 5), "12m"))),NA, !!sym(curr_ipv))) 

# Calculate the global mean of the index among the control groups
mean_glob = mean(ipv_df_consent_cash %>% 
                   filter(treatment_csh_trnsfr == "Control") %>% 
                   select(!!sym(curr_ipv)) %>% 
                   pull(), na.rm = TRUE)

# Calculate the global standard deviation of the index among the control groups
std_glob = sd(ipv_df_consent_cash %>% 
                filter(treatment_csh_trnsfr == "Control") %>% 
                select(!!sym(curr_ipv)) %>% 
                pull(), na.rm = TRUE)

# Standardize the IPV index using the global mean and standard deviation, and add a label
ipv_df_consent_cash <- ipv_df_consent_cash %>%
  mutate(!!sym(curr_ipv) := ifelse(is.na(!!sym(curr_ipv)),NA, ((!!sym(curr_ipv) - mean_glob) / std_glob)) %>%
           structure(label = lab_curr_ipv)) 


####### Missing values imputed replaced by 0 ####################################

# Create the IPV severity index as the mean of selected items, handling missing values
ipv_df_consent_cash <- ipv_df_consent_cash %>%
  # Compute the row-wise mean of the selected IPV variables
  mutate(!!sym(curr_ipv_m0) := apply(select(., paste0(scale_vars,"c")), 1, function(x) {
    x[is.na(x)] <- 0  # Replace NA with 0
    sum(x)             # Compute sum
  })) %>%
  mutate(!!sym(curr_ipv_m0) := !!sym(curr_ipv_m0)/length(scale_vars)) %>%
  # Replace values with 0 if any of the selected variables are 0 for a given row
  mutate(!!sym(curr_ipv) := ifelse(rowSums(select(., all_of(c(paste0(scale_vars, "a"), 
                                                              paste0(scale_vars, "b")))) == 0, na.rm = TRUE) > 0 & is.na(!!sym(curr_ipv)), 0,!!sym(curr_ipv)))

# Calculate the global mean of the index among the control groups
mean_glob_m0 = mean(ipv_df_consent_cash %>% 
                      filter(treatment_csh_trnsfr == "Control") %>% 
                      select(!!sym(curr_ipv_m0)) %>% 
                      pull(), na.rm = TRUE)

# Calculate the global standard deviation of the index among the control groups
std_glob_m0 = sd(ipv_df_consent_cash %>% 
                   filter(treatment_csh_trnsfr == "Control") %>% 
                   select(!!sym(curr_ipv_m0)) %>% 
                   pull(), na.rm = TRUE)

# Standardize the IPV index using the global mean and standard deviation, and add a label
ipv_df_consent_cash <- ipv_df_consent_cash %>%
  mutate(!!sym(curr_ipv_m0) := ifelse(is.na(!!sym(curr_ipv_m0)),NA, ((!!sym(curr_ipv_m0) - mean_glob_m0) / std_glob_m0)) %>%
           structure(label = lab_curr_ipv_m0)) 

####### Missing values imputed replaced by 1 ####################################


# Create the IPV severity index as the mean of selected items, handling missing values
ipv_df_consent_cash <- ipv_df_consent_cash %>%
  # Compute the row-wise mean of the selected IPV variables
  mutate(!!sym(curr_ipv_m1) := apply(select(., paste0(scale_vars,"c")), 1, function(x) {
    x[is.na(x)] <- 1  # Replace NA with 0
    sum(x)             # Compute sum
  })) %>%
  mutate(!!sym(curr_ipv_m1) := !!sym(curr_ipv_m1)/length(scale_vars)) %>%
  # Replace values with 0 if any of the selected variables are 0 for a given row
  mutate(!!sym(curr_ipv) := ifelse(rowSums(select(., all_of(c(paste0(scale_vars, "a"), 
                                                              paste0(scale_vars, "b")))) == 0, na.rm = TRUE) > 0 & is.na(!!sym(curr_ipv)), 0,!!sym(curr_ipv)))

# Calculate the global mean of the index among the control groups
mean_glob_m1 = mean(ipv_df_consent_cash %>% 
                      filter(treatment_csh_trnsfr == "Control") %>% 
                      select(!!sym(curr_ipv_m1)) %>% 
                      pull(), na.rm = TRUE)

# Calculate the global standard deviation of the index among the control groups
std_glob_m1 = sd(ipv_df_consent_cash %>% 
                   filter(treatment_csh_trnsfr == "Control") %>% 
                   select(!!sym(curr_ipv_m1)) %>% 
                   pull(), na.rm = TRUE)

# Standardize the IPV index using the global mean and standard deviation, and add a label
ipv_df_consent_cash <- ipv_df_consent_cash %>%
  mutate(!!sym(curr_ipv_m1) := ifelse(is.na(!!sym(curr_ipv_m1)),NA, ((!!sym(curr_ipv_m1) - mean_glob_m1) / std_glob_m1)) %>%
           structure(label = lab_curr_ipv_m1)) 

# Comparison table
ipv_df_consent_cash %>%
  select(!!sym(curr_ipv), !!sym(curr_ipv_m0), !!sym(curr_ipv_m1)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  group_by(variable) %>%
  summarise(
    n = n(),
    mean = mean(value, na.rm = TRUE),
    median = median(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    min = min(value, na.rm = TRUE),
    max = max(value, na.rm = TRUE),
    q25 = quantile(value, 0.25, na.rm = TRUE),
    q75 = quantile(value, 0.75, na.rm = TRUE)
  )

######################### Economic Inclusion ####################################

ipv_df_consent_pi <- ipv_df_consent_pi %>%
  # Compute the row-wise mean of the selected IPV variables
  mutate(sex_phy_ipv_sev_12m = ifelse(sex_ipv_sev_12m==1  |  phy_ipv_sev_12m==1, 1, 0)) %>% 
  mutate(sex_phy_ipv_sev_12m_m0 = ifelse(is.na(sex_phy_ipv_sev_12m),0,sex_phy_ipv_sev_12m),
         sex_phy_ipv_sev_12m_m1 = ifelse(is.na(sex_phy_ipv_sev_12m),1,sex_phy_ipv_sev_12m)
  )

# Create the IPV severity index as the mean of selected items, handling missing values
ipv_df_consent_pi <- ipv_df_consent_pi %>%
  # Compute the row-wise mean of the selected IPV variables
  mutate(!!sym(curr_ipv) := apply(select(., paste0(scale_vars,"c")), 1, sum, na.rm = TRUE)) %>%
  mutate(!!sym(curr_ipv) := !!sym(curr_ipv)/length(scale_vars)) %>%
  # Replace values with 0 if any of the selected variables are 0 for a given row
  mutate(!!sym(curr_ipv) := ifelse(rowSums(select(., all_of(c(paste0(scale_vars, "a"), paste0(scale_vars, "b")))) == 0, na.rm = TRUE) > 0 & is.na(!!sym(curr_ipv)), 0,!!sym(curr_ipv)))

# Transfort to NA the index values that are missing in the regular indicator
ipv_df_consent_pi <- ipv_df_consent_pi %>%
  mutate(!!sym(curr_ipv) := ifelse(is.na(!!sym(paste0(substr(curr_ipv, 1, nchar(curr_ipv) - 5), "12m"))),NA, !!sym(curr_ipv))) 

# Calculate the global mean of the index among the control groups
mean_glob = mean(ipv_df_consent_pi %>% 
                   filter(treatment_pi_pool == "Control") %>% 
                   select(!!sym(curr_ipv)) %>% 
                   pull(), na.rm = TRUE)

# Calculate the global standard deviation of the index among the control groups
std_glob = sd(ipv_df_consent_pi %>% 
                filter(treatment_pi_pool == "Control") %>% 
                select(!!sym(curr_ipv)) %>% 
                pull(), na.rm = TRUE)

# Standardize the IPV index using the global mean and standard deviation, and add a label
ipv_df_consent_pi <- ipv_df_consent_pi %>%
  mutate(!!sym(curr_ipv) := ifelse(is.na(!!sym(curr_ipv)),NA, ((!!sym(curr_ipv) - mean_glob) / std_glob)) %>%
           structure(label = lab_curr_ipv)) 

####### Missing values imputed replaced by 0 ####################################

# Create the IPV severity index as the mean of selected items, handling missing values
ipv_df_consent_pi <- ipv_df_consent_pi %>%
  # Compute the row-wise mean of the selected IPV variables
  mutate(!!sym(curr_ipv_m0) := apply(select(., paste0(scale_vars,"c")), 1, function(x) {
    x[is.na(x)] <- 0  # Replace NA with 0
    sum(x)             # Compute sum
  })) %>%
  mutate(!!sym(curr_ipv_m0) := !!sym(curr_ipv_m0)/length(scale_vars)) %>%
  # Replace values with 0 if any of the selected variables are 0 for a given row
  mutate(!!sym(curr_ipv_m0) := ifelse(rowSums(select(., all_of(c(paste0(scale_vars, "a"), 
                                                                 paste0(scale_vars, "b")))) == 0, na.rm = TRUE) > 0 & is.na(!!sym(curr_ipv_m0)), 0,!!sym(curr_ipv_m0)))

# Calculate the global mean of the index among the control groups
mean_glob_m0 = mean(ipv_df_consent_pi %>% 
                      filter(treatment_pi_pool == "Control") %>% 
                      select(!!sym(curr_ipv_m0)) %>% 
                      pull(), na.rm = TRUE)

# Calculate the global standard deviation of the index among the control groups
std_glob_m0 = sd(ipv_df_consent_pi %>% 
                   filter(treatment_pi_pool == "Control") %>% 
                   select(!!sym(curr_ipv_m0)) %>% 
                   pull(), na.rm = TRUE)

# Standardize the IPV index using the global mean and standard deviation, and add a label
ipv_df_consent_pi <- ipv_df_consent_pi %>%
  mutate(!!sym(curr_ipv_m0) := ifelse(is.na(!!sym(curr_ipv_m0)),NA, ((!!sym(curr_ipv_m0) - mean_glob_m0) / std_glob_m0)) %>%
           structure(label = lab_curr_ipv_m0)) 

####### Missing values imputed replaced by 1 ####################################

# Create the IPV severity index as the mean of selected items, handling missing values
ipv_df_consent_pi <- ipv_df_consent_pi %>%
  # Compute the row-wise mean of the selected IPV variables
  mutate(!!sym(curr_ipv_m1) := apply(select(., paste0(scale_vars,"c")), 1, function(x) {
    x[is.na(x)] <- 1  # Replace NA with 1
    sum(x)             # Compute sum
  })) %>%
  mutate(!!sym(curr_ipv_m1) := !!sym(curr_ipv_m1)/length(scale_vars)) %>%
  # Replace values with 0 if any of the selected variables are 0 for a given row
  mutate(!!sym(curr_ipv_m1) := ifelse(rowSums(select(., all_of(c(paste0(scale_vars, "a"), 
                                                                 paste0(scale_vars, "b")))) == 0, na.rm = TRUE) > 0 & is.na(!!sym(curr_ipv_m1)), 0,!!sym(curr_ipv_m1)))

# Calculate the global mean of the index among the control groups
mean_glob_m1 = mean(ipv_df_consent_pi %>% 
                      filter(treatment_pi_pool == "Control") %>% 
                      select(!!sym(curr_ipv_m1)) %>% 
                      pull(), na.rm = TRUE)

# Calculate the global standard deviation of the index among the control groups
std_glob_m1 = sd(ipv_df_consent_pi %>% 
                   filter(treatment_pi_pool == "Control") %>% 
                   select(!!sym(curr_ipv_m1)) %>% 
                   pull(), na.rm = TRUE)

# Standardize the IPV index using the global mean and standard deviation, and add a label
ipv_df_consent_pi <- ipv_df_consent_pi %>%
  mutate(!!sym(curr_ipv_m1) := ifelse(is.na(!!sym(curr_ipv_m1)),NA, ((!!sym(curr_ipv_m1) - mean_glob_m1) / std_glob_m1)) %>%
           structure(label = lab_curr_ipv_m1)) 

# Comparison table
ipv_df_consent_pi %>%
  select(!!sym(curr_ipv), !!sym(curr_ipv_m0), !!sym(curr_ipv_m1)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  group_by(variable) %>%
  summarise(
    n = n(),
    mean = mean(value, na.rm = TRUE),
    median = median(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    min = min(value, na.rm = TRUE),
    max = max(value, na.rm = TRUE),
    q25 = quantile(value, 0.25, na.rm = TRUE),
    q75 = quantile(value, 0.75, na.rm = TRUE)
  )
##################################################################################
######################## Economic violence #######################################
##################################################################################

# Define the variables to include in the IPV severity index
scale_vars = c("ipv22","ipv23","ipv25")

# Set the name of the new index variable to be created
curr_ipv = "eco_ipv_sev_index"
lab_curr_ipv = "Eco. IPV severity index"

curr_ipv_m0 = "eco_ipv_sev_index_m0"
lab_curr_ipv_m0 = "Eco. IPV severity index (missing replaced by 0)"

curr_ipv_m1 = "eco_ipv_sev_index_m1"
lab_curr_ipv_m1 = "Eco. IPV severity index (missing replaced by 1)"

# Automatisation de la mutation basée sur scale_vars
ipv_df_consent_cash <- ipv_df_consent_cash %>%
  mutate(across(
    .cols = all_of(paste0(scale_vars, "c")),
    .fns = ~ ifelse(get(paste0(cur_column() %>% str_remove("c$"), "b")) == 0, 0, .),
    .names = "{.col}"
  ))

# Automatisation de la mutation basée sur scale_vars
ipv_df_consent_pi <- ipv_df_consent_pi %>%
  mutate(across(
    .cols = all_of(paste0(scale_vars, "c")),
    .fns = ~ ifelse(get(paste0(cur_column() %>% str_remove("c$"), "b")) == 0, 0, .),
    .names = "{.col}"
  ))

######################### Tekavoul (Cash Transfer) ####################################
# Create the IPV severity index as the mean of selected items, handling missing values
ipv_df_consent_cash <- ipv_df_consent_cash %>%
  # Compute the row-wise mean of the selected IPV variables
  mutate(!!sym(curr_ipv) := apply(select(., paste0(scale_vars,"c")), 1, sum, na.rm = TRUE)) %>%
  mutate(!!sym(curr_ipv) := !!sym(curr_ipv)/length(scale_vars)) %>%
  # Replace values with 0 if any of the selected variables are 0 for a given row
  mutate(!!sym(curr_ipv) := ifelse(rowSums(select(., all_of(c(paste0(scale_vars, "a"), paste0(scale_vars, "b")))) == 0, na.rm = TRUE) > 0 & is.na(!!sym(curr_ipv)), 0,!!sym(curr_ipv)))

# Transfort to NA the index values that are missing in the regular indicator
ipv_df_consent_cash <- ipv_df_consent_cash %>%
  mutate(!!sym(curr_ipv) := ifelse(is.na(!!sym(paste0(substr(curr_ipv, 1, nchar(curr_ipv) - 9), "12m"))),NA, !!sym(curr_ipv))) 

# Calculate the global mean of the index among the control groups
mean_glob = mean(ipv_df_consent_cash %>% 
                   filter(treatment_csh_trnsfr == "Control") %>% 
                   select(!!sym(curr_ipv)) %>% 
                   pull(), na.rm = TRUE)

# Calculate the global standard deviation of the index among the control groups
std_glob = sd(ipv_df_consent_cash %>% 
                filter(treatment_csh_trnsfr == "Control") %>% 
                select(!!sym(curr_ipv)) %>% 
                pull(), na.rm = TRUE)

# Standardize the IPV index using the global mean and standard deviation, and add a label
ipv_df_consent_cash <- ipv_df_consent_cash %>%
  mutate(!!sym(curr_ipv) := ifelse(is.na(!!sym(curr_ipv)),NA, ((!!sym(curr_ipv) - mean_glob) / std_glob)) %>%
           structure(label = lab_curr_ipv)) %>%
  # Rename an existing variable (to make sure the source name is correct)
  rename(eco_ipv_sev_12m = eco_ipv_12m)%>% 
  mutate(eco_ipv_sev_12m_m0 = ifelse(is.na(eco_ipv_sev_12m),0,eco_ipv_sev_12m),
         eco_ipv_sev_12m_m1 = ifelse(is.na(eco_ipv_sev_12m),1,eco_ipv_sev_12m)
  )


####### Missing values imputed replaced by 0 ####################################

# Create the IPV severity index as the mean of selected items, handling missing values
ipv_df_consent_cash <- ipv_df_consent_cash %>%
  # Compute the row-wise mean of the selected IPV variables
  mutate(!!sym(curr_ipv_m0) := apply(select(., paste0(scale_vars,"c")), 1, function(x) {
    x[is.na(x)] <- 0  # Replace NA with 0
    sum(x)             # Compute sum
  })) %>%
  mutate(!!sym(curr_ipv_m0) := !!sym(curr_ipv_m0)/length(scale_vars)) %>%
  # Replace values with NA if all selected variables are NA for a given row
  mutate(!!sym(curr_ipv_m0) := ifelse(rowSums(select(., all_of(c(paste0(scale_vars, "a"), paste0(scale_vars, "b")))) == 0, na.rm = TRUE) > 0 & is.na(!!sym(curr_ipv_m0)), 0,!!sym(curr_ipv_m0)))

# Calculate the global mean of the index among the control groups
mean_glob_m0 = mean(ipv_df_consent_cash %>% 
                      filter(treatment_csh_trnsfr == "Control") %>% 
                      select(!!sym(curr_ipv_m0)) %>% 
                      pull(), na.rm = TRUE)

# Calculate the global standard deviation of the index among the control groups
std_glob_m0 = sd(ipv_df_consent_cash %>% 
                   filter(treatment_csh_trnsfr == "Control") %>% 
                   select(!!sym(curr_ipv_m0)) %>% 
                   pull(), na.rm = TRUE)

# Standardize the IPV index using the global mean and standard deviation, and add a label
ipv_df_consent_cash <- ipv_df_consent_cash %>%
  mutate(!!sym(curr_ipv_m0) := ifelse(is.na(!!sym(curr_ipv_m0)),NA, ((!!sym(curr_ipv_m0) - mean_glob_m0) / std_glob_m0)) %>%
           structure(label = lab_curr_ipv_m0)) 

####### Missing values imputed replaced by 1 ####################################

# Create the IPV severity index as the mean of selected items, handling missing values
ipv_df_consent_cash <- ipv_df_consent_cash %>%
  # Compute the row-wise mean of the selected IPV variables
  mutate(!!sym(curr_ipv_m1) := apply(select(., paste0(scale_vars,"c")), 1, function(x) {
    x[is.na(x)] <- 1  # Replace NA with 0
    sum(x)             # Compute sum
  })) %>%
  mutate(!!sym(curr_ipv_m1) := !!sym(curr_ipv_m1)/length(scale_vars)) %>%
  # Replace values with NA if all selected variables are NA for a given row
  mutate(!!sym(curr_ipv_m1) := ifelse(rowSums(select(., all_of(c(paste0(scale_vars, "a"), paste0(scale_vars, "b")))) == 0, na.rm = TRUE) > 0 & is.na(!!sym(curr_ipv_m1)), 0,!!sym(curr_ipv_m1)))

# Calculate the global mean of the index among the control groups
mean_glob_m1 = mean(ipv_df_consent_cash %>% 
                      filter(treatment_csh_trnsfr == "Control") %>% 
                      select(!!sym(curr_ipv_m1)) %>% 
                      pull(), na.rm = TRUE)

# Calculate the global standard deviation of the index among the control groups
std_glob_m1 = sd(ipv_df_consent_cash %>% 
                   filter(treatment_csh_trnsfr == "Control") %>% 
                   select(!!sym(curr_ipv_m1)) %>% 
                   pull(), na.rm = TRUE)

# Standardize the IPV index using the global mean and standard deviation, and add a label
ipv_df_consent_cash <- ipv_df_consent_cash %>%
  mutate(!!sym(curr_ipv_m1) := ifelse(is.na(!!sym(curr_ipv_m1)),NA, ((!!sym(curr_ipv_m1) - mean_glob_m1) / std_glob_m1)) %>%
           structure(label = lab_curr_ipv_m1)) 

# Comparison table
ipv_df_consent_cash %>%
  select(!!sym(curr_ipv), !!sym(curr_ipv_m0), !!sym(curr_ipv_m1)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  group_by(variable) %>%
  summarise(
    n = n(),
    mean = mean(value, na.rm = TRUE),
    median = median(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    min = min(value, na.rm = TRUE),
    max = max(value, na.rm = TRUE),
    q25 = quantile(value, 0.25, na.rm = TRUE),
    q75 = quantile(value, 0.75, na.rm = TRUE)
  )

######################### Economic Inclusion ####################################
# Create the IPV severity index as the mean of selected items, handling missing values
ipv_df_consent_pi <- ipv_df_consent_pi %>%
  # Compute the row-wise mean of the selected IPV variables
  mutate(!!sym(curr_ipv) := apply(select(., paste0(scale_vars,"c")), 1, sum, na.rm = TRUE)) %>%
  mutate(!!sym(curr_ipv) := !!sym(curr_ipv)/length(scale_vars)) %>%
  # Replace values with 0 if any of the selected variables are 0 for a given row
  mutate(!!sym(curr_ipv) := ifelse(rowSums(select(., all_of(c(paste0(scale_vars, "a"), paste0(scale_vars, "b")))) == 0, na.rm = TRUE) > 0 & is.na(!!sym(curr_ipv)), 0,!!sym(curr_ipv)))

# Transfort to NA the index values that are missing in the regular indicator
ipv_df_consent_pi <- ipv_df_consent_pi %>%
  mutate(!!sym(curr_ipv) := ifelse(is.na(!!sym(paste0(substr(curr_ipv, 1, nchar(curr_ipv) - 9), "12m"))),NA, !!sym(curr_ipv))) 

# Calculate the global mean of the index among the control groups
mean_glob = mean(ipv_df_consent_pi %>% 
                   filter(treatment_pi_pool == "Control") %>% 
                   select(!!sym(curr_ipv)) %>% 
                   pull(), na.rm = TRUE)

# Calculate the global standard deviation of the index among the control groups
std_glob = sd(ipv_df_consent_pi %>% 
                filter(treatment_pi_pool == "Control") %>% 
                select(!!sym(curr_ipv)) %>% 
                pull(), na.rm = TRUE)

# Standardize the IPV index using the global mean and standard deviation, and add a label
ipv_df_consent_pi <- ipv_df_consent_pi %>%
  mutate(!!sym(curr_ipv) := ifelse(is.na(!!sym(curr_ipv)),NA, ((!!sym(curr_ipv) - mean_glob) / std_glob)) %>%
           structure(label = lab_curr_ipv)) %>%
  # Rename an existing variable (to make sure the source name is correct)
  rename(eco_ipv_sev_12m = eco_ipv_12m)%>% 
  mutate(eco_ipv_sev_12m_m0 = ifelse(is.na(eco_ipv_sev_12m),0,eco_ipv_sev_12m),
         eco_ipv_sev_12m_m1 = ifelse(is.na(eco_ipv_sev_12m),1,eco_ipv_sev_12m)
  )


####### Missing values imputed replaced by 0 ####################################

# Create the IPV severity index as the mean of selected items, handling missing values
ipv_df_consent_pi <- ipv_df_consent_pi %>%
  # Compute the row-wise mean of the selected IPV variables
  mutate(!!sym(curr_ipv_m0) := apply(select(., paste0(scale_vars,"c")), 1, function(x) {
    x[is.na(x)] <- 0  # Replace NA with 0
    sum(x)             # Compute sum
  })) %>%
  mutate(!!sym(curr_ipv_m0) := !!sym(curr_ipv_m0)/length(scale_vars)) %>%
  # Replace values with 0 if any of the selected variables are 0 for a given row
  mutate(!!sym(curr_ipv) := ifelse(rowSums(select(., all_of(c(paste0(scale_vars, "a"), paste0(scale_vars, "b")))) == 0, na.rm = TRUE) > 0 & is.na(!!sym(curr_ipv)), 0,!!sym(curr_ipv)))

# Calculate the global mean of the index among the control groups
mean_glob_m0 = mean(ipv_df_consent_pi %>% 
                      filter(treatment_pi_pool == "Control") %>% 
                      select(!!sym(curr_ipv_m0)) %>% 
                      pull(), na.rm = TRUE)

# Calculate the global standard deviation of the index among the control groups
std_glob_m0 = sd(ipv_df_consent_pi %>% 
                   filter(treatment_pi_pool == "Control") %>% 
                   select(!!sym(curr_ipv_m0)) %>% 
                   pull(), na.rm = TRUE)

# Standardize the IPV index using the global mean and standard deviation, and add a label
ipv_df_consent_pi <- ipv_df_consent_pi %>%
  mutate(!!sym(curr_ipv_m0) := ifelse(is.na(!!sym(curr_ipv_m0)),NA, ((!!sym(curr_ipv_m0) - mean_glob_m0) / std_glob_m0)) %>%
           structure(label = lab_curr_ipv_m0)) 

####### Missing values imputed replaced by 1 ####################################

# Create the IPV severity index as the mean of selected items, handling missing values
ipv_df_consent_pi <- ipv_df_consent_pi %>%
  # Compute the row-wise mean of the selected IPV variables
  mutate(!!sym(curr_ipv_m1) := apply(select(., paste0(scale_vars,"c")), 1, function(x) {
    x[is.na(x)] <- 1  # Replace NA with 1
    sum(x)             # Compute sum
  })) %>%
  mutate(!!sym(curr_ipv_m1) := !!sym(curr_ipv_m1)/length(scale_vars)) %>%
  # Replace values with 0 if any of the selected variables are 0 for a given row
  mutate(!!sym(curr_ipv) := ifelse(rowSums(select(., all_of(c(paste0(scale_vars, "a"), paste0(scale_vars, "b")))) == 0, na.rm = TRUE) > 0 & is.na(!!sym(curr_ipv)), 0,!!sym(curr_ipv)))

# Calculate the global mean of the index among the control groups
mean_glob_m1 = mean(ipv_df_consent_pi %>% 
                      filter(treatment_pi_pool == "Control") %>% 
                      select(!!sym(curr_ipv_m1)) %>% 
                      pull(), na.rm = TRUE)

# Calculate the global standard deviation of the index among the control groups
std_glob_m1 = sd(ipv_df_consent_pi %>% 
                   filter(treatment_pi_pool == "Control") %>% 
                   select(!!sym(curr_ipv_m1)) %>% 
                   pull(), na.rm = TRUE)

# Standardize the IPV index using the global mean and standard deviation, and add a label
ipv_df_consent_pi <- ipv_df_consent_pi %>%
  mutate(!!sym(curr_ipv_m1) := ifelse(is.na(!!sym(curr_ipv_m1)),NA, ((!!sym(curr_ipv_m1) - mean_glob_m1) / std_glob_m1)) %>%
           structure(label = lab_curr_ipv_m1)) 

# Comparison table
ipv_df_consent_pi %>%
  select(!!sym(curr_ipv), !!sym(curr_ipv_m0), !!sym(curr_ipv_m1)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  group_by(variable) %>%
  summarise(
    n = n(),
    mean = mean(value, na.rm = TRUE),
    median = median(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    min = min(value, na.rm = TRUE),
    max = max(value, na.rm = TRUE),
    q25 = quantile(value, 0.25, na.rm = TRUE),
    q75 = quantile(value, 0.75, na.rm = TRUE)
  )
################################################################################
######################## All type of violence ##################################
################################################################################


# Define the variables to include in the IPV severity index
scale_vars = c("ipv8","ipv9","ipv10",
               "ipv11","ipv12","ipv13","ipv14","ipv15","ipv16","ipv17",
               "ipv18","ipv19","ipv20")

# Set the name of the new index variable to be created
curr_ipv = "all_ipv_sev_index"
lab_curr_ipv = "All type of IPV (excluding Eco. and control) severity index"

curr_ipv_m0 = "all_ipv_sev_index_m0"
lab_curr_ipv_m0 = "All type of IPV (excluding Eco. and control) severity index (missing replaced by 0)"

curr_ipv_m1 = "all_ipv_sev_index_m1"
lab_curr_ipv_m1 = "All type of IPV (excluding Eco. and control) severity index (missing replaced by 1)"

# Automatisation de la mutation basée sur scale_vars
ipv_df_consent_cash <- ipv_df_consent_cash %>%
  mutate(across(
    .cols = all_of(paste0(scale_vars, "c")),
    .fns = ~ ifelse(get(paste0(cur_column() %>% str_remove("c$"), "b")) == 0, 0, .),
    .names = "{.col}"
  ))

# Automatisation de la mutation basée sur scale_vars
ipv_df_consent_pi <- ipv_df_consent_pi %>%
  mutate(across(
    .cols = all_of(paste0(scale_vars, "c")),
    .fns = ~ ifelse(get(paste0(cur_column() %>% str_remove("c$"), "b")) == 0, 0, .),
    .names = "{.col}"
  ))

######################### Tekavoul (Cash Transfer) ####################################
# Create the IPV severity index as the mean of selected items, handling missing values
ipv_df_consent_cash <- ipv_df_consent_cash %>%
  # Compute the row-wise mean of the selected IPV variables
  mutate(!!sym(curr_ipv) := apply(select(., paste0(scale_vars,"c")), 1, sum, na.rm = TRUE)) %>%
  mutate(!!sym(curr_ipv) := !!sym(curr_ipv)/length(scale_vars)) %>%
  # Replace values with 0 if any of the selected variables are 0 for a given row
  mutate(!!sym(curr_ipv) := ifelse(rowSums(select(., all_of(c(paste0(scale_vars, "a"), paste0(scale_vars, "b")))) == 0, na.rm = TRUE) > 0 & is.na(!!sym(curr_ipv)), 0,!!sym(curr_ipv)))

# Transfort to NA the index values that are missing in the regular indicator
ipv_df_consent_cash <- ipv_df_consent_cash %>%
  mutate(!!sym(curr_ipv) := ifelse(is.na(ipv_all_12m),NA, !!sym(curr_ipv))) 

# Calculate the global mean of the index among the control groups
mean_glob = mean(ipv_df_consent_cash %>% 
                   filter(treatment_csh_trnsfr == "Control") %>% 
                   select(!!sym(curr_ipv)) %>% 
                   pull(), na.rm = TRUE)

# Calculate the global standard deviation of the index among the control groups
std_glob = sd(ipv_df_consent_cash %>% 
                filter(treatment_csh_trnsfr == "Control") %>% 
                select(!!sym(curr_ipv)) %>% 
                pull(), na.rm = TRUE)

# Standardize the IPV index using the global mean and standard deviation, and add a label
ipv_df_consent_cash <- ipv_df_consent_cash %>%
  mutate(!!sym(curr_ipv) := ifelse(is.na(!!sym(curr_ipv)),NA, ((!!sym(curr_ipv) - mean_glob) / std_glob)) %>%
           structure(label = lab_curr_ipv)) %>%
  # Rename an existing variable (to make sure the source name is correct)
  rename(all_ipv_sev_12m = ipv_all_12m)%>% 
  mutate(all_ipv_sev_12m_m0 = ifelse(is.na(all_ipv_sev_12m),0,all_ipv_sev_12m),
         all_ipv_sev_12m_m1 = ifelse(is.na(all_ipv_sev_12m),1,all_ipv_sev_12m)
  )



# Age at marriage 
ipv_df_consent_cash <- ipv_df_consent_cash %>%
  mutate(age_at_mrrg = as.numeric(mr2))

ipv_df_consent_cash <- ipv_df_consent_cash %>%
  mutate(age_at_mrrg_above18 = ifelse(age_at_mrrg > 18, 1, 0))

ipv_df_consent_cash <- ipv_df_consent_cash %>%
  mutate(het_age_at_mrrg_above18 = as.factor(age_at_mrrg_above18))


####### Missing values imputed replaced by 0 ####################################

# Create the IPV severity index as the mean of selected items, handling missing values
ipv_df_consent_cash <- ipv_df_consent_cash %>%
  # Compute the row-wise mean of the selected IPV variables
  mutate(!!sym(curr_ipv_m0) := apply(select(., paste0(scale_vars,"c")), 1, function(x) {
    x[is.na(x)] <- 0  # Replace NA with 0
    sum(x)             # Compute sum
  })) %>%
  mutate(!!sym(curr_ipv_m0) := !!sym(curr_ipv_m0)/length(scale_vars)) %>%
  # Replace values with NA if all selected variables are NA for a given row
  mutate(!!sym(curr_ipv) := ifelse(rowSums(select(., all_of(c(paste0(scale_vars, "a"), paste0(scale_vars, "b")))) == 0, na.rm = TRUE) > 0 & is.na(!!sym(curr_ipv)), 0,!!sym(curr_ipv)))

# Calculate the global mean of the index among the control groups
mean_glob_m0 = mean(ipv_df_consent_cash %>% 
                      filter(treatment_csh_trnsfr == "Control") %>% 
                      select(!!sym(curr_ipv_m0)) %>% 
                      pull(), na.rm = TRUE)

# Calculate the global standard deviation of the index among the control groups
std_glob_m0 = sd(ipv_df_consent_cash %>% 
                   filter(treatment_csh_trnsfr == "Control") %>% 
                   select(!!sym(curr_ipv_m0)) %>% 
                   pull(), na.rm = TRUE)

# Standardize the IPV index using the global mean and standard deviation, and add a label
ipv_df_consent_cash <- ipv_df_consent_cash %>%
  mutate(!!sym(curr_ipv_m0) := ifelse(is.na(!!sym(curr_ipv_m0)),NA, ((!!sym(curr_ipv_m0) - mean_glob_m0) / std_glob_m0)) %>%
           structure(label = lab_curr_ipv_m0)) 

####### Missing values imputed replaced by 1 ####################################

# Create the IPV severity index as the mean of selected items, handling missing values
ipv_df_consent_cash <- ipv_df_consent_cash %>%
  # Compute the row-wise mean of the selected IPV variables
  mutate(!!sym(curr_ipv_m1) := apply(select(., paste0(scale_vars,"c")), 1, function(x) {
    x[is.na(x)] <- 1  # Replace NA with 1
    sum(x)             # Compute sum
  })) %>%
  mutate(!!sym(curr_ipv_m1) := !!sym(curr_ipv_m1)/length(scale_vars)) %>%
  # Replace values with NA if all selected variables are NA for a given row
  mutate(!!sym(curr_ipv) := ifelse(rowSums(select(., all_of(c(paste0(scale_vars, "a"), paste0(scale_vars, "b")))) == 0, na.rm = TRUE) > 0 & is.na(!!sym(curr_ipv)), 0,!!sym(curr_ipv)))

# Calculate the global mean of the index among the control groups
mean_glob_m1 = mean(ipv_df_consent_cash %>% 
                      filter(treatment_csh_trnsfr == "Control") %>% 
                      select(!!sym(curr_ipv_m1)) %>% 
                      pull(), na.rm = TRUE)

# Calculate the global standard deviation of the index among the control groups
std_glob_m1 = sd(ipv_df_consent_cash %>% 
                   filter(treatment_csh_trnsfr == "Control") %>% 
                   select(!!sym(curr_ipv_m1)) %>% 
                   pull(), na.rm = TRUE)

# Standardize the IPV index using the global mean and standard deviation, and add a label
ipv_df_consent_cash <- ipv_df_consent_cash %>%
  mutate(!!sym(curr_ipv_m1) := ifelse(is.na(!!sym(curr_ipv_m1)),NA, ((!!sym(curr_ipv_m1) - mean_glob_m1) / std_glob_m1)) %>%
           structure(label = lab_curr_ipv_m1)) 

# Comparison table
ipv_df_consent_cash %>%
  select(!!sym(curr_ipv), !!sym(curr_ipv_m0), !!sym(curr_ipv_m1)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  group_by(variable) %>%
  summarise(
    n = n(),
    mean = mean(value, na.rm = TRUE),
    median = median(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    min = min(value, na.rm = TRUE),
    max = max(value, na.rm = TRUE),
    q25 = quantile(value, 0.25, na.rm = TRUE),
    q75 = quantile(value, 0.75, na.rm = TRUE)
  )

######################### Economic Inclusion ####################################
# Create the IPV severity index as the mean of selected items, handling missing values
ipv_df_consent_pi <- ipv_df_consent_pi %>%
  # Compute the row-wise mean of the selected IPV variables
  mutate(!!sym(curr_ipv) := apply(select(., paste0(scale_vars,"c")), 1, sum, na.rm = TRUE)) %>%
  mutate(!!sym(curr_ipv) := !!sym(curr_ipv)/length(scale_vars)) %>%
  # Replace values with 0 if any of the selected variables are 0 for a given row
  mutate(!!sym(curr_ipv) := ifelse(rowSums(select(., all_of(c(paste0(scale_vars, "a"), paste0(scale_vars, "b")))) == 0, na.rm = TRUE) > 0 & is.na(!!sym(curr_ipv)), 0,!!sym(curr_ipv)))

# Transfort to NA the index values that are missing in the regular indicator
ipv_df_consent_pi <- ipv_df_consent_pi %>%
  mutate(!!sym(curr_ipv) := ifelse(is.na(ipv_all_12m),NA, !!sym(curr_ipv))) 

# Calculate the global mean of the index among the control groups
mean_glob = mean(ipv_df_consent_pi %>% 
                   filter(treatment_pi_pool == "Control") %>% 
                   select(!!sym(curr_ipv)) %>% 
                   pull(), na.rm = TRUE)

# Calculate the global standard deviation of the index among the control groups
std_glob = sd(ipv_df_consent_pi %>% 
                filter(treatment_pi_pool == "Control") %>% 
                select(!!sym(curr_ipv)) %>% 
                pull(), na.rm = TRUE)

# Standardize the IPV index using the global mean and standard deviation, and add a label
ipv_df_consent_pi <- ipv_df_consent_pi %>%
  mutate(!!sym(curr_ipv) := ifelse(is.na(!!sym(curr_ipv)),NA, ((!!sym(curr_ipv) - mean_glob) / std_glob)) %>%
           structure(label = lab_curr_ipv)) %>%
  # Rename an existing variable (to make sure the source name is correct)
  rename(all_ipv_sev_12m = ipv_all_12m)%>% 
  mutate(all_ipv_sev_12m_m0 = ifelse(is.na(all_ipv_sev_12m),0,all_ipv_sev_12m),
         all_ipv_sev_12m_m1 = ifelse(is.na(all_ipv_sev_12m),1,all_ipv_sev_12m)
  )

####### Missing values imputed replaced by 0 ####################################

# Create the IPV severity index as the mean of selected items, handling missing values
ipv_df_consent_pi <- ipv_df_consent_pi %>%
  # Compute the row-wise mean of the selected IPV variables
  mutate(!!sym(curr_ipv_m0) := apply(select(., paste0(scale_vars,"c")), 1, function(x) {
    x[is.na(x)] <- 0  # Replace NA with 0
    sum(x)             # Compute sum
  })) %>%
  mutate(!!sym(curr_ipv_m0) := !!sym(curr_ipv_m0)/length(scale_vars)) %>%
  # Replace values with NA if all selected variables are NA for a given row
  mutate(!!sym(curr_ipv) := ifelse(rowSums(select(., all_of(c(paste0(scale_vars, "a"), paste0(scale_vars, "b")))) == 0, na.rm = TRUE) > 0 & is.na(!!sym(curr_ipv)), 0,!!sym(curr_ipv)))

# Calculate the global mean of the index among the control groups
mean_glob_m0 = mean(ipv_df_consent_pi %>% 
                      filter(treatment_pi_pool == "Control") %>% 
                      select(!!sym(curr_ipv_m0)) %>% 
                      pull(), na.rm = TRUE)

# Calculate the global standard deviation of the index among the control groups
std_glob_m0 = sd(ipv_df_consent_pi %>% 
                   filter(treatment_pi_pool == "Control") %>% 
                   select(!!sym(curr_ipv_m0)) %>% 
                   pull(), na.rm = TRUE)

# Standardize the IPV index using the global mean and standard deviation, and add a label
ipv_df_consent_pi <- ipv_df_consent_pi %>%
  mutate(!!sym(curr_ipv_m0) := ifelse(is.na(!!sym(curr_ipv_m0)),NA, ((!!sym(curr_ipv_m0) - mean_glob_m0) / std_glob_m0)) %>%
           structure(label = lab_curr_ipv_m0)) 

####### Missing values imputed replaced by 1 ####################################

# Create the IPV severity index as the mean of selected items, handling missing values
ipv_df_consent_pi <- ipv_df_consent_pi %>%
  # Compute the row-wise mean of the selected IPV variables
  mutate(!!sym(curr_ipv_m1) := apply(select(., paste0(scale_vars,"c")), 1, function(x) {
    x[is.na(x)] <- 1  # Replace NA with 1
    sum(x)             # Compute sum
  })) %>%
  mutate(!!sym(curr_ipv_m1) := !!sym(curr_ipv_m1)/length(scale_vars)) %>%
  # Replace values with NA if all selected variables are NA for a given row
  mutate(!!sym(curr_ipv) := ifelse(rowSums(select(., all_of(c(paste0(scale_vars, "a"), paste0(scale_vars, "b")))) == 0, na.rm = TRUE) > 0 & is.na(!!sym(curr_ipv)), 0,!!sym(curr_ipv)))

# Calculate the global mean of the index among the control groups
mean_glob_m1 = mean(ipv_df_consent_pi %>% 
                      filter(treatment_pi_pool == "Control") %>% 
                      select(!!sym(curr_ipv_m1)) %>% 
                      pull(), na.rm = TRUE)

# Calculate the global standard deviation of the index among the control groups
std_glob_m1 = sd(ipv_df_consent_pi %>% 
                   filter(treatment_pi_pool == "Control") %>% 
                   select(!!sym(curr_ipv_m1)) %>% 
                   pull(), na.rm = TRUE)

# Standardize the IPV index using the global mean and standard deviation, and add a label
ipv_df_consent_pi <- ipv_df_consent_pi %>%
  mutate(!!sym(curr_ipv_m1) := ifelse(is.na(!!sym(curr_ipv_m1)),NA, ((!!sym(curr_ipv_m1) - mean_glob_m1) / std_glob_m1)) %>%
           structure(label = lab_curr_ipv_m1)) 

# Comparison table
ipv_df_consent_pi %>%
  select(!!sym(curr_ipv), !!sym(curr_ipv_m0), !!sym(curr_ipv_m1)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  group_by(variable) %>%
  summarise(
    n = n(),
    mean = mean(value, na.rm = TRUE),
    median = median(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    min = min(value, na.rm = TRUE),
    max = max(value, na.rm = TRUE),
    q25 = quantile(value, 0.25, na.rm = TRUE),
    q75 = quantile(value, 0.75, na.rm = TRUE)
  )

# Age at marriage 
ipv_df_consent_pi <- ipv_df_consent_pi %>%
  mutate(age_at_mrrg = as.numeric(mr2))

ipv_df_consent_pi <- ipv_df_consent_pi %>%
  mutate(age_at_mrrg_above18 = ifelse(age_at_mrrg > 18, 1, 0))

ipv_df_consent_pi <- ipv_df_consent_pi %>%
  mutate(het_age_at_mrrg_above18 = as.factor(age_at_mrrg_above18))

# Renaming the age
ipv_df_consent <- ipv_df_consent %>%
  rename(age_ipv_module = age) %>% 
  mutate(age_ipv_module = age_ipv_module %>%
           structure(label = "Age recorded in the IPV module")) 



################################################################################
######################## Variables for heterogeneity ###########################
################################################################################
# Filter for phase 2 data (follow-up)
followup_MRT_hh <- allrounds_MRT_hh %>% 
  filter(phase == 2) 

# ------------------------------------------------------------------------------
# Heterogeneity variables based on baseline characteristics
# ------------------------------------------------------------------------------

# Beneficiary and partner characteristics
## Education
followup_MRT_hh <- followup_MRT_hh %>% 
  mutate(prtnr_edu = ifelse(hhh_fem_bl==0, hhh_edu_bl, pben_edu_bl)) %>%   
  mutate(het_prtnr_edu = as.factor(ifelse(prtnr_edu > median(prtnr_edu, na.rm = TRUE), 1, 0)))


## Litteracy
followup_MRT_hh <- followup_MRT_hh %>% 
  mutate(prtnr_lit = ifelse(hhh_fem_bl==0, hhh_lit_bl, pben_lit_bl)) %>%   
  mutate(het_prtnr_lit = as.factor(prtnr_lit))

## Age gap between partner and beneficiary (continuous and binary split)
followup_MRT_hh <- followup_MRT_hh %>%
  mutate(age_prtnr =  ifelse(hhh_fem_bl==0, hhh_age_bl, pben_age_bl)) %>% 
  mutate(age_gap = as.numeric(age_prtnr - pben_age_bl))

### Age gap above/below median (follow-up)
followup_MRT_hh <- followup_MRT_hh %>%
  mutate(het_age_gap = as.factor(ifelse(age_gap > median(age_gap, na.rm = TRUE), 1, 0)))


# Household head (HHH) is female
followup_MRT_hh <- followup_MRT_hh %>% 
  mutate(het_hhh_fem = as.factor(hhh_fem_bl),      # Binary heterogeneity variable
         hhh_fem = as.numeric(hhh_fem_bl))         # Numeric version for continuous analysis


# HHH is literate
followup_MRT_hh <- followup_MRT_hh %>% 
  mutate(het_hhh_lit = as.factor(hhh_lit_bl),
         hhh_lit = as.numeric(hhh_lit_bl))

# Primary beneficiary is literate
followup_MRT_hh <- followup_MRT_hh %>% 
  mutate(het_pben_lit = as.factor(pben_lit_bl),
         pben_lit = as.numeric(pben_lit_bl))

# Share of beneficiary's savings owned by partner
followup_MRT_hh <- followup_MRT_hh %>% 
  mutate(het_save_share = as.factor(save_share_d),
         save_share = as.numeric(save_share_d))

# HHH years of education (above/below median split)
followup_MRT_hh <- followup_MRT_hh %>% 
  mutate(het_hhh_edu = as.factor(ifelse(hhh_edu_bl > median(hhh_edu_bl, na.rm = TRUE), 1, 0)),
         hhh_edu = as.numeric(hhh_edu_bl))

# Beneficiary years of education (above/below median split)
followup_MRT_hh <- followup_MRT_hh %>% 
  mutate(het_pben_edu = as.factor(ifelse(pben_edu_bl > median(pben_edu_bl, na.rm = TRUE), 1, 0)),
         pben_edu = as.numeric(pben_edu_bl))

# Control over earnings index (above/below median)
followup_MRT_hh <- followup_MRT_hh %>% 
  mutate(het_ctrl_earn_index = as.factor(ifelse(ctrl_earn_index_tr_bl > median(ctrl_earn_index_tr_bl, na.rm = TRUE), 1, 0)),
         ctrl_earn_index = as.numeric(ctrl_earn_index_tr_bl))

# Control over household resources index (above/below median)
followup_MRT_hh <- followup_MRT_hh %>% 
  mutate(het_ctrl_hh_index = as.factor(ifelse(ctrl_hh_index_tr_bl > median(ctrl_hh_index_tr_bl, na.rm = TRUE), 1, 0)),
         ctrl_hh_index = as.numeric(ctrl_hh_index_tr_bl))

# Intra-household dynamics index (above/below median)
followup_MRT_hh <- followup_MRT_hh %>% 
  mutate(het_intrahh_index = as.factor(ifelse(intrahh_vars_index_tr_bl > median(intrahh_vars_index_tr_bl, na.rm = TRUE), 1, 0)),
         intrahh_index = as.numeric(intrahh_vars_index_tr_bl))


# Beneficiary share of total household revenue (above/below median)
followup_MRT_hh <- followup_MRT_hh %>% 
  mutate(het_rev_sum_bohh = as.factor(ifelse(rev_sum_bohh_wemp_98_tr_bl > median(rev_sum_bohh_wemp_98_tr_bl, na.rm = TRUE), 1, 0)),
         rev_sum_bohh = as.numeric(rev_sum_bohh_wemp_98_tr_bl))

# Beneficiary has a disability
followup_MRT_hh <- followup_MRT_hh %>% 
  mutate(het_pben_handicap = as.factor(pben_handicap_bl),
         pben_handicap = as.numeric(pben_handicap_bl))

# Beneficiary is in a polygamous union
followup_MRT_hh <- followup_MRT_hh %>% 
  mutate(het_pben_poly = as.factor(pben_poly_bl),
         pben_poly = as.numeric(pben_poly_bl))

# Age gap between HHH and beneficiary (continuous and binary split)
# Follow-up
# followup_MRT_hh <- followup_MRT_hh %>%
#   mutate(age_gap = as.numeric(hhh_age_bl - pben_age_bl))
# 
# # Baseline
# baseline_MRT_hh <- baseline_MRT_hh %>%
#   mutate(age_gap = as.numeric(hhh_age - pben_age))

# Age gap above/below median (follow-up)
followup_MRT_hh <- followup_MRT_hh %>%
  mutate(het_age_gap = as.factor(ifelse(age_gap > median(age_gap, na.rm = TRUE), 1, 0)))

# Region at baseline (Selibaby ==1 and barkeol ==0)
followup_MRT_hh <- followup_MRT_hh %>%
  mutate(het_region = ifelse(region==2, 0, 1))

# Gap total baseline revenue (follow-up)
followup_MRT_hh <- followup_MRT_hh %>%
  mutate(tot_hh_rev = as.numeric(tot_hh_rev12_98_ppp_bl))

followup_MRT_hh <- followup_MRT_hh %>%
  mutate(het_married_ben =as.numeric(married_ben_bl))

# Consumption at baseline
followup_MRT_hh <- followup_MRT_hh %>%
  mutate(consum = as.numeric(consum_2_day_eq_ppp_tr_bl))

followup_MRT_hh <- followup_MRT_hh %>%
  mutate(het_consum = as.factor(ifelse(consum_2_day_eq_ppp_tr_bl > median(consum_2_day_eq_ppp_tr_bl, na.rm = TRUE), 1, 0)))

# Baseline decision making index

# 1. Define the components (from your Stata list)
dec_vars <- c("dec_pow_earn_bl", "dec_pow_spend_bl", "dec_pow_large_bl", "dec_pow_fert_bl", 
              "dec_pow_edu_bl")

# 1. Prepare the data including the hhid
df_for_pca <- followup_MRT_hh %>%
  select(hhid, all_of(dec_vars)) %>%
  # Create binary versions (1 = Matters a lot/little, 0 = Not at all)
  mutate(across(all_of(dec_vars), 
                ~ ifelse(. >= 2, 1, 0), 
                .names = "{.col}_bin")) %>%
  # Replace all NA values in the binary columns with 0
  mutate(across(ends_with("_bin"), 
                ~ replace_na(., 0)))

# 2. Run PCA on the binary columns only
# We exclude the 'hhid' column from the PCA calculation using select(-hhid)
pca_input_matrix <- df_for_pca %>% select(ends_with("_bin"))
pca_result <- PCA(pca_input_matrix, graph = FALSE)

factoextra::fviz_pca_var(pca_result,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
             
# 3. Add the Index score back to the filtered dataframe
df_for_pca$bp_index <- pca_result$ind$coord[,1]

# 4. Normalize the index (0 to 1 scale)
df_for_pca$bp_index <- (df_for_pca$bp_index - min(df_for_pca$bp_index, na.rm = TRUE)) / 
  (max(df_for_pca$bp_index,na.rm = TRUE) - min(df_for_pca$bp_index, na.rm = TRUE))

# 5. Merge back to the original large dataset
# We use a left_join so that households with NAs stay in the dataset (with an NA index)
followup_MRT_hh <- followup_MRT_hh %>%
  left_join(df_for_pca %>% select(hhid, bp_index), by = "hhid")

followup_MRT_hh <- followup_MRT_hh %>%
  mutate(het_bp_index = as.factor(ifelse(bp_index <= median(bp_index, na.rm = TRUE), 1, 0)))

# Baseline decision making index 2
followup_MRT_hh <- followup_MRT_hh %>%
  mutate(
    bp_index_2 = as.numeric(dec_weight_index_1_tr_bl),
    het_bp_index_2 = as.factor(ifelse(dec_weight_index_1_tr_bl > median(dec_weight_index_1_tr_bl, na.rm = TRUE), 1, 0)))


# Beneficiary has a business at baseline
followup_MRT_hh <- followup_MRT_hh %>%
  mutate(
    has_bus = as.numeric(bus2_ben_dum_bl),
    het_has_bus = as.factor(bus2_ben_dum_bl))

# Beneficiary create a business (baseline is none)
followup_MRT_hh <- followup_MRT_hh %>%
  mutate(
    create_bus = ifelse(bus2_ben_dum_bl==0 & bus2_ben_dum==1, 1, 0),
    het_create_bus = as.factor(ifelse(bus2_ben_dum_bl==0 & bus2_ben_dum==1, 1, 0)))

# Beneficiary launch a business
followup_MRT_hh <- followup_MRT_hh %>%
  mutate(
    lauch_bus = as.numeric(bus2_within_12_dum_ben_bl),
    het_lauch_bus = as.factor(bus2_within_12_dum_ben_bl))


# Poverty we use the yearly consumption per adult equivalent at baseline consum_1_day_pc_ppp_tr_bl 
followup_MRT_hh <- followup_MRT_hh %>%
  mutate(
    consum_eq = as.numeric(tot_hh_rev),
    het_consum_eq = as.factor(ifelse(tot_hh_rev > median(tot_hh_rev, na.rm = TRUE), 1, 0)))


# Household composition
followup_MRT_hh <- followup_MRT_hh %>%
  mutate( # Adult males
         nbr_adult_mal = as.numeric(workageo_mal_n_bl),
         shr_adult_mal = as.numeric(workageo_mal_s_bl),
         nbr_adult_mal_tot = as.numeric(as.numeric(adult_n_bl) * (1 - as.numeric(adult_fem_s_bl))),
         
         # Adult females
         nbr_adult_fem = as.numeric(workageo_fem_n_bl),
         shr_adult_fem = as.numeric(workageo_fem_s_bl),
         nbr_adult_fem_tot = as.numeric(as.numeric(adult_n_bl) * as.numeric(adult_fem_s_bl)),
         shr_adult_fem_mal = as.numeric(adult_fem_s_bl),
         
         # Adult tot
         nbr_adult = as.numeric(workageo_n_bl),
         shr_adult = as.numeric(workageo_s_bl),
         nbr_adult_tot = as.numeric(adult_n_bl),
         
         # Children
         hh_has_kid = as.numeric(kid_d_bl),
         nbr_kid = as.numeric(kid_n_bl),
         
         # Elder
         nbr_elder = as.numeric(elder_n_bl),
         shr_elder = as.numeric(elder_s_bl),
         
         # Hh. level
         nbr_hh_number = as.numeric(mem_n_bl)

         ) %>% 
  
  mutate(
    het_nbr_adult_mal = ifelse(nbr_adult_mal > median(nbr_adult_mal, na.rm = TRUE), 1, 0),
    het_shr_adult_mal = ifelse(shr_adult_mal > median(shr_adult_mal, na.rm = TRUE), 1, 0),
    het_nbr_adult_mal_tot = ifelse(nbr_adult_mal_tot > median(nbr_adult_mal_tot, na.rm = TRUE), 1, 0),
    het_wtht_adult_mal = ifelse(nbr_adult_mal == 0, 1, 0),
    het_wtht_adult_mal_tot = ifelse(nbr_adult_mal_tot == 0, 1, 0),
    
    het_nbr_adult_fem = ifelse(nbr_adult_fem > median(nbr_adult_fem, na.rm = TRUE), 1, 0),
    het_shr_adult_fem = ifelse(shr_adult_fem > median(shr_adult_fem, na.rm = TRUE), 1, 0),
    het_nbr_adult_fem_tot = ifelse(nbr_adult_fem_tot > median(nbr_adult_fem_tot, na.rm = TRUE), 1, 0),
    het_shr_adult_fem_mal = ifelse(shr_adult_fem_mal > median(shr_adult_fem_mal, na.rm = TRUE), 1, 0),
    
    het_nbr_adult = ifelse(nbr_adult > median(nbr_adult, na.rm = TRUE), 1, 0),
    het_shr_adult = ifelse(shr_adult > median(shr_adult, na.rm = TRUE), 1, 0),
    het_nbr_adult_tot = ifelse(nbr_adult_tot > median(nbr_adult_tot, na.rm = TRUE), 1, 0),
    
    het_nbr_elder = ifelse(nbr_elder > median(nbr_elder, na.rm = TRUE), 1, 0),
    het_shr_elder = ifelse(shr_elder > median(shr_elder, na.rm = TRUE), 1, 0),
    
    het_nbr_kid = ifelse(nbr_kid > median(nbr_kid, na.rm = TRUE), 1, 0),
    het_nbr_hh_number = ifelse(nbr_hh_number > median(nbr_hh_number, na.rm = TRUE), 1, 0)
    
    )


# ------------------------------------------------------------------------------
# Enumerator characteristics
# ------------------------------------------------------------------------------

# Convert enumerator identifier to factor in consent and main IPV datasets
ipv_df_consent_cash <- ipv_df_consent_cash %>%
  mutate(surveyor = as.factor(surveyor),
        reg_hh_csh_mrt = 1,
        reg_hh_pi_mrt = 0)

ipv_df_consent_pi <- ipv_df_consent_pi %>%
  mutate(surveyor = as.factor(surveyor),
         reg_hh_csh_mrt = 0,
         reg_hh_pi_mrt = 1)

ipv_df_all <- ipv_df_all %>%
  mutate(surveyor = as.factor(surveyor))


# ------------------------------------------------------------------------------
# Merging the two ipv df
# ------------------------------------------------------------------------------

ipv_df_consent <- plyr::rbind.fill(ipv_df_consent_cash %>% select(-c(treatment_pi_pool)),
                                   ipv_df_consent_pi%>% select(-c(treatment_csh_trnsfr)))


# ------------------------------------------------------------------------------
# Heterogeneity variables for married after median age
# ------------------------------------------------------------------------------
ipv_df_consent <- ipv_df_consent %>% 
  mutate(het_age_at_mrrg = as.factor(ifelse(age_at_mrrg > median(age_at_mrrg, na.rm = TRUE), 1, 0)),
         age_at_mrrg = as.numeric(age_at_mrrg))

################################################################################
######################## Variables for Treat to identification #################
################################################################################

control_vars = c(
  # Elder/children with imbalance
  "nbr_elder",         
  "het_nbr_elder",      
  "shr_elder",           
  # Education with imbalance
  "hhh_prim_bl",          
  "pben_lit_bl",         
  # Economic with imbalance
  "tot_emp_2rev12_ben_98_ppp_bl", 
  # Business with imbalance
  "bus2_within_12_dum_ben_bl" 
)

cluster_vars <- c("cluster")
strata_vars <- c("strata") 

# Selecting the intimate partner violence (IPV) variables of interest
df_treat_id <- ipv_df_all %>% 
  # Remove any rows where the IPV completion status is missing
  filter(!is.na(ipv_completed)) %>% 
  # Keep only rows where the IPV survey was completed (value equals 1)
  filter(ipv_completed==1) %>% 
  # Perform a left join with household data from the MRT follow-up survey
  left_join(followup_MRT_hh %>% 
              # Phase 2
              filter(phase==2) %>%  
              # Select only the household ID and regional household program indicators
              select(hhid, reg_hh_pi_mrt, reg_hh_csh_mrt, treatment_csh_trnsfr, 
                     treatment_pi, treatment_pi_pool, village, commune,
                     married_ben_bl, married_ben, 
                     workageo_mal_n_bl, workageo_mal_n, 
                     elder_n_bl, elder_n, 
                     spouse_in_bl, spouse_in_fl, 
                     spouses_fl, spouses_bl,
                     equiv_n_bl, equiv_n,
                     adult_n_bl, adult_n, 
                     kid_n_bl, kid_n, 
                     kid_d_bl, kid_d, 
                     baby_n_bl, baby_n, 
                     earlychild_n_bl, earlychild_n, 
                     olderchild_n_bl, olderchild_n, 
                     all_of(control_vars), 
                     all_of(strata_vars),
                     all_of(cluster_vars)
              ) %>% 
              mutate(
                # --- Married beneficiary ---
                married_ben_fl = married_ben %>%
                  structure(label = paste0(attr(allrounds_MRT_hh$married_ben, "label"), " at followup")),
                
                # --- Working-age male HH members ---
                workageo_mal_n_fl = workageo_mal_n %>%
                  structure(label = paste0(attr(allrounds_MRT_hh$workageo_mal_n, "label"), " at followup")),
                
                # --- Elderly HH members ---
                elder_n_fl = elder_n %>%
                  structure(label = paste0(attr(allrounds_MRT_hh$elder_n, "label"), " at followup")),
                
                # --- Spouse in household ---
                spouse_in_bl = spouse_in_bl %>%
                  structure(label = paste0(attr(allrounds_MRT_hh$spouse_in_bl, "label"), " at baseline")),
                spouse_in_fl = spouse_in_fl %>%
                  structure(label = paste0(attr(allrounds_MRT_hh$spouse_in_fl, "label"), " at followup")),
                
                # --- Number of spouses ---
                spouses_bl = spouses_bl %>%
                  structure(label = paste0(attr(allrounds_MRT_hh$spouses_bl, "label"), " at baseline")),
                spouses_fl = spouses_fl %>%
                  structure(label = paste0(attr(allrounds_MRT_hh$spouses_fl, "label"), " at followup")),
                
                # --- Equivalence scale HH size ---
                equiv_n_fl = equiv_n %>%
                  structure(label = paste0(attr(allrounds_MRT_hh$equiv_n, "label"), " at followup")),
                
                # --- Adult HH members ---
                adult_n_fl = adult_n %>%
                  structure(label = paste0(attr(allrounds_MRT_hh$adult_n, "label"), " at followup")),
                
                # --- Children (0–30 months, depending on your definition) ---
                kid_n_fl = kid_n %>%
                  structure(label = paste0(attr(allrounds_MRT_hh$kid_n, "label"), " at followup")),

                kid_d_fl = kid_d %>%
                  structure(label = paste0(attr(allrounds_MRT_hh$kid_d, "label"), " at followup")),
                
                # --- Babies ---
                baby_n_fl = baby_n %>%
                  structure(label = paste0(attr(allrounds_MRT_hh$baby_n, "label"), " at followup")),
                
                # --- Early children ---
                earlychild_n_fl = earlychild_n %>%
                  structure(label = paste0(attr(allrounds_MRT_hh$earlychild_n, "label"), " at followup")),
                
                # --- Older children ---
                olderchild_n_fl = olderchild_n %>%
                  structure(label = paste0(attr(allrounds_MRT_hh$olderchild_n, "label"), " at followup"))
                
              ) %>% 
              select(hhid, reg_hh_pi_mrt, reg_hh_csh_mrt, treatment_csh_trnsfr, 
                     treatment_pi,treatment_pi_pool, village, commune, 
                     ends_with("_fl"), ends_with("_bl"), all_of(control_vars), 
                     all_of(strata_vars), all_of(cluster_vars)) %>% 
              # Convert household ID to character type to ensure consistent joining
              mutate(hhid=as.character(hhid)), 
            # Specify that joining should be done using household ID as the key
            by="hhid") %>% 
  # Filtering to keep the individual of interest
  filter(reg_hh_pi_mrt==1 | reg_hh_csh_mrt==1) %>% 
  mutate(
    spouse_in_fl = factor(
      spouse_in_fl,
      levels = c(0, 1),
      labels = c("No", "Yes")
    ),
    spouse_in_bl = factor(
      spouse_in_bl,
      levels = c(0, 1),
      labels = c("No", "Yes")
    )
  )


# 73.2 percent = 869 of women are currently leaving with their partner at followup
tbl_spouse <- df_treat_id %>% 
  select(spouse_in_bl, spouse_in_fl) %>% 
  tbl_summary(
    by = spouse_in_bl,
    label = list(
      spouse_in_fl ~ "Spouse in household at follow-up"
    ),
    type      = list(spouse_in_fl ~ "categorical"),
    statistic = list(all_categorical() ~ "{n} ({p}%)"),
    missing   = "no"
  ) %>% 
  modify_header(label ~ "Spouse in household at baseline") %>%
  add_p(test = list(all_categorical() ~ "chisq.test")) %>% 
  add_overall(last = TRUE, col_label = "Total") %>%
  modify_caption("**Cross-tabulation of baseline vs. follow-up spouse presence (Chi-square test)**")

tbl_spouse

# Table of proportion

# The cross-tabulation shows strong persistence in living arrangements with the husband between baseline and follow-up. 
# Women who lived with their spouse at baseline overwhelmingly remained with their spouse at follow-up (83%), 
# while those who did not live with their spouse at baseline were much more likely to remain apart (52%). 
# The “Missing” category also displays internal consistency, with nearly half remaining missing at both waves. 
# The association between baseline and follow-up spouse presence is highly statistically significant (χ² test, p < 0.001), 
# indicating that living arrangement status is far from random and exhibits substantial temporal stability.

# In the main analysis I restrict the sample to the women living with their husband
# at followup which represent 66 percent of the total sample. 

#     BL  Yes   No  
# FL
# Yes                75%                      
# No                 25%
#Total  76.5%  23.5%   

## removing those with NA at baseline or followup and restricting the sample

df_treat_id <- df_treat_id %>% 
  filter(!is.na(spouse_in_bl)) %>% 
  filter(!is.na(spouse_in_fl)) %>% 
  mutate(
    spouse_in_fl = ifelse(spouse_in_fl=="Yes", 1, 0),
    spouse_in_bl = ifelse(spouse_in_bl=="Yes", 1, 0)
  )



################################################################################
######################## Expenditures variables ################################
################################################################################

# Filter to get only hh who spend money on item
nonfood <- nonfood %>% 
  filter(consum_nf==1) 


# Creates variables using the 6 months as he same base

nonfood <- nonfood %>% 
  mutate(consum_nf_annual = ifelse(nonfood_period == "30 derniers jours", 12 * consum_nf_val,
                                     ifelse(nonfood_period == "7 derniers jours", 12 * 4.4 * consum_nf_val, 
                                            2 * consum_nf_val)) %>%
           structure(label = "Consumption per year"))


# Dépenses Masculines (Male Consumption)
male_consumption_ids <- c(26, 27, 32, 35)

# Dépenses Féminines (Female Consumption)
female_consumption_ids <- c(15, 28, 29, 33, 36, 45)

# Autres Dépenses (Other Consumption)
other_consumption_ids <- c(
  10, 11, 12, 13, 14, 16, 17, 18, 19, 1, 20, 21, 22, 23, 24, 25,
  2, 30, 31, 34, 37, 38, 39, 3, 40, 41, 42, 43, 44, 4, 5, 6, 7, 8, 9
)


# Define a function to categorize expenditures based on the ID vectors
categorize_expenditure_by_id <- function(nonfood_pos) {
  if (nonfood_pos %in% male_consumption_ids) {
    return("male_expenditure")
  } else if (nonfood_pos %in% female_consumption_ids) {
    return("female_expenditure")
  } else if (nonfood_pos %in% other_consumption_ids) {
    return("other_expenditure")
  } else {
    return(NA) # Handle cases where an ID might not be in any defined list
  }
}

# Apply the categorization
nonfood$expenditure_group <- sapply(nonfood$nonfood_pos, categorize_expenditure_by_id)

# Group by household ID (hhid) and expenditure group, then sum the annual expenditures
nonfood_long <- nonfood %>%
  group_by(hhid, expenditure_group) %>%
  summarise(total_annual_expenditure = sum(consum_nf_annual, na.rm = TRUE)*0.0274, .groups = 'drop')

# Pivot wider to get male, female, and other expenditures as separate columns per household
nonfood_long <- nonfood_long %>%
  pivot_wider(
    names_from = expenditure_group,
    values_from = total_annual_expenditure,
    values_fill = list(total_annual_expenditure = 0)
  )


# Reorder columns for clarity
nonfood_long <- nonfood_long %>%
  mutate(male_expenditure = male_expenditure %>%
           structure(label = "Total partner expenditures in USD"),
         female_expenditure = female_expenditure %>%
           structure(label = "Total female expenditures in USD"),
         other_expenditure = other_expenditure %>%
           structure(label = "Total other expenditures in USD"),
         hhid = as.character(hhid)
         ) %>% 
  mutate(across(everything(), ~ replace_na(., 0)))


################################################################################
######################## Merging and exporting #################################
################################################################################

followup_ipv_MRT_hh <- ipv_df_consent %>% 
  # Filtering to keep the individual of interest
  filter(reg_hh_pi_mrt==1 | reg_hh_csh_mrt==1) %>% 
  left_join(followup_MRT_hh %>% 
              filter(hhid %in% lst_hh) %>% 
            select(-c(reg_hh_pi_mrt, reg_hh_csh_mrt, treatment_csh_trnsfr, 
                      treatment_pi_pool, married_ben_bl, married_ben, workageo_mal_n, 
                      elder_n, spouse_in_bl, spouse_in_fl, spouses_bl, spouses_fl,
                      age
                      )) %>% 
            mutate(hhid=as.character(hhid)), 
            by="hhid") %>% 
  left_join(nonfood_long, by="hhid") %>% 
  mutate(pben_relation_bl=labelled::unlabelled(pben_relation_bl),
         male_expenditure = male_expenditure/mem_n,
         female_expenditure = female_expenditure/mem_n,
         other_expenditure = other_expenditure/mem_n,
         
         baby_d_fl = ifelse(baby_n>=1,1,0),
         earlychild_d_fl = ifelse(earlychild_n - earlychild_n_bl>=1,1,0),
         olderchild_d_fl = ifelse(olderchild_n - olderchild_n_bl>=1,1,0),
         tot_mem_d_fl = ifelse(mem_n - mem_n_bl>=1,1,0),
         
         earlychild_n_fl = earlychild_n - earlychild_n_bl,
         olderchild_n_fl = olderchild_n - olderchild_n_bl,
         tot_mem_n_fl = mem_n - mem_n_bl
         ) %>% 
  mutate(
    # --- Married beneficiary ---
    married_ben_fl = married_ben %>%
      structure(label = paste0(attr(allrounds_MRT_hh$married_ben, "label"), " at followup")),
    
    # --- Working-age male HH members ---
    workageo_mal_n_fl = workageo_mal_n %>%
      structure(label = paste0(attr(allrounds_MRT_hh$workageo_mal_n, "label"), " at followup")),
    
    # --- Elderly HH members ---
    elder_n_fl = elder_n %>%
      structure(label = paste0(attr(allrounds_MRT_hh$elder_n, "label"), " at followup")),
    
    # --- Spouse in household ---
    spouse_in_bl = spouse_in_bl %>%
      structure(label = paste0(attr(allrounds_MRT_hh$spouse_in_bl, "label"), " at baseline")),
    spouse_in_fl = spouse_in_fl %>%
      structure(label = paste0(attr(allrounds_MRT_hh$spouse_in_fl, "label"), " at followup")),
    
    # --- Number of spouses ---
    spouses_bl = spouses_bl %>%
      structure(label = paste0(attr(allrounds_MRT_hh$spouses_bl, "label"), " at baseline")),
    spouses_fl = spouses_fl %>%
      structure(label = paste0(attr(allrounds_MRT_hh$spouses_fl, "label"), " at followup")),
    
    # --- Equivalence scale HH size ---
    equiv_n_fl = equiv_n %>%
      structure(label = paste0(attr(allrounds_MRT_hh$equiv_n, "label"), " at followup")),
    
    # --- Adult HH members ---
    adult_n_fl = adult_n %>%
      structure(label = paste0(attr(allrounds_MRT_hh$adult_n, "label"), " at followup")),
    
    # --- Children (0–30 months, depending on your definition) ---
    kid_n_fl = kid_n %>%
      structure(label = paste0(attr(allrounds_MRT_hh$kid_n, "label"), " at followup")),
    
    kid_d_fl = kid_d %>%
      structure(label = paste0(attr(allrounds_MRT_hh$kid_d, "label"), " at followup")),
    
    # --- Babies ---
    baby_n_fl = baby_n %>%
      structure(label = paste0(attr(allrounds_MRT_hh$baby_n, "label"), " at followup")),
    
    baby_d_fl = baby_d_fl %>%
      structure(label =  "New baby born in the last 12 months at followup"),
    
    # --- Early children ---
    earlychild_n_fl = earlychild_n_fl %>%
      structure(label = paste0(attr(allrounds_MRT_hh$earlychild_n, "label"), " at followup")),
    
    earlychild_d_fl = earlychild_d_fl %>%
      structure(label = paste0("New children aged 0-5 at followup")),
    
    # --- Older children ---
    olderchild_n_fl = olderchild_n_fl %>%
      structure(label = paste0(attr(allrounds_MRT_hh$olderchild_n, "label"), " at followup")),
    
    olderchild_d_fl = olderchild_d_fl %>%
      structure(label = "New children aged 6-14 in householdat at followup"),
    
    # --- Men size ---
    tot_mem_d_fl = tot_mem_d_fl %>%
      structure(label = "New hh members at followup"),
    
    tot_mem_n_fl = tot_mem_n_fl %>%
      structure(label = "Number of new hh members at followup")
    # You can add similar structure() calls for village/commune/treatment vars if needed,
    # but they usually already have clean labels.
  )



# Binary for relation to beneficiary
followup_ipv_MRT_hh <- dummy_cols(followup_ipv_MRT_hh, select_columns = "pben_relation_bl", remove_first_dummy = FALSE)

followup_ipv_MRT_hh <- followup_ipv_MRT_hh %>% 
  mutate(pben_relation_bl_other = ifelse( (`pben_relation_bl_Head of household`==0) & (`pben_relation_bl_Spouse`==0), 1, 0))



# keep even those who didn't give their consent for the IPV module.
ipv_df_all <- ipv_df_all %>% 
  distinct(hhid, .keep_all = TRUE) %>% # there are some duplicateds row with the same 
  # in the ipv_df, I consider the first row only
  left_join(followup_MRT_hh %>% 
              distinct(hhid, .keep_all = TRUE) %>% 
              select(hhid,contains("treat"), commune, strata, reg_hh_csh_mrt,reg_hh_pi_mrt,strata, spouse_in_fl,
                     cluster, treatment_pi_spill, pben_age_bl, nbr_elder, het_nbr_elder, shr_elder, 
                     hhh_prim_bl, pben_lit_bl, tot_emp_2rev12_ben_98_ppp_bl, bus2_within_12_dum_ben_bl,
                     village, reg_hh_pi_spl_mrt)%>% 
               mutate(hhid=as.character(hhid)), 
                      by="hhid", 
                      multiple="first") %>% 
  filter(pben_age_bl <= 50) %>% # This is important as the reported age in the ipv not censent don't match everytime
  filter(pben_age_bl >= 18)




# Total dataset
followup_MRT_hh <- followup_MRT_hh %>% 
  # Filtering to keep the individual of interest
  filter(reg_hh_pi_mrt==1 | reg_hh_csh_mrt==1) %>% 
  mutate(hhid=as.character(hhid)) %>% 
  right_join(ipv_df_all %>% 
              select(hhid, mr2)%>% 
              mutate(hhid=as.character(hhid)), 
            by="hhid", 
            multiple="first") %>% 
  mutate(
    # Age at marriage 
      age_at_mrrg = as.numeric(mr2),
      age_at_mrrg_above18 = ifelse(age_at_mrrg > 18, 1, 0),
      het_age_at_mrrg =ifelse(age_at_mrrg > median(age_at_mrrg, na.rm = TRUE), 1, 0)
  ) %>% 
  filter(pben_age_bl <= 50) %>% 
  filter(pben_age_bl >= 18)



# Exporting the data in R format
write_rds(baseline_MRT_hh, "output/data/baseline_MRT_hh.rds")
write_rds(allrounds_MRT_hh, "output/data/allrounds_MRT_hh.rds")
write_rds(followup_MRT_hh, "output/data/followup_MRT_hh.rds")
write_rds(followup_ipv_MRT_hh, "output/data/followup_ipv_MRT_hh.rds")
write_rds(ipv_df_all, "output/data/ipv_df_all.rds")
write_rds(ipv_df_consent, "output/data/ipv_df_consent.rds")
write_rds(df_treat_id, "output/data/df_treat_id.rds")


##################################################################################
######################## Spillover effects #######################################
##################################################################################

# Filter for phase 2 data (follow-up)
ipv_df_spill <- ipv_df_all %>% 
  filter(!is.na(treatment_pi_spill))  %>% 
  filter(ipv_completed==1)

followup_ipv_MRT_hh_spill <- ipv_df_spill %>% 
  # Filtering to keep the individual of interest
  filter(!is.na(treatment_pi_spill)) %>% 
  filter(pben_age_bl <= 50) %>% 
  filter(pben_age_bl >= 18) %>% 
  filter(spouse_in_fl==1)

followup_ipv_MRT_hh_spill <- followup_ipv_MRT_hh_spill %>% 
  mutate(treatment_pi_pool_spill = ifelse(treatment_pi_spill=="Control",0,1)) 

#### For the pool Economic inclusion program

followup_ipv_MRT_hh_spill <- followup_ipv_MRT_hh_spill %>% 
  mutate(treatment_pi_pool_spill = factor(treatment_pi_pool_spill, levels = c(0,1),
                                    labels = c("Control","Pool")))


################################################################################
######################## Controlling behavior ##################################
################################################################################

# Define the variables to include in the IPV severity index
scale_vars = c("ipv1", "ipv2", "ipv3", "ipv5", "ipv6", "ipv7")

# Set the name of the new index variable to be created
curr_ipv = "control_ipv_sev_index"
lab_curr_ipv = "Controlling behavior severity index"



######################### Economic Inclusion ####################################
# Create the IPV severity index as the mean of selected items, handling missing values
followup_ipv_MRT_hh_spill <- followup_ipv_MRT_hh_spill %>%
  # Compute the row-wise mean of the selected IPV variables
  mutate(!!sym(curr_ipv) := apply(select(., scale_vars), 1, sum, na.rm = TRUE)) %>%
  mutate(!!sym(curr_ipv) := !!sym(curr_ipv)/length(scale_vars)) %>%
  # Replace values with NA if all selected variables are NA for a given row
  mutate(!!sym(curr_ipv) := ifelse(rowSums(!is.na(select(., all_of(scale_vars)))) == 0, NA, !!sym(curr_ipv)))

# Transfort to NA the index values that are missing in the regular indicator
followup_ipv_MRT_hh_spill <- followup_ipv_MRT_hh_spill %>%
  mutate(!!sym(curr_ipv) := ifelse(is.na(!!sym(paste0(substr(curr_ipv, 1, nchar(curr_ipv) - 9), "12m"))),NA, !!sym(curr_ipv))) 

# Calculate the global mean of the index among the control groups
mean_glob = mean(followup_ipv_MRT_hh_spill %>% 
                   filter(treatment_pi_pool_spill == "Control") %>% 
                   select(!!sym(curr_ipv)) %>% 
                   pull(), na.rm = TRUE)

# Calculate the global standard deviation of the index among the control groups
std_glob = sd(followup_ipv_MRT_hh_spill %>% 
                filter(treatment_pi_pool_spill == "Control") %>% 
                select(!!sym(curr_ipv)) %>% 
                pull(), na.rm = TRUE)

# Standardize the IPV index using the global mean and standard deviation, and add a label
followup_ipv_MRT_hh_spill <- followup_ipv_MRT_hh_spill %>%
  mutate(!!sym(curr_ipv) := ifelse(is.na(!!sym(curr_ipv)),NA, ((!!sym(curr_ipv) - mean_glob) / std_glob)) %>%
           structure(label = lab_curr_ipv)) %>%
  # Rename an existing variable (to make sure the source name is correct)
  rename(control_ipv_sev_12m = control_ipv_12m)

################################################################################
######################## Emotional violence ####################################
################################################################################

# Define the variables to include in the IPV severity index
scale_vars = c("ipv8","ipv9","ipv10","ipv11")

# Set the name of the new index variable to be created
curr_ipv = "emo_ipv_sev_index"
lab_curr_ipv = "Emo. IPV severity index"

# Automatisation de la mutation basée sur scale_vars
followup_ipv_MRT_hh_spill <- followup_ipv_MRT_hh_spill %>%
  mutate(across(
    .cols = all_of(paste0(scale_vars, "c")),
    .fns = ~ ifelse(get(paste0(cur_column() %>% str_remove("c$"), "b")) == 0, 0, .),
    .names = "{.col}"
  ))

######################### Economic Inclusion ####################################
# Create the IPV severity index as the mean of selected items, handling missing values
followup_ipv_MRT_hh_spill <- followup_ipv_MRT_hh_spill %>%
  # Compute the row-wise mean of the selected IPV variables
  mutate(!!sym(curr_ipv) := apply(select(., paste0(scale_vars,"c")), 1, sum, na.rm = TRUE)) %>%
  mutate(!!sym(curr_ipv) := !!sym(curr_ipv)/length(scale_vars)) %>%
  # Replace values with 0 if any of the selected variables are 0 for a given row
  mutate(!!sym(curr_ipv) := ifelse(rowSums(select(., all_of(c(paste0(scale_vars, "a"), 
                                                              paste0(scale_vars, "b")))) == 0, na.rm = TRUE) > 0 & is.na(!!sym(curr_ipv)), 0,!!sym(curr_ipv)))

# Transfort to NA the index values that are missing in the regular indicator
followup_ipv_MRT_hh_spill <- followup_ipv_MRT_hh_spill %>%
  mutate(!!sym(curr_ipv) := ifelse(is.na(!!sym(paste0(substr(curr_ipv, 1, nchar(curr_ipv) - 9), "12m"))),NA, !!sym(curr_ipv))) 

# Calculate the global mean of the index among the control groups
mean_glob = mean(followup_ipv_MRT_hh_spill %>% 
                   filter(treatment_pi_pool_spill == "Control") %>% 
                   select(!!sym(curr_ipv)) %>% 
                   pull(), na.rm = TRUE)

# Calculate the global standard deviation of the index among the control groups
std_glob = sd(followup_ipv_MRT_hh_spill %>% 
                filter(treatment_pi_pool_spill == "Control") %>% 
                select(!!sym(curr_ipv)) %>% 
                pull(), na.rm = TRUE)

# Standardize the IPV index using the global mean and standard deviation, and add a label
followup_ipv_MRT_hh_spill <- followup_ipv_MRT_hh_spill %>%
  mutate(!!sym(curr_ipv) := ifelse(is.na(!!sym(curr_ipv)), NA, ((!!sym(curr_ipv) - mean_glob) / std_glob)) %>%
           structure(label = lab_curr_ipv)) %>%
  # Rename an existing variable (to make sure the source name is correct)
  rename(emo_ipv_sev_12m = emo_ipv_12m)

################################################################################
######################## Physical violence #####################################
################################################################################

# Define the variables to include in the IPV severity index
scale_vars = c("ipv12","ipv13","ipv14","ipv15","ipv16","ipv17")

# Set the name of the new index variable to be created
curr_ipv = "phy_ipv_sev_index"
lab_curr_ipv = "Phy. IPV severity index"

# Automatisation de la mutation basée sur scale_vars
followup_ipv_MRT_hh_spill <- followup_ipv_MRT_hh_spill %>%
  mutate(across(
    .cols = all_of(paste0(scale_vars, "c")),
    .fns = ~ ifelse(get(paste0(cur_column() %>% str_remove("c$"), "b")) == 0, 0, .),
    .names = "{.col}"
  ))

######################### Economic Inclusion ####################################
# Create the IPV severity index as the mean of selected items, handling missing values
followup_ipv_MRT_hh_spill <- followup_ipv_MRT_hh_spill %>%
  # Compute the row-wise mean of the selected IPV variables
  mutate(!!sym(curr_ipv) := apply(select(., paste0(scale_vars,"c")), 1, sum, na.rm = TRUE)) %>%
  mutate(!!sym(curr_ipv) := !!sym(curr_ipv)/length(scale_vars)) %>%
  # Replace values with 0 if any of the selected variables are 0 for a given row
  mutate(!!sym(curr_ipv) := ifelse(rowSums(select(., all_of(c(paste0(scale_vars, "a"), 
                                                              paste0(scale_vars, "b")))) == 0, na.rm = TRUE) > 0 & is.na(!!sym(curr_ipv)), 0,!!sym(curr_ipv)))

# Transfort to NA the index values that are missing in the regular indicator
followup_ipv_MRT_hh_spill <- followup_ipv_MRT_hh_spill %>%
  mutate(!!sym(curr_ipv) := ifelse(is.na(!!sym(paste0(substr(curr_ipv, 1, nchar(curr_ipv) - 9), "12m"))),NA, !!sym(curr_ipv))) 

# Calculate the global mean of the index among the control groups
mean_glob = mean(followup_ipv_MRT_hh_spill %>% 
                   filter(treatment_pi_pool_spill == "Control") %>% 
                   select(!!sym(curr_ipv)) %>% 
                   pull(), na.rm = TRUE)

# Calculate the global standard deviation of the index among the control groups
std_glob = sd(followup_ipv_MRT_hh_spill %>% 
                filter(treatment_pi_pool_spill == "Control") %>% 
                select(!!sym(curr_ipv)) %>% 
                pull(), na.rm = TRUE)

# Standardize the IPV index using the global mean and standard deviation, and add a label
followup_ipv_MRT_hh_spill <- followup_ipv_MRT_hh_spill %>%
  mutate(!!sym(curr_ipv) := ifelse(is.na(!!sym(curr_ipv)),NA, ((!!sym(curr_ipv) - mean_glob) / std_glob)) %>%
           structure(label = lab_curr_ipv)) %>%
  # Rename an existing variable (to make sure the source name is correct)
  rename(phy_ipv_sev_12m = phy_ipv_12m)

################################################################################
######################## Sexual violence #######################################
################################################################################

# Define the variables to include in the IPV severity index
scale_vars = c("ipv18","ipv19","ipv20")

# Set the name of the new index variable to be created
curr_ipv = "sex_ipv_sev_index"
lab_curr_ipv = "Sex. IPV severity index"

# Automatisation de la mutation basée sur scale_vars
followup_ipv_MRT_hh_spill <- followup_ipv_MRT_hh_spill %>%
  mutate(across(
    .cols = all_of(paste0(scale_vars, "c")),
    .fns = ~ ifelse(get(paste0(cur_column() %>% str_remove("c$"), "b")) == 0, 0, .),
    .names = "{.col}"
  ))

######################### Economic Inclusion ####################################
# Create the IPV severity index as the mean of selected items, handling missing values
followup_ipv_MRT_hh_spill <- followup_ipv_MRT_hh_spill %>%
  # Compute the row-wise mean of the selected IPV variables
  mutate(!!sym(curr_ipv) := apply(select(., paste0(scale_vars,"c")), 1, sum, na.rm = TRUE)) %>%
  mutate(!!sym(curr_ipv) := !!sym(curr_ipv)/length(scale_vars)) %>%
  # Replace values with 0 if any of the selected variables are 0 for a given row
  mutate(!!sym(curr_ipv) := ifelse(rowSums(select(., all_of(c(paste0(scale_vars, "a"), paste0(scale_vars, "b")))) == 0, na.rm = TRUE) > 0 & is.na(!!sym(curr_ipv)), 0,!!sym(curr_ipv)))

# Transfort to NA the index values that are missing in the regular indicator
followup_ipv_MRT_hh_spill <- followup_ipv_MRT_hh_spill %>%
  mutate(!!sym(curr_ipv) := ifelse(is.na(!!sym(paste0(substr(curr_ipv, 1, nchar(curr_ipv) - 9), "12m"))),NA, !!sym(curr_ipv))) 

# Calculate the global mean of the index among the control groups
mean_glob = mean(followup_ipv_MRT_hh_spill %>% 
                   filter(treatment_pi_pool_spill == "Control") %>% 
                   select(!!sym(curr_ipv)) %>% 
                   pull(), na.rm = TRUE)

# Calculate the global standard deviation of the index among the control groups
std_glob = sd(followup_ipv_MRT_hh_spill %>% 
                filter(treatment_pi_pool_spill == "Control") %>% 
                select(!!sym(curr_ipv)) %>% 
                pull(), na.rm = TRUE)

# Standardize the IPV index using the global mean and standard deviation, and add a label
followup_ipv_MRT_hh_spill <- followup_ipv_MRT_hh_spill %>%
  mutate(!!sym(curr_ipv) := ifelse(is.na(!!sym(curr_ipv)),NA, ((!!sym(curr_ipv) - mean_glob) / std_glob)) %>%
           structure(label = lab_curr_ipv)) %>%
  # Rename an existing variable (to make sure the source name is correct)
  rename(sex_ipv_sev_12m = sex_ipv_12m)

##################################################################################
######################## Sexual and Physical violence ############################
##################################################################################
# Define the variables to include in the IPV severity index
scale_vars = c("ipv12","ipv13","ipv14","ipv15","ipv16","ipv17", # Physical 
                            "ipv18","ipv19","ipv20") # Sexual

# Set the name of the new index variable to be created
curr_ipv = "sex_phy_ipv_sev_index"
lab_curr_ipv = "Phy. and sex. IPV severity index"

# Automatisation de la mutation basée sur scale_vars
followup_ipv_MRT_hh_spill <- followup_ipv_MRT_hh_spill %>%
  mutate(across(
    .cols = all_of(paste0(scale_vars, "c")),
    .fns = ~ ifelse(get(paste0(cur_column() %>% str_remove("c$"), "b")) == 0, 0, .),
    .names = "{.col}"
  ))

######################### Economic Inclusion ####################################

followup_ipv_MRT_hh_spill <- followup_ipv_MRT_hh_spill %>%
  # Compute the row-wise mean of the selected IPV variables
  mutate(sex_phy_ipv_sev_12m = ifelse(sex_ipv_sev_12m==1  |  phy_ipv_sev_12m==1, 1, 0)) 

# Create the IPV severity index as the mean of selected items, handling missing values
followup_ipv_MRT_hh_spill <- followup_ipv_MRT_hh_spill %>%
  # Compute the row-wise mean of the selected IPV variables
  mutate(!!sym(curr_ipv) := apply(select(., paste0(scale_vars,"c")), 1, sum, na.rm = TRUE)) %>%
  mutate(!!sym(curr_ipv) := !!sym(curr_ipv)/length(scale_vars)) %>%
  # Replace values with 0 if any of the selected variables are 0 for a given row
  mutate(!!sym(curr_ipv) := ifelse(rowSums(select(., all_of(c(paste0(scale_vars, "a"), paste0(scale_vars, "b")))) == 0, na.rm = TRUE) > 0 & is.na(!!sym(curr_ipv)), 0,!!sym(curr_ipv)))

# Transfort to NA the index values that are missing in the regular indicator
followup_ipv_MRT_hh_spill <- followup_ipv_MRT_hh_spill %>%
  mutate(!!sym(curr_ipv) := ifelse(is.na(!!sym(paste0(substr(curr_ipv, 1, nchar(curr_ipv) - 5), "12m"))),NA, !!sym(curr_ipv))) 

# Calculate the global mean of the index among the control groups
mean_glob = mean(followup_ipv_MRT_hh_spill %>% 
                   filter(treatment_pi_pool_spill == "Control") %>% 
                   select(!!sym(curr_ipv)) %>% 
                   pull(), na.rm = TRUE)

# Calculate the global standard deviation of the index among the control groups
std_glob = sd(followup_ipv_MRT_hh_spill %>% 
                filter(treatment_pi_pool_spill == "Control") %>% 
                select(!!sym(curr_ipv)) %>% 
                pull(), na.rm = TRUE)

# Standardize the IPV index using the global mean and standard deviation, and add a label
followup_ipv_MRT_hh_spill <- followup_ipv_MRT_hh_spill %>%
  mutate(!!sym(curr_ipv) := ifelse(is.na(!!sym(curr_ipv)),NA, ((!!sym(curr_ipv) - mean_glob) / std_glob)) %>%
           structure(label = lab_curr_ipv)) 

##################################################################################
######################## Economic violence #######################################
##################################################################################

# Define the variables to include in the IPV severity index
scale_vars = c("ipv22","ipv23","ipv25")

# Set the name of the new index variable to be created
curr_ipv = "eco_ipv_sev_index"
lab_curr_ipv = "Eco. IPV severity index"

# Automatisation de la mutation basée sur scale_vars
followup_ipv_MRT_hh_spill <- followup_ipv_MRT_hh_spill %>%
  mutate(across(
    .cols = all_of(paste0(scale_vars, "c")),
    .fns = ~ ifelse(get(paste0(cur_column() %>% str_remove("c$"), "b")) == 0, 0, .),
    .names = "{.col}"
  ))

######################### Economic Inclusion ####################################
# Create the IPV severity index as the mean of selected items, handling missing values
followup_ipv_MRT_hh_spill <- followup_ipv_MRT_hh_spill %>%
  # Compute the row-wise mean of the selected IPV variables
  mutate(!!sym(curr_ipv) := apply(select(., paste0(scale_vars,"c")), 1, sum, na.rm = TRUE)) %>%
  mutate(!!sym(curr_ipv) := !!sym(curr_ipv)/length(scale_vars)) %>%
  # Replace values with 0 if any of the selected variables are 0 for a given row
  mutate(!!sym(curr_ipv) := ifelse(rowSums(select(., all_of(c(paste0(scale_vars, "a"), paste0(scale_vars, "b")))) == 0, na.rm = TRUE) > 0 & is.na(!!sym(curr_ipv)), 0,!!sym(curr_ipv)))

# Transfort to NA the index values that are missing in the regular indicator
followup_ipv_MRT_hh_spill <- followup_ipv_MRT_hh_spill %>%
  mutate(!!sym(curr_ipv) := ifelse(is.na(!!sym(paste0(substr(curr_ipv, 1, nchar(curr_ipv) - 9), "12m"))),NA, !!sym(curr_ipv))) 

# Calculate the global mean of the index among the control groups
mean_glob = mean(followup_ipv_MRT_hh_spill %>% 
                   filter(treatment_pi_pool_spill == "Control") %>% 
                   select(!!sym(curr_ipv)) %>% 
                   pull(), na.rm = TRUE)

# Calculate the global standard deviation of the index among the control groups
std_glob = sd(followup_ipv_MRT_hh_spill %>% 
                filter(treatment_pi_pool_spill == "Control") %>% 
                select(!!sym(curr_ipv)) %>% 
                pull(), na.rm = TRUE)

# Standardize the IPV index using the global mean and standard deviation, and add a label
followup_ipv_MRT_hh_spill <- followup_ipv_MRT_hh_spill %>%
  mutate(!!sym(curr_ipv) := ifelse(is.na(!!sym(curr_ipv)),NA, ((!!sym(curr_ipv) - mean_glob) / std_glob)) %>%
           structure(label = lab_curr_ipv)) %>%
  # Rename an existing variable (to make sure the source name is correct)
  rename(eco_ipv_sev_12m = eco_ipv_12m)


################################################################################
######################## All type of violence ##################################
################################################################################

# Define the variables to include in the IPV severity index
scale_vars = c("ipv8","ipv9","ipv10",
               "ipv11","ipv12","ipv13","ipv14","ipv15","ipv16","ipv17",
               "ipv18","ipv19","ipv20")

# Set the name of the new index variable to be created
curr_ipv = "all_ipv_sev_index"
lab_curr_ipv = "All type of IPV (excluding Eco. and control) severity index"

# Automatisation de la mutation basée sur scale_vars
followup_ipv_MRT_hh_spill <- followup_ipv_MRT_hh_spill %>%
  mutate(across(
    .cols = all_of(paste0(scale_vars, "c")),
    .fns = ~ ifelse(get(paste0(cur_column() %>% str_remove("c$"), "b")) == 0, 0, .),
    .names = "{.col}"
  ))

######################### Economic Inclusion ####################################
# Create the IPV severity index as the mean of selected items, handling missing values
followup_ipv_MRT_hh_spill <- followup_ipv_MRT_hh_spill %>%
  # Compute the row-wise mean of the selected IPV variables
  mutate(!!sym(curr_ipv) := apply(select(., paste0(scale_vars,"c")), 1, sum, na.rm = TRUE)) %>%
  mutate(!!sym(curr_ipv) := !!sym(curr_ipv)/length(scale_vars)) %>%
  # Replace values with 0 if any of the selected variables are 0 for a given row
  mutate(!!sym(curr_ipv) := ifelse(rowSums(select(., all_of(c(paste0(scale_vars, "a"), paste0(scale_vars, "b")))) == 0, na.rm = TRUE) > 0 & is.na(!!sym(curr_ipv)), 0,!!sym(curr_ipv)))

# Transfort to NA the index values that are missing in the regular indicator
followup_ipv_MRT_hh_spill <- followup_ipv_MRT_hh_spill %>%
  mutate(!!sym(curr_ipv) := ifelse(is.na(ipv_all_12m),NA, !!sym(curr_ipv))) 

# Calculate the global mean of the index among the control groups
mean_glob = mean(followup_ipv_MRT_hh_spill %>% 
                   filter(treatment_pi_pool_spill == "Control") %>% 
                   select(!!sym(curr_ipv)) %>% 
                   pull(), na.rm = TRUE)

# Calculate the global standard deviation of the index among the control groups
std_glob = sd(followup_ipv_MRT_hh_spill %>% 
                filter(treatment_pi_pool_spill == "Control") %>% 
                select(!!sym(curr_ipv)) %>% 
                pull(), na.rm = TRUE)

# Standardize the IPV index using the global mean and standard deviation, and add a label
followup_ipv_MRT_hh_spill <- followup_ipv_MRT_hh_spill %>%
  mutate(!!sym(curr_ipv) := ifelse(is.na(!!sym(curr_ipv)),NA, ((!!sym(curr_ipv) - mean_glob) / std_glob)) %>%
           structure(label = lab_curr_ipv)) %>%
  # Rename an existing variable (to make sure the source name is correct)
  rename(all_ipv_sev_12m = ipv_all_12m)

############################## Saving ###########################################
write_rds(followup_ipv_MRT_hh_spill, "output/data/followup_ipv_MRT_hh_spill.rds")




################################################################################
######################## PARTICIPATION DATA ####################################
################################################################################

############################## FILM SCREENING ##################################
# ================= 1) Read + rename to questionnaire =================
fiche_film_df <- fiche_film_df %>%
  rename(
    moughataa   = moughata,
    commune     = commune,
    espace      = espace,
    avec        = avec,
    avec_no     = avec_no,
    paquet      = paquet,
    nbre_membre = nbre_membre,
    # Section 1
    q2  = qi_1,
    q3  = qi_2,
    q4  = qi_3,
    q5  = qi_4,
    q6  = qi_5,
    q7  = qi_6,
    q8a = qi_7a,
    q8b = qi_7b,
    q8c = qi_7c,
    # Section 2
    q9  = qii_1,
    # Section 3 (résumés et sélections)
    theme1_film_resume    = them1,
    theme2_identification = them2,
    theme3_traditions     = them3,
    theme4_avenir         = them4,
    theme5_solidarite     = them5,
    them1_sum  = qiii_1,
    them2_sum  = qiii_2,
    them3_sum  = qiii_3,
    them4_sum  = qiii_4,
    them5a_sum = qiii_5a,
    them5b_sum = qiii_5b,
    # Section 4
    q10 = qiv_1,
    q11 = qiv_2,
    q12 = qiv_3,
    q13 = qiv_4,
    q14 = qiv_5,
    q15 = qiv_6,
    q16 = qiv_7,
    q17 = qiv_8
  )

# ================= 2) Variable labels (single place) =================
fiche_film_df <- set_variable_labels(
  fiche_film_df,
  moughataa   = "Moughataa",
  commune     = "Commune",
  espace      = "No de l’espace",
  avec        = "No de l’AVEC",
  avec_no     = "Nom de l’AVEC",
  paquet      = "Type de paquet",
  nbre_membre = "Nombre de membres",
  q2  = "Date",
  q3  = "Nom du preneur de notes",
  q4  = "Nom de l’animateur",
  q5  = "Langue du village / discussion",
  q6  = "Heure de début",
  q7  = "Heure de fin",
  q8a = "Auditoire – nombre de personnes",
  q8b = "Leaders présents (codes ‘1 2 3 4’)",
  q8c = "Leaders présents – autres (texte)",
  q9  = "Engagement pendant le film (1..5; -99=NSP)",
  theme1_film_resume    = "Thème 1 – résumé",
  theme2_identification = "Thème 2 – identification",
  theme3_traditions     = "Thème 3 – traditions d’adaptation",
  theme4_avenir         = "Thème 4 – se projeter",
  theme5_solidarite     = "Thème 5 – solidarité & entraide",
  them1_sum  = "Thème 1 – cases cochées (1..10, ‘Autre’)",
  them2_sum  = "Thème 2 – cases cochées (1..10, ‘Autre’)",
  them3_sum  = "Thème 3 – cases cochées (1..10)",
  them4_sum  = "Thème 4 – cases cochées (1..10; 1–5 actions, 6–9 difficultés)",
  them5a_sum = "Thème 5A – cases cochées (1..8)",
  them5b_sum = "Thème 5B – cases cochées (1..8)",
  q10 = "Engagement pendant la discussion (1..5; -99=NSP)",
  q11 = "Réaction communauté (1..5; -99=NSP)",
  q12 = "Réaction leaders (1..5; -99=NSP)",
  q13 = "Compréhension (1..5; -99=NSP)",
  q14 = "Qui a plus parlé? (1=H → 5=F; -99=NSP)",
  q15 = "Désaccords (1=pas du tout → 5=beaucoup; -99=NSP)",
  q16 = "Observations ambiance sociale (texte)",
  q17 = "Autres observations (texte)"
)

# ============= 3) Dates + heures → durée en minutes ==============
# 2) Dates & heures + durée
# helpers
fmt_hhmm <- function(v){
  s <- trimws(as.character(v))
  s[s %in% c("", ".", "NA", "-98", "-99")] <- NA
  s <- gsub("[^0-9:]", "", s)                           # keep digits/colon
  # if it's 3–4 digits (e.g., 930 or 1510) -> "09:30" / "15:10"
  s <- ifelse(str_detect(s, "^\\d{3,4}$"),
              paste0(str_pad(substr(s, 1, nchar(s)-2), 2, pad = "0"),
                     ":", substr(s, nchar(s)-1, nchar(s))),
              s)
  # keep only HH:MM
  s <- ifelse(str_detect(s, "^\\d{1,2}:\\d{2}$"), s, s)
  s
}


to_minutes <- function(hhmm){
  ifelse(is.na(hhmm), NA_real_,
         as.numeric(lubridate::period_to_seconds(lubridate::hm(hhmm)))/60)
}

ord_1_to_5 <- function(x, labels){
  xnum <- suppressWarnings(as.integer(x))
  xnum[xnum %in% c(-99)] <- NA_integer_
  factor(xnum, levels = 1:5, ordered = TRUE, labels = labels)
}


add_theme <- function(df, col, mapping){
  
  wide <- df %>%
    select(.row, !!sym(col)) %>%
    rename(tmp = !!sym(col)) %>%
    mutate(tmp = str_squish(tmp)) %>%
    separate_rows(tmp, sep=" ") %>%
    filter(!is.na(tmp)) %>%
    mutate(opt = dplyr::recode(tmp, !!!mapping), val = 1L) %>%
    select(.row, opt, val) %>% 
    pivot_wider(names_from = opt, values_from = val, values_fill = 0,
                names_prefix = paste0(col, "_"))
  
  wide
}


fiche_film_df <- fiche_film_df %>%
  mutate(
    q2_date = dmy(q2),
    q6_time = fmt_hhmm(q6),
    q7_time = fmt_hhmm(q7),
    duree_minutes = {
      start_min <- to_minutes(q6_time)
      end_min   <- to_minutes(q7_time)
      ifelse(end_min - start_min < 0, NA_real_, end_min - start_min)
    },
    duree_minutes = ifelse(is.na(duree_minutes), (as.numeric(q7_time) - as.numeric(q6_time))*60, duree_minutes)
    
  )

# Replace -99 and -98 to NA
fiche_film_df <- fiche_film_df %>%
  mutate(across(everything(), ~ ifelse(.==-99, NA, .)))

fiche_film_df <- fiche_film_df %>%
  mutate(across(everything(), ~ ifelse(.==-98, NA, .)))

# ================== 4) Leaders présents (Q8b) ====================
# Codes: 1=Chef; 2=Imam/leader religieux; 3=Conseiller municipal; 4=Autres leaders
leaders_dict <- c(
  `1` = "chef_village",
  `2` = "imam_ou_religieux",
  `3` = "conseiller_municipal",
  `4` = "autres_leaders"
)

fiche_film_df <- fiche_film_df %>%
  mutate(.row = dplyr::row_number())

leaders_wide <- fiche_film_df %>%
  transmute(.row,
            q8b = str_squish(q8b)) %>%
  separate_rows(q8b, sep = "\\s+") %>%
  filter(!is.na(q8b), q8b != "") %>%
  mutate(leader = dplyr::recode(q8b, !!!leaders_dict),
         val = 1L) %>%
  distinct(.row, leader, val) %>%
  tidyr::pivot_wider(
    names_from = leader, values_from = val, values_fill = 0,
    names_prefix = "leader_"
  )

fiche_film_df <- fiche_film_df %>%
  left_join(leaders_wide, by = ".row")

# ================ 5) Likert (Q9–Q15) en facteurs ordonnés ================
fiche_film_df <- fiche_film_df %>%
  mutate(
    q9_engagement_film         = ord_1_to_5(q9,  c("Très désengagée","Plutôt désengagée","Neutre","Plutôt engagée","Très engagée")),
    q10_engagement_discussion  = ord_1_to_5(q10, c("Occupé·e","Plutôt occupé·e","Neutre","Plutôt engagé·e","Très engagé·e")),
    q11_reaction_communaute    = ord_1_to_5(q11, c("Négative","Plutôt négative","Neutre","Plutôt positive","Positive")),
    q12_reaction_leaders       = ord_1_to_5(q12, c("Négative/Pas appuyant","Plutôt négative","Neutre","Plutôt positive","Positive/Appuyant")),
    q13_comprehension          = ord_1_to_5(q13, c("Mal","Plutôt mal","Neutre","Plutôt bien","Bien")),
    q14_qui_parle_plus         = ord_1_to_5(q14, c("Hommes de plus","Plutôt hommes","Également","Plutôt femmes","Femmes de plus")),
    q15_desaccords             = ord_1_to_5(q15, c("Pas du tout","Faible","Moyenne","Élevée","Beaucoup"))
  )

# ================== 6) Thèmes (expansion des cases cochées) ==================
# Maps from the questionnaire
th1_map <- c(
  `1`="rapport_collaboration_epoux", `2`="conflits_entre_epoux",
  `3`="intervention_du_pere",        `4`="entraide_familiale",
  `5`="groupe_epargne",              `6`="enseignement_partage",
  `7`="activites_economiques",       `8`="difficultes_economiques",
  `9`="adaptation",                  `10`="enfants_a_ecole"
)
th2_map <- th1_map
th3_map <- c(
  `1`="scolarisation_enfants", `2`="changement_structure_familiale",
  `3`="chgt_activites_typiquement_F", `4`="chgt_activites_typiquement_H",
  `5`="travail_partage_HF", `6`="decisions_partagees_HF",
  `7`="innovations_technologie", `8`="mobilite_acces_marches",
  `9`="innovations_financieres", `10`="autre"
)
th4_map <- c(
  `1`="action_scolarisation_enfants", `2`="action_planif_entre_epoux",
  `3`="action_groupe_epargne_credit", `4`="action_apprendre_activites_diff",
  `5`="action_innovations_agri", `6`="diff_conflit_familles_villages",
  `7`="diff_chocs_imprevus", `8`="diff_jalousie",
  `9`="diff_desespoir", `10`="autre"
)
th5a_map <- c(
  `1`="conflit_avec_mari", `2`="planification_avec_mari",
  `3`="conseil_argent_mere", `4`="intervention_pere",
  `5`="enseignement_groupe_F", `6`="apprentissage_bissap_cousine",
  `7`="autre", `8`="autre_b"
)
th5b_map <- c(
  `1`="planifier_entre_epoux", `2`="planifier_avec_parents",
  `3`="apprendre_des_aines", `4`="partage_ressources",
  `5`="s_enseigner_groupes_F", `6`="s_enseigner_groupes_H",
  `7`="groupe_epargne_credit", `8`="autre"
)

# Human-readable labels for the expanded columns
lab_th1 <- c(
  rapport_collaboration_epoux="Rapport/collaboration entre époux",
  conflits_entre_epoux="Conflits entre époux",
  intervention_du_pere="Intervention du père",
  entraide_familiale="Entraide familiale",
  groupe_epargne="Groupe d’épargne",
  enseignement_partage="Enseignement/partage",
  activites_economiques="Activités économiques",
  difficultes_economiques="Difficultés économiques",
  adaptation="Adaptation",
  enfants_a_ecole="Enfants à l’école"
)
lab_th2 <- lab_th1
lab_th3 <- c(
  scolarisation_enfants="Scolarisation des enfants",
  changement_structure_familiale="Changement de structure familiale",
  chgt_activites_typiquement_F="Changement d’activités typiquement féminines",
  chgt_activites_typiquement_H="Changement d’activités typiquement masculines",
  travail_partage_HF="Partage du travail H/F",
  decisions_partagees_HF="Décisions partagées H/F",
  innovations_technologie="Innovations/technologie",
  mobilite_acces_marches="Mobilité/accès aux marchés",
  innovations_financieres="Innovations financières",
  autre="Autre"
)
lab_th4 <- c(
  action_scolarisation_enfants="Action: scolarisation des enfants",
  action_planif_entre_epoux="Action: planification entre époux",
  action_groupe_epargne_credit="Action: groupe d’épargne/crédit",
  action_apprendre_activites_diff="Action: apprendre activités différentes",
  action_innovations_agri="Action: innovations agricoles",
  diff_conflit_familles_villages="Difficulté: conflits familles/villages",
  diff_chocs_imprevus="Difficulté: chocs imprévus",
  diff_jalousie="Difficulté: jalousie",
  diff_desespoir="Difficulté: désespoir",
  autre="Autre"
)
lab_th5a <- c(
  conflit_avec_mari="Conflit avec le mari",
  planification_avec_mari="Planification avec le mari",
  conseil_argent_mere="Conseil argent (mère)",
  intervention_pere="Intervention du père",
  enseignement_groupe_F="S’enseigner entre groupes de femmes",
  apprentissage_bissap_cousine="Apprentissage bissap (cousine)",
  autre="Autre"
)
lab_th5b <- c(
  planifier_entre_epoux="Planifier entre époux",
  planifier_avec_parents="Planifier avec les parents",
  apprendre_des_aines="Apprendre des aînés",
  partage_ressources="Partage des ressources",
  s_enseigner_groupes_F="S’enseigner entre groupes de femmes",
  s_enseigner_groupes_H="S’enseigner entre groupes d’hommes",
  groupe_epargne_credit="Groupe d’épargne/crédit",
  autre="Autre"
)

# Expand all themes (creates them*_sum_* 0/1 columns)
fiche_film_df <- fiche_film_df %>%
  left_join(add_theme(., "them1_sum",  th1_map),  by = ".row") %>%
  left_join(add_theme(., "them2_sum",  th2_map),  by = ".row") %>%
  left_join(add_theme(., "them3_sum",  th3_map),  by = ".row") %>%
  left_join(add_theme(., "them4_sum",  th4_map),  by = ".row") %>%
  left_join(add_theme(., "them5a_sum", th5a_map), by = ".row") %>%
  left_join(add_theme(., "them5b_sum", th5b_map), by = ".row")

# Apply human-readable labels to the expanded columns
theme_label_map <- function(df, prefix, dict){
  sel <- grep(paste0("^", prefix, "_"), names(df), value = TRUE)
  if (!length(sel)) return(list())
  suf <- sub(paste0("^", prefix, "_"), "", sel)
  labs <- unname(dict[suf]); labs[is.na(labs)] <- gsub("_"," ", suf[is.na(labs)])
  setNames(as.list(labs), sel)
}

label_map <- c(
  var_label(fiche_film_df),
  theme_label_map(fiche_film_df, "them1_sum",  lab_th1),
  theme_label_map(fiche_film_df, "them2_sum",  lab_th2),
  theme_label_map(fiche_film_df, "them3_sum",  lab_th3),
  theme_label_map(fiche_film_df, "them4_sum",  lab_th4),
  theme_label_map(fiche_film_df, "them5a_sum", lab_th5a),
  theme_label_map(fiche_film_df, "them5b_sum", lab_th5b),
  # Leaders labels if present
  list(
    leader_chef_village                      = "Chef de village présent",
    leader_imam_ou_religieux                 = "Imam/leader religieux présent",
    leader_conseiller_municipal              = "Conseiller municipal",
    leader_autres_leaders                    = "Autres leaders présents"
  )[intersect(
    c("leader_chef_village","leader_imam_ou_religieux",
      "leader_conseiller_municipal","leader_autres_leaders"),
    names(fiche_film_df)
  )]
)

# Set create tables


fiche_film_df <- set_variable_labels(fiche_film_df, .labels = as.list(labels), .strict = FALSE)

fiche_film_df <- dummy_cols(fiche_film_df, c("q9_engagement_film","q10_engagement_discussion",
                                             "q11_reaction_communaute","q12_reaction_leaders",
                                             "q13_comprehension","q14_qui_parle_plus","q15_desaccords"))

# Use french code for group level
fiche_film_df <- fiche_film_df %>% 
  mutate(treatment = ifelse(paquet=="Complet", "Full",
                            ifelse(paquet=="Social","Psychosocial",NA))) %>% 
  rename(moughata=moughataa)


############################## ACV AND GERME ##################################

# ---- Wide -> Long: align menid / presence_germe / presence_acv by shared index ----
acv_germe_df <- acv_germe_df %>%
  mutate(.row = row_number()) %>%                                     # create a stable row id to keep rows aligned through pivots
  pivot_longer(                                                        # reshape wide repeated blocks to long
    cols = matches("^(menid\\d+|presence_germe\\d+|presence_acv\\d+)$"), # take all menid#, presence_germe#, presence_acv# columns
    names_to = c(".value", "idx"),                                    # split the column name into target column (.value) + index (idx)
    names_pattern = "^(menid|presence_germe|presence_acv)(\\d+)$"     # regex capturing the base name and its numeric suffix
  ) %>%
  mutate(idx = as.integer(idx)) %>%                                   # cast the index to integer
  filter(!(is.na(menid))) %>%                                         # keep only rows where a menid is recorded at this index
  arrange(.row, idx) %>%                                              # order by original row and index within row
  select(-.row)                                                       # drop helper id (not needed beyond here)

# ---- Count unique participants (menid) per espace (and code) ----
menid_counts_by_espace <- acv_germe_df %>%
  group_by(code, espace) %>%                                          # group by survey code and espace
  summarise(group_size = n_distinct(menid), .groups = "drop") %>%     # count distinct menid in each group
  arrange(espace)                                                     # sort by espace for readability

# ---- Merge group size back to long data ----
acv_germe_df <- acv_germe_df %>% 
  left_join(menid_counts_by_espace, by = c("code","espace"))          # add group_size (explicit join keys)

# ---- Clean multi-select raw strings: convert "." placeholders to NA ----
acv_germe_df <- acv_germe_df %>%
  mutate(
    .row = dplyr::row_number(),                                       # recreate a stable row id (used next to widen dummies)
    presence_acv   = na_if(presence_acv,   "."),                      # "." -> NA for ACV multi-select
    presence_germe = na_if(presence_germe, ".")                       # "." -> NA for Germe multi-select
  )

# Dictionaries to validate/normalize allowed option codes (kept as identity here)
presence_germe_dict <- c(`1`="1", `2`="2", `3`="3", `4`="4")
presence_acv_dict   <- c(`1`="1", `2`="2", `3`="3")

# ---- Expand presence_germe multi-select into dummy columns presence_germe_1..4 ----
germe_df <- acv_germe_df %>%
  transmute(.row,                                                      # keep row id to merge back later
            presence_germe = str_squish(presence_germe)) %>%          # trim/squish spacing
  separate_rows(presence_germe, sep = "\\s+") %>%                     # split space-separated codes into one-row-per-code
  filter(!is.na(presence_germe), presence_germe != "") %>%            # drop missing/empty codes
  mutate(
    presence_germe = dplyr::recode(presence_germe, !!!presence_germe_dict), # normalize values via dict
    val = 1L                                                          # mark presence
  ) %>%
  distinct(.row, presence_germe, val) %>%                             # keep unique (row, code)
  tidyr::pivot_wider(                                                 # wide: one column per code
    names_from  = presence_germe,
    values_from = val,
    values_fill = 0,
    names_prefix = "presence_germe_"
  )

# ---- Expand presence_acv multi-select into dummy columns presence_acv_1..3 ----
acv_df <- acv_germe_df %>%
  transmute(.row,
            presence_acv = str_squish(presence_acv)) %>%              # trim/squish spacing
  separate_rows(presence_acv, sep = "\\s+") %>%                       # split into one code per row
  filter(!is.na(presence_acv), presence_acv != "") %>%                # drop missing/empty
  mutate(
    presence_acv = dplyr::recode(presence_acv, !!!presence_acv_dict), # normalize via dict
    val = 1L
  ) %>%
  distinct(.row, presence_acv, val) %>%
  tidyr::pivot_wider(
    names_from  = presence_acv,
    values_from = val,
    values_fill = 0,
    names_prefix = "presence_acv_"
  )

# ---- Join the dummy columns back to the long dataset ----
acv_germe_df <- acv_germe_df %>%
  left_join(germe_df, by = ".row") %>%                                # add presence_germe_* dummies
  left_join(acv_df,   by = ".row")                                    # add presence_acv_* dummies

# ---- Define the binary columns we will summarize (0/1) ----
bin_vars <- c(
  "presence_germe_1","presence_germe_2","presence_germe_3","presence_germe_4",
  "presence_acv_1","presence_acv_2","presence_acv_3"
)

# ---- Replace remaining NAs in binary columns by 0 (absence) ----
acv_germe_df <- acv_germe_df %>%
  mutate(across(all_of(bin_vars), ~ tidyr::replace_na(., 0)))

# ---- Presence rates by espace/code/paquet (share of 1s, rounded to 2 decimals) ----
acv_germe_df <- acv_germe_df %>%
  group_by(code, espace, paquet, commune, moughata) %>%                                   # group at desired level
  summarise(
    across(all_of(bin_vars), ~ round(mean(. == 1, na.rm = TRUE), 2)),  # compute mean of 1s per column
    .groups = "drop"
  ) %>%
  arrange(code, espace, paquet)                                        # tidy ordering

# ------- Replace to english
acv_germe_df <- acv_germe_df %>% 
  mutate(treatment = ifelse(paquet=="Complet", "Full",
                            ifelse(paquet=="Social","Psychosocial",
                                   ifelse(paquet=="Capital","Capital",NA))))


# ---- Normalize 'espace' labels by removing leading 'Espace ' (if present) ----
acv_germe_df$espace <- gsub("Espace ", "", acv_germe_df$espace,        fixed = TRUE)
menid_counts_by_espace$espace <- gsub("Espace ", "", menid_counts_by_espace$espace, fixed = TRUE)


############################## Saving ###########################################
write_rds(fiche_film_df, "output/data/fiche_film_df.rds")
write_rds(acv_germe_df, "output/data/acv_germe_df.rds")


# ---- Add group_size to fiche_film_df (merge on code + espace) ----
# (Assumes fiche_film_df is already in your environment)
fiche_film_df <- fiche_film_df %>% 
  left_join(
    menid_counts_by_espace %>% select(code, espace, group_size),       # bring only the needed columns
    by = c("code","espace")                                            # explicit keys
  )

# Presence leader

fiche_film_df <- fiche_film_df %>% 
  mutate(
    presence_leader = as.factor(ifelse(leader_.==1,0,1))
  )
# Correcting traitement
fiche_film_df <- fiche_film_df %>% 
  select(code, espace, paquet, commune, moughata, treatment, starts_with("leader"), presence_leader) %>% 
  mutate(nbr_leader_present = leader_chef_village + leader_imam_ou_religieux + leader_autres_leaders + leader_conseiller_municipal)
## A selibaby the leader were present but in barkeol, the results indicated that 
# there is not a single leader present during

# Heterogeity per the type of leader present and the number of leader present.
# remove data to free space
rm(list = ls())

