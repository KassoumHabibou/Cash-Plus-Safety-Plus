################################################################################
######### IPV in Mauritania ####################################################
################################################################################

# Data wrangling

######################## Importing library and external files ##################
### List of required packages
required_packages <- c("tidyverse", "dplyr","haven","readr","fastDummies","plyr")

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
#                          pben_relation, # Productive beneficiary relationship to household head
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
         pben_relation, # Productive beneficiary relationship to household head
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
             cash_trsfr==0 & treatment_pi==0 ~ 1,  # Control group for cash transfer
             cash_trsfr==1 & treatment_pi==0 ~ 1,  # Cash transfer recipients in pi control group
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



########################### Spill over effects #################################
# Create regression indicator for baseline households
allrounds_MRT_hh <- allrounds_MRT_hh %>% 
  mutate(reg_hh_pi_spl_mrt = 
           case_when(
             cash_trsfr==0 ~ 1,  # Control group for cash transfer
             cash_trsfr==1 ~ 0,  # Cash transfer recipients in pi control group
             .default = 0                       # All other cases
           )%>%
           structure(label = "Filtering condition for spillover effect evaluation"))

# Create treatment indicator for baseline households
allrounds_MRT_hh <- allrounds_MRT_hh %>% 
  mutate(treatment_pi_spill= 
           case_when(
             reg_hh_pi_spl_mrt==1 & treatment_pi==0 ~ 0,  # Control group for spill
             reg_hh_pi_spl_mrt==1 & treatment_pi== 1 ~ 1,  
             reg_hh_pi_spl_mrt==1 & treatment_pi== 2 ~ 2, 
             reg_hh_pi_spl_mrt==1 & treatment_pi== 3 ~ 3,               
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

#### For the pool productive inclusion program

allrounds_MRT_hh <- allrounds_MRT_hh %>% 
  mutate(treatment_pi_pool = factor(treatment_pi_pool, levels = c(0,1),
                                    labels = c("Control","Pool")))

baseline_MRT_hh <- baseline_MRT_hh %>% 
  mutate(treatment_pi_pool = factor(treatment_pi_pool, levels = c(0,1),
                                    labels = c("Control","Pool")))

################################################################################
######################## Cleaning the IPV data #################################
################################################################################

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


ipv_df_consent <- ipv_df_consent %>% 
  # Keep only married women leaving with their husband at followup
  filter(spouse_in_bl==1 & spouse_in_fl==1) 

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

######################### Tekavoul (Cash Transfer) ####################################
# Create the IPV severity index as the mean of selected items, handling missing values
ipv_df_consent_cash <- ipv_df_consent_cash %>%
  # Compute the row-wise mean of the selected IPV variables
  mutate(!!sym(curr_ipv) := apply(select(., scale_vars), 1, sum, na.rm = TRUE)) %>%
  mutate(!!sym(curr_ipv) := !!sym(curr_ipv)/length(scale_vars)) %>%
  # Replace values with NA if all selected variables are NA for a given row
  mutate(!!sym(curr_ipv) := ifelse(rowSums(!is.na(select(., all_of(scale_vars)))) == 0, NA, !!sym(curr_ipv)))

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
  mutate(!!sym(curr_ipv) := ((!!sym(curr_ipv) - mean_glob) / std_glob) %>%
           structure(label = lab_curr_ipv)) %>%
  # Rename an existing variable (to make sure the source name is correct)
  rename(control_ipv_sev_12m = control_ipv_12m)

######################### Productive Inclusion ####################################
# Create the IPV severity index as the mean of selected items, handling missing values
ipv_df_consent_pi <- ipv_df_consent_pi %>%
  # Compute the row-wise mean of the selected IPV variables
  mutate(!!sym(curr_ipv) := apply(select(., scale_vars), 1, sum, na.rm = TRUE)) %>%
  mutate(!!sym(curr_ipv) := !!sym(curr_ipv)/length(scale_vars)) %>%
  # Replace values with NA if all selected variables are NA for a given row
  mutate(!!sym(curr_ipv) := ifelse(rowSums(!is.na(select(., all_of(scale_vars)))) == 0, NA, !!sym(curr_ipv)))

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
  mutate(!!sym(curr_ipv) := ((!!sym(curr_ipv) - mean_glob) / std_glob) %>%
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

######################### Tekavoul (Cash Transfer) ####################################
# Create the IPV severity index as the mean of selected items, handling missing values
ipv_df_consent_cash <- ipv_df_consent_cash %>%
  # Compute the row-wise mean of the selected IPV variables
  mutate(!!sym(curr_ipv) := apply(select(., paste0(scale_vars,"c")), 1, sum, na.rm = TRUE)) %>%
  mutate(!!sym(curr_ipv) := !!sym(curr_ipv)/length(scale_vars)) %>%
  # Replace values with 0 if any of the selected variables are 0 for a given row
  mutate(!!sym(curr_ipv) := ifelse(rowSums(select(., all_of(c(paste0(scale_vars, "a"), 
                                                              paste0(scale_vars, "b")))) == 0, na.rm = TRUE) > 0 & is.na(!!sym(curr_ipv)), 0,!!sym(curr_ipv)))

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
  mutate(!!sym(curr_ipv) := ((!!sym(curr_ipv) - mean_glob) / std_glob) %>%
           structure(label = lab_curr_ipv)) %>%
  # Rename an existing variable (to make sure the source name is correct)
  rename(emo_ipv_sev_12m = emo_ipv_12m)

######################### Productive Inclusion ####################################
# Create the IPV severity index as the mean of selected items, handling missing values
ipv_df_consent_pi <- ipv_df_consent_pi %>%
  # Compute the row-wise mean of the selected IPV variables
  mutate(!!sym(curr_ipv) := apply(select(., paste0(scale_vars,"c")), 1, sum, na.rm = TRUE)) %>%
  mutate(!!sym(curr_ipv) := !!sym(curr_ipv)/length(scale_vars)) %>%
  # Replace values with 0 if any of the selected variables are 0 for a given row
  mutate(!!sym(curr_ipv) := ifelse(rowSums(select(., all_of(c(paste0(scale_vars, "a"), paste0(scale_vars, "b")))) == 0, na.rm = TRUE) > 0 & is.na(!!sym(curr_ipv)), 0,!!sym(curr_ipv)))

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
  mutate(!!sym(curr_ipv) := ((!!sym(curr_ipv) - mean_glob) / std_glob) %>%
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

######################### Tekavoul (Cash Transfer) ####################################
# Create the IPV severity index as the mean of selected items, handling missing values
ipv_df_consent_cash <- ipv_df_consent_cash %>%
  # Compute the row-wise mean of the selected IPV variables
  mutate(!!sym(curr_ipv) := apply(select(., paste0(scale_vars,"c")), 1, sum, na.rm = TRUE)) %>%
  mutate(!!sym(curr_ipv) := !!sym(curr_ipv)/length(scale_vars)) %>%
  # Replace values with 0 if any of the selected variables are 0 for a given row
  mutate(!!sym(curr_ipv) := ifelse(rowSums(select(., all_of(c(paste0(scale_vars, "a"), paste0(scale_vars, "b")))) == 0, na.rm = TRUE) > 0 & is.na(!!sym(curr_ipv)), 0,!!sym(curr_ipv)))

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
  mutate(!!sym(curr_ipv) := ((!!sym(curr_ipv) - mean_glob) / std_glob) %>%
           structure(label = lab_curr_ipv)) %>%
  # Rename an existing variable (to make sure the source name is correct)
  rename(phy_ipv_sev_12m = phy_ipv_12m)

######################### Productive Inclusion ####################################
# Create the IPV severity index as the mean of selected items, handling missing values
ipv_df_consent_pi <- ipv_df_consent_pi %>%
  # Compute the row-wise mean of the selected IPV variables
  mutate(!!sym(curr_ipv) := apply(select(., paste0(scale_vars,"c")), 1, sum, na.rm = TRUE)) %>%
  mutate(!!sym(curr_ipv) := !!sym(curr_ipv)/length(scale_vars)) %>%
  # Replace values with 0 if any of the selected variables are 0 for a given row
  mutate(!!sym(curr_ipv) := ifelse(rowSums(select(., all_of(c(paste0(scale_vars, "a"), paste0(scale_vars, "b")))) == 0, na.rm = TRUE) > 0 & is.na(!!sym(curr_ipv)), 0,!!sym(curr_ipv)))

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
  mutate(!!sym(curr_ipv) := ((!!sym(curr_ipv) - mean_glob) / std_glob) %>%
           structure(label = lab_curr_ipv)) %>%
  # Rename an existing variable (to make sure the source name is correct)
  rename(phy_ipv_sev_12m = phy_ipv_12m)


################################################################################
######################## Sexual violence #######################################
################################################################################

# Define the variables to include in the IPV severity index
scale_vars = scale_vars = c("ipv18","ipv19","ipv20")

# Set the name of the new index variable to be created
curr_ipv = "sex_ipv_sev_index"
lab_curr_ipv = "Sex. IPV severity index"

######################### Tekavoul (Cash Transfer) ####################################
# Create the IPV severity index as the mean of selected items, handling missing values
ipv_df_consent_cash <- ipv_df_consent_cash %>%
  # Compute the row-wise mean of the selected IPV variables
  mutate(!!sym(curr_ipv) := apply(select(., paste0(scale_vars,"c")), 1, sum, na.rm = TRUE)) %>%
  mutate(!!sym(curr_ipv) := !!sym(curr_ipv)/length(scale_vars)) %>%
  # Replace values with 0 if any of the selected variables are 0 for a given row
  mutate(!!sym(curr_ipv) := ifelse(rowSums(select(., all_of(c(paste0(scale_vars, "a"), 
                                                              paste0(scale_vars, "b")))) == 0, na.rm = TRUE) > 0 & is.na(!!sym(curr_ipv)), 0,!!sym(curr_ipv)))

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
  mutate(!!sym(curr_ipv) := ((!!sym(curr_ipv) - mean_glob) / std_glob) %>%
           structure(label = lab_curr_ipv)) %>%
  # Rename an existing variable (to make sure the source name is correct)
  rename(sex_ipv_sev_12m = sex_ipv_12m)

######################### Productive Inclusion ####################################
# Create the IPV severity index as the mean of selected items, handling missing values
ipv_df_consent_pi <- ipv_df_consent_pi %>%
  # Compute the row-wise mean of the selected IPV variables
  mutate(!!sym(curr_ipv) := apply(select(., paste0(scale_vars,"c")), 1, sum, na.rm = TRUE)) %>%
  mutate(!!sym(curr_ipv) := !!sym(curr_ipv)/length(scale_vars)) %>%
  # Replace values with 0 if any of the selected variables are 0 for a given row
  mutate(!!sym(curr_ipv) := ifelse(rowSums(select(., all_of(c(paste0(scale_vars, "a"), paste0(scale_vars, "b")))) == 0, na.rm = TRUE) > 0 & is.na(!!sym(curr_ipv)), 0,!!sym(curr_ipv)))

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
  mutate(!!sym(curr_ipv) := ((!!sym(curr_ipv) - mean_glob) / std_glob) %>%
           structure(label = lab_curr_ipv)) %>%
  # Rename an existing variable (to make sure the source name is correct)
  rename(sex_ipv_sev_12m = sex_ipv_12m)


##################################################################################
######################## Sexual and Physical violence ############################
##################################################################################



# Define the variables to include in the IPV severity index
scale_vars = scale_vars = c("ipv12","ipv13","ipv14","ipv15","ipv16","ipv17", # Physical 
                             "ipv18","ipv19","ipv20") # Sexual

# Set the name of the new index variable to be created
curr_ipv = "sex_phy_ipv_sev_index"
lab_curr_ipv = "Phy. and sex. IPV severity index"

######################### Tekavoul (Cash Transfer) ####################################

ipv_df_consent_cash <- ipv_df_consent_cash %>%
  # Compute the row-wise mean of the selected IPV variables
  mutate(sex_phy_ipv_sev_12m = ifelse(sex_ipv_sev_12m==1  |  phy_ipv_sev_12m==1, 1, 0)) 

# Create the IPV severity index as the mean of selected items, handling missing values
ipv_df_consent_cash <- ipv_df_consent_cash %>%
  # Compute the row-wise mean of the selected IPV variables
  mutate(!!sym(curr_ipv) := apply(select(., paste0(scale_vars,"c")), 1, sum, na.rm = TRUE)) %>%
  mutate(!!sym(curr_ipv) := !!sym(curr_ipv)/length(scale_vars)) %>%
  # Replace values with 0 if any of the selected variables are 0 for a given row
  mutate(!!sym(curr_ipv) := ifelse(rowSums(select(., all_of(c(paste0(scale_vars, "a"), 
                                                              paste0(scale_vars, "b")))) == 0, na.rm = TRUE) > 0 & is.na(!!sym(curr_ipv)), 0,!!sym(curr_ipv)))

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
  mutate(!!sym(curr_ipv) := ((!!sym(curr_ipv) - mean_glob) / std_glob) %>%
           structure(label = lab_curr_ipv)) 

######################### Productive Inclusion ####################################

ipv_df_consent_pi <- ipv_df_consent_pi %>%
  # Compute the row-wise mean of the selected IPV variables
  mutate(sex_phy_ipv_sev_12m = ifelse(sex_ipv_sev_12m==1  |  phy_ipv_sev_12m==1, 1, 0)) 

# Create the IPV severity index as the mean of selected items, handling missing values
ipv_df_consent_pi <- ipv_df_consent_pi %>%
  # Compute the row-wise mean of the selected IPV variables
  mutate(!!sym(curr_ipv) := apply(select(., paste0(scale_vars,"c")), 1, sum, na.rm = TRUE)) %>%
  mutate(!!sym(curr_ipv) := !!sym(curr_ipv)/length(scale_vars)) %>%
  # Replace values with 0 if any of the selected variables are 0 for a given row
  mutate(!!sym(curr_ipv) := ifelse(rowSums(select(., all_of(c(paste0(scale_vars, "a"), paste0(scale_vars, "b")))) == 0, na.rm = TRUE) > 0 & is.na(!!sym(curr_ipv)), 0,!!sym(curr_ipv)))

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
  mutate(!!sym(curr_ipv) := ((!!sym(curr_ipv) - mean_glob) / std_glob) %>%
           structure(label = lab_curr_ipv)) 


##################################################################################
######################## Economic violence #######################################
##################################################################################

# Define the variables to include in the IPV severity index
scale_vars = c("ipv22","ipv23","ipv25")

# Set the name of the new index variable to be created
curr_ipv = "eco_ipv_sev_index"
lab_curr_ipv = "Eco. IPV severity index"

######################### Tekavoul (Cash Transfer) ####################################
# Create the IPV severity index as the mean of selected items, handling missing values
ipv_df_consent_cash <- ipv_df_consent_cash %>%
  # Compute the row-wise mean of the selected IPV variables
  mutate(!!sym(curr_ipv) := apply(select(., paste0(scale_vars,"c")), 1, sum, na.rm = TRUE)) %>%
  mutate(!!sym(curr_ipv) := !!sym(curr_ipv)/length(scale_vars)) %>%
  # Replace values with 0 if any of the selected variables are 0 for a given row
  mutate(!!sym(curr_ipv) := ifelse(rowSums(select(., all_of(c(paste0(scale_vars, "a"), paste0(scale_vars, "b")))) == 0, na.rm = TRUE) > 0 & is.na(!!sym(curr_ipv)), 0,!!sym(curr_ipv)))

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
  mutate(!!sym(curr_ipv) := ((!!sym(curr_ipv) - mean_glob) / std_glob) %>%
           structure(label = lab_curr_ipv)) %>%
  # Rename an existing variable (to make sure the source name is correct)
  rename(eco_ipv_sev_12m = eco_ipv_12m)

######################### Productive Inclusion ####################################
# Create the IPV severity index as the mean of selected items, handling missing values
ipv_df_consent_pi <- ipv_df_consent_pi %>%
  # Compute the row-wise mean of the selected IPV variables
  mutate(!!sym(curr_ipv) := apply(select(., paste0(scale_vars,"c")), 1, sum, na.rm = TRUE)) %>%
  mutate(!!sym(curr_ipv) := !!sym(curr_ipv)/length(scale_vars)) %>%
  # Replace values with 0 if any of the selected variables are 0 for a given row
  mutate(!!sym(curr_ipv) := ifelse(rowSums(select(., all_of(c(paste0(scale_vars, "a"), paste0(scale_vars, "b")))) == 0, na.rm = TRUE) > 0 & is.na(!!sym(curr_ipv)), 0,!!sym(curr_ipv)))

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
  mutate(!!sym(curr_ipv) := ((!!sym(curr_ipv) - mean_glob) / std_glob) %>%
           structure(label = lab_curr_ipv)) %>%
  # Rename an existing variable (to make sure the source name is correct)
  rename(eco_ipv_sev_12m = eco_ipv_12m)

################################################################################
######################## Any type of violence ##################################
################################################################################


# Define the variables to include in the IPV severity index
scale_vars = c("ipv8","ipv9","ipv10",
               "ipv11","ipv12","ipv13","ipv14","ipv15","ipv16","ipv17",
               "ipv18","ipv19","ipv20")

# Set the name of the new index variable to be created
curr_ipv = "all_ipv_sev_index"
lab_curr_ipv = "Any type of IPV (excluding Eco. and control) severity index"

######################### Tekavoul (Cash Transfer) ####################################
# Create the IPV severity index as the mean of selected items, handling missing values
ipv_df_consent_cash <- ipv_df_consent_cash %>%
  # Compute the row-wise mean of the selected IPV variables
  mutate(!!sym(curr_ipv) := apply(select(., paste0(scale_vars,"c")), 1, sum, na.rm = TRUE)) %>%
  mutate(!!sym(curr_ipv) := !!sym(curr_ipv)/length(scale_vars)) %>%
  # Replace values with 0 if any of the selected variables are 0 for a given row
  mutate(!!sym(curr_ipv) := ifelse(rowSums(select(., all_of(c(paste0(scale_vars, "a"), paste0(scale_vars, "b")))) == 0, na.rm = TRUE) > 0 & is.na(!!sym(curr_ipv)), 0,!!sym(curr_ipv)))

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
  mutate(!!sym(curr_ipv) := ((!!sym(curr_ipv) - mean_glob) / std_glob) %>%
           structure(label = lab_curr_ipv)) %>%
  # Rename an existing variable (to make sure the source name is correct)
  rename(all_ipv_sev_12m = ipv_all_12m)


# Age at marriage 
ipv_df_consent_cash <- ipv_df_consent_cash %>%
  mutate(age_at_mrrg = as.numeric(mr2))

ipv_df_consent_cash <- ipv_df_consent_cash %>%
  mutate(age_at_mrrg_above18 = ifelse(age_at_mrrg > 18, 1, 0))

ipv_df_consent_cash <- ipv_df_consent_cash %>%
  mutate(het_age_at_mrrg_above18 = as.factor(age_at_mrrg_above18))


######################### Productive Inclusion ####################################
# Create the IPV severity index as the mean of selected items, handling missing values
ipv_df_consent_pi <- ipv_df_consent_pi %>%
  # Compute the row-wise mean of the selected IPV variables
  mutate(!!sym(curr_ipv) := apply(select(., paste0(scale_vars,"c")), 1, sum, na.rm = TRUE)) %>%
  mutate(!!sym(curr_ipv) := !!sym(curr_ipv)/length(scale_vars)) %>%
  # Replace values with 0 if any of the selected variables are 0 for a given row
  mutate(!!sym(curr_ipv) := ifelse(rowSums(select(., all_of(c(paste0(scale_vars, "a"), paste0(scale_vars, "b")))) == 0, na.rm = TRUE) > 0 & is.na(!!sym(curr_ipv)), 0,!!sym(curr_ipv)))

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
  mutate(!!sym(curr_ipv) := ((!!sym(curr_ipv) - mean_glob) / std_glob) %>%
           structure(label = lab_curr_ipv)) %>%
  # Rename an existing variable (to make sure the source name is correct)
  rename(all_ipv_sev_12m = ipv_all_12m)



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
# Heterogeneity variables for married after median age
# ------------------------------------------------------------------------------

ipv_df_consent <- plyr::rbind.fill(ipv_df_consent_cash %>% select(-c(treatment_pi_pool)),
                                   ipv_df_consent_pi%>% select(-c(treatment_csh_trnsfr)))


# ------------------------------------------------------------------------------
# Merging the two ipv df
# ------------------------------------------------------------------------------

ipv_df_consent <- ipv_df_consent %>% 
  mutate(het_age_at_mrrg = as.factor(ifelse(age_at_mrrg > median(age_at_mrrg, na.rm = TRUE), 1, 0)),
         age_at_mrrg = as.numeric(age_at_mrrg))

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
  summarise(total_annual_expenditure = sum(consum_nf_annual, na.rm = TRUE), .groups = 'drop')

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
           structure(label = "Total male expenditures"),
         female_expenditure = female_expenditure %>%
           structure(label = "Total female expenditures"),
         other_expenditure = other_expenditure %>%
           structure(label = "Total other expenditures"),
         hhid = as.character(hhid)
         ) %>% 
  mutate(across(everything(), ~ replace_na(., 0)))


################################################################################
######################## Merging and exporting #################################
################################################################################

# Including the treatment variable
# strata,cluster,p_region,p_commune,p_village,region,commune,
#village,soc_prmtn,cash_trsfr,
# select(hhid,contains("treat"), reg_hh_csh_mrt,reg_hh_pi_mrt,
#        strata,cluster,p_region,p_commune,p_village,region,commune, village,phase, hhh_fem_bl, mem_n_bl,
#        pben_relation, # Productive beneficiary relationship to household head
#        ctrl_earn_index_tr_bl, ctrl_earn_index, ctrl_hh_index_tr_bl, 
#        ctrl_hh_index, intrahh_vars_index_tr_bl, intrahh_index, 
#        rev_sum_bohh_wemp_98_tr_bl, rev_sum_bohh,hhh_lit, het_pben_lit,
#        age_gap, het_hhh_lit, het_ctrl_earn_index, het_ctrl_hh_index,
#        ctrl_hh_index, intrahh_index, rev_sum_bohh,
#        het_intrahh_index, het_rev_sum_bohh, het_age_gap,
#        pben_handicap, het_pben_handicap, pben_poly, het_pben_poly, 
#        hhh_edu, het_hhh_edu, hhh_fem, het_hhh_fem, hhh_age_bl,
#        pben_lit, het_pben_lit, save_share_d, cash_trsfr, pben_age_bl,
#        het_save_share, save_share,pben_age,hhh_age,
#        pben_edu, het_pben_edu, hhh_edu, het_hhh_edu,
#        ends_with("_bl"), # For the ML algorithm
#        same_cb,pben_fem,hhh_poly,hhh_prim_bl,pben_prim_bl,
#        mem_n,hou_room_bl,hou_hea_min_bl,hou_mar_min_bl,hou_wat_min_bl
#        ) %>% 
# , hhh_fem_bl, mem_n_bl, pben_relation, # Productive beneficiary relationship to household head
# ctrl_earn_index_tr_bl, ctrl_earn_index, ctrl_hh_index_tr_bl, ctrl_hh_index, 
# intrahh_vars_index_tr_bl, intrahh_index, rev_sum_bohh_wemp_98_tr_bl, rev_sum_bohh,hhh_lit,
# age_gap, ctrl_hh_index, intrahh_index, rev_sum_bohh, pben_handicap, 
# pben_poly, hhh_edu, het_hhh_edu, hhh_fem, hhh_age_bl, pben_lit, het_pben_lit, 
# save_share_d, cash_trsfr, pben_age_bl, save_share,pben_age,hhh_age, pben_edu, 
# hhh_edu, same_cb,pben_fem,hhh_poly,hhh_prim_bl,pben_prim_bl, mem_n,hou_room_bl,
# hou_hea_min_bl,hou_wat_min_bl


followup_MRT_hh_and_ipv_df <- ipv_df_consent %>% 
  # Filtering to keep the individual of interest
  filter(reg_hh_pi_mrt==1 | reg_hh_csh_mrt==1) %>% 
  left_join(followup_MRT_hh %>% 
              filter(hhid %in% lst_hh) %>% 
            select(-c(reg_hh_pi_mrt, reg_hh_csh_mrt, treatment_csh_trnsfr, treatment_pi_pool)) %>% 
            mutate(hhid=as.character(hhid)), 
            by="hhid") %>% 
  left_join(nonfood_long, by="hhid") %>% 
  mutate(pben_relation_bl=labelled::unlabelled(pben_relation_bl),
         male_expenditure = male_expenditure/mem_n,
         female_expenditure = female_expenditure/mem_n,
         other_expenditure = other_expenditure/mem_n
         )



# Binary for relation to beneficiary
followup_MRT_hh_and_ipv_df <- dummy_cols(followup_MRT_hh_and_ipv_df, select_columns = "pben_relation_bl", remove_first_dummy = FALSE)

followup_MRT_hh_and_ipv_df <- followup_MRT_hh_and_ipv_df %>% 
  mutate(pben_relation_bl_other = ifelse( (`pben_relation_bl_Head of household`==0) & (`pben_relation_bl_Spouse`==0), 1, 0))


# keep even those who didn't give their consent for the IPV module.
ipv_df_all <- ipv_df_all %>% 
  filter(!is.na(ipv_completed)) %>% 
  distinct(hhid, .keep_all = TRUE) %>% # there are some duplicateds row with the same 
  # in the ipv_df, I consider the first row only
  right_join(followup_MRT_hh %>% 
              select(hhid,contains("treat"), reg_hh_csh_mrt,reg_hh_pi_mrt,strata, cluster, treatment_pi_spill)%>% 
               mutate(hhid=as.character(hhid)), 
                      by="hhid", 
                      multiple="first")



# Exporting the data in R format
write_rds(baseline_MRT_hh, "output/data/baseline_MRT_hh.rds")
write_rds(allrounds_MRT_hh, "output/data/allrounds_MRT_hh.rds")
write_rds(followup_MRT_hh, "output/data/followup_MRT_hh.rds")
write_rds(followup_MRT_hh_and_ipv_df, "output/data/followup_MRT_hh_and_ipv_MRT_hh.rds")
write_rds(ipv_df_all, "output/data/ipv_df_all.rds")
write_rds(ipv_df_consent, "output/data/ipv_df_consent.rds")


##################################################################################
######################## Spillover effects #######################################
##################################################################################

# Filter for phase 2 data (follow-up)
followup_MRT_hh_spill <- followup_MRT_hh 

ipv_df_spill <- ipv_df_all %>% filter(!is.na(treatment_pi_spill))  %>% filter(ipv_completed==1) 

followup_MRT_hh_spill <- ipv_df_spill %>% 
  left_join(followup_MRT_hh_spill %>% 
              select(-c(reg_hh_pi_mrt, reg_hh_csh_mrt, treatment_csh_trnsfr, 
                        treatment_pi_pool, treatment_pi_spill, contains("treat"))) %>% 
              mutate(hhid=as.character(hhid)), 
            by="hhid") %>% 
  # Filtering to keep the individual of interest
  filter(!is.na(treatment_pi_spill)) %>% 
  filter(pben_age_bl <= 49) %>% 
  filter(spouse_in_bl==1 & spouse_in_fl==1)

followup_MRT_hh_spill <- followup_MRT_hh_spill %>% 
  mutate(treatment_pi_pool_spill = ifelse(treatment_pi_spill=="Control",0,1)) 

#### For the pool productive inclusion program

followup_MRT_hh_spill <- followup_MRT_hh_spill %>% 
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


######################### Productive Inclusion ####################################
# Create the IPV severity index as the mean of selected items, handling missing values
followup_MRT_hh_spill <- followup_MRT_hh_spill %>%
  # Compute the row-wise mean of the selected IPV variables
  mutate(!!sym(curr_ipv) := apply(select(., scale_vars), 1, sum, na.rm = TRUE)) %>%
  mutate(!!sym(curr_ipv) := !!sym(curr_ipv)/length(scale_vars)) %>%
  # Replace values with NA if all selected variables are NA for a given row
  mutate(!!sym(curr_ipv) := ifelse(rowSums(!is.na(select(., all_of(scale_vars)))) == 0, NA, !!sym(curr_ipv)))

# Calculate the global mean of the index among the control groups
mean_glob = mean(followup_MRT_hh_spill %>% 
                   filter(treatment_pi_pool_spill == "Control") %>% 
                   select(!!sym(curr_ipv)) %>% 
                   pull(), na.rm = TRUE)

# Calculate the global standard deviation of the index among the control groups
std_glob = sd(followup_MRT_hh_spill %>% 
                filter(treatment_pi_pool_spill == "Control") %>% 
                select(!!sym(curr_ipv)) %>% 
                pull(), na.rm = TRUE)

# Standardize the IPV index using the global mean and standard deviation, and add a label
followup_MRT_hh_spill <- followup_MRT_hh_spill %>%
  mutate(!!sym(curr_ipv) := ((!!sym(curr_ipv) - mean_glob) / std_glob) %>%
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

######################### Productive Inclusion ####################################
# Create the IPV severity index as the mean of selected items, handling missing values
followup_MRT_hh_spill <- followup_MRT_hh_spill %>%
  # Compute the row-wise mean of the selected IPV variables
  mutate(!!sym(curr_ipv) := apply(select(., paste0(scale_vars,"c")), 1, sum, na.rm = TRUE)) %>%
  mutate(!!sym(curr_ipv) := !!sym(curr_ipv)/length(scale_vars)) %>%
  # Replace values with 0 if any of the selected variables are 0 for a given row
  mutate(!!sym(curr_ipv) := ifelse(rowSums(select(., all_of(c(paste0(scale_vars, "a"), paste0(scale_vars, "b")))) == 0, na.rm = TRUE) > 0 & is.na(!!sym(curr_ipv)), 0,!!sym(curr_ipv)))

# Calculate the global mean of the index among the control groups
mean_glob = mean(followup_MRT_hh_spill %>% 
                   filter(treatment_pi_pool_spill == "Control") %>% 
                   select(!!sym(curr_ipv)) %>% 
                   pull(), na.rm = TRUE)

# Calculate the global standard deviation of the index among the control groups
std_glob = sd(followup_MRT_hh_spill %>% 
                filter(treatment_pi_pool_spill == "Control") %>% 
                select(!!sym(curr_ipv)) %>% 
                pull(), na.rm = TRUE)

# Standardize the IPV index using the global mean and standard deviation, and add a label
followup_MRT_hh_spill <- followup_MRT_hh_spill %>%
  mutate(!!sym(curr_ipv) := ((!!sym(curr_ipv) - mean_glob) / std_glob) %>%
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

######################### Productive Inclusion ####################################
# Create the IPV severity index as the mean of selected items, handling missing values
followup_MRT_hh_spill <- followup_MRT_hh_spill %>%
  # Compute the row-wise mean of the selected IPV variables
  mutate(!!sym(curr_ipv) := apply(select(., paste0(scale_vars,"c")), 1, sum, na.rm = TRUE)) %>%
  mutate(!!sym(curr_ipv) := !!sym(curr_ipv)/length(scale_vars)) %>%
  # Replace values with 0 if any of the selected variables are 0 for a given row
  mutate(!!sym(curr_ipv) := ifelse(rowSums(select(., all_of(c(paste0(scale_vars, "a"), paste0(scale_vars, "b")))) == 0, na.rm = TRUE) > 0 & is.na(!!sym(curr_ipv)), 0,!!sym(curr_ipv)))

# Calculate the global mean of the index among the control groups
mean_glob = mean(followup_MRT_hh_spill %>% 
                   filter(treatment_pi_pool_spill == "Control") %>% 
                   select(!!sym(curr_ipv)) %>% 
                   pull(), na.rm = TRUE)

# Calculate the global standard deviation of the index among the control groups
std_glob = sd(followup_MRT_hh_spill %>% 
                filter(treatment_pi_pool_spill == "Control") %>% 
                select(!!sym(curr_ipv)) %>% 
                pull(), na.rm = TRUE)

# Standardize the IPV index using the global mean and standard deviation, and add a label
followup_MRT_hh_spill <- followup_MRT_hh_spill %>%
  mutate(!!sym(curr_ipv) := ((!!sym(curr_ipv) - mean_glob) / std_glob) %>%
           structure(label = lab_curr_ipv)) %>%
  # Rename an existing variable (to make sure the source name is correct)
  rename(phy_ipv_sev_12m = phy_ipv_12m)


################################################################################
######################## Sexual violence #######################################
################################################################################

# Define the variables to include in the IPV severity index
scale_vars = scale_vars = c("ipv18","ipv19","ipv20")

# Set the name of the new index variable to be created
curr_ipv = "sex_ipv_sev_index"
lab_curr_ipv = "Sex. IPV severity index"

######################### Productive Inclusion ####################################
# Create the IPV severity index as the mean of selected items, handling missing values
followup_MRT_hh_spill <- followup_MRT_hh_spill %>%
  # Compute the row-wise mean of the selected IPV variables
  mutate(!!sym(curr_ipv) := apply(select(., paste0(scale_vars,"c")), 1, sum, na.rm = TRUE)) %>%
  mutate(!!sym(curr_ipv) := !!sym(curr_ipv)/length(scale_vars)) %>%
  # Replace values with 0 if any of the selected variables are 0 for a given row
  mutate(!!sym(curr_ipv) := ifelse(rowSums(select(., all_of(c(paste0(scale_vars, "a"), paste0(scale_vars, "b")))) == 0, na.rm = TRUE) > 0 & is.na(!!sym(curr_ipv)), 0,!!sym(curr_ipv)))

# Calculate the global mean of the index among the control groups
mean_glob = mean(followup_MRT_hh_spill %>% 
                   filter(treatment_pi_pool_spill == "Control") %>% 
                   select(!!sym(curr_ipv)) %>% 
                   pull(), na.rm = TRUE)

# Calculate the global standard deviation of the index among the control groups
std_glob = sd(followup_MRT_hh_spill %>% 
                filter(treatment_pi_pool_spill == "Control") %>% 
                select(!!sym(curr_ipv)) %>% 
                pull(), na.rm = TRUE)

# Standardize the IPV index using the global mean and standard deviation, and add a label
followup_MRT_hh_spill <- followup_MRT_hh_spill %>%
  mutate(!!sym(curr_ipv) := ((!!sym(curr_ipv) - mean_glob) / std_glob) %>%
           structure(label = lab_curr_ipv)) %>%
  # Rename an existing variable (to make sure the source name is correct)
  rename(sex_ipv_sev_12m = sex_ipv_12m)


##################################################################################
######################## Sexual and Physical violence ############################
##################################################################################



# Define the variables to include in the IPV severity index
scale_vars = scale_vars = c("ipv12","ipv13","ipv14","ipv15","ipv16","ipv17", # Physical 
                            "ipv18","ipv19","ipv20") # Sexual

# Set the name of the new index variable to be created
curr_ipv = "sex_phy_ipv_sev_index"
lab_curr_ipv = "Phy. and sex. IPV severity index"

######################### Productive Inclusion ####################################

followup_MRT_hh_spill <- followup_MRT_hh_spill %>%
  # Compute the row-wise mean of the selected IPV variables
  mutate(sex_phy_ipv_sev_12m = ifelse(sex_ipv_sev_12m==1  |  phy_ipv_sev_12m==1, 1, 0)) 

# Create the IPV severity index as the mean of selected items, handling missing values
followup_MRT_hh_spill <- followup_MRT_hh_spill %>%
  # Compute the row-wise mean of the selected IPV variables
  mutate(!!sym(curr_ipv) := apply(select(., paste0(scale_vars,"c")), 1, sum, na.rm = TRUE)) %>%
  mutate(!!sym(curr_ipv) := !!sym(curr_ipv)/length(scale_vars)) %>%
  # Replace values with 0 if any of the selected variables are 0 for a given row
  mutate(!!sym(curr_ipv) := ifelse(rowSums(select(., all_of(c(paste0(scale_vars, "a"), paste0(scale_vars, "b")))) == 0, na.rm = TRUE) > 0 & is.na(!!sym(curr_ipv)), 0,!!sym(curr_ipv)))

# Calculate the global mean of the index among the control groups
mean_glob = mean(followup_MRT_hh_spill %>% 
                   filter(treatment_pi_pool_spill == "Control") %>% 
                   select(!!sym(curr_ipv)) %>% 
                   pull(), na.rm = TRUE)

# Calculate the global standard deviation of the index among the control groups
std_glob = sd(followup_MRT_hh_spill %>% 
                filter(treatment_pi_pool_spill == "Control") %>% 
                select(!!sym(curr_ipv)) %>% 
                pull(), na.rm = TRUE)

# Standardize the IPV index using the global mean and standard deviation, and add a label
followup_MRT_hh_spill <- followup_MRT_hh_spill %>%
  mutate(!!sym(curr_ipv) := ((!!sym(curr_ipv) - mean_glob) / std_glob) %>%
           structure(label = lab_curr_ipv)) 


##################################################################################
######################## Economic violence #######################################
##################################################################################

# Define the variables to include in the IPV severity index
scale_vars = c("ipv22","ipv23","ipv25")

# Set the name of the new index variable to be created
curr_ipv = "eco_ipv_sev_index"
lab_curr_ipv = "Eco. IPV severity index"

######################### Productive Inclusion ####################################
# Create the IPV severity index as the mean of selected items, handling missing values
followup_MRT_hh_spill <- followup_MRT_hh_spill %>%
  # Compute the row-wise mean of the selected IPV variables
  mutate(!!sym(curr_ipv) := apply(select(., paste0(scale_vars,"c")), 1, sum, na.rm = TRUE)) %>%
  mutate(!!sym(curr_ipv) := !!sym(curr_ipv)/length(scale_vars)) %>%
  # Replace values with 0 if any of the selected variables are 0 for a given row
  mutate(!!sym(curr_ipv) := ifelse(rowSums(select(., all_of(c(paste0(scale_vars, "a"), paste0(scale_vars, "b")))) == 0, na.rm = TRUE) > 0 & is.na(!!sym(curr_ipv)), 0,!!sym(curr_ipv)))

# Calculate the global mean of the index among the control groups
mean_glob = mean(followup_MRT_hh_spill %>% 
                   filter(treatment_pi_pool_spill == "Control") %>% 
                   select(!!sym(curr_ipv)) %>% 
                   pull(), na.rm = TRUE)

# Calculate the global standard deviation of the index among the control groups
std_glob = sd(followup_MRT_hh_spill %>% 
                filter(treatment_pi_pool_spill == "Control") %>% 
                select(!!sym(curr_ipv)) %>% 
                pull(), na.rm = TRUE)

# Standardize the IPV index using the global mean and standard deviation, and add a label
followup_MRT_hh_spill <- followup_MRT_hh_spill %>%
  mutate(!!sym(curr_ipv) := ((!!sym(curr_ipv) - mean_glob) / std_glob) %>%
           structure(label = lab_curr_ipv)) %>%
  # Rename an existing variable (to make sure the source name is correct)
  rename(eco_ipv_sev_12m = eco_ipv_12m)

################################################################################
######################## Any type of violence ##################################
################################################################################


# Define the variables to include in the IPV severity index
scale_vars = c("ipv8","ipv9","ipv10",
               "ipv11","ipv12","ipv13","ipv14","ipv15","ipv16","ipv17",
               "ipv18","ipv19","ipv20")

# Set the name of the new index variable to be created
curr_ipv = "all_ipv_sev_index"
lab_curr_ipv = "Any type of IPV (excluding Eco. and control) severity index"

######################### Productive Inclusion ####################################
# Create the IPV severity index as the mean of selected items, handling missing values
followup_MRT_hh_spill <- followup_MRT_hh_spill %>%
  # Compute the row-wise mean of the selected IPV variables
  mutate(!!sym(curr_ipv) := apply(select(., paste0(scale_vars,"c")), 1, sum, na.rm = TRUE)) %>%
  mutate(!!sym(curr_ipv) := !!sym(curr_ipv)/length(scale_vars)) %>%
  # Replace values with 0 if any of the selected variables are 0 for a given row
  mutate(!!sym(curr_ipv) := ifelse(rowSums(select(., all_of(c(paste0(scale_vars, "a"), paste0(scale_vars, "b")))) == 0, na.rm = TRUE) > 0 & is.na(!!sym(curr_ipv)), 0,!!sym(curr_ipv)))

# Calculate the global mean of the index among the control groups
mean_glob = mean(followup_MRT_hh_spill %>% 
                   filter(treatment_pi_pool_spill == "Control") %>% 
                   select(!!sym(curr_ipv)) %>% 
                   pull(), na.rm = TRUE)

# Calculate the global standard deviation of the index among the control groups
std_glob = sd(followup_MRT_hh_spill %>% 
                filter(treatment_pi_pool_spill == "Control") %>% 
                select(!!sym(curr_ipv)) %>% 
                pull(), na.rm = TRUE)

# Standardize the IPV index using the global mean and standard deviation, and add a label
followup_MRT_hh_spill <- followup_MRT_hh_spill %>%
  mutate(!!sym(curr_ipv) := ((!!sym(curr_ipv) - mean_glob) / std_glob) %>%
           structure(label = lab_curr_ipv)) %>%
  # Rename an existing variable (to make sure the source name is correct)
  rename(all_ipv_sev_12m = ipv_all_12m)


############################## Saving ###########################################
write_rds(followup_MRT_hh_spill, "output/data/followup_MRT_hh_spill_and_ipv_MRT.rds")








# 
# 
# # Convert cash transfer treatment to factor
# followup_MRT_hh_and_ipv_df$married_ben_bl <- factor(followup_MRT_hh_and_ipv_df$married_ben_bl, levels = c("0", "1"), labels = c("Unmarried", "Married")) 
# followup_MRT_hh_and_ipv_df$married_ben_bl <- relevel(followup_MRT_hh_and_ipv_df$married_ben_bl, ref = "Unmarried") # Set the reference var.
# 
# followup_MRT_hh_and_ipv_df$married_ben <- factor(followup_MRT_hh_and_ipv_df$married_ben, levels = c("0", "1"), labels = c("Unmarried", "Married")) 
# followup_MRT_hh_and_ipv_df$married_ben <- relevel(followup_MRT_hh_and_ipv_df$married_ben, ref = "Unmarried") # Set the reference var.
# 
# 
# followup_MRT_hh_pi <- followup_MRT_hh_and_ipv_df %>% filter(reg_hh_pi_mrt == 1)
# followup_MRT_hh_csh <- followup_MRT_hh_and_ipv_df %>% filter(reg_hh_csh_mrt == 1)
# 
# 
# 
# # Create summary statistics table grouped by treatment
# followup_MRT_hh_pi %>% 
#   dplyr::select(married_ben,married_ben_bl) %>%  
#   tbl_summary(by = married_ben_bl,
#               type = (list="married_ben" ~ "categorical"),
#               label = list(married_ben = "Ben. status at followup"),
#               statistic = list(all_continuous() ~ "{mean}({sd})"),
#               digits = everything() ~ c(0,0), 
#               missing = "always") %>% 
#   modify_header(label = 'Ben. status at baseline')
# 
# # Create summary statistics table grouped by treatment
# followup_MRT_hh_csh %>% 
#   dplyr::select(married_ben,married_ben_bl) %>%  
#   tbl_summary(by = married_ben_bl,
#               type = (list="married_ben" ~ "categorical"),
#               label = list(married_ben = "Ben. status at followup"),
#               statistic = list(all_continuous() ~ "{mean}({sd})"),
#               digits = everything() ~ c(0,0), 
#               missing = "no") %>% 
#   modify_header(label = 'Ben. status at baseline')
# 
# followup_MRT_hh  <- followup_MRT_hh %>% 
#   filter(reg_hh_csh_mrt == 1 | reg_hh_pi_mrt == 1) %>% 
#   filter(hhid %in% lst_hh) %>% 
#   mutate(pben_relation_bl = labelled::unlabelled(pben_relation_bl),
#          pben_relation = labelled::unlabelled(pben_relation))
# 
# followup_MRT_hh <- dummy_cols(followup_MRT_hh, select_columns = "pben_relation_bl", remove_first_dummy = FALSE)
# followup_MRT_hh <- dummy_cols(followup_MRT_hh, select_columns = "pben_relation", remove_first_dummy = FALSE)
# 
# # Convert cash transfer treatment to factor
# followup_MRT_hh$married_ben_bl <- factor(followup_MRT_hh$married_ben_bl, levels = c("0", "1"), labels = c("Unmarried", "Married")) 
# followup_MRT_hh$married_ben_bl <- relevel(followup_MRT_hh$married_ben_bl, ref = "Unmarried") # Set the reference var.
# 
# followup_MRT_hh$married_ben <- factor(followup_MRT_hh$married_ben, levels = c("0", "1"), labels = c("Unmarried", "Married")) 
# followup_MRT_hh$married_ben <- relevel(followup_MRT_hh$married_ben, ref = "Unmarried") # Set the reference var.
# 
# followup_MRT_hh_pi <- followup_MRT_hh %>% filter(reg_hh_pi_mrt == 1)
# followup_MRT_hh_csh <- followup_MRT_hh %>% filter(reg_hh_csh_mrt == 1)
# 
# 
# # Create summary statistics table grouped by treatment
# followup_MRT_hh_pi %>% 
#   dplyr::select(married_ben,married_ben_bl) %>%  
#   tbl_summary(by = married_ben_bl,
#               type = (list="married_ben" ~ "categorical"),
#               label = list(married_ben = "Ben. status at followup"),
#               statistic = list(all_continuous() ~ "{mean}({sd})"),
#               digits = everything() ~ c(0,0), 
#               missing = "always") %>% 
#   modify_header(label = 'Ben. status at baseline')
# 
# 
# var_vec <- c( "pben_relation_bl_Head of household",
#                          "pben_relation_bl_Spouse",
#                          "pben_relation_bl_Co-wife",
#                          "pben_relation_bl_Son, Daughter",
#                          "pben_relation_bl_Son, Daughter of co-wife",
#                          "pben_relation_bl_Father, Mother",
#                          "pben_relation_bl_Little child",
#                          "pben_relation_bl_Grandparents",
#                          "pben_relation_bl_Sibling",
#                          "pben_relation_bl_Cousin",
#                          "pben_relation_bl_Nephew/Niece",
#                          "pben_relation_bl_Spouse of son/daughter (or in-laws')",
#                          "pben_relation_bl_Spouse of brother/sister (or in-law)",
#                          "pben_relation_bl_Parent in law",
#                          "pben_relation_bl_Other Parents of chef/spouse",
#                          "pben_relation_bl_Unrelated",
#                          "pben_relation_bl_Servant/their relative",
#                          "pben_relation_bl_1st wife",
#                          "pben_relation_bl_2nd wife",
#                          "pben_relation_bl_3rd wife",
#                          "pben_relation_bl_4th wife",
#                          "pben_relation_bl_Aunt/uncle",
#                          "pben_relation_bl_Spouse's brother/sister",
#                          "pben_relation_bl_Other to be specified",
#                          "pben_relation_bl_to update!")
# 
# var_vec <- c( "pben_relation_Head of household",
#               "pben_relation_Spouse",
#               "pben_relation_Co-wife",
#               "pben_relation_Son, Daughter",
#               "pben_relation_Son, Daughter of co-wife",
#               "pben_relation_Father, Mother",
#               "pben_relation_Little child",
#               "pben_relation_Grandparents",
#               "pben_relation_Sibling",
#               "pben_relation_Cousin",
#               "pben_relation_Nephew/Niece",
#               "pben_relation_Spouse of son/daughter (or in-laws')",
#               "pben_relation_Spouse of brother/sister (or in-law)",
#               "pben_relation_Parent in law",
#               "pben_relation_Other Parents of chef/spouse",
#               "pben_relation_Unrelated",
#               "pben_relation_Servant/their relative",
#               "pben_relation_1st wife",
#               "pben_relation_2nd wife",
#               "pben_relation_3rd wife",
#               "pben_relation_4th wife",
#               "pben_relation_Aunt/uncle",
#               "pben_relation_Spouse's brother/sister",
#               "pben_relation_Other to be specified",
#               "pben_relation_to update!")
# 
# label_vec = c(
#   "- Head of household",
#   "- Spouse",
#   "- Co-wife",
#   "- Son or Daughter",
#   "- Son/Daughter of co-wife",
#   "- Father or Mother",
#   "- Little child",
#   "- Grandparents",
#   "- Sibling",
#   "- Cousin",
#   "- Nephew/Niece",
#   "- Spouse of child / In-laws",
#   "- Spouse of sibling / In-law",
#   "- Parent-in-law",
#   "- Other parents of head/spouse",
#   "- Unrelated",
#   "- Servant or their relative",
#   "- 1st wife",
#   "- 2nd wife",
#   "- 3rd wife",
#   "- 4th wife",
#   "- Aunt or Uncle",
#   "- Spouse's brother/sister",
#   "- Other to be specified",
#   "- To update"
# )
# 
# test <- followup_MRT_hh_pi %>% 
#   filter(married_ben_bl=="Married") %>% 
#   dplyr::select(married_ben,var_vec, workageo_mal_n_bl, workageo_mal_n) 
# 
# # Apply the variable labels to the dataset
# test <- test %>%
#   set_variable_labels(.labels = c(label_vec,'nbr male at baseline age 25-65','nbr male at baseline age 25-65 (followup)'), .strict = FALSE)
# 
# test %>% 
#   tbl_summary(by = married_ben,
#               type = (list=c(var_vec ~ "continuous2",
#                       workageo_mal_n_bl ~ "continuous2", 
#                       workageo_mal_n ~ "continuous2")
#                       ),
#               label = list(
#                            var_vec = label_vec
#                            ),
#               statistic = list(all_continuous() ~ "{mean}({sd})"),
#               digits = everything() ~ c(2,2), 
#               missing = "no") %>% 
#   modify_header(label = 'Ben. status at followup for unmarried at baseline')
# 
# 
# 
# # Create summary statistics table grouped by treatment
# followup_MRT_hh_csh %>% 
#   dplyr::select(married_ben,married_ben_bl) %>%  
#   tbl_summary(by = married_ben_bl,
#               type = (list="married_ben" ~ "categorical"),
#               label = list(married_ben = "Ben. status at followup"),
#               statistic = list(all_continuous() ~ "{mean}({sd})"),
#               digits = everything() ~ c(0,0), 
#               missing = "no") %>% 
#   modify_header(label = 'Ben. status at baseline')
# 
# 
# 
# test <- followup_MRT_hh_csh %>% 
#   filter(married_ben_bl=="Unmarried") %>% 
#   dplyr::select(married_ben,var_vec, workageo_mal_n_bl, workageo_mal_n) 
# 
# # Apply the variable labels to the dataset
# test <- test %>%
#   set_variable_labels(.labels = c(label_vec,'nbr male at baseline age 25-65','nbr male at baseline age 25-65 (followup)'), .strict = FALSE)
# 
# test %>% 
#   tbl_summary(by = married_ben,
#               type = (list=c(var_vec ~ "continuous2",
#                              workageo_mal_n_bl ~ "continuous2", 
#                              workageo_mal_n ~ "continuous2")
#               ),
#               label = list(
#                 var_vec = label_vec
#               ),
#               statistic = list(all_continuous() ~ "{mean}({sd})"),
#               digits = everything() ~ c(2,2), 
#               missing = "no") %>% 
#   modify_header(label = 'Ben. status at followup for unmarried at baseline')

# remove data to free space
rm(list = ls())

