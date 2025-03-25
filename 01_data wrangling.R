################################################################################
######### IPV in Mauritania #################################################
################################################################################

# Data wrangling

######################## Importing library and external files ##################
### List of required packages
required_packages <- c("tidyverse", "dplyr","haven","readr")

### Check if packages are installed
missing_packages <- setdiff(required_packages, installed.packages()[,"Package"])

### Install missing packages
if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

### Load all packages
lapply(required_packages, library, character.only = TRUE)

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


## Importing the geospatial coordinates
coordinates_data <-  read_dta("input/coordinates/coordinates.dta")

## Data on IPV
#ipv_df <- read_dta("input/data/IPV_Maghama_Clean.dta")
ipv_df_all <- read_dta("output/data/ipv_index_all.dta")
ipv_df_consent1 <- read_dta("output/data/ipv_index_consent1.dta")

######################## Cleaning the data #####################################
# Selecting the columns
## Phase column contains information on the survey rounds
## The balance table was performed with the baseline data
## the line follow up in the balance table is the table for attrition (which is tiny regarding the size)
## to be consider as a household of interest, the recipient household member should be age between 18 and 49 (pben_age_bl >= 18 & pben_age_bl <= 49) during the program inplementation


allrounds_MRT_hh <- allrounds_MRT_hh %>% 
                    select(hhid,treatment,treat_dum,strata,cluster,p_region,p_commune,p_village,region,commune,
                           village,phase, consent, reg_hh_mrt, soc_prmtn,cash_trsfr,same_cb,benef_pos,chef_pos,
                           hhh_fem,pben_fem,hhh_age,pben_age,hhh_poly,hhh_handicap,
                           pben_poly,pben_handicap,pben_relation,equiv_n,mem_n,adult_n,
                           adult_fem_s,mem_n,equiv_n,depend_ratio,extend_ratio,baby_n,
                           earlychild_n,olderchild_n, adult_fem_s, hhh_fem, sleep_days_chef,
                           sleep_days_ben, earlychild_n, olderchild_n, workage1_n, workage2_n, workage3_n, 
                           workage4_n, workage5_n, elder_n, workageo_n, workageo_mal_n, 
                           workageo_fem_n, earlychild_s, olderchild_s, workage1_s, 
                           workage2_s, workage3_s, workage4_s, workage5_s, elder_s, 
                           workageo_s, workageo_mal_s, workageo_fem_s,hhh_edu_bl, 
                           pben_edu_bl,pben_prim_bl,hhh_lit_bl,pben_lit_bl,mem_n, mes_coach_d, 
                           mes_avec_d, mes_germe_d, mes_video_d, mes_acv_d, mes_bourse_d,
                           contains("got"), ctrans_d, hhh_fem_bl, mem_n_bl, 
                           save_share_d, # Share of saving own by partner
                           pben_handicap_bl, pben_poly_bl, hhh_fem_bl, pben_edu_bl,
                           hhh_age_bl, pben_age_bl,
                           pben_relation, # Productive beneficiary relationship to household head
                           ctrl_earn_index_tr_bl, #B.10.5 Control Earnings Z-index at bl, 
                           ctrl_hh_index_tr_bl, # B.10.6 Control HH Z-index at bl
                           intrahh_vars_index_tr_bl, # C.2.3.A Intra-HH Dynamics Z-index at Baseline
                           rev_sum_bohh_wemp_98_tr_bl, # beneficiary share of household total rev 98 wins baseline
                           
                           same_cb,pben_fem,hhh_poly,hhh_prim_bl,pben_prim_bl,
                           mem_n,hou_room_bl,hou_hea_min_bl,hou_mar_min_bl,hou_wat_min_bl
                           # dom_relation_index_tr_bl, not baseline level data collected
                           # gender_attitudes_index_tr_bl  not baseline level data collected
                           ) %>% 
  rename(treatment_pi = treatment,
         treat_dum_pi = treat_dum,
         reg_hh_pi_mrt = reg_hh_mrt,
         mes_ctrans_d = ctrans_d) %>% 
  mutate(treatment_pi = treatment_pi %>%
            structure(label = "PI impact evaluation"),
         reg_hh_pi_mrt = reg_hh_pi_mrt %>%
           structure(label = "Filtering condition for PI impact evaluation")
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
           structure(label = "Cash transfer (Tekavoul only)"))

# Create treatment indicator for cash transfer (1 = received transfer, 0 = control)
allrounds_MRT_hh <- allrounds_MRT_hh %>% 
  mutate(treatment_csh_trnsfr = 
           ifelse(reg_hh_csh_mrt==1 & cash_trsfr==1, 1,     # Treatment group
                  ifelse(reg_hh_csh_mrt==1 & cash_trsfr==0, 0, NA)) %>%  # Control group, NA for excluded
           structure(label = "Cash transfer RCT"),
         treatment_csh_trnsfr = factor(treatment_csh_trnsfr, levels = c(0,1),
                                        labels = c("Control","Cash Assignment")) %>%
           structure(label = "Cash transfer (Tekavoul only program)"))

# Repeat the same process for baseline data
# Create regression indicator for baseline households
baseline_MRT_hh <- baseline_MRT_hh %>% 
  mutate(reg_hh_csh_mrt = 
           case_when(
             cash_trsfr==0 & treatment_pi==0 ~ 1,  # Control group for cash transfer
             cash_trsfr==1 & treatment_pi==0 ~ 1,  # Cash transfer recipients in pi control group
             .default = 0                       # All other cases
           )%>%
           structure(label = "Filtering condition for Tekavoul impact evaluation"))

# Create treatment indicator for baseline households
baseline_MRT_hh <- baseline_MRT_hh %>% 
  mutate(treatment_csh_trnsfr = 
           ifelse(reg_hh_csh_mrt==1 & cash_trsfr==1, 1,     # Treatment group
                  ifelse(reg_hh_csh_mrt==1 & cash_trsfr==0, 0, NA)) %>%  # Control group, NA for excluded
           structure(label = "Cash transfer RCT"),
         treatment_csh_trnsfr = factor(treatment_csh_trnsfr, levels = c(0,1),
                                       labels = c("Control","Cash Assignment")) %>%
           structure(label = "Tekavoul only program impact evaluation"))
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

# Variables for each type of sample
allrounds_MRT_hh <- allrounds_MRT_hh %>%
  mutate(type_treatment = ifelse(reg_hh_pi_mrt==1,"Productive inclusion",
                                 ifelse(reg_hh_csh_mrt==1, "Cash transfert", NA)),
         treatment_group = ifelse(is.na(treatment_csh_trnsfr),treatment_pi,treatment_csh_trnsfr))

allrounds_MRT_hh <- allrounds_MRT_hh %>%
  mutate(type_treatment = ifelse(reg_hh_pi_mrt==1,"Productive inclusion",
                                 ifelse(reg_hh_csh_mrt==1, "Cash transfert", NA)))


baseline_MRT_hh <- baseline_MRT_hh %>%
  mutate(type_treatment = ifelse(reg_hh_pi_mrt==1,"Productive inclusion",
                                 ifelse(reg_hh_csh_mrt==1, "Cash transfert", NA)),
         treatment_group = ifelse(is.na(treatment_csh_trnsfr),treatment_pi,treatment_csh_trnsfr))

baseline_MRT_hh <- baseline_MRT_hh %>%
  mutate(type_treatment = ifelse(reg_hh_pi_mrt==1,"Productive inclusion",
                                 ifelse(reg_hh_csh_mrt==1, "Cash transfert", NA)))


################################################################################
######################## Cleaning the IPV data #################################
################################################################################

# Building intensive index of IPV
# Create a sample variable
# ipv_df_consent1 <- ipv_df_consent1 %>%
#   mutate(hhid = as.numeric(hhid)) # Convert hhid to numeric
#   

######################## Controlling behavior ##################################

scale_vars = c("ipv1","ipv2","ipv3","ipv5","ipv6","ipv7")

ipv_df_consent1 <- ipv_df_consent1 %>%
  mutate(across(all_of(scale_vars), 
                ~scale(., center = TRUE, scale = TRUE),  # Ensure scale() output is a vector
                .names = "{.col}_scalecenter")) %>%
  mutate(control_ipv_inten_index = apply(select(., ends_with("_scalecenter")), 1, 
                                         mean, na.rm = TRUE) %>%
           structure(label = "Controlling behavior intensity index")) %>%
  mutate(control_ipv_inten_index = scale(control_ipv_inten_index, center = TRUE, scale = TRUE),
         control_ipv_inten_index = ifelse(is.na(control_ipv_12m),NA,control_ipv_inten_index)) %>%
  select(-ends_with("_scalecenter")) 

# Create intensive index variables
# ipv_df_consent1 <- ipv_df_consent1 %>%
#   mutate(across(all_of(scale_vars), 
#                     ~scale(., center = TRUE, scale = TRUE), 
#                     .names = "{.col}_scalecenter")) %>% 
#   rowwise() %>% 
#   mutate(control_ipv_inten_index = as.numeric(scale(mean(across(ends_with("_scalecenter")), 
#                                             na.rm = TRUE), center = TRUE, scale = TRUE)) %>% 
#            structure(label = "Controlling behavior intensity index")) %>% 
#   select(-c(ends_with("_scalecenter")))

######################## Emotional violence ####################################

scale_vars = c("ipv8","ipv9","ipv10","ipv11")
# Create intensive index variables

ipv_df_consent1 <- ipv_df_consent1 %>%
  mutate(across(all_of(paste0(scale_vars,"b")), 
                ~scale(., center = TRUE, scale = TRUE),  # Ensure scale() output is a vector
                .names = "{.col}_scalecenter")) %>%
  mutate(emo_ipv_inten_index = apply(select(., ends_with("_scalecenter")), 1,
                                         mean, na.rm = TRUE) %>%
           structure(label = "Emo. IPV intensity index")) %>%
  mutate(emo_ipv_inten_index = scale(emo_ipv_inten_index, center = TRUE, scale = TRUE)) %>%
  select(-ends_with("_scalecenter")) 

ipv_df_consent1 <- ipv_df_consent1 %>%
  mutate(across(all_of(paste0(scale_vars,"a")), 
                ~scale(., center = TRUE, scale = TRUE),  # Ensure scale() output is a vector
                .names = "{.col}_scalecenter")) %>%
  mutate(emo_ipv_inten_index2 = apply(select(., ends_with("_scalecenter")), 1,
                                     mean, na.rm = TRUE) %>%
           structure(label = "Emo. IPV intensity index")) %>%
  mutate(emo_ipv_inten_index2 = scale(emo_ipv_inten_index2, center = TRUE, scale = TRUE),
         emo_ipv_inten_index = ifelse(is.na(emo_ipv_inten_index),emo_ipv_inten_index2,emo_ipv_inten_index),
         emo_ipv_inten_index = ifelse(is.na(emo_ipv_12m),NA,emo_ipv_inten_index)
         ) %>%
  select(-ends_with("_scalecenter")) 

######################## Physical violence #####################################

scale_vars = c("ipv12","ipv13","ipv14","ipv15","ipv16","ipv17")
# Create intensive index variables

ipv_df_consent1 <- ipv_df_consent1 %>%
  mutate(across(all_of(paste0(scale_vars,"b")),
                ~scale(., center = TRUE, scale = TRUE),  # Ensure scale() output is a vector
                .names = "{.col}_scalecenter")) %>%
  mutate(phy_ipv_inten_index = apply(select(., ends_with("_scalecenter")), 1, 
                                     mean, na.rm = TRUE) %>%
           structure(label = "Phy. IPV intensity index")) %>%
  mutate(phy_ipv_inten_index = scale(phy_ipv_inten_index, center = TRUE, scale = TRUE)) %>%
  select(-ends_with("_scalecenter")) 

ipv_df_consent1 <- ipv_df_consent1 %>%
  mutate(across(all_of(paste0(scale_vars,"a")), 
                ~scale(., center = TRUE, scale = TRUE),  # Ensure scale() output is a vector
                .names = "{.col}_scalecenter")) %>%
  mutate(phy_ipv_inten_index2 = apply(select(., ends_with("_scalecenter")), 1,
                                      mean, na.rm = TRUE) %>%
           structure(label = "Phy. IPV intensity index")) %>%
  mutate(phy_ipv_inten_index2 = scale(phy_ipv_inten_index2, center = TRUE, scale = TRUE),
         phy_ipv_inten_index = ifelse(is.na(phy_ipv_inten_index),phy_ipv_inten_index2,phy_ipv_inten_index),
         phy_ipv_inten_index = ifelse(is.na(phy_ipv_12m),NA,phy_ipv_inten_index)
         ) %>%
  select(-ends_with("_scalecenter")) 

######################## Sexual violence #######################################
scale_vars = c("ipv18","ipv19","ipv20")

# Create intensive index variables


ipv_df_consent1 <- ipv_df_consent1 %>%
  mutate(across(all_of(paste0(scale_vars,"b")),
                ~scale(., center = TRUE, scale = TRUE),  # Ensure scale() output is a vector
                .names = "{.col}_scalecenter")) %>%
  mutate(sex_ipv_inten_index = apply(select(., ends_with("_scalecenter")), 1, 
                                     mean, na.rm = TRUE) %>%
           structure(label = "Sex. IPV intensity index")) %>%
  mutate(sex_ipv_inten_index = scale(sex_ipv_inten_index, center = TRUE, scale = TRUE)) %>%
  select(-ends_with("_scalecenter")) 

ipv_df_consent1 <- ipv_df_consent1 %>%
  mutate(across(all_of(paste0(scale_vars,"a")), 
                ~scale(., center = TRUE, scale = TRUE),  # Ensure scale() output is a vector
                .names = "{.col}_scalecenter")) %>%
  mutate(sex_ipv_inten_index2 = apply(select(., ends_with("_scalecenter")), 1,
                                      mean, na.rm = TRUE) %>%
           structure(label = "Sex. IPV intensity index")) %>%
  mutate(sex_ipv_inten_index2 = scale(sex_ipv_inten_index2, center = TRUE, scale = TRUE),
         sex_ipv_inten_index = ifelse(is.na(sex_ipv_inten_index),sex_ipv_inten_index2,sex_ipv_inten_index),
         sex_ipv_inten_index = ifelse(is.na(sex_ipv_12m),NA,sex_ipv_inten_index)
         ) %>%
  select(-ends_with("_scalecenter")) 

######################## Economic violence #######################################

scale_vars = c("ipv22","ipv23","ipv25")
# Create intensive index variables

ipv_df_consent1 <- ipv_df_consent1 %>%
  mutate(across(all_of(paste0(scale_vars,"b")),
                ~scale(., center = TRUE, scale = TRUE),  # Ensure scale() output is a vector
                .names = "{.col}_scalecenter")) %>%
  mutate(eco_ipv_inten_index = apply(select(., ends_with("_scalecenter")), 1, 
                                     mean, na.rm = TRUE) %>%
           structure(label = "Eco. IPV intensity index")) %>%
  mutate(eco_ipv_inten_index = scale(eco_ipv_inten_index, center = TRUE, scale = TRUE)) %>%
  select(-ends_with("_scalecenter")) 

ipv_df_consent1 <- ipv_df_consent1 %>%
  mutate(across(all_of(paste0(scale_vars,"a")), 
                ~scale(., center = TRUE, scale = TRUE),  # Ensure scale() output is a vector
                .names = "{.col}_scalecenter")) %>%
  mutate(eco_ipv_inten_index2 = apply(select(., ends_with("_scalecenter")), 1,
                                      mean, na.rm = TRUE) %>%
           structure(label = "Sex. IPV intensity index")) %>%
  mutate(eco_ipv_inten_index2 = scale(eco_ipv_inten_index2, center = TRUE, scale = TRUE),
         eco_ipv_inten_index = ifelse(is.na(eco_ipv_inten_index),eco_ipv_inten_index2,eco_ipv_inten_index),
         eco_ipv_inten_index = ifelse(is.na(eco_ipv_12m),NA,eco_ipv_inten_index)) %>%
  select(-ends_with("_scalecenter")) 

######################## Any type of violence ##################################

scale_vars = c("ipv8","ipv9","ipv10",
               "ipv11","ipv12","ipv13","ipv14","ipv15","ipv16","ipv17",
               "ipv18","ipv19","ipv20","ipv22","ipv23","ipv25")

# Create intensive index variables
ipv_df_consent1 <- ipv_df_consent1 %>%
  mutate(across(all_of(c(paste0(scale_vars,"b"),"ipv1","ipv2","ipv3","ipv5","ipv6","ipv7")), 
                ~scale(., center = TRUE, scale = TRUE),  # Ensure scale() output is a vector
                .names = "{.col}_scalecenter")) %>%
  mutate(all_ipv_inten_index = apply(select(., ends_with("_scalecenter")), 1, 
                                    mean, na.rm = TRUE) %>%
           structure(label = "All type of IPV intensity index 12m")) %>%
  mutate(all_ipv_inten_index = scale(all_ipv_inten_index, center = TRUE, scale = TRUE)) %>%
  select(-ends_with("_scalecenter")) 

ipv_df_consent1 <- ipv_df_consent1 %>%
  mutate(across(all_of(c(paste0(scale_vars,"a"),"ipv1","ipv2","ipv3","ipv5","ipv6","ipv7")), 
                ~scale(., center = TRUE, scale = TRUE),  # Ensure scale() output is a vector
                .names = "{.col}_scalecenter")) %>%
  mutate(all_ipv_inten_index2 = apply(select(., ends_with("_scalecenter")), 1,
                                      mean, na.rm = TRUE) %>%
           structure(label = "All type of IPV intensity index ever")) %>%
  mutate(all_ipv_inten_index2 = scale(eco_ipv_inten_index2, center = TRUE, scale = TRUE),
         all_ipv_inten_index = ifelse(is.na(all_ipv_inten_index2),all_ipv_inten_index,all_ipv_inten_index2),
         all_ipv_inten_index = ifelse(is.na(ipv_all_12m),NA,all_ipv_inten_index)) %>%
  select(-ends_with("_scalecenter")) 

################################################################################
######################## Variables for heterogeneity ###########################
################################################################################

######################## Variables for heterogeneity ###########################

followup_MRT_hh <- allrounds_MRT_hh %>% 
  filter(phase==2) 

#### hhh is female
followup_MRT_hh <- followup_MRT_hh %>% 
  mutate(het_hhh_fem = as.factor(hhh_fem_bl),
         hhh_fem = as.numeric(hhh_fem_bl))

### hhh literacy
followup_MRT_hh <- followup_MRT_hh %>% 
  mutate(het_hhh_lit = as.factor(hhh_lit_bl),
         hhh_lit = as.numeric(hhh_lit_bl))

### Benef. literacy 
followup_MRT_hh <- followup_MRT_hh %>% 
  mutate(het_pben_lit = as.factor(pben_lit_bl),
         pben_lit = as.numeric(pben_lit_bl))


### Benef. share of saving own by partner
followup_MRT_hh <- followup_MRT_hh %>% 
  mutate(het_save_share = as.factor(save_share_d),
         save_share = as.numeric(save_share_d))

### hhh nbr years of education
followup_MRT_hh <- followup_MRT_hh %>% 
  mutate(het_hhh_edu = as.factor(ifelse(hhh_edu_bl>
                                          median(hhh_edu_bl,
                                                 na.rm = TRUE),1,0)),
         hhh_edu = as.numeric(hhh_edu_bl))

### Benef. nbr years of education
followup_MRT_hh <- followup_MRT_hh %>% 
  mutate(het_pben_edu = as.factor(ifelse(pben_edu_bl>
                                       median(pben_edu_bl,
                                              na.rm = TRUE),1,0)),
         pben_edu = as.numeric(pben_edu_bl))


### Control over earnings
followup_MRT_hh <- followup_MRT_hh %>% 
  mutate(het_ctrl_earn_index = as.factor(ifelse(ctrl_earn_index_tr_bl>
                                                  median(ctrl_earn_index_tr_bl,
                                                         na.rm = TRUE),1,0)),
         ctrl_earn_index = as.numeric(ctrl_earn_index_tr_bl))


### Control over hh. resources
followup_MRT_hh <- followup_MRT_hh %>% 
  mutate(het_ctrl_hh_index = as.factor(ifelse(ctrl_hh_index_tr_bl>
                                                median(ctrl_hh_index_tr_bl,
                                                       na.rm = TRUE),1,0)),
         ctrl_hh_index = as.numeric(ctrl_hh_index_tr_bl))

### Intra-household dynamics 
followup_MRT_hh <- followup_MRT_hh %>% 
  mutate(het_intrahh_index = as.factor(ifelse(intrahh_vars_index_tr_bl>
                                                median(intrahh_vars_index_tr_bl,
                                                       na.rm = TRUE),1,0)),
         intrahh_index = as.numeric(intrahh_vars_index_tr_bl))


### Benef. share of total hh. revenues
followup_MRT_hh <- followup_MRT_hh %>% 
  mutate(het_rev_sum_bohh = as.factor(ifelse(rev_sum_bohh_wemp_98_tr_bl>
                                               median(rev_sum_bohh_wemp_98_tr_bl,
                                                      na.rm = TRUE),1,0)),
         rev_sum_bohh = as.numeric(rev_sum_bohh_wemp_98_tr_bl))

### Benef. is handicap
followup_MRT_hh <- followup_MRT_hh %>% 
  mutate(het_pben_handicap = as.factor(pben_handicap_bl),
         pben_handicap = as.numeric(pben_handicap_bl))

### Benef. is in polygamous union
followup_MRT_hh <- followup_MRT_hh %>% 
  mutate(het_pben_poly = as.factor(pben_poly_bl),
         pben_poly = as.numeric(pben_poly_bl))

### Beneficiary age gap
#### followup
followup_MRT_hh <- followup_MRT_hh %>%
  mutate(age_gap = as.numeric(hhh_age_bl - pben_age_bl))

#### baseline
baseline_MRT_hh <- baseline_MRT_hh %>%
  mutate(age_gap = as.numeric(hhh_age - pben_age))

followup_MRT_hh <- followup_MRT_hh %>%
  mutate(het_age_gap = as.factor(ifelse(age_gap>median(age_gap,na.rm = TRUE),1,0)))

######################## Enumerator characteristics ###########################

ipv_df_consent1 <- ipv_df_consent1 %>%
  mutate(surveyor = as.factor(surveyor))


ipv_df_all <- ipv_df_all %>%
  mutate(surveyor = as.factor(surveyor))

################################################################################
######################## Merging and exporting #################################
################################################################################

# Including the treatment variable
# strata,cluster,p_region,p_commune,p_village,region,commune,
#village,soc_prmtn,cash_trsfr,

followup_MRT_hh_and_ipv_df <- ipv_df_consent1 %>% # Handling survey completion and consent
  distinct(hhid, .keep_all = TRUE) %>% # there are some duplicateds row with the same 13 households 
  left_join(followup_MRT_hh %>% 
               select(hhid,contains("treat"), reg_hh_csh_mrt,reg_hh_pi_mrt,
                      strata,cluster, hhh_fem_bl, mem_n_bl,
                      pben_relation, # Productive beneficiary relationship to household head
                      ctrl_earn_index_tr_bl, ctrl_earn_index, ctrl_hh_index_tr_bl, 
                      ctrl_hh_index, intrahh_vars_index_tr_bl, intrahh_index, 
                      rev_sum_bohh_wemp_98_tr_bl, rev_sum_bohh,hhh_lit, het_pben_lit,
                      age_gap, het_hhh_lit, het_ctrl_earn_index, het_ctrl_hh_index,
                      ctrl_hh_index, intrahh_index, rev_sum_bohh,
                      het_intrahh_index, het_rev_sum_bohh, het_age_gap,
                      pben_handicap, het_pben_handicap, pben_poly, het_pben_poly, 
                      hhh_edu, het_hhh_edu, hhh_fem, het_hhh_fem, hhh_age_bl,
                      pben_lit, het_pben_lit, save_share_d, cash_trsfr, pben_age_bl,
                      het_save_share, save_share,pben_age,hhh_age,
                      pben_edu, het_pben_edu, hhh_edu, het_hhh_edu,
                      
                      same_cb,pben_fem,hhh_poly,hhh_prim_bl,pben_prim_bl,
                      mem_n,hou_room_bl,hou_hea_min_bl,hou_mar_min_bl,hou_wat_min_bl
                      ) %>% 
              mutate(hhid=as.character(hhid)), 
            by="hhid", 
            multiple="first")

# keep even those who didn't give their consent for the IPV module.
ipv_df_all <- ipv_df_all %>% 
  filter(!is.na(ipv_completed)) %>% 
  distinct(hhid, .keep_all = TRUE) %>% # there are some duplicateds row with the same 
  # in the ipv_df, I consider the first row only
  right_join(baseline_MRT_hh %>% 
              select(hhid,contains("treat"), reg_hh_csh_mrt,reg_hh_pi_mrt,strata,
                     cluster)%>% 
               mutate(hhid=as.character(hhid)), 
                      by="hhid", 
                      multiple="first")


# Exporting the data in R format
write_rds(baseline_MRT_hh, "output/data/baseline_MRT_hh.rds")
write_rds(allrounds_MRT_hh, "output/data/allrounds_MRT_hh.rds")
write_rds(followup_MRT_hh, "output/data/followup_MRT_hh.rds")
write_rds(followup_MRT_hh_and_ipv_df, "output/data/followup_MRT_hh_and_ipv_MRT_hh.rds")
write_rds(ipv_df_all, "output/data/ipv_df_all.rds")
write_rds(ipv_df_consent1, "output/data/ipv_df_consent1.rds")
# remove data to free space
rm(list = ls())
