# Screen ASER sediment data
# AMC 2021-03

# Load libraries
library(readr)
library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(magrittr)
library(stringr)

# import sediment data
sed <- read_csv('Data/ASER_sediment_2020.csv',
                col_types = list('sample_date' = col_date("%m/%d/%Y"))) %>%
                select(3:36) %>%
                filter(validation_qualifier != 'R')

# import screening standards
#SSLs are for inorganic and organic
ssl <- read_excel('Data/RS11941_NMED SSLs June 2019_for_analysis.xlsx') %>%
          select(1:8) 

names(ssl) %<>%
  str_replace_all("\\s", "_") %>% tolower()

ssl_tall <- ssl %>%
  pivot_longer(cols = c(3:8), names_to = 'SSL_cat', values_to = 'SSL_value')

# SALs are for rads
sal <- read_excel('Data/screening_action_limits.xlsx') %>%
  select(-6)

sal_tall <- sal %>%
  pivot_longer(cols = c(2:5), names_to = "SAL_cat", values_to = 'SAL_value')

# start with the SSLs and SALs, then deal with the BCGs and figure out if we want to use the recreational SSLs too

# Rads - here do an inner join with SALs
# rad_scr <- sed %>%
#   inner_join(sal_tall, by = c('parameter_name' = 'radionuclide')) %>%
#   mutate(exceedance = ifelse((report_result > SAL_value & detect_flag == 'Y'), 1, 0)) 
# 
# rad_wide <- rad_scr %>%
#   select(-SAL_value) %>%
#   pivot_wider(names_from = 'SAL_cat', values_from = 'exceedance') %>%
#   mutate(exceed = ifelse(rowSums(across(35:38), na.rm = TRUE) >= 1, 1, 0))%>%
#   group_by(location_id, parameter_code, parameter_name, sample_purpose) %>%
#   summarize(analyses_n = n(), detect_n = sum(detect_flag == 'Y'), exceed_n = sum(exceed == 1))

rad_screen <- function(sediment_data){
  init_screen <- sediment_data %>%
    inner_join(sal_tall, by = c('parameter_name' = 'radionuclide')) %>%
    mutate(exceedance = ifelse((report_result > SAL_value & detect_flag == 'Y'), 1, 0)) 
  if (sum(init_screen$exceedance) == 0) {
    print('No radionuclide exceedances!')
  } else {
    init_screen_wide <- init_screen %>%
      select(-SAL_value) %>%
      pivot_wider(names_from = 'SAL_cat', values_from = 'exceedance') %>%
      mutate(exceed = ifelse(rowSums(across(35:38), na.rm = TRUE) >= 1, 1, 0))%>%
      group_by(location_id, parameter_code, parameter_name, sample_purpose) %>%
      summarize(analyses_n = n(), detect_n = sum(detect_flag == 'Y'), exceed_n = sum(exceed == 1))
    return(init_screen_wide)
  }
}

