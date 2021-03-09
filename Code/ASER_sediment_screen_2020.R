# Screen ASER sediment data
# AMC 2021-03

# Load libraries
library(readr)
library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)

# import sediment data
sed <- read_csv('Data/ASER_sediment_2020.csv',
                col_types = list('sample_date' = col_date("%m/%d/%Y"))) %>%
                select(3:36)

# import screening standards
ssl <- read_excel('Data/RS11941_NMED SSLs June 2019_for_analysis.xlsx') %>%
          select(1:8)

# start with the SSLs and SALs, then deal with the BCGs and figure out if we want to use the recreational SSLs too