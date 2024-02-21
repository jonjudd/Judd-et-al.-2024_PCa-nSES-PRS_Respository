# Jon Judd
# 05/31/23

# Introduction: -------------------------------
# The goal of this file is to pull the necessary SES data from the cluster and format it.

# Since I imagine that the SES factors that I pull will change, this file will also get continuously updated. Well I imagine


# Packages: ------------------------------
# Clear environment
rm(list = ls())
"~/R/x86_64-pc-linux-gnu-library/4.0" %>% c(.,.libPaths()) %>% .libPaths(.)

# Base R:
library(tidyverse)
library(data.table)
library(readxl)
library(tidymodels)
library(ggpubr)
library(devtools)
library(introdataviz)


# .-----
# Pull IMD data: ------------------
# I actually already know where the Indices of Multiple Deprivation (IMD) data is on the cluster so I don't need to look for it.
# The IMD data is hidden in this file
ukb25312_file = "/labs/jswitte/Data/ShowcaseDloads/ukb25312.csv"

# Lets load that data
ukb25312_df = fread(ukb25312_file)


# Now let's specifically pull the columns that I know are part of the IMD for the components and different countries
imd_columns = 26410:26434
imd_df = ukb25312_df %>% select(eid, num_range(prefix = "", range = imd_columns, suffix = "-0.0"))

# And then for clarity sake, I want to name them, which will be tedious but pretty necessary
imd_names = c("IMD_E", "Income_E", "Employment_E", "Health_E", "Education_E", "Housing_E", "Crime_E", "Environment_E",
              "Income_W", "Employment_W", "Health_W", "Education_W", "Services_W", "Housing_W", "Environment_W", "Safety_W", "IMD_W",
              "IMD_S", "Income_S", "Employment_S", "Health_S", "Education_S", "Housing_S", "Services_S", "Crime_S")

names(imd_df) = c("eid", imd_names)

# Remove old big file
rm(list = "ukb25312_df")


# .----
# Pull Townsend data: -------------------------------------
# Alright, I know where the Townsend data is. Let me get that name and load the file
ukb6192_file = "/labs/jswitte/Data/ShowcaseDloads/ukb6192.csv"

# Load file
ukb6192_df = fread(ukb6192_file)

# And then indexing for Townsend is actually much sinmpler since it is just one column
townsend_df = ukb6192_df %>% select(eid, '189-0.0', '2365-0.0', '3809-0.0', 
                                    "54-0.0", "50-0.0", "21001-0.0", "1259-0.0")
names(townsend_df) = c("eid", "Townsend_Index", "PSA_Screening","Time_Since_PSA",
                       "Assessment_Centre","Height","BMI", "Household_Smoking")

# Also remove people that aren't either a yes or no on screening. 
# Also remove people that don't know if they've been screened or didn't answer
townsend_df = townsend_df %>% 
  filter(!is.na(PSA_Screening), PSA_Screening == 1 | PSA_Screening == 0) %>% 
  filter(is.na(Time_Since_PSA) | Time_Since_PSA != -3 | Time_Since_PSA != -1)

# I also want to convert in time since screening any value that is -10 into 0.5. Since -10 == "less than a year of screening",
# I will convert it to 0.5 to represent 6 months so I'll be at most half a year off, but it will be easier to 
# work with
townsend_df = townsend_df %>% mutate(Time_Since_PSA = if_else(Time_Since_PSA == -10, 0.5, Time_Since_PSA))

# Remove old big file
rm(list = "ukb6192_df")


# .--------------------------------------
# join individual SES data: ---------------
# I also want to include the file that loads in family history and education (and eventually income)
ind_file = "/labs/jswitte/Projects/jjudd5/SES.PRS_Project/SES_Files/IndSes_9.25.23.csv"
ind_data_raw = fread(ind_file) 

# Pull out important variables
ind_data = ind_data_raw %>% 
  select(eid,"20107-0.0", "6138-0.0")

# Remove old fil
rm(list = "ind_data_raw")


# .-------------------
# Join SES factors: ----------------------
# I want to join all the SES factors of interest
merged_data = imd_df %>% 
  inner_join(townsend_df, by = "eid") %>% 
  inner_join(ind_data, by = "eid")

# And export
write_csv(merged_data, "/labs/jswitte/Projects/jjudd5/SES.PRS_Project/SES_Files/ukb_ses_2023.10.12.csv")
