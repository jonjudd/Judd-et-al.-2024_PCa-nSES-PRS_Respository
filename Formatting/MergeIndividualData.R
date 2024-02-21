# Jon Judd
# 05/31/23

# Introduction: -------------------------------
# The goal of this file is to load the PRS, cancer, and ses files and then merge them based on individual ID. This
# allows me to identify people with all necessary data into a single file


# Packages: ------------------------------
# Reset environment
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
# Find and load raw files: ----------------
prs_file = "/labs/jswitte/Projects/jjudd5/SES.PRS_Project/PCa_PRS269/eur_Score_PRS_chrAll.sscore"
cancer_file = "/labs/jswitte/Projects/jjudd5/SES.PRS_Project/Cancer_Files/ukb_cancer_2023.07.05.csv"
ses_file = "/labs/jswitte/Projects/jjudd5/SES.PRS_Project/SES_Files/ukb_ses_2023.10.12.csv"


prs_df = fread(prs_file)
cancer_df = fread(cancer_file)
ses_df = fread(ses_file)

# Merge datasets: -------------------
# I want to merge the datasets. I'm going to start based on prs, since I want to ensure I have individuals of EUR ancestry with genetic data
merged_df_raw = prs_df %>% rename(eid = IID) %>% 
  left_join(cancer_df, by = "eid") %>% 
  left_join(ses_df, by = "eid")

# I realize that there is a lot of missing data particularly for people w/o cacer or ses data. I want those removed
merged_df = merged_df_raw %>% filter(!is.na(prostate))

# I also want to have some composite IMD score that tracks people across countries so I will prioritize England > Wales > Scotland.
# I then want to also keep track of the specific country
imd_scores = merged_df %>% select(IMD_E, IMD_W, IMD_S) # This pulls the scores from the countries
consolidated_scores = imd_scores %>% pmap_dbl(max,na.rm = T) %>% na_if(-Inf) # This finds the max score, which technically isn't the priority above, but shouldn't really matter
consolidated_scores_country = imd_scores %>% replace(is.na(.), 0) %>% apply(1, which.max) %>% unlist() # This finds the index (country) of the max score
consolidated_scores_country[which(is.na(consolidated_scores))] = 0 # If they don't have an IMD score for any country, they get a 0


# now I can add this to the dataset. This should be my dataset for a minutes
merged_df = merged_df %>% mutate(Regional_IMD = consolidated_scores, IMD_country = consolidated_scores_country)


# Remove everything except merged_df
rm(list = ls()[(ls() %>% str_which("^merged_df$", negate = T))]
   )


# let's output this file
write_csv(merged_df, "/labs/jswitte/Projects/jjudd5/SES.PRS_Project/Composite_DataFiles/composite_data_2023.10.12.csv")

