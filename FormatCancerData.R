# Jon Judd
# 05/31/23

# Introduction: -------------------------------
# The goal of this file is to take the UKB cancer file that is massive and pull the
# necessary prostate cancer data to find an association between PCa and SES factors

# Packages: ------------------------------
# Reset environment
rm(list = ls())

# Base R:
library(tidyverse)
library(data.table)
library(readxl)
library(tidymodels)
library(ggpubr)
library(devtools)
library(introdataviz)


# .----
# Pull cancer data: ----------
# This is the name of the raw UKB cancer file that I will use
cancer_file = "/labs/jswitte/Data/Phenotype/ukb-cancer-rephenotyping/pheno_files/ukPheno_2022-03-09.csv"

# Let's fread this data
cancer_df = fread(input = cancer_file)

# Choose columns of interest
prostate_df = cancer_df %>% select(eid, contains("prostate"), case, case_malig, str_c("PC", 1:10), age_assessment, yob, mob, genotyping.array, Inferred.Gender) %>% 
  # I also want to replace controls that list NA into 0's. This is just a data processsing step
  mutate(across(matches("^prostate$|^prostate_incid_any$"), 
                ~ replace_na(.x,0))) %>% 
  # I then also only want men so let's pull that too
  filter(Inferred.Gender == "M") %>% 
  #Lastly, I want to know the date of assessment or at least something close so I will use birth + age_assessment to figure that out
  mutate(assessment_date = lubridate::my(paste0(mob,"-",yob)) + lubridate::years(as.integer(signif(age_assessment,2))) ) %>% 
  select(-yob, -mob)

# Great. Now let's output this into a specific place
write_csv(prostate_df, "/labs/jswitte/Projects/jjudd5/SES.PRS_Project/Cancer_Files/ukb_cancer_2023.07.05.csv")
