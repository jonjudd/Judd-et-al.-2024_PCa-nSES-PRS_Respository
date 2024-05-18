# Jon Judd
# 11/17/23

# Introduction: -------------------------------
# This file is going to combine my previous code that I used to merge my data into a single file. This includes the cancer, ses, and prs data.
# I consider this v2 only because it is a substantial coding step even though the code will remain relatively the same

# Changelog: --------------------------
# 2.21.24 - I want to see if my result is specific to prostate cancer so I'm making a composite file with prostate cancer instead
# 4.16.24 - I shifted back to PCa and PRS_451. I am also adding the number of clinics in the last year measurement

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


# Variables to save: -------------------
keep_vars = c("keep_vars","prostate_df", "merged_data", "total_prs", "health_data")
rm(list = ls()[!(ls() %in% keep_vars)])


# .----
# Pull cancer data: ----------
# This sectino of the code will pull and format the cancer data for my expanded ancestry and prs analysis

# find and load cancer file
cancer_file = "/labs/jswitte/Data/Phenotype/ukb-cancer-rephenotyping/pheno_files/ukPheno_2022-03-09.csv"
cancer_df = fread(input = cancer_file)

# Choose columns of interest
prostate_df = cancer_df %>% select(eid, prostate, prostate_incid_any, othskin, othskin_incid_any, case, str_c("PC", 1:10), age_assessment, yob, mob, genotyping.array, Inferred.Gender) %>% 
  # I also want to replace controls that list NA into 0's. This is just a data processsing step
  mutate(across(matches("^prostate|^othskin"), 
                ~ replace_na(.x,0))) %>% 

  # I then also only want men so let's pull that too
  filter(Inferred.Gender == "M") %>% 
  #Lastly, I want to know the date of assessment or at least something close so I will use birth + age_assessment to figure that out
  mutate(assessment_date = lubridate::my(paste0(mob,"-",yob)) + lubridate::years(as.integer(signif(age_assessment,2))) ) %>% 
  select(-yob, -mob)

# Great. Now let's output this into a specific place
write_csv(prostate_df, "/labs/jswitte/Projects/jjudd5/SES.PRS_Project/Cancer_Files/ukb_cancer_2024.05.16.csv")


# .----
# RESET THE ENVIRONMENT: -------------
rm(list = ls()[!(ls() %in% keep_vars)])
# .----


# Pull SES data: --------------------
# I actually already know where the Indices of Multiple Deprivation (IMD) data is on the cluster so I don't need to look for it.
# The IMD data is hidden in this file
ukb25312_file = "/labs/jswitte/Data/ShowcaseDloads/ukb25312.csv"

# Lets load that data
ukb25312_df = fread(ukb25312_file)


# Now let's specifically pull the columns that I know are part of the IMD for the components
# As I've updated this code, I'm only pulling the England data
imd_columns = 26410:26417
imd_df = ukb25312_df %>% select(eid, num_range(prefix = "", range = imd_columns, suffix = "-0.0"))

# And then for clarity sake, I want to name them, which will be tedious but pretty necessary
imd_names = c("IMD", "Income", "Employment", "Health", "Education", "Housing", "Crime", "Environment")

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
                                    "54-0.0", "50-0.0", "21001-0.0")
names(townsend_df) = c("eid", "Townsend_Index", "PSA_Screening","Time_Since_PSA",
                       "Assessment_Centre","Height","BMI")

# Dont' remove people that don't have PSA testing data, but make them all NAs
townsend_df = townsend_df %>% 
  mutate(PSA_Screening = ifelse(PSA_Screening != 0 & PSA_Screening != 1, NA, PSA_Screening)
         )

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
names(ind_data) = c("eid","Family_History", "Indv_Ed")

# Format family history
ind_data = ind_data %>% 
  filter(Family_History > 0) %>% 
  mutate(Family_History = ifelse(Family_History == 13, 1, 0)
         )

# Remove old fil
rm(list = "ind_data_raw")


# .-------------------
# Join SES factors: ----------------------
# I want to join all the SES factors of interest
merged_data = imd_df %>% 
  inner_join(townsend_df, by = "eid") %>% 
  inner_join(ind_data, by = "eid")

# And export
write_csv(merged_data, "/labs/jswitte/Projects/jjudd5/SES.PRS_Project/SES_Files/ukb_ses_2024.05.16.csv")


# .----
# RESET THE ENVIRONMENT: -------------
rm(list = ls()[!(ls() %in% keep_vars)])
# .----


# Merge PRS files: ----------------------------------
# I want to take the PRS files that I've generated for PRS-451, add the ancestry, and combine into 1 file

# Load PRS files
eur_prs = "/labs/jswitte/Projects/jjudd5/PrCa_PRS451/Totals/eur_Score_PRS_chrAll.sscore"
chb_prs = "/labs/jswitte/Projects/jjudd5/PrCa_PRS451/Totals/chinese_Score_PRS_chrAll.sscore"
afr_prs = "/labs/jswitte/Projects/jjudd5/PrCa_PRS451/Totals/afr_Score_PRS_chrAll.sscore"
mix_prs = "/labs/jswitte/Projects/jjudd5/PrCa_PRS451/Totals/mixed_Score_PRS_chrAll.sscore"
sas_prs = "/labs/jswitte/Projects/jjudd5/PrCa_PRS451/Totals/sas_Score_PRS_chrAll.sscore"

# Document files and ancestries
prs_files = c(eur_prs, chb_prs, afr_prs, mix_prs, sas_prs)
ances = c("eur","chb","afr","mix","sas")

# Create a variable to save the data
total_prs = tibble(IID = numeric(),
                   PRS = numeric(),
                   PRS_z = numeric(),
                   Ances = character())

# Loop through the files and add the ancestry
for (i in 1:length(ances)){
    
  # Load the file
  ances_prs = fread(prs_files[i])
  
  # Add the ancestry
  ances_prs = ances_prs %>% 
    mutate(Ances = ances[i])
  
  # Add to total prs file
  total_prs = rbind(total_prs, ances_prs)
}

# Output this file
write_csv(total_prs, "/labs/jswitte/Projects/jjudd5/PrCa_PRS451/AllAnces_Score_PRS451_chrAll.sscore")



# .----
# RESET THE ENVIRONMENT: -------------
rm(list = ls()[!(ls() %in% keep_vars)])
# .----


# Calculate healthcare utilization: -----
# I will use the GP clinical data to calculate the number of clinic visits the year before enrollments
# This file is so large I'm immediately going to load the file and filter it to the needed distinct columns before saving a variable
clinical = fread("/labs/jswitte/Data/ShowcaseDloads/gp_clinical.txt", sep = "\t") %>% 
  select(eid, event_dt) %>% distinct()

# Now I need to match these IDs to the people I have in my dataset and pull out their date of assessment
assessment_date = prostate_df %>% select(eid, assessment_date)
clinical = clinical %>% inner_join(.,assessment_date)

# Edit the dates to be in "date" format and then check if the date was within the last year
clinical_new = clinical %>% mutate(event_dt = dmy(event_dt),
                               assessment_date = ymd(assessment_date),
                               within_year = (event_dt < assessment_date) & 
                                             (abs(as.duration(interval(event_dt, assessment_date))) <= years(1))
                               ) %>% 
  filter(within_year)

health_data = clinical_new %>% group_by(eid) %>% summarize(no_visits = n())


# .----
# RESET THE ENVIRONMENT: -------------
rm(list = ls()[!(ls() %in% keep_vars)])
# .----


# Now merge the files: ---------------------
# Rename files
prs_df = total_prs
cancer_df = prostate_df
ses_df = merged_data

# Merge datasets: -------------------
# I want to merge the datasets. I'm going to start based on prs, since I want to ensure I have individuals of EUR ancestry with genetic data
merged_df_raw = prs_df %>% rename(eid = IID) %>% 
  left_join(cancer_df, by = "eid") %>% 
  left_join(ses_df, by = "eid") %>% 
  left_join(health_data, by = "eid")

# I realize that there is a lot of missing data particularly for people w/o cacer or ses data. I want those removed
merged_df = merged_df_raw %>% filter(!is.na(prostate))


# Remove everything except merged_df
rm(list = ls()[(ls() %>% str_which("^merged_df$", negate = T))])



# let's output this file
write_csv(merged_df, "/labs/jswitte/Projects/jjudd5/SES.PRS_Project/Composite_DataFiles/composite_data_2024.05.16.csv")

