# Jon Judd
# 05/31/23

# Introduction: -------------------------------
# The goal of this file is to load the data file that includes PRS, Cancer, and SES to make some introductory plots and analyses (regressions)


# Packages: ------------------------------
# Reset environment
rm(list = ls())

# Base R:
library(tidyverse)
library(data.table)
library(readxl)
library(tidymodels)
library(corrr)
library(ggpubr)
library(devtools)
library(introdataviz)


# .-----------------
# Load the necessary data
data_df = fread("/labs/jswitte/Projects/jjudd5/SES.PRS_Project/Composite_DataFiles/composite_data_2023.07.05.csv")


# I also want to make sure to have new cancer variables that I can plug into my models. 
data_df = data_df %>% 
  #First, I want a variable that counts any prostate cancer as case and no cancer at all as the control. 
  # If no prostate cancer, but other cancer, then that is NA
  mutate(prostate_case = if_else(prostate == 1, 1, 
                                 if_else(case == 1, NA, 0)
                                 )
         ) %>% 
  # I also want to include a measure of incident cancer
  mutate(prostate_incident = if_else(prostate == 1 & prostate_incid_any == 0, NA,
                                     if_else(prostate_incid_any == 0 & case == 0, 0, 
                                             if_else(prostate_incid_any == 1, 1, NA))
                                     )
         ) %>% 
  # I also want prevalent cancer
  mutate(prostate_prevalent = if_else(prostate_incid_any == 1, NA,
                                      if_else(prostate==0 & prostate_incid_any==0 & case ==0, 0,
                                              if_else(prostate==1 & prostate_incid_any==0, 1, NA))
                                      )
         ) %>% 
  # Make all of these factors
  mutate(prostate_case = as.factor(prostate_case),
         prostate_incident = as.factor(prostate_incident),
         prostate_prevalent = as.factor(prostate_prevalent)) %>% 
  # Remove non-NA's for screening PSA
  filter(!is.na(PSA_Screening))


  # I need this for plotting
  # I don't need to plot right now
  # mutate(prostate = as.factor(prostate),
  #        prostate_malig = as.factor(prostate_malig),
  #        case = as.factor(case),
  #        case_malig = as.factor(case_malig),
  #        PSA_Screening = as.factor(PSA_Screening)) %>% 



# .----------------
# IMPORTANT VARIABLES: -------------------------------
ses_factors = (names(data_df) %>% last((length(.) + 1) - 31 ) )[-c(27,28,30:33)]
# .-------------------------------





#Some plots to play with: ------------------------------------
#I want to maek some intro plots to check that the data looks right and to do some cursory analysis

# Some plots to especially use as a sanity check
# Distribution of PRS
# (data_df %>% ggplot(aes(PRS)) + geom_histogram()) %>%
#   ggsave(filename = "/labs/jswitte/Projects/jjudd5/SES.PRS_Project/Plots/prs_hist.pdf",
#          plot = .)
# 
# # Distrution of IMD and Townsend
# (data_df %>% ggplot(aes(Regional_IMD)) + geom_histogram()) %>%
#   ggsave(filename = "/labs/jswitte/Projects/jjudd5/SES.PRS_Project/Plots/imd_hist.pdf")
# (data_df %>% ggplot(aes(Townsend_Index)) + geom_histogram()) %>%
#   ggsave(filename = "/labs/jswitte/Projects/jjudd5/SES.PRS_Project/Plots/townsend_hist.pdf")
# 
# 
# # Now let's do some immediate checks to see if PCa status
# (data_df %>% ggplot(aes(PRS, fill = prostate)) + geom_density()) %>%
#   ggsave(filename = "/labs/jswitte/Projects/jjudd5/SES.PRS_Project/Plots/prostate_StratPrs_density.pdf")
# (data_df %>% ggplot(aes(Regional_IMD, fill = prostate)) + geom_density()) %>%
#   ggsave(filename = "/labs/jswitte/Projects/jjudd5/SES.PRS_Project/Plots/prostate_StratImd_density.pdf")
# (data_df %>% ggplot(aes(Townsend_Index, fill = prostate)) + geom_density()) %>%
#   ggsave(filename = "/labs/jswitte/Projects/jjudd5/SES.PRS_Project/Plots/prostate_StratTownsend_density.pdf")
# 
# 
# # Distrubtion of consolidated IMD and the English IMD
# data_df %>% ggplot(aes(Regional_IMD)) + geom_histogram(binwidth = 1)
# data_df %>% ggplot(aes(IMD_E)) + geom_histogram(binwidth = 1)
# 
# # Distribution of IMD when separating on PCa incidence
# data_df %>% ggplot(aes(Regional_IMD)) + geom_density()
# data_df %>% ggplot(aes(Regional_IMD, fill = prostate)) + geom_density()
# 
# data_df %>% ggplot(aes(Employment_E, fill = prostate)) + geom_density()
# 
# # Some additional checks
# data_df %>% ggplot(aes(Regional_IMD, fill = PSA_Screening)) + geom_density()



# .-------------------------------------------------------------------------------------------------------------
# Screening differences for cases vs controls: ------------------------------------------------------------------
# I want to answer the question about how many cases and controls have been screened
# This feels like it can be answered using a continguency table

# I want to do this for all my definitions of cancer. This is any cancer, incident cancer, and prevelant cancer
contig_case = data_df %>% select(prostate_case, PSA_Screening) %>% table()
contig_incident = data_df %>% select(prostate_incident, PSA_Screening) %>% table()
contig_prevalent = data_df %>% select(prostate_prevalent, PSA_Screening) %>% table()

# Then just find the sum of elements
sum(contig_case)
sum(contig_incident)
sum(contig_prevalent)



# .----------------------------------------------------------------------------------------------------------------------------
# Total vs Prevalent vs Incident: ---------------------------------
# This set of models will analyze if there is different associations between ses factors and PCa
# when I look at all cancer cases, prevalent cases, and incident cases

# I also want to store the outputs of the model so I'm going to put them in a list and them based on the main components
# I actually want to store multiple different models for total cases, incident cases, and prevalent cases
model_results_case = list()
model_results_incident = list()
model_results_prevalent = list()

keep_variables = c("data_df", "keep_variables", "ses_factors", "model_results_case", 
                   "model_results_incident", "model_results_prevalent")
rm(list = ls()[!(ls() %in% keep_variables)])

# Now create a loop that models the variables of interest and then saves them to a list of models
# Each loop will run a model for total, incidence, and prevalent cases
for (i in ses_factors){
  
  
  # Create the total cases model for the ses, edit the output, and then save in the list
  case_formula = paste0("prostate_case ~", i, " + age_assessment + PSA_Screening + genotyping.array") %>% 
    as.formula()
  model_case = logistic_reg() %>% fit(case_formula, 
                                       data = data_df)
  
  OR_case = tidy(model_case, exponentiate = T) %>% 
    mutate(low95 = exp(log(estimate) - 1.96*std.error), up95 = exp(log(estimate) + 1.96*std.error)) %>% 
    select(-std.error, -statistic) %>% 
    rename(OR = estimate)
  
  model_results_case[[i]] <- OR_case
  
  
  # ---
  # ---
  # Repeat for incident cases
  incident_formula = paste("prostate_incident ~ ",i, " + age_assessment + PSA_Screening + genotyping.array") %>% 
    as.formula()
  model_incident = logistic_reg() %>% fit(incident_formula, 
                                       data = data_df)
  
  OR_incident = tidy(model_incident, exponentiate = T) %>% 
    mutate(low95 = exp(log(estimate) - 1.96*std.error), up95 = exp(log(estimate) + 1.96*std.error)) %>% 
    select(-std.error, -statistic) %>% 
    rename(OR = estimate)
  
  model_results_incident[[i]] <- OR_incident

  
  # ---
  # ---
  # Repeat for prevalent cases
  prevalent_formula = paste("prostate_prevalent ~ ",i, " + age_assessment + PSA_Screening + genotyping.array") %>% 
    as.formula()
  model_prevalent = logistic_reg() %>% fit(prevalent_formula, 
                                          data = data_df)
  
  OR_prevalent = tidy(model_prevalent, exponentiate = T) %>% 
    mutate(low95 = exp(log(estimate) - 1.96*std.error), up95 = exp(log(estimate) + 1.96*std.error)) %>% 
    select(-std.error, -statistic) %>% 
    rename(OR = estimate)
  
  model_results_prevalent[[i]] <- OR_prevalent
  
  
  # Benchmark that this set of model is complete
  print(i)
  rm(list = ls()[!(ls() %in% keep_variables)])
  
}


# And now export these models. 
# All cases
list_rbind(model_results_case, names_to = "ses") %>% 
  write_csv( file = "/labs/jswitte/Projects/jjudd5/SES.PRS_Project/SES_Models/allcases_ses_models_2023.06.16.csv")

# Incident cases
list_rbind(model_results_incident, names_to = "ses") %>% 
  write_csv( file = "/labs/jswitte/Projects/jjudd5/SES.PRS_Project/SES_Models/incident_ses_models_2023.06.16.csv")

# Prevalent cases
list_rbind(model_results_prevalent, names_to = "ses") %>% 
  write_csv( file = "/labs/jswitte/Projects/jjudd5/SES.PRS_Project/SES_Models/prevalent_ses_models_2023.06.16.csv")


# .-----------------------------------------------------------------------------------------------
# Continuous vs Quartiles vs Rank: -----------------------------------------------------------------------
# This analysis will run models to see if there are different effects when the IMD indices are continuous vs
# when I run then as discrete quartiles. This will simply be done on all cases. 
# From the recommendation of Matt, I'm going to try to rank normalize my data to maintain the quantifiable data
# but alter the definition

# Set up
model_results_quartile = list()
model_results_rank = list()

keep_variables = c("data_df", "keep_variables", "ses_factors", 
                   "model_results_quartile", "model_results_rank")
rm(list = ls()[!(ls() %in% keep_variables)])

# Now create a loop that models the variables of interest and then saves them to a list of models
# Each loop will run a model for total, incidence, and prevalent cases
for (i in ses_factors){
  
  # Make symbol for coding sake
  i = sym(i)
  
  # Create new data frame with quartile and rank data for each of the SES factors
  temp_df = data_df %>% 
    drop_na( !!i ) %>% 
    mutate(Quartile = as.factor(ntile(!!i,5)),
           Rank = rank( !!i ) / max(rank( !!i ) )
           )
  
  # Have the model with the quartiles too
  model_quartile = logistic_reg() %>% fit(prostate_case ~ Quartile + age_assessment + 
                                          PSA_Screening + genotyping.array, 
                                          data = temp_df)
  
  # Save the quartiles model
  OR_quartile = tidy(model_quartile, exponentiate = T) %>% 
    mutate(low95 = exp(log(estimate) - 1.96*std.error), up95 = exp(log(estimate) + 1.96*std.error)) %>% 
    select(-std.error, -statistic) %>% 
    rename(OR = estimate)
  
  model_results_quartile[[i]] <- OR_quartile
  
  
  # ---
  # ---
  # Repeat for rank data
  model_rank = logistic_reg() %>% fit(prostate_case ~ Rank + age_assessment + 
                                      PSA_Screening + genotyping.array, 
                                      data = temp_df)
  
  OR_rank = tidy(model_rank, exponentiate = T) %>% 
    mutate(low95 = exp(log(estimate) - 1.96*std.error), up95 = exp(log(estimate) + 1.96*std.error)) %>% 
    select(-std.error, -statistic) %>% 
    rename(OR = estimate)
  
  model_results_rank[[i]] <- OR_rank
  
  
  
  # Benchmark that this set of model is complete
  print(i)
  rm(list = ls()[!(ls() %in% keep_variables)])
  
  }


# And now export these models. 
list_rbind(model_results_quartile, names_to = "ses") %>% 
  write_csv( file = "/labs/jswitte/Projects/jjudd5/SES.PRS_Project/SES_Models/quartile_ses_models_2023.06.17.csv")

list_rbind(model_results_rank, names_to = "ses") %>% 
  write_csv( file = "/labs/jswitte/Projects/jjudd5/SES.PRS_Project/SES_Models/rank_ses_models_2023.06.17.csv")



# .----------------------------------------------------------------------------------------------------
# Model excluding PSA screening: ----------------------------------------------------------------
# I want to create the original model of SES factors, but just exclude the PSA screening
# Measurement to see how it differs from the model and see the effect of that specific element

# Set up
model_results_noPSA = list()

keep_variables = c("data_df", "keep_variables", "ses_factors", 
                   "model_results_noPSA")
rm(list = ls()[!(ls() %in% keep_variables)])


for (i in ses_factors){
  
  # Create the total cases model for the ses, edit the output, and then save in the list
  case_formula = paste0("prostate_case ~", i, " + age_assessment + genotyping.array") %>% 
    as.formula()
  model_case = logistic_reg() %>% fit(case_formula, 
                                      data = data_df)
  
  # Edit the results
  OR_case = tidy(model_case, exponentiate = T) %>% 
    mutate(low95 = exp(log(estimate) - 1.96*std.error), up95 = exp(log(estimate) + 1.96*std.error)) %>% 
    select(-std.error, -statistic) %>% 
    rename(OR = estimate)
  
  model_results_noPSA[[i]] <- OR_case
  
  
  # Benchmark that this set of model is complete
  print(i)
  rm(list = ls()[!(ls() %in% keep_variables)])

}


# And now export these models. 
list_rbind(model_results_noPSA, names_to = "ses") %>% 
  write_csv( file = "/labs/jswitte/Projects/jjudd5/SES.PRS_Project/SES_Models/noPSA_ses_models_2023.06.17.csv")




# .-----------------------------------------------------------------------------------------------------
# Stratify on PSA Screening: -------------------------------------------------------
# What I want to do less is better understand the screening by stratifying my data on if they've ever had a PSA test
# If they've never has PSA test and I still see this effect then that's odd


# Set up
model_results_neverPSA = list()
model_results_PSA = list()

keep_variables = c("data_df", "keep_variables", "ses_factors", 
                   "model_results_neverPSA", "model_results_PSA",
                   "data_neverPSA", "data_PSA")
rm(list = ls()[!(ls() %in% keep_variables)])


# Stratify data
data_neverPSA = data_df %>% filter(PSA_Screening == 0)
data_PSA = data_df %>% filter(PSA_Screening == 1)


# Run the total cases model on both to see what happens
for (i in ses_factors){
  
  # Create the total cases model for the ses, edit the output, and then save in the list
  case_formula = paste0("prostate_case ~", i, " + age_assessment + genotyping.array") %>% 
    as.formula()
  model_case = logistic_reg() %>% fit(case_formula, 
                                      data = data_neverPSA)
  
  # Edit the results
  OR_case = tidy(model_case, exponentiate = T) %>% 
    mutate(low95 = exp(log(estimate) - 1.96*std.error), up95 = exp(log(estimate) + 1.96*std.error)) %>% 
    select(-std.error, -statistic) %>% 
    rename(OR = estimate)
  
  model_results_neverPSA[[i]] <- OR_case
  
  
  
  # Repeat the process for only people who have all had PSA tests
  case_formula = paste0("prostate_case ~", i, " + age_assessment + genotyping.array") %>% 
    as.formula()
  model_case = logistic_reg() %>% fit(case_formula, 
                                      data = data_PSA)
  
  # Edit the results
  OR_case = tidy(model_case, exponentiate = T) %>% 
    mutate(low95 = exp(log(estimate) - 1.96*std.error), up95 = exp(log(estimate) + 1.96*std.error)) %>% 
    select(-std.error, -statistic) %>% 
    rename(OR = estimate)
  
  model_results_PSA[[i]] <- OR_case
  

  
  # Benchmark that this set of model is complete
  print(i)
  rm(list = ls()[!(ls() %in% keep_variables)])
  
}


# And now export these models. 
list_rbind(model_results_neverPSA, names_to = "ses") %>% 
  write_csv( file = "/labs/jswitte/Projects/jjudd5/SES.PRS_Project/SES_Models/neverPSA_ses_models_2023.06.17.csv")

# And now export these models. 
list_rbind(model_results_PSA, names_to = "ses") %>% 
  write_csv( file = "/labs/jswitte/Projects/jjudd5/SES.PRS_Project/SES_Models/allPSA_ses_models_2023.06.17.csv")









# .---------------------------------------------------------------------------------------------------------------
# Find correlations between SES factors and PSA screening: ------------------------------------------------------------

# Let's set up a data frame that I will use
# This requires all my ses factors along with PSA screening, which is just an augment of my normal
# dataframe
# I also want PSA_screening to be numeric
corr_df = data_df %>% select(names(data_df)[21:length(names(data_df))][c(-29)]) %>% 
  mutate(PSA_Screening = as.numeric(PSA_Screening) - 1)

# Run the correlation
corr_results = correlate(corr_df)
  




# .---------------------------------------------------------------------------------------------------------------
# PCa ~ PRS + Townsend Model: --------------------------------------
PRS_Townsend_PCa_model = logistic_reg() %>% 
  fit(prostate ~ PRS + Townsend_quartile + PRS*Townsend_quartile + age_assessment + genotyping.array + PSA_Screening  + PC1 + PC2 + PC3 + PC4 + PC5 , data = data_train)

# Check the model. This is very similar to just the IMD so I'm gonna use this
effect_PRS_Townsend = tidy(PRS_Townsend_PCa_model)
OR_PRS_Townsend = tidy(PRS_Townsend_PCa_model, exponentiate = T) %>% 
  mutate(low95 = exp(log(estimate) - 1.96*std.error), up95 = exp(log(estimate) + 1.96*std.error))

# Now add to list
model_results = list_modify(model_results, PRS_Townsend = OR_PRS_Townsend)
rm(list = ls()[!(ls() %in% keep_variables)])



# .------------------------------------------------------------------------------------------------------------------
# Identify PRS effect after stratifying on SES: -----------------------------------------------
# Now I want to separate my data based on the SES factors and then run a regression just on PRS to see if there is a difference in the association
# post stratification

# I realize that I can actually loop this 5 times for each quartile
#First, let's create a list to save my models
IMD_model_results = list()
keep_variables = c("data_df", "data_split", "data_train", "data_test", "model_results", "keep_variables", "IMD_model_results")


# Now loop through the different quartiles
for (i in 1:5){
  
  #Pull the quatile data
  data_IMD = data_df %>% filter(IMD_quartile == i)
  
  print(i)
  nrow(data_IMD)
  
  # Create the training/test data
  split = initial_split(data_IMD, prop = 0.75, strata = prostate)
  train = data_split %>% training()
  test = data_split %>% testing()
  
  # Make the model
  PRS_PCa_model = logistic_reg() %>% 
    fit(prostate ~ PRS + age_assessment + genotyping.array + PC1 + PC2 + PC3 + PC4 + PC5 , data = train)
  
  # Check the model. This is very similar to just the IMD so I'm gonna use this
  OR_PRS = tidy(PRS_PCa_model, exponentiate = T) %>% 
    mutate(low95 = exp(log(estimate) - 1.96*std.error), up95 = exp(log(estimate) + 1.96*std.error))
  
  # Now add to list
  model_name <- str_c("IMD_quartile_",i)
  IMD_model_results[[model_name]] <- OR_PRS
  rm(list = ls()[!(ls() %in% keep_variables)])
  
  
}

# Repeat the process for Town send index
Townsend_model_results = list()
keep_variables = c("data_df", "data_split", "data_train", "data_test", "model_results", "keep_variables", 
                   "IMD_model_results", "Townsend_model_results")


# Now loop through the different quartiles
for (i in 1:5){
  #Pull the quatile data
  data_Townsend = data_df %>% filter(Townsend_quartile == i)
  
  print(i)
  print(nrow(data_Townsend))
  
  # Create the training/test data
  split = initial_split(data_Townsend, prop = 0.75, strata = prostate)
  train = data_split %>% training()
  test = data_split %>% testing()
  
  # Make the model
  PRS_PCa_model = logistic_reg() %>% 
    fit(prostate ~ PRS + age_assessment + genotyping.array + PC1 + PC2 + PC3 + PC4 + PC5 , data = train)
  
  # Check the model. This is very similar to just the Townsend so I'm gonna use this
  OR_PRS = tidy(PRS_PCa_model, exponentiate = T) %>% 
    mutate(low95 = exp(log(estimate) - 1.96*std.error), up95 = exp(log(estimate) + 1.96*std.error))
  
  # Now add to list
  model_name <- str_c("Townsend_quartile_",i)
  Townsend_model_results[[model_name]] <- OR_PRS
  rm(list = ls()[!(ls() %in% keep_variables)])
  
}



# .------------------------------------------------------------------------------------------------------------------
# Association between SES -> PSA: -----------------------------------------------
# I want to run logistic regression models to see if SES are actually associated with PSA screening. 

# Set up
model_results_PSAoutcome = list()
data_controls = data_df %>% filter(prostate_case == 0) %>% mutate(PSA_Screening = as.factor(PSA_Screening))
keep_variables = c("data_df", "data_controls", "keep_variables", "ses_factors", 
                   "model_results_PSAoutcome")
rm(list = ls()[!(ls() %in% keep_variables)])


for (i in ses_factors){
  
  # Create the total cases model for the ses, edit the output, and then save in the list
  case_formula = paste0("PSA_Screening ~", i, " + age_assessment + genotyping.array") %>% 
    as.formula()
  model_case = logistic_reg() %>% fit(case_formula, 
                                      data = data_controls)
  
  # Edit the results
  OR_case = tidy(model_case, exponentiate = T) %>% 
    mutate(low95 = exp(log(estimate) - 1.96*std.error), up95 = exp(log(estimate) + 1.96*std.error)) %>% 
    select(-std.error, -statistic) %>% 
    rename(OR = estimate)
  
  model_results_PSAoutcome[[i]] <- OR_case
  
  
  # Benchmark that this set of model is complete
  print(i)
  rm(list = ls()[!(ls() %in% keep_variables)])
  
}

# Export models
list_rbind(model_results_PSAoutcome, names_to = "ses") %>% 
  write_csv( file = "/labs/jswitte/Projects/jjudd5/SES.PRS_Project/SES_Models/PSA_outcome_ses_models_2023.07.05.csv")

