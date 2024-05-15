# Jon Judd
# 02/28/22

# Introduction: -------------------------------
# The goal of this file is to take output from the Plink PRS calculation and then 
# combine them based on individual

# Packages: ------------------------------
# Base R:
library(tidyverse)
library(data.table)
library(readxl)


# Changelog: ---------------------------
# 01/24/22: Load files and build SFS from popMaxMAF and large meta pop groups
# 04/26/22: Running this again for PRS-CSx

# .----
# .----


# Pull files: ---------------------------------

# # Test files
# files = c("/labs/jswitte/Projects/jjudd5/PrCa_PRScsx/eur/Score_PRS_chr1.sscore",
#           "/labs/jswitte/Projects/jjudd5/PrCa_PRScsx/eur/Score_PRS_chr2.sscore",
#           "/labs/jswitte/Projects/jjudd5/PrCa_PRScsx/eur/Score_PRS_chr3.sscore")
# 
# output = "TotalPRS.sscore"
# ---

# The first thing is to pull files from command line
args = commandArgs(trailingOnly=TRUE)

param = args[1]
output = args[2]
files = list.files(param, pattern = ".sscore$")
files = paste(param,files, sep = "")

print(files)


# Load the data into the variables: -------------------
dataNames = map_chr(files, ~ str_split(., pattern = "/") %>% unlist() %>% last())

# Add 1st data to  final data frame
assign(dataNames[1], fread(files[1]))
assign(dataNames[1], get(dataNames[1]) %>% mutate("chr" = 1))

mergedData = get(dataNames[1])

print("Get merge")

# Remove old data
rm(list = dataNames[1])


# Loop for other score files
if (length(files) > 1){
  
  for (i in 2:length(files)) {
    
    # Load data, manipulate, add on
    assign(dataNames[i], fread(files[i]))
    assign(dataNames[i], get(dataNames[i]) %>% mutate("chr" = i) )
    mergedData = rbind(mergedData, get(dataNames[i]))
    
    # Now remove
    rm(list = dataNames[i])
    
    print(paste("Added", i))
  }
}

print("Merged all")

# Sum PRS over individual
mergedPRS = mergedData %>% group_by(IID) %>% summarise(PRS = sum(weight_SUM))
mergedPRS$PRS_z = scale(mergedPRS$PRS, center = TRUE, scale = TRUE) %>% as.numeric()

print("Summed PRS")
head(mergedPRS)


# Output
write_tsv(mergedPRS, output)
