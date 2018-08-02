# Analysis of Surveys of Rural Cambodian Latrine Owners
# Written by James Harper, PE, ENV SP of the University of Colorado Boulder
# Started October 1, 2017
# Last updated February 24, 2018
# Useful References: https://www.statmethods.net/stats/frequencies.html, 
#                    www.mit.edu/~6.s085/notes/lecture6.pdf 

###############################################################################
# INITIALIZE
###############################################################################
rm(list = ls())                                      # Clear global environment
cat("\014")                                          # Clear console window
# file.remove(dir(paste(getwd(),"/output/", sep = ""), # Clear output folder
#                 full.names = TRUE))
source("functions.R")                                # Load custom functions
load_libraries(c("rio", "gmodels", "vcd", "gtools",  # Install & load libraries
                 "ca", "extracat", "iplots", 
                 "FactoMineR", "gplots", "factoextra",
                 "corrplot", "ggpubr", "ltm"))

###############################################################################
# USER INPUT
###############################################################################
# Select test to run; set others equal to 0 to ignore
Freqs = 2     # Frequency analysis as singles, pairs, or triplets (1, 2, or 3)
CA = 0        # Correspondence Analysis
MCA = 0       # Multiple Correspondence Analysis

# Select file to import based on selected test
# file_to_import_freqs_ca = paste(getwd(), 
#                                 "/data/data_latowner_6monthpostconst.xlsx", 
#                                 sep = "")
# file_to_import_mca = paste(getwd(), 
#                            "/data/data_mca.csv", 
#                            sep = "")

# Define variables; set equal to 0 to ignore
names(data); summary(data$Prov)
subset_by_prov = "Banteay Meanchey"
var_skip = 0        # Variables to skip during analyses
var_interest = 38   # Variable of interest for two/three-way/CA/MCA analyses
var_stratify = 45          # Stratifiers for two/three-way analysis
quali_sup = c()      # Qualitative supplementary variables for MCA
quanti_sup = c()     # Quantitative supplementary variables for MCA

###############################################################################
# CHECK FOR PROBLEMS WITH USER INPUT
###############################################################################
# Check if more than one test is selected
if ((Freqs != 0) + (CA != 0) + (MCA != 0) > 1) {
  stop("Please select only one test to perform.")
}

###############################################################################
# IMPORT AND CLEAN DATA
###############################################################################
load(file = "iDE_Oct2017.Rdata")
if (subset_by_prov != "") {
  data = subset(data, Prov == bquote(.(subset_by_prov)))
}
# str(data)
# summary(data)

###############################################################################
# ANALYZE DATA
###############################################################################
# Run 1-way frequency analyses
if (Freqs == 1) {
  print("Running one-way frequency analyses...")
  for (num in 1:length(data)) {
    
    # Skip to next variable if current variable is a skipped variable
    if (num %in% var_skip) {
      next
    }
    
    # Run analyses
    print(num)
    frequency_analysis_1way(data, num)
  }
}

# Run 2-way frequency analyses
if (Freqs == 2) {
  print("Running two-way frequency analyses...")
  
  # Create permutations of metrics to test for association
  metrics_2way = permutations(n = length(data), r = 2, 
                              v = 1:length(data), repeats.allowed = FALSE)
  metrics_2way = data.frame(A = metrics_2way[,1], B = metrics_2way[,2])
  
  # Remove pairs that contain skipped variables
  if (var_skip[[1]] != 0) {
    for (i in 1:length(var_skip)) {
      metrics_2way = metrics_2way[which(metrics_2way$A != var_skip[[i]] &
                                        metrics_2way$B != var_skip[[i]]), ]
    }
  }
  
  # Retain only pairs that begin with variables of interest
  if (var_interest[[1]] != 0) {
    for (i in 1:length(var_interest)) {
      metrics_2way = metrics_2way[which(metrics_2way$A == var_interest[[i]]), ]
    }
  }
  
  # Retain only pairs that end with stratifiers
  if (var_stratify[[1]] != 0) {
    for (i in 1:length(var_stratify)) {
      metrics_2way = metrics_2way[which(metrics_2way$B == var_stratify[[i]]), ]
    }
  }
  
  # Run analyses
  for (num in 1:length(metrics_2way[,1])) {
    print(paste(metrics_2way[num,1],"_",metrics_2way[num,2]))
    frequency_analysis_2way(data, metrics_2way[num,1], metrics_2way[num,2])
  }
}

# Run 3-way frequency analyses
if (Freqs == 3) {
  print("Running three-way frequency analyses...")
  
  # Create permutations of metrics to test for association
  metrics_3way = permutations(n = length(data), r = 3, 
                              v = 1:length(data), repeats.allowed = FALSE)
  metrics_3way = data.frame(A = metrics_3way[,1], B = metrics_3way[,2], 
                            C = metrics_3way[,3])
  
  # Remove triplets that contain skipped variables
  if (var_skip[[1]] != 0) {
    for (i in 1:length(var_skip)) {
      metrics_3way = metrics_3way[which(metrics_3way$A != var_skip[[i]] &
                                        metrics_3way$B != var_skip[[i]] &
                                        metrics_3way$C != var_skip[[i]]), ]
    }
  }
  
  # Retain only triplets that begin with variables of interest
  if (var_interest[[1]] != 0) {
    for (i in 1:length(var_interest)) {
      metrics_3way = metrics_3way[which(metrics_3way$A == var_interest[[i]]), ]
    }
  }
  
  # Retain only pairs that end with stratifiers
  if (var_stratify[[1]] != 0) {
    for (i in 1:length(var_stratify)) {
      metrics_3way = metrics_3way[which(metrics_3way$C == var_stratify[[i]]), ]
    }
  }
  
  # Run analyses
  for (num in 1:length(metrics_3way[,1])) {
    print(paste(metrics_3way[num, 1], "_", metrics_3way[num, 2], "_", 
                metrics_3way[num, 3]))
    frequency_analysis_3way(data, metrics_3way[num, 1], metrics_3way[num, 2], 
                              metrics_3way[num, 3])
  }
}

# Run correspondence analysis
if (CA == 1) {
  print("Running correspondence analyses...")

  # Create permutations of metrics to test for association
  metrics_2way = permutations(n = length(data), r = 2, 
                              v = 1:length(data), repeats.allowed = FALSE)
  metrics_2way = data.frame(A = metrics_2way[, 1], B = metrics_2way[, 2])
  
  # Remove pairs that contain skipped variables
  if (var_skip[[1]] != 0) {
    for (i in 1:length(var_skip)) {
      metrics_2way = metrics_2way[which(metrics_2way$A != var_skip[[i]] &
                                        metrics_2way$B != var_skip[[i]]), ]
    }
  }
  
  # Retain only pairs that begin with variables of interest
  if (var_interest[[1]] != 0) {
    for (i in 1:length(var_interest)) {
      metrics_2way = metrics_2way[which(metrics_2way$A == var_interest[[i]]), ]
    }
  }
  
  # Retain only pairs that end with stratifiers
  if (var_stratify[[1]] != 0) {
    for (i in 1:length(var_stratify)) {
      metrics_2way = metrics_2way[which(metrics_2way$B == var_stratify[[i]]), ]
    }
  }

  # Run correspondence analyses
  for (num in 1:length(metrics_2way[,1])) {
    print(paste(metrics_2way[num,1], "_", metrics_2way[num,2]))
    correspondence_analysis(data, metrics_2way[num,1], metrics_2way[num,2])
  }
}

if (MCA == 1) {
  print("Running multiple correspondence analysis...")
  
  # Retain variables of interest
  if (var_interest[[1]] != 0) {
    data = data[var_interest]
  }
  
  # Run multiple correspondence analysis
  multiple_correspondence_analysis(data, quali_sup, quanti_sup)
}

##########################################################################
# CLEAN UP
##########################################################################
sink()
dev.off()
closeAllConnections()
file.remove(paste(getwd(),"/Output/dump.txt", sep = ""))