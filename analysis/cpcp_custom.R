###############################################################################
# INITIALIZE AND LOAD DATA
###############################################################################

rm(list = ls())                                      # Clear global environment
cat("\014")                                          # Clear console window
file.remove(dir(paste(getwd(),"/output/", sep = ""), # Clear output folder
                full.names = TRUE))    
source("functions.R")                                # Load custom functions
load_libraries(c("rio", "gmodels", "vcd", "gtools",  # Install & load libraries
                 "ca", "extracat", "iplots", 
                 "FactoMineR", "gplots", "factoextra",
                 "corrplot", "ggpubr", "rgl"))
file_to_import = paste(getwd(), 
                       "/data/data_latowner_6monthpostconstruction.xlsx", 
                       sep = "")
data = import(file_to_import)

# Convert categorical data from characters into factors
for (i in 1:length(names(data))) {
  if (is.character(data[i][[1]])) {
    data[i][[1]] = as.factor(data[i][[1]])
  }
}

###############################################################################
# Correspondence Analysis - Subsets of Districts and Intention When Pit Fills
###############################################################################
# Create temporary vectors and name variables from data
A = data$IDPoorDich
B = data$IntndPitFull
C = data$CGend
name = "IDPoorDich - IntndPitFull - CGend"
plot_name = name

# Perform categorical analyses
freqs = table(A, B, C)
freqs_prop = prop.table(freqs, 3)
chisq_cramv = assocstats(freqs)

# Create dataframe from data
data_frame = data.frame(A, B, C)

# Create plots
scpcp(data_frame, sel = "data[,2]")
scpcp(data_frame, level.width = 0)
scpcp(data_frame, gap = 0, sel = "data[,3]")
scpcp(data_frame, sel.palette = "s")
