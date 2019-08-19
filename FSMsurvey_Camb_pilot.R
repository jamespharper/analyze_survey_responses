# The FSM Survey, Aug 2019 to Dec 2019
# Analyze survey responses from rural Cambodian latrine owners about FSM
# Written by James Harper, PE of the University of Colorado Boulder
# Last updated Aug 19, 2019
###############################################################################
# Initialize environment and user input
rm(list = ls())                                      # Clear global environment
cat("\014")                                              # Clear console window
source("functions.R")                                   # Load custom functions
load_libraries(                                      # Install & load libraries
  c("rio", "gplots", "Amelia", "car", "fastDummies", "FactoMineR", "lavaan",
    "extrafont", "gmodels", "vcd", "ltm", "MASS", "ggpubr", "dplyr", "CDFt",
    "fitdistrplus", "goft", "flexsurv", "reldist", "vioplot", "aplpack",
    "rgdal", "viridis", "lubridate", "citr"))
loadfonts(device = "win")
options(max.print = 1000)
###############################################################################
# Load cleaned data (If need to clean raw data, open STUDY_NAME-clean_data.R)
load(file = paste(getwd(), "/path/to/file.RData", sep = ""))
###############################################################################
# Summarize data
###############################################################################
summary(data)
prop.table(table(data$Catergory1))
###############################################################################
# ANALYSIS SECTION 2
###############################################################################



###############################################################################
# ANALYSIS SECTION 3
###############################################################################



###############################################################################
# CLEAN UP
###############################################################################
# sink()
# dev.off()
# closeAllConnections()
# file.remove(paste(getwd(),"/Output/dump.txt", sep = ""))
###############################################################################
# NOTES
###############################################################################
