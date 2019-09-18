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
# Load data
load_and_clean_raw_data = 1
if (load_and_clean_raw_data == 1) {
  
  # Import raw data
  file.to.import = paste(getwd(), 
                         "/data/surveys/FSMsurvey/FSMsurvey_pilot.xlsx",
                         sep = "")
  d1 = import(file.to.import, which = "Sheet1")
  
  file.to.import = paste(getwd(), 
                         "/data/surveys/FSMsurvey/FSMsurvey_pilot.xlsx",
                         sep = "")
  d2 = import(file.to.import, which = "Sheet2")
  
  file.to.import = paste(getwd(), 
                         "/data/surveys/FSMsurvey/FSMsurvey_pilot.xlsx",
                         sep = "")
  d3 = import(file.to.import, which = "Sheet3")
  
  # Rename variables (columns)
  names(d1)
  old.col.names1 = names(d1)
  names(d.NAME) = c("", "", "")
  print(data.frame(names(d.NAME), old.col.names1))
  
  # Remove unused variables
  # names(d.NAME)
  # d.baseline = subset(d.NAME, select = -c(Cat1, Cat2))
  
  # Convert data formats
  
  # Create new variable Var3 based on Var1 and Var2
  # summary(d.NAME$Var1)
  # summary(d.NAME$Var2)
  # d.NAME$Var3 = as.character(NA)
  # for (row in 1:length(d.NAME$Var1)) {
  #   if (is.na(d.NAME[row,]$Var1)) {
  #     d.NAME[row,]$Var3 = NA
  #   } else if (d.NAME[row,]$Var1 == "TEXT1" |
  #              d.NAME[row,]$Var1 == "TEXT2" ) {
  #     d.NAME[row,]$Var3 = "TEXT3"
  #   }
  # }
  
  # Data Quality Control
  # dset = d.NAME
  # summary(d.NAME, maxsum = 5)
  # names(d.NAME)
  # sapply(d.NAME, function(x) sum(is.na(x)))
  # missmap(d.NAME, main = "Missing Values in Variables", legend = F)
  
  # Save data to disk
  # save(d.NAME,
  #      file = paste(getwd(), "/path/to/file.RData", sep = ""))
  
} else {
  load(file = paste(getwd(), "/path/to/file.RData", sep = ""))
}
###############################################################################
# Summarize data
###############################################################################
# summary(data)
# prop.table(table(data$Catergory1))
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
