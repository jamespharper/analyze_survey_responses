###############################################################################
# INITIALIZE
###############################################################################
rm(list = ls())                                      # Clear global environment
cat("\014")                                          # Clear console window
file.remove(dir(paste(getwd(),"/output/", sep = ""), # Clear output folder
                full.names = TRUE))    
source("functions.R")                                # Load custom functions
load_libraries("rio")                                # Install & load libraries

###############################################################################
# LOAD DATA
###############################################################################
file_to_import = paste(getwd(), "/data/data.xlsx",
                       sep = "")
data = import(file_to_import)
# names(data)

###############################################################################
# CLEAN DATA
###############################################################################
# Shorten variable (column) names
var_desc = names(data)
names(data) = c("NewName1", "NewName2", "NewName3", "NewName4")
print(data.frame(names(data), var_desc))     # Verify consistency

# Convert data formats
for (i in 1:length(names(data))) {
  if (is.character(data[i][[1]])) {
    data[i][[1]] = as.factor(data[i][[1]])            # Characters into factors
  }
}
data$Date = as.Date(data$Date)                          # Date into date format

# Remove unused columns if needed
drops = c("NewName1", "NewName2")
data = data[, !(names(data) %in% drops)]

# Remove rows with an empty variable
summary(data$Var1)                                      # Before
data = subset(data, !is.na(Var1))
summary(data$Var1)                                      # After

# Rename responses in IDPoorTyp
summary(data$Var1)                                           # Before
levels(data$Var1)[match("Long name that should be shorter",
                        levels(data$Var1))] = "Shorter name"
summary(data$IDPoorTyp)                                      # After

# Create Yr and Mnth variables as factors
data$Yr = as.factor(format(as.Date(data$Date, format="%Y-%m-%d"),"%Y"))
data$Mnth = as.factor(format(as.Date(data$Date, format="%Y-%m-%d"),"%m"))

# Save data to disk
save(data, file = "data.RData")