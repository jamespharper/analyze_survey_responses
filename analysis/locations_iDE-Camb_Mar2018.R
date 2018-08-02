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
                 "corrplot", "ggpubr", "rgl", "missMDA",
                 "pscl", "Amelia"))

###############################################################################
# LOAD AND CLEAN DATA
###############################################################################
file_to_import = paste(getwd(), "/data/Villages exported attribute table.csv",
                       sep = "")
locs = import(file_to_import)
locs$Code = as.factor(locs$Code)
locs$Name = as.factor(locs$Name)
summary(locs)
subset(locs, Code == "14110403")
subset(locs, Name == "1271778")
locs = subset(locs, Name != "1271778")
subset(locs, Code == "14110502")
subset(locs, Name == "1271393")
locs = subset(locs, Name != "1271393")
subset(locs, Code == "14120402")
subset(locs, Name == "Ta Kreab\r\n")
locs = subset(locs, Name != "Ta Kreab\r\n")
subset(locs, Code == "14111001")
subset(locs, Name == "1273806.231")
locs = subset(locs, Name != "1273806.231")
subset(locs, Code == "14110503")
subset(locs, Name == "1272613")
locs = subset(locs, Name != "1272613")
summary(locs)
summary(locs$Name, maxsum = 1000)

###############################################################################
# SAVE DATA TO DISK
###############################################################################
save(locs, file = "locations.RData")
load(file = "locations.RData")