###############################################################################
# INITIALIZE
###############################################################################
rm(list = ls())                                      # Clear global environment
cat("\014")                                          # Clear console window
file.remove(dir(paste(getwd(),"/output/", sep = ""), # Clear output folder
                full.names = TRUE))    
source("functions.R")                                # Load custom functions
load_libraries(c("rio", "Amelia"))                   # Install & load libraries

###############################################################################
# LOAD DATA
###############################################################################
file.to.import1 = 
  paste(getwd(),
        "/data/Survey Data/iDE, 2018 - FSM Survey, Baseline, data only.xlsx",
        sep = "")
file.to.import2 = 
  paste(getwd(),
        "/data/Survey Data/iDE, 2018 - FSM Survey, Follow-up, data only.xlsx",
        sep = "")
data1 = import(file.to.import1)
data2 = import(file.to.import2)
names(data1)
names(data2)

###############################################################################
# CLEAN DATA1
###############################################################################
# Shorten variable (column) names
old.col.names1 = names(data1)
names(data1) = c("ID", "HHID", "Intrviewr", "PGInstlDate", "Gend", "Vill", 
                "Comm", "Dist", "Prov", "Lat", "Lon", "VillTyp",
                "RelToLatOwnr", "HavIDPoorCard", "LatOwnrIDPoor", 
                "LatOwnrJob1", "LatOwnrJob2", "NumAdlts18", "NumChld217",
                "NumInf2", "NumPplHH", "LatInstlYr", "NumRings",
                "DateStartUseLat", "UseLatReg", "EmptyPitBefor", "WhyNoEmpty",
                "EmptyMethds", "PiercdPit", "SurveyYr", "LatAge")
print(data.frame(names(data1), old.col.names1))     # Verify consistency

# Remove unused variables
data1 = subset(data1, select = -c(ID, HHID, SurveyYr))

# Convert data formats
data1$PGInstlDate = as.Date(data1$PGInstlDate)
# data1$LatDouble = as.double(data1$Lat)
for (i in 1:length(names(data1))) {                # All characters into factors
  if (is.character(data1[i][[1]])) {
    data1[i][[1]] = as.factor(data1[i][[1]])
  }
}
summary(data1, maxsum = 10)

###############################################################################
# CLEAN DATA2
###############################################################################
# Shorten variable (column) names
old.col.names2 = names(data2)
names(data2) = c("ID", "Intrviewr", "Date", "Prov", "Dist", "Comm", "Vill", 
                 "VillTyp", "RGend", "HavLat", "RPurLat", "CGend", 
                 "RelToLatOwnr", "RelToLatOwnr_Othr", "IDPoor", "IDPoorTyp",
                 "NumM61HH", "NumM1860HH", "NumM518HH", "NumM05HH", "NumF61HH",
                 "NumF1860HH", "NumF518HH", "NumF05HH", "NumPplHH", 
                 "NumM61LatUsr", "NumM1860LatUsr", "NumM518LatUsr",
                 "NumM05LatUsr", "NumF61LatUsr", "NumF1860LatUsr", 
                 "NumF518LatUsr", "NumF05LatUsr", "NumPplHHLatUsr", "LivRP",
                 "LivField", "HousFlod", "FlodSevrty", "FlodSevrty.Othr",
                 "RoadAccsTrck", "LatInstldDate", "LatStartUseDate", "PiercdPit",
                 "NumPits", "NumRngs", "NumRngs.Othr", "PGInstld", "PGID",
                 "EmptyBefor", "EmptyChlngs", "EmptyChlngs.Othr", "EmptyNum",
                 "EmptyLast", "EmptyMethds", "EmptyMethds.Othr", "EmptyWho", 
                 "EmptyWho.Othr", "EmptyCost", "EmptyExprience", "EmptyTime", 
                 "EmptyWhy", "EmptyWhy.Othr", "EmptyDispos", 
                 "EmptyDispos.Othr", "EmptyWhyNot", "EmptyWhyNot.Othr", 
                 "EmptyPlan", "EmptyPlanMethds", "EmptyPlanMethds.Othr", 
                 "EmptyWilPay", "FSMServProvdrs", "FSMServProvdrs.Contact",
                 "FSMServProvdrs.Cost", "CommntQues")
print(data.frame(names(data2), old.col.names2))     # Verify consistency

# Remove unused variables
data2 = subset(data2, select = -c(ID, PGID))

# Convert data formats
data2$Date = as.Date(data2$Date)
data2$NumF61HH = as.numeric(data2$NumF61HH)
data2$LatInstldDate = as.Date(data2$LatInstldDate)
data2$LatStartUseDate = as.Date(data2$LatStartUseDate)
for (i in 1:length(names(data2))) {               # All characters into factors
  if (is.character(data2[i][[1]])) {
    data2[i][[1]] = as.factor(data2[i][[1]])
  }
}
summary(data2, maxsum = 10)

###############################################################################
# SUMMARIZE DATA
###############################################################################
summary(data1, maxsum = 10)
summary(data2, maxsum = 10)

###############################################################################
# SAVE DATA TO DISK
###############################################################################
save(data1, data2, file = "iDE_May2018.RData")
# load(file = "iDE_Oct2017.RData")

###############################################################################
# DATA QUALITY CONTROL
###############################################################################
# Visualize NAs
missmap(data1, main = "Missing vs observed", legend = F)
missmap(data2, main = "Missing vs observed", legend = F)

# Search for NAs and reduce to important data
summary(data1)
sapply(data1, function(x) sum(is.na(x)))
summary(data2)
sapply(data2, function(x) sum(is.na(x)))

# TASKS REMAINING
# Skipped data$RDefBeforOthr
# Khmer character in Borw$Lat
# Clean CanBuyLat thru LatTypOwndBefor
# Double check IntndChng renames
# Skipped data$IntndChngOthr
# Skipped data$IntndPitFullOthr
# Skipped data$DateSurvCreated
