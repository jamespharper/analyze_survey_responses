# Analysis of Survey Responses of Rural Cambodian Latrine Owners
# Written by James Harper, PE, ENV SP of the University of Colorado Boulder
# Started October 1, 2017; Last updated Aug 8, 2018
###############################################################################
# Initialize environment and user input
rm(list = ls())                                      # Clear global environment
cat("\014")                                              # Clear console window
source("functions.R")                                   # Load custom functions
load_libraries(                                      # Install & load libraries
  c("rio", "gplots", "Amelia", "car", "fastDummies", "FactoMineR", "lavaan"))
loadfonts(device = "win")
clean.raw.data = 0                                      # Clean raw data: 1=yes
###############################################################################
# Load, clean, and summarize data
if (clean.raw.data == 0) {
  load(file = paste(getwd(),"/data/raw/surveys/iDE_May2018.RData", sep = ""))
  } else if (clean.raw.data == 1) {
    file.to.import1 = 
      paste(
        getwd(),
        "/data/raw/surveys/iDE, 2018 - FSM Survey, Baseline, data only.xlsx",
        sep = "")
    file.to.import2 = 
      paste(
        getwd(),
        "/data/raw/surveys/iDE, 2018 - FSM Survey, Follow-up, data only.xlsx",
        sep = "")
    data.baseline = import(file.to.import1)
    data.followup = import(file.to.import2)
    summary(data.baseline, maxsum = 30)
    summary(data.followup, maxsum = 30)
    names(data.baseline)
    names(data.followup)
    
    ###########################################################################
    # CLEAN data.baseline
    ###########################################################################
    # Shorten variable (column) names
    old.col.names1 = names(data.baseline)
    names(data.baseline) = c("ID", "HHID", "Intrviewr", "PGInstlDate", "Gend", "Vill",
                     "Comm", "Dist", "Prov", "Lat", "Lon", "VillTyp",
                     "RelToLatOwnr", "HavIDPoorCard", "LatOwnrIDPoor",
                     "LatOwnrJob1", "LatOwnrJob2", "NumAdlts18", "NumChld217",
                     "NumInf2", "NumPplHH", "LatInstlYr", "NumRings",
                     "DateStartUseLat", "UseLatReg", "EmptyPitBefor", 
                     "WhyNoEmpty", "EmptyMethds", "PiercdPit", "SurveyYr", 
                     "LatAge")
    print(data.frame(names(data.baseline), old.col.names1))        # Verify consistency
    
    # Remove unused variables
    data.baseline = subset(data.baseline, select = -c(ID, HHID, SurveyYr))
    
    # Convert data formats
    data.baseline$PGInstlDate = as.Date(data.baseline$PGInstlDate)
    data.baseline$Lat = as.numeric(data.baseline$Lat)
    data.baseline$Lon = as.numeric(data.baseline$Lon)
    for (i in 1:length(names(data.baseline))) {           # All characters into factors
      if (is.character(data.baseline[i][[1]])) {
        data.baseline[i][[1]] = as.factor(data.baseline[i][[1]])
      }
    }
    summary(data.baseline, maxsum = 10)
    
    # Rename and convert data format of NumInf2
    summary(data.baseline$NumInf2)  # Before
    levels(data.baseline$NumInf2) = c("0", "1", "2", "3", "0")
    summary(data.baseline$NumInf2)  # After
    data.baseline$NumInf2 = as.numeric(data.baseline$NumInf2)
    summary(data.baseline$NumInf2)  # After
    
    # Rename responses in RelToLatOwnr
    summary(data.baseline$RelToLatOwnr)  # Before
    levels(data.baseline$RelToLatOwnr) = c("LatOwnr", "Child", "LatOwnr", 
                                           "Sibling", "Child", "Child",
                                           "Child", "Parent", "Parent",
                                           "Parent", "Child", "Child",
                                           "LatOwnr", "LatOwnr", "Child",
                                           "Parent", "Parent", "LatOwnr",
                                           "Sibling", "Child", "Child",
                                           "Spouse")
    summary(data.baseline$RelToLatOwnr)  # After
    
    # Rename responses in LatInstlYr
    summary(data.baseline$LatInstlYr, maxsum = 30)  # Before
    data.baseline$LatInstlYr[data.baseline$LatInstlYr == "Not Remember"] = 
      NA
    data.baseline$LatInstlYr = droplevels(data.baseline$LatInstlYr)
    summary(data.baseline$LatInstlYr)  # After
    
    # Rename responses in DateStartUseLat
    summary(data.baseline$DateStartUseLat, maxsum = 30)  # Before
    data.baseline$DateStartUseLat[data.baseline$DateStartUseLat == 
                                    "Not Remember"] = NA
    data.baseline$DateStartUseLat = droplevels(data.baseline$DateStartUseLat)
    summary(data.baseline$DateStartUseLat)  # After
    
    # Create new variable Job based on LatOwnrJob1 and LatOwnrJob2
    summary(data.baseline$LatOwnrJob1)
    summary(data.baseline$LatOwnrJob2)
    print(data.frame(data.baseline$LatOwnrJob1, data.baseline$LatOwnrJob2))
    # plot(summary(data.baseline$LatOwnrJob2))
    data.baseline$Job = as.character(NA)
    for (row in 1:length(data.baseline$LatOwnrJob2)) {
      if (is.na(data.baseline[row,]$LatOwnrJob2)) {
        data.baseline[row,]$Job = NA
      } else if (data.baseline[row,]$LatOwnrJob2 == "Blacksmith" |
                 data.baseline[row,]$LatOwnrJob2 == "Cameraman" |
                 data.baseline[row,]$LatOwnrJob2 == "Car repair" |
                 data.baseline[row,]$LatOwnrJob2 == "Carpenter" |
                 data.baseline[row,]$LatOwnrJob2 == "Engineer" |
                 data.baseline[row,]$LatOwnrJob2 == "Mason" |
                 data.baseline[row,]$LatOwnrJob2 == "Nurse" ) {
        data.baseline[row,]$Job = "Technical"
      } else if (data.baseline[row,]$LatOwnrJob2 == "Commune Chief" |
                 data.baseline[row,]$LatOwnrJob2 == "Deputy village chief" |
                 data.baseline[row,]$LatOwnrJob2 == "Village chief" |
                 data.baseline[row,]$LatOwnrJob2 == "Village Chief" |
                 data.baseline[row,]$LatOwnrJob2 == 
                 "Village Chief\r\n Assistant" ) {
        data.baseline[row,]$Job = "Leadership"
      } else if (data.baseline[row,]$LatOwnrJob2 == "Clergyman" |
                 data.baseline[row,]$LatOwnrJob2 == "Factory worker" |
                 data.baseline[row,]$LatOwnrJob2 == "Grocery seller" |
                 data.baseline[row,]$LatOwnrJob2 == "Hotel staff" | 
                 data.baseline[row,]$LatOwnrJob2 == "Housekeeper" | 
                 data.baseline[row,]$LatOwnrJob2 == "Labor woker" | 
                 data.baseline[row,]$LatOwnrJob2 == "Moto taxi" | 
                 data.baseline[row,]$LatOwnrJob2 == "Motor taxi" | 
                 data.baseline[row,]$LatOwnrJob2 == "Petrol seller" | 
                 data.baseline[row,]$LatOwnrJob2 == "Scrap dealer" |
                 data.baseline[row,]$LatOwnrJob2 == "Seller" |
                 data.baseline[row,]$LatOwnrJob2 == "Small shop" |
                 data.baseline[row,]$LatOwnrJob2 == "Taxi driver" |
                 data.baseline[row,]$LatOwnrJob2 == "Trade" |
                 data.baseline[row,]$LatOwnrJob2 == 
                 "Traditional medicine seller" ) {
        data.baseline[row,]$Job = "Unskilled"
      } else if (data.baseline[row,]$LatOwnrJob2 == "NGO staff" |
                 data.baseline[row,]$LatOwnrJob2 == "Police" |
                 data.baseline[row,]$LatOwnrJob2 == "Teacher" ) {
        data.baseline[row,]$Job = "OtherSkilled"
      } else if (data.baseline[row,]$LatOwnrJob2 == "Student" ) {
        data.baseline[row,]$Job = "Student"
      } else if (data.baseline[row,]$LatOwnrJob2 == "Farmer" ) {
        data.baseline[row,]$Job = "Agriculture"
      }
    }
    print(data.frame(data.baseline$Job, data.baseline$LatOwnrJob1, 
                     data.baseline$LatOwnrJob2))  # After
    data.baseline$Job = as.factor(data.baseline$Job)
    summary(data.baseline$Job)
    
    # Rename UseLatReg
    summary(data.baseline$UseLatReg)  # Before
    levels(data.baseline$UseLatReg) = c("Never", "Some", "Always")
    summary(data.baseline$UseLatReg)  # After
    
    # Rename EmptyPitBefor
    summary(data.baseline$EmptyPitBefor)  # Before
    levels(data.baseline$EmptyPitBefor) = c("No", "Yes", "Yes")
    summary(data.baseline$EmptyPitBefor)  # After
    
    # Rename WhyNoEmpty
    summary(data.baseline$WhyNoEmpty)  # Before
    levels(data.baseline$WhyNoEmpty) = c("Busy", "Busy", "NotClogged", 
                                         "NotUsing", "NoProAvailable",
                                         "NoProAvailable", "NotFull",
                                         "NotFull", "NotFull", 
                                         "PiercedPit", "NotUsing",
                                         "Abandoned")
    summary(data.baseline$WhyNoEmpty)  # After
    
    # Rename EmptyMethds
    summary(data.baseline$EmptyMethds)  # Before
    levels(data.baseline$EmptyMethds) = c("Self-Bucketing", "Self-Pump", 
                                          "PaidPro", "Self-Pump", 
                                          "Self-Pump", "Self-Pump",
                                          "Self-Bucketing", "Self-Bucketing", 
                                          "Unknown", "Self-Pump")
    summary(data.baseline$EmptyMethds)  # After
    
    # Rename PiercdPit
    summary(data.baseline$PiercdPit)  # Before
    levels(data.baseline$PiercdPit) = c("No", "No", "Yes")
    summary(data.baseline$PiercdPit)  # After
    
    # Remove inaccurate values in LatAge
    summary(as.factor(data.baseline$LatAge))  # Before
    for (row in 1:length(data.baseline$LatAge)) {
      if (!is.na(data.baseline$LatAge[row]) & 
          data.baseline$LatAge[row] > 50) {
        data.baseline$LatAge[row] = NA
      }
    }
    summary(as.factor(data.baseline$LatAge))  #After
    
    ###########################################################################
    # CLEAN data.followup
    ###########################################################################
    # Shorten variable (column) names
    old.col.names2 = names(data.followup)
    names(data.followup) = c("ID", "Intrviewr", "Date", "Prov", "Dist", "Comm", "Vill", 
                     "Group", "RGend", "HavLat", "RPurLat", "CGend", 
                     "RelToLatOwnr", "RelToLatOwnr_Othr", "IDPoor", 
                     "IDPoorTyp", "NumM61HH", "NumM1860HH", "NumM518HH", 
                     "NumM05HH", "NumF61HH", "NumF1860HH", "NumF518HH", 
                     "NumF05HH", "NumPplHH", "NumM61LatUsr", "NumM1860LatUsr", 
                     "NumM518LatUsr", "NumM05LatUsr", "NumF61LatUsr",
                     "NumF1860LatUsr", "NumF518LatUsr", "NumF05LatUsr",
                     "NumPplHHLatUsr", "LivRP", "LivField", "HousFlod",
                     "FlodSevrty", "FlodSevrty.Othr", "RoadAccsTrck",
                     "LatInstldDate", "LatStartUseDate", "PiercdPit",
                     "NumPits", "NumRngs", "NumRngs.Othr", "PGInstld", "PGID",
                     "EmptyBefor", "EmptyChlngs", "EmptyChlngs.Othr",
                     "EmptyNum", "EmptyLast", "EmptyMethds", 
                     "EmptyMethds.Othr", "EmptyWho", "EmptyWho.Othr",
                     "EmptyCost", "EmptyExprience", "EmptyTime", "EmptyWhy",
                     "EmptyWhy.Othr", "EmptyDispos", "EmptyDispos.Othr",
                     "EmptyWhyNot", "EmptyWhyNot.Othr", "EmptyPlan",
                     "EmptyPlanMethds", "EmptyPlanMethds.Othr", "EmptyWilPay",
                     "FSMServProvdrs", "FSMServProvdrs.Contact",
                     "FSMServProvdrs.Cost", "CommntQues")
    print(data.frame(names(data.followup), old.col.names2))     # Verify consistency
    
    # Remove unused variables
    data.followup = subset(data.followup, select = -c(ID, PGID))
    
    # Convert data formats
    data.followup$Date = as.Date(data.followup$Date)
    data.followup$NumF61HH = as.numeric(data.followup$NumF61HH)
    data.followup$LatInstldDate = as.Date(data.followup$LatInstldDate)
    data.followup$LatStartUseDate = as.Date(data.followup$LatStartUseDate)
    for (i in 1:length(names(data.followup))) {           # All characters into factors
      if (is.character(data.followup[i][[1]])) {
        data.followup[i][[1]] = as.factor(data.followup[i][[1]])
      }
    }
    summary(data.followup, maxsum = 10)
    
    # Rename RGend
    summary(data.followup$RGend)  # Before
    levels(data.followup$RGend) = c("F", "M")
    summary(data.followup$RGend)  # After
    
    # Rename CGend
    summary(data.followup$CGend)  # Before
    levels(data.followup$CGend) = c("F", "M")
    summary(data.followup$CGend)  # After
    
    # Rename RelToLatOwnr
    summary(data.followup$RelToLatOwnr)  # Before
    levels(data.followup$RelToLatOwnr) = c("Sibling", "Spouse", "Parent", "Other", "Self")
    summary(data.followup$RelToLatOwnr)  # After
    for (row in 1:length(data.followup$RelToLatOwnr)) {
      if (is.na(data.followup$RelToLatOwnr[row])) {
        if (!is.na(data.followup$RPurLat[row]) & 
            data.followup$RPurLat[row] == "Yes") {
          data.followup$RelToLatOwnr[row] = "Self"
          }
      }
    }
    summary(data.followup$RelToLatOwnr)  # After
    
    # Rename HousFlod
    summary(data.followup$LivRP)  # Before
    levels(data.followup$LivRP) = c("No", "No")
    summary(data.followup$LivRP)  # After
    
    # Rename HousFlod
    summary(data.followup$HousFlod)  # Before
    levels(data.followup$HousFlod) = c("No", "No", "Yes")
    summary(data.followup$HousFlod)  # After
    
    # Rename FlodSevrty
    summary(data.followup$FlodSevrty)  # Before
    levels(data.followup$FlodSevrty) = c("Moderate", "Moderate", "Other",
                                         "Severe")
    summary(data.followup$FlodSevrty)  # After
    for (row in 1:length(data.followup$FlodSevrty)) {
      if (!is.na(data.followup$FlodSevrty[row])) {
        if (data.followup$FlodSevrty[row] == "Other") {
          data.followup$FlodSevrty[row] = NA
        }
      }
    }
    summary(data.followup$FlodSevrty)  # After
    data.followup$FlodSevrty = droplevels(data.followup$FlodSevrty)
    summary(data.followup$FlodSevrty)  # After
    
    
    
    # Create new variable Group2 based on Group and PGInstld
    summary(data.followup$Group)  # Before
    data.followup$Group2 = as.character(NA)
    for (row in 1:length(data.followup$Group)) {
      if (is.na(data.followup[row,]$Group) | is.na(data.followup[row,]$PGInstld)) {
        data.followup[row,]$Group2 = NA
      } else if (data.followup[row,]$Group == "Control") {
        data.followup[row,]$Group2 = "Cntrl"
      } else if (data.followup[row,]$Group == "Treatment" & 
                 data.followup[row,]$PGInstld == "Yes") {
        data.followup[row,]$Group2 = "TreatPG"
      } else if (data.followup[row,]$Group == "Treatment" & 
                 data.followup[row,]$PGInstld == "No") {
        data.followup[row,]$Group2 = "TreatNei"
      }
    }
    print(data.frame(data.followup$Group, data.followup$PGInstld, data.followup$Group2))  # After
    data.followup$Group2 = as.factor(data.followup$Group2)
    summary(data.followup$Group2)
    
    # Remove rows without full group description
    data.followup = subset(data.followup, !is.na(Group2))
    
    # Rename responses in EmptyPlanMethds
    summary(data.followup$EmptyPlanMethds)  # Before
    levels(data.followup$EmptyPlanMethds) = c("DK", "Bucket", "Othr", "VTrck")
    summary(data.followup$EmptyPlanMethds)  # After
    
    # Rename responses in EmptyPlanMethds.Othr
    summary(data.followup$EmptyPlanMethds.Othr)  # Before
    levels(data.followup$EmptyPlanMethds.Othr) = c("Pump", "Pump", "FSMServProvdr", 
                                           "MixAsh", "Pump", "VTrck")
    summary(data.followup$EmptyPlanMethds.Othr)  # After
    
    # If EmptyPlanMethds is Othr or NA, then copy EmptyPlanMethds.Othr to 
    # EmptyPlanMethds
    data.followup$EmptyPlanMethds = as.character(data.followup$EmptyPlanMethds)
    data.followup$EmptyPlanMethdsOLD = data.followup$EmptyPlanMethds
    data.followup$EmptyPlanMethds.Othr = as.character(data.followup$EmptyPlanMethds.Othr)
    print(data.frame(data.followup$EmptyPlanMethds, data.followup$EmptyPlanMethds.Othr))
    for (row in 1:length(data.followup$EmptyPlanMethds)) {
      if (data.followup[row,]$EmptyPlanMethds == "Othr" | 
          is.na(data.followup[row,]$EmptyPlanMethds)) {
        data.followup[row,]$EmptyPlanMethds = data.followup[row,]$EmptyPlanMethds.Othr
      }
    }
    data.followup$EmptyPlanMethds = as.factor(data.followup$EmptyPlanMethds)
    data.followup$EmptyPlanMethds.Othr = as.factor(data.followup$EmptyPlanMethds.Othr)
    data.followup$EmptyPlanMethdsOLD = as.factor(data.followup$EmptyPlanMethdsOLD)
    options(max.print = 1000000)
    print(data.frame(data.followup$EmptyPlanMethdsOLD, data.followup$EmptyPlanMethds,
                     data.followup$EmptyPlanMethds.Othr))   # After
    summary(data.followup$EmptyPlanMethds)
    
    # Rename responses in EmptyWilPay to describe # Cambodian Riel to empty a
    # 3-ring pit
    summary(data.followup$EmptyWilPay)  # Before
    levels(data.followup$EmptyWilPay) = c(3*4054.50, 0, 10000, 100000, 14000, 1500, 
                                  15000, 18000, 20000, 200000, 30000, 3500, 
                                  3700, 40000, 50*4054.50, 50000, 6000, 60000, 
                                  7000, 9500, "DK", "DK")
    summary(data.followup$EmptyWilPay)  # After
    
    # Rename responses in EmptyCost
    summary(data.followup$EmptyCost)  # Before
    levels(data.followup$EmptyCost) = c(0, 0, 10000, 12000, 120000, 1500, 20000, 3500,
                                3700, 4000, 40000, 50000, 50000, 7000, "DK", 
                                "DK", "DK")
    summary(data.followup$EmptyCost)  # After
    
    # Rename responses in EmptyChlngs
    summary(data.followup$EmptyChlngs)  # Before
    levels(data.followup$EmptyChlngs) = c("None", "EmptyReqTooFreq")
    summary(data.followup$EmptyChlngs)  # After
    
    # Rename responses in EmptyLast
    # summary(data.followup$EmptyLast, maxsum = 50)  # Before
    # levels(data.followup$EmptyLast) = c("2001", "2004", "2008", "2011", "2012", 
    #                                     "2014", "2015", "2016", "2017", "2018", 
    #                                     "12/2012", "7/2016", "1/2017")
    # summary(data.followup$EmptyLast)  # After
    
    # Rename responses in EmptyMethds
    summary(data.followup$EmptyMethds, maxsum = 50)  # Before
    levels(data.followup$EmptyMethds) = c("Self-Bucketing", "Self-Pump")
    summary(data.followup$EmptyMethds)  # After
    
    # Rename responses in EmptyWho
    summary(data.followup$EmptyWho, maxsum = 50)  # Before
    levels(data.followup$EmptyWho) = c("FamilyMemb", "Pro", "Self")
    summary(data.followup$EmptyWho)  # After
    
    # Rename responses in EmptyExprience
    summary(data.followup$EmptyExprience, maxsum = 50)  # Before
    levels(data.followup$EmptyExprience) = c("Good", "Neutral", "Bad")
    summary(data.followup$EmptyExprience)  # After
    
    # CLEANING TASKS REMAINING
    # Skipped data.followup$NumRngs and data.followup$NumRngs.Othr
    #    because not relevant
    # Khmer character in Borw$Lat
    # Clean CanBuyLat thru LatTypOwndBefor
    # Double check IntndChng renames
    # Skipped data$IntndChngOthr
    # Skipped data$IntndPitFullOthr
    # Skipped data$DateSurvCreated
    
    ###############################################################################
    # DATA QUALITY CONTROL
    ###############################################################################
    summary(data.baseline, maxsum = 30)
    summary(data.followup, maxsum = 30)
    # missmap(data.baseline, main = "Missing vs observed", legend = F)  # visualize NAs
    # missmap(data.followup, main = "Missing vs observed", legend = F)
    sapply(data.baseline, function(x) sum(is.na(x)))  # count NAs
    sapply(data.followup, function(x) sum(is.na(x)))
    
    ###############################################################################
    # SAVE DATA TO DISK
    ###############################################################################
    save(data.baseline, data.followup,
         file = paste(getwd(),"/data/raw/surveys/iDE_May2018.RData", sep = ""))

  }
load(file = paste(getwd(),"/data/raw/surveys/iDE_Oct2017.Rdata", sep = ""))
cat("\014")                                              # Clear console window
summary(data.baseline)
summary(data.followup)
names(data.baseline)
names(data.followup)
print(sapply(data.baseline, function(x) sum(is.na(x))))
print(sapply(data.followup, function(x) sum(is.na(x))))
missmap(data.baseline, main = "Missing Values in Variables", legend = F)
missmap(data.followup, main = "Missing Values in Variables", legend = F)
###############################################################################


# Exploratory Factor Analysis (EFA)
##libraries
library(GPArotation)
library(car)
library(psych)
##normal data screening (fake style) goes here
##screen all the items (but not demographics)

data.efa = data.baseline
summary(data.efa)
# ##correlation adequacy Bartlett's test
# correlations = cor(efadata)
# cortest.bartlett(correlations, n = nrow(efadata))
# 
# ##sampling adequacy KMO test
# KMO(correlations)

##how many factors?
num_factors = fa.parallel(data.efa, fm = "ml", fa = "fa")
nofactors$fa.values
sum(nofactors$fa.values > 1.0) ##old kaiser criterion
sum(nofactors$fa.values > .7) ##new kaiser criterion

##simple structure with a two factor model
fa(efadata, nfactors=2, rotate = "oblimin", fm = "ml")
fa(efadata[ , -c(23)], nfactors=2, rotate = "oblimin", fm = "ml")

##get cfi
finalmodel = fa(efadata[ , -c(23)], nfactors=2, rotate = "oblimin", fm = "ml")
1 - ((finalmodel$STATISTIC-finalmodel$dof)/
       (finalmodel$null.chisq-finalmodel$null.dof))

##reliability
factor1 = c(1:7, 9:10, 12:16, 18:22)
factor2 = c(8, 11, 17)
alpha(efadata[, factor1])
alpha(efadata[, factor2])

##simple structure with a five factor model
##an example of OVERfactoring
fa(efadata, nfactors=5, rotate = "oblimin", fm = "ml")
fa(efadata[ , -c(15)], nfactors=5, rotate = "oblimin", fm = "ml")
##all items load but factor five only has two items
##try four factor model
fa(efadata[ , -c(15)], nfactors=4, rotate = "oblimin", fm = "ml")
fa(efadata[ , -c(3,14,15)], nfactors=4, rotate = "oblimin", fm = "ml")
fa(efadata[ , -c(3,7,10,14,15,18)], nfactors=4, rotate = "oblimin", fm = "ml")
##at this point you would get rid of 12, and then factor four
##only has two items ... this pattern indicates that you should 
##try the smaller numbers of factors


# Multiple Correspondence Analysis
# Select relevant data
data.mca = subset(data.baseline, select = -c(Intrviewr, PGInstlDate, Dist, 
                                             Prov, Lat, Lon, LatOwnrJob1,
                                             LatOwnrJob2, NumRings, LatAge,
                                             LatInstlYr))
# Remove/rename missing data
print(sapply(data.mca, function(x) sum(is.na(x))))
data.mca$NumAdlts18[is.na(data.mca$NumAdlts18)] = 
  round(mean(!is.na(data.mca$NumAdlts18)), 0)
data.mca$NumChld217[is.na(data.mca$NumChld217)] = 
  round(mean(!is.na(data.mca$NumChld217)), 0)
data.mca$NumInf2[is.na(data.mca$NumInf2)] = 
  round(mean(!is.na(data.mca$NumInf2)), 0)
levels(data.mca$UseLatReg) = c(levels(data.mca$UseLatReg), "Unknown")
data.mca$UseLatReg[is.na(data.mca$UseLatReg)] = "Unknown"
levels(data.mca$EmptyPitBefor) = c(levels(data.mca$EmptyPitBefor), "Unknown")
data.mca$EmptyPitBefor[is.na(data.mca$EmptyPitBefor)] = "Unknown"
levels(data.mca$WhyNoEmpty) = c(levels(data.mca$WhyNoEmpty), "Emptied")
data.mca$WhyNoEmpty[is.na(data.mca$WhyNoEmpty)] = "Emptied"
levels(data.mca$EmptyMethds) = c(levels(data.mca$EmptyMethds), "HasntEmptied")
data.mca$EmptyMethds[is.na(data.mca$EmptyMethds)] = "HasntEmptied"
levels(data.mca$Job) = c(levels(data.mca$Job), "Unknown")
data.mca$Job[is.na(data.mca$Job)] = "Unknown"
print(sapply(data.mca, function(x) sum(is.na(x))))
# Change data types
data.mca$DateStartUseLat[is.na(data.mca$DateStartUseLat)] = 
  round(mean(as.numeric(as.character(
    data.mca$DateStartUseLat[!is.na(data.mca$DateStartUseLat)]))), 0)
data.mca$DateStartUseLat = as.numeric(as.character(data.mca$DateStartUseLat))
# Run analysis
varTable(data.mca)
quali.sup = c(2:5)
quanti.sup = c(8:12)
results = MCA(data.mca, quali.sup = quali.sup, quanti.sup = quanti.sup)
# Print and plot results
summary(results, ncp = 3, nbelements = Inf)
dimdesc(results)
plot(results, label = c("var","quali.sup"), cex = 0.7)
plot(results, invisible = c("var","quali.sup"), cex = 0.7)
plot(results, invisible = c("ind","quali.sup"), autoLab = "y", cex = 0.7, title = "Active Categories")
plot(results, invisible = c("ind","quali.sup"), autoLab = "y", cex = 0.7, title = "Active Categories", selectMod = "contrib 20")
plot5 = recordPlot(plot(results, invisible = c("ind","quali.sup"), cex = 0.7, title = "Active Categories"))
plot6 = recordPlot(plot(results, invisible = c("ind","var"), autoLab = "y", cex = 0.7, title = "Supplementary Categories"))
plot7 = recordPlot(plot(results, invisible = "ind", autoLab = "y", cex = 0.7, selectMod = "cos2 10"))
plot8 = recordPlot(plot(results, invisible = "ind", autoLab = "y", cex = 0.7, selectMod = "contrib 20"))
plot9 = recordPlot(plot(results, invisible = c("var","quali.sup"), autoLab = "y", cex = 0.7, select = "cos2 10"))
plot10 = recordPlot(plot(results, autoLab = "y", cex = 0.7, selectMod = "cos2 20", select = "cos2 10"))
plot11 = recordPlot(plot(results, choix = "var", xlim = c(0,0.6), ylim = c(0,0.6)))
plot12 = recordPlot(plot(results, choix = "var", xlim = c(0,0.6), ylim = c(0,0.6), invisible = c("ind","quali.sup")))
plot13 = recordPlot(plot(results, invisible = c("var","quali.sup"), cex = 0.7, select = "contrib 20", axes = 3:4))
plot14 = recordPlot(plot(results, invisible = c("ind"), cex = 0.7, select = "contrib 20", axes = 3:4))
plot15 = recordPlot(plotellipses(results, keepvar = c(1:4)))

# Save plots to PDF
ggexport(plotlist = list(plot1, plot2, plot3, plot4, plot5, plot6, plot7,
                         plot8, plot9, plot10, plot11, plot12, plot13,
                         plot14, plot15),
         filename = paste(folder, "/", name, ".pdf", sep = ""))


# Confirmatory Factor Analysis
?HolzingerSwineford1939
model = "visual  =~ x1 + x2 + x3
         textual =~ x4 + x5 + x6
         speed   =~ x7 + x8 + x9"
fit = cfa(model, data = HolzingerSwineford1939)
summary(fit, fit.measures = TRUE)

model.group = "visual  =~ x1 + x2 + x3
               textual =~ x4 + x5 + x6
               speed   =~ x7 + x8 + x9"
fit = cfa(model.group, data = HolzingerSwineford1939, group = "school")
summary(fit)

# Structural Equation Modeling
model = "
  # measurement model
    ind60 =~ x1 + x2 + x3
    dem60 =~ y1 + y2 + y3 + y4
    dem65 =~ y5 + y6 + y7 + y8
  # regressions
    dem60 ~ ind60
    dem65 ~ ind60 + dem60
  # residual correlations
    y1 ~~ y5
    y2 ~~ y4 + y6
    y3 ~~ y7
    y4 ~~ y8
    y6 ~~ y8"
fit = sem(model, data = PoliticalDemocracy)
summary(fit, standardized = TRUE)

data.test = subset(data, !is.na(IntndPitFull))
data.test = subset(data.test, Yr == 2017)
data.test = subset(data.test, 
                   select = c(Prov, IDPoor, VillOD, Mnth,         # Exogenous
                              IntndPitFull, Satis, Rec, SatisSup, # Endogenous
                                            RecSup))
data.test = subset(data.test, Satis != "DK")
data.test = subset(data.test, SatisSup != "DK")
data.test = droplevels(data.test)
summary(data.test)
varTable(data.test)
data.test$Prov = recode(data.test$Prov, "'Banteay Meanchey' = 'BM';
                                         'Kampong Thom'     = 'KT';
                                         'Kandal'           = 'KL';
                                         'Oddar Meanchey'   = 'OM';
                                         'Prey Veng'        = 'PV';
                                         'Siem Reap'        = 'SR';
                                         'Svay Rieng'       = 'SG'")
data.test$IDPoor = as.numeric(as.character(recode(data.test$IDPoor, 
                                                  "'Yes' = 1; 'No' = 0")))
data.test$Mnth = as.numeric(as.character(data.test$Mnth))
data.test$Satis = as.numeric(as.character(data.test$Satis))
data.test$Rec = as.numeric(as.character(recode(data.test$Rec, 
                                               "'Yes' = 1; 'No' = 0")))
data.test$SatisSup = as.numeric(as.character(data.test$SatisSup))
data.test$RecSup = as.numeric(as.character(recode(data.test$RecSup, 
                                                  "'Yes' = 1; 'No' = 0")))
varTable(data.test)
data.test = dummy_cols(data.test)
varTable(data.test)
model1 = "PercepSanSys =~ Satis + Rec
          PercepSanSys ~ Prov_SG + Prov_KL + Prov_PV + Prov_BM + Prov_KT + 
                         Prov_OM + Prov_SR + IDPoor + VillOD_Some +
                         VillOD_Most + VillOD_None + Mnth"
model2 = "PercepSanSys =~ Satis + Rec
          PercepSanSys ~ IDPoor + Mnth"
model3 = "PercepSanSys =~ Satis + Rec + SatisSup + RecSup"
fit = sem(model3, data = data.test, std.lv = TRUE)
summary(fit, standardized = TRUE)


# Plot jobs, baseline data
df = data.frame(Job = names(summary(data.baseline$Job)), 
                Count = summary(data.baseline$Job))
df$Percent = df$Count/(sum(df$Count))
ggplot(df, aes(x = "", y = Percent, fill = Job)) +
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start = 0)

# Compare LatInstlYr and DateStartUseLat
data.frame(data.baseline$LatInstlYr, data.baseline$DateStartUseLat)
data.baseline$DiffLatInstlStartUse = 
  as.numeric(data.baseline$DateStartUseLat) - 
  as.numeric(data.baseline$LatInstlYr)
summary(data.baseline$Diff)
summary(as.factor(data.baseline$Diff))
data.frame(LatInstlYr = 
             data.baseline$LatInstlYr[data.baseline$DiffLatInstlStartUse < 0],
           DateStartUseLat = 
             data.baseline$DateStartUseLat[data.baseline$DiffLatInstlStartUse < 0])


freqs.2way(data = data, metric1 = "Vill", metric2 = "Group2")

# Plot EmptyPlanMethds by Group2
levels(data.followup$EmptyPlanMethds) = c("Bucket", "DK", "FSMServProvdr", 
                                  "Bucket", "Pump", "FSMServProvdr")
data.followup.control = subset(data.followup, Group2 == "Cntrl" & !is.na(EmptyPlanMethds))
data.followup.treatNei = subset(data.followup, Group2 == "TreatNei" & !is.na(EmptyPlanMethds))
data.followup.treatPG = subset(data.followup, Group2 == "TreatPG" & !is.na(EmptyPlanMethds))
data_plot = 
  c(summary(data.followup.control$EmptyPlanMethds)/length(data.followup.control[,1]),
    summary(data.followup.treatNei$EmptyPlanMethds)/length(data.followup.treatNei[,1]),
    summary(data.followup.treatPG$EmptyPlanMethds)/length(data.followup.treatPG[,1]))
data_plot = 
  data.frame(x = rep(c("Control", "Neighbors of Treatment", "Treatment"), 
                     each = 4),
             y = data_plot, fill = names(data_plot))
data_plot = ddply(data_plot, .(x),
                     transform, pos = 1 - cumsum(y) + (0.5 * y))
# order = c("Bucket", "Pump", "FSMServProvdr", "DK")
# data_plot = data_plot[match(order, data_plot$fill),,drop = FALSE]
ggplot() + 
  geom_bar(aes(y = y, x = x, fill = fill), data = data_plot, 
           stat = "identity") +
  scale_x_discrete(name = "Group", 
                   breaks = c("Control", "Neighbors of Treatment", "Treatment"),
                   labels = c("Control", "Neighbors of Intervention", 
                              "Intervention")) +
  scale_y_continuous(name = "Percentage of rural latrine owners", 
                     breaks = round(seq(0.0, 1.0, 0.1), 2), 
                     limits = c(0.0, 1.0),
                     labels = paste(as.character(seq(0, 100, 10)), "%", 
                                    sep = "")) +
  scale_fill_discrete(name = "Emptying Methods Planned: ",
                      breaks = c("Bucket", "DK", "FSMServProvdr", "Pump"),
                      labels = c("Empty with a bucket myself",
                                 "Undecided",
                                 "Pay a professional",
                                 "Empty with a pump myself")) +
  theme_bw() +
  theme(text = element_text(family = "serif", size = 14), 
        legend.position = "bottom",
        legend.direction = "horizontal",
        panel.background = element_blank()) #+ 
  # geom_text(data = data_plot,
  #           aes(x = x, y = data_plot$y,
  #           label = paste0(round(data_plot$y*100, 0), "%")), size = 4)

###############################################################################
# ANALYZE DATA BY GROUP (CONTROL/TREATMENT)
###############################################################################
# Do EmptyPlan or EmptyPlanMethds vary between treatment and control groups?
data = data.followup
freqs.2way(data = data, metric1 = "EmptyPlan", metric2 = "Group2")
freqs.2way(data = data, metric1 = "EmptyPlanMethds", metric2 = "Group2")
results = genlinmod(data = data, 
                    formula = paste("EmptyPlan ~ Group2 + ",
                                    "IDPoor + ",
                                    "NumPplHH + NumPplHHLatUsr + HousFlod + ",
                                    "RoadAccsTrck + LatStartUseDate + ",
                                    "PiercdPit + NumPits + EmptyBefor"),
                    iter = 1, perc_train = 1, return = 1)
results = genlinmod(data = data, 
                    formula = paste("EmptyPlanMethds ~ Group2 + ",
                                    "IDPoor + ",
                                    "NumPplHH + NumPplHHLatUsr + HousFlod + ",
                                    "RoadAccsTrck + LatStartUseDate + ",
                                    "PiercdPit + NumPits + EmptyBefor"),
                    iter = 1, perc_train = 1, return = 1)

# GLM Formula with all relevant variables
# "EmptyPlan ~ VillTyp + PGInstld + ",
# "Intrviewr + Comm + Vill + IDPoor + ",
# "NumPplHH + NumPplHHLatUsr + HousFlod + ",
# "RoadAccsTrck + LatStartUseDate + ",
# "PiercdPit + NumPits + EmptyBefor"

# Compare TreatPG and TreatNei
data = subset(data, Group2 != "Cntrl")
data = droplevels(data)
freqs.2way(data = data, metric1 = "EmptyPlan", metric2 = "Group2")
freqs.2way(data = data, metric1 = "EmptyPlanMethds", metric2 = "Group2")

# Investigate other variables
data = data.followup
freqs.1way(data = data, metric1 = "PiercdPit")
freqs.1way(data = data, metric1 = "FSMServProvdrs")
freqs.1way(data = data, metric1 = "EmptyBefor")
freqs.1way(data = data, metric1 = "EmptyWho")

data = subset(data.followup, EmptyWilPay != "DK")
data = droplevels(data)
summary(data$EmptyWilPay)
length(data$EmptyWilPay[data$EmptyWilPay == 0])/length(data$EmptyWilPay)
summary(as.numeric(
  as.character(data$EmptyWilPay[data$EmptyWilPay != 0])))/4054.50

data = subset(data.followup, EmptyCost != "DK")
data$EmptyCost = as.numeric(as.character(data$EmptyCost))
freqs.1way(data = data, metric1 = "EmptyCost")
mean(data$EmptyCost)/4054.50
max(data$EmptyCost)/4054.50
summary(data$EmptyCost)
summary(as.factor(data$EmptyCost))
23/length(data$EmptyCost)
mean(data$EmptyCost[data$EmptyCost != 0])
median(data$EmptyCost[data$EmptyCost != 0])
hist(data$EmptyCost, breaks = c(0, 3000, 6000, 9000, 12000, 15000, 100000))

data = subset(data.followup, EmptyCost != "DK" & EmptyCost != 0)
summary(as.numeric(as.character(data$EmptyCost)))/4054.50

###############################################################################
# ANALYZE DATA BY INTERVIEWERS
###############################################################################
# Do EmptyPlan or EmptyMethods vary between interviewers? Yes.
data = data.followup
results = genlinmod(data = data, 
                    formula = paste("EmptyPlan ~ Group2 + ",
                                    "Intrviewr + IDPoor + ",
                                    "NumPplHH + NumPplHHLatUsr + HousFlod + ",
                                    "RoadAccsTrck + LatStartUseDate + ",
                                    "PiercdPit + NumPits + EmptyBefor"),
                    iter = 1, perc_train = 1, return = 1)
results = genlinmod(data = data, 
                    formula = paste("EmptyPlanMethds ~ Group2 + ",
                                    "Intrviewr + IDPoor + ",
                                    "NumPplHH + NumPplHHLatUsr + HousFlod + ",
                                    "RoadAccsTrck + LatStartUseDate + ",
                                    "PiercdPit + NumPits + EmptyBefor"),
                    iter = 1, perc_train = 1, return = 1)

# How do EmptyPlan and EmptyMethods vary between interviewers?
data = subset(data.followup, Intrviewr == "Meak Saran")
summary(data$EmptyPlan)
results = genlinmod(data = data, 
                    formula = paste("EmptyPlan ~ Group2 + ",
                                    "IDPoor + ",
                                    "NumPplHH + NumPplHHLatUsr + HousFlod + ",
                                    "RoadAccsTrck + LatStartUseDate + ",
                                    "PiercdPit + NumPits + EmptyBefor"),
                    iter = 1, perc_train = 1, return = 1)
results = genlinmod(data = data, 
                    formula = paste("EmptyPlanMethds ~ Group2 + ",
                                    "IDPoor + ",
                                    "NumPplHH + NumPplHHLatUsr + HousFlod + ",
                                    "RoadAccsTrck + LatStartUseDate + ",
                                    "PiercdPit + NumPits + EmptyBefor"),
                    iter = 1, perc_train = 1, return = 1)
data = subset(data.followup, Intrviewr == "Pen Senkosal")
summary(data$EmptyPlan)
results = genlinmod(data = data, 
                    formula = paste("EmptyPlan ~ Group2 + ",
                                    "IDPoor + ",
                                    "NumPplHH + NumPplHHLatUsr + HousFlod + ",
                                    "RoadAccsTrck + LatStartUseDate + ",
                                    "PiercdPit + NumPits + EmptyBefor"),
                    iter = 1, perc_train = 1, return = 1)
results = genlinmod(data = data, 
                    formula = paste("EmptyPlanMethds ~ Group2 + ",
                                    "IDPoor + ",
                                    "NumPplHH + NumPplHHLatUsr + HousFlod + ",
                                    "RoadAccsTrck + LatStartUseDate + ",
                                    "PiercdPit + NumPits + EmptyBefor"),
                    iter = 1, perc_train = 1, return = 1)

###############################################################################
# PLOT MAP OF TREATMENT AND CONTROL GROUPS
###############################################################################
coords = cbind(Longitude = as.numeric(as.character(data.followup$DistLong)), 
               Latitude = as.numeric(as.character(data.followup$DistLat)))
data.pts = SpatialPointsDataFrame(coords, 
                                  data[,(1:52)],
                                  proj4string = CRS("+init=epsg:4326"))
plot(data.pts, pch = ".", col = "darkred")
base.map = qmap("Cambodia", zoom = 7, maptype = "roadmap")
base.map + 
  geom_point(data = data, aes(x = DistLong, y = DistLat), 
             color = "red", size = 3, alpha = 0.5)
base.map + 
  stat_density2d(aes(x = DistLong, y = DistLat, 
                     fill = ..level.., alpha = ..level..*2),
                 size = 2, bins = 5, data = data, geom = "polygon") +
  scale_fill_gradient(low = "black", high = "red") + 
  geom_point(data = data, aes(x = DistLong, y = DistLat), 
             color = "red", size = 3, alpha = 0.5)

### OLD CODE

# ALL FACTORS that showed statistically significant and important associations 
# with the desirability of FSM intentions
# Removed Dist because it created too many errors
# IntndPitFullDes ~ Prov + Yr + Mnth + IDPoor + RDefBefor_BshFld + FreqNeiToi + 
# InfLatDump + ChlngsFlood + ChlngsNoWtr + Satis + SatisSup + Rec + RecSup + 
# Rain.mm
# LESS FACTORS WITH LOW P IN ABOVE MODEL: Rec, ChlngsNoWtr, Rain.mm, FreqNeiToi
print(results)
print(summary(results[[2]]))
print(anova(results[[2]], test = "Chisq"))
# FINAL MODEL
results = genlinmod(data = data.sub, 
                    formula = 
                      paste("IntndPitFullDes ~ Prov + Yr + Mnth + IDPoor +",
                            "RDefBefor_BshFld + ChlngsFlood + Satis +",
                            "SatisSup + RecSup"),
                    iter = 1, perc_train = 1, return = 1)
print(results)
print(summary(results[[2]]))
print(anova(results[[2]], test = "Chisq"))
print(confint(results[[2]]))
# FINAL MODEL WITH FACTORS REORDERED BY DECREASING DEVIANCE
results = genlinmod(data = data.sub, 
                    formula = paste("IntndPitFullDes ~ Prov + Satis + Mnth + ",
                                    "RecSup + Yr + SatisSup + ",
                                    "RDefBefor_BshFld + ChlngsFlood + IDPoor"),
                    iter = 1, perc_train = 1, return = 1)
print(results)
print(summary(results[[2]]))
print(anova(results[[2]], test = "Chisq"))
# pR2(results[[2]])
PseudoR2(results[[2]])
print(confint(results[[2]]))

model = glm(formula = paste("IntndPitFullDes ~ Prov + Satis + Mnth + ",
                            "RecSup + Yr + SatisSup + ",
                            "RDefBefor_BshFld + ChlngsFlood + IDPoor"),
            data = data.sub,
            family = binomial(link = "logit"),
            na.action = na.omit)
print(model)
print(summary(model))
print(anova(model, test = "Chisq"))
pR2(model)
PseudoR2(model, which = "all")
print(confint(model))

### OLD
# ALL VARIABLES POSSIBLE
# "IntndPitFullDes ~ Prov + IDPoor",
# "+ LivRP + VillOD + FreqNeiToi + AdltUseLat + ChldUseLat + Satis +",
# "Rec + SatisSup + RecSup + Mnth + Chlngs +",
# "RDefBefor_BshFld + RDefBefor_RivPnd + Rain.mm +",
# "InfLatDump"
# Factors with low p: RDefBefor_RivPnd, LivRP, ChldUseLat
# LESS LOW-P FACTORS
# "IntndPitFullDes ~ Prov + IDPoor",
# "+ VillOD + FreqNeiToi + AdltUseLat + Satis +",
# "Rec + SatisSup + RecSup + Mnth + Chlngs +",
# "RDefBefor_BshFld + Rain.mm +",
# "InfLatDump"
# Replaced Chlngs with significant dich Chlngs
# "IntndPitFullDes ~ Prov + IDPoor",
# "+ VillOD + FreqNeiToi + AdltUseLat + Satis +",
# "Rec + SatisSup + RecSup + Mnth +",
# "RDefBefor_BshFld + Rain.mm + ChlngsFlood + ChlngsNoWtr +",
# "InfLatDump"
### OLD 2
# Removed due to low info: AdltUseLat, InfLatDump

# By year
data.sub = subset(data, Yr != 2014)
data.sub = droplevels(data.sub)
freqs_2way_IntndPitFull_Yr = freqs.2way(data.sub, "IntndPitFull", "Yr", 1)
freqs_2way_IntndPitFullDK_Yr = freqs.2way(data.sub, "IntndPitFullDK", "Yr", 1)
freqs_2way_IntndPitFullEmpSlf_Yr = 
  freqs.2way(data.sub, "IntndPitFullEmpSlf", "Yr", 1)
freqs_2way_IntndPitFullPit_Yr = freqs.2way(data.sub, "IntndPitFullPit", "Yr", 1)
freqs_2way_IntndPitFullOthr_Yr = 
  freqs.2way(data.sub, "IntndPitFullOthr", "Yr", 1)
freqs_2way_IntndPitFullPay_Yr = freqs.2way(data.sub, "IntndPitFullPay", "Yr", 1)
freqs_2way_IntndPitFullStop_Yr = 
  freqs.2way(data.sub, "IntndPitFullStop", "Yr", 1)
crosstable_IntndPitFull_Yr = CrossTable(freqs_2way_IntndPitFull_Yr[[2]])
print(crosstable_IntndPitFull_Yr)
barplot(crosstable_IntndPitFull_Yr$prop.col, beside = T,
        col = c(1:6), family = "serif", axes = T,
        legend.text = c("Don't know", "Empty myself", "Install new pit",
                        "Other", "Pay someone", "Stop using latrine"), 
        args.legend = list(x = "topright", bty = "n"))
df = data.frame(IntndPitFull = crosstable_IntndPitFull_Yr$prop.col)
ggplot(df, aes(x = IntndPitFull.B, y = IntndPitFull.Freq, 
               group = IntndPitFull.A)) + 
  geom_line(aes(color = IntndPitFull.A))
# NOT SURE WHY SCRIPTS BELOW DON'T GIVE p and v
# data_sub = subset(data, IntndPitFull == "Pay", select = c("IntndPitFull", "Yr"))
# data_sub = droplevels(data_sub)
# summary(data_sub)
# assocstats(table(data_sub$IntndPitFull, data_sub$Yr))

# Analyze FSM intentions by month only in 2017
data.sub = subset(data, YrMnth == 201610 | YrMnth == 201611 |
                    YrMnth == 201612 | YrMnth == 201701 |
                    YrMnth == 201702 | YrMnth == 201703 |
                    YrMnth == 201704 | YrMnth == 201705 |
                    YrMnth == 201706 | YrMnth == 201707 |
                    YrMnth == 201708 | YrMnth == 201709, 
                  select = c(YrMnth, IntndPitFull, Mnth, Prov))
summary(data.sub, maxsum = 100)
length(data.sub$IntndPitFull)
freqs.2way.IntndPitFull.Mnth = freqs.2way(data = data.sub, 
                                          metric1 = "IntndPitFull",
                                          metric2 = "Mnth", return = 1)
# THE FOLLOWING COMMENTED CODE WEIGHTS THE COUNTS OF EACH FSM INTENTION PER 
# MONTH BY THE NUMBER OF SURVEYS IN THAT MONTH.  BUT, CROSSTABLE() IS ALREADY
# DOING THIS; THUS, THE FOLLOWING CODE IS COMMENTED OUT AND UNNECESSARY.
# summary(data.sub$YrMnth)
# summary(data.sub$Mnth)
# avg.n.survey.mnth = mean(summary(data.sub$Mnth))
# freqs.2way.IntndPitFull.Mnth[[2]]
# for (i in 1:length(summary(data.sub$YrMnth))) {
#   # print(i)
#   freqs.2way.IntndPitFull.Mnth[[2]][,i] = 
#     round(freqs.2way.IntndPitFull.Mnth[[2]][,i] * avg.n.survey.mnth / 
#             summary(data.sub$Mnth)[i], digits = 0)
#   print(sum(freqs.2way.IntndPitFull.Mnth[[2]][,i]))
# }
# freqs.2way.IntndPitFull.Mnth[[2]]
crosstable.IntndPitFull.Mnth = CrossTable(freqs.2way.IntndPitFull.Mnth[[2]])
print(crosstable.IntndPitFull.Mnth)
# BAD BARPLOT USING BUILT-IN PLOT FUNCTION
# barplot(crosstable.IntndPitFull.Mnth$prop.col, beside = F,
#         col = c(1:6), family = "serif", axes = T,
#         legend.text = c("Don't know", "Empty myself", "Install new pit",
#                         "Other", "Pay someone", "Stop using latrine"),
#         args.legend = list(x = "topright", bty = "n"))
prop.col = crosstable.IntndPitFull.Mnth$prop.col
typeof(prop.col)
order = c("Pay", "Pit", "EmpSlf", "DK", "Stop", "Othr")
prop.col = prop.col[match(order, row.names(prop.col)),,drop = FALSE]
df = data.frame(IntndPitFull = prop.col)
names(df) = c("IntndPitFull", "Mnth", "Prop")
df$Desir = rep(x = c("Des", "Des", "Undes", "Undes", "Undes", "Undes"), 
               times = 12)
ggplot(df, aes(x = Mnth, y = Prop, fill = IntndPitFull)) +
  geom_col() + 
  coord_flip()
windowsFonts()
title = "                       Desirable                                 " +
  "            Undesirable"
data.sub.des = subset(df, Desir == "Des")
data.sub.undes = subset(df, Desir == "Undes")
perc_des = round(data.sub.des$Prop[data.sub.des$IntndPitFull == "Pay"] +
                   data.sub.des$Prop[data.sub.des$IntndPitFull == "Pit"], 2)*100
perc_undes = 100 - perc_des
ggplot(df, aes(x = Mnth)) + 
  geom_col(data = data.sub.des, aes(y = -Prop, fill = IntndPitFull)) + 
  geom_col(data = data.sub.undes, aes(y = Prop, fill = IntndPitFull)) + 
  coord_flip() + 
  geom_hline(yintercept = 0, colour = "white", lwd = 1) + 
  scale_x_discrete(name = "Month", 
                   breaks = unique(df$Mnth),
                   limits = rev(unique(df$Mnth)),
                   labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                              "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  scale_y_continuous(name = "Percentage of rural latrine owners", 
                     breaks = round(seq(-0.8, 0.8, 0.1), 1), 
                     limits = c(-0.8, 0.8),
                     labels = paste(as.character(abs(seq(-80, 80, 10))), "%", 
                                    sep = "")) +
  scale_fill_discrete(name = "Intention when Pit Fills: ",
                      breaks = order,
                      labels = c("Pay professional",
                                 "Install a new pit",
                                 "Self-empty",
                                 "Undecided",
                                 "Stop using latrine",
                                 "Other")) +
  ggtitle(title) + 
  theme_bw() +
  theme(text = element_text(family = "serif", size = 14), 
        legend.position = "bottom",
        legend.direction = "horizontal",
        panel.background = element_blank()) +
  annotate(geom = "text", size = 4, family = "serif",
           x = unique(df$Mnth), y = -perc_des/100 - 0.04, 
           label = paste(as.character(perc_des), "%", sep = "")) + 
  annotate(geom = "text", size = 4, family = "serif",
           x = unique(df$Mnth), y = perc_undes/100 + 0.05, 
           label = paste(as.character(perc_undes), "%", sep = ""))
ggplot(df, aes(x = Mnth, y = Prop, group = IntndPitFull)) + 
  geom_line(aes(color = IntndPitFull), size = 2) +
  guides(linetype = guide_legend(override.aes = list(alpha = 1)))
mean(perc_des)
sd(perc_des)

# Plot FSM intentions by YrMnth
data.sub = subset(data, select = c(YrMnth, IntndPitFull))
freqs.2way.IntndPitFull.YrMnth = freqs.2way(data = data.sub, 
                                            metric1 = "IntndPitFull",
                                            metric2 = "YrMnth", return = 1)
crosstable.IntndPitFull.YrMnth = CrossTable(freqs.2way.IntndPitFull.YrMnth[[2]])
print(crosstable.IntndPitFull.YrMnth)
prop.col = crosstable.IntndPitFull.YrMnth$prop.col
df = data.frame(IntndPitFull = prop.col)
names(df) = c("IntndPitFull", "YrMnth", "Prop")
ggplot(df, aes(x = YrMnth, y = Prop, group = IntndPitFull)) + 
  geom_line(aes(color = IntndPitFull))
counts = crosstable.IntndPitFull.YrMnth$t
df = data.frame(IntndPitFull = counts)
names(df) = c("IntndPitFull", "YrMnth", "Count")
ggplot(df, aes(x = YrMnth, y = Count, group = IntndPitFull)) + 
  geom_line(aes(color = IntndPitFull), size = 1)

# By IDPoor status
freqs_2way_IntndPitFull_IDPoor = freqs.2way(data, "IntndPitFull", "IDPoor", 1)
freqs_2way_IntndPitFullDK_IDPoor = freqs.2way(data, "IntndPitFullDK", "IDPoor", 1)
freqs_2way_IntndPitFullEmpSlf_IDPoor = 
  freqs.2way(data, "IntndPitFullEmpSlf", "IDPoor", 1)
freqs_2way_IntndPitFullPit_IDPoor = 
  freqs.2way(data, "IntndPitFullPit", "IDPoor", 1)
freqs_2way_IntndPitFullOthr_IDPoor = 
  freqs.2way(data, "IntndPitFullOthr", "IDPoor", 1)
freqs_2way_IntndPitFullPay_IDPoor = 
  freqs.2way(data, "IntndPitFullPay", "IDPoor", 1)
freqs_2way_IntndPitFullStop_IDPoor = 
  freqs.2way(data, "IntndPitFullStop", "IDPoor", 1)
crosstable_IntndPitFull_IDPoor = CrossTable(freqs_2way_IntndPitFull_IDPoor[[2]])
print(crosstable_IntndPitFull_IDPoor)
barplot(crosstable_IntndPitFull_IDPoor$prop.col, beside = T,
        col = c(1:6), family = "serif", axes = T,
        legend.text = c("Don't know", "Empty myself", "Install new pit",
                        "Other", "Pay someone", "Stop using latrine"), 
        args.legend = list(x = "topright", bty = "n"))
df = data.frame(IntndPitFull = crosstable_IntndPitFull_IDPoor$prop.col)
ggplot(df, aes(x = IntndPitFull.B, y = 
                 IntndPitFull.Freq, group = IntndPitFull.A)) + 
  geom_line(aes(color = IntndPitFull.A))

# By gender
freqs_2way_IntndPitFull_CGend = freqs.2way(data, "IntndPitFull", "CGend", 1)
freqs_2way_IntndPitFullDK_CGend = freqs.2way(data, "IntndPitFullDK", "CGend", 1)
freqs_2way_IntndPitFullEmpSlf_CGend = 
  freqs.2way(data, "IntndPitFullEmpSlf", "CGend", 1)
freqs_2way_IntndPitFullPit_CGend = freqs.2way(data, "IntndPitFullPit", "CGend", 1)
freqs_2way_IntndPitFullOthr_CGend = 
  freqs.2way(data, "IntndPitFullOthr", "CGend", 1)
freqs_2way_IntndPitFullPay_CGend = freqs.2way(data, "IntndPitFullPay", "CGend", 1)
freqs_2way_IntndPitFullStop_CGend = 
  freqs.2way(data, "IntndPitFullStop", "CGend", 1)
crosstable_IntndPitFull_CGend = CrossTable(freqs_2way_IntndPitFull_CGend[[2]])
print(crosstable_IntndPitFull_CGend)
barplot(crosstable_IntndPitFull_CGend$prop.col, beside = T,
        col = c(1:6), family = "serif", axes = T,
        legend.text = c("Don't know", "Empty myself", "Install new pit",
                        "Other", "Pay someone", "Stop using latrine"), 
        args.legend = list(x = "topright", bty = "n"))
df = data.frame(IntndPitFull = crosstable_IntndPitFull_CGend$prop.col)
ggplot(df, aes(x = IntndPitFull.B, y = 
                 IntndPitFull.Freq, group = IntndPitFull.A)) + 
  geom_line(aes(color = IntndPitFull.A))

# By province
data.sub = subset(data, select = c(Prov, IntndPitFull))
data.sub = subset(data.sub, Prov != "Kampong Cham" &
                    Prov != "Kampong Speu" &
                    Prov != "Takeo")
data.sub = droplevels(data.sub)
summary(data.sub, maxsum = 100)
length(data.sub$IntndPitFull)
freqs.2way.IntndPitFull.Prov = freqs.2way(data.sub, "IntndPitFull", "Prov", 1)
crosstable.IntndPitFull.Prov = CrossTable(freqs.2way.IntndPitFull.Prov[[2]])
print(crosstable.IntndPitFull.Prov)
prop.col = crosstable.IntndPitFull.Prov$prop.col
typeof(prop.col)
order = c("Pay", "Pit", "EmpSlf", "DK", "Stop", "Othr")
prop.col = prop.col[match(order, row.names(prop.col)),,drop = FALSE]
order2 = c("Oddar Meanchey", "Banteay Meanchey", "Siem Reap", 
           "Kampong Thom", "Kandal", "Prey Veng", "Svay Rieng")
prop.col = prop.col[,match(order2, colnames(prop.col)),drop = FALSE]
df = data.frame(IntndPitFull = prop.col)
names(df) = c("IntndPitFull", "Prov", "Prop")
df$Desir = rep(x = c("Des", "Des", "Undes", "Undes", "Undes", "Undes"), 
               times = 7)
ggplot(df, aes(x = Prov, y = Prop, fill = IntndPitFull)) +
  geom_col() + 
  coord_flip()
title = "                     Desirable                                    " +
  "Undesirable"
data.sub.des = subset(df, Desir == "Des")
data.sub.undes = subset(df, Desir == "Undes")
perc_des = round(data.sub.des$Prop[data.sub.des$IntndPitFull == "Pay"] +
                   data.sub.des$Prop[data.sub.des$IntndPitFull == "Pit"], 
                 2)*100
perc_undes = 100 - perc_des
ggplot(df, aes(x = Prov)) + 
  geom_col(data = data.sub.des, aes(y = -Prop, fill = IntndPitFull)) + 
  geom_col(data = data.sub.undes, aes(y = Prop, fill = IntndPitFull)) + 
  coord_flip() + 
  geom_hline(yintercept = 0, colour = "white", lwd = 1) + 
  scale_x_discrete(name = "Province", 
                   breaks = unique(df$Prov),
                   limits = rev(unique(df$Prov))) +
  scale_y_continuous(name = "Percentage of rural latrine owners", 
                     breaks = round(seq(-0.8, 0.8, 0.1), 1), 
                     limits = c(-0.8, 0.8),
                     labels = paste(as.character(abs(seq(-80, 80, 10))), "%", 
                                    sep = "")) +
  scale_fill_discrete(name = "Intention when Pit Fills: ",
                      breaks = order,
                      labels = c("Pay professional",
                                 "Install a new pit",
                                 "Self-empty",
                                 "Undecided",
                                 "Stop using latrine",
                                 "Other")) +
  ggtitle(title) + 
  theme_bw() +
  theme(text = element_text(family = "serif", size = 14), 
        legend.position = "bottom",
        legend.direction = "horizontal",
        panel.background = element_blank()) +
  annotate(geom = "text", size = 4, family = "serif",
           x = unique(df$Prov), y = -perc_des/100 - 0.04, 
           label = paste(as.character(perc_des), "%", sep = "")) + 
  annotate(geom = "text", size = 4, family = "serif",
           x = unique(df$Prov), y = perc_undes/100 + 0.05, 
           label = paste(as.character(perc_undes), "%", sep = ""))
ggplot(df, aes(x = Prov, y = Prop, group = IntndPitFull)) + 
  geom_line(aes(color = IntndPitFull), size = 2) +
  guides(linetype = guide_legend(override.aes = list(alpha = 1)))
mean(perc_des)
sd(perc_des)
windowsFonts()
correspondence(data = data.sub, metric1 = "IntndPitFull", metric2 = "Prov")

# By rainfall
summary(as.factor(data$Rain.mm))
length(data$Rain.mm)
# missmap(data[45], main = "Missing values vs observed", legend = F)
biserial.cor(data$Rain.mm, data$IntndPitFullDes, use = "complete.obs")
ggplot(data = data, aes(x = as.numeric(Mnth), y = Rain.mm, colour = Prov)) +
  geom_line(aes(colour = Prov), size = 1.5) +
  theme_minimal() +
  labs(x = "Month", y = "Rainfall (mm)", colour = "Provinces") +
  scale_x_continuous(breaks = c(1:12)) +
  coord_cartesian(xlim = c(1, 12), ylim = c(0, max(data$Rain.mm) + 30))
data.sub = subset(data, select = c(Rain.mm, IntndPitFullDes))
model = glm(formula = IntndPitFullDes ~ Rain.mm,
            data = data.sub,
            family = binomial(link = "logit"),
            na.action = na.omit)
plot(data.sub$IntndPitFullDes, data.sub$Rain.mm)
biserial.cor(data.sub$Rain.mm, data.sub$IntndPitFullDes, use = "complete.obs")
anova(model, test = "Chisq")
summary(data.sub)
plot(data$IntndPitFullDes, data$Rain.mm)
df = data.frame(Prov = factor(), PtBisCor = double(), P = double())
levels(df$Prov) = unique(data$Prov)
# for (i in 1:1) {
for (i in 1:length(unique(data$Prov))) {
  data.sub = subset(data, Prov == unique(data$Prov)[i],
                    select = c(Rain.mm, IntndPitFullDes))
  model = glm(formula = IntndPitFullDes ~ Rain.mm,
              data = data.sub,
              family = binomial(link = "logit"),
              na.action = na.omit)
  plot(data.sub$IntndPitFullDes, data.sub$Rain.mm, main = unique(data$Prov)[i])
  prov = unique(data$Prov)[i]
  ptbiscor = biserial.cor(data.sub$Rain.mm, data.sub$IntndPitFullDes, 
                          use = "complete.obs")
  p = anova(model, test = "Chisq")$`Pr(>Chi)`[2]
  df = rbind(df, data.frame(Prov = prov, PtBisCor = ptbiscor, P = p))
}
df
summary(data$Prov)
mean(df$P[!is.na(df$P)])

# With latrine coverage
provs = c("OddM", "BanM", "SieR", "KamT", "KanL", "PreV", "SvaG")
perc_des_by_prov = c(67, 76, 33, 54, 71, 48, 56)
perc_latcov_by_prov_2016 = c(35, 57, 48, 46, 69, 53, 65)
data.sub = data.frame(Des = perc_des_by_prov, LatCov = perc_latcov_by_prov_2016)
p.mat = cor.mtest(data.sub)
print(p.mat)
cor(data.sub)
corrplot(cor(data.sub), method = "pie", type = "lower", order = "original",
         diag = FALSE, p.mat = p.mat, sig.level = 0.05, 
         addCoef.col = "black", tl.col = "black", tl.srt = 45,
         family = "serif", number.font = 1)

###############################################################################
# ANALYZE DATA AGGREGATED BY PROVINCE
###############################################################################
provinces = unique(data$Prov)
for (prov in provinces) {
  print(prov)
  
  # Subset data for this province only
  sub = subset(data, Prov == paste(prov))
  # print(summary(sub$Prov))
  # print(summary(sub))
  sub = droplevels(sub)
  
  # Characterize data for this province, focusing on NAs
  print(summary(sub))
  # print(sapply(sub, function(x) sum(is.na(x))))
  # missmap(sub, main = "Missing Values in Variables", legend = F)
  
  # Remove rows missing IntndPitFull response
  sub = subset(sub, !is.na(IntndPitFull))
  # print(sapply(sub, function(x) sum(is.na(x))))
  # missmap(sub[1:35], main = "Missing Values in Variables", legend = F)
  # print(summary(sub))
  
  # Run 1-way frequency analysis on selected variable
  # names(sub)
  # var1 = c(2:length(sub))
  # for (num in var1) {
  #   print(num)
  #   freqs.1way(data = sub, metric1 = num, prov = prov)
  # }
  
  # Run 2-way frequency and correspondence analyses on selected variable pairs
  # names(sub)
  # var1 = c(17, 38:44)
  # var2 = c(2, 5, 6, 23, 24)
  # pairs = expand.grid(var1, var2)
  # for (num in 1:length(pairs[,1])) {
  #   print(paste(pairs[num,1], "_", pairs[num,2]))
  #   freqs.2way(data = sub, metric1 = pairs[num,1], metric2 = pairs[num,2], 
  #              prov = prov)
  #   # correspondence(data = sub, metric1 = pairs[num,1], metric2 = 
  # pairs[num,2], 
  #   #    prov = prov)     # NOT WORKING
  # }
  
  # Run multiple correspondence analysis
  # multiple.correspondence(data = sub[2:length(sub)],
  #                         quali_sup = c(3:4, 7, 10, 12, 13, 18),
  #                         quanti_sup = c())   # NOT WORKING
  
  # Run generalized linear model
  genlinmod(data = sub, iter = 1)
  
}

##########################################################################
# CLEAN UP
##########################################################################
sink()
dev.off()
closeAllConnections()
file.remove(paste(getwd(),"/Output/dump.txt", sep = ""))
