# Pit Gauge Study, May 2017 to June 2018
# Clean Survey Responses of Rural Cambodian Latrine Owners
# Written by James Harper, PE, ENV SP of the University of Colorado Boulder
# Started October 1, 2017; Last updated Dec 18, 2018
###############################################################################
# Initialize environment and user input
rm(list = ls())                                      # Clear global environment
cat("\014")                                              # Clear console window
source("functions.R")                                   # Load custom functions
load_libraries(                                      # Install & load libraries
  c("rio", "gplots", "Amelia", "car", "fastDummies", "FactoMineR", "lavaan",
    "extrafont", "stringr"))
loadfonts(device = "win")
options(max.print = 1000000)
###############################################################################
# Load raw data
file.to.import1 = 
  paste(
    getwd(),
    "/data/surveys/pit_gauge/baseline_data_analysis.xlsx",
    sep = "")
file.to.import2 = 
  paste(
    getwd(),
    "/data/surveys/pit_gauge/followup_data_analysis.xlsx",
    sep = "")
file.to.import3 = 
  paste(
    getwd(),
    "/data/surveys/pit_gauge/sludge_levels.xlsx",
    sep = "")
file.to.import4 = 
  paste(
    getwd(),
    "/data/surveys/pit_gauge/adp_sales.xlsx",
    sep = "")
d.baseline = import(file.to.import1, which = "Data")
d.followup = import(file.to.import2, which = "Data")
d.sldglvls = import(file.to.import3, which = "Data")
d.adpsales = import(file.to.import4, which = "Data")

###############################################################################
# CLEAN d.baseline
###############################################################################
# Shorten variable (column) names
names(d.baseline)
old.col.names1 = names(d.baseline)
names(d.baseline) = 
  c("PGidOld", "PGid", "HHid", "Intrviewr", "PGInstlDate", "RespName", 
    "Gend", "Phone", "Vill", "Comm", "Dist", "Prov", "Lat", "Lon", 
    "VillTyp", "RelToLatOwnr", "HavIDPoorCard", "LatOwnrIDPoor", 
    "LatOwnrJob1", "LatOwnrJob2", "NumAdlts18", "NumChld217", "NumInf2", 
    "NumPplHH", "LatInstlYr", "NumRings", "LatStartUseYr", "UseLatReg", 
    "EmptyBefor","WhyNoEmpty", "EmptyMethds", "PiercdPit", "SurveyYr",
    "LatAge")
print(data.frame(names(d.baseline), old.col.names1))

# Remove unused variables
names(d.baseline)
print(d.baseline$Lat)
d.baseline = subset(d.baseline, select = -c(PGidOld, SurveyYr, 
                                            NumRings))

# Remove bad rows due to problem with pit gauge or similar
d.baseline = subset(d.baseline, PGid != "9V5")

# Convert data formats
summary(d.baseline, maxsum = 10)
d.baseline$Intrviewr = as.factor(d.baseline$Intrviewr)
d.baseline$Lat = as.numeric(as.character(d.baseline$Lat))
d.baseline$Lon = as.numeric(as.character(d.baseline$Lon))
d.baseline$LatStartUseYr = 
  as.numeric(as.character(d.baseline$LatStartUseYr))
for (i in 1:length(names(d.baseline))) {   # All characters into factors
  if (is.character(d.baseline[i][[1]])) {
    d.baseline[i][[1]] = as.factor(d.baseline[i][[1]])
  }
}

# Rename PGid
summary(d.baseline$PGid)
d.baseline$PGid[d.baseline$PGid == 0] = NA

# Rename responses in RelToLatOwnr
summary(d.baseline$RelToLatOwnr)
levels(d.baseline$RelToLatOwnr) = 
  c("Child", "Child", "LatOwnr", "Sibling", "Child", "Child", "Child",
    "Parent", "Parent", "Parent", "Child", "Child", "LatOwnr", 
    "LatOwnr", "Child", "Parent", "Parent", "LatOwnr", "Sibling",
    "Child", "Child", "Spouse")

# Create new variable Job based on LatOwnrJob1 and LatOwnrJob2
summary(d.baseline$LatOwnrJob1)
summary(d.baseline$LatOwnrJob2)
# print(data.frame(d.baseline$LatOwnrJob1, d.baseline$LatOwnrJob2))
# plot(summary(d.baseline$LatOwnrJob2))
d.baseline$Job = as.character(NA)
for (row in 1:length(d.baseline$LatOwnrJob2)) {
  if (is.na(d.baseline[row,]$LatOwnrJob2)) {
    d.baseline[row,]$Job = NA
  } else if (d.baseline[row,]$LatOwnrJob2 == "Blacksmith" |
             d.baseline[row,]$LatOwnrJob2 == "Cameraman" |
             d.baseline[row,]$LatOwnrJob2 == "Car repair" |
             d.baseline[row,]$LatOwnrJob2 == "Carpenter" |
             d.baseline[row,]$LatOwnrJob2 == "Engineer" |
             d.baseline[row,]$LatOwnrJob2 == "Mason" |
             d.baseline[row,]$LatOwnrJob2 == "Nurse" ) {
    d.baseline[row,]$Job = "Technical"
  } else if (d.baseline[row,]$LatOwnrJob2 == "Commune Chief" |
             d.baseline[row,]$LatOwnrJob2 == "Deputy village chief" |
             d.baseline[row,]$LatOwnrJob2 == "Village chief" |
             d.baseline[row,]$LatOwnrJob2 == "Village Chief" |
             d.baseline[row,]$LatOwnrJob2 == 
             "Village Chief\r\n Assistant" ) {
    d.baseline[row,]$Job = "Leadership"
  } else if (d.baseline[row,]$LatOwnrJob2 == "Clergyman" |
             d.baseline[row,]$LatOwnrJob2 == "Factory worker" |
             d.baseline[row,]$LatOwnrJob2 == "Grocery seller" |
             d.baseline[row,]$LatOwnrJob2 == "Hotel staff" | 
             d.baseline[row,]$LatOwnrJob2 == "Housekeeper" | 
             d.baseline[row,]$LatOwnrJob2 == "Labor woker" | 
             d.baseline[row,]$LatOwnrJob2 == "Moto taxi" | 
             d.baseline[row,]$LatOwnrJob2 == "Motor taxi" | 
             d.baseline[row,]$LatOwnrJob2 == "Petrol seller" | 
             d.baseline[row,]$LatOwnrJob2 == "Scrap dealer" |
             d.baseline[row,]$LatOwnrJob2 == "Seller" |
             d.baseline[row,]$LatOwnrJob2 == "Small shop" |
             d.baseline[row,]$LatOwnrJob2 == "Taxi driver" |
             d.baseline[row,]$LatOwnrJob2 == "Trade" |
             d.baseline[row,]$LatOwnrJob2 == 
             "Traditional medicine seller" ) {
    d.baseline[row,]$Job = "Unskilled"
  } else if (d.baseline[row,]$LatOwnrJob2 == "NGO staff" |
             d.baseline[row,]$LatOwnrJob2 == "Police" |
             d.baseline[row,]$LatOwnrJob2 == "Teacher" ) {
    d.baseline[row,]$Job = "OtherSkilled"
  } else if (d.baseline[row,]$LatOwnrJob2 == "Student" ) {
    d.baseline[row,]$Job = "Student"
  } else if (d.baseline[row,]$LatOwnrJob2 == "Farmer" ) {
    d.baseline[row,]$Job = "Agriculture"
  }
}
print(data.frame(d.baseline$Job, d.baseline$LatOwnrJob1, 
                 d.baseline$LatOwnrJob2))
d.baseline$Job = as.factor(d.baseline$Job)
summary(d.baseline$Job)
print(subset(data.frame(A = d.baseline$Job, 
                        B = d.baseline$LatOwnrJob1, 
                        C = d.baseline$LatOwnrJob2), 
             is.na(A)))
# plot(summary(d.baseline$Job))

# Rename responses in NumInf2 and change type
summary(d.baseline$NumInf2, maxsum = 30)
levels(d.baseline$NumInf2) = c(0, 1, 2, 3, 0)
d.baseline$NumInf2 = as.numeric(as.character(d.baseline$NumInf2))

# Rename responses in LatInstlYr and change type
summary(d.baseline$LatInstlYr, maxsum = 30)
d.baseline$LatInstlYr[d.baseline$LatInstlYr == "Not Remember"] = 
  NA
d.baseline$LatInstlYr = droplevels(d.baseline$LatInstlYr)
d.baseline$LatInstlYr = 
  as.numeric(as.character(d.baseline$LatInstlYr))

# Rename UseLatReg
summary(d.baseline$UseLatReg)
levels(d.baseline$UseLatReg) = c("Never", "Some", "Always")

# Rename EmptyBefor
summary(d.baseline$EmptyBefor)
levels(d.baseline$EmptyBefor) = c("No", "Yes", "Yes")

# Rename WhyNoEmpty
summary(d.baseline$WhyNoEmpty)
levels(d.baseline$WhyNoEmpty) = c("Busy", "Busy", "NotClogged", 
                                  "NotUsing", "NoProAvailable",
                                  "NoProAvailable", "NotFull",
                                  "NotFull", "NotFull", 
                                  "PiercedPit", "NotUsing",
                                  "Abandoned")

# Rename PiercdPit
summary(d.baseline$PiercdPit)
levels(d.baseline$PiercdPit) = c("No", NA, "Yes")

# Rename EmptyMethds
summary(d.baseline$EmptyMethds)
levels(d.baseline$EmptyMethds) = c("Self-Bucketing", "Self-Pump", 
                                   "PaidPro", "Self-Pump", 
                                   "Self-Pump", "Self-Pump",
                                   "Self-Bucketing", "Self-Bucketing", 
                                   "Unknown", "Self-Pump")

###############################################################################
# CLEAN d.followup
###############################################################################
# Shorten variable (column) names
names(d.followup)
old.col.names2 = names(d.followup)
names(d.followup) = 
  c("HHid2", "Intrviewr", "Date", "SuprvsrName", "Prov", "Dist", "Comm", 
    "Vill", "VillTyp2", "Avail", "HHheadName", "Phone", "RespLName", 
    "RespFName", "RGend", "Phone2", "HavLat", "RPurLat", "LatPurLName",
    "LatPurFName", "CGend", "RelToLatOwnr", "RelToLatOwnr_Othr", "IDPoor",
    "IDPoorTyp", "NumM61HH", "NumM1860HH", "NumM518HH", "NumM05HH", 
    "NumF61HH", "NumF1860HH", "NumF518HH", "NumF05HH", "NumPplHH", 
    "NumM61LatUsr", "NumM1860LatUsr", "NumM518LatUsr", "NumM05LatUsr", 
    "NumF61LatUsr", "NumF1860LatUsr", "NumF518LatUsr", "NumF05LatUsr",
    "NumPplHHLatUsr", "LivRP", "LivField", "HousFlod", "FlodSevrty", 
    "FlodSevrty.Othr", "RoadAccsTruck", "LatInstlDate2", "LatInstlDate",
    "LatStartUseDate2", "LatStartUseDate", 
    "PiercdPit", "NumPits", "NumRngs", "NumRngs.Othr", "NumRngs.Othr2",
    "PGInstld", "PGid", "EmptyBefor", "EmptyChlngs", "EmptyChlngs.Othr",
    "EmptyNum", "EmptyLast2", "EmptyLast", "EmptyMethds",
    "EmptyMethds.Othr", "EmptyWho",
    "EmptyWho.Othr", "EmptyCost", "EmptyExprience", "EmptyTimeHrs", 
    "EmptyTimeHrs.Othr", "EmptyWhy", "EmptyWhy.Othr", "EmptyDispos", 
    "EmptyDispos.Othr", "EmptyWhyNot", "EmptyWhyNot.Othr", "EmptyPlan",
    "EmptyPlanMethds", "EmptyPlanMethds.Othr", "EmptyWilPay",
    "FSMServProvdrs", "FSMServProvdrs.Contact", "FSMServProvdrs.Cost", 
    "CommntQues")
print(data.frame(names(d.followup), old.col.names2))

# Convert data formats
summary(d.followup, maxsum = 30)
d.followup$Date = as.Date(d.followup$Date)
d.followup$NumF61HH = as.numeric(d.followup$NumF61HH)
# Rename and change format of LatInstlDate
summary(as.factor(d.followup$LatInstlDate))
d.followup$LatInstlDate[d.followup$LatInstlDate == "Don't know"] = NA
d.followup$LatInstlDate[d.followup$LatInstlDate == 0] = NA
d.followup$LatInstlDate = 
  as.Date(as.character(d.followup$LatInstlDate), "%m-%d-%Y")
summary(d.followup$LatInstlDate)
# Rename and change format of LatStartUseDate
summary(as.factor(d.followup$LatStartUseDate))
d.followup$LatStartUseDate[d.followup$LatStartUseDate == "Don't know"] = NA
d.followup$LatStartUseDate[d.followup$LatStartUseDate == 0] = NA
d.followup$LatStartUseDate = as.Date(d.followup$LatStartUseDate,
                                     "%m-%d-%Y")
summary(d.followup$LatStartUseDate)
# Rename and change format of EmptyLast
summary(as.factor(d.followup$EmptyLast))
d.followup$EmptyLast[d.followup$EmptyLast == "Don't know"] = NA
d.followup$EmptyLast[d.followup$EmptyLast == "Don't Know"] = NA
d.followup$EmptyLast[d.followup$EmptyLast == 0] = NA
d.followup$EmptyLast = as.Date(d.followup$EmptyLast,
                               "%m-%d-%Y")
summary(d.followup$EmptyLast)
for (i in 1:length(names(d.followup))) {   # All characters into factors
  if (is.character(d.followup[i][[1]])) {
    d.followup[i][[1]] = as.factor(d.followup[i][[1]])
  }
}

# Remove unused variables
# d.followup = subset(d.followup, select = -c())

# Rename Avail
summary(d.followup$Avail)
levels(d.followup$Avail) = c("No", "Yes", "Yes")

# Rename RGend
summary(d.followup$RGend)
levels(d.followup$RGend) = c("F", "M")

# Rename CGend
summary(d.followup$CGend)
levels(d.followup$CGend) = c("F", "M")

# Rename RelToLatOwnr
summary(d.followup$RelToLatOwnr)
levels(d.followup$RelToLatOwnr) = 
  c("Sibling", "Spouse", "Parent", "Other", "Self")
for (row in 1:length(d.followup$RelToLatOwnr)) {
  if (is.na(d.followup$RelToLatOwnr[row])) {
    if (!is.na(d.followup$RPurLat[row]) & 
        d.followup$RPurLat[row] == "Yes") {
      d.followup$RelToLatOwnr[row] = "Self"
    }
  }
}

# Update IDPoorTyp with IDPoor
summary(d.followup$IDPoorTyp)
summary(d.followup$IDPoor)
levels(d.followup$IDPoorTyp) = c("IDPoor1", "IDPoor2", "NotIDPoor")
for (row in 1:length(d.followup$IDPoorTyp)) {
  if (!is.na(d.followup$IDPoor[row])) {
    if (d.followup$IDPoor[row] == "No") {
      d.followup$IDPoorTyp[row] = "NotIDPoor"
    }
  }
}

# Change households with 0 members to NA in NumPplHH and NumPplHHLatUsr
summary(d.followup$NumPplHH)
summary(d.followup$NumPplHHLatUsr)
summary(as.factor(d.followup$NumPplHH))
summary(as.factor(d.followup$NumPplHHLatUsr))
for (row in 1:length(d.followup$NumPplHH)) {
  if (!is.na(d.followup$NumPplHH[row])) {
    if (d.followup$NumPplHH[row] == 0) {
      d.followup$NumPplHH[row] = NA
    }
  }
  if (!is.na(d.followup$NumPplHHLatUsr[row])) {
    if (d.followup$NumPplHHLatUsr[row] == 0) {
      d.followup$NumPplHHLatUsr[row] = NA
    }
  }
}

# Rename LivRP
summary(d.followup$LivRP)
levels(d.followup$LivRP) = c("No", "No")

# Rename HousFlod
summary(d.followup$HousFlod)
levels(d.followup$HousFlod) = c("No", "No", "Yes")

# Rename FlodSevrty
summary(d.followup$FlodSevrty)
levels(d.followup$FlodSevrty) = c("Mild", "Moderate", "Other", "Severe")
d.followup$FlodSevrty[d.followup$FlodSevrty == "Other"] = NA
d.followup$FlodSevrty = droplevels(d.followup$FlodSevrty)

# Create new variable VillTyp based on VillTyp2 and PGInstld
summary(d.followup$VillTyp2)
summary(d.followup$PGInstld)
print(data.frame(d.followup$VillTyp2, d.followup$PGInstld))
d.followup$VillTyp = as.character(NA)
for (row in 1:length(d.followup$VillTyp2)) {
  if (is.na(d.followup[row,]$VillTyp2) | 
      is.na(d.followup[row,]$PGInstld)) {
    d.followup[row,]$VillTyp = NA
  } else if (d.followup[row,]$VillTyp2 == "Control") {
    d.followup[row,]$VillTyp = "Cntrl"
  } else if (d.followup[row,]$VillTyp2 == "Treatment" & 
             d.followup[row,]$PGInstld == "Yes") {
    d.followup[row,]$VillTyp = "TreatPG"
  } else if (d.followup[row,]$VillTyp2 == "Treatment" & 
             d.followup[row,]$PGInstld == "No") {
    d.followup[row,]$VillTyp = "TreatNei"
  }
}
print(data.frame(d.followup$VillTyp2, d.followup$PGInstld, 
                 d.followup$VillTyp))
d.followup$VillTyp = as.factor(d.followup$VillTyp)
summary(d.followup$VillTyp)

# Remove rows without full group description
# d.followup = subset(d.followup, !is.na(VillTyp))

# Rename responses in EmptyChlngs
summary(d.followup$EmptyChlngs)
levels(d.followup$EmptyChlngs) = c("None", "EmptyReqTooFreq")

# Rename responses in EmptyMethds
summary(d.followup$EmptyMethds)
levels(d.followup$EmptyMethds) = c("Self-Bucketing", "Self-Pump")

# Rename responses in EmptyWho
summary(d.followup$EmptyWho)
levels(d.followup$EmptyWho) = c("FamilyMemb", "Pro", "Self")

# Rename responses in EmptyCost
summary(d.followup$EmptyCost)
levels(d.followup$EmptyCost) = 
  c(0, 0, 10000, 12000, 120000, 1500, 20000, 3500, 3700, 4000, 50000, 7000, 
    "DK", "DK", "DK")

# Rename responses in EmptyExprience
summary(d.followup$EmptyExprience)
levels(d.followup$EmptyExprience) = c("Good", "Neutral", "Bad")

# Rename responses in EmptyTimeHrs
summary(d.followup$EmptyTimeHrs)
levels(d.followup$EmptyTimeHrs) = 
  c(1, 1.5, 10/60, 12, 15/60, 2, 3, 0.5, 4, 4/60, 5, 5/60, 6, 8)
d.followup$EmptyTimeHrs = as.numeric(as.character(d.followup$EmptyTimeHrs))

# Rename responses in EmptyWhy and EmptyWhy.Othr, and combine into EmptyWhy
summary(d.followup$EmptyWhy)
levels(d.followup$EmptyWhy) = c("Smell", "Smell", "Unusable", "Other", 
                                "FertlizCrops")
summary(d.followup$EmptyWhy.Othr)
levels(d.followup$EmptyWhy.Othr) = 
  c("FertlizCrops", "FertlizCrops", "FertlizCrops", "ScheduledTimeArrived",
    "Don'tKnow", "PitOverflowd")
levels(d.followup$EmptyWhy) = c(levels(d.followup$EmptyWhy), 
                                levels(d.followup$EmptyWhy.Othr))
data.frame(d.followup$EmptyWhy, d.followup$EmptyWhy.Othr)
for (row in 1:length(d.followup$EmptyWhy)) {
  if (!is.na(d.followup$EmptyWhy[row])) {
    if (d.followup$EmptyWhy[row] == "Other") {
      d.followup$EmptyWhy[row] = d.followup$EmptyWhy.Othr[row]
    }
  }
}
d.followup$EmptyWhy = droplevels(d.followup$EmptyWhy)

# Rename EmptyDispos
summary(d.followup$EmptyDispos)
levels(d.followup$EmptyDispos) = 
  c("Buried", "FertlizCrops", "InField", "InPondRiv")

# Rename EmptyWhyNot and combine with EmptyWhyNot.Othr
summary(d.followup$EmptyWhyNot)
summary(d.followup$EmptyWhyNot.Othr)
levels(d.followup$EmptyWhyNot.Othr) = c("TooBusy", "WrongEquipment")
levels(d.followup$EmptyWhyNot) = 
  c("NotFull", "Other", "PreferOvrflw", levels(d.followup$EmptyWhyNot.Othr))
for (row in 1:length(d.followup$EmptyWhyNot)) {
  if (!is.na(d.followup$EmptyWhyNot[row])) {
    if (d.followup$EmptyWhyNot[row] == "Other") {
      d.followup$EmptyWhyNot[row] = d.followup$EmptyWhyNot.Othr[row]
    }
  }
}
d.followup$EmptyWhyNot = droplevels(d.followup$EmptyWhyNot)

# Rename EmptyPlanMethds and combine with EmptyPlanMethds.Othr
summary(d.followup$EmptyPlanMethds)
summary(d.followup$EmptyPlanMethds.Othr)
levels(d.followup$EmptyPlanMethds.Othr) = c("PayPro", "MixWithAsh")
levels(d.followup$EmptyPlanMethds) = 
  c("Pump", "DK", "Bucketing", "Other", "VTruck", 
    levels(d.followup$EmptyPlanMethds.Othr))
for (row in 1:length(d.followup$EmptyPlanMethds)) {
  if (!is.na(d.followup$EmptyPlanMethds[row])) {
    if (d.followup$EmptyPlanMethds[row] == "Other") {
      d.followup$EmptyPlanMethds[row] = d.followup$EmptyPlanMethds.Othr[row]
    }
  }
}
d.followup$EmptyPlanMethds = droplevels(d.followup$EmptyPlanMethds)

# Rename responses in EmptyWilPay to describe # Cambodian Riel to empty a
# 3-ring pit
summary(d.followup$EmptyWilPay)
levels(d.followup$EmptyWilPay) = 
  c(10000, 100000, 14000, 1500, 15000, 18000, 20000, 200000, 30000, 3500, 
    3700, 40000, 50000, 6000, 60000, 7000, 9500, 0, "DK", "DK")

# Rename FSMServProvdrs
summary(d.followup$FSMServProvdrs)
levels(d.followup$FSMServProvdrs) = c("DK", "None", "None", "Yes")

# Rename FSMServProvdrs.Contact
summary(d.followup$FSMServProvdrs.Contact)
levels(d.followup$FSMServProvdrs.Contact) = 
  c("KnowPhone1", "KnowPhone2", "KnowPhone3", "KnowPhone4", "DKPhone")

# Rename FSMServProvdrs.Cost
summary(d.followup$FSMServProvdrs.Cost)
levels(d.followup$FSMServProvdrs.Cost) = 
  c("$10/ring", "$150/1thru5rings", "$40/3rings", "$25/3rings", "$50",
    "50000Riel", "DK")
subset(data.frame(A = d.followup$FSMServProvdrs,
                  B = d.followup$FSMServProvdrs.Contact,
                  C = d.followup$FSMServProvdrs.Cost),
       A == "Yes")

# Create RespQuesTopic based on CommntQues
summary(d.followup$CommntQues)
d.followup$RespQuesTopic = d.followup$CommntQues
levels(d.followup$RespQuesTopic) =
  c("RequestEquipToSelfEmpty", "RequestIDEimproveTheirOldLatrine",
    "RequestIDEprovideFreeLatrine", "RequestIDEprovideFreeLatrine",
    "RequestIDEprovideFreeLatrine", "NeedMoreRings", "NeedVTruck",
    "WantAddPit", "NeedPro", "AskedIfEnvAffctdBySludg",
    "No", "WantAddRings", "WantADP", "WantAddPit", "WantAddRings",
    "WantAddRings", "AskIDEnameAndPurpose", "AskIntrvwPurpose",
    "AskIntrvwPurpose", "AskIDEname", "AskHowToSubmergSludg",
    "RequestIDEprovideEmptyOrMoneytoEmpty", "AskHowSludgAffctsCommHealth",
    "RequestIDEprovideFreeLatrine", "RequestIDEprovideFreeLatrine",
    "RequestIDEprovideFreeLatrine", "RequestIDEprovideEmptyOrMoneytoEmpty",
    "RequestIDEprovidePrivacyStructure")
data.frame(d.followup$CommntQues, d.followup$RespQuesTopic) 
summary(d.followup$RespQuesTopic)

# Rename NumRngs
summary(d.followup$NumRngs)
levels(d.followup$NumRngs) = c("100cm", "100cm", "Other", "80cm")

###############################################################################
# CLEAN d.sldglvls
###############################################################################
# Shorten variable (column) names
names(d.sldglvls)
old.col.names3 = names(d.sldglvls)
names(d.sldglvls) = 
  c("ID", "PGid", "Intrviewr", "PGInstlDate", "CreatDate", "MeasurDate",
    "CreatName", "PGclientName", "Vill", "Comm", "Dist", "Prov",
    "SludgDepth_mm", "LiquidDepth_mm", "PGProblem")
print(data.frame(names(d.sldglvls), old.col.names3))

# Convert data formats
summary(d.sldglvls, maxsum = 30)
d.sldglvls$PGInstlDate = as.Date(d.sldglvls$PGInstlDate)
d.sldglvls$CreatDate = as.Date(d.sldglvls$CreatDate)
d.sldglvls$MeasurDate = as.Date(d.sldglvls$MeasurDate)
for (i in 1:length(names(d.sldglvls))) {   # All characters into factors
  if (is.character(d.sldglvls[i][[1]])) {
    d.sldglvls[i][[1]] = as.factor(d.sldglvls[i][[1]])
  }
}

###############################################################################
# Create d.ADPsales based on correspondence from iDE Cambodia
###############################################################################
d.ADPsales.villtyp = data.frame(VillTyp = c("Control", "Treatment"),
                                NumSalesVisits = c(934, 806),
                                NumSales = c(136, 101),
                                NumDeliverd = c(107, 52),
                                NumCanceld = c(25, 40),
                                NumBacklog = c(4, 9))
d.ADPsales.PG = data.frame(HasPG = c("Yes", "No"),
                           NumSalesVisits = c(225, 1515),
                           NumSales = c(60, 177),
                           NumDeliverd = c(52, 107),
                           NumCanceld = c(5, 60),
                           NumBacklog = c(3, 10))

NumADPSoldByWave = c(106, 89, 42)

###############################################################################
# Create d.customers based on iDE Cambodia report
###############################################################################
d.customers = data.frame(Poss = c(163+166+170+108+147+178+176+169+129+131,
                                NA, NA),
                       Sales = c(106, 89, 42),
                       Visits = c(484, 628, 628))
d.customers$Poss[2] = d.customers$Poss[1] - d.customers$Sales[1]
d.customers$Poss[3] = d.customers$Poss[2] - d.customers$Sales[2]
d.customers$Rate = d.customers$Sales / d.customers$Visits
d.customers$UnvistdMin = NA   # All visits were to unvisited HHs
d.customers$UnvistdMax = NA   # All visits were to visited HHs
d.customers$UnvistdMin[1] = d.customers$Poss[1] - d.customers$Visits[1] 
d.customers$UnvistdMax[1] = d.customers$UnvistdMin[1]
d.customers$UnvistdMin[2] = d.customers$UnvistdMin[1] - d.customers$Visits[2]
d.customers$UnvistdMax[2] = d.customers$UnvistdMax[1] - 
  (d.customers$Visits[2] - (d.customers$Visits[1] - d.customers$Sales[1]))
d.customers$UnvistdMin[3] = 0
d.customers$UnvistdMax[3] = d.customers$UnvistdMax[2] - 
  (d.customers$Visits[3] - (d.customers$Visits[2] - d.customers$Sales[2]))
d.customers$RevisitsMin = c(0,0,NA)
d.customers$RevisitsMin[3] = -(d.customers$UnvistdMin[2] - d.customers$Visits[3])
d.customers$RevisitsMax = c(0, d.customers$Visits[1], d.customers$Visits[3])

###############################################################################
# Create d.households based on iDE Cambodia report
###############################################################################
d.households = data.frame(VillTyp = c("Cntrl", "Intrv", "Nei-Intrv"),
                          HHs = c(178+176+169+129+131,
                                  163+166+170+108+147,
                                  NA),
                          LatCov = c(mean(0.97, 0.98, 0.85, 0.85, 0.95),
                                     mean(0.98, 0.95, 0.94, 0.91, 0.97),
                                     NA))
num_PGs = 44+47+54+30+51
d.households$HHs[3] = d.households$HHs[2] - num_PGs
d.households$HHs[2] = num_PGs
d.households$LatCov[3] = d.households$LatCov[2]

###############################################################################
# Gather customer survey data from old study for these villages
###############################################################################
load(file = paste(getwd(),"/data/surveys/iDEcustomer/iDE_Oct2017.Rdata", 
                  sep = ""))
d.custmrsurvs = data
rm(data)
d.custmrsurvs.SvyRng = subset(d.custmrsurvs, Prov == "Svay Rieng")
d.custmrsurvs.SvyRng.Rmduol = subset(d.custmrsurvs.SvyRng, 
                                     Dist == "Rumduol")
d.custmrsurvs.SvyRng = droplevels(d.custmrsurvs.SvyRng)
d.custmrsurvs.SvyRng.Rmduol = droplevels(d.custmrsurvs.SvyRng.Rmduol)

###############################################################################
# Add ADP sales data to d.baseline and d.followup
###############################################################################
summary(d.adpsales)

# Rename columns, make variables factors, and remove irrelevant columns
colnames(d.adpsales) = c("ID", "Name", "Phone", "Vill", "Comm", "Dist", "Prov",
                         "VillTyp", "SalesAgent", "OrderStatus", "OrderDate",
                         "HasPG")
for (col in 1:length(d.adpsales)) {
  d.adpsales[,col] = as.factor(d.adpsales[,col])
}
d.adpsales = subset(d.adpsales, select = -c(ID, Name))

# Add ADP sales data to d.baseline using phone as customer identifier
summary(d.adpsales$Phone)
summary(subset(d.adpsales, Phone == "016704485" | Phone == "0884896342"))
summary(d.baseline)
summary(d.baseline$Phone, maxsum = 1000)
d.baseline$Phone = gsub("N/A", NA, d.baseline$Phone, fixed = TRUE)
d.baseline$Phone = gsub(" ", "", d.baseline$Phone, fixed = TRUE)
d.baseline$Phone = as.factor(d.baseline$Phone)
d.baseline$ADPorderStatus = NA
d.baseline$ADPorderDate = NA
d.baseline$SalesAgent = NA
d.baseline$HasPGcheck = NA
for (row1 in 1:length(d.baseline$Phone)) {
  for (row2 in 1:length(d.adpsales$Phone)) {
    if (!is.na(as.character(d.baseline$Phone[row1]))) {
      if (as.character(d.baseline$Phone[row1]) == 
          as.character(d.adpsales$Phone[row2])) {
        d.baseline$ADPorderStatus[row1] = 
          as.character(d.adpsales$OrderStatus[row2])
        d.baseline$ADPorderDate[row1] = 
          as.character(d.adpsales$OrderDate[row2])
        d.baseline$SalesAgent[row1] = as.character(d.adpsales$SalesAgent[row2])
        d.baseline$HasPGcheck[row1] = as.character(d.adpsales$HasPG[row2])
      }
    }
  }
  # grepl("016704485", d.adpsales$Phone)
  # grepl(as.character(d.baseline$Phone[row1]), d.adpsales$Phone)
}
d.baseline$ADPorderStatus = as.factor(d.baseline$ADPorderStatus)
d.baseline$ADPorderDate = as.factor(d.baseline$ADPorderDate)
d.baseline$SalesAgent = as.factor(d.baseline$SalesAgent)
d.baseline$HasPGcheck = as.factor(d.baseline$HasPGcheck)
summary(d.baseline)
test = data.frame(A = d.baseline$PGid, B = d.baseline$HasPGcheck)
test = subset(test, !is.na(B))
length(test[,1])
# PG check not passed for 2 out of 47 ADP sales in d.baseline.
for (PGid in c("39V3", "51V5")) {
  d.baseline$ADPorderStatus[d.baseline$PGid == PGid] = NA
  d.baseline$ADPorderDate[d.baseline$PGid == PGid] = NA
  d.baseline$SalesAgent[d.baseline$PGid == PGid] = NA
  d.baseline$HasPGcheck[d.baseline$PGid == PGid] = NA
}
d.baseline$ADPorderStatus[d.baseline$PGid != "39V3"]
d.baseline$ADPorderStatus[d.baseline$PGid != "51V5"]

# Add ADP sales data to d.followup using phone as customer identifier
summary(d.adpsales$Phone)
summary(d.followup)
summary(d.followup$Phone, maxsum = 1000)
d.followup$Phone = gsub("Dany", NA, d.followup$Phone, fixed = TRUE)
d.followup$Phone = gsub("N/A", NA, d.followup$Phone, fixed = TRUE)
d.followup$Phone = gsub(" ", "", d.followup$Phone, fixed = TRUE)
d.followup$Phone = as.factor(d.followup$Phone)
summary(d.followup$Phone2, maxsum = 1000)
d.followup$Phone2 = gsub("N/A", NA, d.followup$Phone2, fixed = TRUE)
d.followup$Phone2 = gsub("`", "", d.followup$Phone2, fixed = TRUE)
d.followup$Phone2 = gsub(" ", "", d.followup$Phone2, fixed = TRUE)
d.followup$Phone2 = as.factor(d.followup$Phone2)
d.followup$ADPorderStatus = NA
d.followup$ADPorderDate = NA
d.followup$SalesAgent = NA
d.followup$HasPGcheck = NA
count = 0
for (row1 in 1:length(d.followup$Phone)) {
  for (row2 in 1:length(d.adpsales$Phone)) {
    if (!is.na(as.character(d.followup$Phone[row1]))) {
      if (as.character(d.followup$Phone[row1]) == 
          as.character(d.adpsales$Phone[row2])) {
        d.followup$ADPorderStatus[row1] = 
          as.character(d.adpsales$OrderStatus[row2])
        d.followup$ADPorderDate[row1] = 
          as.character(d.adpsales$OrderDate[row2])
        d.followup$SalesAgent[row1] = as.character(d.adpsales$SalesAgent[row2])
        d.followup$HasPGcheck[row1] = as.character(d.adpsales$HasPG[row2])
        count = count + 1
      }
    }
  }
}
count
# No ADP sales data found.
count = 0
for (row1 in 1:length(d.followup$Phone2)) {
  for (row2 in 1:length(d.adpsales$Phone)) {
    if (!is.na(as.character(d.followup$Phone2[row1]))) {
      if (as.character(d.followup$Phone2[row1]) == 
          as.character(d.adpsales$Phone[row2])) {
        d.followup$ADPorderStatus[row1] = 
          as.character(d.adpsales$OrderStatus[row2])
        d.followup$ADPorderDate[row1] = 
          as.character(d.adpsales$OrderDate[row2])
        d.followup$SalesAgent[row1] = as.character(d.adpsales$SalesAgent[row2])
        d.followup$HasPGcheck[row1] = as.character(d.adpsales$HasPG[row2])
        count = count + 1
      }
    }
  }
}
count
d.followup$ADPorderStatus = as.factor(d.followup$ADPorderStatus)
d.followup$ADPorderDate = as.factor(d.followup$ADPorderDate)
d.followup$SalesAgent = as.factor(d.followup$SalesAgent)
d.followup$HasPGcheck = as.factor(d.followup$HasPGcheck)
summary(d.followup, maxsum = 1000)
test = data.frame(A = d.followup$PGid, B = d.followup$HasPGcheck)
test = subset(test, !is.na(B))
length(test[,1])
# PG check not passed for 4 out of 58 ADP sales in d.followup.
for (PGid in c("39V3")) {
  d.followup$ADPorderStatus[d.followup$PGid == PGid] = NA
  d.followup$ADPorderDate[d.followup$PGid == PGid] = NA
  d.followup$SalesAgent[d.followup$PGid == PGid] = NA
  d.followup$HasPGcheck[d.followup$PGid == PGid] = NA
}
d.followup$ADPorderStatus[d.followup$PGid != "39V3"]
for (row in c(377, 530, 573)) {
  d.followup$ADPorderStatus[row] = NA
  d.followup$ADPorderDate[row] = NA
  d.followup$SalesAgent[row] = NA
  d.followup$HasPGcheck[row] = NA
}
summary(d.followup)

###############################################################################
# DATA QUALITY CONTROL
###############################################################################
# Dataframes
dataframes = c(d.baseline, d.followup, d.sldglvls, d.custmrsurvs, 
               d.ADPsales.villtyp, d.ADPsales.PG, d.custmrsurvs.SvyRng, 
               d.custmrsurvs.SvyRng.Rmduol, d.customers)
dset = d.baseline
summary(dset, maxsum = 5)
names(dset)
sapply(dset, function(x) sum(is.na(x)))
missmap(dset, main = "Missing Values in Variables", legend = F)

# Variables
print(NumADPSoldByWave)

###############################################################################
# SAVE DATA TO DISK
###############################################################################
save(d.baseline, d.followup, d.sldglvls, d.custmrsurvs, d.custmrsurvs.SvyRng,
     d.ADPsales.villtyp, d.ADPsales.PG, d.custmrsurvs.SvyRng.Rmduol, 
     NumADPSoldByWave, d.customers, d.households,
     file = paste(getwd(), "/data/raw/surveys/pit_gauge/pit_gauge.RData", 
                  sep = ""))
