# Analysis of Survey Responses of Rural Cambodian Latrine Owners
# Written by James Harper, PE, ENV SP of the University of Colorado Boulder
# Started October 1, 2017; Last updated Dec 18, 2018
###############################################################################
# Initialize environment and user input
rm(list = ls())                                      # Clear global environment
cat("\014")                                              # Clear console window
source("functions.R")                                   # Load custom functions
load_libraries(                                      # Install & load libraries
  c("rio", "gplots", "Amelia", "car", "fastDummies", "FactoMineR", "lavaan",
    "extrafont"))
loadfonts(device = "win")
clean.raw.data = 1                                      # Clean raw data: 1=yes
###############################################################################
# Load, clean, and summarize data
if (clean.raw.data == 0) {
  load(file = paste(getwd(),
                    "/data/raw/surveys/pit_gauge/pit_gauge.RData", sep = ""))
  } else if (clean.raw.data == 1) {
    file.to.import1 = 
      paste(
        getwd(),
        "/data/raw/surveys/pit_gauge/pit_gauge_baseline_data_analysis.xlsx",
        sep = "")
    file.to.import2 = 
      paste(
        getwd(),
        "/data/raw/surveys/pit_gauge/pit_gauge_followup_data_analysis.xlsx",
        sep = "")
    file.to.import3 = 
      paste(
        getwd(),
        "/data/raw/surveys/pit_gauge/pit_gauge_sludge_levels.xlsx",
        sep = "")
    data.baseline = import(file.to.import1, which = "Data")
    data.followup = import(file.to.import2, which = "Data")
    data.sldglvls = import(file.to.import3, which = "Data")
    
    ###########################################################################
    # CLEAN data.baseline
    ###########################################################################
    # Shorten variable (column) names
    names(data.baseline)
    old.col.names1 = names(data.baseline)
    names(data.baseline) = 
      c("PGidOld", "PGid", "HHid", "Intrviewr", "PGInstlDate", "RespName", 
        "Gend", "Phone", "Vill", "Comm", "Dist", "Prov", "Lat", "Lon", 
        "VillTyp2", "RelToLatOwnr", "HavIDPoorCard", "LatOwnrIDPoor", 
        "LatOwnrJob1", "LatOwnrJob2", "NumAdlts18", "NumChld217", "NumInf2", 
        "NumPplHH", "LatInstlYr", "NumRings", "LatStartUseYr", "UseLatReg", 
        "EmptyBefor","WhyNoEmpty", "EmptyMethds", "PiercdPit", "SurveyYr",
        "LatAge")
    print(data.frame(names(data.baseline), old.col.names1))
    
    # Remove unused variables
    names(data.baseline)
    print(data.baseline$Lat)
    data.baseline = subset(data.baseline, select = -c(PGidOld, SurveyYr, 
                                                      NumRings))
    
    # Remove bad rows due to problem with pit gauge or similar
    data.baseline = subset(data.baseline, PGid != "9V5")
    
    # Convert data formats
    summary(data.baseline, maxsum = 10)
    data.baseline$Intrviewr = as.factor(data.baseline$Intrviewr)
    data.baseline$Lat = as.numeric(as.character(data.baseline$Lat))
    data.baseline$Lon = as.numeric(as.character(data.baseline$Lon))
    data.baseline$LatStartUseYr = 
      as.numeric(as.character(data.baseline$LatStartUseYr))
    for (i in 1:length(names(data.baseline))) {   # All characters into factors
      if (is.character(data.baseline[i][[1]])) {
        data.baseline[i][[1]] = as.factor(data.baseline[i][[1]])
      }
    }
    
    # Rename PGid
    summary(data.baseline$PGid)
    data.baseline$PGid[data.baseline$PGid == 0] = NA
    
    # Rename responses in RelToLatOwnr
    summary(data.baseline$RelToLatOwnr)
    levels(data.baseline$RelToLatOwnr) = 
      c("Child", "Child", "LatOwnr", "Sibling", "Child", "Child", "Child",
        "Parent", "Parent", "Parent", "Child", "Child", "LatOwnr", 
        "LatOwnr", "Child", "Parent", "Parent", "LatOwnr", "Sibling",
        "Child", "Child", "Spouse")
    
    # Create new variable Job based on LatOwnrJob1 and LatOwnrJob2
    summary(data.baseline$LatOwnrJob1)
    summary(data.baseline$LatOwnrJob2)
    # print(data.frame(data.baseline$LatOwnrJob1, data.baseline$LatOwnrJob2))
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
                     data.baseline$LatOwnrJob2))
    data.baseline$Job = as.factor(data.baseline$Job)
    summary(data.baseline$Job)
    print(subset(data.frame(A = data.baseline$Job, 
                            B = data.baseline$LatOwnrJob1, 
                            C = data.baseline$LatOwnrJob2), 
                 is.na(A)))
    # plot(summary(data.baseline$Job))
    
    # Rename responses in NumInf2 and change type
    summary(data.baseline$NumInf2, maxsum = 30)
    levels(data.baseline$NumInf2) = c(0, 1, 2, 3, 0)
    data.baseline$NumInf2 = as.numeric(as.character(data.baseline$NumInf2))
    
    # Rename responses in LatInstlYr and change type
    summary(data.baseline$LatInstlYr, maxsum = 30)
    data.baseline$LatInstlYr[data.baseline$LatInstlYr == "Not Remember"] = 
      NA
    data.baseline$LatInstlYr = droplevels(data.baseline$LatInstlYr)
    data.baseline$LatInstlYr = 
      as.numeric(as.character(data.baseline$LatInstlYr))
    
    # Rename UseLatReg
    summary(data.baseline$UseLatReg)
    levels(data.baseline$UseLatReg) = c("Never", "Some", "Always")
    
    # Rename EmptyBefor
    summary(data.baseline$EmptyBefor)
    levels(data.baseline$EmptyBefor) = c("No", "Yes", "Yes")
    
    # Rename WhyNoEmpty
    summary(data.baseline$WhyNoEmpty)
    levels(data.baseline$WhyNoEmpty) = c("Busy", "Busy", "NotClogged", 
                                         "NotUsing", "NoProAvailable",
                                         "NoProAvailable", "NotFull",
                                         "NotFull", "NotFull", 
                                         "PiercedPit", "NotUsing",
                                         "Abandoned")

    # Rename EmptyMethds
    summary(data.baseline$EmptyMethds)
    levels(data.baseline$EmptyMethds) = c("Self-Bucketing", "Self-Pump", 
                                          "PaidPro", "Self-Pump", 
                                          "Self-Pump", "Self-Pump",
                                          "Self-Bucketing", "Self-Bucketing", 
                                          "Unknown", "Self-Pump")
    
    ###########################################################################
    # CLEAN data.followup
    ###########################################################################
    # Shorten variable (column) names
    names(data.followup)
    old.col.names2 = names(data.followup)
    names(data.followup) = 
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
    print(data.frame(names(data.followup), old.col.names2))
    
    # Convert data formats
    summary(data.followup, maxsum = 30)
    data.followup$Date = as.Date(data.followup$Date)
    data.followup$NumF61HH = as.numeric(data.followup$NumF61HH)
    # Rename and change format of LatInstlDate
      summary(as.factor(data.followup$LatInstlDate))
      data.followup$LatInstlDate[data.followup$LatInstlDate == "Don't know"] = NA
      data.followup$LatInstlDate[data.followup$LatInstlDate == 0] = NA
      data.followup$LatInstlDate = 
        as.Date(as.character(data.followup$LatInstlDate), "%m-%d-%Y")
      summary(data.followup$LatInstlDate)
    # Rename and change format of LatStartUseDate
      summary(as.factor(data.followup$LatStartUseDate))
      data.followup$LatStartUseDate[data.followup$LatStartUseDate == "Don't know"] = NA
      data.followup$LatStartUseDate[data.followup$LatStartUseDate == 0] = NA
      data.followup$LatStartUseDate = as.Date(data.followup$LatStartUseDate,
                                              "%m-%d-%Y")
      summary(data.followup$LatStartUseDate)
    # Rename and change format of EmptyLast
      summary(as.factor(data.followup$EmptyLast))
      data.followup$EmptyLast[data.followup$EmptyLast == "Don't know"] = NA
      data.followup$EmptyLast[data.followup$EmptyLast == "Don't Know"] = NA
      data.followup$EmptyLast[data.followup$EmptyLast == 0] = NA
      data.followup$EmptyLast = as.Date(data.followup$EmptyLast,
                                              "%m-%d-%Y")
      summary(data.followup$EmptyLast)
    for (i in 1:length(names(data.followup))) {   # All characters into factors
      if (is.character(data.followup[i][[1]])) {
        data.followup[i][[1]] = as.factor(data.followup[i][[1]])
      }
    }
    
    # Remove unused variables
    # data.followup = subset(data.followup, select = -c())
    
    # Rename Avail
    summary(data.followup$Avail)
    levels(data.followup$Avail) = c("No", "Yes", "Yes")
    
    # Rename RGend
    summary(data.followup$RGend)
    levels(data.followup$RGend) = c("F", "M")
    
    # Rename CGend
    summary(data.followup$CGend)
    levels(data.followup$CGend) = c("F", "M")
    
    # Rename RelToLatOwnr
    summary(data.followup$RelToLatOwnr)
    levels(data.followup$RelToLatOwnr) = 
      c("Sibling", "Spouse", "Parent", "Other", "Self")
    for (row in 1:length(data.followup$RelToLatOwnr)) {
      if (is.na(data.followup$RelToLatOwnr[row])) {
        if (!is.na(data.followup$RPurLat[row]) & 
            data.followup$RPurLat[row] == "Yes") {
          data.followup$RelToLatOwnr[row] = "Self"
          }
      }
    }
    
    # Update IDPoorTyp with IDPoor
    summary(data.followup$IDPoorTyp)
    summary(data.followup$IDPoor)
    levels(data.followup$IDPoorTyp) = c("IDPoor1", "IDPoor2", "NotIDPoor")
    for (row in 1:length(data.followup$IDPoorTyp)) {
      if (!is.na(data.followup$IDPoor[row])) {
        if (data.followup$IDPoor[row] == "No") {
          data.followup$IDPoorTyp[row] = "NotIDPoor"
        }
      }
    }
    
    # Rename LivRP
    summary(data.followup$LivRP)
    levels(data.followup$LivRP) = c("No", "No")
    
    # Rename HousFlod
    summary(data.followup$HousFlod)
    levels(data.followup$HousFlod) = c("No", "No", "Yes")
    
    # Rename FlodSevrty
    summary(data.followup$FlodSevrty)
    levels(data.followup$FlodSevrty) = c("Mild", "Moderate", "Other", "Severe")
    data.followup$FlodSevrty[data.followup$FlodSevrty == "Other"] = NA
    data.followup$FlodSevrty = droplevels(data.followup$FlodSevrty)
    
    # Create new variable VillTyp based on VillTyp2 and PGInstld
    summary(data.followup$VillTyp2)
    summary(data.followup$PGInstld)
    print(data.frame(data.followup$VillTyp2, data.followup$PGInstld))
    data.followup$VillTyp = as.character(NA)
    for (row in 1:length(data.followup$VillTyp2)) {
      if (is.na(data.followup[row,]$VillTyp2) | 
          is.na(data.followup[row,]$PGInstld)) {
        data.followup[row,]$VillTyp = NA
      } else if (data.followup[row,]$VillTyp2 == "Control") {
        data.followup[row,]$VillTyp = "Cntrl"
      } else if (data.followup[row,]$VillTyp2 == "Treatment" & 
                 data.followup[row,]$PGInstld == "Yes") {
        data.followup[row,]$VillTyp = "TreatPG"
      } else if (data.followup[row,]$VillTyp2 == "Treatment" & 
                 data.followup[row,]$PGInstld == "No") {
        data.followup[row,]$VillTyp = "TreatNei"
      }
    }
    print(data.frame(data.followup$VillTyp2, data.followup$PGInstld, 
                     data.followup$VillTyp))
    data.followup$VillTyp = as.factor(data.followup$VillTyp)
    summary(data.followup$VillTyp)
    
    # Remove rows without full group description
    # data.followup = subset(data.followup, !is.na(VillTyp))
    
    # Rename responses in EmptyChlngs
    summary(data.followup$EmptyChlngs)
    levels(data.followup$EmptyChlngs) = c("None", "EmptyReqTooFreq")
    
    # Rename responses in EmptyMethds
    summary(data.followup$EmptyMethds)
    levels(data.followup$EmptyMethds) = c("Self-Bucketing", "Self-Pump")
    
    # Rename responses in EmptyWho
    summary(data.followup$EmptyWho)
    levels(data.followup$EmptyWho) = c("FamilyMemb", "Pro", "Self")
    
    # Rename responses in EmptyCost
    summary(data.followup$EmptyCost)
    levels(data.followup$EmptyCost) = 
      c(0, 0, 10000, 12000, 120000, 1500, 20000, 3500, 3700, 4000, 50000, 7000, 
        "DK", "DK", "DK")
    
    # Rename responses in EmptyExprience
    summary(data.followup$EmptyExprience)
    levels(data.followup$EmptyExprience) = c("Good", "Neutral", "Bad")
    
    # Rename responses in EmptyTimeHrs
    summary(data.followup$EmptyTimeHrs)
    levels(data.followup$EmptyTimeHrs) = 
      c(1, 1.5, 10/60, 12, 15/60, 2, 3, 0.5, 4, 4/60, 5, 5/60, 6, 8)
    data.followup$EmptyTimeHrs = as.numeric(as.character(data.followup$EmptyTimeHrs))
    
    # Rename responses in EmptyWhy and EmptyWhy.Othr, and combine into EmptyWhy
    summary(data.followup$EmptyWhy)
    levels(data.followup$EmptyWhy) = c("Smell", "Smell", "Unusable", "Other", 
                                       "FertlizCrops")
    summary(data.followup$EmptyWhy.Othr)
    levels(data.followup$EmptyWhy.Othr) = 
      c("FertlizCrops", "FertlizCrops", "FertlizCrops", "ScheduledTimeArrived",
        "Don'tKnow", "PitOverflowd")
    levels(data.followup$EmptyWhy) = c(levels(data.followup$EmptyWhy), 
                                       levels(data.followup$EmptyWhy.Othr))
    data.frame(data.followup$EmptyWhy, data.followup$EmptyWhy.Othr)
    for (row in 1:length(data.followup$EmptyWhy)) {
      if (!is.na(data.followup$EmptyWhy[row])) {
        if (data.followup$EmptyWhy[row] == "Other") {
          data.followup$EmptyWhy[row] = data.followup$EmptyWhy.Othr[row]
        }
      }
    }
    data.followup$EmptyWhy = droplevels(data.followup$EmptyWhy)
    
    # Rename EmptyDispos
    summary(data.followup$EmptyDispos)
    levels(data.followup$EmptyDispos) = 
      c("Buried", "FertlizCrops", "InField", "InPondRiv")
    
    # Rename EmptyWhyNot and combine with EmptyWhyNot.Othr
    summary(data.followup$EmptyWhyNot)
    summary(data.followup$EmptyWhyNot.Othr)
    levels(data.followup$EmptyWhyNot.Othr) = c("TooBusy", "WrongEquipment")
    levels(data.followup$EmptyWhyNot) = 
      c("NotFull", "Other", "PreferOvrflw", levels(data.followup$EmptyWhyNot.Othr))
    for (row in 1:length(data.followup$EmptyWhyNot)) {
      if (!is.na(data.followup$EmptyWhyNot[row])) {
        if (data.followup$EmptyWhyNot[row] == "Other") {
          data.followup$EmptyWhyNot[row] = data.followup$EmptyWhyNot.Othr[row]
        }
      }
    }
    data.followup$EmptyWhyNot = droplevels(data.followup$EmptyWhyNot)
    
    # Rename EmptyPlanMethds and combine with EmptyPlanMethds.Othr
    summary(data.followup$EmptyPlanMethds)
    summary(data.followup$EmptyPlanMethds.Othr)
    levels(data.followup$EmptyPlanMethds.Othr) = c("PayPro", "MixWithAsh")
    levels(data.followup$EmptyPlanMethds) = 
      c("Pump", "DK", "Bucketing", "Other", "VTruck", 
        levels(data.followup$EmptyPlanMethds.Othr))
    for (row in 1:length(data.followup$EmptyPlanMethds)) {
      if (!is.na(data.followup$EmptyPlanMethds[row])) {
        if (data.followup$EmptyPlanMethds[row] == "Other") {
          data.followup$EmptyPlanMethds[row] = data.followup$EmptyPlanMethds.Othr[row]
        }
      }
    }
    data.followup$EmptyPlanMethds = droplevels(data.followup$EmptyPlanMethds)
    
    # Rename responses in EmptyWilPay to describe # Cambodian Riel to empty a
    # 3-ring pit
    summary(data.followup$EmptyWilPay)
    levels(data.followup$EmptyWilPay) = 
      c(10000, 100000, 14000, 1500, 15000, 18000, 20000, 200000, 30000, 3500, 
        3700, 40000, 50000, 6000, 60000, 7000, 9500, 0, "DK", "DK")
    
    # Rename FSMServProvdrs
    summary(data.followup$FSMServProvdrs)
    levels(data.followup$FSMServProvdrs) = c("DK", "None", "None", "Yes")
    
    # Rename FSMServProvdrs.Contact
    summary(data.followup$FSMServProvdrs.Contact)
    levels(data.followup$FSMServProvdrs.Contact) = 
      c("KnowPhone1", "KnowPhone2", "KnowPhone3", "KnowPhone4", "DKPhone")
    
    # Rename FSMServProvdrs.Cost
    summary(data.followup$FSMServProvdrs.Cost)
    levels(data.followup$FSMServProvdrs.Cost) = 
      c("$10/ring", "$150/1thru5rings", "$40/3rings", "$25/3rings", "$50",
        "50000Riel", "DK")
    subset(data.frame(A = data.followup$FSMServProvdrs,
                      B = data.followup$FSMServProvdrs.Contact,
                      C = data.followup$FSMServProvdrs.Cost),
           A == "Yes")
    
    # Create RespQuesTopic based on CommntQues
    summary(data.followup$CommntQues)
    data.followup$RespQuesTopic = data.followup$CommntQues
    levels(data.followup$RespQuesTopic) =
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
    data.frame(data.followup$CommntQues, data.followup$RespQuesTopic)  
    
    ###########################################################################
    # CLEAN data.sldglvls
    ###########################################################################
    # Shorten variable (column) names
    names(data.sldglvls)
    old.col.names3 = names(data.sldglvls)
    names(data.sldglvls) = 
      c("ID", "PGid", "Intrviewr", "PGInstlDate", "CreatDate", "MeasurDate",
        "CreatName", "PGclientName", "Vill", "Comm", "Dist", "Prov",
        "SludgDepth(mm)", "LiquidDepth(mm)", "PGProblem")
    print(data.frame(names(data.sldglvls), old.col.names3))
    
    # Convert data formats
    summary(data.sldglvls, maxsum = 30)
    data.sldglvls$PGInstlDate = as.Date(data.sldglvls$PGInstlDate)
    data.sldglvls$CreatDate = as.Date(data.sldglvls$CreatDate)
    data.sldglvls$MeasurDate = as.Date(data.sldglvls$MeasurDate)
    for (i in 1:length(names(data.sldglvls))) {   # All characters into factors
      if (is.character(data.sldglvls[i][[1]])) {
        data.sldglvls[i][[1]] = as.factor(data.sldglvls[i][[1]])
      }
    }
    
    ###########################################################################
    # Create data.sales based on correspondence from iDE Cambodia
    ###########################################################################
    NumADPSoldByWave = c(106, 89, 42)
    ADPclosRateWithWithoutPG = c(0.27, 0.12)
    NumADPdelvredWithWithoutPG = c(0.870, 0.600)
    NumADPcancledWithWithoutPG = c(0.080, 0.340)
    NumADPbacklgdWithWithoutPG = c(0.050, 0.060)
    
    
    ###########################################################################
    # Customer Survey data for these villages
    ###########################################################################
    load(file = paste(getwd(),"/data/raw/surveys/iDE_Oct2017.Rdata", sep = ""))
    data.custmrsurvs = data
    rm(data)
    summary(data.baseline)
    data.custmrsurvs.SvyRng = subset(data.customersurveys, 
                                     Prov == "Svay Rieng")
    data.custmrsurvs.SvyRng.Rmduol = subset(data.custmrsurvs.SvyRng, 
                                            Dist == "Rumduol")
    data.custmrsurvs.SvyRng = droplevels(data.custmrsurvs.SvyRng)
    data.custmrsurvs.SvyRng.Rmduol = droplevels(data.custmrsurvs.SvyRng.Rmduol)
    
    ###############################################################################
    # DATA QUALITY CONTROL
    ###############################################################################
    summary(data.baseline, maxsum = 30)
    summary(data.followup, maxsum = 30)
    summary(data.sldglvls, maxsum = 30)
    # missmap(data.baseline, main = "Missing vs observed", legend = F)  # visualize NAs
    # missmap(data.followup, main = "Missing vs observed", legend = F)
    # missmap(data.sldglvls, main = "Missing vs observed", legend = F)
    sapply(data.baseline, function(x) sum(is.na(x)))  # count NAs
    sapply(data.followup, function(x) sum(is.na(x)))
    sapply(data.sldglvls, function(x) sum(is.na(x)))
    
    ###############################################################################
    # SAVE DATA TO DISK
    ###############################################################################
    save(data.baseline, data.followup, data.sldglvls,
         file = paste(getwd(),"/data/raw/surveys/pit_gauge/pit_gauge.RData", sep = ""))

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




# Knowledge of FSM Service Providers
subset(data.frame(A = data.followup$FSMServProvdrs,
                  B = data.followup$FSMServProvdrs.Contact,
                  C = data.followup$FSMServProvdrs.Cost), A == "Yes")

# Customer Survey data relevant to Pit Gauge Study
summary(data.custmrsurvs.SvyRng)
  # 478 surveys from Province Svay Rieng
summary(data.custmrsurvs.SvyRng.Rmduol)
  # Only 81 surveys from District Rumduol 
summary(data.custmrsurvs.SvyRng.Rmduol$Comm)
  # Only 10 surveys from Communes Chrung Popel and Muen Chey












##########################################################################
# CLEAN UP
##########################################################################
sink()
dev.off()
closeAllConnections()
file.remove(paste(getwd(),"/Output/dump.txt", sep = ""))
