# Analysis of Survey Responses of Rural Cambodian Latrine Owners
# Written by James Harper, PE, ENV SP of the University of Colorado Boulder
# Started October 1, 2017; Last updated Aug 8, 2018
###############################################################################
# Initialize environment and user input
rm(list = ls())                                      # Clear global environment
cat("\014")                                              # Clear console window
source("functions.R")                                   # Load custom functions
load_libraries(                                      # Install & load libraries
  c("rio", "gplots", "Amelia", "car", "fastDummies", "corrplot",
    "FactoMineR", "factoextra", "lavaan"))
loadfonts(device = "win")
clean.raw.data = 0                                      # Clean raw data: 1=yes
###############################################################################
# Load, clean, and summarize data
if (clean.raw.data != 1) {
  load(file = 
         paste(getwd(),"/data/raw/surveys/",
               "describe_FSMintentions_regional_seasonal_iDE_Oct2017.RData",
               sep = ""))
} else if (clean.raw.data == 1) {
  
  ###############################################################################
  # LOAD DATA
  ###############################################################################
  file_to_import = 
    paste(getwd(),
          "/data/raw/surveys/Customer Survey-exported30oct2017.xlsx", sep = "")
  data = import(file_to_import)
  # names(data)
  
  ###############################################################################
  # CLEAN DATA
  ###############################################################################
  # Shorten variable (column) names
  old.col.names = names(data)
  names(data) = c("ID", "Creatr", "Date", "LBO", "Prov", "Dist", "Comm", "Vill",
                  "Lat", "Lng", "CLoc", "LatPur", "RLname", "RFname", "RGend",
                  "Phone", "RisC", "CLname", "CFname", "CGend", "CrelR",
                  "IDPoor", "IDPoorTyp", "M01", "M1824", "M217", "M2559",
                  "M60", "F01", "F1824", "F217",
                  "F2559", "F60", "LivRP", "VillOD", "DateSlabPur", "LatInst",
                  "BelowGrndInst", "DateBelowGrndInst", "ShltrInst",
                  "DateInstComp", "RDefBefor", "RDefBeforOthr", "FreqNeiToi",
                  "WhoInstLat", "KnwSubsdy", "RecSubsdy", "BorwLat",
                  "CanBuyLat", "UseFincOthr", "SlabTil", "Npits", "PitConfig",
                  "NringsDir", "NringsOff", "NringsOthr", "WallMat",
                  "WallMatOthr", "RoofMat", "RoofMatOthr", 
                  "MatsPurTgthr", "Cost", "CostInclInst", "CostPitSlab", 
                  "CostPitSlabKnwn", "CostShltrMats", "CostShltrMatsKnwn",
                  "LabPitShltrPurTgthr", "CostLabLat", "CostLabPitSlab",
                  "CostLabPitSlabKnwn", "CostLabShltr", "CostLabShltrKnwn",
                  "LatTypOwndBefor", "IntndChngDich", "IntndChng", 
                  "IntndChngOthr", "AdltUseLat", "ChldUseLat", "InfLatDump",
                  "IntndPitFull", "IntndPitFullOthrAns", "Chlngs", "Satis", "Rec",
                  "SatisSup", "RecSup", "RQues", "DateSurvCreated")
  # print(data.frame(names(data), old.col.names))
  
  # Convert data formats
  # summary(data)
  for (i in 1:length(names(data))) {
    if (is.character(data[i][[1]])) {
      data[i][[1]] = as.factor(data[i][[1]])
    }
  }
  data$Date = as.Date(data$Date)
  data$DateSlabPur = as.Date(data$DateSlabPur)
  data$DateBelowGrndInst = as.Date(data$DateBelowGrndInst)
  data$DateInstComp = as.Date(data$DateInstComp)
  data$DateSurvCreated = as.Date(data$DateSurvCreated)
  
  # Remove unused variables
  data = subset(data, select = -c(ID, Creatr, Lat, Lng, CLoc, LatPur, RLname,
                                  RFname, RGend, Phone, RisC, CLname, CFname, 
                                  CGend, CrelR, RQues))
  
  # Remove data that is not applicable to this study
  data = subset(data, Prov != "Phnom Penh")
  
  # Remove data with low frequencies
  data = subset(data, Prov != "Takeo" & Prov != "Kampong Cham" &
                  Prov != "Kampong Speu")
  
  # Create Yr, Mnth, and YrMnth variables as factors
  data$Yr = as.factor(format(as.Date(data$Date, format = "%Y-%m-%d"),"%Y"))
  data$Mnth = as.factor(format(as.Date(data$Date, format = "%Y-%m-%d"),"%m"))
  data$YrMnth = as.factor(paste(data$Yr, data$Mnth, sep = ""))
  
  # Rename responses in variables as needed
  levels(data$Prov) = c("Banteay Meanchey", "Kampong Cham", "Kampong Speu",
                        "Kampong Thom", "Kandal", "Oddar Meanchey", "Phnom Penh",
                        "Prey Veng", "Siem Reap", "Svay Rieng", "Takeo")
  levels(data$IDPoorTyp) = c("IDP1", "IDP2", "UnkIDP", "No")
  levels(data$LivRP) = c("No", "Pond", "Rivr", "Pond", "Rivr")
  levels(data$VillOD) = c("Most", "None", "Some")
  levels(data$RDefBefor) = c("Bsh/Fld", "Bsh/Fld;NA/AlwysToi", "Bsh/Fld;Othr",
                             "NeiToi", "NeiToi;Bsh/Fld", "NeiToi;Bsh/Fld;Othr",
                             "NeiToi;NA/AlwysToi", "NeiToi;Othr", "NA/AlwysToi",
                             "Othr", "NA/AlwysToi", "Riv/Pnd", "Riv/Pnd;Bsh/Fld",
                             "Riv/Pnd;NeiToi;Bsh/Fld", "Riv/Pnd;Othr")
  levels(data$FreqNeiToi) = c("Freq", "Freq", "Nevr", "NA/AlwysToi",
                              "NA/AlwysToi", "Some")
  levels(data$WhoInstLat) = c("Self/Fam", "Self/Fam", "Gov", "Friend$", "Mason",
                              "LBO", "NGO")
  levels(data$RecSubsdy) = c("DK", "No", "Yfull", "Ypart", "Yfull", "Ypart")
  levels(data$BorwLat) = c("Yes", "DK", "No", "Yes")
  levels(data$IntndChng) = c("Shltr", "Shltr;Othr", "Shwr", "Shltr;Shwr",
                             "Shltr;Shwr;Othr", "Shwr;Sink", 
                             "Shltr;Shwr;Sink", "Shwr;Sink;Othr",
                             "Shwr;Othr", "WtrRes", "Shltr;WtrRes",
                             "Shwr;WtrRes", "Shltr;Shwr;WtrRes",
                             "Shwr;Sink;WtrRes", "Shltr;Shwr;Sink;WtrRes",
                             "Shltr;Shwr;Sink;WtrRes;Othr",
                             "Shwr;Sink;WtrRes;Othr", "Shwr;WtrRes;Othr",
                             "Sink;WtrRes", "Shltr;Sink;WtrRes",
                             "WtrRes;Othr", "Sink", "Shltr;Sink", "Pit",
                             "Shltr;Pit", "Shltr;Pit;Othr", "Shwr;Pit",
                             "Shltr;Shwr;Pit", "Shwr;Sink;Pit",
                             "Shwr;Pit;Othr", "WtrRes;Pit",
                             "Shltr;WtrRes;Pit", "Shwr;WtrRes;Pit",
                             "Shltr;Shwr;WtrRes;Pit", "Shwr;Sink;WtrRes;Pit",
                             "Shltr;Shwr;Sink;WtrRes;Pit", "Sink;Pit",
                             "Shltr;Sink;Pit", "Pit;Othr", "NA/AlwysToi",
                             "Othr")
  levels(data$AdltUseLat) = c("Freq", "Freq", "DK", "Rare", "Some")
  levels(data$ChldUseLat) = c("Freq", "NoChld", "DK/NA", "DK/NA", "Rare", "Some")
  levels(data$InfLatDump) = c("Freq", "NoInf", "DK/NA", "DK/NA", "Rare", "Some")
  levels(data$IntndPitFull) = c("DK", "EmpSlf", "Pit", "Othr", "Pay", "Stop")
  levels(data$Chlngs) = c("NoFlsh", "NoFlsh;Flood", "NoFlsh;Othr",
                          "NoFlsh;Ful/OvrFlw", "NoFlsh;Ful/OvrFlw;Flood",
                          "Flood", "NoWtr", "NoFlsh;NoWtr", 
                          "NoFlsh;Ful/OvrFlw;NoWtr", "NoWtr;Smels",
                          "OK", "NoFlsh", "NoWtr", "Ful/OvrFlw", "Smels",
                          "Othr", "Ful/OvrFlw", "Ful/OvrFlw", "Smels",
                          "NoFlsh;Smels", "NoFlsh;Flood;Smels",
                          "NoFlsh;Ful/OvrFlw;Smels", "Flood;Smels", "Smels;Othr",
                          "Ful/OvrFlw;Flood;Smels")
  levels(data$Satis) = c("DK", 3, 2, 4, 1, 5)
  levels(data$SatisSup) = c("DK", 3, 2, 4, 1, 5)
  levels(data$WallMat) = c("Bamboo / Palm Leaves / Thatch", 
                           "Bamboo / Palm Leaves / Thatch",
                           "Concrete / Brick", "Concrete / Brick",
                           "Galvanized steel", "No walls",
                           "Other", "Plastic Sheet", "Wood")
  levels(data$RoofMat) = c("No roof", "Plastic Sheet", "Galvanized steel",
                           "Bamboo / Palm Leaves / Thatch",
                           "Bamboo / Palm Leaves / Thatch",
                           "Concrete / Brick", "Concrete / Brick",
                           "Galvanized steel", "No roof", "No roof",
                           "Other", "Plastic Sheet", "Tiles", "Wood")
  
  # If IDPoor is No and IDPoorType is NA, then copy IDPoor to IDPoorTyp
  # print(data.frame(data$IDPoor, data$IDPoorTyp))
  for (row in 1:length(data$IDPoor)) {
    if (data[row,]$IDPoor == "No" & is.na(data[row,]$IDPoorTyp)) {
      data[row,]$IDPoorTyp = data[row,]$IDPoor
    }
  }
  print(data.frame(data$IDPoor, data$IDPoorTyp))
  # summary(data$IDPoorTyp)
  
  # Create new variables based on RDefBefor
  # summary(data$RDefBefor)
  data$RDefBefor_BshFld = ifelse(is.na(data$RDefBefor), NA,
                                 ifelse(grepl("Bsh/Fld", data$RDefBefor,
                                              fixed = TRUE), 1, 0))
  data$RDefBefor_RivPnd = ifelse(is.na(data$RDefBefor), NA,
                                 ifelse(grepl("Riv/Pnd", data$RDefBefor,
                                              fixed = TRUE), 1, 0))
  data$RDefBefor_NeiToi = ifelse(is.na(data$RDefBefor), NA,
                                 ifelse(grepl("NeiToi", data$RDefBefor,
                                              fixed = TRUE), 1, 0))
  data$RDefBefor_Othr = ifelse(is.na(data$RDefBefor), NA,
                               ifelse(grepl("Othr", data$RDefBefor,
                                            fixed = TRUE), 1, 0))
  data$RDefBefor_NAAlwysToi = ifelse(is.na(data$RDefBefor), NA,
                                     ifelse(grepl("AlwysToi", data$RDefBefor,
                                                  fixed = TRUE), 1, 0))
  data$RDefBefor_BshFld = as.factor(data$RDefBefor_BshFld)
  data$RDefBefor_RivPnd = as.factor(data$RDefBefor_RivPnd)
  data$RDefBefor_NeiToi = as.factor(data$RDefBefor_NeiToi)
  data$RDefBefor_Othr = as.factor(data$RDefBefor_Othr)
  data$RDefBefor_NAAlwysToi = as.factor(data$RDefBefor_NAAlwysToi)
  # summary(data$RDefBefor_BshFld)
  # summary(data$RDefBefor_RivPnd)
  # summary(data$RDefBefor_NeiToi)
  # summary(data$RDefBefor_Othr)
  # summary(data$RDefBefor_NAAlwysToi)
  
  # Create new variables based on IntndChng
  # summary(data$IntndChng)
  data$IntndChng_Shltr = ifelse(is.na(data$IntndChng), NA,
                                ifelse(grepl("Shltr", data$IntndChng,
                                             fixed = TRUE), "1", "0"))
  data$IntndChng_Shwr = ifelse(is.na(data$IntndChng), NA,
                               ifelse(grepl("Shwr", data$IntndChng,
                                            fixed = TRUE), 1, 0))
  data$IntndChng_Sink = ifelse(is.na(data$IntndChng), NA,
                               ifelse(grepl("Sink", data$IntndChng,
                                            fixed = TRUE), 1, 0))
  data$IntndChng_WtrRes = ifelse(is.na(data$IntndChng), NA,
                                 ifelse(grepl("WtrRes", data$IntndChng,
                                              fixed = TRUE), 1, 0))
  data$IntndChng_Pit = ifelse(is.na(data$IntndChng), NA,
                              ifelse(grepl("Pit", data$IntndChng,
                                           fixed = TRUE), 1, 0))
  data$IntndChng_Othr = ifelse(is.na(data$IntndChng), NA,
                               ifelse(grepl("Othr", data$IntndChng,
                                            fixed = TRUE), 1, 0))
  data$IntndChng_NAAlwysToi = ifelse(is.na(data$IntndChng), NA,
                                     ifelse(grepl("NA/AlwysToi", data$IntndChng,
                                                  fixed = TRUE), 1, 0))
  data$IntndChng_Shltr = as.factor(data$IntndChng_Shltr)
  data$IntndChng_Shwr = as.factor(data$IntndChng_Shwr)
  data$IntndChng_Sink = as.factor(data$IntndChng_Sink)
  data$IntndChng_WtrRes = as.factor(data$IntndChng_WtrRes)
  data$IntndChng_Pit = as.factor(data$IntndChng_Pit)
  data$IntndChng_Othr = as.factor(data$IntndChng_Othr)
  data$IntndChng_NAAlwysToi = as.factor(data$IntndChng_NAAlwysToi)
  # summary(data$IntndChng_Shltr)
  # summary(data$IntndChng_Shwr)
  # summary(data$IntndChng_Sink)
  # summary(data$IntndChng_WtrRes)
  # summary(data$IntndChng_Pit)
  # summary(data$IntndChng_Othr)
  # summary(data$IntndChng_NAAlwysToi)
  
  # Create new variables based on IntndPitFull
  # summary(data$IntndPitFull)
  data$IntndPitFullDes = ifelse(is.na(data$IntndPitFull), NA,
                                ifelse(grepl("Pit", data$IntndPitFull,
                                             fixed = TRUE) |
                                         grepl("Pay", data$IntndPitFull,
                                               fixed = TRUE), "1", "0"))
  data$IntndPitFullDes = as.factor(data$IntndPitFullDes)
  # summary(data$IntndPitFullDes)
  data$IntndPitFullPit = ifelse(is.na(data$IntndPitFull), NA,
                                ifelse(grepl("Pit", data$IntndPitFull,
                                             fixed = TRUE), "1", "0"))
  data$IntndPitFullPit = as.factor(data$IntndPitFullPit)
  # summary(data$IntndPitFullPit)
  data$IntndPitFullEmpSlf = ifelse(is.na(data$IntndPitFull), NA,
                                   ifelse(grepl("EmpSlf", data$IntndPitFull,
                                                fixed = TRUE), "1", "0"))
  data$IntndPitFullEmpSlf = as.factor(data$IntndPitFullEmpSlf)
  # summary(data$IntndPitFullEmpSlf)
  data$IntndPitFullDK = ifelse(is.na(data$IntndPitFull), NA,
                               ifelse(grepl("DK", data$IntndPitFull,
                                            fixed = TRUE), "1", "0"))
  data$IntndPitFullDK = as.factor(data$IntndPitFullDK)
  # summary(data$IntndPitFullDK)
  data$IntndPitFullOthr = ifelse(is.na(data$IntndPitFull), NA,
                                 ifelse(grepl("Othr", data$IntndPitFull,
                                              fixed = TRUE), "1", "0"))
  data$IntndPitFullOthr = as.factor(data$IntndPitFullOthr)
  # summary(data$IntndPitFullOthr)
  data$IntndPitFullPay = ifelse(is.na(data$IntndPitFull), NA,
                                ifelse(grepl("Pay", data$IntndPitFull,
                                             fixed = TRUE), "1", "0"))
  data$IntndPitFullPay = as.factor(data$IntndPitFullPay)
  # summary(data$IntndPitFullPay)
  data$IntndPitFullStop = ifelse(is.na(data$IntndPitFull), NA,
                                 ifelse(grepl("Stop", data$IntndPitFull,
                                              fixed = TRUE), "1", "0"))
  data$IntndPitFullStop = as.factor(data$IntndPitFullStop)
  # summary(data$IntndPitFullStop)
  
  # Create new variables based on Chlngs
  # summary(data$Chlngs)
  data$ChlngsNoFlsh = ifelse(is.na(data$Chlngs), NA,
                             ifelse(grepl("NoFlsh", data$Chlngs,
                                          fixed = TRUE), "1", "0"))
  data$ChlngsNoFlsh = as.factor(data$ChlngsNoFlsh)
  # summary(data$ChlngsNoFlsh)
  data$ChlngsFlood = ifelse(is.na(data$Chlngs), NA,
                            ifelse(grepl("Flood", data$Chlngs,
                                         fixed = TRUE), "1", "0"))
  data$ChlngsFlood = as.factor(data$ChlngsFlood)
  # summary(data$ChlngsFlood)
  data$ChlngsOthr = ifelse(is.na(data$Chlngs), NA,
                           ifelse(grepl("Othr", data$Chlngs,
                                        fixed = TRUE), "1", "0"))
  data$ChlngsOthr = as.factor(data$ChlngsOthr)
  # summary(data$ChlngsOthr)
  data$ChlngsFulOvrFlw = ifelse(is.na(data$Chlngs), NA,
                                ifelse(grepl("Ful/OvrFlw", data$Chlngs,
                                             fixed = TRUE), "1", "0"))
  data$ChlngsFulOvrFlw = as.factor(data$ChlngsFulOvrFlw)
  # summary(data$ChlngsFulOvrFlw)
  data$ChlngsNoWtr = ifelse(is.na(data$Chlngs), NA,
                            ifelse(grepl("NoWtr", data$Chlngs,
                                         fixed = TRUE), "1", "0"))
  data$ChlngsNoWtr = as.factor(data$ChlngsNoWtr)
  # summary(data$ChlngsNoWtr)
  data$ChlngsSmels = ifelse(is.na(data$Chlngs), NA,
                            ifelse(grepl("Smels", data$Chlngs,
                                         fixed = TRUE), "1", "0"))
  data$ChlngsSmels = as.factor(data$ChlngsSmels)
  # summary(data$ChlngsSmels)
  data$ChlngsOK = ifelse(is.na(data$Chlngs), NA,
                         ifelse(grepl("OK", data$Chlngs,
                                      fixed = TRUE), "1", "0"))
  data$ChlngsOK = as.factor(data$ChlngsOK)
  # summary(data$ChlngsOK)                                # After
  
  # Create collapsed satisfaction variables for latrine and supplier
  data$SatisColaps = recode(data$Satis, "'DK' = 'DK'; 1 = 1; 2 = 1; 3 = 2;
                            4 = 3; 5 = 3")
  data$SatisSupColaps = recode(data$SatisSup, "'DK' = 'DK'; 1 = 1; 2 = 1; 
                               3 = 2; 4 = 3; 5 = 3")
  data$SatisColapsMore = recode(data$Satis, "'DK' = 'DK'; 1 = 1; 2 = 1; 3 = 1;
                                4 = 2; 5 = 2")
  data$SatisSupColapsMore = recode(data$SatisSup, "'DK' = 'DK'; 1 = 1; 2 = 1; 
                                   3 = 1; 4 = 2; 5 = 2")
  
  # Drop empty levels from data
  data = droplevels(data)
  
  #############################################################################
  # SAVE DATA TO DISK
  #############################################################################
  save(data,
       file = 
         paste(getwd(),"/data/raw/surveys/",
               "describe_FSMintentions_regional_seasonal_iDE_Oct2017.RData", 
               sep = ""))
  
}
cat("\014")                                              # Clear console window
summary(data)
names(data)
print(sapply(data, function(x) sum(is.na(x))))
# missmap(data, main = "Missing Values in Variables", legend = F)
###############################################################################
# Multiple Correspondence Analysis
# Select relevant data
names(data)
select = c("Prov", "IDPoor", "LivRP", "VillOD", 
           "FreqNeiToi", "WhoInstLat", "KnwSubsdy", "RecSubsdy", "BorwLat", 
           "SlabTil", "WallMat", "RoofMat", 
           "AdltUseLat", "ChldUseLat", "InfLatDump", "Yr", "Mnth", 
           "RDefBefor_BshFld", "RDefBefor_RivPnd", "RDefBefor_NeiToi",
           "IntndPitFull", 
           "ChlngsNoFlsh", "ChlngsFlood", "ChlngsOthr", "ChlngsFulOvrFlw", 
           "ChlngsNoWtr", "ChlngsSmels", "ChlngsOK",
           "SatisColapsMore", "SatisSupColapsMore", "Rec", "RecSup",
           "M01", "M1824", "M217", "M2559", "M60", "F01", "F1824", "F217", 
           "F2559", "F60")
exclude = c("IntndChng_Shltr", "IntndChng_Shwr", 
            "IntndChng_Sink", "IntndChng_WtrRes", "IntndChng_Pit",
            "IntndChng_Othr", "IntndChng_NAAlwysToi",
            "RDefBefor_Othr", "RDefBefor_NAAlwysToi")
data.mca = subset(data, select = select)
data.mca = subset(data.mca, !is.na(IntndPitFull))
# Summarize data, visualize missing data, and modify as needed
par(mfrow = c(2,2))
for (i in 1:length(data.mca)) {
  if (i %% 4 == 0) {par(mfrow = c(2,2))}
  plot(data.mca[,i], main = colnames(data.mca)[i],
       ylab = "Count", col = "steelblue", las = 2)
}
summary(data.mca, maxsum = 12)
levels(data.mca$Prov) = c("BM", "KT", "K", "OM", "PV", "SR", "SG")
levels(data.mca$IDPoor) = c("NotIDPoor", "IDPoor")
colnames(data.mca)[which(names(data.mca) == "LivRP")] = "LivWtr"
data.mca$LivWtr = recode(data.mca$LivWtr, "'No' = 'NoLivWtr'; 
                         'Pond' = 'LivWtr'; 'Rivr' = 'LivWtr'")
levels(data.mca$VillOD) = c("MostVillOD", "NoVillOD" ,"SomeVillOD")
levels(data.mca$FreqNeiToi) = c("FreqNeiToi", "NoNeiToi" ,"AlwysToi",
                                "SomeNeiToi")
levels(data.mca$WhoInstLat) = c("Slf/FamInstLat", "GovInstLat",
                                "FrndInstLat", "MasonInstLat",
                                "LBOInstLat", "NGOInstLat",
                                "DKWhoInstLat")
data.mca$WhoInstLat[is.na(data.mca$WhoInstLat)] = "DKWhoInstLat"
levels(data.mca$KnwSubsdy) = c("KnwSubsdy", "DKSubsdy")
levels(data.mca$RecSubsdy) = c("UnkSubsdy", "NoSubsdy", "FulSubsdy", "PrtSubsdy")
levels(data.mca$BorwLat) = c("Borw", "NoBorw", "NoBorw", "UnkBorw")
data.mca$BorwLat[is.na(data.mca$BorwLat)] = "UnkBorw"
levels(data.mca$SlabTil) = c("NoSlabTil", "SlabTil", "UnkSlabTil")
data.mca$SlabTil[is.na(data.mca$SlabTil)] = "UnkSlabTil"
levels(data.mca$WallMat) = c("WoodWal", "MasnWal", "StelWal", "NoWal",
                             "OthrWal", "PlstcWal", "WoodWal", "UnkWal")
data.mca$WallMat[is.na(data.mca$WallMat)] = "UnkWal"
levels(data.mca$RoofMat) = c("NoRoof", "PlstcRoof", "StelRoof", "WoodRoof",
                             "MasnRoof", "OthrRoof", "OthrRoof", "WoodRoof",
                             "UnkRoof")
data.mca$RoofMat[is.na(data.mca$RoofMat)] = "UnkRoof"
levels(data.mca$AdltUseLat) = c("AdltFreqLat", "AdltDKLat", "AdltRarLat", 
                                "AdltSomLat")
data.mca$AdltUseLat[is.na(data.mca$AdltUseLat)] = "AdltDKLat"
levels(data.mca$ChldUseLat) = c("ChldFreqLat", "ChldDKLat", "ChldDKLat", 
                                "ChldRarLat", "ChldSomLat")
data.mca$ChldUseLat[is.na(data.mca$ChldUseLat)] = "ChldDKLat"
levels(data.mca$InfLatDump) = c("InfFreqLat", "InfDKLat", "InfDKLat", 
                                "InfRarLat", "InfSomLat")
data.mca$InfLatDump[is.na(data.mca$InfLatDump)] = "InfDKLat"
data.mca$ODBefor = 0
data.mca$ODBefor =
  ifelse(data.mca$RDefBefor_BshFld == 1 | data.mca$RDefBefor_RivPnd == 1, 1, 0)
data.mca$ODBefor = as.factor(data.mca$ODBefor)
levels(data.mca$ODBefor) = c("NoODBefor", "ODBefor")
levels(data.mca$RDefBefor_NeiToi) = c("NoDefBeforNeiToi", "DefBeforNeiToi")
levels(data.mca$ChlngsNoFlsh) = c("FlshOK", "NoFlsh", "DKFlsh")
data.mca$ChlngsNoFlsh[is.na(data.mca$ChlngsNoFlsh)] = "DKFlsh"
levels(data.mca$ChlngsFlood) = c("NoFlood", "Flood", "DKFlood")
data.mca$ChlngsFlood[is.na(data.mca$ChlngsFlood)] = "DKFlood"
levels(data.mca$ChlngsOthr) = c("NoOthrChlngs", "OthrChlngs", "DKOthrChlngs")
data.mca$ChlngsOthr[is.na(data.mca$ChlngsOthr)] = "DKOthrChlngs"
levels(data.mca$ChlngsFulOvrFlw) = c("NoFulOvrflw", "FulOvrflw", "DKFulOvrflw")
data.mca$ChlngsFulOvrFlw[is.na(data.mca$ChlngsFulOvrFlw)] = "DKFulOvrflw"
levels(data.mca$ChlngsNoWtr) = c("WtrOK", "NoWtr", "DKWtr")
data.mca$ChlngsNoWtr[is.na(data.mca$ChlngsNoWtr)] = "DKWtr"
levels(data.mca$ChlngsSmels) = c("NoSmel", "Smel", "DKSmel")
data.mca$ChlngsSmels[is.na(data.mca$ChlngsSmels)] = "DKSmel"
levels(data.mca$ChlngsOK) = c("Chlngs", "NoChlngs", "DKChlngs")
data.mca$ChlngsOK[is.na(data.mca$ChlngsOK)] = "DKChlngs"
colnames(data.mca)[which(names(data.mca) == "SatisColapsMore")] = "Satis"
levels(data.mca$Satis) = c("UnsatLat", "SatLat", "DKSatLat")
data.mca$Satis[is.na(data.mca$Satis)] = "DKSatLat"
colnames(data.mca)[which(names(data.mca) == "SatisSupColapsMore")] = "SatisSup"
levels(data.mca$SatisSup) = c("UnsatSup", "SatSup", "DKSatSup")
data.mca$SatisSup[is.na(data.mca$SatisSup)] = "DKSatSup"
levels(data.mca$Rec) = c("NoRecLat", "RecLat", "DKRecLat")
data.mca$Rec[is.na(data.mca$Rec)] = "DKRecLat"
levels(data.mca$RecSup) = c("NoRecSup", "RecSup")
data.mca = subset(data.mca, WhoInstLat != "GovInstLat")
data.mca = subset(data.mca, !is.na(KnwSubsdy))
data.mca = subset(data.mca, Yr != 2014)
data.mca = subset(data.mca, select = -c(RDefBefor_BshFld, RDefBefor_RivPnd))
data.mca = subset(data.mca, !is.na(RecSup))
data.mca = subset(data.mca, ChlngsOK != "DKChlngs")
data.mca = subset(data.mca, SlabTil != "UnkSlabTil")
data.mca = subset(data.mca, RoofMat != "UnkRoof")
data.mca = subset(data.mca, AdltUseLat != "AdltDKLat")
data.mca = subset(data.mca, Rec != "DKRecLat")
data.mca = subset(data.mca, BorwLat != "UnkBorw")
data.mca$F217[is.na(data.mca$F217)] = 
  mean(data.mca$F217[!is.na(data.mca$F217)])
data.mca$M1824[is.na(data.mca$M1824)] = 
  mean(data.mca$M1824[!is.na(data.mca$M1824)])
data.mca = droplevels(data.mca)
summary(data.mca, maxsum = 12)
print(sapply(data.mca, function(x) sum(is.na(x))))
# Run analysis
varTable(data.mca)
quali.sup = c(1, 16:17)
quanti.sup = c(31:40)
res.mca = MCA(data.mca, quanti.sup = quanti.sup, quali.sup = quali.sup,
              na.method = "NA", graph = FALSE)
# Print and plot results
par(mfrow = c(1, 1))
print(res.mca)
get_eigenvalue(res.mca)
fviz_screeplot(res.mca, addlabels = TRUE, ylim = c(0, 50))
fviz_mca_biplot(res.mca, repel = FALSE, ggtheme = theme_minimal())
res.mca.vars = get_mca_var(res.mca)
fviz_mca_var(res.mca, choice = "mca.cor", repel = TRUE,      # Cor, vars & dims
             ggtheme = theme_minimal())
fviz_mca_var(res.mca, repel = TRUE, ggtheme = theme_minimal())
fviz_mca_var(res.mca, col.var = "black", shape.var = 15, repel = TRUE)
head(round(res.mca.vars$coord, 2))
head(round(res.mca.vars$coord, 2))
head(round(res.mca.vars$cos2, 4))
corrplot(res.mca.vars$cos2, is.corr = FALSE)
fviz_mca_var(res.mca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE,
             ggtheme = theme_minimal())
fviz_mca_var(res.mca, alpha.var = "cos2",
             repel = TRUE,
             ggtheme = theme_minimal())
fviz_cos2(res.mca, choice = "var", axes = 1:2)
head(round(res.mca.vars$contrib, 3))
fviz_contrib(res.mca, choice = "var", axes = 1, top = 15)
fviz_contrib(res.mca, choice = "var", axes = 2, top = 15)
fviz_contrib(res.mca, choice = "var", axes = 1:2, top = 15)
fviz_mca_var(res.mca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, 
             ggtheme = theme_minimal())
fviz_mca_var(res.mca, alpha.var = "contrib",
             repel = TRUE,
             ggtheme = theme_minimal())
res.mca.ind = get_mca_ind(res.mca)
print(res.mca.ind)
head(res.mca.ind$coord)
head(res.mca.ind$contrib)
head(res.mca.ind$cos2)
# fviz_mca_ind(res.mca, col.ind = "cos2", 
#              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
#              repel = TRUE,
#              ggtheme = theme_minimal())
fviz_mca_ind(res.mca, 
             label = "none", # hide individual labels
             habillage = data.mca$Prov, # color by groups 
             # palette = c("#00AFBB", "#E7B800"),
             addEllipses = TRUE, ellipse.type = "confidence",
             ggtheme = theme_minimal())


summary(res.mca, ncp = 3, nbelements = Inf)
dimdesc(res.mca)
plot(results, label = c("var","quali.sup"), cex = 0.7)
plot(results, invisible = c("var","quali.sup"), cex = 0.7)
plot(results, invisible = c("ind","quali.sup"), autoLab = "y", cex = 0.7, title = "Active Categories")
plot(results, invisible = c("ind","quali.sup"), autoLab = "y", cex = 0.7, title = "Active Categories", selectMod = "contrib 20")
plot(results, invisible = c("ind","quali.sup"), cex = 0.7, title = "Active Categories")
plot(results, invisible = c("ind","var"), autoLab = "y", cex = 0.7, title = "Supplementary Categories")
plot(results, invisible = "ind", autoLab = "y", cex = 0.7, selectMod = "cos2 10")
plot(results, invisible = "ind", autoLab = "y", cex = 0.7, selectMod = "contrib 20")
plot(results, invisible = c("var","quali.sup"), autoLab = "y", cex = 0.7, select = "cos2 10")
plot(results, autoLab = "y", cex = 0.7, selectMod = "cos2 20", select = "cos2 10")
plot(results, choix = "var", xlim = c(0,0.6), ylim = c(0,0.6))
plot(results, choix = "var", xlim = c(0,0.6), ylim = c(0,0.6), invisible = c("ind","quali.sup"))
plot(results, invisible = c("var","quali.sup"), cex = 0.7, select = "contrib 20", axes = 3:4)
plot(results, invisible = c("ind"), cex = 0.7, select = "contrib 20", axes = 3:4)
plotellipses(results, keepvar = c(1:4))


# Exploratory Factor Analysis (EFA)
names(data)
include = c("Prov", "IDPoor", "LivRP", "VillOD", 
            "FreqNeiToi", "WhoInstLat", "KnwSubsdy", "RecSubsdy", "BorwLat", 
            "SlabTil", "WallMat", "RoofMat", 
            "AdltUseLat", "ChldUseLat", "InfLatDump", "Yr", "Mnth", 
            "RDefBefor_BshFld", "RDefBefor_RivPnd", "RDefBefor_NeiToi", 
            "RDefBefor_Othr", "RDefBefor_NAAlwysToi", 
            "IntndChng_Shltr", "IntndChng_Shwr", 
            "IntndChng_Sink", "IntndChng_WtrRes", "IntndChng_Pit",
            "IntndChng_Othr", "IntndChng_NAAlwysToi", 
            "IntndPitFull", 
            "ChlngsNoFlsh", "ChlngsFlood", "ChlngsOthr", "ChlngsFulOvrFlw", 
            "ChlngsNoWtr", "ChlngsSmels", "ChlngsOK",
            "Satis", "Rec", "SatisSup", "RecSup",
            "M01", "M1824", "M217", "M2559", "M60", "F01", "F1824", "F217", 
            "F2559", "F60")
data.efa = subset(data, select = include)
num.cols = length(data.efa)
varTable(data.efa)
make_binary = c("Prov", "IDPoor", "VillOD", "Yr", "Mnth", "IntndPitFull",
                "Satis", "Rec", "SatisSup", "RecSup")
data.efa.binary = dummy_cols(data.efa, select_columns = make_binary)
data.efa.binary = data.efa.binary[-c(1:num.cols)]
data.efa.binary = data.efa.binary[c(1:1000),]
varTable(data.efa.binary)
data.efa.binary.factors = sapply(data.efa.binary, as.factor)
head(data.efa.binary.factors)
het.mat = hetcor(data.efa.binary.factors)
# not working yet...








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
