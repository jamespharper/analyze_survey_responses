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
                 "corrplot", "ggpubr", "rgl", "missMDA"))

###############################################################################
# LOAD, CLEAN AND CATEGORIZE DATA
###############################################################################
# Load data file
load(file = "iDE_Oct2017.RData")
# file_to_import_mca = paste(getwd(), "/data/data_mca_noNAs.xlsx", sep = "")
# data = import(file_to_import_mca)
# data = read.table(file_to_import_mca, header = TRUE, sep = ",")
# names(data)[1] = "IntndPitFull"
# data(tea)
# data(poison)
# head(poison, 3)

# Remove NAs and low-freq responses as needed
sapply(data, function(x) sum(is.na(x)))
sapply(data, function(x) length(which(!is.na(x))))
data = subset(data, !is.na(IntndPitFull))
data = subset(data, !is.na(IntndChng_Shltr))
data$Satis[is.na(data$Satis)] = round(mean(as.numeric(data$Satis), na.rm = T), digits = 0)
data$Satis[data$Satis == "DK"] = round(mean(as.numeric(data$Satis), na.rm = T), digits = 0)
data$Satis = factor(data$Satis)
data$SatisSup[data$SatisSup == "DK"] = round(mean(as.numeric(data$SatisSup), na.rm = T), digits = 0)
data$SatisSup = factor(data$SatisSup)
data$RecSup[is.na(data$RecSup)] = sample(c("Yes", "No"), 1, replace = TRUE)
data$AdltUseLat[is.na(data$AdltUseLat)] = "Freq"
data$AdltUseLat = factor(data$AdltUseLat)
data$ChldUseLat[is.na(data$ChldUseLat)] = "DK/NA"
data$ChldUseLat[data$ChldUseLat == "NoChld"] = "DK/NA"
data$ChldUseLat = factor(data$ChldUseLat)
data$InfLatDump[is.na(data$InfLatDump)] = "DK/NA"
data$InfLatDump[data$InfLatDump == "NoInf"] = "DK/NA"
data$InfLatDump = factor(data$InfLatDump)

# Collapse satisfaction variables to binary
summary(data$Satis)
levels(data$Satis) = c("3", "2", "4", "1", "5", "Sat", "Unsat")
data$Satis[data$Satis == "5" | data$Satis == "4"] = "Sat"
data$Satis[data$Satis == "3" | data$Satis == "2" | data$Satis == "1"] = "Unsat"
data$Satis = factor(data$Satis)
summary(data$Satis)
summary(data$SatisSup)
levels(data$SatisSup) = c("3", "2", "4", "1", "5", "Sat", "Unsat")
data$SatisSup[data$SatisSup == "5" | data$SatisSup == "4"] = "Sat"
data$SatisSup[data$SatisSup == "3" | data$SatisSup == "2" | data$SatisSup == "1"] = "Unsat"
data$SatisSup = factor(data$SatisSup)
summary(data$SatisSup)

# Categorize data as active or supplementary (qualitative or quantitative)
names(data)
summary(data)
data = subset(data, select = -c(LBO, IntndPitFullDes, RDefBefor_Othr, 
                                RDefBefor_NAAlwysToi, IntndChng_Othr, 
                                IntndChng_NAAlwysToi, Rec))
# May want to remove RDefBefor_RivPnd due to low freq
# Removed Rec due to nearly equal freqs
# Active data = beliefs, intentions, or behaviors related to sanitation
indices.active = match(c("AdltUseLat", "ChldUseLat", "InfLatDump", 
                         "IntndPitFull", "Satis", "SatisSup", 
                         "RecSup", "RDefBefor_BshFld", 
                         "RDefBefor_RivPnd", "RDefBefor_NeiToi",
                         "IntndChng_Shltr", "IntndChng_Shwr",
                         "IntndChng_HndWsh", "IntndChng_WtrRes",
                         "IntndChng_2pit"), names(data))
indices.sup.quali = match(c("Prov", "CGend", "IDPoor", "LivRP", "VillOD", "Yr",
                            "Mnth"), names(data))
indices.sup.quanti = 0

###############################################################################
# PERFORM MULTIPLE CORRESPONDENCE ANALYSIS 
###############################################################################
# Summarize data
summary(data[indices.active])
summary(data[indices.sup.quali])
summary(data[indices.sup.quanti])
for (i in 1:length(data)) {
  plot(data[, i], main = colnames(data)[i],
       ylab = "Count", col = "steelblue")
}

# Perform multiple correspondence analysis
results = MCA(X = data, quali.sup = indices.sup.quali,
              graph = T,
              na.method = "NA")
print(results)
summary(results)
summary(results, ncp = 3, nbelements = Inf)
dimdesc(results)

# Generate plots
# Categories and Individuals
plot.MCA(results, choix = "ind", title = "Categories and Individuals", cex = 0.7, label = c("var"))
plot.MCA(results, autoLab = "y", cex = 0.7, selectMod = "cos2 20", select = "cos2 10")
# Variables
plot.MCA(results, choix = "var", title = "Variables and Supplementary Variables", autoLab = "no", cex = 0.7)
# Categories
plot.MCA(results, invisible = c("ind", "quanti.sup", "quali.sup"), cex = 0.7, title = "Responses/Categories", autoLab = "no")
plot.MCA(results, invisible = c("ind", "quanti.sup", "quali.sup"), cex = 0.7, title = "Responses/Categories", autoLab = "no", selectMod = "contrib 20")
plot.MCA(results, invisible = c("ind", "quanti.sup", "quali.sup"), cex = 0.7, title = "Responses/Categories", autoLab = "no", selectMod = "cos2 10")

# Individuals
plot.MCA(results, invisible = c("var", "quanti.sup", "quali.sup"), cex = 0.7, title = "Individuals")
plot.MCA(results, invisible = c("var", "quanti.sup", "quali.sup"), cex = 0.7, title = "Individuals", select = "cos2 10")
plot.MCA(results, invisible = c("var", "quanti.sup", "quali.sup"), cex = 0.7, select = "contrib 20", axes = 3:4)
# Supplementary Variables
plot.MCA(results, invisible = c("var", "ind", "quali.sup"), cex = 0.7, autoLab = "n", title = "Quantitative Supplementary Variables")
plot.MCA(results, invisible = c("var", "ind", "quanti.sup"), cex = 0.7, autoLab = "n", title = "Qualitative Supplementary Variables")











plot(results, invisible = c("ind"), cex = 0.7, select = "contrib 20", axes = 3:4)
plotellipses(results, keepvar = c(1:4))

# Impute data due to missing data (NOT WORKING)
# require(missMDA)
# data(vnf)
# completed = imputeMCA(vnf, ncp = 2)
# res.mca = MCA(vnf)
# res.mca = MCA(vnf,tab.disj=completed$tab.disj)
# imputed = imputeMCA(don = data.active, ncp = 2)
# results = MCA(X = data.active, 
#               quali.sup = data.sup.quali,
#               quanti.sup = data.sup.quanti,
#               tab.disj = imputed$tab.disj)
# results = MCA(tea, quanti.sup = 19, quali.sup = c(20:36))

# Tea example
data(tea)
res.mca = MCA(tea, quanti.sup=19, quali.sup=c(20:36))
plot.MCA(res.mca, invisible=c("var","quali.sup"), cex=0.7)
plot.MCA(res.mca, invisible=c("ind","quali.sup"), cex=0.7)
plot.MCA(res.mca, invisible=c("ind"))
plot.MCA(res.mca, invisible=c("ind", "var"))