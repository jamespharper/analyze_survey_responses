# Analysis of Survey Responses of Rural Cambodian Latrine Owners
# Written by James Harper, PE, ENV SP of the University of Colorado Boulder
# Started October 1, 2017
# Last updated June 9, 2018

###############################################################################
# INITIALIZE, LOAD DATA AND CLEAN DATA
###############################################################################
rm(list = ls())                                      # Clear global environment
cat("\014")                                          # Clear console window
file.remove(dir(paste(getwd(),"/output/", sep = ""), # Clear output folder
                full.names = TRUE))    
source("functions.R")
load_libraries(c("rio", "gmodels", "vcd", "gtools",  # Install & load libraries
                 "ca", "extracat", "iplots", 
                 "FactoMineR", "gplots", "factoextra",
                 "corrplot", "ggpubr", "rgl", "missMDA",
                 "pscl", "ltm", "Amelia", "ROCR", "extrafont",
                 "DescTools", "sp"))
loadfonts(device = "win")
load(file = "iDE_May2018.Rdata")
# data = subset(data, select = -c(IntndChngDich, IntndChng, IntndChng_Shltr,
#                                 IntndChng_Shwr, IntndChng_Sink, IntndChng_Pit,
#                                 IntndChng_WtrRes, IntndChng_Othr,
#                                 IntndChng_NAAlwysToi, RDefBefor))

# Characterize data, focusing on NAs
# summary(data2)
# names(data2)
# freqs.2way(data = data, metric1 = "Vill", metric2 = "Group2")
# print(sapply(data2, function(x) sum(is.na(x))))
# missmap(data2, main = "Missing Values in Variables", legend = F)

# Remove rows missing EmptyPlan response
# data2 = subset(data2, !is.na(EmptyPlan))
# print(sapply(data, function(x) sum(is.na(x))))
# missmap(data[1:35], main = "Missing Values in Variables", legend = F)
summary(data1)
summary(data2)


library(plyr)



# Plot EmptyPlanMethds by Group2
levels(data2$EmptyPlanMethds) = c("Bucket", "DK", "FSMServProvdr", 
                                  "Bucket", "Pump", "FSMServProvdr")
data2.control = subset(data2, Group2 == "Cntrl" & !is.na(EmptyPlanMethds))
data2.treatNei = subset(data2, Group2 == "TreatNei" & !is.na(EmptyPlanMethds))
data2.treatPG = subset(data2, Group2 == "TreatPG" & !is.na(EmptyPlanMethds))
data_plot = c(summary(data2.control$EmptyPlanMethds)/length(data2.control[,1]),
              summary(data2.treatNei$EmptyPlanMethds)/length(data2.treatNei[,1]),
              summary(data2.treatPG$EmptyPlanMethds)/length(data2.treatPG[,1]))
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
                   breaks = c("Control", "Neighbors of Treatment", "Treatment")) +
  scale_y_continuous(name = "Percentage of rural latrine owners", 
                     breaks = round(seq(0.0, 1.0, 0.1), 2), 
                     limits = c(0.0, 1.0),
                     labels = paste(as.character(seq(0, 100, 10)), "%", sep = "")) +
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
data = data2
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
data = data2
freqs.1way(data = data, metric1 = "PiercdPit")
freqs.1way(data = data, metric1 = "FSMServProvdrs")
freqs.1way(data = data, metric1 = "EmptyBefor")
freqs.1way(data = data, metric1 = "EmptyWho")

data = subset(data2, EmptyWilPay != "DK")
data = droplevels(data)
summary(data$EmptyWilPay)
length(data$EmptyWilPay[data$EmptyWilPay == 0])/length(data$EmptyWilPay)
summary(as.numeric(as.character(data$EmptyWilPay[data$EmptyWilPay != 0])))/4054.50

data = subset(data2, EmptyCost != "DK")
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

data = subset(data2, EmptyCost != "DK" & EmptyCost != 0)
summary(as.numeric(as.character(data$EmptyCost)))/4054.50

###############################################################################
# ANALYZE DATA BY INTERVIEWERS
###############################################################################
# Do EmptyPlan or EmptyMethods vary between interviewers? Yes.
data = data2
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
data = subset(data2, Intrviewr == "Meak Saran")
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
data = subset(data2, Intrviewr == "Pen Senkosal")
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
coords = cbind(Longitude = as.numeric(as.character(data2$DistLong)), 
               Latitude = as.numeric(as.character(data2$DistLat)))
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

# ALL FACTORS that showed statistically significant and important associations with the desirability of FSM intentions
# Removed Dist because it created too many errors
# IntndPitFullDes ~ Prov + Yr + Mnth + IDPoor + RDefBefor_BshFld + FreqNeiToi + InfLatDump + ChlngsFlood + ChlngsNoWtr + Satis + SatisSup + Rec + RecSup + Rain.mm
# LESS FACTORS WITH LOW P IN ABOVE MODEL: Rec, ChlngsNoWtr, Rain.mm, FreqNeiToi
print(results)
print(summary(results[[2]]))
print(anova(results[[2]], test = "Chisq"))
# FINAL MODEL
results = genlinmod(data = data.sub, 
                    formula = paste("IntndPitFullDes ~ Prov + Yr + Mnth + IDPoor +",
                                    "RDefBefor_BshFld + ",
                                    "ChlngsFlood + ",
                                    "Satis + SatisSup + RecSup"),
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
freqs_2way_IntndPitFullEmpSlf_Yr = freqs.2way(data.sub, "IntndPitFullEmpSlf", "Yr", 1)
freqs_2way_IntndPitFullPit_Yr = freqs.2way(data.sub, "IntndPitFullPit", "Yr", 1)
freqs_2way_IntndPitFullOthr_Yr = freqs.2way(data.sub, "IntndPitFullOthr", "Yr", 1)
freqs_2way_IntndPitFullPay_Yr = freqs.2way(data.sub, "IntndPitFullPay", "Yr", 1)
freqs_2way_IntndPitFullStop_Yr = freqs.2way(data.sub, "IntndPitFullStop", "Yr", 1)
crosstable_IntndPitFull_Yr = CrossTable(freqs_2way_IntndPitFull_Yr[[2]])
print(crosstable_IntndPitFull_Yr)
barplot(crosstable_IntndPitFull_Yr$prop.col, beside = T,
        col = c(1:6), family = "serif", axes = T,
        legend.text = c("Don't know", "Empty myself", "Install new pit",
                        "Other", "Pay someone", "Stop using latrine"), 
        args.legend = list(x = "topright", bty = "n"))
df = data.frame(IntndPitFull = crosstable_IntndPitFull_Yr$prop.col)
ggplot(df, aes(x = IntndPitFull.B, y = IntndPitFull.Freq, group = IntndPitFull.A)) + 
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
df$Desir = rep(x = c("Des", "Des", "Undes", "Undes", "Undes", "Undes"), times = 12)
ggplot(df, aes(x = Mnth, y = Prop, fill = IntndPitFull)) +
  geom_col() + 
  coord_flip()
windowsFonts()
title = "                       Desirable                                             Undesirable"
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
                     labels = paste(as.character(abs(seq(-80, 80, 10))), "%", sep = "")) +
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
freqs_2way_IntndPitFullEmpSlf_IDPoor = freqs.2way(data, "IntndPitFullEmpSlf", "IDPoor", 1)
freqs_2way_IntndPitFullPit_IDPoor = freqs.2way(data, "IntndPitFullPit", "IDPoor", 1)
freqs_2way_IntndPitFullOthr_IDPoor = freqs.2way(data, "IntndPitFullOthr", "IDPoor", 1)
freqs_2way_IntndPitFullPay_IDPoor = freqs.2way(data, "IntndPitFullPay", "IDPoor", 1)
freqs_2way_IntndPitFullStop_IDPoor = freqs.2way(data, "IntndPitFullStop", "IDPoor", 1)
crosstable_IntndPitFull_IDPoor = CrossTable(freqs_2way_IntndPitFull_IDPoor[[2]])
print(crosstable_IntndPitFull_IDPoor)
barplot(crosstable_IntndPitFull_IDPoor$prop.col, beside = T,
        col = c(1:6), family = "serif", axes = T,
        legend.text = c("Don't know", "Empty myself", "Install new pit",
                        "Other", "Pay someone", "Stop using latrine"), 
        args.legend = list(x = "topright", bty = "n"))
df = data.frame(IntndPitFull = crosstable_IntndPitFull_IDPoor$prop.col)
ggplot(df, aes(x = IntndPitFull.B, y = IntndPitFull.Freq, group = IntndPitFull.A)) + 
  geom_line(aes(color = IntndPitFull.A))

# By gender
freqs_2way_IntndPitFull_CGend = freqs.2way(data, "IntndPitFull", "CGend", 1)
freqs_2way_IntndPitFullDK_CGend = freqs.2way(data, "IntndPitFullDK", "CGend", 1)
freqs_2way_IntndPitFullEmpSlf_CGend = freqs.2way(data, "IntndPitFullEmpSlf", "CGend", 1)
freqs_2way_IntndPitFullPit_CGend = freqs.2way(data, "IntndPitFullPit", "CGend", 1)
freqs_2way_IntndPitFullOthr_CGend = freqs.2way(data, "IntndPitFullOthr", "CGend", 1)
freqs_2way_IntndPitFullPay_CGend = freqs.2way(data, "IntndPitFullPay", "CGend", 1)
freqs_2way_IntndPitFullStop_CGend = freqs.2way(data, "IntndPitFullStop", "CGend", 1)
crosstable_IntndPitFull_CGend = CrossTable(freqs_2way_IntndPitFull_CGend[[2]])
print(crosstable_IntndPitFull_CGend)
barplot(crosstable_IntndPitFull_CGend$prop.col, beside = T,
        col = c(1:6), family = "serif", axes = T,
        legend.text = c("Don't know", "Empty myself", "Install new pit",
                        "Other", "Pay someone", "Stop using latrine"), 
        args.legend = list(x = "topright", bty = "n"))
df = data.frame(IntndPitFull = crosstable_IntndPitFull_CGend$prop.col)
ggplot(df, aes(x = IntndPitFull.B, y = IntndPitFull.Freq, group = IntndPitFull.A)) + 
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
order2 = c("Oddar Meanchey", "Banteay Meanchey", "Siem Reap", "Kampong Thom", "Kandal", "Prey Veng", 
           "Svay Rieng")
prop.col = prop.col[,match(order2, colnames(prop.col)),drop = FALSE]
df = data.frame(IntndPitFull = prop.col)
names(df) = c("IntndPitFull", "Prov", "Prop")
df$Desir = rep(x = c("Des", "Des", "Undes", "Undes", "Undes", "Undes"), times = 7)
ggplot(df, aes(x = Prov, y = Prop, fill = IntndPitFull)) +
  geom_col() + 
  coord_flip()
title = "                     Desirable                                    Undesirable"
data.sub.des = subset(df, Desir == "Des")
data.sub.undes = subset(df, Desir == "Undes")
perc_des = round(data.sub.des$Prop[data.sub.des$IntndPitFull == "Pay"] +
                   data.sub.des$Prop[data.sub.des$IntndPitFull == "Pit"], 2)*100
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
                     labels = paste(as.character(abs(seq(-80, 80, 10))), "%", sep = "")) +
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
  #   # correspondence(data = sub, metric1 = pairs[num,1], metric2 = pairs[num,2], 
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
