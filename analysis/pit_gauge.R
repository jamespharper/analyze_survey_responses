# Pit Gauge Study, May 2017 to June 2018
# Analyze Survey Responses of Rural Cambodian Latrine Owners
# Written by James Harper, PE, ENV SP of the University of Colorado Boulder
# Started October 1, 2017; Last updated Dec 18, 2018
###############################################################################
# Initialize environment and user input
rm(list = ls())                                      # Clear global environment
cat("\014")                                              # Clear console window
source("functions.R")                                   # Load custom functions
load_libraries(                                      # Install & load libraries
  c("rio", "gplots", "Amelia", "car", "fastDummies", "FactoMineR", "lavaan",
    "extrafont", "gmodels", "vcd", "ltm", "MASS", "ggpubr", "dplyr", "CDFt",
    "fitdistrplus", "goft", "flexsurv", "reldist"))
loadfonts(device = "win")
###############################################################################
# Load cleaned data (If need to clean raw data, open pit_gauge-clean_data.R)
load(file = paste(getwd(),"/data/raw/surveys/pit_gauge/pit_gauge.RData",
                  sep = ""))
###############################################################################
# Summarize Data
###############################################################################
# Display data to aid in analysis
d.ADPsales.PG
d.ADPsales.villtyp
summary(d.baseline)
prop.table(table(d.baseline$LatOwnrIDPoor))
summary(as.numeric(as.character(d.baseline$NumPplHH)))
sd(as.numeric(as.character(d.baseline$NumPplHH)))
prop.table(table(d.baseline$UseLatReg))
prop.table(table(d.baseline$EmptyBefor))
prop.table(table(d.baseline$WhyNoEmpty))
prop.table(table(d.baseline$EmptyMethds))
prop.table(table(d.baseline$PiercdPit))
summary(as.numeric(as.character(d.baseline$LatAge[d.baseline$LatAge != 2018])))
sd(as.numeric(as.character(d.baseline$LatAge[d.baseline$LatAge != 2018 &
                                               !is.na(d.baseline$LatAge)])))
prop.table(table(d.baseline$Job))
# 77% not IDPoor
# 4.9 ppl in HH mean, max = 14, sd = 2.0
# 98% use lat reg
# 16% emptied before, 90% self-emptied with bucket, 9% self-emptied with pump
# 96% hadn't emptied before because not full
# 10% had pierced pit
# 4.6 year old lat mean, max = 33, sd = 3.3
summary(d.followup)
prop.table(table(d.followup$IDPoorTyp))
summary(as.numeric(as.character(d.followup$NumPplHH)))
sd(as.numeric(as.character(d.followup$NumPplHH[!is.na(d.followup$NumPplHH)])))
summary(as.numeric(as.character(d.followup$NumPplHHLatUsr)))
sd(as.numeric(as.character(d.followup$NumPplHHLatUsr[!is.na(d.followup$NumPplHH)])))
mean(
  as.numeric(as.character(
    d.followup$NumPplHHLatUsr[!is.na(d.followup$NumPplHH)])) /
    as.numeric(as.character(
      d.followup$NumPplHH[!is.na(d.followup$NumPplHH)])))
prop.table(table(d.followup$HousFlod))
prop.table(table(d.followup$RoadAccsTruck))
prop.table(table(d.followup$PiercdPit))
prop.table(table(d.followup$NumPits))
prop.table(table(d.followup$NumRngs))
prop.table(table(d.followup$NumRngs.Othr))
prop.table(table(d.followup$EmptyBefor))
summary(as.numeric(as.character(d.followup$EmptyWilPay[d.followup$EmptyWilPay != "DK"])))
sd(as.numeric(as.character(d.followup$EmptyWilPay[d.followup$EmptyWilPay != "DK" &
                                                    !is.na(d.followup$EmptyWilPay)])))
prop.table(table(d.followup$FSMServProvdrs))
prop.table(table(d.followup$FSMServProvdrs.Contact))
summary(d.followup$FSMServProvdrs.Contact)
prop.table(table(d.followup$FSMServProvdrs.Cost))
summary(d.followup$FSMServProvdrs.Cost)
avg.cost.per.ring = mean(c(10, 40/3, 25/3)); print(avg.cost.per.ring)
avg.flat.rate = mean(c(150, 50, 50000/4000)); print(avg.flat.rate)
# 88% not IDPoor
# 4.7 ppl in HH mean, min = 1, max = 19, sd = 2.2
# 4.1 ppl using lat mean, min = 1, max = 12, sd = 1.8
# 90% of household used latrine on average
# All lived by a field, none by river or pond
# 24% experienced flooding
# 89% had good road access
# 14% had pierced pits
# 88% had 1 pit, 12% had 2 pits
# 52% 80-cm rings
# 93% had 3-6 rings
# 18% emptied before
# WTP for emptying 16667 Riel mean, min = 0, max = 200000, sd = 40000
# 80% believed there were no service providers that served their region, 17%
#   did not know, 2% said yes. Of the yeses, 7 out of 14 knew how to contact
#   them, and 11 knew their prices for emptying ($10.56 per ring or $71 flat
#   rate on average).
summary(d.custmrsurvs.SvyRng)
summary(d.custmrsurvs.SvyRng.Rmduol)
summary(d.sldglvls)
# Knowledge of FSM Service Providers
subset(data.frame(A = d.followup$FSMServProvdrs,
                  B = d.followup$FSMServProvdrs.Contact,
                  C = d.followup$FSMServProvdrs.Cost), A == "Yes")
# Customer Survey data relevant to Pit Gauge Study
summary(d.custmrsurvs.SvyRng)
# 478 surveys from Province Svay Rieng
summary(d.custmrsurvs.SvyRng.Rmduol)
# Only 81 surveys from Dist. Rumduol
summary(d.custmrsurvs.SvyRng.Rmduol$Comm)  
# Only 10 surveys from Comm Chrung Popel and Muen Chey
###############################################################################
# Emptied Before vs. Never Emptied Before     ADDED TO FSM5 PRESENTATION
###############################################################################
summary(d.baseline)
summary(d.baseline$EmptyBefor)
summary(glm(EmptyBefor ~ LatOwnrIDPoor + NumPplHH + LatStartUseYr + VillTyp + 
              Job + PiercdPit,
            data = d.baseline, family = binomial(link = "logit"),
            na.action = na.omit))
# Treatment group much less likely to have emptied before 
#   (-1.16, p = 0.000).
# Older latrines less likely to be emptied before (-0.13, p = 0.000).
# Non-poor HHs trended with being emptied before (1.42, p = 0.06).
freqs = table(d.baseline$EmptyBefor, d.baseline$PiercdPit)
CrossTable(freqs); assocstats(freqs)
# Piercing pit did not change emptying before (p = 0.99).
freqs = table(d.baseline$EmptyBefor, d.baseline$HavIDPoorCard)
CrossTable(freqs); assocstats(freqs)
# IDPoor less likely to have emptied before (v = 0.11, p = 0.002). 
#   Contradicted below.
summary(d.followup)
summary(glm(EmptyBefor ~ IDPoorTyp + NumPplHH + NumPplHHLatUsr +
              HousFlod + RoadAccsTruck + LatStartUseDate + PiercdPit + NumPits + 
              NumRngs + PGInstld + EmptyPlan + VillTyp,
            data = d.followup, family = binomial(link = "logit"),
            na.action = na.omit))
# Compare to above linreg with d.baseline
# HH with PGs much less likely to have emptied before. Confirmed below.
freqs = table(d.followup$EmptyBefor, d.followup$VillTyp)
CrossTable(freqs); assocstats(freqs)
# More HHs in control group (26%) emptied before compared to treatment group
# (10%) and neighbors of treatment group (11%) (p = 0.000, v = 0.20).
freqs = table(d.followup$EmptyBefor, d.followup$PiercdPit)
CrossTable(freqs); assocstats(freqs)
# Piercing pit did not change emptying before (p = 0.31).
freqs = table(d.followup$EmptyBefor, d.followup$IDPoor)
CrossTable(freqs); assocstats(freqs)
# IDPoor status not associated with emptied before (v = 0.01, p = 0.82).
#   COntradicted above.
freqs = table(d.followup$EmptyBefor, d.followup$NumPplHH)
CrossTable(freqs); assocstats(freqs)
cor.test(d.followup$NumPplHH, as.numeric(d.followup$EmptyBefor))
# NumPplHH not associated with EmptyBefor (p = 0.13, R = 0.07)
summary(d.followup$NumPplHH)
summary(d.followup$NumPplHHLatUsr)
summary(d.followup$NumPits)
summary(d.followup$NumRngs.Othr)
cor.test(d.followup$NumPplHH, d.followup$NumPits)
cor.test(d.followup$NumPplHH, d.followup$NumRngs.Othr)
cor.test(d.followup$NumPplHHLatUsr, d.followup$NumPits)
cor.test(d.followup$NumPplHHLatUsr, d.followup$NumRngs.Othr)
# The number of rings and number of pits are generally not associated 
# with the number of ppl in the HH or using the lat.  WEIRD.
# NumPplHH trends negatively with num rings (p. = 0.09). WEIRD.
# Pits are not installed based on num ppl in HH or using latrine.
freqs = table(d.followup$EmptyBefor, d.followup$NumPits)
CrossTable(freqs); assocstats(freqs)
temp = subset(d.followup, !is.na(NumRngs.Othr) & !is.na(EmptyBefor))
biserial.cor(as.numeric(temp$NumRngs.Othr), 
             as.numeric(temp$EmptyBefor))
freqs = table(temp$EmptyBefor, temp$NumRngs.Othr)
CrossTable(freqs); assocstats(freqs)
# Number of pits not associated with emptying before.
# Fewer rings associated with empting before (p = 0.005, v = 0.20)
###############################################################################
# Emptied Before     ADDED TO FSM5 PRESENTATION
###############################################################################
dset = subset(d.followup, EmptyBefor == "Yes")
dset2 = subset(d.followup, EmptyBefor == "No")
length(dset$EmptyBefor) / (length(dset$EmptyBefor) + length(dset2$EmptyBefor))
summary(dset)
# Of the 98 respondents that have emptied before (18% of respondents that 
# answered if they emptied before or not),
# No household was near a river or pond.
# Every household was near a field.
prop.table(table(dset$VillTyp))
# 71% were in the control group, 29% were in the treatment group (19% did not
# have PGs, 9% did).
prop.table(table(dset$IDPoorTyp))
# 89% were not IDPoor, 8% IDPoor2, 3% IDPoor1.
summary(as.factor(dset$NumPplHH))
hist(dset$NumPplHH)
summary(dset$NumPplHH)
summary(dset2$NumPplHH)
t.test(x = dset$NumPplHH, y = dset2$NumPplHH)
# Average number of people per household was 5.0, trending higher
# than average number of people that had not emptied 4.6 (p = 0.07).
mean(dset$NumPplHHLatUsr)
mean(dset$NumPplHHLatUsr) / mean(dset$NumPplHH)
t.test(x = dset$NumPplHHLatUsr, y = dset2$NumPplHHLatUsr)
# Average number of latrine users per household was 4.4 (88% of household).
# Statistically more than 4.0 of HHs that had not emptied (p = 0.05).
prop.table(table(dset$HousFlod))
prop.table(table(dset$FlodSevrty))
t.test(x = as.numeric(dset$HousFlod), y = as.numeric(dset2$HousFlod))
# 30% of households flooded at some time in the year (59% mild, 
# 41% moderate, 0% severe). CHECK IF WHEN FLOODED ASKED.
prop.table(table(dset$RoadAccsTruck))
# 91% were accessible to a large truck via road. CHECK IF SEASONALITY ASKED.
freqs = table(dset$RoadAccsTruck, dset$EmptyMethds)
CrossTable(freqs); assocstats(freqs)
t.test(x = as.numeric(dset$RoadAccsTruck), y = as.numeric(dset2$RoadAccsTruck))
# No association between RoadAccess and EmptyMthds. Makes sense because
# no HHs reported VTruckEmptying. p = 0.55
freqs = table(dset$EmptyWho, dset$RoadAccsTruck)
CrossTable(freqs); assocstats(freqs)
freqs = table(dset$EmptyCost, dset$RoadAccsTruck)
CrossTable(freqs); assocstats(freqs)
freqs = table(dset$EmptyMethds, dset$RoadAccsTruck)
CrossTable(freqs); assocstats(freqs)
# No association between road access and emptying method and who.
# Trend between road access and empty cost. Not enough data though.
summary(dset$Date)
dset$LatAge = as.numeric(difftime(dset$Date, 
                                  dset$LatStartUseDate, units = "days")/365)
summary(dset$LatAge); sd(dset$LatAge, na.rm = TRUE)
# Average number of years using latrine was 7.9 years (sd = 5.9, max = 30.8,
# min = 0.2).
prop.table(table(dset$PiercdPit))
t.test(x = as.numeric(dset$PiercdPit), y = as.numeric(dset2$PiercdPit))
# 17% had pierced pits. Not stat diff from non-emptiers.
prop.table(table(dset$NumPits))
# 92% had only 1 pit, and 8% had 2 pits.
prop.table(table(dset$NumRngs.Othr))
summary(dset$NumRngs.Othr)
hist(dset$NumRngs.Othr)
sd(dset$NumRngs.Othr, na.rm = TRUE)
t.test(x = as.numeric(dset$NumRngs.Othr), y = as.numeric(dset2$NumRngs.Othr))
# Most (85%) had pits with 3 or more rings (mean = 3.5, sd = 1.0, max = 6, 
# min = 1). p = 0.06. Linreg above gives more accurate results.
prop.table(table(dset$EmptyChlngs))
# Only 5% reported any challenge with emptying, and all were about 
# needing to empty too frequently.
summary(as.factor(dset$EmptyNum))
hist(dset$EmptyNum, breaks = 30)
prop.table(table(as.factor(dset$EmptyNum)))
# Number of times emptied before ranged from 1 to 21.
# 35% emptied 1x, 27% emptied 2x, 19% emptied 3x, 
# 16% emptied 4-6x, and 2% (2 HHs) emptied 20x
cor.test(dset$EmptyNum, dset$LatAge)
# EmptyNum goes up as LatAge goes up (p = 0.029, R = 0.23)
dset = droplevels(dset)
summary(as.factor(dset$EmptyNum))
gf = goodfit(dset$EmptyNum, type = "poisson", method = "ML"); gf; summary(gf)
# EmptyNum not Poisson.
temp = subset(dset, EmptyNum < 20)
summary(as.factor(temp$EmptyNum)); hist(temp$EmptyNum, breaks = 30)
gamma_test(temp$EmptyNum)
descdist(temp$EmptyNum, discrete = TRUE)
fit.pois = fitdist(temp$EmptyNum, "pois")
plot(fit.pois)
# EmptyNum without outliers is Poisson!
summary(glm(EmptyNum ~ IDPoor + NumPplHHLatUsr + HousFlod +
              RoadAccsTruck + PiercdPit + NumPits + NumRngs + NumRngs.Othr + 
              EmptyChlngs + EmptyMethds + EmptyWho + 
              EmptyExprience + EmptyTimeHrs + EmptyWhy +
              EmptyDispos + EmptyPlan + FSMServProvdrs + LatAge,
            data = temp, family = poisson(link = "log"),
            na.action = na.omit,
            control = list(maxit = 50)))
# Older latrines emptied more (0.04, p = 0.004).
# Hiring a service provider emptied less (-1.3, p =...)
dset$EmptyNumPerYrLatOwn = round(dset$EmptyNum / dset$LatAge, digits = 3)
summary(dset$EmptyNumPerYrLatOwn)
summary(as.factor(dset$EmptyNumPerYrLatOwn))
hist(dset$EmptyNumPerYrLatOwn, breaks = 30)
descdist(subset(dset, !is.na(EmptyNumPerYrLatOwn))$EmptyNumPerYrLatOwn, 
         discrete = FALSE)
shapiro.test(dset$EmptyNumPerYrLatOwn)
# EmptyNumPerYrLatOwn not Gaussian
hist(log(dset$EmptyNumPerYrLatOwn), breaks = 30)
descdist(log(subset(dset, !is.na(EmptyNumPerYrLatOwn))$EmptyNumPerYrLatOwn), 
         discrete = FALSE)
shapiro.test(log(dset$EmptyNumPerYrLatOwn))
# Log(EmptyNumPerYrLatOwn) is Gaussian!
summary(glm(EmptyNumPerYrLatOwn ~ IDPoor + NumPplHHLatUsr + HousFlod +
              RoadAccsTruck + PiercdPit + NumPits + NumRngs + NumRngs.Othr + 
              EmptyChlngs + EmptyMethds + EmptyWho + 
              EmptyExprience + EmptyTimeHrs + EmptyWhy +
              EmptyDispos + EmptyPlan + FSMServProvdrs,
            data = dset, family = gaussian(link = "log"),
            na.action = na.omit,
            control = list(maxit = 50)))
# Emptied more per yr lat owned:
#   Road access. Convenient? Easier? More money because better location?
#   Pierced pit. Fill up faster? Want to fertilize fields? (CHECK)
#   More pits. Pits undersized for num lat users? (CHECK)
#   Self-empty with pump. Easier? Faster? Needed more? (CHECK)
#   Neutral emptying experience. More emptying = more irritating?
#   Dipos in fields. Fertilize crops?
#   Fertilize crops trended. (same as right above)
#   Had a plan to empty. FSM on their mind? Emptied regularly?
#   HHs that didn't know FSM service providers. Didn't look for them because
#     couldn't afford? More convenient to empty self?
# Emptied less per yr lat owned:
#   IDPoor. Less time? Less money?
#   More people using lat. Pits oversized? More pits? (CHECK)
#   Flooding. Flooding out? Don't use during part of year?
#   Smaller rings. With smaller HH? (CHECK)
#   Num rings emptied less. Hold more volume.
#   Self-emptiers. Smell, disgust, busy? (CHECK)
#   Longer time to empty. More of a burden? More expensive?
# Didn't affect empties per yr lat owned:
#   Why emptied. 
gamma_test(dset$EmptyNumPerYrLatOwn)
# Not gamma.
subset(dset, EmptyNumPerYrLatOwn > 1.5 & EmptyNum > 1)
# The 3 HHs that empty very frequently:
# 2 not IDPoor, 1 IDPoor2
# 5, 6, 3 ppl in HH, all using lat
# All had road access
# None flooded
# 1 had a pierced pit
# 1 pit, 3 rings, 2 had 80 cm rings, 1 had 100 cm rings
# Self- or family-empty, good and neutral exp
# No challenges
# Emptynum 5, 21, 3
# LatAge 3, 7, 2 years
# empties per yr lat owned = 1.5, 3, 1.7
# 2, 4 hrs, 10 mins to empty
# 2 bucket, 1 pump
# Lat unusable, dispos in field
# Plan same, will pay 0 or DK, no FSM serv providrs
summary(dset$EmptyLast)
# Lots of missing data.
# Mean last empty was 1 year prior to survey (min 1 month, max 5.5 years)
prop.table(table((dset$EmptyMethds)))
# 82% emptied by bucket, 18% emptied by pump.
freqs = table(dset$EmptyMethds, dset$EmptyPlanMethds)
CrossTable(freqs); assocstats(freqs)
# HHs that emptied before overwhelmingly plan to empty the same way in the
# future (p = 0.000, v = 0.84). Big problem for behavior change away from 
# self-emptying! Recommend improvements to safety in self-emptying as interim
# step in rural comms.
# Of HHs that pumped, 77% plan to pump, 15% plan to Vtruck, 8% plan to bucket.
#    None don't know.
prop.table(table(dset$EmptyWho))
# 71% self-emptied, 23% emptied by family member, 5% paid someone
freqs = table(dset$EmptyWho, dset$EmptyCost)
CrossTable(freqs); assocstats(freqs)
# Results not useful. Need to convert EmptyCost to numeric. Done below.
summary(dset$EmptyCost)
temp = droplevels(subset(dset, EmptyCost != "DK"))
temp$EmptyCost = as.numeric(as.character(temp$EmptyCost))
summary(temp$EmptyCost); sd(temp$EmptyCost)
summary(temp$EmptyCost[temp$EmptyCost != 0]); sd(temp$EmptyCost[temp$EmptyCost != 0])
hist(temp$EmptyCost, breaks = 50)
plot(temp$EmptyCost[order(empty.cost, decreasing = TRUE)])
boxplot(temp$EmptyCost)
# Mean cost to empty 5344 Riel (sd = 17495, min = 0, max = 120000).
# Thse small costs included bottled water, soda and perhaps fragrances.
cor.test(temp$EmptyCost, as.numeric(temp$EmptyWho))
# No association between EmptyCost and EmptyWho.
num.rngs = droplevels(subset(dset, EmptyCost != "DK"))$NumRngs.Othr
num.rngs = as.numeric(as.character(num.rngs))
dset$EmptyCostPerRing = empty.cost / num.rngs
temp = droplevels(subset(dset, EmptyCost != "DK"))
freqs = table(temp$EmptyWho, temp$EmptyCost)
CrossTable(freqs); assocstats(freqs)
temp2 = as.numeric(as.character(temp$EmptyCost[temp$EmptyWho == "Pro"]))
mean(temp2); sd(temp2); summary(temp2); boxplot(temp2)
# Empty cost strongly associated with who emptied (p = 0.001, v = 0.59).
# Self and family mostly 0 cost (93% and 71%). Some HHs reported relatively
#   small costs between 1500 to 20000 Riel.
# Paying someone cost between 10000 and 120000 Riel. Only 4 paid someone tho.
#   Mean = 50000, sd = 49666  Hahaha.
dset$EmptyCostNumeric = dset$EmptyCost
dset$EmptyCostNumeric[dset$EmptyCostNumeric == "DK"] = NA
dset = droplevels(dset)
dset$EmptyCostNumeric = as.numeric(as.character(dset$EmptyCostNumeric))
summary(dset$EmptyCostNumeric)
cor.test(dset$EmptyCostNumeric, as.numeric(dset$EmptyWho))
# Nothing new. Bad test because EmptyWho not ratio.
prop.table(table(dset$EmptyExprience))
# 87% good experience with pit emptying, 13% neutral, 0% bad. Soc des bias.
summary(dset$EmptyTimeHrs); summary(as.factor(dset$EmptyTimeHrs))
sd(dset$EmptyTimeHrs)
prop.table(table(as.factor(dset$EmptyTimeHrs)))
# Mean empty time is 2.1 hours (sd = 2.2, min = 4 minutes ?, max = 12 hours).
freqs = table(dset$EmptyTimeHrs, dset$NumPits)
CrossTable(freqs); assocstats(freqs)
freqs = table(dset$EmptyTimeHrs, dset$NumRngs.Othr)
CrossTable(freqs); assocstats(freqs)
# No association between empty time and num pits or num rings.
summary(dset$EmptyWhy)
prop.table(table(dset$EmptyWhy))
# 58% emptied because latrine was unusable, 23% smelled, 15% to fertilize
# crops, and 1% each scheduled time arrived, pit overflowed, and don't know 
summary(subset(dset, EmptyWhy == "FertlizCrops"))
summary(dset)
dset = dummy_cols(dset, select_columns = "EmptyWhy")
summary(glm(EmptyWhy_Unusable ~ LatAge + IDPoor + NumPplHHLatUsr + HousFlod +
              RoadAccsTruck + PiercdPit + NumPits + NumRngs + NumRngs.Othr +
              EmptyChlngs + EmptyNum + EmptyMethds + EmptyWho + 
              EmptyExprience + EmptyTimeHrs + 
              EmptyDispos + EmptyPlan + FSMServProvdrs, 
            data = dset, family = binomial(link = "logit"),
            na.action = na.omit,
            control = list(maxit = 50)))
# Long time to empty trends with less likely usuable is reason to empty.
summary(glm(EmptyWhy_Smell ~ LatAge + IDPoor + NumPplHHLatUsr + HousFlod +
              RoadAccsTruck + PiercdPit + NumPits + NumRngs + NumRngs.Othr +
              EmptyChlngs + EmptyNum + EmptyMethds + EmptyWho + 
              EmptyExprience + EmptyTimeHrs + 
              EmptyDispos + EmptyPlan + FSMServProvdrs, 
            data = dset, family = binomial(link = "logit"),
            na.action = na.omit,
            control = list(maxit = 50)))
# LatAge trends with less likely smell being a reason why emptied.
# IDPoor trends with more likely smell being a reason.
# More rings trends with more likely smell.
# More times emptying associated with less likely smell.
# Dispose in field associated with less likely smell being reason.
# Having a plan associated with less likely smell.
summary(glm(EmptyWhy_FertlizCrops ~ LatAge + IDPoor + NumPplHHLatUsr + HousFlod +
              RoadAccsTruck + PiercdPit + NumPits + NumRngs + NumRngs.Othr +
              EmptyChlngs + EmptyNum + EmptyMethds + EmptyWho + 
              EmptyExprience + EmptyTimeHrs + 
              EmptyDispos + EmptyPlan + FSMServProvdrs, 
            data = dset, family = binomial(link = "logit"),
            na.action = na.omit,
            control = list(maxit = 50)))
# Nothing.
prop.table(table(dset$EmptyDispos))
# 74% disposed in field, 13% fertilized crops, 10% buried, and 2% river/pond.
dset = dummy_cols(dset, select_columns = "EmptyDispos")
summary(dset)
summary(glm(EmptyDispos_InField ~ LatAge + IDPoor + NumPplHHLatUsr + HousFlod +
              RoadAccsTruck + PiercdPit + NumPits + NumRngs + NumRngs.Othr +
              EmptyChlngs + EmptyNum + EmptyMethds + EmptyWho + 
              EmptyExprience + EmptyTimeHrs + 
              EmptyWhy + EmptyPlan + FSMServProvdrs, 
            data = dset, family = binomial(link = "logit"),
            na.action = na.omit,
            control = list(maxit = 50)))
# Old latrine associated with less likely to dispos in field.
# Long time to empty trends with more likely to dispos in field.
summary(glm(EmptyDispos_FertlizCrops ~ LatAge + IDPoor + NumPplHHLatUsr + HousFlod +
              RoadAccsTruck + PiercdPit + NumPits + NumRngs + NumRngs.Othr +
              EmptyChlngs + EmptyNum + EmptyMethds + EmptyWho + 
              EmptyExprience + EmptyTimeHrs + 
              EmptyWhy + EmptyPlan + FSMServProvdrs, 
            data = dset, family = binomial(link = "logit"),
            na.action = na.omit,
            control = list(maxit = 50)))
# Nothing
summary(glm(EmptyDispos_Buried ~ LatAge + IDPoor + NumPplHHLatUsr + HousFlod +
              RoadAccsTruck + PiercdPit + NumPits + NumRngs + NumRngs.Othr +
              EmptyChlngs + EmptyNum + EmptyMethds + EmptyWho + 
              EmptyExprience + EmptyTimeHrs + 
              EmptyWhy + EmptyPlan + FSMServProvdrs, 
            data = dset, family = binomial(link = "logit"),
            na.action = na.omit,
            control = list(maxit = 50)))
# Flood trends with burial.
# More rings trends with not burying.
# More empties trends with burying.
# Unusable reason associated with burying.
summary(dset$EmptyPlan); prop.table(table(dset$EmptyPlan))
summary(dset$EmptyPlanMethds); prop.table(table(dset$EmptyPlanMethds))
# 89% planned out how they will empty in the future. Of them, 80% planned to 
# bucket, 15% planned to use a pump, 2% weren't sure which method they'd use,
# and 2% planned to use a vacuum truck.
summary(dset$EmptyWilPay)
prop.table(table(dset$EmptyWilPay))
empty.wil.pay = as.numeric(as.character(
  droplevels(subset(dset, EmptyWilPay != "DK"))$EmptyWilPay))
summary(empty.wil.pay); hist(empty.wil.pay, breaks = 50)
sd(empty.wil.pay)
empty.wil.pay2 = as.numeric(as.character(
  droplevels(subset(dset, EmptyWilPay != "DK" & EmptyWilPay != 0))$EmptyWilPay))
summary(empty.wil.pay2); hist(empty.wil.pay2, breaks = 50); sd(empty.wil.pay2)
summary(as.factor(empty.wil.pay2))
# 61% did not know what they were willing to pay, 24% were not willing to pay.
# Of everyone that reported a willingness to pay, mean = 10888 Riel, 
# sd = 25280, max = 100000 Riel.
# Of those willing to pay anything, mean = 28477, min = 3500.
summary(dset$FSMServProvdrs); prop.table(table(dset$FSMServProvdrs))
summary(dset2$FSMServProvdrs); prop.table(table(dset2$FSMServProvdrs))
t.test(as.numeric(dset$FSMServProvdrs), as.numeric(dset2$FSMServProvdrs))
# Of people that already emptied, only 3% knew of an FSM service provider,
# 6% didn't know of any, and 91% said there were none in their area.
# A lack of emptying experience decreased knowledge of FSM service providers 
# in the area (p = 0.000).
summary(dset$FSMServProvdrs.Contact); 2/98
# 2 of the 4 HHs (2% of HHs that emptied before) that knew an FSM serv 
# provider knew how to get in contact with them.
summary(dset$FSMServProvdrs.Cost)
# HHs reported FSM serv costs of $150 to empty 1-5 rings, $40/3 rings, and
# 50000 Riel.  Another HH did not know what they charged.
summary(dset$LatAge); sd(dset$LatAge, na.rm = TRUE)
# Latrines ranged in age from 0.2 yrs to 31 yrs (mean = 7.9, sd = 5.9).
summary(dset$EmptyNumPer20Yr); sd(dset$EmptyNumPer20Yr, na.rm = TRUE)
# Empties per 20 yrs ranged from 1 to 91 (mean = 10, med = 6, sd = 12.3)
summary(dset$YrsBtwnEmpties); sd(dset$YrsBtwnEmpties, na.rm = TRUE)
# Years between empties ranged from 0.2 to 31 yrs (mean = 4.4, sd = 4.5)
###############################################################################
# Have Not Emptied Before
###############################################################################
dset2 = droplevels(subset(d.followup, EmptyBefor == "No"))
summary(dset2)
length(dset2$EmptyBefor) / (length(dset$EmptyBefor) + length(dset2$EmptyBefor))
# Of the 440 respondents that have not emptied before (82% of respondents 
# that answered if they emptied before or not),
# No household was near a river or pond.
# Every household was near a field.  
prop.table(table(dset2$VillTyp))
# 46% were in the control group, 54% treatment (36% did not have PG, 18% 
# did).
prop.table(table(dset2$IDPoorTyp))
# 88% were not IDPoor, 8% IDPoor2, 4% IDPoor1. Very similar to HHs that
# emptied before.
summary(as.factor(dset2$NumPplHH))
hist(dset2$NumPplHH)
summary(dset$NumPplHH)
summary(dset2$NumPplHH)
t.test(x = dset2$NumPplHH, y = dset$NumPplHH)
# Average number of people per household was 4.6, trending lower
# than average number of people that hademptied 5.0 (p = 0.07).
mean(dset2$NumPplHHLatUsr)
mean(dset2$NumPplHHLatUsr) / mean(dset2$NumPplHH)
t.test(x = dset2$NumPplHHLatUsr, y = dset$NumPplHHLatUsr)
# Average number of latrine users per household was 4.0 (88% of household),
# significantly less than 4.4 of HHs that emptied (p = 0.05).
prop.table(table(dset2$HousFlod))
prop.table(table(dset2$FlodSevrty))
# 23% of households flooded at some time in the year (59% mild, 
# 41% moderate, 0% severe), lower than 30% of HHs that emptied before.
# CHECK IF WHEN FLOODED ASKED.
prop.table(table(dset2$RoadAccsTruck))
# 88% were accessible to a large truck via road, similar to HHs that emptied.
# CHECK IF SEASONALITY ASKED.
summary(dset2$Date)
dset2$LatAge = as.numeric(difftime(dset2$Date, 
                                   dset2$LatStartUseDate, units = "days")/365)
summary(dset2$LatAge); sd(dset2$LatAge, na.rm = TRUE)
t.test(x = dset2$LatAge, y = dset$LatAge)
# Average number of years using latrine was 5.1 years (sd = 3.7, max = 23,
# min = 0). Statistically significant difference from HHs that emptied: 
# HHs that emptied had longer latage with lower sd.
summary(dset2)
prop.table(table(dset2$PiercdPit))
t.test(x = as.numeric(dset2$PiercdPit), y = as.numeric(dset$PiercdPit))
# 13% had pierced pits. Not statistically different from HHs that emptied
# (p = 0.34).
prop.table(table(dset2$NumPits))
t.test(x = dset2$NumPits, y = dset$NumPits)
# 87% had only 1 pit, 13% had 2 pits, and 0.5% had 3 pits. Trending different
# (p = 0.08).
prop.table(table(dset2$NumRngs))
t.test(x = as.numeric(dset2$NumRngs), y = as.numeric(dset$NumRngs))
# 54% had 80-cm rings, 39% had 100-cm rings, 7% had other.
# Sig diff (p = 0.05)
prop.table(table(dset2$NumRngs.Othr))
summary(dset2$NumRngs.Othr)
hist(dset2$NumRngs.Othr)
sd(dset2$NumRngs.Othr, na.rm = TRUE)
t.test(x = dset2$NumRngs.Othr, y = dset$NumRngs.Othr)
# Most (96%) had pits with 3 or more rings (mean = 3.5, sd = 1.0, max = 8, 
# min = 2). Trending different (p = 0.06).
dset2 = dummy_cols(dset2, select_columns = "EmptyWhyNot")
summary(dset2)
sapply(dset2, function(x) sum(is.na(x)))
summary(glm(EmptyWhyNot_NotFull ~ LatAge + IDPoor + NumPplHHLatUsr + HousFlod +
              RoadAccsTruck + PiercdPit + NumPits + NumRngs + NumRngs.Othr +
              EmptyPlan + EmptyPlanMethds + FSMServProvdrs, 
            data = dset2, family = binomial(link = "logit"),
            na.action = na.omit,
            control = list(maxit = 50)))
# Older lat yields less likely to say not full.
# Pierced pit yields less likely to say not full.
# More pits yields more likely to say not full.
# Small rings yields more likely to say not full. Weird.
# Plan to empty with vtruck yields less likely to say not full.
summary(glm(EmptyWhyNot_PreferOvrflw ~ LatAge + IDPoor + NumPplHHLatUsr + HousFlod +
              RoadAccsTruck + PiercdPit + NumPits + NumRngs + NumRngs.Othr +
              EmptyPlan + EmptyPlanMethds + FSMServProvdrs, 
            data = dset2, family = binomial(link = "logit"),
            na.action = na.omit,
            control = list(maxit = 50)))
# Older lat yields more likely to prefer to let it overflow.
# Pierced pit yields strongly likely to prefer to overflow.
# More pits yields less likely to prefer overflow.
# Smaller rings yields yields less likely to prefer overflow.
# Plan to vtruck yields more likely to prefer overflow.
summary(glm(EmptyWhyNot_WrongEquipment ~ LatAge + IDPoor + NumPplHHLatUsr + HousFlod +
              RoadAccsTruck + PiercdPit + NumPits + NumRngs + NumRngs.Othr +
              EmptyPlan + EmptyPlanMethds + FSMServProvdrs, 
            data = dset2, family = binomial(link = "logit"),
            na.action = na.omit,
            control = list(maxit = 50)))
# Nothing. Too little data.
summary(glm(EmptyWhyNot_TooBusy ~ LatAge + IDPoor + NumPplHHLatUsr + HousFlod +
              RoadAccsTruck + PiercdPit + NumPits + NumRngs + NumRngs.Othr +
              EmptyPlan + EmptyPlanMethds + FSMServProvdrs, 
            data = dset2, family = binomial(link = "logit"),
            na.action = na.omit,
            control = list(maxit = 50)))
# Nothing. Too little data.
summary(dset2$EmptyPlan); prop.table(table(dset2$EmptyPlan))
t.test(x = as.numeric(dset2$EmptyPlan), y = as.numeric(dset2$EmptyPlan))
summary(dset2$EmptyPlanMethds); prop.table(table(dset2$EmptyPlanMethds))
t.test(x = as.numeric(dset2$EmptyPlanMethds), y = as.numeric(dset2$EmptyPlanMethds))
# 86% planned out how they will empty in the future. Not stat diff (p = 1).
# Of them, 76% planned to bucket, 12% weren't sure which method they'd use, 
# 7% planned to use a pump, and 4% planned to use a vacuum truck. (not stat diff
# p = 1)
summary(dset2$EmptyWilPay)
prop.table(table(dset2$EmptyWilPay))
empty.wil.pay = as.numeric(as.character(
  droplevels(subset(dset2, EmptyWilPay != "DK"))$EmptyWilPay))
summary(empty.wil.pay); hist(empty.wil.pay, breaks = 50)
sd(empty.wil.pay)
t.test(x = as.numeric(as.character(
  droplevels(subset(dset2, EmptyWilPay != "DK"))$EmptyWilPay)),
  y = as.numeric(as.character(
    droplevels(subset(dset, EmptyWilPay != "DK"))$EmptyWilPay)))
# 79% did not know what they were willing to pay, 13% were not willing to pay.
# Of everyone that reported a willingness to pay, mean = 19186 Riel, 
# sd = 44797, max = 200000 Riel. Not stat diff (p = 0.22).
empty.wil.pay2 = as.numeric(as.character(
  droplevels(subset(dset2, EmptyWilPay != "DK" & EmptyWilPay != 0))$EmptyWilPay))
summary(empty.wil.pay2); hist(empty.wil.pay2, breaks = 50)
sd(empty.wil.pay2)
t.test(x = as.numeric(as.character(droplevels(subset(
  dset, EmptyWilPay != "DK" & EmptyWilPay != 0))$EmptyWilPay)), 
  y = as.numeric(as.character(droplevels(subset(
    dset2, EmptyWilPay != "DK" & EmptyWilPay != 0))$EmptyWilPay)))
# Of those willing to pay anything, mean = 53446, min = 1500, sd = 61840.
# Not stat diff (p = 0.11).
summary(dset2$FSMServProvdrs); prop.table(table(dset2$FSMServProvdrs))
t.test(as.numeric(dset$FSMServProvdrs), as.numeric(dset2$FSMServProvdrs))
# Of HHs that have not emptied, only 2% knew of an FSM service provider,
# 20% didn't know of any, and 78% said there were none in their area.
# Stat diff (p = 0.000).
summary(dset2$FSMServProvdrs.Contact); 5/440
# 5 of the 10 HHs (1% of HHs that had not emptied before) that knew an FSM 
# serv provider knew how to get in contact with them.
summary(dset2$FSMServProvdrs.Cost)
# HHs reported FSM serv costs of $10/ring, $40/3 rings, $25/3 rings, 
# $50 flat, and 2 HHs did not know what they charged.
summary(dset2$LatAge); sd(dset2$LatAge, na.rm = TRUE)
t.test(dset2$LatAge, dset$LatAge)
# Latrines ranged in age from 0 yrs to 22.8 yrs (mean = 5.1, sd = 3.7).
# Stat diff (p = 0.000).
###############################################################################
# Flood vs. No Flood
###############################################################################
summary(d.followup)
d.followup$LatAge = as.numeric(difftime(d.followup$Date, 
                                        d.followup$LatStartUseDate, 
                                        units = "days")/365)
summary(d.followup$LatAge)
dset3 = subset(d.followup, !is.na(HousFlod))
summary(dset3)
print(sapply(dset3, function(x) sum(is.na(x))))
summary(glm(HousFlod ~ RGend + IDPoorTyp + NumPplHHLatUsr + 
              RoadAccsTruck + PiercdPit + NumPits + NumRngs + NumRngs.Othr +
              EmptyBefor + EmptyPlan + FSMServProvdrs + LatAge, 
            data = dset3, family = binomial(link = "logit"),
            na.action = na.omit,
            control = list(maxit = 50)))
# More pits, less likely to flood (p = 0.04).
# Pierced pit trends with more likely to flood (p = 0.05)
# More rings, less likely to flood (p = 0.03).
dset4 = subset(dset3, EmptyBefor == "Yes")
summary(dset4)
summary(glm(HousFlod ~ RGend + IDPoor + NumPplHHLatUsr + 
              RoadAccsTruck + PiercdPit + NumPits + NumRngs + NumRngs.Othr +
              EmptyChlngs + EmptyNum + EmptyMethds + EmptyWho + 
              EmptyExprience + EmptyTimeHrs + 
              EmptyWhy + EmptyPlan + FSMServProvdrs + LatAge, 
            data = dset4, family = binomial(link = "logit"),
            na.action = na.omit,
            control = list(maxit = 50)))
# Nothing.
dset5 = subset(dset3, EmptyBefor != "Yes")
summary(dset5)
summary(glm(HousFlod ~ IDPoor + NumPplHHLatUsr + 
              RoadAccsTruck + PiercdPit + NumPits + NumRngs + NumRngs.Othr +
              EmptyWhyNot + EmptyPlan + EmptyPlanMethds + FSMServProvdrs + LatAge, 
            data = dset5, family = binomial(link = "logit"),
            na.action = na.omit,
            control = list(maxit = 50)))
# IDPoor trends with more flooding.
# More pits trends with less flooding.
# More rings, less flooding.
# Planning to bucket and not having a plan, more flooding
# Planning to vtruck trends with more flooding
###############################################################################
# Pierced Pits
###############################################################################
freqs = table(d.baseline$PiercdPit, d.baseline$VillTyp)
CrossTable(freqs)
assocstats(freqs)  
# p = 0.000, v = 0.20 -> Treatment group in baseline survey has more HHs 
# with pierced pits. This may imply that HHs in treatment group think about 
# FSM more (more amenable/interested in ADP) or may have already dealt with
# the FSM problem (less amenable/interest in ADP).
freqs = table(d.followup$PiercdPit, d.followup$VillTyp)
CrossTable(freqs)
assocstats(freqs)
# p = 0.67 -> No difference in number of HHs with pierced pits in followup.
dset = subset(d.followup, VillTyp == "Cntrl" | VillTyp == "TreatPG")
dset = droplevels(dset)
freqs = table(dset$PiercdPit, dset$VillTyp)
CrossTable(freqs)
assocstats(freqs)  
# p = 0.39 -> No difference in number of HHs with pierced pits in followup.
# Unknown reason for difference in sample between baseline and followup.

summary(d.followup$PiercdPit)
summary(d.followup$NumPplHH)
biserial.cor(d.followup$NumPplHH, d.followup$PiercdPit, use = "complete.obs")
# r_pb = -0.10 -> Very weak correlation between piercing pit and # ppl in HH
# NOT SUPPORTED IN LOGISTIC REGRESSION BELOW.
summary(d.followup$NumPplHHLatUsr)
biserial.cor(d.followup$NumPplHHLatUsr, d.followup$PiercdPit, 
             use = "complete.obs")
# r_pb = -0.10 -> Very weak correlation between piercing pit and # ppl in HH
# using latrine.  NOT SUPPORTED IN LOGISTIC REGRESSION BELOW.

freqs = table(d.baseline$PiercdPit, d.baseline$LatOwnrIDPoor)
CrossTable(freqs)
assocstats(freqs)  
# p = 0.11 -> Pierced pit not affected by IDPoor status
freqs = table(d.followup$PiercdPit, d.followup$IDPoor)
CrossTable(freqs)
assocstats(freqs)
# p = 0.69 -> Pierced pit not affected by IDPoor status
freqs = table(d.followup$PiercdPit, d.followup$IDPoorTyp)
CrossTable(freqs)
assocstats(freqs)
# p = 0.53 -> Pierced pit not affected by IDPoor status

freqs = table(d.followup$PiercdPit, d.followup$HousFlod)
CrossTable(freqs)
assocstats(freqs)
# p = 0.002 -> Pierced pit not affected by IDPoor status

summary(d.baseline$PGid)
# summary(d.followup$PGid)
summary(d.sldglvls$PGid)
# paste(d.followup$RespLName[1], d.followup$RespFName[1])
# test1 = data.frame(Name = paste(d.followup$RespLName, d.followup$RespFName))
# test1[1]
grepl(d.sldglvls$PGid[89], d.baseline$PGid)
d.sldglvls$PiercdPit = NA
for (row in 1:length(d.sldglvls$PGid)) {
  d.sldglvls$PiercdPit[row] = 
    as.character(
      d.baseline$PiercdPit[grepl(d.sldglvls$PGid[row], d.baseline$PGid)])
}
d.sldglvls$PiercdPit = as.factor(d.sldglvls$PiercdPit)
levels(d.sldglvls$PiercdPit) = c("No", NA, "Yes")
summary(d.sldglvls$PiercdPit)
biserial.cor(d.sldglvls$`LiquidDepth(mm)`, d.sldglvls$PiercdPit, 
             use = "complete.obs")
# r_pb = 0.00 -> No correlation between liquid depth and pierced pit

freqs = table(d.baseline$PiercdPit, d.baseline$Job)
CrossTable(freqs)
assocstats(freqs)
# p = 0.10 -> No association between job and pierced pit.

freqs = table(d.baseline$PiercdPit, d.baseline$LatInstlYr)
CrossTable(freqs)
assocstats(freqs)
# Unsolvable
biserial.cor(as.numeric(d.baseline$LatInstlYr), d.baseline$PiercdPit, 
             use = "complete.obs")
cor.test(as.numeric(d.baseline$LatInstlYr), as.numeric(d.baseline$PiercdPit))
# r_pb = 0.07, p = 0.08 -> Very weak trending correlation between year 
# latrine installed and presence of a pierced pit.  More pierced pits on
# older latrines.
biserial.cor(as.numeric(d.baseline$LatStartUseYr), d.baseline$PiercdPit, 
             use = "complete.obs")
cor.test(as.numeric(d.baseline$LatStartUseYr), as.numeric(d.baseline$PiercdPit))
# r_pb = 0.08, p = 0.03 -> Very weak significant correlation between year 
# latrine started use and presence of a pierced pit.  More pierced pits on
# older latrines.

freqs = table(d.followup$PiercdPit, d.followup$RoadAccsTruck)
CrossTable(freqs); assocstats(freqs)
# No association between road access and pierced pits.

summary(d.baseline)
summary(glm(PiercdPit ~ VillTyp + LatOwnrIDPoor + NumPplHH + 
              LatStartUseYr + EmptyBefor + Job, 
            data = d.baseline, family = binomial(link = "logit"), 
            na.action = na.omit))
# exp(cbind(OR = coef(logit1), confint(logit1)))  # odds ratios and 95% CI
# confint.default(logit1)  # CIs using standard errors
# with(logit1, null.deviance – deviance)  # Model fit: Chi-square
# with(logit1, df.null – df.residual)  # Model fit: degree of freedom
# with(logit1, 
#      pchisq(null.deviance – deviance, 
#             df.null – df.residual, lower.tail = FALSE))  # Model fit: p-value
# plot(logit1)
# Older latrines trend with more pierced pits (p = 0.07).
# Technical jobs trend with pierced pits (p = 0.08).
# Treatment HHs had more pierced pits (p = 0.000)

summary(d.baseline)
dset = subset(d.baseline, EmptyBefor == "No")
logit2 = glm(PiercdPit ~ VillTyp + LatOwnrIDPoor + NumPplHH + 
               LatStartUseYr + WhyNoEmpty + Job, 
             data = dset, family = binomial(link = "logit"), 
             na.action = na.omit)
summary(logit2)
# exp(cbind(OR = coef(logit1), confint(logit1)))  # odds ratios and 95% CI
# confint.default(logit1)  # CIs using standard errors
# with(logit1, null.deviance – deviance)  # Model fit: Chi-square
# with(logit1, df.null – df.residual)  # Model fit: degree of freedom
# with(logit1, 
#      pchisq(null.deviance – deviance, 
#             df.null – df.residual, lower.tail = FALSE))  # Model fit: p-value
plot(logit2)
# Among HHs that have not emptied before,
# Older latrines had more pierced pits (p = 0.04).
# Technical jobs had more pierced pits (p = 0.08).
# Treatment HHs had more pierced pits (p = 0.000)

summary(d.baseline)
dset = subset(d.baseline, EmptyBefor == "Yes")
logit3 = glm(PiercdPit ~ VillTyp + LatOwnrIDPoor + NumPplHH + 
               LatStartUseYr + EmptyMethds + Job, 
             data = dset, family = binomial(link = "logit"), 
             na.action = na.omit)
summary(logit3)
# exp(cbind(OR = coef(logit1), confint(logit1)))  # odds ratios and 95% CI
# confint.default(logit1)  # CIs using standard errors
# with(logit1, null.deviance – deviance)  # Model fit: Chi-square
# with(logit1, df.null – df.residual)  # Model fit: degree of freedom
# with(logit1, 
#      pchisq(null.deviance – deviance, 
#             df.null – df.residual, lower.tail = FALSE))  # Model fit: p-value
plot(logit3)
# Among HHs that have not emptied before,
# Not enough data.

summary(d.followup)
logit4 = 
  glm(PiercdPit ~ IDPoorTyp + NumPplHH + NumPplHHLatUsr +
        HousFlod + LatStartUseDate + RoadAccsTruck + NumPits + NumRngs + 
        EmptyBefor + PGInstld +  
        EmptyPlan + EmptyPlanMethds + FSMServProvdrs + VillTyp, 
      data = d.followup, family = binomial(link = "logit"), 
      na.action = na.omit)
summary(logit4)
# exp(cbind(OR = coef(logit1), confint(logit1)))  # odds ratios and 95% CI
# confint.default(logit1)  # CIs using standard errors
plot(logit4)
# Experiencing flooding (check survey wording!) associated with piercing
# pit (p = 0.02).
# Being unwilling to pay to empty (EmptyWilPay0) trends with 
# piercing pit (p = 0.07).
# Older latrines had more pierced pits (p = 0.000).
# Planning to self-empty had more pierced pits (p = 0.02).

summary(d.followup)
dset = subset(d.followup, HousFlod == "Yes")
summary(dset)
logit_flooding =
  glm(PiercdPit ~ IDPoorTyp + NumPplHH + NumPplHHLatUsr +
        LatStartUseDate + RoadAccsTruck + NumPits + NumRngs + 
        EmptyBefor + PGInstld +  FlodSevrty +
        EmptyPlan  + VillTyp,
      data = dset, family = binomial(link = "logit"),
      na.action = na.omit)
summary(logit_flooding)
# exp(cbind(OR = coef(logit1), confint(logit1)))  # odds ratios and 95% CI
# confint.default(logit1)  # CIs using standard errors
# plot(logit_emptied_before)
# Nothing new.

summary(d.followup)
dset = subset(d.followup, HousFlod == "No")
summary(dset)
logit_noflooding =
  glm(PiercdPit ~ IDPoorTyp + NumPplHH + NumPplHHLatUsr +
        LatStartUseDate + RoadAccsTruck + NumPits + NumRngs + 
        EmptyBefor + PGInstld +  EmptyPlanMethds +
        EmptyPlan  + VillTyp,
      data = dset, family = binomial(link = "logit"),
      na.action = na.omit)
summary(logit_noflooding)
# exp(cbind(OR = coef(logit1), confint(logit1)))  # odds ratios and 95% CI
# confint.default(logit1)  # CIs using standard errors
# plot(logit_emptied_before)
# Nothing new.

summary(d.followup)
dset = subset(d.followup, EmptyBefor == "No")
summary(dset)
logit_not_emptied_before =
  glm(PiercdPit ~ IDPoorTyp + NumPplHH + NumPplHHLatUsr +
        LatStartUseDate + RoadAccsTruck + NumPits + NumRngs + 
        PGInstld +  EmptyPlanMethds +
        EmptyPlan  + VillTyp,
      data = dset, family = binomial(link = "logit"),
      na.action = na.omit)
summary(logit_not_emptied_before)
# exp(cbind(OR = coef(logit1), confint(logit1)))  # odds ratios and 95% CI
# confint.default(logit1)  # CIs using standard errors
# plot(logit_emptied_before)
# Nothing new.

summary(d.followup)
dset = subset(d.followup, EmptyBefor == "Yes")
summary(dset$PiercdPit)
summary(glm(PiercdPit ~ IDPoorTyp + NumPplHH + NumPplHHLatUsr +
              HousFlod + RoadAccsTruck + 
              LatStartUseDate + NumPits + NumRngs + PGInstld + 
              EmptyChlngs + EmptyNum + EmptyLast + EmptyMethds + EmptyWho +
              EmptyDispos + EmptyPlan,
            data = dset, family = binomial(link = "logit"),
            na.action = na.omit))
# exp(cbind(OR = coef(logit1), confint(logit1)))  # odds ratios and 95% CI
# confint.default(logit1)  # CIs using standard errors
# plot(logit_never_emptied_before)
# Nearly all HHs that emptied before self-emptied, 70 self, 23 family member,
# and 5 pro.
# Most emptied by bucket (80), others by pump (18).
# Nothing else new.
###############################################################################
# Sludge Levels
###############################################################################
# Compare 
names(d.followup)
# ...
###############################################################################
# Closing Rates     ADDED TO FSM5 PRESENTATION
###############################################################################
sum(d.ADPsales.villtyp$NumSales) / sum(d.ADPsales.villtyp$NumSalesVisits)
# Total closing rate = 14%
d.ADPsales.villtyp$NumSales / d.ADPsales.villtyp$NumSalesVisits
# Closing rates of CnT groups are 15% and 13%
prop.test(x = d.ADPsales.villtyp$NumSales,
          n = d.ADPsales.villtyp$NumSalesVisits)
# p = 0.2457 -> Closing rates of CnT groups not different

sum(d.ADPsales.PG$NumSales) / sum(d.ADPsales.PG$NumSalesVisits)
# Total closing rate = 14%
d.ADPsales.PG$NumSales / d.ADPsales.PG$NumSalesVisits
# Closing rates of HHs with and without a PG are 27% and 12%
prop.test(x = d.ADPsales.PG$NumSales,
          n = d.ADPsales.PG$NumSalesVisits)
# p = 0.000 -> HHs with PG have higher closing rate.  PG seems to trigger 
# ADP sales.  However, we do suspect that some biases exist, including 
# the engagement of village chief and regular visits by monitoring staff to 
# households during non-sales months to measure pit fill rates.
###############################################################################
# Delivery and Cancellation Rates     ADDED TO FSM5 PRESENTATION
###############################################################################
sum(d.ADPsales.villtyp$NumDeliverd) / sum(d.ADPsales.villtyp$NumSales)
# Total delivery rate = 67%
d.ADPsales.villtyp$NumDeliverd / d.ADPsales.villtyp$NumSales
# Delivery rates of CnT groups are 79% and 51%
prop.test(x = d.ADPsales.villtyp$NumDeliverd,
          n = d.ADPsales.villtyp$NumSales)
# p = 0.000 -> More completed deliveries in treatment group
sum(d.ADPsales.villtyp$NumCanceld) / sum(d.ADPsales.villtyp$NumSales)
# Total cancellation rate = 27%
d.ADPsales.villtyp$NumCanceld / d.ADPsales.villtyp$NumSales
# Cancellation rates of CnT groups are 18% and 40%
prop.test(x = d.ADPsales.villtyp$NumCanceld,
          n = d.ADPsales.villtyp$NumSales)
# p = 0.000 -> Fewer cancellations in treatment group

sum(d.ADPsales.PG$NumDeliverd) / sum(d.ADPsales.PG$NumSales)
# Total delivery rate = 67%
d.ADPsales.PG$NumDeliverd / d.ADPsales.PG$NumSales
# Delivery rates of HHs with and without a PG are 87% and 60%
prop.test(x = d.ADPsales.PG$NumDeliverd,
          n = d.ADPsales.PG$NumSales)
# p = 0.000 -> More completed deliveries to HHs with PG
sum(d.ADPsales.PG$NumCanceld) / sum(d.ADPsales.PG$NumSales)
# Total cancellation rate = 27%
d.ADPsales.PG$NumCanceld / d.ADPsales.PG$NumSales
# Cancellation rates of HHs with and without a PG are 8% and 34%
prop.test(x = d.ADPsales.PG$NumCanceld,
          n = d.ADPsales.PG$NumSales)
# p = 0.000 -> Fewer cancellations in HHs with PG
###############################################################################
# Backlog Rates   OMITTED FROM FSM5 PRESENTATION
###############################################################################
sum(d.ADPsales.villtyp$NumBacklog) / sum(d.ADPsales.villtyp$NumSales)
# Total backlog rate = 5%
d.ADPsales.villtyp$NumBacklog / d.ADPsales.villtyp$NumSales
# Backlog rates of CnT groups are 3% and 9%
prop.test(x = d.ADPsales.villtyp$NumBacklog,
          n = d.ADPsales.villtyp$NumSales)
# p = 0.09 -> Backlog rates not different

sum(d.ADPsales.PG$NumBacklog) / sum(d.ADPsales.PG$NumSales)
# Total backlog rate = 5%
d.ADPsales.PG$NumBacklog / d.ADPsales.PG$NumSales
# Backlog rates of HHs with and without a PG are 5% and 6%
prop.test(x = d.ADPsales.PG$NumBacklog,
          n = d.ADPsales.PG$NumSales)
# p = 1 -> Backlog rates not different
###############################################################################
# Sales Waves
###############################################################################
# Could not describe any meaningful differences because amount of available
# customers in each wave decreased as ADP sales were made.
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
# Useful Distribution Fitting Functions
# gengammafit = fitdist(dset2$YrsBwtwnEmpties, "gengamma",
#                       start = function(d) 
#                         list(mu = mean(d), sigma = sd(d), Q = 0))
# qqcomp(list(gammafit, gengammafit),
#        legendtext = c("gamma", "gengamma") )
# N = length(dset2$YrsBwtwnEmpties)
# q_rel = qgengamma(ppoints(N), mu=gengammafit$estimate["mu"],
#                   sigma=gengammafit$estimate["sigma"],
#                   Q=gengammafit$estimate["Q"])
# reldist(dset2$YrsBwtwnEmpties, q_rel, 
#         main = "Relative distribution comp to generalized gamma")
# gf = goodfit(dset2$YrsBwtwnEmpties, type = "gamma", method = "ML"); gf; summary(gf)
# plot(gf, main = "Count data vs Poisson distribution")
# plot(gf, main = "Count data vs Poisson distribution", type = "dev")
# ggdensity(dset$EmptyNum)
# ggqqplot(dset$EmptyNum)
# qqPlot(dset$EmptyNum)
# shapiro.test(dset$EmptyNum)
# # Data is not Gaussian.
# ks.test(dset$EmptyNum, "pweibull")
# load_libraries(c("nortest", "tseries", "fBasics"))
# ad.test(dset$EmptyNum)
# chisq.test(dset$EmptyNum)
# cvm.test(dset$EmptyNum)
# plot(ecdf(dset$EmptyNum))
# hist(dset$EmptyNum)
# jarque.bera.test(dset$EmptyNum)
# lillie.test(dset$EmptyNum)
# pearson.test(dset$EmptyNum)
# sf.test(dset$EmptyNum)
# skewness(dset$EmptyNum)
# kurtosis(dset$EmptyNum)
# descdist(dset$EmptyNum, discrete = TRUE)
# fit.pois = fitdist(dset$EmptyNum, "pois")
# fit.nbinom = fitdist(dset$EmptyNum, "nbinom")
# fit.exp = fitdist(dset$EmptyNum, "exp")
# plot(fit.pois)
# plot(fit.nbinom)
# plot(fit.exp)
# fit.pois$aic
# fit.nbinom$aic
# #It looks like the last variable in the above model is problematic
# #Regression Model2 |||| This is a better model
# logit2 <- glm(Hashtag ~ COMSTRS+COMCOP+ADVSS+BOUND+IDENT+GRPCOM+INFODIS, data = data, family = “binomial”)
# summary(logit2)
# ## Confidence intervals (CIs) using profiled log-likelihood
# confint(logit2)
# ## CIs using standard errors
# confint.default(logit2)
# ## odds ratios only
# exp(coef(logit2))
# ## odds ratios and 95% CI
# exp(cbind(OR = coef(logit1), confint(logit2)))
# #Model fit
# with(logit2, null.deviance – deviance)#Chi-square
# with(logit2, df.null – df.residual) #degree of freedom
# with(logit2, pchisq(null.deviance – deviance, df.null – df.residual, lower.tail = FALSE))#p-value