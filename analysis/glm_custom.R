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
# LOAD, CLEAN AND CATEGORIZE DATA
###############################################################################
# Load data file
load(file = "iDE_Oct2017.RData");
names(data)
summary(data$Prov)
# data = subset(data, Prov == "Banteay Meanchey")

###############################################################################
# MODEL IntndPitFullDes
###############################################################################
# Subset data
# summary(data); length(data[,1])
data = subset(data, !is.na(IntndPitFullDes))
data = subset(data, Prov != "Phnom Penh")
data = droplevels(data)
data = subset(data, select = -c(IntndChngDich, IntndChng, IntndChng_Shltr,
                                IntndChng_Shwr, IntndChng_Sink, IntndChng_Pit,
                                IntndChng_WtrRes, IntndChng_Othr,
                                IntndChng_NAAlwysToi, RDefBefor,
                                RDefBefor_Othr, RDefBefor_NAAlwysToi))
# summary(data); length(data[,1])
# print(sapply(data, function(x) sum(is.na(x))))
# missmap(data, main = "Missing Values in Variables", legend = F)

# Model IntndPitFullDes
vars = ""
for (var in names(data)) {
  if (vars == "") { vars = var }
  else (vars = paste(vars, " + ", var, sep = ""))
}
# All possibly relevant variables
options(max.print = 999999)
model = glm(formula = IntndPitFullDes ~ CGend +
              IDPoor + IDPoorTyp + LivRP + VillOD + FreqNeiToi + AdltUseLat +
              ChldUseLat + InfLatDump + Chlngs + Satis + Rec +
              SatisSup + RecSup + Yr + Mnth + YrMnth + RDefBefor_BshFld +
              RDefBefor_RivPnd + RDefBefor_NeiToi + Rain.mm + ChlngsNoFlsh +
              ChlngsFlood + ChlngsOthr + ChlngsFulOvrFlw + ChlngsNoWtr +
              ChlngsSmels + ChlngsOK,
            data = data,
            family = binomial(link = "logit"),
            na.action = na.omit); summary(model); anova(model, test = "Chisq"); pR2(model)
model = glm(formula = IntndPitFullDes ~ Prov + CGend +
              IDPoor + IDPoorTyp + LivRP + VillOD + FreqNeiToi + AdltUseLat +
              ChldUseLat + InfLatDump + Chlngs + Satis + Rec +
              SatisSup + RecSup + Yr + Mnth + YrMnth + RDefBefor_BshFld +
              RDefBefor_RivPnd + RDefBefor_NeiToi + Rain.mm + ChlngsNoFlsh +
              ChlngsFlood + ChlngsOthr + ChlngsFulOvrFlw + ChlngsNoWtr +
              ChlngsSmels + ChlngsOK,
            data = data,
            family = binomial(link = "logit"),
            na.action = na.omit); summary(model); anova(model, test = "Chisq"); pR2(model)
model = glm(formula = IntndPitFullDes ~ Dist + CGend +
              IDPoor + IDPoorTyp + LivRP + VillOD + FreqNeiToi + AdltUseLat +
              ChldUseLat + InfLatDump + Chlngs + Satis + Rec +
              SatisSup + RecSup + Yr + Mnth + YrMnth + RDefBefor_BshFld +
              RDefBefor_RivPnd + RDefBefor_NeiToi + Rain.mm + ChlngsNoFlsh +
              ChlngsFlood + ChlngsOthr + ChlngsFulOvrFlw + ChlngsNoWtr +
              ChlngsSmels + ChlngsOK,
            data = data,
            family = binomial(link = "logit"),
            na.action = na.omit); summary(model); anova(model, test = "Chisq"); pR2(model)
model = glm(formula = IntndPitFullDes ~ Comm + CGend +
              IDPoor + IDPoorTyp + LivRP + VillOD + FreqNeiToi + AdltUseLat +
              ChldUseLat + InfLatDump + Chlngs + Satis + Rec +
              SatisSup + RecSup + Yr + Mnth + YrMnth + RDefBefor_BshFld +
              RDefBefor_RivPnd + RDefBefor_NeiToi + Rain.mm + ChlngsNoFlsh +
              ChlngsFlood + ChlngsOthr + ChlngsFulOvrFlw + ChlngsNoWtr +
              ChlngsSmels + ChlngsOK,
            data = data,
            family = binomial(link = "logit"),
            na.action = na.omit); summary(model); anova(model, test = "Chisq"); pR2(model)
model = glm(formula = IntndPitFullDes ~ Prov + Dist + CGend +
              IDPoor + IDPoorTyp + LivRP + VillOD + FreqNeiToi + AdltUseLat +
              ChldUseLat + InfLatDump + Chlngs + Satis + Rec +
              SatisSup + RecSup + Yr + Mnth + YrMnth + RDefBefor_BshFld +
              RDefBefor_RivPnd + RDefBefor_NeiToi + Rain.mm + ChlngsNoFlsh +
              ChlngsFlood + ChlngsOthr + ChlngsFulOvrFlw + ChlngsNoWtr +
              ChlngsSmels + ChlngsOK,
            data = data,
            family = binomial(link = "logit"),
            na.action = na.omit); summary(model); anova(model, test = "Chisq"); pR2(model)
model = glm(formula = IntndPitFullDes ~ Prov + Dist + Comm + CGend +
              IDPoor + IDPoorTyp + LivRP + VillOD + FreqNeiToi + AdltUseLat +
              ChldUseLat + InfLatDump + Chlngs + Satis + Rec +
              SatisSup + RecSup + Yr + Mnth + YrMnth + RDefBefor_BshFld +
              RDefBefor_RivPnd + RDefBefor_NeiToi + Rain.mm + ChlngsNoFlsh +
              ChlngsFlood + ChlngsOthr + ChlngsFulOvrFlw + ChlngsNoWtr +
              ChlngsSmels + ChlngsOK,
            data = data,
            family = binomial(link = "logit"),
            na.action = na.omit); summary(model); anova(model, test = "Chisq"); pR2(model)
# names(data)
# p > 0.10: CGend, IDPoorTyp, ChldUseLat, InfLatDump, Rec, SatisSup, YrMnth, 
#           RDefBefor_RivPnd, RDefBefor_NeiToi, Rain.mm, ChlngsOK, ChlngsOthr,
#           ChlngsSmels, ChlngsNoFlsh, ChlngsFulOvrFlw
# p < 0.10: LivRP
# Unavailable when predicting IntndPitFullDes: Yr

# Statistically relevant and trending variables from above models
model1 = glm(formula = IntndPitFullDes ~ Prov + IDPoor + LivRP + VillOD +
              FreqNeiToi + AdltUseLat + Satis + SatisSup + RecSup + Mnth + 
              RDefBefor_BshFld + ChlngsFlood + ChlngsNoWtr,
            data = data,
            family = binomial(link = "logit"),
            na.action = na.omit); summary(model1); anova(model1, test = "Chisq"); pR2(model1)
model2 = glm(formula = IntndPitFullDes ~ Prov + Dist + IDPoor + LivRP + VillOD +
               FreqNeiToi + AdltUseLat + Satis + SatisSup + RecSup + Mnth + 
               RDefBefor_BshFld + ChlngsFlood + ChlngsNoWtr,
             data = data,
             family = binomial(link = "logit"),
             na.action = na.omit); summary(model2); anova(model2, test = "Chisq"); pR2(model2)

# Subset data to specific province and run model3
summary(data$Prov)
data = subset(data, Prov == "Kampong Thom")
data = droplevels(data)
summary(data)
model3 = glm(formula = IntndPitFullDes ~ Dist + 
               IDPoor + LivRP + VillOD + FreqNeiToi + AdltUseLat +
               Satis + 
               SatisSup + RecSup + Mnth + YrMnth + RDefBefor_BshFld,
             data = data,
             family = binomial(link = "logit"),
             na.action = na.omit); summary(model3); anova(model3, test = "Chisq"); pR2(model3)

###############################################################################
# TEST ACCURACY OF MODELS OF IntndPitFullDes
###############################################################################
iter = 100
iter = 1:iter
accuracy1 = 0
# accuracy2 = 0
for (i in iter) {
  print(i)
  
  continue = 0
  while (continue == 0) {
    
    # Create training and testing data sets using random sampling
    percent_train = 0.95
    indices_all = 1:length(data[,1])
    indices_train = sort(sample(x = indices_all, 
                                size = round(percent_train*length(data[,1]), 
                                             0), replace = F), decreasing = F)
    indices_test = indices_all[!(indices_all %in% indices_train)]
    # data.frame(indices_train[1:100], indices_test[1:100])
    train = data[indices_train,]; test = data[indices_test,]
    # print(summary(train)); print(summary(test))
    
    # Check if levels
    continue = 1
  }
  
  # Run model1
  model1 = try(glm(formula = IntndPitFullDes ~ Prov + IDPoor + LivRP + VillOD +
                     FreqNeiToi + AdltUseLat + Satis + SatisSup + RecSup + Mnth + 
                     RDefBefor_BshFld + ChlngsFlood + ChlngsNoWtr,
                   data = train,
                   family = binomial(link = "logit"),
                   na.action = na.omit), silent = T)
  # model1 = glm(formula = IntndPitFullDes ~ Dist + IDPoor + LivRP + VillOD +
  #                FreqNeiToi + AdltUseLat + Satis + SatisSup + RecSup + Mnth + 
  #                RDefBefor_BshFld + ChlngsFlood + ChlngsNoWtr,
  #              data = train,
  #              family = binomial(link = "logit"),
  #              na.action = na.omit)
  if (inherits(model1, "try-error")) {
    print("error1")
    next
  }
  # if (i == 1) {
  #   print(summary(model1))
  #   print(anova(model1, test = "Chisq"))
  #   print(pR2(model1))
  # }
  
  # Run model2
  # model2 = glm(formula = IntndPitFullDes ~ Prov + Dist + IDPoor + LivRP + VillOD +
  #                FreqNeiToi + AdltUseLat + Satis + SatisSup + RecSup + Mnth + 
  #                RDefBefor_BshFld + ChlngsFlood + ChlngsNoWtr,
  #              data = train,
  #              family = binomial(link = "logit"),
  #              na.action = na.omit)
  # print(summary(model2))
  # print(anova(model2, test = "Chisq"))
  # print(pR2(model2))
  
  # Test predictive power of model1 on test data
  fitted.results1 = try(predict(object = model1, newdata = test,
                               type = "response"), silent = T)
  if (inherits(fitted.results1, "try-error")) {
    print("error2")
    next
  }
  fitted.results1 = ifelse(fitted.results1 > 0.5, 1, 0)
  # data.frame(test$IntndPitFullDes[!is.na(fitted.results)],
  #            fitted.results[!is.na(fitted.results)])
  misclass.error1 = mean(fitted.results1[!is.na(fitted.results1)] !=
                          test$IntndPitFullDes[!is.na(fitted.results1)])
  if (accuracy1 == 0) {
    accuracy1 = 1 - misclass.error1
    # print(accuracy1)
  } else {
    accuracy1 = c(accuracy1, 1 - misclass.error1)
    # print(accuracy1)
  }
  
  # Test predictive power of model2 on test data
  # fitted.results2 = try(predict(object = model2, newdata = test,
  #                               type = "response"), silent = T)
  # if (inherits(fitted.results2, "try-error")) {
  #   next
  # }
  # fitted.results2 = ifelse(fitted.results2 > 0.5, 1, 0)
  # # data.frame(test$IntndPitFullDes[!is.na(fitted.results)],
  # #            fitted.results[!is.na(fitted.results)])
  # misclass.error2 = mean(fitted.results2[!is.na(fitted.results2)] !=
  #                          test$IntndPitFullDes[!is.na(fitted.results2)])
  # if (accuracy2 == 0) { accuracy2 = 1 - misclass.error2 }
  # else {accuracy2 = c(accuracy2, 1 - misclass.error2)}
  
}
print(accuracy1)
print(mean(accuracy1))
# print(accuracy2)

  
# Visualize
# plot1 = ggplot(data, aes(x = IntndPitFullDes,
#                          y = Prov)) + geom_count(); plot1
# plot2 = ggplot(data, aes(x = IntndPitFullDes,
#                          y = Prov, color = CGend)) + geom_count(); plot2
# plot3 = ggplot(data, aes(x = IntndPitFullDes)) + geom_bar(); plot3
