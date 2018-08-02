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
                 "pscl"))

###############################################################################
# LOAD, CLEAN AND CATEGORIZE DATA
###############################################################################
# Load data file
load(file = "iDE_Oct2017.RData");
names(data); summary(data$Prov)
data = subset(data, Prov == "Banteay Meanchey")

###############################################################################
# PERFORM GENERALIZED LINEAR MODELING - IntndPitFullDes
###############################################################################
# Subset data to include only rows with responses to IntndPitFullDes
summary(data); length(data[,1])
data_sub = subset(data, !is.na(IntndPitFullDes))
summary(data_sub); length(data_sub[,1])
len = length(data_sub[,1])

# Run generalized linear model repeatedly to calculate average accuracy
iter = 1:10
accuracy = rep(0, times = length(iter))
for (i in iter) {
  
  # Create training (80%) and testing (20%) sets using random sampling
  indices_all = 1:len
  indices_train = sort(sample(x = indices_all, size = round(0.7*len, 0), 
                              replace = F), decreasing = F)
  indices_test = indices_all[!(indices_all %in% indices_train)]
  # data.frame(indices_train[1:100], indices_test[1:100])
  train = data_sub[indices_train,]
  test = data_sub[indices_test,]
  # summary(train); length(train[,1])
  # summary(test); length(test[,1])
  # hist(indices_train); hist(indices_test)
  
  # Summarize training data
  # for (j in 1:length(train)) {
  #   plot(train[, j], main = colnames(train)[j],
  #        ylab = "Count", col = "steelblue")
  # }
  
  ################################
  # INDIVIDUAL VARIABLES
  ################################
  # names(data)
  # [1] "Prov"                 "Dist"                 "Comm"                
  # [4] "Vill"                 "CGend"                "IDPoor"              
  # [7] "IDPoorTyp"            "LivRP"                "VillOD"              
  # [10] "RDefBefor"            "FreqNeiToi"           "IntndChngDich"       
  # [13] "IntndChng"            "AdltUseLat"           "ChldUseLat"          
  # [16] "InfLatDump"           "IntndPitFull"         "Chlngs"              
  # [19] "Satis"                "Rec"                  "SatisSup"            
  # [22] "RecSup"               "Yr"                   "Mnth"                
  # [25] "YrMnth"               "RDefBefor_BshFld"     "RDefBefor_RivPnd"    
  # [28] "RDefBefor_NeiToi"     "RDefBefor_Othr"       "RDefBefor_NAAlwysToi"
  # [31] "IntndChng_Shltr"      "IntndChng_Shwr"       "IntndChng_Sink"      
  # [34] "IntndChng_WtrRes"     "IntndChng_Pit"        "IntndChng_Othr"      
  # [37] "IntndChng_NAAlwysToi" "IntndPitFullDes"      "IntndPitFullPit"     
  # [40] "IntndPitFullEmpSlf"   "IntndPitFullDK"       "IntndPitFullOthr"    
  # [43] "IntndPitFullPay"      "IntndPitFullStop"     "Rain.mm"
  # summary(data)
  data = subset(data, select = -c(IntndChng_NAAlwysToi, RDefBefor_NAAlwysToi,
                                  IntndPitFullOthr, IntndPitFullPit,
                                  IntndChng_Othr, IntndPitFull,
                                  RDefBefor_Othr, IntndChng_Pit,
                                  IntndPitFullDK,
                                  IntndPitFullStop, IntndPitFullPay,
                                  IntndPitFullEmpSlf, RDefBefor))
  
  # Test each individual variable's predictive power of IntndPitFullDes
  df = data.frame(Var = character(), p = double(), RDevNull = double(), RDev = double())
  for (i in 2:length(names(data))) {
    model = glm(formula = paste("IntndPitFullDes ~ ", names(data)[i]),
                data = data,
                family = binomial(link = "logit"),
                na.action = na.omit); summary(model); anova(model, test = "Chisq")
    results = anova(model, test = "Chisq")
    p = results$`Pr(>Chi)`[2]
    rdev.null = results$`Resid. Dev`[1]
    rdev = results$`Resid. Dev`[2]
    df = rbind(df, data.frame(Var = names(data)[i], P = p, RDevNull = rdev.null, RDev = rdev))
  }
  print(unique(data$Prov)[1])
  df
  
  model = glm(formula = IntndPitFullDes ~ Vill,
              data = data,
              family = binomial(link = "logit"),
              na.action = na.omit); summary(model); anova(model, test = "Chisq")
  
  # All Provinces
  # p<.05: LBO, Prov, IDPoor, InfLatDump, Satis, Rec, SatisSup, RecSup, Yr, 
  #        Mnth, RDefBefor_BshFld, RDefBefor_NeiToi, IntndChng_Shwr, 
  #        IntndChng_HndWsh, IntndChng_WtrRes, IntndChng_2pit
  # p>.05: CGend, LivRP, VillOD, AdltUseLat, ChldUseLat, RDefBefor_RivPnd, 
  #        IntndChng_Shltr
  
  # Banteay Meanchey
  # p<0.05: Dist, Comm, Vill, IDPoor, IDPoorTyp, VillOD, FreqNeiToi, 
  #         IntndChng, ChldUseLat, Satis, SatisSup, Yr, Mnth, YrMnth,
  #         RDefBefor_BshFld, IntndChng_Sink
  
  
  ################################
  # ALL PROVINCES
  ################################
  # Test model with all variables to predict IntndPitFullDes
  # names(train)
  # model = glm(formula = IntndPitFullDes ~ Prov + CGend + IDPoor + LivRP + 
  #               VillOD + AdltUseLat + ChldUseLat + InfLatDump + Satis + Rec +
  #               SatisSup + RecSup + Yr + Mnth + RDefBefor_BshFld +
  #               RDefBefor_RivPnd + RDefBefor_NeiToi + IntndChng_Shltr +
  #               IntndChng_Shwr + IntndChng_Sink + IntndChng_WtrRes +
  #               IntndChng_2pit,
  #             data = train,
  #             family = binomial(link = "logit"),
  #             na.action = na.omit); summary(model); anova(model, test = "Chisq")
  # Removed due to low interest: LBO
  # p<.05: Prov, VillOD, Satis, SatisSup, RecSup, RDefBefor_BshFld,
  #        IntndChng_Shwr, IntndChng_HndWsh, IntndChng_WtrRes,
  #        IntndChng_2pit
  # p<.10: IDPoor
  # p>.10: CGend, LivRP, AdltUseLat, ChldUseLat, InfLatDump, Rec, Yr, Mnth
  # Removed because wouldn't be available when predicting FSM intentions: Yr, IntndChng_Pit
  # 
  # Refine model to include only variables that 1) significantly related to
  # IntndPitFull, and 2) reduce residual deviance markedly
  # model = glm(formula = IntndPitFullDes ~ Prov + IDPoor + VillOD + Satis + 
  #               SatisSup + RecSup + Mnth + RDefBefor_BshFld +
  #               IntndChng_Shwr + IntndChng_Sink + IntndChng_WtrRes +
  #               Rain.mm,
  #             data = train,
  #             family = binomial(link = "logit"),
  #             na.action = na.omit)
  
  ################################
  # ONE PROVINCE
  ################################
  # names(data)
  # [1] "Prov"                 "Dist"                 "Comm"                
  # [4] "Vill"                 "CGend"                "IDPoor"              
  # [7] "IDPoorTyp"            "LivRP"                "VillOD"              
  # [10] "RDefBefor"            "FreqNeiToi"           "IntndChngDich"       
  # [13] "IntndChng"            "AdltUseLat"           "ChldUseLat"          
  # [16] "InfLatDump"           "IntndPitFull"         "Chlngs"              
  # [19] "Satis"                "Rec"                  "SatisSup"            
  # [22] "RecSup"               "Yr"                   "Mnth"                
  # [25] "YrMnth"               "RDefBefor_BshFld"     "RDefBefor_RivPnd"    
  # [28] "RDefBefor_NeiToi"     "RDefBefor_Othr"       "RDefBefor_NAAlwysToi"
  # [31] "IntndChng_Shltr"      "IntndChng_Shwr"       "IntndChng_Sink"      
  # [34] "IntndChng_WtrRes"     "IntndChng_Pit"        "IntndChng_Othr"      
  # [37] "IntndChng_NAAlwysToi" "IntndPitFullDes"      "IntndPitFullPit"     
  # [40] "IntndPitFullEmpSlf"   "IntndPitFullDK"       "IntndPitFullOthr"    
  # [43] "IntndPitFullPay"      "IntndPitFullStop"     "Rain.mm"
  
  # Test model with all variables to predict IntndPitFullDes
  # names(train)
  # model = glm(formula = IntndPitFullDes ~ CGend + IDPoor + LivRP +
  #               VillOD + FreqNeiToi + AdltUseLat + ChldUseLat + InfLatDump + Satis + Rec +
  #               SatisSup + RecSup + Yr + Mnth + RDefBefor_BshFld +
  #               RDefBefor_RivPnd + RDefBefor_NeiToi + IntndChng_Shltr +
  #               IntndChng_Shwr + IntndChng_Sink + IntndChng_WtrRes + Rain.mm,
  #             data = train,
  #             family = binomial(link = "logit"),
  #             na.action = na.omit); summary(model); anova(model, test = "Chisq")
  # Removed due to low interest: LBO
  # p<.05: Prov, VillOD, Satis, SatisSup, RecSup, RDefBefor_BshFld,
  #        IntndChng_Shwr, IntndChng_HndWsh, IntndChng_WtrRes,
  #        IntndChng_2pit
  # p<.10: IDPoor
  # p>.10: CGend, LivRP, AdltUseLat, ChldUseLat, InfLatDump, Rec, Yr, Mnth
  # Removed because wouldn't be available when predicting FSM intentions: Yr, IntndChng_Pit
  
  # BANTEAY MEANCHEY
  # Refine model to include only variables that 1) significantly related to
  # IntndPitFull, and 2) reduce residual deviance markedly
  model = glm(formula = IntndPitFullDes ~ Dist + Comm + IDPoor + VillOD + 
                IntndChng + ChldUseLat + Satis + SatisSup + Yr + Mnth + RDefBefor_BshFld + IntndChng_Sink,
              data = data,
              family = binomial(link = "logit"),
              na.action = na.omit); summary(model); anova(model, test = "Chisq"); pR2(model)
  
  # Analyze model fit
  # summary(model)
  # anova(model, test = "Chisq")
  # pR2(model)
  
  # Test predictive power of model on testing data
  predict_results = function(model, test, type) {
    fitted.results = predict(model, newdata = test, type = type)
    return(fitted.results)
  }
  fitted.results = try(predict_results(model, test, "response"), silent = T)
  if (inherits(fitted.results, "try-error")) {
    next
  }
  fitted.results = ifelse(fitted.results > 0.5, 1, 0)
  # data.frame(test$IntndPitFullDes[!is.na(fitted.results)], 
  #            fitted.results[!is.na(fitted.results)])
  misclass.error = mean(fitted.results[!is.na(fitted.results)] != 
                          test$IntndPitFullDes[!is.na(fitted.results)])
  accuracy[i] = 1 - misclass.error
  
}
head(accuracy)
# hist(accuracy)
mean(accuracy[accuracy != 0])

# Visualization
plot1 = ggplot(data, aes(x = IntndPitFullDes, 
                         y = Prov)) + geom_count(); plot1
plot2 = ggplot(data, aes(x = IntndPitFullDes, 
                         y = Prov, color = CGend)) + geom_count(); plot2
plot3 = ggplot(data, aes(x = IntndPitFullDes)) + geom_bar(); plot3

#Examples
# counts <- c(18,17,15,20,10,20,25,13,12)
# outcome <- gl(3,1,9)
# treatment <- gl(3,3)
# print(d.AD <- data.frame(treatment, outcome, counts))
# glm.D93 <- glm(counts ~ outcome + treatment, family = poisson())
# anova(glm.D93)
# summary(glm.D93)
# anova(glm.D93, test = "Cp")
# anova(glm.D93, test = "Chisq")
# glm.D93a <- update(glm.D93, ~treatment*outcome) # equivalent to Pearson Chi-square
# anova(glm.D93, glm.D93a, test = "Rao")
# 
# data("Titanic")
# data = Titanic
# train <- data[1:800,]
# test <- data[801:889,]
# model <- glm(Survived ~.,family=binomial(link='logit'),data=train)
# 
# data(mtcars)
# mtcars

# Did not find to be useful for my data
# confint(model) # 95% CI for the coefficients
# exp(coef(model)) # exponentiated coefficients
# exp(confint(model)) # 95% CI for exponentiated coefficients
# predict(model, type="response") # predicted values
# residuals(model, type="deviance") # residuals


###############################################################################
# PERFORM GENERALIZED LINEAR MODELING - IntndPitFull2pit or IntndPitFullEmpSlf
###############################################################################
# Subset data to include only rows with responses to IntndPitFullDes
summary(data); length(data[,1])
data_sub = subset(data, !is.na(IntndPitFullEmpSlf))
summary(data_sub); length(data_sub[,1])
len = length(data_sub[,1])

# Run generalized linear model repeatedly to calculate average accuracy
iter = 1:100
accuracy = rep(0, times = length(iter))
for (i in iter) {
  
  # Create training (80%) and testing (20%) sets using random sampling
  indices_all = 1:len
  indices_train = sort(sample(x = indices_all, size = round(0.7*len, 0), 
                              replace = F), decreasing = F)
  indices_test = indices_all[!(indices_all %in% indices_train)]
  # data.frame(indices_train[1:100], indices_test[1:100])
  train = data_sub[indices_train,]
  test = data_sub[indices_test,]
  # summary(train); length(train[,1])
  # summary(test); length(test[,1])
  # hist(indices_train); hist(indices_test)
  
  # Summarize training data
  # for (j in 1:length(train)) {
  #   plot(train[, j], main = colnames(train)[j],
  #        ylab = "Count", col = "steelblue")
  # }
  
  # List names in data
  # names(data_sub)
  # [1] "LBO"                  "Prov"                 "CGend"                "IDPoor"              
  # [5] "LivRP"                "VillOD"               "AdltUseLat"           "ChldUseLat"          
  # [9] "InfLatDump"           "IntndPitFull"         "Satis"                "Rec"                 
  # [13] "SatisSup"             "RecSup"               "Yr"                   "Mnth"                
  # [17] "RDefBefor_BshFld"     "RDefBefor_RivPnd"     "RDefBefor_NeiToi"     "RDefBefor_Othr"      
  # [21] "RDefBefor_NAAlwysToi" "IntndChng_Shltr"      "IntndChng_Shwr"       "IntndChng_HndWsh"    
  # [25] "IntndChng_WtrRes"     "IntndChng_2pit"       "IntndChng_Othr"       "IntndChng_NAAlwysToi"
  # [29] "IntndPitFullDes"
  
  # Test individual variables predictive power of IntndPitFullDes
  # model = glm(formula = IntndPitFullDes ~ RDefBefor_BshFld,
  #             data = train,
  #             family = binomial(link = "logit"),
  #             na.action = na.omit); summary(model); anova(model, test = "Chisq")
  # names(train)
  # p<.05: LBO, Prov, IDPoor, InfLatDump, Satis, Rec, SatisSup, RecSup, Yr, 
  #        Mnth, RDefBefor_BshFld, RDefBefor_NeiToi, IntndChng_Shwr, 
  #        IntndChng_HndWsh, IntndChng_WtrRes, IntndChng_2pit
  # p>.05: CGend, LivRP, VillOD, AdltUseLat, ChldUseLat, RDefBefor_RivPnd, 
  #        IntndChng_Shltr
  
  # Test model with all variables to predict IntndPitFullDes
  # names(train)
  # model = glm(formula = IntndPitFullDes ~ Prov + CGend + IDPoor + LivRP + 
  #               VillOD + AdltUseLat + ChldUseLat + InfLatDump + Satis + Rec +
  #               SatisSup + RecSup + Yr + Mnth + RDefBefor_BshFld +
  #               RDefBefor_RivPnd + RDefBefor_NeiToi + IntndChng_Shltr +
  #               IntndChng_Shwr + IntndChng_HndWsh + IntndChng_WtrRes +
  #               IntndChng_2pit,
  #             data = train,
  #             family = binomial(link = "logit"),
  #             na.action = na.omit); summary(model); anova(model, test = "Chisq")
  # Removed due to low interest: LBO
  # p<.05: Prov, VillOD, Satis, SatisSup, RecSup, RDefBefor_BshFld,
  #        IntndChng_Shwr, IntndChng_HndWsh, IntndChng_WtrRes,
  #        IntndChng_2pit
  # p<.10: IDPoor
  # p>.10: CGend, LivRP, AdltUseLat, ChldUseLat, InfLatDump, Rec, Yr, Mnth, 
  
  # Refine model to include only variables that 1) significantly related to
  # IntndPitFull, adn 2) reduce residual deviance markedly
  model = glm(formula = IntndPitFullEmpSlf ~ Prov + IDPoor + VillOD + Satis + 
                SatisSup + RecSup + Yr + Mnth + RDefBefor_BshFld +
                IntndChng_Shwr + IntndChng_HndWsh + IntndChng_WtrRes +
                IntndChng_2pit,
              data = train,
              family = binomial(link = "logit"),
              na.action = na.omit)
  
  # Analyze model fit
  # summary(model)
  # anova(model, test = "Chisq")
  # pR2(model)
  
  # Test predictive power of model on testing data
  predict_results = function(model, test, type) {
    fitted.results = predict(model, newdata = test, type = type)
    return(fitted.results)
  }
  fitted.results = try(predict_results(model, test, "response"), silent = T)
  if (inherits(fitted.results, "try-error")) {
    next
  }
  fitted.results = ifelse(fitted.results > 0.5, 1, 0)
  # data.frame(test$IntndPitFullDes[!is.na(fitted.results)], 
  #            fitted.results[!is.na(fitted.results)])
  misclass.error = mean(fitted.results[!is.na(fitted.results)] != 
                          test$IntndPitFullDes[!is.na(fitted.results)])
  accuracy[i] = 1 - misclass.error
  
}
head(accuracy)
# hist(accuracy)
mean(accuracy[accuracy != 0])

# Visualization
plot1 = ggplot(data, aes(x = IntndPitFullDes, 
                         y = Prov)) + geom_count(); plot1
plot2 = ggplot(data, aes(x = IntndPitFullDes, 
                         y = Prov, color = CGend)) + geom_count(); plot2
plot3 = ggplot(data, aes(x = IntndPitFullDes)) + geom_bar(); plot3
