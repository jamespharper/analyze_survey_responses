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
data = droplevels(data)

###############################################################################
# PERFORM GENERALIZED LINEAR MODELING - IntndPitFullDes
###############################################################################
# Subset data to include only rows with responses to IntndPitFullDes
# summary(data); length(data[,1])
data_sub = subset(data, !is.na(IntndPitFullDes))
# summary(data_sub); length(data_sub[,1])
len = length(data_sub[,1])

# Run generalized linear model repeatedly to calculate average accuracy
iter = 1:10
accuracy = rep(0, times = length(iter))
for (i in iter) {
  
  # Create training (70%) and testing (30%) sets using random sampling
  indices_all = 1:len
  indices_train = sort(sample(x = indices_all, size = round(0.7*len, 0), 
                              replace = F), decreasing = F)
  indices_test = indices_all[!(indices_all %in% indices_train)]
  # data.frame(indices_train[1:100], indices_test[1:100])
  train = data_sub[indices_train,]
  test = data_sub[indices_test,]
  
  # Refine model to include only variables that 1) significantly related to
  # IntndPitFull, and 2) reduce residual deviance markedly
  # summary(train)
  model = glm(formula = IntndPitFullDes ~ IDPoor + VillOD + Satis +
                SatisSup + RecSup + Mnth + RDefBefor_BshFld +
                Rain.mm,
              data = train,
              family = binomial(link = "logit"),
              na.action = na.omit)
  
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
