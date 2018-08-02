# Rainfall of Cambodia
# Written by James Harper, PE, ENV SP of the University of Colorado Boulder
# Started February 24, 2018
# Last updated February 24, 2018

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
                 "corrplot", "ggpubr", "gridExtra",
                 "xlsx", "Amelia", "ltm"))
load(file = "iDE_Oct2017.Rdata")
# names(data)

###############################################################################
# ANALYZE DATA AND PLOT RESULTS
###############################################################################
summary(as.factor(data$Rain.mm))
length(data)
missmap(data[45], main = "Missing values vs observed", legend = F)
biserial.cor(data$Rain.mm, data$IntndPitFullDes, use = "complete.obs")
ggplot(data = data, aes(x = as.numeric(Mnth), y = Rain.mm, colour = Prov)) +
  geom_line(aes(colour = Prov), size = 1.5) +
  theme_minimal() +
  labs(x = "Month", y = "Rainfall (mm)", colour = "Provinces") +
  scale_x_continuous(breaks = c(1:12)) +
  coord_cartesian(xlim = c(1, 12), ylim = c(0, max(data$Rain.mm) + 30))
data.sub = subset(data, select = c(Rain.mm, IntndPitFullDes))
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
