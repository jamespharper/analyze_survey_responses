# Data Management Scripts
# Written by James Harper, PE, ENV SP of the University of Colorado Boulder
# Started October 1, 2017; Last updated Aug 8, 2018
###############################################################################
# Retain only rows of interest
data = subset(data, !is.na(Category1))
data = subset(data, select = -c(Category1, Category2))

# Subset data
data = subset(data, Group2 == "Cntrl" & !is.na(EmptyPlanMethds))
data = subset(data, select = -c(IntndChngDich, IntndChng))