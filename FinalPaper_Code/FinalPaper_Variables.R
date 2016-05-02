# Collaborative Social Sience Data - Pair Assignment 3
# Variables

library(DataCombine)

# Set Working Directory
try(setwd("/Users/Lukas/Documents/Git/FinalPaper/"),silent=TRUE)
try(setwd("C:/Users/Dani/Documents/GitHub2/FinalPaper/"),silent=TRUE)
getwd()

# Dynamical Link to first R script file
source("FinalPaper_Code/FinalPaper_CleaningMerging.R")

#delete obsolete columns
merge10$iso3c <- NULL
merge10$iso3c.x <- NULL
merge10$iso3c.y <- NULL
merge10$V4 <- NULL
merge10$V5 <- NULL

# define year and Date as numeric
merge10$year <- as.factor(merge10$year)
merge10$Date <- gsub("Q",".",merge10$Date)
merge10$Date <- as.numeric(merge10$Date)

# replace NA's for ECB after 1998.4
sel <- which(merge10$Date>1998.4) 
merge10$ECB.MRO.change[sel][is.na(merge10$ECB.MRO.change[sel])] <- 0
merge10$ECB.dep.change[sel][is.na(merge10$ECB.dep.change[sel])] <- 0

rm(sel)

# creating time lags
# lag variables by one time period (one quarter year)

var <- c("USA.GDP", "JPN.GDP", "GBR.GDP", "FRA.GDP", "DEU.GDP", "USA.unempl", "JPN.unempl", "GBR.unempl",
         "FRA.unempl", "DEU.unempl", "USA.prvconsm", "JPN.prvconsm", "GBR.prvconsm", "FRA.prvconsm",
         "DEU.prvconsm", "WTI.dollar.change", "Brent.dollar.change", "ECB.MRO.change", "ECB.dep.change")

for (i in var) {
  merge10[, paste0("L.", i)] <- DataCombine::shift(merge10[, i], shiftBy = -1)
}

rm(i, var)

# creating new outcome variable: percentage change of stock indices
var <- c("CAC.Close", "DAX.Close", "FTSE.Close", "NIK.Close")

for (i in var)
merge10 <- change(merge10, Var = i,
                      type = 'percent',
                      NewVar = paste0(i, ".change"),
                      slideBy = -1)

rm(i, var)

# re-order columns
merge10 <- merge10[ ,c(1,3,2,4:68)]
