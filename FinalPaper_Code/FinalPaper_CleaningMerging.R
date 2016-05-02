# Collaborative Social Sience Data - Pair Assignment 3
# Data

library(countrycode)
library(WDI)
library(plyr)
library(reshape2)
library(zoo)

# Set Working Directory
try(setwd("/Users/Lukas/Documents/Git/FinalPaper/FinalPaper_Data"),silent=TRUE)
try(setwd("C:/Users/Dani/Documents/GitHub2/FinalPaper/FinalPaper_Data"),silent=TRUE)
getwd()

# quarterly oil price change: Brent
brent <- read.csv("DCOILBRENTEU.csv", header = FALSE, sep = ",", stringsAsFactors = FALSE, na.strings = c("", "NA"))
brent <- brent[-1, ]
brent$Date <- as.yearqtr(brent$V1, format = "%Y-%m-%d")
format(brent$Date, format = "%y/0%q")
brent$Date <- gsub("[^a-zA-Z0-9]","",brent$Date) #get rid of special characters
brent$V1 <- NULL
names(brent)[1] <- 'Brent.dollar.change'
brent$Brent.dollar.change <- as.numeric(brent$Brent.dollar.change)


# quarterly oil price change: WTI
wti <- read.csv("DCOILWTICO.csv", header = FALSE, sep = ",", stringsAsFactors = FALSE, na.strings = c("", "NA"))
wti <- wti[-1, ]
wti$Date <- as.yearqtr(wti$V1, format = "%Y-%m-%d")
format(wti$Date, format = "%y/0%q")
wti$Date <- gsub("[^a-zA-Z0-9]","",wti$Date) #get rid of special characters
wti$V1 <- NULL
names(wti)[1] <- 'WTI.dollar.change'
wti$WTI.dollar.change <- as.numeric(wti$WTI.dollar.change)


# main refinancing operation (ECB)
MRO <- read.csv("MainRefinancingOperations.csv", header = FALSE, sep = ",", stringsAsFactors = FALSE, na.strings = c("", "NA"))
MRO <- MRO[-1, ]
MRO <- MRO[-1, ]
MRO <- MRO[-1, ]
MRO <- MRO[-1, ]
MRO <- MRO[-1, ]

names(MRO)[1] <- 'time'
names(MRO)[2] <- 'ECB.MRO'

MRO$ECB.MRO <- as.numeric(MRO$ECB.MRO)

MRO$Date <- as.yearqtr(MRO$time, format = "%Y-%m-%d")
format(MRO$Date, format = "%y/0%q")
MRO$Date <- gsub("[^a-zA-Z0-9]","",MRO$Date) #get rid of special characters

MRO$new <- MRO$Date
MRO <- MRO[ ,c(1,3,4,2)]
aggMRO <- dcast(MRO, Date ~ new, mean) #p317 R for Dummies
aggMRO[is.na(aggMRO)] <- 0
aggMRO$ECB.MRO.change <- rowSums(aggMRO[2:33])
aggMRO <- aggMRO[ ,c(1,34,2:33)]
aggMRO <- aggMRO[order(aggMRO$Date), ]
aggMRO <- aggMRO[ ,-(3:34)]
rm(MRO)

# deposit facility (ECB)
deposit <- read.csv("DepositFacility.csv", header = FALSE, sep = ",", stringsAsFactors = FALSE, na.strings = c("", "NA"))
deposit <- deposit[-1, ]
deposit <- deposit[-1, ]
deposit <- deposit[-1, ]
deposit <- deposit[-1, ]
deposit <- deposit[-1, ]

names(deposit)[1] <- 'time'
names(deposit)[2] <- 'ECB.depofacil'

deposit$ECB.depofacil <- as.numeric(deposit$ECB.depofacil)

deposit$Date <- as.yearqtr(deposit$time, format = "%Y-%m-%d")
format(deposit$Date, format = "%y/0%q")
deposit$Date <- gsub("[^a-zA-Z0-9]","",deposit$Date) #get rid of special characters

deposit$new <- deposit$Date
deposit <- deposit[ ,c(1,3,4,2)]
dep <- dcast(deposit, Date ~ new, mean) #p317 R for Dummies
dep[is.na(dep)] <- 0
dep$ECB.dep.change <- rowSums(dep[2:35])
dep <- dep[ ,c(1,36,2:35)]
dep <- dep[order(dep$Date), ]
dep <- dep[ ,-(3:36)]
rm(deposit)


# quarterly GDP growth (OECD)
GDPq <- read.csv("QNA_07042016182022982.csv", header = FALSE, sep = ",", stringsAsFactors = FALSE, na.strings = c("", "NA"))
GDPq <- GDPq[-1,]
GDPq$GPSA <- 0
GDPq$GPSA[which(GDPq$V5=="GPSA")] <- 1
GDPq$GDP <- 0
GDPq$GDP[which(GDPq$V4=="Gross domestic product - expenditure approach")] <- 1
GDPq$year <- GDPq$V9
GDPq$year <- gsub("\\-.*","",GDPq$year)

names(GDPq)[1] <- 'iso3c'
names(GDPq)[2] <- 'country'

GDPq$V3 <- NULL
GDPq$V6 <- NULL
GDPq$V7 <- NULL
GDPq$V8 <- NULL
GDPq$V10 <- NULL
GDPq$V11 <- NULL
GDPq$V12 <- NULL
GDPq$V13 <- NULL
GDPq$V14 <- NULL
GDPq$V15 <- NULL
GDPq$V16 <- NULL
GDPq$V18 <- NULL
GDPq$V19 <- NULL

names(GDPq)[5] <- 'Date'
GDPq$Date <- gsub("[^a-zA-Z0-9]","",GDPq$Date) #get rid of special characters
names(GDPq)[6] <- 'GDPq.gr'
GDPq$GDPq.gr <- as.numeric(GDPq$GDPq.gr)

sub <- subset(GDPq, GPSA > 0)
GPSA <- subset(sub, GDP > 0)
GPSA$GPSA <- NULL
GPSA$GDP <- NULL
rm(GDPq, sub)


# consumption spending
consume <- read.csv("KEI_07042016174245293.csv", header = FALSE, sep = ",", stringsAsFactors = FALSE, na.strings = c("", "NA"))
consume <- consume[-1,]

names(consume)[3] <- 'iso3c'
names(consume)[4] <- 'country'
consume$computation <- 0
consume$computation[which(consume$V5=="GP")] <- 1

consume$V1 <- NULL
consume$V2 <- NULL
consume$V5 <- NULL
consume$V6 <- NULL
consume$V7 <- NULL
consume$V8 <- NULL
consume$V10 <- NULL
consume$V11 <- NULL
consume$V12 <- NULL
consume$V13 <- NULL
consume$V14 <- NULL
consume$V15 <- NULL
consume$V16 <- NULL
consume$V18 <- NULL
consume$V19 <- NULL

names(consume)[3] <- 'Date'
consume$Date <- gsub("[^a-zA-Z0-9]","",consume$Date) #get rid of special characters
consume$consumption.spending <- as.numeric(consume$V17)
consume$V17 <- NULL

prvconsm <- subset(consume, computation > 0)
prvconsm$computation <- NULL
rm(consume)


# rate of unemployment (OECD)
unempl <- read.csv("KEI_07042016174849777.csv", header = FALSE, sep = ",", stringsAsFactors = FALSE, na.strings = c("", "NA"))
unempl <- unempl[-1,]

names(unempl)[3] <- 'iso3c'
names(unempl)[4] <- 'country'

unempl$V1 <- NULL
unempl$V2 <- NULL
unempl$V5 <- NULL
unempl$V8 <- NULL

names(unempl)[5] <- 'Date'
unempl$Date <- gsub("[^a-zA-Z0-9]","",unempl$Date) #get rid of special characters
unempl$unempl <- as.numeric(unempl$V17)
unempl$V6 <- NULL
unempl$V7 <- NULL
unempl$V10 <- NULL
unempl$V11 <- NULL
unempl$V12 <- NULL
unempl$V13 <- NULL
unempl$V14 <- NULL
unempl$V15 <- NULL
unempl$V16 <- NULL
unempl$V17 <- NULL
unempl$V18 <- NULL
unempl$V19 <- NULL


# Germany
URL <- "http://real-chart.finance.yahoo.com/table.csv?s=%5EGDAXI&d=3&e=8&f=2016&g=d&a=10&b=26&c=1990&ignore=.csv"
DAX <- read.csv(URL)
colnames(DAX) <- paste("DAX", colnames(DAX), sep = ".")
names(DAX)[1] <- 'Day'
DAX$Date <- as.yearqtr(DAX$Day, format = "%Y-%m-%d")
format(DAX$Date, format = "%y/0%q")
DAX$Date <- gsub("[^a-zA-Z0-9]","",DAX$Date) #get rid of special characters
DAX <- ddply(DAX, .(Date), function(DAX) c(DAX.Open=mean(DAX$DAX.Open), DAX.High=mean(DAX$DAX.High), DAX.Low=mean(DAX$DAX.Low), DAX.Close=mean(DAX$DAX.Close), DAX.Volume=mean(DAX$DAX.Volume), DAX.Adj.Close=mean(DAX$DAX.Adj.Close)))


# Japan
URL <- "http://real-chart.finance.yahoo.com/table.csv?s=%5EN225&a=00&b=1&c=1991&d=03&e=8&f=2016&g=d&ignore=.csv"
NIKKEI <- read.csv(URL)
colnames(NIKKEI) <- paste("NIK", colnames(NIKKEI), sep = ".")
names(NIKKEI)[1] <- 'Day'
NIKKEI$Date <- as.yearqtr(NIKKEI$Day, format = "%Y-%m-%d")
format(NIKKEI$Date, format = "%y/0%q")
NIKKEI$Date <- gsub("[^a-zA-Z0-9]","",NIKKEI$Date) #get rid of special characters
NIKKEI <- ddply(NIKKEI, .(Date), function(NIKKEI) c(NIK.Open=mean(NIKKEI$NIK.Open), NIK.High=mean(NIKKEI$NIK.High), NIK.Low=mean(NIKKEI$NIK.Low), NIK.Close=mean(NIKKEI$NIK.Close), NIK.Volume=mean(NIKKEI$NIK.Volume), NIK.Adj.Close=mean(NIKKEI$NIK.Adj.Close)))

# Great Britain
URL <- "http://real-chart.finance.yahoo.com/table.csv?s=%5EFTSE&a=00&b=1&c=1991&d=03&e=8&f=2016&g=d&ignore=.csv"
FTSE <- read.csv(URL)
colnames(FTSE) <- paste("FTSE", colnames(FTSE), sep = ".")
names(FTSE)[1] <- 'Day'
FTSE$Date <- as.yearqtr(FTSE$Day, format = "%Y-%m-%d")
format(FTSE$Date, format = "%y/0%q")
FTSE$Date <- gsub("[^a-zA-Z0-9]","",FTSE$Date) #get rid of special characters
FTSE <- ddply(FTSE, .(Date), function(FTSE) c(FTSE.Open=mean(FTSE$FTSE.Open), FTSE.High=mean(FTSE$FTSE.High), FTSE.Low=mean(FTSE$FTSE.Low), FTSE.Close=mean(FTSE$FTSE.Close), FTSE.Volume=mean(FTSE$FTSE.Volume), FTSE.Adj.Close=mean(FTSE$FTSE.Adj.Close)))

# France
URL <- "http://real-chart.finance.yahoo.com/table.csv?s=%5EFCHI&a=00&b=1&c=1991&d=03&e=8&f=2016&g=d&ignore=.csv"
CAC <- read.csv(URL)
colnames(CAC) <- paste("CAC", colnames(CAC), sep = ".")
names(CAC)[1] <- 'Day'
CAC$Date <- as.yearqtr(CAC$Day, format = "%Y-%m-%d")
format(CAC$Date, format = "%y/0%q")
CAC$Date <- gsub("[^a-zA-Z0-9]","",CAC$Date) #get rid of special characters
CAC <- ddply(CAC, .(Date), function(CAC) c(CAC.Open=mean(CAC$CAC.Open), CAC.High=mean(CAC$CAC.High), CAC.Low=mean(CAC$CAC.Low), CAC.Close=mean(CAC$CAC.Close), CAC.Volume=mean(CAC$CAC.Volume), CAC.Adj.Close=mean(CAC$CAC.Adj.Close)))

rm(URL)

USA <- subset(GPSA, iso3c == "USA")
DEU <- subset(GPSA, iso3c == "DEU")
GBR <- subset(GPSA, iso3c == "GBR")
JPN <- subset(GPSA, iso3c == "JPN")
FRA <- subset(GPSA, iso3c == "FRA")
names(USA)[6] <- 'USA.GDP'
names(DEU)[6] <- 'DEU.GDP'
names(GBR)[6] <- 'GBR.GDP'
names(JPN)[6] <- 'JPN.GDP'
names(FRA)[6] <- 'FRA.GDP'

dfs <- list(USA,DEU,GBR,JPN,FRA)

merge0 <- join_all(dfs,by=c("Date"),  type = "full", match = "first")

rm(USA, DEU, GBR, JPN, FRA, GPSA, dfs)

# merge the data sets
merge1 <- merge(merge0,CAC,by=c("Date"), all.x = TRUE)
merge2 <- merge(merge1,DAX,by=c("Date"), all.x = TRUE)
merge3 <- merge(merge2,FTSE,by=c("Date"), all.x = TRUE)
merge4 <- merge(merge3,NIKKEI,by=c("Date"), all.x = TRUE)
merge4$country <- NULL
rm(CAC, DAX, FTSE, NIKKEI, merge0, merge1, merge2, merge3)

USA <- subset(unempl, iso3c == "USA")
DEU <- subset(unempl, iso3c == "DEU")
GBR <- subset(unempl, iso3c == "GBR")
JPN <- subset(unempl, iso3c == "JPN")
FRA <- subset(unempl, iso3c == "FRA")
names(USA)[4] <- 'USA.unempl'
names(DEU)[4] <- 'DEU.unempl'
names(GBR)[4] <- 'GBR.unempl'
names(JPN)[4] <- 'JPN.unempl'
names(FRA)[4] <- 'FRA.unempl'

dfs <- list(USA,DEU,GBR,JPN,FRA)

unempl2 <- join_all(dfs,by=c("Date"),  type = "full", match = "first")

rm(USA, DEU, GBR, JPN, FRA, dfs)

merge5 <- merge(merge4,unempl2,by=c("Date"), all.x = TRUE)
merge5$country <- NULL
rm(merge4, unempl, unempl2)

USA <- subset(prvconsm, iso3c == "USA")
DEU <- subset(prvconsm, iso3c == "DEU")
GBR <- subset(prvconsm, iso3c == "GBR")
JPN <- subset(prvconsm, iso3c == "JPN")
FRA <- subset(prvconsm, iso3c == "FRA")
names(USA)[4] <- 'USA.prvconsm'
names(DEU)[4] <- 'DEU.prvconsm'
names(GBR)[4] <- 'GBR.prvconsm'
names(JPN)[4] <- 'JPN.prvconsm'
names(FRA)[4] <- 'FRA.prvconsm'

dfs <- list(USA,DEU,GBR,JPN,FRA)

prvconsm2 <- join_all(dfs,by=c("Date"),  type = "full", match = "first")

rm(USA, DEU, GBR, JPN, FRA, dfs)

merge6 <- merge(merge5,prvconsm2,by=c("Date"), all.x = TRUE)
merge6$country <- NULL
rm(merge5, prvconsm, prvconsm2)

merge7 <- merge(merge6,wti,by=c("Date"), all.x = TRUE)
rm(wti, merge6)

merge8 <- merge(merge7,brent,by=c("Date"), all.x = TRUE)
rm(brent, merge7)

merge9 <- merge(merge8,aggMRO,by=c("Date"), all.x = TRUE)
rm(aggMRO, merge8)

merge10 <- merge(merge9,dep,by=c("Date"), all.x = TRUE)
rm(dep, merge9)
