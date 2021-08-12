rm(list = ls())
library(readr)
library(dplyr)
library(dummies)
library(stringr)
library(sandwich)
library(lmtest)
library(mice)
library(imputeTS)
library(modelr)

setwd('AUC Course by Course/Capstone/Data')

DischargeData <- read.csv('DischargeData.csv')
PrecipitationData <- read.csv('PrecipitationData.csv')
#need from PrecipitationData:
#     - column year
#     - column station
#     - column average (spatial AND temporal) precipitation

PrecipitationData$X <- NULL
PrecipitationData$stationyear <- NULL
#delete leftover columns

trimmedprecipitation <- data.frame(station = double(),
                                   year = double(),
                                   basin = double(),
                                   precipitation = double())

fullymissingstations <- vector()

for(stationnumber in unique(PrecipitationData$station)){
  perstation <- filter(PrecipitationData, station == stationnumber)
  #filter for the current iteration of the station to obtain a time series for that station
  perstation <- arrange(perstation, year)
  #order the time series from old to new
  if(!(length(perstation$precipitation) == sum(is.na(perstation$precipitation)))){
    tstart <- perstation$year[!is.na(perstation$precipitation)[1]]
    tend <- perstation$year[!is.na(perstation$precipitation)[-1]]
    #find and store the index of the first and last non-na entries in precipitation to get a "start year" and "end year"
    perstation <- filter(perstation, year >= tstart & year <= tend)
    #discard all the observations before the start year
    perstation$precipitation <- na_interpolation(perstation$precipitation)
    #interpolate remaining missing values
    trimmedprecipitation <- bind_rows(trimmedprecipitation, perstation)
    #add the trimmed station to the new table
  }else{
    fullymissingstations <- c(fullymissingstations, perstation$station[1])
  }
}

Data <- left_join(DischargeData, trimmedprecipitation)
#combine the tables into one frame

Data$basin <- NULL

TreatmentList <- read.csv('treaties and stations.csv')
#read in a list of all treatment group stations

Data$treatment <- ifelse(Data$station %in% TreatmentList$grdc_no, 1, 0)
#assign binary treatment indicator

TreatyList <- read.csv('C:/Users/Gebruiker/Documents/AUC Course by Course/Capstone/GRDC_Request_Laurens_Hof.csv')

for(i in 1:52){
  if(TreatyList$frequency[i] == 'Daily'){
    TreatyList$startyear[i] <- substr(TreatyList$Period[i], start = 7, stop = 10)
    TreatyList$endyear[i] <- substr(TreatyList$Period[i], start = 18, stop = 21)
  } else {
    TreatyList$startyear[i] <- substr(TreatyList$Period[i], start = 4, stop = 7)
    TreatyList$endyear[i] <- substr(TreatyList$Period[i], start = 12, stop = 15)
  }
}
# split the request dates into start and end years

TreatyList$startyear <- as.numeric(TreatyList$startyear)
TreatyList$endyear <- as.numeric(TreatyList$endyear)
#Make them numeric

for(i in 1:52){
  TreatyList$treatyyear[i] <- as.integer(median(c(TreatyList$startyear[i], TreatyList$endyear[i])))
}
#The treaty year is the median of the requested periods: compute it for each river

TreatyList$grdc_no <- TreatyList$grdc_no + 1*as.numeric(duplicated(TreatyList$grdc_no))
#increment duplicate codes by 1

for(i in 1:3692){
  stationnumber <- Data$station[i]
  Data$river[i] <- as.character(TreatyList$river[which(TreatyList$grdc_no == stationnumber)])
}
#look up and copy the river

for(i in 1:3692){
  x <- which(TreatyList$grdc_no == Data$station[i])
  Data$treatyyear[i] <- TreatyList$treatyyear[x]
}
#Look up the index of matching station id. Set new column treatyyyear to treatyyear based in index

Data$relativeyear <- Data$year - Data$treatyyear
#create a vector of relative years

Data$after <- ifelse(Data$year >= Data$treatyyear, 1, 0)

Data$river <- str_replace(Data$river, ",", "")
Data$river <- str_replace(Data$river, " ", "")
Data$river <- str_replace(Data$river, "SAINTLAWRENCE RIVER", "STLWRNCE")
Data$relativeyear <- str_replace(Data$relativeyear, "-", "minus")
#before factoring into dummies, make the levels more amenable to use in further code

riverdummies <- dummy(Data$river, sep = "")
Data <- cbind(Data, riverdummies)
#add river fixed effects

yeardummies <- dummy(Data$relativeyear, sep = "")
Data <- cbind(Data, yeardummies)
#add year fixed effects

prediction <- lm(flow ~ precipitation+
                   riverCOLORADORIO + riverDOURORIO + riverINCOMATIRIO + riverISAR + 
                   riverJORDANRIVER + riverLIMPOPORIVER + riverMAAS + riverMAPUTO + 
                   riverMAUDAN + riverNEIDENELVA + riverNILERIVER + 
                   riverPARANARIO + riverPILCOMAYORIO +
                   riverRAINYRIVER + riverRHINERIVER + riverRIOGRANDE + 
                   riverSOCA + riverSOURISRIVER + riverUMBELUZIRIO + riverVUOKSI,
                   data = Data)
#create a model to predict the discharge based on precipitation level and river dummies

Data <- add_predictions(Data, prediction, var = "pred")
Data <- filter(Data, treatment == 1)
Data$deviation <- (abs(Data$flow - Data$pred)/Data$pred)

model1 <- lm(deviation ~relativeyearminus5 + relativeyearminus4 + relativeyearminus3 + relativeyearminus2 +
                        relativeyear0 + relativeyear1 + relativeyear2 +
                        relativeyear3 + relativeyear4 + relativeyear5 + riverDOURORIO + riverINCOMATIRIO + riverISAR + 
                        riverJORDANRIVER + riverLIMPOPORIVER + riverMAAS + riverMAPUTO + 
                        riverMAUDAN + riverNEIDENELVA + riverNIAGARARIVER + riverNILERIVER + 
                        riverOKAVANGORIVER + riverORANGERIVER + riverPARANARIO + riverPILCOMAYORIO +
                        riverRAINYRIVER + riverRHINERIVER + riverRIOGRANDE + riverSTLWRNCE + 
                        riverSOCA + riverSOURISRIVER + riverUMBELUZIRIO + riverVUOKSI, data = Data
                        )

robust1 <- coeftest(model1, vcov = vcovHC(model1, type = "HC0"))
trendtest <- cor.test(Data$deviation, Data$year, method = "kendall")
