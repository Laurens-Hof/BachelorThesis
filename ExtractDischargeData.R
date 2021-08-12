rm(list = ls())
library(readr)
library(dplyr)
library(imputeTS)

StationList <- read_csv("AUC Course by Course/Capstone/GRDC_Request_Laurens_Hof.csv")
#import the list of stations so it can be used as a comparison table

DischargeData <- data.frame(station = integer(1), year = integer(1), flow = double(1), stringsAsFactors = FALSE)
#initialize target table

for(i in 1:52){
  
  stationnumber <- StationList$grdc_no[i]
  #store the station number so it can be called in the read.csv functions
  
  if(StationList$frequency[i] == 'Daily'){
    
    timeseries <- utils::read.table(paste0('AUC Course by Course/Capstone/Data/DischargeData/',stationnumber,'_Q_Day.Cmd.txt'),header = TRUE, sep = ";", comment = "#")
    #read the csv2 file, store it in a temporary dataframe
    
    timeseries$Value <- dplyr::na_if(timeseries$Value, -999.00)
    #make missing values into 'true' NA's
    
    timeseries$Value <- imputeTS::na_seadec(timeseries$Value, find_frequency = TRUE)
    #removes seasonality, imputes na values by linear interpolation, then adds seasonality back on
    
    timeseries$year <- substring(timeseries$YYYY.MM.DD, 0, 4)
    #create a year flag
    
    for(t in 1800:2020){
      if(t %in% timeseries$year){
        subdays <- filter(timeseries, year == t)
        #take all the entries the current year in the loop, and add them to a temporary table
      
        DischargeData <- dplyr::add_row(DischargeData, station = stationnumber, year = t, flow = mean(subdays$Value))
        #add an entry to the target table, containing measuring station, year, and the mean of discharge over the year
      }
    }  
    
  }else if(StationList$frequency[i] == 'Monthly'){
    
    timeseries <- utils::read.table(paste0('AUC Course by Course/Capstone/Data/DischargeData/',stationnumber,'_Q_Month.txt'),header = TRUE, sep = ";", comment = "#")
    #read the csv2 file, store it in a temporary dataframe
    
    timeseries$Original <- dplyr::na_if(timeseries$Original, -999.00)
    timeseries$Calculated <- dplyr::na_if(timeseries$Calculated, -999.00)
    #make missing values into true NA's
    
    if(sum(is.na(timeseries$Original)) < sum(is.na(timeseries$Calculated))){
      timeseries$monthflow <- timeseries$Original
    } else{
      timeseries$monthflow <- timeseries$Calculated
    }
    #small loop to make the working column into the one with the best quality data out of "Original" and "Calculated"
    
    timeseries$monthflow <- imputeTS::na_seadec(timeseries$monthflow, find_frequency = TRUE)
    #removes seasonality, imputes na values by linear interpolation, then adds seasonality back on
    
    timeseries$year <- substring(timeseries$YYYY.MM.DD, 0, 4)
    #create a year flag
    
    
    
    for(t in 1800:2020){
      if(t %in% timeseries$year){
        submonths <- filter(timeseries, year == t)
        #take all the entries the current year in the loop, and add them to a temporary table
      
        DischargeData <- dplyr::add_row(DischargeData, station = stationnumber, year = t, flow = mean(submonths$monthflow))
        #add an entry to the target table, containing measuring station, year, and the mean of discharge over the year
      }
    }
    
  }
}

DischargeData <- filter(DischargeData, year > 1000)
#remove the dummy column

write.csv(DischargeData, file = 'AUC Course by Course/Capstone/Data/DischargeData.csv')