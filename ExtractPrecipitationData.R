rm(list = ls())
library(readr)
library(rgdal)
library(dplyr)
library(ncdf4)
library(raster)
library(chron)
library(stringr)

p4s <- '+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext +no_defs'
# store the projections used in a variable, for easier consistency

setwd('C:\\Users\\Gebruiker\\Documents\\AUC Course by Course\\Capstone\\Data\\precipitation')

BasinShapes <- readOGR(".", 'shedswithstations', verbose = TRUE, p4s = p4s)
#read in the shapefiles covering basins with stations

monthlyprecipitation <- data.frame(ID = integer(),
                                   precipitation = double(),
                                   flag = character())
yearlyprecipitation <- data.frame(precipitation = double(),
                                  flag = character())
#initialize dumping tables

filelist <- list.files('DecadeByDecadepre')
#create a list of files to iterate over

for(filename in filelist){

  worldprecipitation <- nc_open(paste0('DecadeByDecadepre\\', filename))
  #load in the ncdf file
  
  pre_array <- ncvar_get(worldprecipitation, "pre")
  #get a three-dimensional array from the dataset
  
  options(chron.origin = c(month=1, day=1, year=1900))
  #set the "starting date" to 1st jan, 1900
  
  time <- ncvar_get(worldprecipitation, "time")
  #extract the time variable
  
  absolutetime <- as.character(chron(time, out.format = 'yyyy-mm-dd'))
  #convert the dates to a string in a readable format
  
  slices <- dim(time)
  
  for(i in 1:slices){
    pre_slice <- pre_array[,,i]
    #take a slice of the array
    
    worldraster <- raster(pre_slice, xmn=-85.06, xmx=85.06, ymn=-180, ymx=180, crs = p4s)
    #load the layer into a raster object
    
    worldrastert <- raster::t(worldraster)
    worldrastertflip<- raster::flip(worldrastert, 'y')
    #reorient it because R reads in matrices to rasters in a strange way
    
    selectedraster <- raster::extract(worldrastertflip, BasinShapes, fun = mean, na.omit = TRUE, df = TRUE)
    #extract the subset of raster squares that fall under the basin shapefile. Take the mean value over the area
    
    selectedraster <- mutate(selectedraster, flag = paste(substr(absolutetime[i], start = 0, stop = 4), ID, sep = "/"))
    #add a flag by concatenating year and ID label
    
    monthlyprecipitation <- bind_rows(monthlyprecipitation, selectedraster)
  }
}

for(yearflag in unique(monthlyprecipitation$flag)){
    peryear <- filter(monthlyprecipitation, flag == yearflag)
    yearlyprecipitation <- add_row(yearlyprecipitation, precipitation = mean(peryear$layer, na.rm = TRUE), flag = yearflag)
}
#aggregate per year


lookuptable <- read.csv('stationbasinlookuptable.csv')
#load a lookup table to link basin and station

yearlyprecipitation$year <- substring(yearlyprecipitation$flag, first = 1, last = 4)
yearlyprecipitation$basin <- substring(yearlyprecipitation$flag, first = 6)
#decompose the flag into tidy information

precipitation <- expand.grid(lookuptable$station, yearlyprecipitation$year)
#create the final grid from combinations of station ids and years

names(precipitation) <- c('station', 'year')
#rename columns

for(i in 1:192576){
  lookupindex <- which(lookuptable$station == precipitation$station[i])
  precipitation$basin[i] <- lookuptable$basin[lookupindex]
}

for(i in 1:128252){
  
    precipitation$precipitation[i] <- yearlyprecipitation$precipitation[which(yearlyprecipitation$basin == precipitation$basin[i] &
                                                                              yearlyprecipitation$year == precipitation$year[i])] 
  }

setwd('C:\\Users\\Gebruiker\\Documents\\AUC Course by Course\\Capstone\\Data')

DischargeData <- read.csv('DischargeData.csv')

DischargeData$stationyear <- paste0(DischargeData$station, DischargeData$year)
precipitation$stationyear <- paste0(precipitation$station, precipitation$year)

precipitation$stationyear <- as.numeric(precipitation$stationyear)
DischargeData$stationyear <- as.numeric(DischargeData$stationyear)

precipitation <- filter(precipitation, stationyear %in% DischargeData$stationyear)

uniqueprecipitation <- distinct(precipitation, stationyear, .keep_all = TRUE)

write.csv(uniqueprecipitation, file = "PrecipitationData.csv")





#making a map

filelist <- list.files('DecadeByDecadepre')
#create a list of files to iterate over
worldprecipitation <- nc_open(paste0('DecadeByDecadepre\\', filelist[-1]))
#load in the ncdf file
pre_array <- ncvar_get(worldprecipitation, "pre")
#get a three-dimensional array from the dataset
pre_slice <- pre_array[,,1]
#take a slice of the array
worldraster <- raster(pre_slice, xmn=-85.06, xmx=85.06, ymn=-180, ymx=180, crs = p4s)
#load the layer into a raster object
worldrastert <- raster::t(worldraster)
worldrastertflip<- raster::flip(worldrastert, 'y')
#reorient it because R reads in matrices to rasters in a strange way

plot(worldrastertflip)
plot(BasinShapes, col = "Green", add = TRUE)
