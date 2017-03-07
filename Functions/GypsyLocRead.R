# Gypsy Functions
require(dplyr)
require(lubridate)
#################################################################################################################################
# This function takes the RAW text files (.txt) from Gypsy5 data loggers and outputs a tidy data.frame with the location informan
GypsyLocRead<-function(File){
  
  # If you are loading a single file
  if(length(File)==1){  
    
    # Read in the data
    GypsyLocations<-read.table(File,fill = T,stringsAsFactors = F,header = T)
    
    # change the Date format
    GypsyLocations$Date<-dmy(GypsyLocations$Date,tz = "GMT",quiet = T)
    
    # Drop out all obs that dod not have a date in the first column
    GypsyLocations<-subset(GypsyLocations,!is.na(Date))
    
    # Make a clean data frame with columns in the correct classes
    GypsyLocations1<-data.frame(Date=dmy(GypsyLocations$Date,tz = "GMT"),
                                Time=GypsyLocations$Time,
                                DateTime=ymd_hms(paste(GypsyLocations$Date,GypsyLocations$Time), tz="GMT"),
                                Longitude=as.numeric(GypsyLocations$Longitude),
                                Latitude=as.numeric(GypsyLocations$Latitude),
                                Alt.=as.numeric(GypsyLocations$Alt.),
                                Speed=as.numeric(GypsyLocations$Speed),
                                NSat=as.integer(GypsyLocations$NSat),
                                Dop=as.numeric(GypsyLocations$Dop),
                                GSVsum=as.integer(GypsyLocations$GSVsum),
                                Position_in_File=GypsyLocations$Position,
                                File=gsub(".txt","",str_extract(File,pattern = "\\w*\\.txt")),
                                Event_Type="Position")
    GypsyLocations1<-separate(GypsyLocations1,File,into = c("Year","GPSNum","IslandSpeciesCode","CaptureID","Band"),sep="_",convert = T, remove = F)
    
    # Return that object as the output from the function
    return(GypsyLocations1)
  }
  
  # If you have multiple files the function will start here
  if(length(File)>1){  
    
    # initalize a new object to iteratively add data to in the loop below
    GypsyLocFinal<-NULL
    
    # for loop to read in multiple data files from multiple units (or just one unit)
    for(i in 1:length(File)){  
      
      # Read in the data
      GypsyLocations<-read.table(File[i],fill = T,stringsAsFactors = F,header = T)
      
      # change the Date format
      GypsyLocations$Date<-dmy(GypsyLocations$Date,tz = "GMT",quiet = T)
      
      # Drop out all obs that dod not have a date in the first column
      GypsyLocations<-subset(GypsyLocations,!is.na(Date))
      
      #     Make a clean data frame with columns in the correct classes
      GypsyLocations1<-data.frame(Date=GypsyLocations$Date,
                                  Time=GypsyLocations$Time,
                                  DateTime=ymd_hms(paste(GypsyLocations$Date,GypsyLocations$Time), tz="GMT"),
                                  Longitude=as.numeric(GypsyLocations$Longitude),
                                  Latitude=as.numeric(GypsyLocations$Latitude),
                                  Alt.=as.numeric(GypsyLocations$Alt.),
                                  Speed=as.numeric(GypsyLocations$Speed),
                                  NSat=as.integer(GypsyLocations$NSat),
                                  Dop=as.numeric(GypsyLocations$Dop),
                                  GSVsum=as.integer(GypsyLocations$GSVsum),
                                  Position_in_File=GypsyLocations$Position,
                                  File=gsub(".txt","",str_extract(File[i],pattern = "\\w*\\.txt")),
                                  Event_Type="Position")
      GypsyLocations1<-separate(GypsyLocations1,File,into = c("Year","GPSNum","IslandSpeciesCode","CaptureID","Band"),sep="_",convert = T, remove = F)
      
      #       Rbind all the data together from each file
      GypsyLocFinal<-rbind(GypsyLocFinal,GypsyLocations1)
    }
    # Return that object as the output from the function
    return(GypsyLocFinal)
    
  }
}