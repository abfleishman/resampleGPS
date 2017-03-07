# Gypsy Functions
require(dplyr)
require(lubridate)
require(stringr)
require(tidyr)
#################################################################################################################################
# This function takes the RAW text files (.txt) from Gypsy5 data loggers and outputs a tidy data.frame with the location informan

GPSLocRead<-function(File,Type="Gypsy"){
  if(Type=="gypsy"|Type=="Gypsy"){  
    
    # If you are loading a single file
    if(length(File)==1){  
      
      # Read in the data
      GPSLocations<-read.table(File,fill = T,stringsAsFactors = F,header = T)
      
      # change the Date format
      if(str_detect(GPSLocations$Date[1],pattern = "2015$")){
        GPSLocations$Date<-mdy(GPSLocations$Date,tz = "GMT",quiet = T)
      }
      if(str_detect(GPSLocations$Date[1],pattern = "^2015")){
        GPSLocations$Date<-ymd(GPSLocations$Date,tz = "GMT",quiet = T)
      }
      
      # Drop out all obs that dod not have a date in the first column
      GPSLocations<-subset(GPSLocations,!is.na(Date))
      
      # Make a clean data frame with columns in the correct classes
      GPSLocations1<-data.frame(Date=dmy(GPSLocations$Date,tz = "GMT"),
                                Time=GPSLocations$Time,
                                DateTime=ymd_hms(paste(GPSLocations$Date,GPSLocations$Time), tz="GMT"),
                                Longitude=as.numeric(GPSLocations$Longitude),
                                Latitude=as.numeric(GPSLocations$Latitude),
                                Alt.=as.numeric(GPSLocations$Alt.),
                                Speed=as.numeric(GPSLocations$Speed),
                                NSat=as.integer(GPSLocations$NSat),
                                Dop=as.numeric(GPSLocations$Dop),
                                GSVsum=as.integer(GPSLocations$GSVsum),
                                Position_in_File=GPSLocations$Position,
                                File=gsub(".txt","",str_extract(File,pattern = "\\w*\\.txt")),
                                Event_Type="Position")
      GPSLocations1<-separate(GPSLocations1,File,into = c("Year","GPSNum","IslandSpeciesCode","CaptureID","Band"),sep="_",convert = T, remove = F)
      
      # Return that object as the output from the function
      return(GPSLocations1)
    }
    
    # If you have multiple files the function will start here
    if(length(File)>1){  
      
      # initalize a new object to iteratively add data to in the loop below
      GPSLocFinal<-NULL
      
      # for loop to read in multiple data files from multiple units (or just one unit)
      for(i in 1:length(File)){  
        
        # Read in the data
        GPSLocations<-read.table(File[i],fill = T,stringsAsFactors = F,header = T)
        head(GPSLocations)
        # change the Date format
        if(str_detect(GPSLocations$Date[1],pattern = "2015$")){
          GPSLocations$Date<-mdy(GPSLocations$Date,tz = "GMT",quiet = T)
        }
        
        if(str_detect(GPSLocations$Date[1],pattern = "^2015")){
          GPSLocations$Date<-ymd(GPSLocations$Date,tz = "GMT",quiet = T)
        }
        
        # Drop out all obs that dod not have a date in the first column
        GPSLocations<-subset(GPSLocations,!is.na(Date))
        
        #     Make a clean data frame with columns in the correct classes
        GPSLocations1<-data.frame(Date=GPSLocations$Date,
                                  Time=GPSLocations$Time,
                                  DateTime=ymd_hms(paste(GPSLocations$Date,GPSLocations$Time), tz="GMT"),
                                  Longitude=as.numeric(GPSLocations$Longitude),
                                  Latitude=as.numeric(GPSLocations$Latitude),
                                  Alt.=as.numeric(GPSLocations$Alt.),
                                  Speed=as.numeric(GPSLocations$Speed),
                                  NSat=as.integer(GPSLocations$NSat),
                                  Dop=as.numeric(GPSLocations$Dop),
                                  GSVsum=as.integer(GPSLocations$GSVsum),
                                  Position_in_File=GPSLocations$Position,
                                  File=gsub(".txt","",str_extract(File[i],pattern = "\\w*\\.txt")),
                                  Event_Type="Position")
        GPSLocations1<-separate(GPSLocations1,File,into = c("Year","GPSNum","IslandSpeciesCode","CaptureID","Band"),sep="_",convert = T, remove = F)
        
        #       Rbind all the data together from each file
        GPSLocFinal<-rbind(GPSLocFinal,GPSLocations1)
      }
      # Return that object as the output from the function
      return(GPSLocFinal)
      
    }
  }
  if(Type=="igotu"){
    # If you are loading a single file
    if(length(File)==1){  
      
      # Read in the data
      GPSLocations<-read.csv(File,stringsAsFactors = F,header = T)
      
      # change the Date format
      if(str_detect(GPSLocations$Date[1],pattern = "2015$")){
        GPSLocations$Date<-mdy(GPSLocations$Date,tz = "GMT",quiet = T)
      }
      if(str_detect(GPSLocations$Date[1],pattern = "^2015")){
        GPSLocations$Date<-ymd(GPSLocations$Date,tz = "GMT",quiet = T)
      }
      
      # Drop out all obs that dod not have a date in the first column
      GPSLocations<-subset(GPSLocations,!is.na(Date))
      
      # Make a clean data frame with columns in the correct classes
      GPSLocations1<-data.frame(Date=ymd(GPSLocations$Date,tz = "GMT"),
                                Time=GPSLocations$Time,
                                DateTime=ymd_hms(paste(GPSLocations$Date,GPSLocations$Time), tz="GMT"),
                                Longitude=as.numeric(GPSLocations$Longitude),
                                Latitude=as.numeric(GPSLocations$Latitude),
                                Alt.=as.numeric(GPSLocations$Altitude),
                                Speed=as.numeric(GPSLocations$Speed),
                                Course=as.numeric(GPSLocations$Course),
                                Distance=as.numeric(GPSLocations$Distance),
                                Essential=GPSLocations$Essential,
                                Position_in_File=GPSLocations$Index,
                                File=basename(File),
                                Event_Type="Position")
      
      GPSLocations1<-separate(GPSLocations1,File,into = c("Species","Island","GPSNum","NestNum","Sex","TripNum"),sep="_",convert = T, remove = F)
      
      # Return that object as the output from the function
      return(GPSLocations1)
    }
    
    # If you have multiple files the function will start here
    if(length(File)>1){  
      
      # initalize a new object to iteratively add data to in the loop below
      GPSLocFinal<-NULL
      
      # for loop to read in multiple data files from multiple units (or just one unit)
      for(i in 1:length(File)){  
        
        # Read in the data
        GPSLocations<-read.csv(File[i],stringsAsFactors = F,header = T)
        head(GPSLocations)
        # change the Date format
        if(str_detect(GPSLocations$Date[1],pattern = "2015$")){
        GPSLocations$Date<-mdy(GPSLocations$Date,tz = "GMT")
        }
        
        if(str_detect(GPSLocations$Date[1],pattern = "^2015")){
          GPSLocations$Date<-ymd(GPSLocations$Date,tz = "GMT")
        }
        
        # Drop out all obs that dod not have a date in the first column
        GPSLocations<-subset(GPSLocations,!is.na(Date))
        
        # Make a clean data frame with columns in the correct classes
        GPSLocations1<-data.frame(Date=ymd(GPSLocations$Date,tz = "GMT"),
                                  Time=GPSLocations$Time,
                                  DateTime=ymd_hms(paste(GPSLocations$Date,GPSLocations$Time), tz="GMT"),
                                  Longitude=as.numeric(GPSLocations$Longitude),
                                  Latitude=as.numeric(GPSLocations$Latitude),
                                  Alt.=as.numeric(GPSLocations$Altitude),
                                  Speed=as.numeric(GPSLocations$Speed),
                                  Course=as.numeric(GPSLocations$Course),
                                  Distance=as.numeric(GPSLocations$Distance),
                                  Essential=GPSLocations$Essential,
                                  Position_in_File=GPSLocations$Index,
                                  File=basename(File[i]),
                                  Event_Type="Position")
        
        GPSLocations1<-separate(GPSLocations1,File,into = c("Species","Island","GPSNum","NestNum","Sex","TripNum"),sep="_",convert = T, remove = F)
        
        #       Rbind all the data together from each file
        GPSLocFinal<-rbind(GPSLocFinal,GPSLocations1)
      }
      # Return that object as the output from the function
      return(GPSLocFinal)
      
    }
  }
}
