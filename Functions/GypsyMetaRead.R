# Gypsy Functions
require(dplyr)
require(lubridate)
################################################################`
# Function to read non-loacation events from Gypsy4 data loggers
GypsyMetaRead<-function(File){
  # If using a single data file  
  if(length(File)==1){  
    # Use read lines to bring in data
    Lines<-readLines(File)
    
    #     an overly complex tidbit of code to first use regular expressions to extract data from the text that you read in, then subset out all position data which is event_type==NA
    GypsyMeta<-subset(data.frame(Date=dmy(str_extract(Lines,pattern  =  "([0-9]{2})/([0-9]{2})/([0-9]{4})"),tz="GMT"),
                                 Time=str_extract(Lines,pattern  =  "([0-9]{2}):([0-9]{2}):([0-9]{2})"),
                                 Voltage=as.numeric(gsub(" V", "", str_extract(Lines,pattern  =  "([0-9]\\.?[0-9]*) V"))),
                                 Event_Type=str_extract(Lines,pattern  =  c("Power on",
                                                                            "Turning off",
                                                                            "Entering PC Mode",
                                                                            "Signal lost. Going to sleep.",
                                                                            "Battery voltage",
                                                                            "Start searching for satellites",
                                                                            "Signal obtained, starting schedule",
                                                                            "Switch to Long Periods mode:  1 fix / 10 minutes",
                                                                            "No visible satellite, going to sleep",
                                                                            "Unit restart",
                                                                            "Remote communication timeout. Resetting")),
                                 Position_in_File=str_extract(Lines,pattern  =  "([[:alnum:]]{5,})$")),
                      !is.na(Event_Type))
    
    # Make a DateTime Field
    GypsyMeta$DateTime<-ymd_hms(paste(GypsyMeta$Date,GypsyMeta$Time), tz="GMT")
    
    # Add the file that it is comming from
    GypsyMeta$File<-gsub(".txt","",str_extract(File,pattern = "\\w*\\.txt"))
    
    # and make that the output from the function
    return(GypsyMeta)
    
  }
  
  # If you have multiple files the function will start here
  if(length(File)>1){
    # initialize the object that the data will be put in
    GypsyMetaFinal<-NULL
    # for loop to read in multiple data files from multiple units (or just one unit)
    for(i in 1:length(File)){  
      # Use read lines to bring in data
      
      Lines<-readLines(File[i])
      #     an overly complex tidbit of code to first use regular expressions to extract data from the text that you read in, then subset out all position data which is event_type==NA
      GypsyMeta<-subset(data.frame(Date=dmy(str_extract(Lines,pattern  =  "([0-9]{2})/([0-9]{2})/([0-9]{4})"),tz="GMT"),
                                   Time=str_extract(Lines,pattern  =  "([0-9]{2}):([0-9]{2}):([0-9]{2})"),
                                   Voltage=as.numeric(gsub(" V", "", str_extract(Lines,pattern  =  "([0-9]\\.?[0-9]*) V"))),
                                   Event_Type=str_extract(Lines,pattern  =  c("Power on",
                                                                              "Turning off",
                                                                              "Entering PC Mode",
                                                                              "Signal lost. Going to sleep.",
                                                                              "Battery voltage",
                                                                              "Start searching for satellites",
                                                                              "Signal obtained, starting schedule",
                                                                              "Switch to Long Periods mode:  1 fix / 10 minutes",
                                                                              "No visible satellite, going to sleep",
                                                                              "Unit restart",
                                                                              "Remote communication timeout. Resetting")),
                                   Position_in_File=str_extract(Lines,pattern  =  "([[:alnum:]]{5,})$")),
                        !is.na(Event_Type))
      
      # Make a DateTime Field
      GypsyMeta$DateTime<-ymd_hms(paste(GypsyMeta$Date,GypsyMeta$Time),tz="GMT")
      # Add the file that it is comming from
      GypsyMeta$File<-gsub(".txt","",str_extract(File[i],pattern = "\\w*\\.txt"))
      GypsyMetaFinal<-rbind(GypsyMetaFinal,GypsyMeta)
    }
    # and make that the output from the function
    return(GypsyMetaFinal)
  }
}
