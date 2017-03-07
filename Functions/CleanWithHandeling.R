#' CleanWithHandeling
#'
#' Removes rows from when tags were not deployed on birds (or other animals)
#' @author Abram B. Fleishman <abram.fleishman AT sjsu.edu>

#' @param GypsyLocations a dataframe made with GypsyLocRead()
#' @param HandelingData a dataframe made with Importhandling()
#' @return A data frame that has had rows of data from before or after deployment removed
#' @examples
#' CleanWithHandeling(GypsyLocations=GypsyLocations,HandelingData=HandelingData)
#'
#' @export

CleanWithHandeling<-function(GypsyLocations=GypsyLocations,HandelingData=HandelingData){
  if(!exists("GypsyLocations")) stop("GypsyLocations not yet loaded")
  if(!exists("HandelingData")) stop("HandelingData not yet loaded")
  Birds<-as.character(unique(GypsyLocations$CaptureID))

  dataOut<-NULL
  for(i in 1:length(Birds)){
    Data<-subset(GypsyLocations,CaptureID==Birds[i])
    MetaData<-subset(HandelingData,CaptureID==Birds[i])
    Data2<-subset(Data,DateTime>MetaData$RelDateTimeC1&DateTime<MetaData$CapDateTimeC2)
    dataOut<-bind_rows(dataOut,Data2)
  }

  data<-dataOut
  rm(Data);rm(Data2);rm(MetaData);rm(dataOut)
  return(data)
}

#' CleanWithHandeling
#'
#' Removes rows from when tags were not deployed on birds (or other animals)
#' @author Rachael Orben <>

#' @param Data data frame being subset
#' @param Bdatetimes vector of datetimes format from GPS/GLS logger: 2015-06-05 20:58:00
#' @param C1datetime release time from capture 1 date time, format: 2015-06-05 20:58:00
#' @param C2datetime recapture time from capture 2, format: 2015-06-05 20:55:00
#' @return returns indicies to keep
#' @examples
#' trimDeploy(Data=data, Bdatetimes="2015-06-05 20:58:00", C1datetime="2015-06-05 20:58:00", C2datetime="2015-06-05 20:55:00")
#'
#' @export
#'
##returns indicies to keep
trimDeploy<-function(Data, Bdatetimes, C1datetime, C2datetime){

  Data2<-subset(Data,Bdatetimes > C2datetime & Bdatetimes < C2datetime)

}
