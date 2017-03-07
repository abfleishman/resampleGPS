#' Function to import and clean handeling data
#'
#' @author Abram B. Fleishman <abram.fleishman AT sjsu.edu>
#' @param path Path to data file in .csv format
#' @param location TimeZone from OlsonNames()
#'
#' @return Reads in data con converts times to GMT and removes "B" from the Capture number
#' @examples
#' ImportHandeling(path="~/Dropbox/RLKI_rawdata/SGRK_Handling_23Aug15.csv",location="America/Anchorage")
#' @export
#'

ImportHandeling<-function(path="~/Dropbox/RLKI_rawdata/SGRK_Handling_23Aug15.csv",location="America/Anchorage"){
  require(lubridate)
  HandlingData<-read.csv(path)
  HandlingData$CaptureNum<-paste("B",HandlingData$CaptureNum,sep="")
  HandlingData$DateC1<-mdy(HandlingData$DateC1)
  HandlingData$DateC2<-mdy(HandlingData$DateC2)

  HandlingData$CapDateTimeC1<- with_tz(ymd_hm(paste(HandlingData$DateC1,HandlingData$CaptureTimeC1),tz=location),tz="GMT")
  HandlingData$RelDateTimeC1<-with_tz(ymd_hm(paste(HandlingData$DateC1,HandlingData$ReleaseTimeC1),tz=location),tz="GMT")

  HandlingData$CapDateTimeC2<-with_tz(ymd_hm(paste(HandlingData$DateC2,HandlingData$CaptureTimeC2),tz=location),tz="GMT")
  HandlingData$RelDateTimeC2<-with_tz(ymd_hm(paste(HandlingData$DateC2,HandlingData$ReleaseTimeC2),tz=location),tz="GMT")
  return(HandlingData)
}

