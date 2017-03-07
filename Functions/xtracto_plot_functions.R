#' mapFrame function for mapping xtractomatic raster layer
#'
#' @author Abram B. Fleishman <abram.fleishman AT sjsu.edu>

#' @param longitude a vector of longitude values
#' @param latitude a vector of latitude values
#' @param variable a commented string indicating what variable you are using (currently only works with a single variable)
#' @param dataset a commented string indicating what dataset you are using (currently only works with a single variable)
#' @return a ggplot map frame for the data to be ploted with
#' @examples
#' mapFrame( data$longitude,data$latitude,"sst", "")
#'
#' @export
# subset the data into the frame here
mapFrame<- function(longitude,latitude,variable, dataset=""){
  dims<-dim(variable)
  variable<-array(variable,dims[1]*dims[2])
  longitude<-longitude
  variableFrame<-expand.grid(x=longitude,y=latitude)
  variableFrame$variable<-variable
  attr(variableFrame,which = "dataset")<-dataset
  return(variableFrame)
}

#' plotFrame function for mapping xtractomatic raster layer
#'
#' @author Abram B. Fleishman <abram.fleishman AT sjsu.edu>
#'
#' @param variableFrame output from mapFrame
#' @param xlim x limits in the for c(0,5)
#' @param ylim y limits in the for c(0,5)
#' @param ttext a commented string indicating the plot title text
#' @param logplot True or false for if the plot should have log axes
#' @return a ggplot map frame for the data to be ploted with
#' @examples
#' plotFrame( variableFrame, xlim=c(0,10),ylim=c(10,20), logplot=F)
#' @export

# the output of this function is a ggplot object. If it is not assigned to something it will just print the maap
# Currently not generic, need to change variable and syntax to allow generic version of this function

plotFrame<-function(Frame=variableFrame,	xlim=xlim,	ylim=ylim,title=ttext,logplot=TRUE){
  require(mapdata)
  require(ggplot2)
  require(RColorBrewer)
  w	<- map_data(map = "worldHires",	ylim	=	ylim,	xlim	=	(xlim))
  myplot<-ggplot(data	=	variableFrame,	aes(x	=	x,	y	=	y,	fill	=	variable))	+
    geom_raster(interpolate	= FALSE)	+
    geom_polygon(data	=	w,	aes(x=long,	y	=	lat,	group	=	group),	fill	= "grey80")	+
    theme_bw()	+ ylab("latitude")	+ xlab("longitude")	+
    coord_fixed(1.3,xlim	=	(xlim),	ylim	=	ylim)
  if(logplot){
    my.col	<- colorRampPalette(rev(brewer.pal(11,	"RdBu")))(5.5)
    myplot<-myplot	+ scale_fill_gradientn(colours	=	my.col,	na.value	= NA,limits=c(-2,4.5))	+
      ggtitle(title)
  }else{
    my.col	<- colorRampPalette(rev(brewer.pal(11,	"RdBu")))((diff(range(variableFrame$variable,na.rm=TRUE))))
    myplot<-myplot	+ scale_fill_gradientn(colours	=	my.col,	na.value	= NA)	+
      ggtitle(title)
  }
  return(myplot)
}
