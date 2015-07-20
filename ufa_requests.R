#!/usr/bin/env Rscript

library(rgdal)
library(jsonlite)
library(leafletR)
library(ggplot2)
library(scales)
library(lubridate)

IntersectPtWithPoly <- function(x, y) { 
  z <- overlay(y, x)   
  x2 <- cbind(x, z) 
  if(("coords.x1" %in% colnames(x2)) & ("coords.x2" %in% colnames(x2))) { 
    coordinates(x2) <- ~coords.x1 + coords.x2 
  } else if(("x" %in% colnames(x2)) & ("x" %in% colnames(x2))) { 
    coordinates(x2) <- ~x + y 
  } 
  if(is.na(CRSargs(x@proj4string)) == "FALSE") { 
    x2@proj4string <- x@proj4string 
  } 
  return(x2) 
} 

d <- function(date) {
  return(as.POSIXct(date/1000, origin="1970-01-01"))
}

percentHist = function(data,year) {
  ggplot(data, aes(x = WARD, fill=WARD)) +  
    geom_bar(aes(y = (..count..)/sum(..count..)), binwidth = 25) + 
    xlab("ward") + ylab("percent") + ggtitle(paste0("Distribution of Requests by Ward, April 20-June 15, ",year)+
    guides(fill=FALSE)+
    ## scale_y_continuous(labels = percent_format()) #version 3.0.9
    scale_y_continuous(labels = percent,limits=c(0,0.35)) #version 3.1.0  
}

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  if (numPlots==1) {
    print(plots[[1]])    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#Pulls in UFA Street Trees data from JSON API
for(i in 1422:1518){
  index_bottom <- as.integer(i*1000)
  index_top <- as.integer((i+1)*1000)
  url <- paste0("http://maps2.dcgis.dc.gov/dcgis/rest/services/DDOT/UFACityworks/MapServer/1/query?where=REQUESTID%3E%3D",index_bottom,"+AND+REQUESTID%3C",index_top,"&text=&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&relationParam=&outFields=OBJECTID%2CREQUESTID%2CDESCRIPTION%2CADDRESS%2CINITIATEDDATE%2CCLOSEDDATE%2CWORKORDERID%2CSTATUS%2CCSRNUMBER&returnGeometry=true&maxAllowableOffset=&geometryPrecision=&outSR=4326&returnIdsOnly=false&returnCountOnly=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&returnZ=false&returnM=false&gdbVersion=&returnDistinctValues=false&f=pjson")
  cat(i)
  cat('\n')
  mydata <- NULL
  times <- 0
  while(length(mydata) < 6) {
    times <- times + 1
    cat(paste0("Tries: ",times,"\n"))
    tryCatch({
      mydata <- readLines(url,warn=FALSE)
      mydata <- fromJSON(mydata) 
      cat(paste0("Length: ",length(mydata),"\n"))
    }, error = function(e) {
      cat('There was an error\n')
    })
    Sys.sleep(1)
  }
  plots <- data.frame(mydata[6])
  if (length(plots)>0){
    plots <- cbind(plots[,1],plots[,2])
    if (exists("ufa")) {
      ufa <- rbind(ufa,plots)    
    } else {
      ufa <- plots
    }    
  } else {
    cat("No observations in this OBJECTID range\n")
  }
}

write.csv(ufa,'~/Code/tree-map/data/ufa_requests_new.csv',row.names=FALSE)

# ufa1 = read.csv('~/Code/tree-map/data/ufa_requests.csv')
ufa <- ufa[,c("OBJECTID","REQUESTID","DESCRIPTION","ADDRESS","INITIATEDDATE","CLOSEDDATE","WORKORDERID","STATUS","CSRNUMBER","x","y")]
colnames(ufa)[colnames(ufa)=="x"] <- "LONGITUDE"
colnames(ufa)[colnames(ufa)=="y"] <- "LATITUDE"
colnames(ufa)[colnames(ufa)=="INITIATEDDATE"] <- "DATE"

setwd('~/Code/tree-map/data/')
coordinates(ufa) <- ~LONGITUDE+LATITUDE
wards <- readOGR('wards','wards')
ufa <- IntersectPtWithPoly(ufa, wards)
ufa = data.frame(ufa)

ufa$WARD = as.factor(ufa$WARD)
ufa$DATE = as.POSIXct(ufa$DATE/1000, origin="1970-01-01")


june.15.2015 = 1434427200000 # June 2, 2015
april.20.2015 = 1429502400000 # April 20, 2015 
interval = 31536000000 # 1 calendar year

jan.1.2011 = 1293858000000

# ufa.inspection = ufa[ufa$DESCRIPTION=="TREE INSPECTION",]
# ufa.pruning = ufa[ufa$DESCRIPTION=="TREE PRUNING",]
# ufa.removal = ufa[ufa$DESCRIPTION=="TREE REMOVAL",]
plant = ufa[ufa$DESCRIPTION=="TREE PLANTING",]
plant$DATE = floor_date(plant$DATE, "day")

# Split planting requests, April 20 til June 2 for years 2015, 2014, 2013, 2012, 2011

plant.april.2015 = plant[plant$DATE>d(1427860800000-interval*0) & plant$DATE<d(1430452800000-interval*0),]
qplot(plant.april.2015$DATE,ylab="requests",xlab="",main="Planting Requests in April 2015")

plant.sub.2015 = plant[plant$DATE>=d(april.20.2015-interval*0) & plant$DATE<d(june.15.2015-interval*0),]
plant.sub.2015$YEAR = 2015
plant.sub.2014 = plant[plant$DATE>=d(april.20.2015-interval*1) & plant$DATE<d(june.15.2015-interval*1),]
plant.sub.2014$YEAR = 2014
# plant.sub.2013 = plant[plant$DATE>=d(april.20.2015-interval*2) & plant$DATE<d(june.15.2015-interval*2),]
# plant.sub.2013$YEAR = 2013
# plant.sub.2012 = plant[plant$DATE>=d(april.20.2015-interval*3) & plant$DATE<d(june.15.2015-interval*3),]
# plant.sub.2012$YEAR = 2012
# plant.sub.2011 = plant[plant$DATE>=d(april.20.2015-interval*4) & plant$DATE<d(june.15.2015-interval*4),]
# plant.sub.2011$YEAR = 2011

plant.sub.all = rbind(plant.sub.2015,plant.sub.2014)

plant.all = plant[plant$DATE>=d(jan.1.2011),]

plant.all.2015 = plant[plant$DATE>d(1420088400000) & plant$DATE<=d(jan.1.2011+interval*5),]
plant.post.2015 = plant.all.2015[plant.all.2015$DATE>=d(april.20.2015),]
plant.pre.2015 = plant.all.2015[plant.all.2015$DATE<d(april.20.2015),]

plant.all.2014 = plant[plant$DATE>=d(1388552400000) & plant$DATE<=d(1420088400000),]
# plant.all.2013 = plant[plant$DATE>=d(1357016400000) & plant$DATE<=d(1388552400000),]
# plant.all.2012 = plant[plant$DATE>=d(1325394000000) & plant$DATE<=d(1357016400000),]
# plant.all.2011 = plant[plant$DATE>=d(1293858000000) & plant$DATE<d(1325394000000),]
