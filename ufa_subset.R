library(rgdal)
library(jsonlite)
library(leafletR)

IntersectPtWithPoly <- function(x, y) { 
  # Extracts values from a SpatialPolygonDataFrame with SpatialPointsDataFrame, and appends table (similar to ArcGIS intersect) 
  # Args: 
  #   x: SpatialPoints*Frame 
  #   y: SpatialPolygonsDataFrame 
  # Returns: 
  # SpatialPointsDataFrame with appended table of polygon attributes 
  # Set up overlay with new column of join IDs in x 
  z <- overlay(y, x)   
  # Bind captured data to points dataframe 
  x2 <- cbind(x, z) 
  # Make it back into a SpatialPointsDataFrame 
  # Account for different coordinate variable names 
  if(("coords.x1" %in% colnames(x2)) & ("coords.x2" %in% colnames(x2))) { 
    coordinates(x2) <- ~coords.x1 + coords.x2 
  } else if(("x" %in% colnames(x2)) & ("x" %in% colnames(x2))) { 
    coordinates(x2) <- ~x + y 
  } 
  # Reassign its projection if it has one 
  if(is.na(CRSargs(x@proj4string)) == "FALSE") { 
    x2@proj4string <- x@proj4string 
  } 
  return(x2) 
} 

#Pulls in UFA Street Trees data from JSON API
for(i in 0:226){
  index_bottom <- as.integer(i*1000)
  index_top <- as.integer((i+1)*1000)
  url <- paste0("http://maps2.dcgis.dc.gov/dcgis/rest/services/DDOT/UFATrees2/MapServer/0/query?where=OBJECTID+%3E+",index_bottom,"+AND+OBJECTID+%3C+",index_top,"&text=&objectIds=&time=&geometry=&geometryType=esriGeometryPoint&inSR=&spatialRel=esriSpatialRelIntersects&relationParam=&outFields=OBJECTID%2CFACILITYID%2CVICINITY%2CTBOX_STAT%2CDATE_PLANT%2CDISEASE%2CPESTS%2CCONDITION%2CCONDITIODT%2COWNERSHIP&returnGeometry=true&maxAllowableOffset=&geometryPrecision=&outSR=%7B\"wkid\"+%3A+4326%7D&returnIdsOnly=false&returnCountOnly=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&returnZ=false&returnM=false&gdbVersion=&returnDistinctValues=false&f=pjson")
  cat(i)
  cat('\n')
  mydata <- NULL
  times <- 0
  while(length(mydata) < 6) {
    times <- times + 1
    cat(paste0("Tries: ",times,"\n"))
    tryCatch({
      mydata <- readLines(url)
      mydata <- fromJSON(mydata) 
      cat(paste0("Length: ",length(mydata),"\n"))
    }, error = function(e) {
      cat('There was an error\n')
    })
    Sys.sleep(1)
  }
  plots <- data.frame(mydata[6])
  plots <- cbind(plots[,1],plots[,2])
  if (exists("ufa")) {
    ufa <- rbind(ufa,plots)    
  } else {
    ufa <- plots
  }
}

write.csv(ufa,'~/Code/tree-map/data/ufa_all.csv',row.names=FALSE)

ufa_sub <- ufa[which(ufa$TBOX_STAT!='Retired' & ufa$TBOX_STAT!='Conflict' & ufa$TBOX_STAT!='Delete' & ufa$TBOX_STAT!='Proposed'),]
ufa_sub$TBOX_STAT <- sub("Open","Open",ufa_sub$TBOX_STAT,ignore.case=TRUE)
ufa_sub$TBOX_STAT <- sub("^$","Unknown",ufa_sub$TBOX_STAT,ignore.case=TRUE)

ufa_sub<-ufa_sub[which(ufa_sub$OWNERSHIP=='UFA' | is.na(ufa_sub$OWNERSHIP) | ufa_sub$OWNERSHIP==''),]
ufa_sub$OWNERSHIP <- sub("^$","UFA",ufa_sub$OWNERSHIP)

#Clean disease information
ufa_sub$DISEASE <- sub(".*Root Rot.*","Root Rot",ufa_sub$DISEASE)
ufa_sub$DISEASE <- sub(".*None.*","None",ufa_sub$DISEASE)
ufa_sub$DISEASE <- sub("^$","None",ufa_sub$DISEASE)
ufa_sub$DISEASE <- sub("^DED$","Dutch Elm Disease",ufa_sub$DISEASE)
ufa_sub$DISEASE <- sub("^BLS$","Bacterial Leaf Scorch",ufa_sub$DISEASE)
ufa_sub$DISEASE <- sub(".*Trunk.*","Trunk Rot",ufa_sub$DISEASE)
ufa_sub$DISEASE <- sub(".*Hypoxylon*","Hypoxylon Canker",ufa_sub$DISEASE)

#Clean and order condition information
ufa_sub$CONDITION <- sub("^$","Unknown",ufa_sub$CONDITION)
ufa_sub$CONDITION[which(ufa_sub$TBOX_STAT=="Proposed"|ufa_sub$TBOX_STAT=="Open")]<-""
ufa_sub$CONDITION <- factor(ufa_sub$CONDITION, levels = c("Unknown","Dead","Poor","Fair","Good","Excellent"))

ufa <- ufa[,c("x","y","OBJECTID","FACILITYID","VICINITY","TBOX_STAT","DATE_PLANT","DISEASE","PESTS","CONDITION","CONDITIODT","OWNERSHIP")]

colnames(ufa_sub)[colnames(ufa_sub)=="x"]<-"longitude"
colnames(ufa_sub)[colnames(ufa_sub)=="y"]<-"latitude"
colnames(ufa_sub)[colnames(ufa_sub)=="VICINITY"]<-"address"
colnames(ufa_sub)[colnames(ufa_sub)=="OBJECTID"]<-"objectid"
colnames(ufa_sub)[colnames(ufa_sub)=="FACILITYID"]<-"facilityid"
colnames(ufa_sub)[colnames(ufa_sub)=="TBOX_STAT"]<-"status"
colnames(ufa_sub)[colnames(ufa_sub)=="DISEASE"]<-"disease"
colnames(ufa_sub)[colnames(ufa_sub)=="CONDITION"]<-"condition"

ufa_sub <- ufa_sub[which(!is.na(ufa_sub$longitude)),]

setwd('~/Code/tree-map/data/')
coordinates(ufa_sub) <- ~longitude+latitude
wards <- readOGR('wards','wards')
ufa_sub <- IntersectPtWithPoly(ufa_sub, wards)
colnames(ufa_sub)[colnames(ufa_sub)=="WARD"]<-"ward"

setwd('~/Code/tree-map/data/')
coordinates(ufa_sub) <- ~longitude+latitude
neighborhoods <- readOGR('neighborhoods','neighborhoods')
ufa_sub <- IntersectPtWithPoly(ufa_sub, neighborhoods)
colnames(ufa_sub)[colnames(ufa_sub)=="subhood"]<-"neighborhood"

ufa_sub<-ufa_sub[,c("longitude","latitude","status","address","objectid","facilityid","neighborhood","condition","disease","ward")]

#Make data files

# condition.ward <- round(prop.table(table(ufa_sub$ward,ufa_sub$condition), 1),digits=3)
# condition.ward.df <- data.frame(condition.ward)
# colnames(condition.ward.df)<-c("ward","condition","freq.condition")
# condition.ward.df<-reshape(condition.ward.df, timevar="condition", idvar="ward", direction="wide")
# write.csv(condition.ward.df,"~/Code/tree-map/data/condition_ward.csv",row.names=FALSE)
# condition.ward.json <- toJSON(condition.ward.df,pretty=TRUE)
# write(condition.ward.json, "~/Code/tree-map/data/condition_ward.json")

# condition.neighborhood <- round(prop.table(table(ufa_sub$neighborhood,ufa_sub$condition), 1),digits=3)
# condition.neighborhood.df <- data.frame(condition.neighborhood)
# colnames(condition.neighborhood.df)<-c("neighborhood","condition","freq.condition")
# condition.neighborhood.df<-reshape(condition.neighborhood.df, timevar="condition", idvar="neighborhood", direction="wide")
# write.csv(condition.neighborhood.df,"~/Code/tree-map/data/condition_neighborhood.csv",row.names=FALSE)
# neighborhoods@data = data.frame(neighborhoods@data, condition.neighborhood.df[match(neighborhoods@data[,"subhood"], condition.neighborhood.df[,"neighborhood"]),])

status.ward.freq <- round(prop.table(table(ufa_sub$ward,ufa_sub$status), 1),digits=3)
status.ward.freq.df <- data.frame(status.ward.freq)
colnames(status.ward.freq.df)<-c("ward","status","freq.status")
status.ward.freq.df<-reshape(status.ward.freq.df, timevar="status", idvar="ward", direction="wide")
write.csv(status.ward.freq.df,"~/Code/tree-map/data/status_ward.csv",row.names=FALSE)
status.ward.freq.json <- toJSON(status.ward.freq.df,pretty=TRUE)
write(status.ward.freq.json, "~/Code/tree-map/data/status_ward_freq.json")
# wards@data = data.frame(wards@data, status.ward.freq.df[match(wards@data[,"WARD"], status.ward.freq.df[,"ward"]),])
# toGeoJSON(data=wards, name="status_ward_freq", dest="~/Code/tree-map/data", lat.lon=c("latitude","longitude"), overwrite=TRUE)

status.ward.count <- table(ufa_sub$ward,ufa_sub$status)
status.ward.count.df <- data.frame(status.ward.count)
colnames(status.ward.count.df)<-c("ward","status","count.status")
status.ward.count.df<-reshape(status.ward.count.df, timevar="status", idvar="ward", direction="wide")

status.ward.df <- merge(status.ward.freq.df, status.ward.count.df, by="ward")
status.ward.json <- toJSON(status.ward.df,pretty=TRUE)
write(status.ward.json, "~/Code/tree-map/data/status_ward.json")

status.neighborhood <- round(prop.table(table(ufa_sub$neighborhood,ufa_sub$status), 1),digits=3)
status.neighborhood.df <- data.frame(status.neighborhood)
colnames(status.neighborhood.df)<-c("neighborhood","status","freq.status")
status.neighborhood.df<-reshape(status.neighborhood.df, timevar="status", idvar="neighborhood", direction="wide")
write.csv(status.neighborhood.df,"~/Code/tree-map/data/status_neighborhood.csv",row.names=FALSE)
# neighborhoods@data = data.frame(neighborhoods@data, status.neighborhood.df[match(neighborhoods@data[,"subhood"], status.neighborhood.df[,"neighborhood"]),])
# toGeoJSON(data=neighborhoods, name="status_ward_freq", dest="~/Code/tree-map/data", lat.lon=c("latitude","longitude"), overwrite=TRUE)

ufa_open<-ufa_sub[which(ufa_sub$status=='Open' | ufa_sub$status=='OPEN'),]

ufa_open<-ufa_open[,c("longitude","latitude","address","objectid","facilityid","neighborhood","ward")]

write.csv(ufa_open,"~/Code/tree-map/data/ufa_open.csv",row.names=FALSE)
write.csv(ufa_open,"~/Dropbox/ufa_open.csv",row.names=FALSE)
