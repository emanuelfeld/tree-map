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


ufa <- read.csv('~/Desktop/ufa.csv')

ufa_sub <- ufa[which(ufa$TBOX_STAT!='Retired'),]
ufa_sub <- ufa_sub[which(ufa_sub$TBOX_STAT!='Conflict'),]
ufa_sub <- ufa_sub[which(ufa_sub$TBOX_STAT!='Delete'),]
ufa_sub$TBOX_STAT <- sub("Open","Open",ufa_sub$TBOX_STAT,ignore.case=TRUE)
ufa_sub$TBOX_STAT <- sub("^$","Unknown",ufa_sub$TBOX_STAT,ignore.case=TRUE)

ufa_sub<-ufa_sub[which(ufa_sub$OWNERSHIP=='UFA' | ufa_sub$OWNERSHIP==''),]
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

ufa_sub <- ufa_sub[,c("X","Y","OBJECTID","FACILITYID","VICINITY","TBOX_STAT","DATE_PLANT","DISEASE","PESTS","CONDITION","CONDITIODT","TREE_NOTES","OWNERSHIP")]

colnames(ufa_sub)[colnames(ufa_sub)=="X"]<-"longitude"
colnames(ufa_sub)[colnames(ufa_sub)=="Y"]<-"latitude"
colnames(ufa_sub)[colnames(ufa_sub)=="VICINITY"]<-"address"
colnames(ufa_sub)[colnames(ufa_sub)=="OBJECTID"]<-"objectid"
colnames(ufa_sub)[colnames(ufa_sub)=="FACILITYID"]<-"facilityid"
colnames(ufa_sub)[colnames(ufa_sub)=="TBOX_STAT"]<-"status"
colnames(ufa_sub)[colnames(ufa_sub)=="DISEASE"]<-"disease"
colnames(ufa_sub)[colnames(ufa_sub)=="CONDITION"]<-"condition"

coordinates(ufa_sub) <- ~longitude+latitude

setwd('~/Code/dctrees/')
boundaries <- readOGR('boundaries','boundaries')

setwd('~/Code/dctrees/')
wards <- readOGR('WardPly','WardPly')

ufa_sub <- IntersectPtWithPoly(ufa_sub, boundaries)

colnames(ufa_sub)[colnames(ufa_sub)=="subhood"]<-"neighborhood"
colnames(ufa_sub)[colnames(ufa_sub)=="WARD"]<-"ward"

ufa_sub<-ufa_sub[,c("longitude","latitude","status","address","objectid","facilityid","neighborhood","ward","condition","disease")]

condition.ward <- round(prop.table(table(ufa_sub$ward,ufa_sub$condition), 1)*100,digits=1)
condition.ward.df <- data.frame(condition.ward)
colnames(condition.ward.df)<-c("ward","condition","freq.condition")
condition.ward.df<-reshape(condition.ward.df, timevar="condition", idvar="ward", direction="wide")

# write.csv(condition.ward.df,"~/Code/tree-map/data/condition_ward.csv",row.names=FALSE)
condition.ward.json <- toJSON(condition.ward.df,pretty=TRUE)
write(condition.ward.json, "~/Code/tree-map/data/condition_ward.json")

condition.neighborhood <- round(prop.table(table(ufa_sub$neighborhood,ufa_sub$condition), 1)*100,digits=1)
condition.neighborhood.df <- data.frame(condition.neighborhood)
colnames(condition.neighborhood.df)<-c("neighborhood","condition","freq.condition")
condition.neighborhood.df<-reshape(condition.neighborhood.df, timevar="condition", idvar="neighborhood", direction="wide")
# write.csv(condition.neighborhood.df,"~/Code/tree-map/data/condition_neighborhood.csv",row.names=FALSE)
# boundaries@data = data.frame(boundaries@data, condition.neighborhood.df[match(boundaries@data[,"subhood"], condition.neighborhood.df[,"neighborhood"]),])

status.ward <- round(prop.table(table(ufa_sub$ward,ufa_sub$status), 1)*100,digits=1)
status.ward.df <- data.frame(status.ward)
colnames(status.ward.df)<-c("ward","status","freq.status")
status.ward.df<-reshape(status.ward.df, timevar="status", idvar="ward", direction="wide")
write.csv(status.ward.df,"~/Code/tree-map/data/status_ward.csv",row.names=FALSE)
status.ward.json <- toJSON(status.ward.df,pretty=TRUE)
write(status.ward.json, "~/Code/tree-map/data/status_ward.json")
wards@data = data.frame(wards@data, status.ward.df[match(wards@data[,"WARD"], status.ward.df[,"ward"]),])
toGeoJSON(data=wards, name="status_ward", dest="~/Code/tree-map/data", lat.lon=c("latitude","longitude"), overwrite=TRUE)


status.neighborhood <- round(prop.table(table(ufa_sub$neighborhood,ufa_sub$status), 1)*100,digits=1)
status.neighborhood.df <- data.frame(status.neighborhood)
colnames(status.neighborhood.df)<-c("neighborhood","status","freq.status")
status.neighborhood.df<-reshape(status.neighborhood.df, timevar="status", idvar="neighborhood", direction="wide")
# write.csv(status.neighborhood.df,"~/Code/tree-map/data/status_neighborhood.csv",row.names=FALSE)
# boundaries@data = data.frame(boundaries@data, status.neighborhood.df[match(boundaries@data[,"subhood"], status.neighborhood.df[,"neighborhood"]),])

ufa_open<-ufa_sub[which(ufa_sub$status=='Open' | ufa_sub$status=='OPEN'),]

ufa_open<-ufa_open[,c("longitude","latitude","address","objectid","facilityid","neighborhood","ward")]

write.csv(ufa_open,"~/ufa_open.csv",row.names=FALSE)