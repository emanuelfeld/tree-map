ufa <- read.csv('~/ufa.csv')

ufa_sub<-ufa[which(ufa$TBOX_STAT!='Retired'),]
ufa_sub<-ufa_sub[which(ufa_sub$TBOX_STAT!='Conflict'),]
ufa_sub<-ufa_sub[which(ufa_sub$TBOX_STAT!='Delete'),]
ufa_sub<-ufa_sub[which((ufa_sub$DBH>0)==FALSE | is.na(ufa_sub$DBH>0)),]
ufa_sub<-ufa_sub[,c("X","Y","VICINITY","OBJECTID","FACILITYID")]
colnames(ufa_sub)[colnames(ufa_sub)=="X"]<-"longitude"
colnames(ufa_sub)[colnames(ufa_sub)=="Y"]<-"latitude"
colnames(ufa_sub)[colnames(ufa_sub)=="VICINITY"]<-"address"
colnames(ufa_sub)[colnames(ufa_sub)=="OBJECTID"]<-"objectid"
colnames(ufa_sub)[colnames(ufa_sub)=="FACILITYID"]<-"facilityid"

write.csv(ufa_sub,"~/ufa_empty.csv",row.names=FALSE)