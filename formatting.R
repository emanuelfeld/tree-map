par(mfrow=c(1,1))
h.2015 = hist(plant.all.2015$DATE,breaks="weeks",xlab="",ylab="requests",main="2015",plot=TRUE,freq=TRUE,ylim=c(0,550))
# h.2014 = hist(plant.all.2014$DATE,breaks="weeks",xlab="",ylab="2014",main="",plot=TRUE,freq=TRUE,ylim=c(0,550))
# h.2013 = hist(plant.all.2013$DATE,breaks="weeks",xlab="",ylab="2013",main="",plot=TRUE,freq=TRUE,ylim=c(0,550))
# h.2012 = hist(plant.all.2012$DATE,breaks="weeks",xlab="",ylab="2012",main="",plot=TRUE,freq=TRUE,ylim=c(0,550))
# h.2011 = hist(plant.all.2011$DATE,breaks="weeks",xlab="",ylab="2011",main="",plot=TRUE,freq=TRUE,ylim=c(0,550))

multiplot(h.2015,h.2014,h.2013,h.2012,h.2011,cols=1)

freq.ward.all = table(plant.sub.all$YEAR,plant.sub.all$WARD)
freq.ward.all.df = data.frame(freq.ward.all)
colnames(freq.ward.all.df) = c("year","ward","freq")
freq.ward.all.df<-reshape(freq.ward.all.df, timevar="year", idvar="ward", direction="wide")
freq.ward.all.json = toJSON(freq.ward.all.df,pretty=TRUE)
write(freq.ward.all.json, "~/Code/tree-map/data/outcome_ward.json")

# create frequency tables with all dates in range (not just those present)

days.freq.2015 = data.frame(table(plant.all.2015$DATE))
colnames(days.freq.2015) = c("day","freq")
days.freq.2015$day = as.character(days.freq.2015$day)
day = data.frame(seq(as.Date("2015-01-01"), as.Date("2015-06-15"), by="1 day"))
colnames(day) = c("day")
day$day = as.character(day$day)
days.freq.2015 = merge(days.freq.2015,day,by="day",all.y=TRUE)
days.freq.2015[is.na(days.freq.2015)] <- 0
days.freq.2015.json = toJSON(days.freq.2015,pretty=TRUE)
write(days.freq.2015.json, "~/Code/tree-map/data/days_freq.json")

round(prop.table(freq.ward.all, 1)*100,digits=1)

p.2015 = percentHist(plant.post.2015,"2015")
p.2014 = percentHist(plant.2014,"2014")
p.2013 = percentHist(plant.2013,"2013")
p.2012 = percentHist(plant.2012,"2012")
p.2011 = percentHist(plant.2011,"2011")

multiplot(p.2015,p.2014,p.2013,p.2012,p.2011,cols=5)

m.2015 = qplot(plant.2015$DATE,ylim=c(0,350),ylab="",xlab="",main="2015",binwidth=1,geometry="histogram") + theme(axis.text.x = element_blank(),axis.ticks.x = element_blank())
m.2014 = qplot(plant.2014$DATE,ylim=c(0,350),ylab="",xlab="",main="2014",geometry="histogram") + theme(axis.text.x = element_blank(),axis.ticks.x = element_blank())
m.2013 = qplot(plant.2013$DATE,ylim=c(0,350),ylab="",xlab="",main="2013",geometry="histogram") + theme(axis.text.x = element_blank(),axis.ticks.x = element_blank())
m.2012 = qplot(plant.2012$DATE,ylim=c(0,350),ylab="",xlab="",main="2012",geometry="histogram") + theme(axis.text.x = element_blank(),axis.ticks.x = element_blank())
m.2011 = qplot(plant.2011$DATE,ylim=c(0,350),ylab="requests",xlab="",main="2011",geometry="histogram") + theme(axis.text.x = element_blank(),axis.ticks.x = element_blank())

multiplot(m.2011,m.2012,m.2013,m.2014,m.2015,cols=5)

plant.2015.rest = plant[plant$DATE>=d(1420088400000) & plant$DATE<d(april.20.2015-interval*0),]
p.2015.rest = percentHist(plant.2015.rest,"Before Launch")
p.2015 = percentHist(plant.2015,"After Launch")
multiplot(p.2015,p.2015.rest,cols=2)

qplot(plant.2015[plant.2015$WARD==1,]$date,ylim=c(0,50),ylab="requests",xlab="",main="2015",geometry="histogram")
qplot(plant.2015[plant.2015$WARD==2,]$date,ylim=c(0,50),ylab="requests",xlab="",main="2015",geometry="histogram")
qplot(plant.2015[plant.2015$WARD==3,]$date,ylim=c(0,50),ylab="requests",xlab="",main="2015",geometry="histogram")
qplot(plant.2015[plant.2015$WARD==4,]$date,ylim=c(0,50),ylab="requests",xlab="",main="2015",geometry="histogram")
qplot(plant.2015[plant.2015$WARD==5,]$date,ylim=c(0,50),ylab="requests",xlab="",main="2015",geometry="histogram")
qplot(plant.2015[plant.2015$WARD==6,]$date,ylim=c(0,50),ylab="requests",xlab="",main="2015",geometry="histogram")
qplot(plant.2015[plant.2015$WARD==7,]$date,ylim=c(0,50),ylab="requests",xlab="",main="2015",geometry="histogram")
qplot(plant.2015[plant.2015$WARD==8,]$date,ylim=c(0,50),ylab="requests",xlab="",main="2015",geometry="histogram")

qplot(ufa.p$DATE,ylim=c(0,750),main="DC Tree Planting Requests, January to June 2015",xlab="",ylab="requests",geometry="histogram")

ggplot(plant.2015, aes(x = WARD, fill=WARD)) +  
  geom_bar(aes(y = (..count..)), binwidth = 20) + 
  xlab("Ward") + ylab("Requests") + ggtitle("Tree Planting Requests by Ward (since April 20)\n")+
  guides(fill=FALSE)+theme(plot.title = element_text(face="bold", size=20),axis.title.y = element_text(face="bold", size=20,vjust=1),axis.title.x = element_text(face="bold", size=20,vjust=0),axis.text.x  = element_text(face="bold",size=20),axis.text.y  = element_text(face="bold",size=20))
