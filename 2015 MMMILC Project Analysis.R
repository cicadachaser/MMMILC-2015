#This is an R script to analyze the MMMILC Project data from 2015

#clear all variables
rm(list=ls())
graphics.off()
pardefault <- par(no.readonly = T)

#load packages, se function
library(ggplot2)
std.err <- function(x) sd(x)/sqrt(length(x))

#setwd("C:\\Users\\lhyang\\Dropbox\\Milkweeds and Monarchs\\The MMMILC Project") #louie laptop
#setwd("/Users/mmcmunn/Desktop/GitHub/MMMILC-2015/") #marshall laptop
setwd("/Users/mmcmunn/Desktop/GitHub/MMMILC-2015/")

trip<-read.csv("trip 2016-01-13.csv",header=T,strip.white=T,na.strings= c(" ", "")) #trip log
data<-read.csv("data 2016-01-13.csv",header=T,strip.white=T,na.strings= c(" ", "")) #observations

data$milkweed.status<-as.character(data$milkweed.status)

data$date<-as.Date(data$date, "%m/%d/%Y") 
trip$date<-as.Date(trip$date, "%m/%d/%Y")

#order by date, then by milkweed.ID
data<-data[order(data$date, data$milkweed.ID),]


#exclude training data
data<-data[data$date>"2015-04-26",]
trip<-trip[trip$date>"2015-04-26",]

#calculate the project.day and week
data$julian.date<-as.integer(julian(data$date, origin=as.Date("2015-01-01")))
data$project.day<-data$julian.date-min(data$julian.date, na.rm=TRUE)+1
data$week<-(data$project.day-1) %/% 7+1

trip$julian.date<-as.integer(julian(trip$date, origin=as.Date("2015-01-01")))
trip$project.day<-trip$julian.date-min(trip$julian.date)+1
trip$week<-(trip$project.day-1) %/% 7+1

#data cleaning
#any observations 0 percent.green, but ALIVE status. replace with DEAD
data[ which(data$percent.green==0 & data$milkweed.status=="ALIVE") , ]
"DEAD" <- data[ which(data$percent.green==0 & data$milkweed.status=="ALIVE") , "milkweed.status"]

#observations with 0 or only numeric in stage.length field. set to none.
grep("^[0-9]*$", data$stage.length)
data[grep("^[0-9]*$", data$stage.length) , "stage.length"] <- "none"

#make any version of NONE, None, etc -> "none"
data[grep("[Nn][Oo][Nn][Ee]", data$stage.length), "stage.length"] <- "none"

unique(data$stage.length)


#plot of milkweed.count by week
milkweed.count.by.week<-aggregate(milkweed.count~week,sum,data=trip);milkweed.count.by.week
p1 <- ggplot(milkweed.count.by.week, aes(x=week, y=milkweed.count))
p1+geom_point(size=6,col="red")+geom_line()+geom_hline(yintercept=318,lty='dashed')+coord_cartesian(ylim = c(0, 400))+scale_x_continuous(breaks=c(1:max(trip$week)))+ylab("milkweed count from the trip log")


#plot of milkweed.obs by week
milkweed.obs.by.week<-aggregate(obs.ID~week,length,data=data)
p1.1 <- ggplot(milkweed.obs.by.week, aes(x=week, y=obs.ID))
p1.1+geom_point(size=6,col="red")+geom_line()+geom_hline(yintercept=318,lty='dashed')+coord_cartesian(ylim = c(0, 400))+scale_x_continuous(breaks=c(1:max(trip$week)))+ylab("milkweed count from the data set")


#How long did it take to measure each milkweed?
speed.by.week<-aggregate(cbind(duration.min,milkweed.count)~week,data=trip,sum); 
speed.by.week$minutes.per.milkweed<-with(speed.by.week,duration.min/milkweed.count);speed.by.week
p2<-ggplot(speed.by.week,aes(x=week,y=minutes.per.milkweed))
p2+geom_point(size=6,col="blue")+geom_line()+coord_cartesian(ylim = c(0, 6.5))+scale_x_continuous(breaks=c(1:max(trip$week)))

data$mean.len<-rowMeans(data[,which(colnames(data)=="len.1"):which(colnames(data)=="len.10")],na.rm=T)
data$mean.dia<-rowMeans(data[,which(colnames(data)=="dia.1"):which(colnames(data)=="dia.10")],na.rm=T)

#total stem len and area
data$stem.count<-as.integer(data$stem.count)
data$total.stem.len<-data$stem.count*data$mean.len
data$total.stem.area<-data$stem.count*data$mean.dia^2*pi

#plant size by week
total.stem.len.by.week<-aggregate(total.stem.len~week,function(x) c(mean=mean(x),se=std.err(x)),data=data);total.stem.len.by.week

total.stem.area.by.week<-aggregate(total.stem.area~week,function(x) c(mean=mean(x),se=std.err(x)),data=data);total.stem.area.by.week

#plant growth
p3.limits <- aes(ymax = total.stem.len[,"mean"] + total.stem.len[,"se"], ymin=total.stem.len[,"mean"] - total.stem.len[,"se"])
p3<-ggplot(total.stem.len.by.week,aes(x=week,y=total.stem.len[,"mean"]))
p3+geom_point(size=6,col="forestgreen")+geom_line()+scale_x_continuous(breaks=c(1:max(trip$week)))+ylab("total stem length per plant (cm)")+geom_errorbar(p3.limits, width=0.2)

p3.1.limits <- aes(ymax = total.stem.area[,"mean"] + total.stem.area[,"se"], ymin=total.stem.area[,"mean"] - total.stem.area[,"se"])
p3.1<-ggplot(total.stem.area.by.week,aes(x=week,y=total.stem.area[,"mean"]))
p3.1+geom_point(size=6,col="forestgreen")+geom_line()+scale_x_continuous(breaks=c(1:max(trip$week)))+ylab("total stem area per plant (sq mm)")+geom_errorbar(p3.1.limits, width=0.2)

#mapping and spatial analysis

####not yet working
library(ggmap)

mw.locs<-read.csv("milkweed coordinates 2015-05-23.csv")
mw.locs$transect<-as.factor(mw.locs$transect)

lowerleftlong=-121.765274
lowerleftlat= 38.566483
upperrightlong=-121.747067
upperrightlat= 38.576428

NDC<-get_map(location=c(lowerleftlong,lowerleftlat,upperrightlong,upperrightlat),maptype="terrain",source="google")

NDCmap<-ggmap(NDC, extent = "panel")

NDCmap+geom_point(aes(x=longitude,y=latitude,color=transect),data=mw.locs)

NDCmap+geom_point(aes(x=longitude,y=latitude,size=data[data$week==3,"total.stem.len"]),alpha=0.5,color="red",data=mw.locs[mw.locs$transect=="2",])

NDCmap+geom_point(aes(x=longitude,y=latitude,color=data[data$week==3,"leaf.damage"]),alpha=.5,size=4,data=mw.locs[mw.locs$transect=="2",])+scale_colour_gradient(limits=c(0, 100),low="green", high="red")