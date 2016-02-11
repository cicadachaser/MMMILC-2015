#This is an R script to analyze the MMMILC Project data from 2015

#clear all variables
rm(list=ls())
graphics.off()
pardefault <- par(no.readonly = T)

#load packages, se function that removes NA's
library(ggplot2)
std.err <- function(x) sd(x[!is.na(x)])/sqrt(length(x[!is.na(x)]))

setwd("/Users/mmcmunn/Desktop/GitHub/MMMILC-2015/") #marshall laptop
#setwd("/Users/mmcmunn/Desktop/GitHub/MMMILC-2015/")
#setwd("C:\\Users\\louie\\Documents\\GitHub\\MMMILC-2015") #LHY SP4

trip<-read.csv("trip 2016-02-09.csv",header=T,strip.white=T,na.strings= c(" ", "")) #trip log
data<-read.csv("data 2016-02-09.csv",header=T,strip.white=T,na.strings= c(" ", "")) #observations

data$milkweed.status<-as.character(data$milkweed.status)

data$date<-as.Date(data$date, "%m/%d/%Y") 
trip$date<-as.Date(trip$date, "%m/%d/%Y")

#order by date, then by milkweed.ID
data<-data[order(data$date, data$milkweed.ID),]

#exclude training data
data<-data[data$date>"2015-04-26",]
trip<-trip[trip$date>"2015-04-26",]

#exclude blank plant ID
data[ - which(is.na(data$milkweed.ID)) , ]

#calculate the project.day and week
data$julian.date<-as.integer(julian(data$date, origin=as.Date("2015-01-01")))
data$project.day<-data$julian.date-min(data$julian.date, na.rm=TRUE)+1
data$week<-(data$project.day-1) %/% 7+1

trip$julian.date<-as.integer(julian(trip$date, origin=as.Date("2015-01-01")))
trip$project.day<-trip$julian.date-min(trip$julian.date)+1
trip$week<-(trip$project.day-1) %/% 7+1

#data cleaning
#any observations 0 percent.green, but ALIVE status. replace with DEAD
data[ which(data$percent.green==0 & data$milkweed.status=="ALIVE") , "milkweed.status"] <- "DEAD"

#any observations NA percent.green, but ALIVE status. action TBD
data[ which(is.na(data$percent.green) & data$milkweed.status=="ALIVE") , ]

#any observation with a non-zero percent green and NA for status, replace with ALIVE
data[ which(data$percent.green > 0 & is.na(data$milkweed.status)) , "milkweed.status"] <- "ALIVE"

#remove all rows where milkweed status is NA
data <- data[ - which(is.na(data$milkweed.status)) , ]

#observations with 0 or only numeric in stage.length field. set to none.
data[grep("^[0-9]*$", data$stage.length) , "stage.length"] <- "none"

#make any version of NONE, None, etc -> "none"
data[grep("[Nn][Oo][Nn][Ee]", data$stage.length), "stage.length"] <- "none"


##dealing with the stage.length field
#split the field by commas
split <- strsplit(as.character(data$stage.length), split = ",")

#add number of larvae column
data$nLTotal <- unlist(lapply(split,  function(x) sum((grepl("[L1-5]", x) ) ) ) )

#add number of eggs column
data$nEggs <- unlist(lapply(split,  function(x) sum((grepl("[E]", x) ) ) ) )

#add monarch load column
data$monarchLoad <- data$nLTotal + data$nEggs

#add L class specific counts
data$nL1 <- as.numeric(unlist(lapply(split,  function(x) sum((grepl("[L][1]", x) ) ) ) ) )
data$nL2 <- as.numeric(unlist(lapply(split,  function(x) sum((grepl("[L][2]", x) ) ) ) ) )
data$nL3 <- as.numeric(unlist(lapply(split,  function(x) sum((grepl("[L][3]", x) ) ) ) ) )
data$nL4 <- as.numeric(unlist(lapply(split,  function(x) sum((grepl("[L][4]", x) ) ) ) ) )
data$nL5 <- as.numeric(unlist(lapply(split,  function(x) sum((grepl("[L][5]", x) ) ) ) ) )

#check if sum of larval class counts is equal to total larvae.
which(data$nLTotal!=rowSums(cbind(data$nL1, data$nL2, data$nL3, data$nL4, data$nL5)))

#a function to collect all measurements by instar, impute all missing L measurements according to instar, convert all to numeric
impute.for.instar <- function(instar.number, split.list = split){
  #get all matches for a particular instar
  Llist <- lapply(split,  function(x) x[grep(paste("[L][",instar.number,"]", sep = ""), x)] )
  #split each match by hyphen, and take the second part, the measurement. 
  #note: if no length measurement given but L# present, results NA
  Llengths <- lapply(Llist, function(y) sapply(strsplit(y, "-"),  function (x) x[2]))
  #mean of this instar
  Lmean <- mean(as.numeric(unlist(Llengths)), na.rm=TRUE)
  #impute all "L#" without lengths (these are NA's instead of empty lists)
  Llengths <- rapply(Llengths, f=function(x) ifelse(is.na(x) , Lmean, as.numeric(x)), how = "replace")
  invisible(Llengths)
}

#impute all "L#" and convert to numerics within lists
data$L1lengths <- impute.for.instar(1)
data$L2lengths <- impute.for.instar(2)
data$L3lengths <- impute.for.instar(3)
data$L4lengths <- impute.for.instar(4)
data$L5lengths <- impute.for.instar(5)

#make column for all lengths
Llist <- mapply( c, data$L1lengths, data$L2lengths, data$L3lengths, data$L4lengths, data$L5lengths)
data$catLengths <- lapply(Llist, unlist)


#function to call all observations from a student and summarize or plot data
student.summary <- function(student.name){
  #list trips student.name was on
  trips  <- c  ( trip[student.name == trip$name.1, "trip.ID"] , 
                 trip[student.name == trip$name.2, "trip.ID"] )
  #drop NA's
  trips <- sort(trips[!is.na(trips)])
  
  #get observations student.name was a part of
  observations <- which(!is.na(match(data$trip.ID, trips)))
  dStudent <- data[observations,]
  
  #trips
    tripsTaken <- length(trips)
  #monarchs found
    monarchsFound <- sum(dStudent$nLTotal)
    monarchsFound
  #live plants surveyed
    livePlants <- nrow(dStudent[dStudent$milkweed.status== "ALIVE" , ])
    livePlants
  #total plants surveyed
    totalPlants <- nrow(dStudent)
    totalPlants
  #productivity per week
   plot(table(dStudent$julian.date), type = "l", 
        xlab = "monarchs per day", ylab = "julian date", main = student.name)
    list(tripsTaken = tripsTaken, monarchsFound = monarchsFound, livePlants = livePlants, 
         totalPlants = totalPlants)
}

student.summary("Louie Yang")

sum(trip$name.1=="Louie Yang")
sum(trip$name.2=="Louie Yang", na.rm=TRUE)
sum(trip$name.3=="Louie Yang", na.rm=TRUE)

#apply student.summary to all students
sapply(sort(unique(c(as.character(trip$name.1), as.character(trip$name.2)))), 
       function(x) student.summary(x))

###########
##plots

#a function to add data to density graph, both density estimate and points
add.plot.instar.data <- function(unlisted.instar.length, color = 1) {
  lines(density(unlisted.instar.length) , lwd = 2, col = color)
  points(unlisted.instar.length, y=rep(0,length(unlisted.instar.length) ) , col=color) }
#plot larval lengths by instar
plot(1,1,type="n", xlim = c(0,60), ylim = c(0,.25), main = "larval lengths by instar", xlab = "length (mm)", ylab ="frequency")
add.plot.instar.data(unlist(data$L1lengths), 1)
add.plot.instar.data(unlist(data$L2lengths), 2)
add.plot.instar.data(unlist(data$L3lengths), 3)
add.plot.instar.data(unlist(data$L4lengths), 4)
add.plot.instar.data(unlist(data$L5lengths), 5)


###plots
#average larval length by week
#egg count by week
#larvae count by week
#monarch load (egg + larveae) by week

#cumulative versions by day (monarchs per plant on plants observed)


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


#plot egg counts by week and cumulative by day
eggs.by.week<-aggregate(nEggs~week,sum,data=data)
p4.1 <- ggplot(eggs.by.week, aes(x=week, y=nEggs))
p4.1+geom_point(size=6,col="red")+geom_line()+geom_hline(yintercept=318,lty='dashed')+coord_cartesian(ylim = c(0, 50))+
  scale_x_continuous(breaks=c(1:max(trip$week)))+ylab("Eggs per week")

#plot larvae counts by week and cumulative by day
larvae.by.week<-aggregate(nLTotal~week,sum,data=data)
p5.1 <- ggplot(larvae.by.week, aes(x=week, y=nLTotal))
p5.1+geom_point(size=6,col="red")+geom_line()+geom_hline(yintercept=318,lty='dashed')+coord_cartesian(ylim = c(0, 50))+
  scale_x_continuous(breaks=c(1:max(trip$week)))+ylab("Larvae per week")

#plot egg + larvae counts by week
monarch.by.week<-aggregate(monarchLoad~week,sum,data=data)
p6.1 <- ggplot(monarch.by.week, aes(x=week, y=monarchLoad))
p6.1+geom_point(size=6,col="red")+geom_line()+geom_hline(yintercept=318,lty='dashed')+coord_cartesian(ylim = c(0, 50))+
  scale_x_continuous(breaks=c(1:max(trip$week)))+ylab("Monarch per week")

#cumulative larvae by day
larvae.by.day <- aggregate(nLTotal ~ julian.date,sum, data = data)
cum.larvae.by.day <- larvae.by.day
cum.larvae.by.day[,2] <- cumsum(cum.larvae.by.day$nLTotal) 
with(cum.larvae.by.day, plot(julian.date, nLTotal, type = "l"))
#add lines for each instar
#fill in polygons to represent instar abundance

#cumulative L1
L1.by.day <- aggregate(nL1 ~ julian.date,sum, data = data)
with(L1.by.day, plot(julian.date,cumsum(nL1), type = "l", ylim = c(0,75)))

L2.by.day <- aggregate(nL2 ~ julian.date,sum, data = data)

lines(L1.by.day[,1], cumsum(L1.by.day[,2] + L2.by.day[,2]))


#cumulative egg by day
eggs.by.day <- aggregate(nEggs ~ julian.date,sum, data = data)
cum.eggs.by.day <- eggs.by.day
cum.eggs.by.day[,2] <- cumsum(cum.eggs.by.day$nEggs) 
with(cum.eggs.by.day, plot(julian.date, nEggs, type = "l"))


#plot phenology-ontogeny landscape plotting function
phen.ont.landscape <- function(interval.as.char){
    cat.length <- tapply(data$catLengths ,  cut(data$date, interval.as.char) ,    
                         function(x) mean(as.numeric(unlist(x)), na.rm=TRUE))
    
    cat.se <-  tapply(data$catLengths ,  cut(data$date, interval.as.char) ,    
                      function(x) std.err(as.numeric(unlist(x))))
    
    plant.area <- tapply(data$total.stem.area ,  cut(data$date, interval.as.char) ,    
                     function(x) mean(as.numeric(x), na.rm=TRUE))
    
    plant.se <-  tapply(data$total.stem.area ,  cut(data$date, interval.as.char) ,    
                      function(x) std.err(as.numeric(x)))
    
    plot(as.Date(names(cat.length)), cat.length, type = "b", pch = 16, xlab = "date")
    plot(as.Date(names(plant.area)), plant.area, type = "b", pch = 16, xlab = "date")
    
    plot(cat.length, plant.area, xlim = c(0, 35), ylim = c(0, 450), type = "b", 
                      main = paste("phenology-ontogeny: ", interval.as.char, sep = ""))
    
    arrows(cat.length - cat.se, plant.area, cat.length + cat.se, plant.area, length = 0)
    arrows(cat.length, plant.area - plant.se, cat.length, plant.area + plant.se, length = 0)
        
}


with(data,   aggregate(cbind(nL1, nL2, nL3, nL4, nL5), list(cut(date, "month")) , sum))


phen.ont.landscape("1 weeks")
phen.ont.landscape("2 weeks")
phen.ont.landscape("3 weeks")
phen.ont.landscape("4 weeks")
phen.ont.landscape("6 weeks")

unique(data$catLengths)
rapply(data$L1lengths, function(x) x[x<5])

#update trip.csv

###plots
#average larval length by week
#cumulative versions by day (monarchs per plant on plants observed)



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