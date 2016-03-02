#This is an R script to analyze the MMMILC Project data from 2015

#clear all variables
rm(list=ls())
graphics.off()
pardefault <- par(no.readonly = T)

#load packages, 
library(ggplot2)
library(rmarkdown)
library(knitr)
library(plyr)
library(gridExtra)

#se function that removes NA's
std.err <- function(x) sd(x[!is.na(x)])/sqrt(length(x[!is.na(x)]))

#setwd("/Users/mmcmunn/Desktop/GitHub/MMMILC-2015/") #marshall laptop
#setwd("/Users/mmcmunn/Desktop/GitHub/MMMILC-2015/")
#setwd("C:\\Users\\louie\\Documents\\GitHub\\MMMILC-2015") #LHY SP4
setwd("C:\\Users\\louie\\Documents\\GitHub\\MMMILC-2015") #LHY desktop

trip<-read.csv("trip 2016-02-09.csv",header=T,strip.white=T,na.strings= c(" ", "")) #trip log
data<-read.csv("data 2016-02-09.csv",header=T,strip.white=T,na.strings= c(" ", "")) #observations


#order by date, then by milkweed.ID
data<-data[order(data$date, data$milkweed.ID),]

data$milkweed.status<-as.character(data$milkweed.status)

#cleaning object data
#any observations 0 percent.green, but ALIVE status. replace with DEAD
data[ which(data$percent.green==0 & data$milkweed.status=="ALIVE") , "milkweed.status"] <- "DEAD"

#any observations NA percent.green, but ALIVE status. action TBD, but real data here, so leaving intact.
data[ which(is.na(data$percent.green) & data$milkweed.status=="ALIVE") , ]

#any observation with a non-zero percent green and NA for status, replace with ALIVE
data[ which(data$percent.green > 0 & is.na(data$milkweed.status)) , "milkweed.status"] <- "ALIVE"

#observations with 0 or only numeric in stage.length field. set to none.
data[grep("^[0-9]*$", data$stage.length) , "stage.length"] <- "none"

#make any version of NONE, None, etc -> "none"
data[grep("[Nn][Oo][Nn][Ee]", data$stage.length), "stage.length"] <- "none"

data$date<-as.Date(data$date, "%m/%d/%Y") 
trip$date<-as.Date(trip$date, "%m/%d/%Y")

#exclude training data
data<-data[data$date>"2015-04-26",]
trip<-trip[trip$date>"2015-04-26",]

#exclude blank plant ID
data <- data[ - which(is.na(data$milkweed.ID)) , ]

#remove all rows where milkweed status is NA
data <- data[ - which(is.na(data$milkweed.status)) , ]

#check if all trips in trop log in data, and vis versa
#list trips that have a trip log, but no data
unique(trip$trip.ID)[which(is.na(match(unique(trip$trip.ID), unique(data$trip.ID))))]

#list trips that have data, but no trip log
trips.to.add <- unique(data$trip)[which(is.na(match(unique(data$trip), unique(trip$trip.ID))))]

#a loop to get data for each trip that occurs in data,and paste that trips basic information into trip log for matching
for (i in 1:length(trips.to.add)){
  #the first row that does not have a trip log entry 
  pull.row  <- data[which(data$trip.ID==trips.to.add[i])[1],]
  #rename the name column to name.1
  colnames(pull.row)[which(colnames(pull.row)=="name")] <- "name.1"
  #add this data at the end of trip log
  trip <- rbind.fill(trip, pull.row[,c("trip.ID", "date", "name.1") ] )
}

#list NA's and deal with any if present
data[which(is.na(data$trip.ID)),]


#calculate the project.day and week
trip$julian.date<-as.integer(julian(trip$date, origin=as.Date("2015-01-01")))
trip$project.day<-trip$julian.date-min(trip$julian.date)+1
trip$week<-(trip$project.day-1) %/% 7+1



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

#which trip.ID's contain multiple dates?
trip.counts <- data.frame("number.dates"=rep(NA, length(unique(data$trip.ID))), "trip.ID"=rep(NA, length(unique(data$trip.ID))))
for (i in 1:length(unique(data$trip.ID))){
  allTrips <- unique(data$trip.ID)
   trip.counts$number.dates[i] <- length(unique(data[data$trip.ID==allTrips[i],"date"]))
   trip.counts$trip.ID[i] <- allTrips[i]
}
trip.counts[trip.counts$number.dates>1,]

#merge trip and main data frames
data <- merge(trip, data, by.x = "trip.ID", by.y = "trip.ID")


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

#function to call all observations from a student and summarize
#add elements within this function to add rows to student summary table
###################################################
student.plots <- list()
student.summary <- function(student.name){
  student <- list()
  #list trips student.name was on
  student.obs <- which(student.name==data$name.1 | student.name==data$name.2 | student.name==data$name.3)
  
  #drop NA's
  student.obs <- sort(student.obs[!is.na(student.obs)])
  
  #get observations student.name was a part of
  dStudent <- data[student.obs,]
  
      #trips
      student$tripsTaken <- length(unique(dStudent$trip.ID))
    
      #monarchs found
      student$monarchsFound <- sum(dStudent$nLTotal)
    
      #live plants surveyed
      student$livePlants <- nrow(dStudent[dStudent$milkweed.status== "ALIVE" , ])
    
      #total plants surveyed
      student$totalPlants <- nrow(dStudent)
    
      #total time in field
      tripStudent <- unique(dStudent$trip.ID)
      tripStudent <- dStudent[match(tripStudent, dStudent$trip.ID),]
      student$timeInField <- sum(tripStudent$duration.min, na.rm = TRUE)
    
      #mean milkweed observation time
      student$timePerMilkweed <- weighted.mean( x = tripStudent$duration.min / tripStudent$milkweed.count , 
                                              w = tripStudent$milkweed.count, na.rm = TRUE)
  
      
  #make some plots, put them into a list to save for student reports
      #student monarch discovery rate compared to average
        monarchProb <- sapply(unique(data$week), 
                              function(x) sum (data[data$week == x , "nLTotal"]) / nrow(data[data$week == x ,]))
        
        studentMonarchProb <- sapply(unique(data$week), 
                                   function(x) sum (dStudent[dStudent$week == x , "nLTotal"]) / nrow(dStudent[dStudent$week == x ,]))
        monarchDetection <- data.frame(monarchProb = monarchProb, studentMonarchProb = studentMonarchProb)
        
        p1 <- qplot(data = monarchDetection, y = monarchProb) + geom_line() + ylab("p(monarch larva)") + xlab("week")
        p1 <- p1 + geom_point(size = 3,data = monarchDetection, aes(y = studentMonarchProb), colour = "red" )
        
        #student egg discovery rate compared to average
        eggProb <- sapply(unique(data$week), 
                              function(x) sum (data[data$week == x , "nEggs"]) / nrow(data[data$week == x ,]))
        
        studentEggProb <- sapply(unique(data$week), 
                                     function(x) sum (dStudent[dStudent$week == x , "nEggs"]) / nrow(dStudent[dStudent$week == x ,]))
        eggDetection <- data.frame(eggProb = eggProb, studentEggProb = studentEggProb)
        
        p2 <- qplot(data = eggDetection, y = eggProb) + geom_line() + ylab("p(egg)") + xlab("week")
        p2 <- p2 + geom_point(size = 3,data = eggDetection, aes(y = studentEggProb), colour = "red" )
        
        #time per plant by week, average vs student
        tripLevel <- data[match(sort(unique(data$trip.ID)), data$trip.ID) , ]
        weeklyTimeAvg <- sapply(unique(data$week), function(z) 
          weighted.mean( x = tripLevel[tripLevel$week == z ,"duration.min" ] /  tripLevel[tripLevel$week == z ,"milkweed.count" ] , 
                       w = tripLevel[tripLevel$week == z ,"milkweed.count"], na.rm = TRUE))
        
        weeklyStudentAvg <-         sapply(unique(data$week), function(z) 
          weighted.mean( x = tripStudent[tripStudent$week == z ,"duration.min" ] /  tripStudent[tripStudent$week == z ,"milkweed.count" ] , 
                         w = tripStudent[tripStudent$week == z ,"milkweed.count"], na.rm = TRUE))
        timeDevotion <- data.frame(weeklyTimeAvg, weeklyStudentAvg)
        
        p3 <- qplot(data = timeDevotion, y = weeklyTimeAvg) + geom_line() + ylab("minutes / plant") + xlab("week")
        p3 <- p3 + geom_point(size = 3,data = timeDevotion, aes(y = weeklyStudentAvg), colour = "red" )
        
        #student number of plants censused per week
        weekCount <- data.frame(week = 1:max(data$week), count = NA, cumMonarch = 0)
        weekCount[match(names(table(dStudent$week)) , weekCount$week) , "count"] <- table(dStudent$week)
        
        p4 <- qplot(data = weekCount, x = weekCount$week , y = as.numeric(weekCount$count)) 
             p4  <- p4 + geom_line() + xlab("week") + ylab("plants per week") 
             p4 <- p4 + coord_cartesian(xlim = c(0, max(data$week))) + coord_cartesian(ylim = c(0, 1.2*max(weekCount$count, na.rm = TRUE))) 
        
        
        #cumulative monarch eggs and larvae found on plants
        monarchs <- with(dStudent, tapply(dStudent$monarchLoad, dStudent$week, sum))
        weekCount[match(names(monarchs), weekCount$week) , "cumMonarch"] <- monarchs
        weekCount$cumMonarch <- cumsum(weekCount$cumMonarch)
        
        p5 <- qplot(data = weekCount, x = weekCount$week , y = as.numeric(weekCount$cumMonarch)) 
            p5  <- p5 + geom_line() + xlab("week") + ylab("cumulat. monarchs") 
            p5 <- p5 + coord_cartesian(xlim = c(0, max(data$week))) + coord_cartesian(ylim = c(0, 1.2*max(weekCount$cumMonarch, na.rm = TRUE))) 

            head(data)
            
        #average percent green in each week compared to average
        weekGreen <- with(data, tapply(percent.green, week, function(x) mean(x, na.rm = TRUE)))
        studentGreen <- with(dStudent, tapply(percent.green, week, function(x) mean(x, na.rm = TRUE)))
        
        greenPlot <- data.frame(week = 1:max(data$week) , weekGreen = NA, studentGreen = NA)
        greenPlot[match(names(studentGreen), greenPlot$week) , "studentGreen"] <- studentGreen
        greenPlot[match(names(weekGreen), greenPlot$week) , "weekGreen"] <- weekGreen
        
        p6 <- qplot(data = greenPlot, y = weekGreen) + geom_line() 
        p6 <- p6 + ylab("avg % green") + xlab("week") + coord_cartesian(ylim = c(min(c(weekGreen, studentGreen)*.6, na.rm = TRUE), 100))
        p6 <- p6 + geom_point(size = 3,data = greenPlot, aes(y = studentGreen), colour = "red" )
      
        #create a list of all plots for this student, append that list of plots for later plotting, and plot them now
        student.plots[[student.name]] <<- list(p1, p2, p3, p4, p5, p6)
       # do.call(grid.arrange, student.plots[[student.name]])
  #convert summary list to data.frame and print
  data.frame(student)
}

student.summary("Louie Yang")

#make a list of names, remove NA value
name.list <- unique(c(as.character(trip$name.1), as.character(trip$name.2)))
name.list <- sort(name.list[!is.na(name.list)])

#apply student.summary function to all students, create a data frame form the result
student.df <- data.frame(sapply( name.list, function(x) student.summary(x)))

#add a column of averages using a mean function that can deal with NaN's and NA's
student.df$average <- sapply(1:nrow(student.df), function(x) mean(unlist( student.df[x,]), na.rm = TRUE))

#remove spaces for pdf file names, add current date for report and pdf extension
file.names.list <- paste(format(Sys.time(), '%m-%d-%Y'),"_",
                         gsub(" ", "_", name.list), 
                         ".pdf", sep = "")

#this is the loop to run the report generating script
#for each student name in name.list, it generates a report including summary statistics from
#student.df and plots several things
<<<<<<< Updated upstream
setwd("C:\\Users\\louie\\Documents\\GitHub\\MMMILC-2015\\student reports")
=======

#get the last week interval
last.week <- sort(seq( min(data$julian.date), min(data$julian.date) + max(data$week)*7, by = 7), decreasing = TRUE)[c(2,1)]
last.week <- format(as.Date(last.week, origin=as.Date("2015-01-01")) ,  '%d %b')
last.week <- paste("week", " ", max(data$week), ": ", last.week[1], " - ",last.week[2], sep = "" )

setwd("/Users/mmcmunn/Desktop/GitHub/MMMILC-2015/student reports")
>>>>>>> Stashed changes
    for(i in 1:length(name.list)){
            student.name <- name.list[i]
            render(input = "student report.Rmd", output_format = "pdf_document", 
                  output_file = file.names.list[i] )
            }
#switch back to main working dir
setwd("C:\\Users\\louie\\Documents\\GitHub\\MMMILC-2015")

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