#This is an R script to analyze the MMMILC Project data from 2016

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
  library(ggmap)

#clear variables
  rm(list=ls())
  graphics.off()
  pardefault <- par(no.readonly = T)


#se function that removes NA's
  std.err <- function(x) sd(x[!is.na(x)])/sqrt(length(x[!is.na(x)]))

#set wd and load data
  setwd("/Users/mmcmunn/Desktop/GitHub/MMMILC-2015/2016 TRIAL DATA") #marshall laptop
#go up a directory for spatial data
  setwd('..')
  mw.locs<-read.csv("milkweed coordinates 2015-05-23.csv")
  setwd("/Users/mmcmunn/Desktop/GitHub/MMMILC-2015/2016 TRIAL DATA") #marshall laptop

#setwd("/Users/mmcmunn/Desktop/GitHub/MMMILC-2015/")
#setwd("C:\\Users\\louie\\Documents\\GitHub\\MMMILC-2015") #LHY SP4
#setwd("C:\\Users\\louie\\Documents\\GitHub\\MMMILC-2015") #LHY desktop

#load data
  data<-read.csv("2016 MMMILC Project Data.csv",header=T,strip.white=T,na.strings= c(" ", "")) #observations

#milkweed status to character  and monarch stage columns to character
  data$milkweed.status<-as.character(data$milkweed.status)
  data[,"monarch1.stage"] <- as.character(data[,"monarch1.stage"] )
  data[,"monarch2.stage"] <- as.character(data[,"monarch2.stage"] )
  data[,"monarch3.stage"] <- as.character(data[,"monarch3.stage"] )
  data[,"monarch4.stage"] <- as.character(data[,"monarch4.stage"] )
  data[,"monarch5.stage"] <- as.character(data[,"monarch5.stage"] )

# the two % columns to numerics
data$percent.green <- as.numeric(gsub("%", "", data$percent.green))
data$leaf.damage <- as.numeric(gsub("%", "", data$leaf.damage))

#date and time to date.time object
  data$date.time <- strptime(paste(data$date, data$time), format = "%m/%d/%Y %H:%M")
  data$julianDate <- as.numeric(strftime(data$date.time, format = "%j"))
#calculate the project.day and week
  data$project.day <- data$julianDate - min(data$julianDate) + 1
  data$week<-(data$project.day-1) %/% 7+1

#####
#need to create a proabable time spent on each milkweed
#order by paste(name.1, name.2, name.3)
#order by date.time
#calculate the diff between given milkweed and following milkweed (if observers equal, if not then 3 min)
#if the difference is more than 15 minutes or less than 0 minutes, make the elapsed time 4 minutes
#this accounts for lunch breaks, different trips, final plant observed by that team (all elapsed times we cannot know)
  data <- data[order(paste(data$name.1, data$name.2, data$name.3), data$date.time) , ]
  rawTimes <- difftime(c(data$date.time[-1], min(data$date.time)), data$date.time, units = "mins")
  data$elapsedTime <- ifelse(rawTimes>0&rawTimes<15, rawTimes, 4)

#order by date, then by milkweed.ID
  data<-data[order(data$date, data$milkweed.ID),]

#cleaning object data
  #any observations 0 percent.green, but ALIVE status. replace with DEAD
    data[ which(data$percent.green==0 & data$milkweed.status=="ALIVE") , "milkweed.status"] <- "DEAD"
  
  #any observations NA percent.green, but ALIVE status. action TBD, but real data here, so leaving intact.
    data[ which(is.na(data$percent.green) & data$milkweed.status=="ALIVE") , ]
  
  #any observation with a non-zero percent green and NA for status, replace with ALIVE
    data[ which(data$percent.green > 0 & is.na(data$milkweed.status)) , "milkweed.status"] <- "ALIVE"

#exclude training data
###############
#DATES TBD
    #data<-data[data$date>"2015-04-26",]
    #trip<-trip[trip$date>"2015-04-26",]
  
#add number of larvae and egg columns
  monarchStages <- cbind(data$monarch1.stage, data$monarch2.stage, data$monarch3.stage, data$monarch4.stage, data$monarch5.stage)
  data$nL1<-apply(monarchStages , 1 , function(x) sum(grepl("L1", x)))
  data$nL2<-apply(monarchStages , 1 , function(x) sum(grepl("L2", x)))
  data$nL3<-apply(monarchStages , 1 , function(x) sum(grepl("L3", x)))
  data$nL4<-apply(monarchStages , 1 , function(x) sum(grepl("L4", x)))
  data$nL5<-apply(monarchStages , 1 , function(x) sum(grepl("L5", x)))
  data$nEggs<-apply(monarchStages , 1 , function(x) sum(grepl("E", x)))

#total larvae on each plant
  data$nLTotal <- rowSums(cbind(data$nL1, data$nL2, data$nL3, data$nL4, data$nL5), na.rm=TRUE)

#add monarch load column
  data$monarchLoad <- data$nLTotal + data$nEggs

#check if sum of larval class counts is equal to total larvae.
  which(data$nLTotal!=rowSums(cbind(data$nL1, data$nL2, data$nL3, data$nL4, data$nL5)))

  monarchLengths <- data[,c("monarch1.length", "monarch2.length", "monarch3.length", "monarch4.length", "monarch5.length")]


  data$L1lengths <- sapply(1:nrow(data), function(x) c(monarchLengths[ x , which(grepl("L1", monarchStages[x , ]))]))
  data$L2lengths <- sapply(1:nrow(data), function(x) c(monarchLengths[ x , which(grepl("L2", monarchStages[x , ]))]))
  data$L3lengths <- sapply(1:nrow(data), function(x) c(monarchLengths[ x , which(grepl("L3", monarchStages[x , ]))]))
  data$L4lengths <- sapply(1:nrow(data), function(x) c(monarchLengths[ x , which(grepl("L4", monarchStages[x , ]))]))
  data$L5lengths <- sapply(1:nrow(data), function(x) c(monarchLengths[ x , which(grepl("L5", monarchStages[x , ]))]))
  
  data$catLengths <- sapply(1:nrow(data), function(x) c(monarchLengths[ x , which(grepl("[L1-5]", monarchStages[x , ]))]))

#function to call all observations from a student and summarize
#add elements within this function to add rows to student summary table
###################################################
#these are empty objects to store: summary statistics, plots, and a lists of suspected missed plants.
#student.summary function outputs to these objects by default
  student.plots <- list()
  compare.plots <- list()
  missedPlants <- list()

    student.summary <- function(student.name){
      student <- list()
      #list trips student.name was on
      student.obs <- sort(which(student.name==data$name.1 | student.name==data$name.2 | student.name==data$name.3))
      
      #get observations student.name was a part of
      dStudent <- data[student.obs,]
      
      #monarchs found
      student$larvaeFound <- sum(dStudent$nLTotal)
      
      #live plants surveyed
      student$livePlants <- sum(dStudent$milkweed.status== "ALIVE")
      
      #total plants surveyed
      student$totalPlants <- nrow(dStudent)
      
      #total time in field and average time spent per milkweed
      student$timeInField <- sum(dStudent$elapsedTime, na.rm = TRUE)
      student$timePerMilkweed <- mean(dStudent$elapsedTime, na.rm = TRUE)
      
      #list plants censused in the most recent trip, determine if that sequence is a complete one
      #print any plants missing from the sequence
      plantsRecentWeek <- sort(dStudent[dStudent$week==max(dStudent$week),"milkweed.ID"])
      completeSeries <- min(plantsRecentWeek):max(plantsRecentWeek)
      missedPlants[[student.name]] <<- c(completeSeries[which(is.na(match(completeSeries , plantsRecentWeek)))])
      
      
      #make some plots, put them into a list to save for student reports
      #student monarch discovery rate compared to average
      monarchProb <- sapply(unique(data$week), 
                            function(x) sum (data[data$week == x , "nLTotal"]) / nrow(data[data$week == x ,]))
      
      studentMonarchProb <- sapply(unique(data$week), 
                                   function(x) sum (dStudent[dStudent$week == x , "nLTotal"]) / nrow(dStudent[dStudent$week == x ,]))
      
      monarchDetection <- data.frame(monarchProb = monarchProb, studentMonarchProb = studentMonarchProb)
      
      p1 <- qplot(data = monarchDetection, y = monarchProb) + geom_line() + ylab("p(monarch larva)") + xlab("week")+ ggtitle("Student comparison to average")
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
      weeklyStudentAvg <- with(dStudent, tapply(elapsedTime, INDEX = week, FUN = mean))
      weeklyTimeAvg <- with(data, tapply(elapsedTime, INDEX = week, FUN = mean))
      
      timeDevotion <- data.frame(weeklyTimeAvg, weeklyStudentAvg)
      
      p3 <- qplot(data = timeDevotion, y = weeklyTimeAvg) + geom_line() + ylab("minutes / plant") + xlab("week")
      p3 <- p3 + geom_point(size = 3,data = timeDevotion, aes(y = weeklyStudentAvg), colour = "red" )
      
      #student number of plants censused per week
      weekCount <- data.frame(week = 1:max(data$week), count = NA, cumMonarch = 0)
      weekCount[match(names(table(dStudent$week)) , weekCount$week) , "count"] <- table(dStudent$week)
      
      p4 <- qplot(data = weekCount, x = weekCount$week , y = as.numeric(weekCount$count)) 
      p4  <- p4 + geom_line() + xlab("week") + ylab("plants per week") + ggtitle("Student data")
      p4 <- p4 + coord_cartesian(xlim = c(0, max(data$week))) + coord_cartesian(ylim = c(0, 1.2*max(weekCount$count, na.rm = TRUE))) 
      
      
      #cumulative monarch eggs and larvae found on plants
      monarchs <- with(dStudent, tapply(dStudent$monarchLoad, dStudent$week, sum))
      weekCount[match(names(monarchs), weekCount$week) , "cumMonarch"] <- monarchs
      weekCount$cumMonarch <- cumsum(weekCount$cumMonarch)
      
      p5 <- qplot(data = weekCount, x = weekCount$week , y = as.numeric(weekCount$cumMonarch)) 
      p5  <- p5 + geom_line() + xlab("week") + ylab("cumulat. monarchs") 
      p5 <- p5 + coord_cartesian(xlim = c(0, max(data$week))) + coord_cartesian(ylim = c(0, 1.2*max(weekCount$cumMonarch, na.rm = TRUE))) 
      
      
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
      compare.plots[[student.name]] <<- list(p1, p2, p3, p6)
      student.plots[[student.name]] <<- list( p4, p5)
      #convert summary list to data.frame and print
      data.frame(student)
      #plot individual student plots (commented out to silence output when all are run together)
      # do.call(grid.arrange, c(compare.plots[[student.name]], student.plots[[student.name]]))
    }
  student.summary(as.character(data$name.1[1]))

#apply the function of all students, and create student reports
#make a list of names, remove NA value
  name.list <- unique(c(as.character(data$name.1), as.character(data$name.2)), as.character(data$name.3))
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

#get the last week interval
  last.week <- sort(seq( min(data$julianDate), min(data$julianDate) + max(data$week)*7, by = 7), decreasing = TRUE)[c(2,1)]
  last.week <- format(as.Date(last.week, origin=as.Date("2015-01-01")) ,  '%d %b')
  last.week <- paste("week", " ", max(data$week), ": ", last.week[1], " - ",last.week[2], sep = "" )

#go to the only (or first) sub-folder present for printing of reports
  setwd(list.dirs(recursive=FALSE)[1])

  for(i in 1:length(name.list)){
    student.name <- name.list[i]
    render(input = "student report.Rmd", output_format = "pdf_document", 
           output_file = file.names.list[i] )
  }

##################################################
#overall summary report
#calculate summary statistics, turn them into a data.frames for a table at top of report
#summary statistics from a given subset of the data
    summary.table <- function(dataToSumm = data){
          #number of observations last week 
          nObs <- nrow(dataToSumm)
          #percent green last week
          avgGreen <- mean(dataToSumm$percent.green, na.rm = TRUE) 
          #larvae per plant last week
          larvaePerPlant <- sum(dataToSumm$nLTotal, na.rm = TRUE) / nObs
          #eggs per plant last week
          eggPerPlant <- sum(dataToSumm$nEggs, na.rm = TRUE) / nObs
          
          summ.df <- data.frame(nObs, avgGreen, larvaePerPlant, eggPerPlant)   
          summ.df
        }


#apply the summary function to whole dataset and last weeks data, rbind for report      
  overall <- summary.table(dataToSumm = data)
  lastWeek <- summary.table(dataToSumm = data[data$week == max(data$week, na.rm = TRUE) , ] )
#this object goes to report
  summStats <- rbind("overall" = overall, "lastWeek" = lastWeek)

#status counts
  weekStatus <-table( data$week, data$milkweed.status )
  weekStatus <- cbind(weekStatus , "total" = rowSums(weekStatus))
  overallStatus <- table(data$milkweedStatus)
  status.table <- tail(weekStatus)
#this object goes to report
  status.table <- rbind(weekStatus, overallStatus)

#overall student rankings
#this object goes to report
  rankTable <- as.data.frame( t( student.df[, -ncol(student.df) ]) )
  rankTable <- rankTable[ order(unlist(rankTable$totalPlants) , decreasing = TRUE), ]
  rankTable <- head(rankTable, 20)
  
#plots
#milkweed count by week
  milkweed.obs.by.week <- aggregate(milkweed.ID~week,length,data=data)
  p1 <- ggplot(milkweed.obs.by.week, aes(x=week, y=milkweed.ID))
  p1 <- p1 + geom_point(col="red")+geom_line()+geom_hline(yintercept=length(unique(data$milkweed.ID)),lty='dashed')+coord_cartesian(ylim = c(0, 400))+scale_x_continuous(breaks=c(1:max(data$week)))+ylab("milkweed count")


#How long did it take to measure each milkweed?
  speed.by.week<-aggregate(elapsedTime~week,data=data,mean) 
  p2<-ggplot(speed.by.week,aes(x=week,y=elapsedTime))
  p2 <- p2+geom_point(col="blue")+geom_line()+coord_cartesian(ylim = c(0, 8))+scale_x_continuous(breaks=c(1:max(data$week)))+ylab("minutes per plant")

#average total stem length per plant
#total stem len and area
  data$stem.count<-as.integer(data$stem.count)
  data$mean.len<-rowMeans(data[,which(colnames(data)=="len.1"):which(colnames(data)=="len.10")],na.rm=T)
  data$mean.dia<-rowMeans(data[,which(colnames(data)=="dia.1"):which(colnames(data)=="dia.10")],na.rm=T)
  data$total.stem.len<-data$stem.count*data$mean.len
  data$total.stem.area<-data$stem.count*data$mean.dia^2*pi

#plant size by week
  total.stem.len.by.week<-aggregate(total.stem.len~week,function(x) c(mean=mean(x),se=std.err(x)),data=data)
  total.stem.area.by.week<-aggregate(total.stem.area~week,function(x) c(mean=mean(x),se=std.err(x)),data=data);total.stem.area.by.week

#plant growth
  p3.limits <- aes(ymax = total.stem.len[,"mean"] + total.stem.len[,"se"], ymin=total.stem.len[,"mean"] - total.stem.len[,"se"])
  p3<-ggplot(total.stem.len.by.week,aes(x=week,y=total.stem.len[,"mean"]))
  p3 <- p3+geom_point(col="forestgreen")+geom_line()+scale_x_continuous(breaks=c(1:max(data$week)))+ylab("total stem per plant (cm)")+geom_errorbar(p3.limits, width=0.2)

  
#catepillar growth
 catLength.by.week<- as.data.frame(with( data, tapply(catLengths , week ,function(x) mean(as.numeric(unlist(x)), na.rm=TRUE))))
 catLength.by.week$week <- 1:nrow(catLength.by.week)
 colnames(catLength.by.week)[1]<-"mean"
  p4 <- ggplot(data=catLength.by.week)
  p4 <- p4+geom_point(aes(x=catLength.by.week[,"week"], y=catLength.by.week[,"mean"]), col="purple")
  p4 <- p4+scale_x_continuous(breaks=c(1:max(data$week)))+ylab("avg catepillar (mm)") + geom_line(aes(x=catLength.by.week[,"week"], y=catLength.by.week[,"mean"]))
 
#plot egg counts by week
  eggs.by.week<-aggregate(nEggs~week,sum,data=data)
  p5 <- ggplot(eggs.by.week, aes(x=week, y=nEggs))
  p5 <- p5+geom_point(col="red")+geom_line()+geom_hline(yintercept=318,lty='dashed')+coord_cartesian(ylim = c(0, 50))+
    scale_x_continuous(breaks=c(1:max(data$week)))+ylab("Eggs per week")

#plot larvae counts by week
  larvae.by.week<-aggregate(nLTotal~week,sum,data=data)
  p6 <- ggplot(larvae.by.week, aes(x=week, y=nLTotal))
  p6 <- p6+geom_point(col="red")+geom_line()+geom_hline(yintercept=318,lty='dashed')+coord_cartesian(ylim = c(0, 50))+
    scale_x_continuous(breaks=c(1:max(data$week)))+ylab("Larvae per week")

#put weekly plots together for overall report
  weekSummPlots <- list(p1,p2,p3,p4,p5, p6)


#plot phenology-ontogeny landscape plotting
    
      cat.length <- as.numeric(tapply(data$catLengths ,  cut(data$date.time, "2 weeks") ,    
                                    function(x) mean(as.numeric(unlist(x)), na.rm=TRUE)))
    
      cat.se <-  as.numeric(tapply(data$catLengths ,  cut(data$date.time, "2 weeks") ,    
                                 function(x) std.err(as.numeric(unlist(x)))))
    
      plant.area <- as.numeric(tapply(data$total.stem.area ,  cut(data$date.time, "2 weeks") ,    
                                    function(x) mean(as.numeric(x), na.rm=TRUE)))
    
      plant.se <-  as.numeric(tapply(data$total.stem.area ,  cut(data$date.time, "2 weeks") ,    
                                   function(x) std.err(as.numeric(x))))
    
      dates <- as.character(format( as.Date(sort(unique(cut(data$date.time, "2 weeks")))) , format = "%m-%d"))
    
      phenOntD <- as.data.frame(cbind(cat.length, cat.se, plant.area, plant.se, dates))
    #this object is plotted directly in overall report script
      p6 <- ggplot(data = phenOntD, aes(x = cat.length, y = plant.area)) + geom_path() + geom_point(col = "forestgreen") +ggtitle("Phenology Ontogeny landscape")
      p6 <- p6+geom_label(label = dates)

#mapping and spatial analysis
    #go up a directory for spatial data
    
      mw.locs$transect<-as.factor(mw.locs$transect)
      mw.locs <-mw.locs[mw.locs$transect==2 , ]
      lowerleftlong=-121.765274
      lowerleftlat= 38.566483
      upperrightlong=-121.747067
      upperrightlat= 38.576428
      
      NDC<-get_map(location=c(lowerleftlong,lowerleftlat,upperrightlong,upperrightlat),maptype="terrain",source="google")
    
    #Assign an error status to any milkweeds missing last week, keep other statuses
      mw.locs$recentStatus <- "ERROR"
      mw.locs$recentStatus[match(unique(data[data$week==max(data$week) ,"milkweed.ID" ]) , mw.locs$name)] <- data[data$week==max(data$week), "milkweed.status"] 
    
      NDCmap<-ggmap(NDC, extent = "panel")
    #this object is plotted directly in overall report script
      p7 <- NDCmap+geom_point(aes(x=longitude,y=latitude,color=recentStatus),data=mw.locs)+ggtitle("Milkweeds surveyed last week")


#time of observations: day of week, time of day
p8 <- qplot(weekdays(data$date.time))
p9 <- qplot(data$date.time$hour)
timePlots <- list(p8, p9)
#print report
  render(input = "overall report.Rmd", output_format = "pdf_document", 
       output_file = paste(format(Sys.time(), "%m-%d-%Y"), " overall report.pdf", sep = "" ))
