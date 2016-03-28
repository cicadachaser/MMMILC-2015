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
  library(ggmap)
  

rm(list=ls())
graphics.off()
pardefault <- par(no.readonly = T)
 

#se function that removes NA's
  std.err <- function(x) sd(x[!is.na(x)])/sqrt(length(x[!is.na(x)]))

#set wd and load data
  #setwd("/Users/mmcmunn/Desktop/GitHub/MMMILC-2015/") #marshall laptop
  #setwd("/Users/mmcmunn/Desktop/GitHub/MMMILC-2015/")
  #setwd("C:\\Users\\louie\\Documents\\GitHub\\MMMILC-2015") #LHY SP4
  setwd("C:\\Users\\louie\\Documents\\GitHub\\MMMILC-2015") #LHY desktop

  trip<-read.csv("trip 2016-02-09.csv",header=T,strip.white=T,na.strings= c(" ", "")) #trip log
  data<-read.csv("data 2016-02-09.csv",header=T,strip.white=T,na.strings= c(" ", "")) #observations


#order by date, then by milkweed.ID
  data<-data[order(data$date, data$milkweed.ID),]
#milkweed status to character  
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
  #these are empty objects to store: summary statistics, plots, and a lists of suspected missed plants.
  #student.summary function outputs to these objects by default
    student.plots <- list()
    compare.plots <- list()
    missedPlants <- list()

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
      student$larvaeFound <- sum(dStudent$nLTotal)
    
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

#apply the function of all students, and create student reports
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
        
        #setwd("C:\\Users\\louie\\Documents\\GitHub\\MMMILC-2015\\student reports")
        #get the last week interval
        last.week <- sort(seq( min(data$julian.date), min(data$julian.date) + max(data$week)*7, by = 7), decreasing = TRUE)[c(2,1)]
        last.week <- format(as.Date(last.week, origin=as.Date("2015-01-01")) ,  '%d %b')
        last.week <- paste("week", " ", max(data$week), ": ", last.week[1], " - ",last.week[2], sep = "" )
        
        setwd("C:\\Users\\louie\\Documents\\GitHub\\MMMILC-2015\\student reports")
            for(i in 1:length(name.list)){
                    student.name <- name.list[i]
                    render(input = "studentreport.Rmd", output_format = "html_document", 
                          output_file = file.names.list[i] )
                    }
        #switch back to main working dir
        setwd("C:\\Users\\louie\\Documents\\GitHub\\MMMILC-2015")

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
    weekStatus <-table(data[data$week == max(data$week, na.rm = TRUE) , "milkweed.status"])
    overallStatus <- table(data$milkweed.status)
    #this object goes to report
    status.table <- rbind(weekStatus, overallStatus)
    
  #overall student rankings
    rankTable <- sort(table(c(as.character(data$name.1), as.character(data$name.2), as.character(data$name.3))), decreasing = TRUE)
    #this object goes to report
    rankTable <- data.frame("Milkweeds observed" = rankTable[1:10])
    
  #plots
    #milkweed count by week
    milkweed.obs.by.week<-aggregate(obs.ID~week,length,data=data)
    p1 <- ggplot(milkweed.obs.by.week, aes(x=week, y=obs.ID))
    p1 <- p1 + geom_point(col="red")+geom_line()+geom_hline(yintercept=318,lty='dashed')+coord_cartesian(ylim = c(0, 400))+scale_x_continuous(breaks=c(1:max(trip$week)))+ylab("milkweed count")
    
    
    #How long did it take to measure each milkweed?
      speed.by.week<-aggregate(cbind(duration.min,milkweed.count)~week,data=trip,sum); 
      speed.by.week$minutes.per.milkweed<-with(speed.by.week,duration.min/milkweed.count);speed.by.week
      p2<-ggplot(speed.by.week,aes(x=week,y=minutes.per.milkweed))
      p2 <- p2+geom_point(col="blue")+geom_line()+coord_cartesian(ylim = c(0, 6.5))+scale_x_continuous(breaks=c(1:max(trip$week)))+ylab("minutes per plant")
      
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
          p3 <- p3+geom_point(col="forestgreen")+geom_line()+scale_x_continuous(breaks=c(1:max(trip$week)))+ylab("total stem per plant (cm)")+geom_errorbar(p3.limits, width=0.2)
    
    
          #plot egg counts by week and cumulative by day
          eggs.by.week<-aggregate(nEggs~week,sum,data=data)
          p4 <- ggplot(eggs.by.week, aes(x=week, y=nEggs))
          p4 <- p4+geom_point(col="red")+geom_line()+geom_hline(yintercept=318,lty='dashed')+coord_cartesian(ylim = c(0, 50))+
            scale_x_continuous(breaks=c(1:max(trip$week)))+ylab("Eggs per week")
          
          #plot larvae counts by week and cumulative by day
          larvae.by.week<-aggregate(nLTotal~week,sum,data=data)
          p5 <- ggplot(larvae.by.week, aes(x=week, y=nLTotal))
          p5 <- p5+geom_point(col="red")+geom_line()+geom_hline(yintercept=318,lty='dashed')+coord_cartesian(ylim = c(0, 50))+
            scale_x_continuous(breaks=c(1:max(trip$week)))+ylab("Larvae per week")
    
    #put weekly plots together for overall report
    weekSummPlots <- list(p1,p2,p3,p4,p5)
    
    
    #plot phenology-ontogeny landscape plotting

          cat.length <- as.numeric(tapply(data$catLengths ,  cut(data$date.x, "2 weeks") ,    
                               function(x) mean(as.numeric(unlist(x)), na.rm=TRUE)))

          cat.se <-  as.numeric(tapply(data$catLengths ,  cut(data$date.x, "2 weeks") ,    
                            function(x) std.err(as.numeric(unlist(x)))))
          
          plant.area <- as.numeric(tapply(data$total.stem.area ,  cut(data$date.x, "2 weeks") ,    
                               function(x) mean(as.numeric(x), na.rm=TRUE)))
          
          plant.se <-  as.numeric(tapply(data$total.stem.area ,  cut(data$date.x, "2 weeks") ,    
                              function(x) std.err(as.numeric(x))))
          
          phenOntD <- as.data.frame(cbind(cat.length, cat.se, plant.area, plant.se))
          #this object is plotted directly in overall report script
          p6 <- ggplot(data = phenOntD, aes(x = cat.length, y = plant.area)) + geom_path() + geom_point(col = "forestgreen") +ggtitle("Phenology Ontogeny landscape")
          p6 <- p6+geom_label(label = rownames(phenOntD))

      #mapping and spatial analysis
      
          mw.locs<-read.csv("milkweed coordinates 2015-05-23.csv")
          mw.locs$transect<-as.factor(mw.locs$transect)
          
          lowerleftlong=-121.765274
          lowerleftlat= 38.566483
          upperrightlong=-121.747067
          upperrightlat= 38.576428
          
          NDC<-get_map(location=c(lowerleftlong,lowerleftlat,upperrightlong,upperrightlat),maptype="terrain",source="google")
          
          NDCmap<-ggmap(NDC, extent = "panel")
          #this object is plotted directly in overall report script
          p7 <- NDCmap+geom_point(aes(x=longitude,y=latitude,color=transect),data=mw.locs)+ggtitle("Milkweed locations")
          

  #print report
      setwd("C:\\Users\\louie\\Documents\\GitHub\\MMMILC-2015\\student reports")  
      render(input = "overall report.Rmd", output_format = "html_document", 
           output_file = paste(format(Sys.time(), "%m-%d-%Y"), " overall report.pdf", sep = "" ))
      setwd("C:\\Users\\louie\\Documents\\GitHub\\MMMILC-2015")  
          
#unused plot
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