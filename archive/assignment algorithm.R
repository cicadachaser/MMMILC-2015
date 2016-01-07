#This is an R script which assigns weekly milkweed plants MMMILC Project participants. 
#The algorithm randomly pairs N students into floor(N/2) pairs, forming a triplet if N is odd.
#Each pair is randomly assigned to a differnt segment of the MMMILC transect each week.


#clear all variables
rm(list=ls())
graphics.off()
pardefault <- par(no.readonly = T)

setwd("C:\\Users\\lhyang\\Dropbox\\Milkweeds and Monarchs\\The MMMILC Project") #laptop
#setwd("C:\\Users\\lhyang.ent-yang01\\Documents\\Dropbox\\Milkweeds and monarchs\\The MMMILC Project") #desktop

roster<-read.csv("roster2015GG.csv") #roster of student names

weeks.total<-12 #10 weeks from April 1 to the end of the school year
#plants.total<-318 
plants.start<-199 #For 2015, we will focus on transect 2 only
plants.end<-516
students<-roster$name 

weeks<-seq(as.Date("2015/6/22"), by="weeks",length.out=weeks.total) #vector of weekly starts

#The number and length of the segments is determined by the number of students
#all the +1's and -1's are to ensure that we don't end up with overlapping assignments
segments.total<-floor(length(students)/2) 
intervals<-as.integer(seq(plants.start-1,plants.end,length.out=segments.total+1))
starts<-intervals[1:segments.total]+1
ends<-intervals[2:(segments.total+1)]
segment.intervals<-paste(starts,ends,sep=" to ")
segment.ID<-seq(1:segments.total)
segments<-cbind.data.frame(segment.intervals,segment.ID)
assignments<-vector("list", weeks.total)

for(week in 1:weeks.total){

  if(length(students)%%2==0){
    evenset<-sample(students,length(students))
    pairs<-matrix(evenset,length(evenset)/2,ncol=2)
    pairs<-cbind(pairs,matrix(nrow=length(students)/2,ncol=1))
  } else { 
    evenset<-sample(students,length(students)-1)
    pairs<-matrix(evenset,length(evenset)/2,ncol=2)
    oddman<-students[students %in% evenset==F]
    pairs<-cbind(pairs,matrix(nrow=length(evenset)/2,ncol=1))  
    pairs[length(evenset)/2,3]<-as.character(oddman)
  }
  pairs<-cbind(pairs,segments[order(sample(1:segments.total)),],rep(week,segments.total),rep(weeks[week],segments.total))
  pairs<-as.data.frame(pairs)
  colnames(pairs)<-c("student.1","student.2", "student.3", "segment","segment.ID","week","week.start")
  pairs<-pairs[order(pairs$segment.ID),]
  assignments[[week]]<-pairs
  #writes a single CSV file with all weekly assignments
  #This code is set to append - TO REPLACE, THE PREVIOUS FILE MUST BE MANUALLY DELETED
  suppressWarnings(write.table(pairs,file="assignmentsGG.csv",sep=",",append=T,row.names=F,col.names=T))
}

assignments
