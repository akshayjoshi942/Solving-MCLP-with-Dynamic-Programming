#Project: Solving MCLP with Dynamic Programming
#Author: Akshay Joshi
#Date: 02/27/2018
#ReadMe available in the code package


#Code Prerequisities
#install.packages("readxl")
rm(list=ls())
#Import Raw Data
library(readxl)
DistanceArray <- read_excel("C:/model/DistanceGrid.xlsx")
CallVolume <- read_excel("C:/model/CallVolume.xlsx")
#Start Computing Runtime
StartTime <- Sys.time()

################################
################################

#Define set up parameters
Stations<-16 #No of Stations
Vehicles<-5 #No of vehicles
CoverageRadius<-4 #Coverage Radius

################################
################################


#Initial Set Up
StationArray<-c(1:Stations)
AllocationArray<-c()
assign(paste("f",Vehicles+1,sep="_"),0)

#Create a array of the distance matrix with 0 and 1 according to coverage radius. 1 if the station j covers demand zone i. 0 otherwise.
  i=1
  while(i<=Stations)
  {
    DistanceArray[,i+1]<-ifelse(DistanceArray[,i+1]<=CoverageRadius,1,0)
    i=i+1
  }

  
#Following Function will be used to compute rewards. 
  ValueAddition<-function(j)
  {
    ValueArray<-rep(0,122)
    
    #The following for loop creates a set of all demand zones which will "add" calls 
    #if the vehicle is allocated to station j. 
    #In short, it creates the set Nj \ Mj (Refer report for definition)
    
    for (k in AllocationArray)
    {
      
      ValueArray<-(ValueArray|DistanceArray[,k+1]) # | i.e. OR means union
      ValueArray[ValueArray==FALSE]<-0
      ValueArray[ValueArray==TRUE]<-1
    }
    
    
    vectorC<<- (DistanceArray[,j+1] - ValueArray)*CallVolume[,2]
    vectorC[vectorC<0]<<-0
    return(sum(vectorC))
  }


#Recursive Loop
#This loop runs from vehicles p to 1. Based on optimality Equations

  for(v in Vehicles:1)
  {
    
    ValueArray2<-rep(0,length(Stations))
    
    #For each vehicle v, compute the maximum reward and return the value.
    for(m in StationArray)
    {
      ValueArray2[m]<-ValueAddition(m)  + get(paste("f",v+1,sep="_"))
      ValueArray2[is.na(ValueArray2)]<-0
    }
    
    
    #Return the maximal calls added and the corresponding station
    assign(paste("f",v,sep="_"),max(ValueArray2),envir = .GlobalEnv)
    assign(paste("AssignedStation",v,sep="_"),which.max(ValueArray2),envir = .GlobalEnv)
    
    #Update {S} and {D}
    StationArray<<-StationArray[-which(StationArray==which.max(ValueArray2))]
    AllocationArray<<-c(AllocationArray,which.max(ValueArray2))
    
  }

#End Runtime
EndTime <- Sys.time() 

#Print Results in friendly form

cat("The maximum calls which can be handled by the system using",Vehicles,"vehicles is",f_1,"i.e a coverage of",round((f_1/1711)*100,2),"%. The vehicles will be located at stations (",paste(AllocationArray, collapse=","),").Runtime of this code is",EndTime-StartTime,"secs.")

