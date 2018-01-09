#Bring In Libraries
#Library List
library(foreign)
library(car)
library(readstata13)
library(plyr)
library(seg)
library(reshape)
library(reshape2)
library(rJava)
library(xlsx)
library(maptools)
library(rgdal)
library(haven)

trim <- function( x ) {
  gsub("(^[[:space:]]+|[[:space:]]+$)", "", x)
}

#Bring in all data to build one file with all cities
citylist<-read.csv(("Z:/Projects/Preparing 1880 Files/City Lists.csv"))
citylist <- data.frame(lapply(citylist, as.character), stringsAsFactors=FALSE)
Cityname<-citylist$Cityname

a<-read.csv("Z:/Users/Matt/Segregation Project_Irish and Germans/Household Data/Albany_HH.csv")

for (i in 2:40){
  d<-read.csv(paste("Z:/Users/Matt/Segregation Project_Irish and Germans/Household Data/", Cityname[i],"_HH.csv", sep=""))
  a<-rbind(a, d)
}

#Write Out Large File
  write.csv(a, "Z:/Users/Matt/Segregation Project_Irish and Germans/Combined Households_AllCities.csv")
  names(a)
  
  
  