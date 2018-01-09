
#Bring in Segregation Values
  Seg<-read.delim("Z:/Projects/SIS/Nate 1880-1940 Project/meltedmeasures.germ.txt", sep="|")
  Seg<-plyr::rename(Seg, c("variable"="Seg_Type"))
  
#Bring in Population Values  
  setwd("C:/Users/mmarti24/Dropbox/Papers/SIS - Early Arriving/")
  Pop <- read.csv("tabs_all_years.csv")

  #City Names from Seg File
    sp<-(as.character(Seg$citystate))
  #City Names from Pop File
    cp<-(as.character(Pop$citystate))
  
  #Is city in population file?
    Seg$insegfile<-sp %in% cp
    #Subset
    Seg<-Seg[which(Seg$insegfile==TRUE),]
    
#Combine Files
  Pop<-Pop[order(Pop$citystate, Pop$year),]
  Pop$combine<-paste(Pop$citystate, Pop$year, sep="")
  Seg$combine<-
  