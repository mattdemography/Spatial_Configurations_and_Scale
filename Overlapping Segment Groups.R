

citylist<-read.csv(("C:/Users/mmarti24/Documents/Segregation Project/City Lists.csv"))
citylist <- data.frame(lapply(citylist, as.character), stringsAsFactors=FALSE)
LongName<-citylist$LongName
Cityname<-citylist$Cityname
Shortname<-citylist$Shortname
Ex<-citylist$Ex
SG<-citylist$SG
Seg<-citylist$Seg
hex150<-citylist$hex150
hex225<-citylist$hex225
Seg_ID<-citylist$Seg_ID
street<-citylist$st

#***ADD****#
segid_1<-citylist$Seg_ID_1
seggrid<-citylist$SegGrID
seggrid_1<-citylist$SegGrID_1

for(i in 1:1){
}

Segment<-read.dbf(paste("Z:/Users/_Exchange/1880 Stuff/AllCities/",LongName[i],"/",Seg[i], sep=""))
SegGroup<-read.dbf(paste("Z:/Users/_Exchange/1880 Stuff/AllCities/",LongName[i],"/",Cityname[i],SG[i], sep=""))
Extended<-read.dbf(paste("Z:/Users/_Exchange/1880 Stuff/AllCities/",LongName[i],"/",Cityname[i],Ex[i], sep=""))
Points<-read.dta13(paste("Z:/Projects/1880Data/1880Microdata/1880MicrodataSouthernCities_LatestAnalysesFiles/",Shortname[i],"_Jan23.dta", sep=""))

#This shapefile has a list of segment ID's and their corresponding segments in the group.
  #Seg_ID_1 is the group id which gets dissolved.
  #Seg_ID is the segment that is part of each group.  So if there is seg_id=10, that means that segment id 10 is in
    #all of the segment groups listed in Seg_ID_1
  OverSeg_Join<-read.dbf(paste("Z:/Users/_Exchange/1880 Stuff/AllCities/",LongName[i],"/OverSeg_Join.dbf", sep=""))
  OverSeg_Join$SegGrID<-eval(parse(text=(paste("OverSeg_Join$",segid_1[i],sep=""))))
  OverSeg_Join$SegmentID<-eval(parse(text=(paste("OverSeg_Join$",Seg_ID[i],sep=""))))
  myvars<-c("SegmentID", "SegGrID")
  OverSeg_Join<-OverSeg_Join[myvars]
  OverSeg_Join$City<-Cityname[i]
  
#This shapefile is the dissolved City_OverSeg_Join
  OverSeg_Dissolve<-read.dbf(paste("Z:/Users/_Exchange/1880 Stuff/AllCities/", LongName[i],"/OverSeg_Dissolve.dbf", sep=""))
  
#This shapefile has a list of all segment group ID's and their corresponding segment groups in the extended segment group
  #SegGrID is the segment group id.  This value is repeated indicating the unique extended segment groups
    #that this segment group is part of.
  #SegGrID_1 is the focal segment group.
  ExtSeg_Join<-read.dbf(paste("Z:/Users/_Exchange/1880 Stuff/AllCities/",LongName[i],"/ExtSeg_Join2.dbf", sep=""))
  names(ExtSeg_Join)<-tolower(names(ExtSeg_Join))
  ExtSeg_Join$ExtSegGrID<-eval(parse(text=(paste("ExtSeg_Join$",seggrid_1[i],sep=""))))
  ExtSeg_Join$SegGrID<-eval(parse(text=(paste("ExtSeg_Join$",seggrid[i],sep=""))))
  myvars<-c("ExtSegGrID","SegGrID")
  ExtSeg_Join<-ExtSeg_Join[myvars]
  ExtSeg_Join$City<-Cityname[i]
  
  Overlap<-merge(OverSeg_Join[,c("SegmentID", "SegGrID")], ExtSeg_Join, by="SegGrID", all=T)
  Overlap<-Overlap[order(Overlap$SegmentID),]
  Overlap2<-aggregate(Overlap$SegmentID, by=Overlap[c('SegmentID','ExtSegGrID')],length)
  Overlap2<-Overlap2[order(Overlap2$SegmentID),]
  myvars<-c("ExtSegGrID", "SegmentID")
  Overlap2<-Overlap2[myvars]
  Overlap2$City<-Cityname[i]
    
#This is the dissolved ExtSeg_Join2
  ExtSeg_Dissolve<-read.dbf(paste("Z:/Users/_Exchange/1880 Stuff/AllCities/",LongName[i],"/ExtSeg_Dissolve.dbf", sep=""))

  #Create spreadsheet for each city with list of segments making up segment groups - segment groups making up extended segment groups and
  #segments within extended segment groups
    write.xlsx2(OverSeg_Join, file = paste("C:/Users/mmarti24/Documents/Segregation Project/",Cityname[i],"List.xlsx",sep=""), 
                sheetName = "SegmentGroups", row.names = FALSE, append=T)
    write.xlsx2(ExtSeg_Join, file = paste("C:/Users/mmarti24/Documents/Segregation Project/",Cityname[i],"List.xlsx",sep=""), 
                sheetName = "ExtendedSegmentGroups", row.names = FALSE, append=T)
    write.xlsx2(Overlap2, file = paste("C:/Users/mmarti24/Documents/Segregation Project/",Cityname[i],"List.xlsx",sep=""), 
                sheetName = "ExtSegmentGroups-Segments", row.names = FALSE, append=T)
    write.xlsx2(Overlap, file = paste("C:/Users/mmarti24/Documents/Segregation Project/",Cityname[i],"List.xlsx",sep=""), 
                sheetName = "AllSegments", row.names = FALSE, append=T)
  