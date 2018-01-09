library(Hmisc)
library(DataCombine)
library(readstata13)
library(gmodels)
library(foreign)
library(car)
library(plyr)
library(seg)
library(spdep)
library(reshape)
library(reshape2)
library(rJava)
library(xlsx)
library(maptools)
library(rgdal)
library(spatialsegregation)


trim <- function( x ) {
  gsub("(^[[:space:]]+|[[:space:]]+$)", "", x)
}

citylist<-read.csv(("Z:/Users/Matt/Segregation Project/City Lists.csv"))
ispresent<-data.frame(City=character())

#Create stable variable names to be used in the for loop.  This is necessary because not all variables have the same naming structure in the files
LongName<-citylist$LongName
Cityname<-citylist$Cityname
Shortname<-citylist$Shortname
Seg_ID<-citylist$Seg_ID
Seg<-citylist$Seg
street<-citylist$st
rows<-nrow(citylist)

AllSegTable<-data.frame(City=character(), Type=character(), Values=numeric())

for(i in 9:9){
}
  tryCatch({
    Segment<-read.dbf(paste("Z:/Users/_Exchange/1880 Stuff/AllCities/",LongName[i],"/",Seg[i], sep=""))
    #Set Working Directory
    library(shapefiles)  
      setwd(paste("Z:/Users/_Exchange/1880 Stuff/AllCities/",LongName[i],"/", sep=""))
      Blockshape<-readOGR(dsn=getwd(), layer="Final_blocks")
      Blockshapepoints<-readShapePoints(paste("Z:/Users/_Exchange/1880 Stuff/AllCities/",LongName[i],"/Block_points.shp", sep=""))
      #Must detach package after this line because package shapefiles affects how read.dbf works
    detach("package:shapefiles", unload=TRUE)
    
    Points<-read.dta13(paste("Z:/Projects/1880Data/1880Microdata/1880MicrodataSouthernCities_LatestAnalysesFiles/",Shortname[i],"_Jan23.dta", sep=""))
    
    names(Segment)<-tolower(names(Segment))
    names(Blockshape)<-tolower(names(Blockshape))
    names(Blockshapepoints)<-tolower(names(Blockshapepoints))
    names(Points)<-tolower(names(Points))

    #Change factor serial to numeric serial to merge
      Blockshapepoints$serial<-as.integer(levels(Blockshapepoints$serial))[Blockshapepoints$serial]
    #Remove X/Y from Points file
      myvars <- names(Points) %in% c("x", "y") 
      Points<-Points[!myvars]
    #Merge Households with Persons
      Block_Points<-merge(Points, Blockshapepoints[,c("serial", "fid_blocks","housenum_n", "x", "y")], 
                          by="serial", all.x=T)
    #Remove households outside of the city boundaries
      Block_Points<-Block_Points[which(Block_Points$fid_blocks!=0),]
    #Create Unique Person Identifier then remove duplicate records
      Block_Points$PersonID<-id(Block_Points[c("serial","pernum")], drop=T)
      Block_Points<-Block_Points[!duplicated(Block_Points$PersonID),]
      Block_Points$Person<-recode(Block_Points$serial,"\" \"=0; else=1")
    #Create Dummy of 18 Plus
        Block_Points$Above18<-recode(Block_Points$age,"c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,NA)=0; else=1")
    #Create Head of Household Variable
        Block_Points$hh<-recode(Block_Points$relate,"c(101)=1; else=0")
    #Unrelated Individuals
        Block_Points$UnRelate<-recode(Block_Points$relate,"c(1200,1202,1203,1204,1205,1206,1221,1222,1223,1230,1239)=1; else=0")
    #Servants
        #Block_Points$Serve<-recode(Block_Points$relate,"1210:1219=1; else=0")
        Block_Points$Serve2<-recode(Block_Points$occ50us, "700:720=1; else=0")
    #Unrelated and Over 18
        Block_Points$UnRelate18<-ifelse(Block_Points$UnRelate==1 & Block_Points$Above18==1,1,0)
    #Whites
        Block_Points$whites<-recode(Block_Points$race,"100=1; else=0")
        Block_Points$WHH<-ifelse(Block_Points$whites==1 & Block_Points$hh==1,1,0)
    #Blacks 
        Block_Points$blacks<-recode(Block_Points$race,"c(200,210)=1; else=0")
        Block_Points$BHH<-ifelse(Block_Points$blacks==1 & Block_Points$hh==1,1,0)
    #Black and Mulatto
        Block_Points$b<-recode(Block_Points$race,"c(200)=1; else=0")
        Block_Points$m<-recode(Block_Points$race,"c(210)=1; else=0")
    #In Laboor Force - 2 is the Code for being in the labor force
        Block_Points$labforce<-recode(Block_Points$labforce,"9=0; else=Block_Points$labforce")
    #Black Servants
        Block_Points$bserve<-ifelse(Block_Points$blacks==1 & Block_Points$Serve2==1,1,0)
    #Black Servents in White Households
        whh<-tapply(Block_Points$WHH, INDEX=list(Block_Points$serial), FUN=sum)
        whhs<-data.frame(serial=names(whh), WhiteHH=whh)
        Block_Points<-merge(x=Block_Points, y=whhs, by="serial", all.x=T)
        Block_Points$bserveWhite<-ifelse(Block_Points$bserve==1 & Block_Points$WhiteHH==1,1,0)
    #####Change Black Servant Code Here ######
    #Drop Black Servants in White HH
        Block_Points<-Block_Points[which(Block_Points$bserveWhite==0),]
    #Drop Black Children in White HH after Dropping Black Servants in White HH
        Block_Points$bchild<-ifelse(Block_Points$blacks==1 & Block_Points$Above18==0, 1, 0)
        Block_Points$bchildWHH<-ifelse(Block_Points$bchild==1 & Block_Points$WhiteHH==1, 1, 0)
        Block_Points<-Block_Points[which(Block_Points$bchildWHH==0),]
        
    ######Merge Points to Building Level using Segment file and stnum_id (Building Level ID)######
      #Make Changes to Segment
      Segment$housenum_n<-trim(gsub("[?]","", Segment$housenum_n, perl=T))
      Segment$housenum_n<-as.numeric(Segment$housenum_n)
      ifelse(exists("newaddress", where=Segment), (Segment$newaddress=Segment$newaddress),
             (Segment$newaddress=Segment$street_new))
      Segment$newaddress<-ifelse(is.na(Segment$newaddress), as.character(Segment$street), as.character(Segment$newaddress))
      Segment<-Segment[complete.cases(Segment$side,Segment$housenum_n),]
      Segment<-Segment[complete.cases(Segment[Seg_ID[i]]),]
      Segment<-subset(Segment, (eval(parse(text=paste("Segment$",Seg_ID[i],sep="")))!=0))
      
      #Order by Address and House Number
        Segment<-Segment[order(Segment$newaddress,Segment$housenum_n),]
      #Tag Unique Streets and Housenumbers
        Segment$stnum_id<-id(Segment[c("newaddress","housenum_n")], drop=T)  
        Segment$serial<-as.numeric(levels(Segment$serial))[Segment$serial]
      
      #Merge
        Building<-merge(Block_Points,Segment[,c("serial","newaddress","stnum_id")], by="serial", all=T)
        Building<-Building[which(!is.na(Building$stnum_id)),]

      #####Aggregate to Building
        blbuild<-tapply(Building$blacks, INDEX=list(Building$stnum_id), FUN=sum)
        blkbuild<-data.frame(stnum_id=names(blbuild), blkbuild=blbuild)
        wbuild<-tapply(Building$whites, INDEX=list(Building$stnum_id), FUN=sum)
        whbuild<-data.frame(stnum_id=names(wbuild), whbuild=wbuild)
        tbuild<-tapply(Building$Person, INDEX=list(Building$stnum_id), FUN=sum)
        totbuild<-data.frame(stnum_id=names(tbuild), totbuild=tbuild)
      
        buildData<-merge(x=blkbuild, y=whbuild, by="stnum_id", all.x=T)
        buildData<-merge(x=totbuild, y=buildData, by="stnum_id", all.x=T)
        buildData$stnum_id<-as.integer(levels(buildData$stnum_id))[buildData$stnum_id]
      
      #Remove Missings - These are buildings that match to a serial number, but are outside of the block boundary file
        buildData<-buildData[!is.na(buildData$totbuild),]
       
      #Keep only one unique building in the dataset to merge with aggregate data
        Building<-Building[!duplicated(Building$stnum_id),]
      #Merge these data with buildData
        Building<-merge(buildData, Building, by="stnum_id", all.x=T)
      #Grab Coordinates from Building file
        coordinates(Building)<-c('x', 'y')
        Building_Points<-SpatialPointsDataFrame(Building, data.frame(Building))
      #Transform shapefile to fit area UTM
        proj4string(Building_Points)<-CRS("+proj=longlat +datum=NAD83")
        Building_Points<-spTransform(Building_Points, CRS("+proj=utm +zone=16 ellps=NAD83"))
      #Keep only black and white from buildData
        myvars<-c("blkbuild", "whbuild")
        buildData<-buildData[myvars]
      
      #Spatial Segregation Using Building Points#
      #  Seg1B<-spseg(Building_Points, data=buildData ,method="all", smoothing="none", power=0, maxdist=150)
      #  print(Seg1B)
      #  Seg2B<-spseg(Building_Points, data=buildData ,method="all",  smoothing="none", power=0, maxdist=100)
      #  print(Seg2B)
        
    #Create New Shapefile for Person 
      #Create a Coordinates File
      myvars<-c("x","y")
      coordinates(Block_Points)<-c('x','y')
      Person_Points<-SpatialPointsDataFrame(Block_Points, data.frame(Block_Points))
      #Transform shapefile to fit area UTM
      proj4string(Person_Points)<-CRS("+proj=longlat +datum=NAD83")
      Person_Points<-spTransform(Person_Points, CRS("+proj=utm +zone=16 ellps=NAD83"))

    #####Spatial Segregation Measures####
      #Create a Black/White Files
      myvars<-c("blacks", "whites")
      BW<-Person_Points@data
      BW<-BW[myvars] 
      #Use smoothing = "kernel" or "equal" to change how distances are handled
      #Plots to assist in determining Power and maxdist
        #d <- seq(0, 100, by = 0.5)
        #plot(d, exp(-d * 0.01), type = "l", main = "Power = 0.01")
        #plot(d, exp(-d * 0.1), type = "l", main = "Power = 0.1")
        #plot(d, exp(-d * 0.5), type = "l", main = "Power = 0.5")
        #plot(d, exp(-d * 3), type = "l", main = "Power = 3")
        #plot(d, exp(-d * 10), type = "l", main = "Power = 10")
        #plot(d, exp(-d * 0), type = "l", main = "Power = 100")
      
      #Spatial<-spseg(Coord, data=BW, method="all", negative.rm=FALSE, verbose=T)
      Seg5<-spseg(Person_Points, data=BW ,method="all", smoothing="none", power=0, maxdist=5)
      print(Seg5, digits=3)
      tab<-Seg5@p
      df<-as.data.frame(tab[1,1])
      df$Type<-"BlackIso-MaxDist=5m"
      
      Seg10<-spseg(Person_Points, data=BW ,method="all",  smoothing="none", power=0, maxdist=10)
      print(Seg10, digits=3)
      tab<-Seg10@p
      df1<-as.data.frame(tab[1,1])
      df1$Type<-"BlackIso-MaxDist=10m"
      
      Seg25<-spseg(Person_Points, data=BW ,method="all", smoothing="none", power=0, maxdist=25)
      print(Seg25, digits=3)
      tab<-Seg25@p
      df2<-as.data.frame(tab[1,1])
      df2$Type<-"BlackIso-MaxDist=25m"
      
      Seg50<-spseg(Person_Points, data=BW ,method="all",  smoothing="none", power=0, maxdist=50)
      print(Seg50, digits=3)
      tab<-Seg50@p
      df3<-as.data.frame(tab[1,1])
      df3$Type<-"BlackIso-MaxDist=50m"
      
      Seg100<-spseg(Person_Points, data=BW ,method="all",  smoothing="none", power=0, maxdist=100)
      print(Seg100, digits=3)
      tab<-Seg100@p
      df4<-as.data.frame(tab[1,1])
      df4$Type<-"BlackIso-MaxDist=100m"
      
      Seg150<-spseg(Person_Points, data=BW ,method="all",  smoothing="none", power=0, maxdist=150)
      print(Seg150, digits=3)
      tab<-Seg150@p
      df5<-as.data.frame(tab[1,1])
      df5$Type<-"BlackIso-MaxDist=150m"
      
      Seg225<-spseg(Person_Points, data=BW ,method="all",  smoothing="none", power=0, maxdist=225)
      print(Seg225, digits=3)
      tab<-Seg225@p
      df6<-as.data.frame(tab[1,1])
      df6$Type<-"BlackIso-MaxDist=225m"
      
      Seg500<-spseg(Person_Points, data=BW ,method="all",  smoothing="none", power=0, maxdist=500)
      print(Seg500, digits=3)
      tab<-Seg500@p
      df7<-as.data.frame(tab[1,1])
      df7$Type<-"BlackIso-MaxDist=500m"
      
      PointSeg<-rbind(df, df1, df2, df3, df4, df5, df6, df7)
      PointSeg<-rename(PointSeg,c("tab[1, 1]"="Values"))
      PointSeg$city<-Cityname[i]
      
      AllSegTable<-rbind(AllSegTable, PointSeg)
      ispresent<-rbind(ispresent,tab)
  }, error=function(e){cat(conditionMessage(e))})
}


    #####Aggregate to Blocks
      blblock<-tapply(Block_Points$blacks, INDEX=list(Block_Points$fid_blocks), FUN=sum)
      blkblock<-data.frame(fid_blocks=names(blblock), blkblock=blblock)
      wblock<-tapply(Block_Points$whites, INDEX=list(Block_Points$fid_blocks), FUN=sum)
      whblock<-data.frame(fid_blocks=names(wblock), whblock=wblock)
      tblock<-tapply(Block_Points$Person, INDEX=list(Block_Points$fid_blocks), FUN=sum)
      totblock<-data.frame(fid_blocks=names(tblock), totblock=tblock)
      
      BlockData<-merge(x=blkblock, y=whblock, by="fid_blocks", all.x=T)
      BlockData<-merge(x=totblock, y=BlockData, by="fid_blocks", all.x=T)
      BlockData$fid_blocks<-as.integer(levels(BlockData$fid_blocks))[BlockData$fid_blocks]
      
    #####Use block data#####
      myvars<-c("blkblock","whblock")
      BWBlock<-BlockData[myvars]
      #Delete blocks from shapefile that do not have any households in them to make the data and shapefile
      #have the same number of rows.
        obs<-BlockData[order(BlockData$fid_blocks),]
        myvars<-c("fid_blocks")
        obs<-obs[myvars]
        #For Blockshape
        Blockshape2<-Blockshape[(Blockshape$fid_blocks %in% obs$fid_blocks),]

    #No Smoothing
    BlockSeg5<-spseg(Blockshape2, data=BWBlock, smoothing="kernel",nrow=100, ncol=100, power=0, maxdist=5)
    print(BlockSeg5, digits=3)
    BlockSeg10<-spseg(Blockshape2, data=BWBlock, smoothing="kernel",nrow=100, ncol=100, power=0, maxdist=10)
    print(BlockSeg10, digits=3)
    BlockSeg25<-spseg(Blockshape2, data=BWBlock, smoothing="kernel",nrow=100, ncol=100, power=0, maxdist=25)
    print(BlockSeg25, digits=3)
    BlockSeg50<-spseg(Blockshape2, data=BWBlock, smoothing="kernel",nrow=100, ncol=100, power=0, maxdist=50)
    print(BlockSeg50, digits=3)
    BlockSeg150<-spseg(Blockshape2, data=BWBlock, smoothing="kernel",nrow=100, ncol=100, power=0, maxdist=150)
    print(BlockSeg150, digits=3)
    BlockSeg225<-spseg(Blockshape2, data=BWBlock, smoothing="kernel",nrow=100, ncol=100, power=0, maxdist=225)
    print(BlockSeg225, digits=3)
    BlockSeg500<-spseg(Blockshape2, data=BWBlock, smoothing="kernel",nrow=100, ncol=100, power=0, maxdist=500)
    print(BlockSeg500, digits=3)
    
    #Kernel Smoothing
    BlockSegSmooth<-spseg(Blockshape2, data=BWBlock, smoothing="kernel",nrow=20, ncol=20)
    BlockSegSmooth<-spseg(Blockshape2, data=BWBlock, smoothing="equal",nrow=20, ncol=20)
    
    spplot(BlockSegSmooth)
    print(BlockSegSmooth, digits=3)
    
    testSmooth<-BlockSegSmooth@env
    testSmooth2<-BlockSegSmooth@data
    {   
 }

keep<-AllSegTable
  write.csv(keep, file="Z:/Users/Matt/Segregation Project/keep_temp.csv",row.names=F)

    
    
#Data for DC
  dat<-rename(BlockData, c(fid_blocks="id",blkblock="blk", whblock="wh", totblock="tot"))
  Blockshape2<-rename(Blockshape2, c(fid_blocks="id"))
  myvars<-c("id")
  Blockshape2<-Blockshape2[myvars]
  #Make columns numeric so that they can be written as dbf
  dat$tot<-as.numeric(dat$tot)
  dat$blk<-as.numeric(dat$blk)
  dat$wh<-as.numeric(dat$wh)
  Blockshape3<-merge(Blockshape2, dat, by="id", all.x=T)
  
  setwd("Z:/Users/Matt/Map Requests/")
  writeOGR(Blockshape3, dsn=getwd(),layer="DC", driver="ESRI Shapefile")

  