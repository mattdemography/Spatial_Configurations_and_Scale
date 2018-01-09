library(Hmisc)
library(DataCombine)
library(readstata13)
library(gmodels)
library(foreign)
library(car)
library(plyr)
library(seg)
library(reshape)
library(reshape2)
options(java.parameters = "-Xmx80000m")
library(rJava)
library(xlsx)
trim <- function( x ) {
  gsub("(^[[:space:]]+|[[:space:]]+$)", "", x)
}

shift<-function(x,shift_by){
  stopifnot(is.numeric(shift_by))
  stopifnot(is.numeric(x))
  
  if (length(shift_by)>1)
    return(sapply(shift_by,shift, x=x))
  
  out<-NULL
  abs_shift_by=abs(shift_by)
  if (shift_by > 0 )
    out<-c(tail(x,-abs_shift_by),rep(0,abs_shift_by))
  else if (shift_by < 0 )
    out<-c(rep(0,abs_shift_by), head(x,-abs_shift_by))
  else
    out<-x
  out
}

jgc <- function()
{
  .jcall("java/lang/System", method = "gc")
}    

citylist<-read.csv(("Z:/Users/Matt/Segregation Project/City Lists.csv"))
citylist <- data.frame(lapply(citylist, as.character), stringsAsFactors=FALSE)

ispresent<-data.frame(City=character())
#Create stable variable names to be used in the for loop.  This is necessary because not all variables have the same naming structure in the files
  LongName<-citylist$LongName
  Cityname<-citylist$Cityname
  Shortname<-citylist$Shortname
  Ex<-citylist$Ex
  SG<-citylist$SG
  Seg<-citylist$Seg
  hex150<-citylist$hex150
  hex225<-citylist$hex225
  Seg_ID<-citylist$Seg_ID
  seg_id1<-citylist$Seg_ID_1
  seggrid<-citylist$SegGrID
  seggrid_1<-citylist$SegGrID_1
  street<-citylist$st
  rows<-nrow(citylist)

AllSegTable<-data.frame(City=character(),DissHouse=numeric(), IsoHouse=numeric(), DissBLD=numeric(), IsoBLD=numeric(), IsoBLDGroup2=numeric(), 
                        DissSeg=numeric(), IsoSeg=numeric(), DissSegside=numeric(),IsoSegside=numeric(),DissSG=numeric(),
                        IsoOverSegG=numeric(), NumSG=numeric(), DissEX=numeric(), IsoOverEG=numeric(), NumExtG=numeric(),
                        DissED=numeric(), IsoED=numeric(), IsoCity=numeric(), PerBlack=numeric())
for(i in 1:1){
}
  tryCatch({
    Segment<-read.dbf(paste("Z:/Users/_Exchange/1880 Stuff/AllCities/",LongName[i],"/",Seg[i], sep=""))
    SegGroup<-read.dbf(paste("Z:/Users/_Exchange/1880 Stuff/AllCities/",LongName[i],"/",LongName[i],SG[i], sep=""))
    Extended<-read.dbf(paste("Z:/Users/_Exchange/1880 Stuff/AllCities/",LongName[i],"/",LongName[i],Ex[i], sep=""))
    Points<-read.dta13(paste("Z:/Projects/1880Data/1880Microdata/1880MicrodataSouthernCities_LatestAnalysesFiles/",Shortname[i],"_Jan23.dta", sep=""))
    
    #This shapefile has a list of segment ID's and their corresponding segments in the group.
    #Seg_ID_1 is the group id which gets dissolved.
    #Seg_ID is the segment that is part of each group.  So if there is seg_id=10, that means that segment id 10 is in
    #all of the segment groups listed in Seg_ID_1
      OverSeg_Join<-read.dbf(paste("Z:/Users/_Exchange/1880 Stuff/AllCities/",LongName[i],"/OverSeg_Join.dbf", sep=""))
      names(OverSeg_Join)<-tolower(names(OverSeg_Join))
      OverSeg_Join$SegGrID<-eval(parse(text=(paste("OverSeg_Join$",seg_id1[i],sep=""))))
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
    
      names(Segment)<-tolower(names(Segment))
      names(SegGroup)<-tolower(names(SegGroup))
      names(Extended)<-tolower(names(Extended))
      names(Points)<-tolower(names(Points))
      
      #Make changes to Yi's Files
      ifelse(exists(paste(Seg_ID[i]), where=Extended), 
             (assign("Extended$",Seg_ID[i])),
             (Extended$seg_id=Extended$join_fid))
      
      ifelse(exists(paste(Seg_ID[i]), where=SegGroup), 
             (assign("SegGroup$",Seg_ID[i])),
             (SegGroup$seg_id=SegGroup$join_fid))   
      
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
      #Tag Unique Streets and Housenumbers - I.E. BUILDINGS
        Segment$stnum_id<-id(Segment[c("newaddress","housenum_n")], drop=T)
      #Tag Unique Sides and Segments
        Segment$segside<-id(Segment[c(Seg_ID[i],"side")], drop=T)
      
    merge1<-merge(x=SegGroup[,c(hex150[i],"serial")],y=Extended[,c(hex225[i],Seg_ID[i],"serial")], by="serial", all.x=TRUE)
    merge2<-merge(x=merge1,y=Segment[,c("serial","housenum_n","newaddress","stnum_id", "side","type", "segside")], 
                  by="serial", all.x=TRUE)
    merge2<-subset(merge2, !duplicated(merge2$serial))
    merge2$serial<-as.numeric(levels(merge2$serial))[merge2$serial]
    merge2<-merge2[complete.cases(merge2$side,merge2$housenum_n),]
    
    #Create Merge3
      merge3<-merge(x=Points[,c("incplcus", "race","relate","age","occ50us","labforce","serial","enumdist")],y=merge2, by="serial", all.x=TRUE)
      merge3<-merge3[complete.cases(merge3[Seg_ID[i]]),]
      merge3$Person<-recode(merge3$serial,"\" \"=0; else=1")
      merge3<-subset(merge3, (eval(parse(text=paste("merge3$",Seg_ID[i],sep="")))!=0))
    
      SegTable<-as.data.frame(unique(merge3$incplcus))
      SegTable<-rename(SegTable,c("unique(merge3$incplcus)"="City"))
    
    #Create Dummy of 18 Plus
      merge3$Above18<-recode(merge3$age,"c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,NA)=0; else=1")
    #Create Head of Household Variable
      merge3$hh<-recode(merge3$relate,"c(101)=1; else=0")
    #Unrelated Individuals
      merge3$UnRelate<-recode(merge3$relate,"c(1200,1202,1203,1204,1205,1206,1221,1222,1223,1230,1239)=1; else=0")
    #Servants
      #merge3$Serve<-recode(merge3$relate,"1210:1217=1; else=0")
      merge3$Serve2<-recode(merge3$occ50us, "700:720=1; else=0")
    #Unrelated and Over 18
      merge3$UnRelate18<-ifelse(merge3$UnRelate==1 & merge3$Above18==1,1,0)

    #Whites
      merge3$whites<-recode(merge3$race,"100=1; else=0")
      merge3$WHH<-ifelse(merge3$whites==1 & merge3$hh==1,1,0)
    #Blacks 
      merge3$blacks<-recode(merge3$race,"c(200,210)=1; else=0")
      merge3$BHH<-ifelse(merge3$blacks==1 & merge3$hh==1,1,0)
    #In Laboor Force - 2 is the Code for being in the labor force
      merge3$labforce<-recode(merge3$labforce,"9=0; else=merge3$labforce")

    #Black Servants
      merge3$bserve<-ifelse(merge3$blacks==1 & merge3$Serve2==1,1,0)
    #Black Servents in White Households
      whh<-tapply(merge3$WHH, INDEX=list(merge3$serial), FUN=sum)
      whhs<-data.frame(serial=names(whh), WhiteHH=whh)
      merge3<-merge(x=merge3, y=whhs, by="serial", all.x=T)
      merge3$bserveWhite<-ifelse(merge3$bserve==1 & merge3$WhiteHH==1,1,0)

    ##### Where Drop Is Made #####
    #Drop Black Servants in White HH
      merge3<-merge3[which(merge3$bserveWhite==0),]
    
    #Drop Black Children in White HH after Dropping Black Servants in White HH
      merge3$bchild<-ifelse(merge3$blacks==1 & merge3$Above18==0, 1, 0)
      merge3$bchildWHH<-ifelse(merge3$bchild==1 & merge3$WhiteHH==1, 1, 0)
      merge3<-merge3[which(merge3$bchildWHH==0),]
      
      write.dta(merge3, paste("Z:/Users/Matt/Segregation Project/",LongName[i],"_serve.dta", sep=""))
                
  }, error=function(e){cat(conditionMessage(e))})
}      
      
    ####### Population Totals ########
      bltot<-tapply(merge3$blacks, INDEX=list(merge3$Person), FUN=sum)
      blktot<-data.frame(Person=names(bltot), blktot=bltot)
      wtot<-tapply(merge3$whites, INDEX=list(merge3$Person), FUN=sum)
      whtot<-data.frame(Person=names(wtot), whtot=wtot)
      tot<-tapply(merge3$Person, INDEX=list(merge3$Person), FUN=sum)
      tottot<-data.frame(Person=names(tot), tottot=tot)
      
      Pop<-merge(x=blktot, y=whtot, by="Person", all.x=TRUE)
      Pop2<-merge(x=tottot, y=Pop, by="Person", all.x=TRUE)
      Pop3<-merge(x=merge3, y=Pop2, by="Person", all.x=TRUE)
   
    #Calculate Household Totals
      blserial<-tapply(merge3$blacks, INDEX=list(merge3$serial), FUN=sum)
      blserialtot<-data.frame(serial=names(blserial), blserialtot=blserial)
      wserial<-tapply(merge3$whites, INDEX=list(merge3$serial), FUN=sum)
      wserialtot<-data.frame(serial=names(wserial), wserialtot=wserial)
      tserial<-tapply(merge3$Person, INDEX=list(merge3$serial), FUN=sum)
      tserialtot<-data.frame(serial=names(tserial), tserialtot=tserial)
      
      serial<-merge(x=blserialtot, y=wserialtot, by="serial", all.x=TRUE)
      serial<-merge(x=serial, y=tserialtot, by="serial", all.x=TRUE)
      serial$Person<-recode(serial$serial,"\" \"=0; else=1")
      serial<-merge(x=Pop2, y=serial, by="Person", all.x=TRUE)
    
    #CALCULATE DISSIMILARITY BY HOUSEHOLD
      serial$DissimHouse<-abs((serial$blserialtot/serial$blktot)-(serial$wserialtot/serial$whtot))
      SegTable$DissHouse<-0.5*(sum(serial$DissimHouse))
    
    #Isolation by Household
      serial$IsoHouse<-((serial$blserialtot/serial$blktot)*(serial$blserialtot/serial$tserialtot))
      SegTable$IsoHouse<-sum(serial$IsoHouse)
      
    #########Neighbors by Race ##########
      blhouse<-tapply(merge3$blacks, INDEX=list(merge3$stnum_id), FUN=sum)
      blkhouse<-data.frame(stnum_id=names(blhouse), blkhouse=blhouse)
      whouse<-tapply(merge3$whites, INDEX=list(merge3$stnum_id), FUN=sum)
      whhouse<-data.frame(stnum_id=names(whouse), whhouse=whouse)
      thouse<-tapply(merge3$Person, INDEX=list(merge3$stnum_id), FUN=sum)
      tothouse<-data.frame(stnum_id=names(thouse), tothouse=thouse)
        
      House<-merge(x=blkhouse, y=whhouse, by="stnum_id", all.x=T)
      House2<-merge(x=tothouse, y=House, by="stnum_id", all.x=T)
      House2$Person<-recode(House2$stnum_id,"\" \"=0; else=1")
      House2<-merge(y=Pop2, x=House2, by="Person", all.x=T)
      House2$Person<-as.numeric(House2$Person)
        
      Neigh<-merge(y=House2, x=Segment, by="stnum_id", all.x=T)
      Neigh<-Neigh[!duplicated(Neigh$stnum_id),]

    #Divide by Side of Street
      sgright<-Neigh[which(Neigh$side=="R"),]
      sgleft<-Neigh[which(Neigh$side=="L"),]
    
    #Order
      sgright<-sgright[order(sgright$newaddress, sgright$housenum_n),]
      sgleft<-sgleft[order(sgleft$newaddress, sgleft$housenum_n),]
    
    #Position of observation
    #Create Neighbor Pairs
      sgright$N1<-shift(sgright$stnum_id, 1)
      sgright$N2<-shift(sgright$stnum_id, -1)
      #Create a sequence of numbers by segment id.  This code counts from 1 to N+1 for every unique segment id.
      sgright$Seq<-with(sgright, ave(eval(parse(text=paste("sgright$",Seg_ID[i],sep=""))),
                                     eval(parse(text=paste("sgright$",Seg_ID[i],sep=""))), FUN=seq_along))
      #Marks the maximum number in the sequence of numbers.  This is used to determine the last building on the segment
      sgright$Max<-with(sgright, ave(sgright$Seq,eval(parse(text=paste("sgright$",Seg_ID[i],sep=""))), FUN=function(x)
        seq_along(x)==which.max(x)))==1
      #Marking the first and last building of each segment
      sgright$N1<-ifelse(sgright$Max=="TRUE", "NA", sgright$N1)
      sgright$N2<-ifelse(sgright$Seq==1, "NA", sgright$N2)
    
    #Shift Housing Totals same as above;
      sgright$blkhouse<-as.numeric(sgright$blkhouse)
      sgright$whhouse<-as.numeric(sgright$whhouse)
      sgright$tothouse<-as.numeric(sgright$tothouse)
      
      sgright$N1blh<-shift(sgright$blkhouse,1)
        sgright$N1blh<-ifelse(sgright$Max=="TRUE", 0, sgright$N1blh)
        sgright$N1blh<-ifelse(is.na(sgright$N1blh), 1, sgright$N1blh)
      sgright$N1whh<-shift(sgright$whhouse,1)
        sgright$N1whh<-ifelse(sgright$Max=="TRUE", 0, sgright$N1whh)
        sgright$N1whh<-ifelse(is.na(sgright$N1whh), 1, sgright$N1whh)
      sgright$N1toth<-shift(sgright$tothouse,1)
        sgright$N1toth<-ifelse(sgright$Max=="TRUE", 0, sgright$N1toth)
        sgright$N1toth<-ifelse(is.na(sgright$N1toth), 1, sgright$N1toth)
      
      sgright$N2blh<-shift(sgright$blkhouse,-1)
        sgright$N2blh<-ifelse(sgright$Seq==1, 0, sgright$N2blh)
        sgright$N2blh<-ifelse(is.na(sgright$N2blh), 1, sgright$N2blh)
      sgright$N2whh<-shift(sgright$whhouse, -1)
        sgright$N2whh<-ifelse(sgright$Seq==1, 0, sgright$N2whh)
        sgright$N2whh<-ifelse(is.na(sgright$N2whh), 1, sgright$N2whh)
      sgright$N2toth<-shift(sgright$tothouse,-1)
        sgright$N2toth<-ifelse(sgright$Seq==1, 0, sgright$N2toth)
        sgright$N2toth<-ifelse(is.na(sgright$N2toth), 1, sgright$N2toth)
        
      sgright$Ntotb<-as.numeric(sgright$N1blh + sgright$N2blh + sgright$blkhouse)
      sgright$Ntotw<-as.numeric(sgright$N1whh + sgright$N2whh + sgright$whhouse)
      sgright$Ntott<-as.numeric(sgright$N1toth + sgright$N2toth + sgright$tothouse)
    
      sgright$Ntotb2<-as.numeric(sgright$N1blh + sgright$N2blh)
      sgright$Ntotw2<-as.numeric(sgright$N1whh + sgright$N2whh)
      sgright$Ntott2<-as.numeric(sgright$N1toth + sgright$N2toth)
      
      #Create Neighbor Pairs - LEFT SIDE
        sgleft$N1<-shift(sgleft$stnum_id, 1)
        sgleft$N2<-shift(sgleft$stnum_id, -1)
        sgleft$Seq<-with(sgleft, ave(eval(parse(text=paste("sgleft$",Seg_ID[i],sep=""))),
                                       eval(parse(text=paste("sgleft$",Seg_ID[i],sep=""))), FUN=seq_along))
        sgleft$Max<-with(sgleft, ave(sgleft$Seq,eval(parse(text=paste("sgleft$",Seg_ID[i],sep=""))), FUN=function(x)
          seq_along(x)==which.max(x)))==1
        sgleft$N1<-ifelse(sgleft$Max=="TRUE", "NA", sgleft$N1)
        sgleft$N2<-ifelse(sgleft$Seq==1, "NA", sgleft$N2)
        
      #Shift Housing Totals same as above;
        sgleft$blkhouse<-as.numeric(sgleft$blkhouse)
        sgleft$whhouse<-as.numeric(sgleft$whhouse)
        sgleft$tothouse<-as.numeric(sgleft$tothouse)
        
      sgleft$N1blh<-shift(sgleft$blkhouse,1)
        sgleft$N1blh<-ifelse(sgleft$Max=="TRUE", 0, sgleft$N1blh)
        sgleft$N1blh<-ifelse(is.na(sgleft$N1blh), 1, sgleft$N1blh)
      sgleft$N1whh<-shift(sgleft$whhouse,1)
        sgleft$N1whh<-ifelse(sgleft$Max=="TRUE", 0, sgleft$N1whh)
        sgleft$N1whh<-ifelse(is.na(sgleft$N1whh), 1, sgleft$N1whh)
      sgleft$N1toth<-shift(sgleft$tothouse,1)
        sgleft$N1toth<-ifelse(sgleft$Max=="TRUE", 0, sgleft$N1toth)
        sgleft$N1toth<-ifelse(is.na(sgleft$N1toth), 1, sgleft$N1toth)
      
      sgleft$N2blh<-shift(sgleft$blkhouse,-1)
        sgleft$N2blh<-ifelse(sgleft$Seq==1, 0, sgleft$N2blh)
        sgleft$N2blh<-ifelse(is.na(sgleft$N2blh), 1, sgleft$N2blh)
      sgleft$N2whh<-shift(sgleft$whhouse, -1)
        sgleft$N2whh<-ifelse(sgleft$Seq==1, 0, sgleft$N2whh)
        sgleft$N2whh<-ifelse(is.na(sgleft$N2whh), 1, sgleft$N2whh)
      sgleft$N2toth<-shift(sgleft$tothouse,-1)
        sgleft$N2toth<-ifelse(sgleft$Seq==1, 0, sgleft$N2toth)
        sgleft$N2toth<-ifelse(is.na(sgleft$N2toth), 1, sgleft$N2toth)
        
      sgleft$Ntotb<-as.numeric(sgleft$N1blh + sgleft$N2blh + sgleft$blkhouse)
      sgleft$Ntotw<-as.numeric(sgleft$N1whh + sgleft$N2whh + sgleft$whhouse)
      sgleft$Ntott<-as.numeric(sgleft$N1toth + sgleft$N2toth + sgleft$tothouse)
      
      sgleft$Ntotb2<-as.numeric(sgleft$N1blh + sgleft$N2blh)
      sgleft$Ntotw2<-as.numeric(sgleft$N1whh + sgleft$N2whh)
      sgleft$Ntott2<-as.numeric(sgleft$N1toth + sgleft$N2toth)
    
      hhfinal<-rbind(sgright,sgleft)
      hhfinal<-hhfinal[!duplicated(hhfinal$stnum_id),]
      
      #SUM NUMERATORs
      #Drop Cases where person = NA.  This would occur if a building were geocoded on two segments.
        hhfinal<-hhfinal[which(!is.na(hhfinal$Person)),]
        hhfinal$Person<-as.numeric(hhfinal$Person)
        blbg<-tapply(hhfinal$Ntotb, INDEX=list(hhfinal$Person), FUN=sum)
        blkbg<-data.frame(Person=names(blbg), blkbg=blbg)
        wbg<-tapply(hhfinal$Ntotw, INDEX=list(hhfinal$Person), FUN=sum)
        whbg<-data.frame(Person=names(wbg), whhb=wbg)
        tbg<-tapply(hhfinal$Ntott, INDEX=list(hhfinal$Person), FUN=sum)
        totbg<-data.frame(Person=names(tbg), totbg=tbg)
        
        hhfinal2<-hhfinal[which(hhfinal$Ntott2!=0),]
        blbg2<-tapply(hhfinal2$Ntotb2, INDEX=list(hhfinal2$Person), FUN=sum)
        blkbg2<-data.frame(Person=names(blbg2), blkbg2=blbg2)
        wbg2<-tapply(hhfinal2$Ntotw2, INDEX=list(hhfinal2$Person), FUN=sum)
        whbg2<-data.frame(Person=names(wbg2), whhb2=wbg2)
        tbg2<-tapply(hhfinal2$Ntott2, INDEX=list(hhfinal2$Person), FUN=sum)
        totbg2<-data.frame(Person=names(tbg2), totbg2=tbg2)
        
        BLD<-merge(x=blkbg, y=whbg, by="Person", all.x=T)
        BLD<-merge(x=totbg, y=BLD, by="Person", all.x=T)
        BLD<-merge(x=blkbg2, y=BLD, by="Person", all.x=T)
        BLD<-merge(x=whbg2, y=BLD, by="Person", all.x=T)
        BLD<-merge(x=totbg2, y=BLD, by="Person", all.x=T)
        hhfinal<-merge(x=hhfinal, y=BLD, by="Person", all.x=T)
        hhfinal2<-merge(x=hhfinal2, y=BLD, by="Person", all.x=T)
        
      hhfinal<-hhfinal[which(!is.na(hhfinal$Person)),]
      hhfinal2<-hhfinal2[which(!is.na(hhfinal2$Person)),]
      
      #Calculate Building Level Dissimilarity
      #DISSIMILARITY
        hhfinal$DissimBLD<-abs((hhfinal$blkhouse/hhfinal$blktot)-(hhfinal$whhouse/hhfinal$whtot))
        SegTable$DissBLD<-0.5*(sum(hhfinal$DissimBLD))
      
      #Calculate Exposure - # WEIGHT IS BUILDING (blkhouse/blktot)
        #At Building Level
        hhfinal$IsoBLD<-((hhfinal$blkhouse/hhfinal$blktot)*(hhfinal$blkhouse/hhfinal$tothouse))
        SegTable$IsoBLD<-sum(hhfinal$IsoBLD)
          #At Building Group Level
        #hhfinal$IsoBLDGroup<-((hhfinal$Ntotb/hhfinal$blkbg)*(hhfinal$Ntotb/hhfinal$Ntott))
        hhfinal$IsoBLDGroup<-((hhfinal$blkhouse/hhfinal$blktot)*(hhfinal$Ntotb/hhfinal$Ntott))
        SegTable$IsoBLDGroup<-sum(hhfinal$IsoBLDGroup)
          #At Building Level - SANS YOUR BUILDING
        #hhfinal$IsoBLDGroup2<-((hhfinal$Ntotb2/hhfinal$blkbg2)*(hhfinal$Ntotb2/hhfinal$Ntott2))
        hhfinal2$IsoBLDGroup2<-((hhfinal2$blkhouse/hhfinal2$blktot)*(hhfinal2$Ntotb2/hhfinal2$Ntott2))
        SegTable$IsoBLDGroup2<-sum(hhfinal2$IsoBLDGroup2, na.rm=T)
    
    #Segment by Race
      bls<-tapply(merge3$blacks, INDEX=list(eval(parse(text=paste("merge3$",Seg_ID[i],sep="")))), FUN=sum)
      blseg<-data.frame(Seg_ID=names(bls), blseg=bls)
      whs<-tapply(merge3$whites, INDEX=list(eval(parse(text=paste("merge3$",Seg_ID[i],sep="")))), FUN=sum)
      whseg<-data.frame(Seg_ID=names(whs), whseg=whs)
      tots<-tapply(merge3$Person, INDEX=list(eval(parse(text=paste("merge3$",Seg_ID[i],sep="")))), FUN=sum)
      totseg<-data.frame(Seg_ID=names(tots), totseg=tots)
    
      Segment<-merge(x=blseg, y=whseg, by="Seg_ID", all.x=TRUE)
      Segment2<-merge(x=Segment, y=totseg, by="Seg_ID", all.x=TRUE)
      Segment2$Person<-recode(Segment2$Seg_ID,"\" \"=0; else=1")
      Segment2<-merge(y=Pop2, x=Segment2, by="Person", all.x=T)
    
    #DISSIMILARITY
      Segment2$DissimSeg<-abs((Segment2$blseg/Segment2$blktot)-(Segment2$whseg/Segment2$whtot))
      SegTable$DissSeg<-0.5*(sum(Segment2$DissimSeg))
    
    #ISOLATION
      Segment2$IssoSeg<-((Segment2$blseg/Segment2$blktot)*(Segment2$blseg/Segment2$totseg))
      SegTable$IsoSeg<-sum(Segment2$IssoSeg)
 
    ######Segment Side by Race#####
      blsi<-tapply(merge3$blacks, INDEX=list(merge3$segside), FUN=sum)
      blsiseg<-data.frame(Segside=names(blsi), blsiseg=blsi)
      whsi<-tapply(merge3$whites, INDEX=list(merge3$segside), FUN=sum)
      whsiseg<-data.frame(Segside=names(whsi), whsiseg=whsi)
      totsi<-tapply(merge3$Person, INDEX=list(merge3$segside), FUN=sum)
      totsiseg<-data.frame(Segside=names(totsi), totsiseg=totsi)
      
      Segside<-merge(x=blsiseg, y=whsiseg, by="Segside", all.x=TRUE)
      Segside<-merge(x=Segside, y=totsiseg, by="Segside", all.x=TRUE)
      Segside$Person<-recode(Segside$segside,"\" \"=0; else=1")
      Segside<-merge(y=Pop2, x=Segside, by="Person", all.x=T)
      
      #DISSIMILARITY
        Segside$DissimSegside<-abs((Segside$blsiseg/Segside$blktot)-(Segside$whsiseg/Segside$whtot))
        SegTable$DissSegside<-0.5*(sum(Segside$DissimSegside))
      
      #ISOLATION
        Segside$IssoSegside<-((Segside$blsiseg/Segside$blktot)*(Segside$blsiseg/Segside$totsiseg))
        SegTable$IsoSegside<-sum(Segside$IssoSegside)
      
      
    ######Segment Group by Race - Pop Total######
      blsg<-tapply(merge3$blacks, INDEX=list(eval(parse(text=paste("merge3$",hex150[i],sep="")))), FUN=sum)
      blsegG<-data.frame(hex150=names(blsg), blsegG=blsg)
      whsg<-tapply(merge3$whites, INDEX=list(eval(parse(text=paste("merge3$",hex150[i],sep="")))), FUN=sum)
      whsegG<-data.frame(hex150=names(whsg), whsegG=whsg)
      totsg<-tapply(merge3$Person, INDEX=list(eval(parse(text=paste("merge3$",hex150[i],sep="")))), FUN=sum)
      totsegG<-data.frame(hex150=names(totsg), totsegGroup=totsg)
    
    SegmentGroup<-merge(x=blsegG, y=whsegG, by="hex150", all.x=TRUE)
    SegmentGroup2<-merge(x=SegmentGroup, y=totsegG, by="hex150", all.x=TRUE)
    SegmentGroup2$Person<-recode(SegmentGroup2$hex150,"\" \"=0; else=1")
    SegmentGroup2<-merge(y=Pop2, x=SegmentGroup2, by="Person", all.x=T)
    
    #DISSIMILARITY
      SegmentGroup2$DissimSegG<-abs((SegmentGroup2$blsegG/SegmentGroup2$blktot)-(SegmentGroup2$whsegG/SegmentGroup2$whtot))
      SegTable$DissSG<-0.5*(sum(SegmentGroup2$DissimSegG))

    #######Overlapping Segment Group - ISOLATION######
      #Bring in Segment2 which is aggregated by segment and merge it to overlapping segment groups
        OverSeg<-merge(x=Segment2, y=OverSeg_Join[,c("SegmentID", "SegGrID")], by.x="Seg_ID", by.y="SegmentID",all.x=TRUE)
      #Aggregate by Segment Group
        bloverseg<-tapply(OverSeg$blseg, INDEX=list(OverSeg$SegGrID), FUN=sum)
        bloseg<-data.frame(OverSegGr=names(bloverseg), bloseg=bloverseg)
        whoverseg<-tapply(OverSeg$whseg, INDEX=list(OverSeg$SegGrID), FUN=sum)
        whoseg<-data.frame(OverSegGr=names(whoverseg), whoseg=whoverseg)
        totoverseg<-tapply(OverSeg$totseg, INDEX=list(OverSeg$SegGrID), FUN=sum)
        totoseg<-data.frame(OverSegGr=names(totoverseg), totoseg=totoverseg)
        
      #Merge aggregate data to one dataframe (OverlappingSG)
        OverlappingSG<-merge(x=bloseg, y=whoseg, by="OverSegGr", all.x=T)
        OverlappingSG<-merge(x=OverlappingSG, y=totoseg, by="OverSegGr", all.x=T)  
      
      #Attach aggregated segment group information to OverSeg in order to do isolation calculations
        OverSeg<-merge(x=OverSeg, y=OverlappingSG, by.x="SegGrID", by.y="OverSegGr", all.x=TRUE)
      #Keep focal segments which are found if Seg_ID==SegGrID
        OverSeg<-OverSeg[which(OverSeg$SegGrID==OverSeg$Seg_ID),]
        #Count number of unique populated segment groups
        #count<-count(unique(OverSeg$SegGrID))
      
    #ISOLATION
      OverSeg$IssoOverSeg<-((OverSeg$blseg/OverSeg$blktot)*(OverSeg$bloseg/OverSeg$totoseg))
      SegTable$IsoOverSegG<-sum(OverSeg$IssoOverSeg)
      SegTable$NumSG<-length(OverSeg$SegGrID)
      
    #Extended Segment Group by Race - Pop Total
      blsgE<-tapply(merge3$blacks, INDEX=list(eval(parse(text=paste("merge3$",hex225[i],sep="")))), FUN=sum)
      blsegEG<-data.frame(hex225=names(blsgE), blsegEG=blsgE)
      whsgE<-tapply(merge3$whites, INDEX=list(eval(parse(text=paste("merge3$",hex225[i],sep="")))), FUN=sum)
      whsegEG<-data.frame(hex225=names(whsgE), whsegEG=whsgE)
      totsgE<-tapply(merge3$Person, INDEX=list(eval(parse(text=paste("merge3$",hex225[i],sep="")))), FUN=sum)
      totsegEG<-data.frame(hex225=names(totsgE), totsegEG=totsgE)
      
      EXSegmentGroup<-merge(x=blsegEG, y=whsegEG, by="hex225", all.x=TRUE)
      EXSegmentGroup2<-merge(x=EXSegmentGroup, y=totsegEG, by="hex225", all.x=TRUE)
      EXSegmentGroup2$Person<-recode(EXSegmentGroup2$hex225,"\" \"=0; else=1")
      EXSegmentGroup2<-merge(y=Pop2, x=EXSegmentGroup2, by="Person", all.x=T)
    
    #DISSIMILARITY
      EXSegmentGroup2$DissimSegEG<-abs((EXSegmentGroup2$blsegEG/EXSegmentGroup2$blktot)-(EXSegmentGroup2$whsegEG/EXSegmentGroup2$whtot))
      SegTable$DissEX<-0.5*(sum(EXSegmentGroup2$DissimSegEG))
    
    #######Overlapping Extended Segment Group - ISOLATION######
      #Bring in Segment2 which is aggregated by segment and merge it to overlapping extended segment groups (Overlap2)
        ExtOverSeg<-merge(x=Segment2, y=Overlap2[,c("SegmentID", "ExtSegGrID")], by.x="Seg_ID", by.y="SegmentID",all.x=TRUE)
      #Aggregate by Extended Segment Group
        bloverextseg<-tapply(ExtOverSeg$blseg, INDEX=list(ExtOverSeg$ExtSegGrID), FUN=sum)
        bloextseg<-data.frame(ExtOverSegGr=names(bloverextseg), bloextseg=bloverextseg)
        whoverextseg<-tapply(ExtOverSeg$whseg, INDEX=list(ExtOverSeg$ExtSegGrID), FUN=sum)
        whoextseg<-data.frame(ExtOverSegGr=names(whoverextseg), whoextseg=whoverextseg)
        totoverextseg<-tapply(ExtOverSeg$totseg, INDEX=list(ExtOverSeg$ExtSegGrID), FUN=sum)
        totoextseg<-data.frame(ExtOverSegGr=names(totoverextseg), totoextseg=totoverextseg)
        
      #Merge aggregate data into one data frame (OverlappingExtSG)
        OverlappingExtSG<-merge(x=bloextseg, y=whoextseg, by="ExtOverSegGr", all.x=T)
        OverlappingExtSG<-merge(x=OverlappingExtSG, y=totoextseg, by="ExtOverSegGr", all.x=T)  
      
      #Attach aggregated segment group information to OverSeg in order to do isolation calculations
        ExtOverSeg<-merge(x=ExtOverSeg, y=OverlappingExtSG, by.x="ExtSegGrID", by.y="ExtOverSegGr", all.x=TRUE)
      #Keep focal segments which are found if Seg_ID==ExtSegGrID
        ExtOverSeg<-ExtOverSeg[which(ExtOverSeg$ExtSegGrID==ExtOverSeg$Seg_ID),]
      
    #ISOLATION
      ExtOverSeg$IssoOverSegEG<-((ExtOverSeg$blseg/ExtOverSeg$blktot)*(ExtOverSeg$bloextseg/ExtOverSeg$totoextseg))
      SegTable$IsoOverEG<-sum(ExtOverSeg$IssoOverSegEG)
      SegTable$NumExtG<-length(ExtOverSeg$ExtSegGrID)

    #####Calculate Dissimilarity by ED #####
      bed<-tapply(merge3$blacks, INDEX=list(merge3$enumdist), FUN=sum)
      bedt<-data.frame(enumdist=names(bed), bed=bed)
      wed<-tapply(merge3$whites, INDEX=list(merge3$enumdist), FUN=sum)
      wedt<-data.frame(enumdist=names(wed), wed=wed)
      ted<-tapply(merge3$Person, INDEX=list(merge3$enumdist), FUN=sum)
      tedtot<-data.frame(enumdist=names(ted), toted=ted)
      
      ed<-merge(x=bedt, y=wedt, by="enumdist", all.x=TRUE)
      ed<-merge(x=ed, y=tedtot, by="enumdist", all.x=TRUE)
      ed$Person<-recode(ed$enumdist,"\" \"=0; else=1")
      ed<-merge(x=Pop2, y=ed, by="Person", all.x=TRUE)
  
      ed$DissimED<-abs((ed$bed/ed$blktot)-(ed$wed/ed$whtot))
      SegTable$DissED<-0.5*(sum(ed$DissimED))
      
      ed$IsoED<-((ed$bed/ed$blktot)*(ed$bed/ed$toted))
      SegTable$IsoED<-sum(ed$IsoED)
      
    #City
      bcity<-tapply(merge3$blacks, INDEX=list(merge3$incplcus), FUN=sum)
      bcityt<-data.frame(incplcus=names(bcity), bcityt=bcity)
      wcity<-tapply(merge3$whites, INDEX=list(merge3$incplcus), FUN=sum)
      wcityt<-data.frame(incplcus=names(wcity), wcityt=wcity)
      tcity<-tapply(merge3$Person, INDEX=list(merge3$incplcus), FUN=sum)
      tcityt<-data.frame(incplcus=names(tcity), tcityt=tcity)
    
      totcity<-merge(x=bcityt, y=wcityt, by="incplcus", all.x=TRUE)
      totcity<-merge(x=totcity, y=tcityt, by="incplcus", all.x=TRUE)
      
      IsoCity<-((totcity$bcityt/totcity$bcityt)*(totcity$bcityt/totcity$tcityt))
      SegTable$IsoCity<-sum(IsoCity)
      SegTable$PerBlack<-(totcity$bcityt/totcity$tcityt)
    
    ###########RACE BY STREET TYPE #######
      merge3$type<-as.character(merge3$type)
      RbT<-xtabs(~merge3$blacks+merge3$type, data=merge3)
      RbT<-as.data.frame(RbT)
      RaceByType<-cast(RbT, merge3.blacks ~ merge3.type)
      RaceByType$merge3.blacks<-as.integer(RaceByType$merge3.blacks)
      RaceByType<-RaceByType[,-1]
      RaceByType$Total<-rowSums(RaceByType)
      RaceByType<-rbind(RaceByType,(colSums(RaceByType)))
      labels<-c("White", "Black", "Total")
      RaceByType<-cbind(labels, RaceByType)
      RaceByType$City<-Cityname[i]
      
    AllSegTable<-rbind(SegTable,AllSegTable)

    write.csv(AllSegTable, file=("C:/Users/mmarti24/Dropbox/Papers/Southern Cities/Updated_Calculations.csv"))
    
    
    ispresent<-rbind(ispresent,tab)
  }, error=function(e){cat(conditionMessage(e))})
}
    
    #Create Spreadsheets for Total Isolation and Dissimilarity Indicies and a table for each city.  Also create a table with 
    #tabs that correspond to each city and to each Race By Street Type
    write.csv(hhfinal, file=paste("Z:/Users/Matt/Segregation Project/",Cityname[i],"segcalc.csv",sep=""))
    write.xlsx2(RaceByType, file = paste("Z:/Users/Matt/Segregation Project/RaceByType(AllCities).xlsx",sep=""), 
                sheetName = Cityname[i], row.names = FALSE, append=T)
    
    rm(RaceByType, hhfinal, SegmentGroup2, SegmentGroup, Segment, Points, OverSeg, SegGroup, tothouse, totoextseg,
       totseg, totsegEG, Pop3, blkhouse, bloextseg, bloseg, blseg, blsegEG, blsegG, Extended)
    
    #Create spreadsheet for each city with list of segments making up segment groups - segment groups making up extended segment groups and
    #segments within extended segment groups
    write.xlsx2(OverSeg_Join, file = paste("Z:/Users/Matt/Segregation Project/",Cityname[i],"_IDList.xlsx",sep=""), 
                sheetName = "SegmentGroups", row.names = FALSE, append=T)
    rm(OverSeg_Join, OverlappingSG, OverlappingExtSG, merge1, merge2, merge3,Neigh)
    write.xlsx2(ExtSeg_Join, file = paste("Z:/Users/Matt/Segregation Project/",Cityname[i],"_IDList.xlsx",sep=""), 
                sheetName = "ExtendedSegmentGroups", row.names = FALSE, append=T)
    rm(ExtSeg_Join, Overlap)
    write.xlsx2(Overlap2, file = paste("Z:/Users/Matt/Segregation Project/",Cityname[i],"_IDList.xlsx",sep=""), 
                sheetName = "ExtSegmentGroups-Segments", row.names = FALSE, append=T)
  
    tab<-as.data.frame(unique(merge3$incplcus))
    tab<-rename(tab,c("unique(merge3$incplcus)"="City"))
    ispresent<-rbind(ispresent,tab)
  }, error=function(e){cat(conditionMessage(e))})
}

write.csv(AllSegTable, file="Z:/Users/Matt/Segregation Project/Segregation Tables(AllCities).csv",row.names=F)
