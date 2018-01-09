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

  sumby<- function(x,y){
    y1<-deparse(substitute(y))
    y2<-unlist((strsplit(as.character(y1), "[$]")))[2]
    myvars<-"y"
    nrows<-length(x)
    df<-data.frame(x=numeric(), y=numeric())
    df<-rename(df, c(x=y2, y=y))
    for(i in 1:nrows){
      x2<-(colnames(x[i]))
      t<-(tapply(x[,i], INDEX=list(y), FUN=sum, na.rm=T))
      df2<-data.frame(x=names(t), y=t)
      df2<-rename(df2, c(x=y2, y=x2))
      df<-merge(df, df2, by=y2, all=T, accumulate=T)
      df<-df[!names(df) %in% myvars]
    }
    df
  }
  
  sumby_1<- function(x,y){
    y1<-deparse(substitute(y))
    y2<-unlist((strsplit(as.character(y1), "[$]")))[2]
    myvars<-"y"
    nrows<-length(x)
    df<-data.frame(x=numeric(), y=numeric())
    df<-rename(df, c(x=y2, y=y))
    for(i in 1:nrows){
      x2<-(colnames(x[i]))
      t<-(tapply(x[,i], INDEX=list(y), FUN=sum, na.rm=T))
      df2<-data.frame(x=names(t), y=t)
      df2$x<-ifelse(df2$x>0, df2$x-1, df2$x)
      df2$y<-ifelse(df2$x>0, df2$y-1, df2$y)
      df2<-rename(df2, c(x=y2, y=x2))
      df<-merge(df, df2, by=y2, all=T, accumulate=T)
      df<-df[!names(df) %in% myvars]
    }
    df
  }

  x<-All$german
  
  
  
  
  hh<-sumby(All[,c("german", "irish", "native_white", "Person")], All$serial)
  
  
  
  
  
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
  
  
  
  
  citylist<-read.csv(("Z:/Projects/Preparing 1880 Files/City Lists.csv"))
  citylist <- data.frame(lapply(citylist, as.character), stringsAsFactors=FALSE)
  #citylist<-subset(citylist, (citylist$Maps=="x"))
  
  Cityname<-citylist$Cityname
  Seg_ID<-citylist$Seg_ID
  Ex<-citylist$Ex
  SG<-citylist$SG
  Seg<-citylist$Seg
  hex150<-citylist$hex150
  hex225<-citylist$hex225
  Seg_ID<-citylist$Seg_ID
  seg_id1<-citylist$Seg_ID_1
  seggrid<-citylist$SegGrID
  seggrid_1<-citylist$SegGrID_1
  rows<-nrow(citylist)
  
  AllSegTable<-data.frame(City=character(), PerMulti_H=numeric(), PerMulti_B=numeric(), IsoHouse_g=numeric(), IsoBLD_g=numeric(),
                          IsoMultiBLD_g=numeric(), IsoBLDGroup_g=numeric(), IsoBLDGroup2_g=numeric(), IsoSegside_g=numeric(),
                          IsoSeg_g=numeric(), IsoOverSeg_g=numeric(), IsoOverEG_g=numeric(), IsoED_g=numeric(), IsoCity_g=numeric(),
                          IsoHouse_i=numeric(), IsoBLD_i=numeric(),IsoMultiBLD_i=numeric(), IsoBLDGroup_i=numeric(), IsoBLDGroup2_i=numeric(),
                          IsoSegside_i=numeric(), IsoSeg_i=numeric(), IsoOverSegG_i=numeric(),IsoOverEG_i=numeric(),IsoED_i=numeric(), IsoCity_i=numeric(),
                          DissimHouse_g=numeric(), DissimHouse_g=numeric(), DissimBLD_g=numeric(), DissimMultiBLD_g=numeric(),
                          DissimSegside_g=numeric(), Dissim_g=numeric(), DissimSegG_g=numeric(),DissimExSegG_g=numeric(), DissimED_g=numeric(),
                          DissimCity_g=numeric(), DissimHouse_i=numeric(),DissimBLD_i=numeric(), DissimMultiBLD_i=numeric(), DissimSegside_i=numeric(),
                          Dissim_i=numeric(), DissimSegG_i=numeric(), DissimExSegG_i=numeric(), DissimED_i=numeric(),DissimCity_i=numeric())
  
for(i in 1:1){
}
  tryCatch({
    Segment<-read.dta13(paste("Z:/Projects/Preparing 1880 Files/", Cityname[i],"/Match Address/", Cityname[i],".dta", sep=""))
    Points<-read.dta13(paste("Z:/Projects/1880Data/MicroData For Publication/", Cityname[i],".dta",sep=""))
    Off<-read.dbf(paste("Z:/Projects/Preparing 1880 Files/", Cityname[i], "/Off Street/", Cityname[i],"_OffStreet.dbf", sep=""))
    
    #Fix Names in Segment File
      names(Segment)<-tolower(names(Segment))
      Segment$serial<-as.character(Segment$serial)
      Segment<-subset(Segment, !duplicated(Segment$serial))
      ifelse(exists("street.1", where=Segment), (Segment$street_stnum=Segment$street.1), (Segment$street_stnum=Segment$street))
    
    #Make Changes to 'Off' data
      names(Off)<-tolower(names(Off))    
      Off<- data.frame(lapply(Off, as.character), stringsAsFactors=FALSE)
  
      Off<-subset(Off, !duplicated(Off$serial))
      Off$serial<-as.numeric(Off$serial)
      Off$nhn<-trim(laply(strsplit(as.character(Off$addr_match), split=" "), "[", 1))
      Off$street<-trim(laply(strsplit(as.character(Off$addr_match), split=","), "[", 1))
      Off$street<-trim(gsub("^[0-9]* ","",Off$street, perl=T))
  
    #Tag Unique Streets and Housenumbers - I.E. BUILDINGS
      Off<-Off[order(Off$street,Off$nhn),]
      Off$stnum_id<-id(Off[c("street","nhn")], drop=T)
    
    #Fix names in Points file
      names(Points)<-tolower(names(Points))
      ifelse(exists("bpldet", where=Points), (Points$bpldet=Points$bpldet), (Points$bpldet=Points$bp))
      ifelse(exists("fbpldtus", where=Points), (Points$fbpldtus=Points$fbpldtus), (Points$fbpldtus=Points$fbp))
      ifelse(exists("mbpldtus", where=Points), (Points$mbpldtus=Points$mbpldtus), (Points$mbpldtus=Points$mbp))
      Points$serial<-as.character(Points$serial)
      Points$bpldet<-as.character(Points$bpldet)
      Points$fbpldtus<-as.character(Points$fbpldtus)
      Points$mbpldtus<-as.character(Points$mbpldtus)
    
    All<-merge(Points[,c("bpldet", "fbpldtus", "mbpldtus","race","relate","age","occ50us","labforce","serial","enumdist")],
               Segment[,c("serial", "side", "segment_id","seggroup_id", "extgroup_id", "building_id", "house_number", "street_stnum")], by="serial")
    #Trim leading zeros to match with OffStreet File
      All$serial<-as.numeric(All$serial)
      #All<-merge(All, Off[,c("serial", "nhn", "stnum_id", "street")], by="serial", all.x=T)
    #Tag Unique Sides and Segments
      All$segside<-id(All[c("segment_id","side")], drop=T)
    #Changing factors to numeric (seggroup_id)  
      All$seggroup_id<-as.numeric(All$seggroup_id)
    #Create City Name Field
      All$City<-Cityname[i]
    #Create Addresses to Geocode
      All$Address<-paste(All$house_number, " ", All$street_stnum, sep="")
    #Create stnum_id
      All<-All[order(All$street_stnum, All$house_number),]
      All$stnum_id<-id(All[c("street_stnum", "house_number")], drop=T)
    #Create Person Counter for Aggregation  
      All$Person<-recode(All$serial,"\" \"=0; else=1")
      All<-subset(All, All$segment_id!=0)
    #Create Dummy of 18 Plus
      All$Above18<-recode(All$age,"c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,NA)=0; else=1")
    #Create Head of Household Variable
      All$hh<-recode(All$relate,"c(101)=1; else=0")
    #Unrelated Individuals
      All$UnRelate<-recode(All$relate,"c(1200,1202,1203,1204,1205,1206,1221,1222,1223,1230,1239)=1; else=0")
    #Servants
    #All$Serve<-recode(All$relate,"1210:1217=1; else=0")
      All$Serve2<-recode(All$occ50us, "700:720=1; else=0")
    #Unrelated and Over 18
      All$UnRelate18<-ifelse(All$UnRelate==1 & All$Above18==1,1,0)
    #Race
      All$race<-car::recode(All$race, "100='white';c(200, 210)='black';300='American Indian/Alaska Native';
                               400='Chinese';500='Japanese';else='Missing'")  
    #Birthplace
      bpl<-read.csv("Z:/Projects/Preparing 1880 Files/BPL_Codes.csv", stringsAsFactors = F)
      All<-merge(All, bpl[,c("Code", "Label")], by.x="bpldet", by.y="Code", all.x=T)
      All<-rename(All, c("Label"="BPL"))
    #Father's Birthplace
      fbpl<-read.csv("Z:/Projects/Preparing 1880 Files/FBPL_Codes.csv", stringsAsFactors = F)
      All<-merge(All, fbpl[,c("Code", "Label")], by.x="fbpldtus", by.y="Code", all.x=T)
      All<-rename(All, c("Label"="FBPL"))
    #Mother's Birthplace
      mbpl<-read.csv("Z:/Projects/Preparing 1880 Files/MBPL_Codes.csv", stringsAsFactors = F)
      All<-merge(All, mbpl[,c("Code", "Label")], by.x="mbpldtus", by.y="Code", all.x=T)
      All<-rename(All, c("Label"="MBPL"))
    #Native Born
      All$bpldet<-as.numeric(All$bpldet)
      All$native_b<-ifelse(All$bpldet<=9900, 1, 0)
    #Native Parents
      All$mbpldtus<-as.numeric((All$mbpldtus))
      All$fbpldtus<-as.numeric((All$fbpldtus))
      All$native_p<-ifelse(All$mbpldtus<=9900 & All$fbpldtus<=9900, 1, 0)
    #Native Born and Native Born Parents
      All$native_bp<-ifelse(All$native_b==1 & All$native_p==1, 1,0)
    #Native Born, Native Parents, White
      All$native_white<-ifelse(All$native_bp==1 & All$race=="white", 1, 0)
      
    #Germans (_f=first generation; _s=second generation; _t=total)
      #Disregard any person who has a mother that is Irish
      All$german_f<-ifelse((All$bpldet>=45300 & All$bpldet<=45362), 1,0)
      All$german_s<-ifelse(All$native_b==1 & ((All$mbpldtus>=45300 & All$mbpldtus<=45362) | 
                                              (All$fbpldtus>=45300 & All$fbpldtus<=45362)) & 
                                              (All$fbpldtus!=41400 | All$fbpldtus!=41410),1,0)
      #######Make Change to German Definition Here #####
      All$german<-ifelse(All$german_f==1 | All$german_s==1, 1, 0)
      #All$german<-ifelse(All$german_f==1, 1, 0)
      
    #Irish (_f=first generation; _s=second generation; _t=total)
      #Disregard any person who has a mother that is German
      All$irish_f<-ifelse((All$bpldet==41400 | All$bpldet==41410),1,0)
      All$irish_s<-ifelse(All$native_b==1 & ((All$mbpldtus==41400 | All$mbpldtus==41410) |
                                              (All$fbpldtus==41400 | All$fbpldtus==41410)) &
                                              (All$fbpldtus!=45300 | All$fbpldtus!=45362),1,0)
      #######Make Change to Irish Definition Here #####
      All$irish<-ifelse(All$irish_f==1 | All$irish_s==1, 1, 0)
      #All$irish<-ifelse(All$irish_f==1, 1, 0)
    #Keep only adults and only heads of households      
      All<-All[which(All$Above18==1),]
      
    #For Only Multiunit Buildings
      #Create Unique Household/Building Identifier
        All<-All[order(All$Address, All$serial),]
        All$hb<-id(All[c("Address", "serial")],drop=T)
      
      #Count both total persons per building and total persons per household. If the two match then only one household is in building
        hb<-tapply(All$Person, INDEX=list(All$hb), FUN = sum, na.rm=T)
        hb2<-data.frame(hb=names(hb), hb_Max=hb)
        All<-merge(x=All, y=hb2, by="hb", all.x=T)
      
        Address<-tapply(All$Person, INDEX=list(All$Address), FUN=sum, na.rm=T)
        Address2<-data.frame(Address=names(Address), Address_Max=Address)
        All<-merge(x=All, y=Address2, by="Address", all.x=T)
      
      #Mark MultiUnit Households and Subset
        All$multi<-ifelse(All$hb_Max==All$Address_Max, 0, 1)
        multi<-subset(All, All$multi==1)
      
      ##### Trim to head of household only ######    
        multi<-multi[which(multi$hh==1),]
        All<-All[which(All$hh==1),]
      
      t<-data.frame(table(All$german))
      All$german_t<-t[2,2]
      
      t<-data.frame(table(All$irish))
      All$irish_t<-t[2,2]
      
      t<-data.frame(table(All$native_white))
      All$native_white_t<-t[2,2]

      #This shapefile has a list of segment ID's and their corresponding segments in the group.
      #Seg_ID_1 is the group id which gets dissolved.
      #Seg_ID is the segment that is part of each group.  So if there is seg_id=10, that means that segment id 10 is in
      #all of the segment groups listed in Seg_ID_1
        OverSeg_Join<-read.dbf(paste("Z:/Users/_Exchange/1880 Stuff/AllCities/",Cityname[i],"/",Cityname[i],"_OverSeg_Join.dbf", sep=""))
        names(OverSeg_Join)<-tolower(names(OverSeg_Join))
        OverSeg_Join$SegGrID<-eval(parse(text=(paste("OverSeg_Join$",seg_id1[i],sep=""))))
        OverSeg_Join$SegmentID<-eval(parse(text=(paste("OverSeg_Join$",Seg_ID[i],sep=""))))
        myvars<-c("SegmentID", "SegGrID")
        OverSeg_Join<-OverSeg_Join[myvars]
        OverSeg_Join$City<-Cityname[i]
      
      ####### Overlapping Segment Group and Overlapping Extended Segment Group #####
        
      #This shapefile has a list of all segment group ID's and their corresponding segment groups in the extended segment group
      #SegGrID is the segment group id.  This value is repeated indicating the unique extended segment groups
      #that this segment group is part of.
      #SegGrID_1 is the focal segment group.
        ExtSeg_Join<-read.dbf(paste("Z:/Users/_Exchange/1880 Stuff/AllCities/",Cityname[i],"/",Cityname[i],"_ExtSeg_Join2.dbf", sep=""))
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
      
    #Create segregation table where output will be stored  
      SegTable<-as.data.frame(unique(All$City))
      SegTable<-rename(SegTable,c("unique(All$City)"="City"))
      
      ####### Population Totals ########
      #Pop_A is analytic sample Pop_t is city total
        All$Pop_t<-length((All$serial))
        All$Pop_a<-length((All$Person))
        t<-data.frame(table(All$native_b))
        All$Pop_native<-t[2,2]
        
    #Calculate Household Totals
        hh<-sumby(All[,c("german", "irish", "native_white", "Person")], All$serial)
        hh<-rename(hh, c("Person"="hh_total"))
        
        hh<-sumby_1(All[,c("german", "irish", "native_white", "Person")], All$serial)
        hh<-rename(hh, c("Person"="hh_total"))
        
        
        hh<-merge(x=hh, y=All[,c("serial", "german_t","irish_t", "native_white_t")], by="serial", all.x=T)
        hh<-subset(hh, !duplicated(hh$serial))
      
      #Isolation by Household
        hh$IsoHouse_g<-((hh$german/hh$german_t)*(hh$german/hh$hh_total))
        SegTable$IsoHouse_g<-sum(hh$IsoHouse_g)
      
        hh$IsoHouse_i<-((hh$irish/hh$irish_t)*(hh$irish/hh$hh_total))
        SegTable$IsoHouse_i<-sum(hh$IsoHouse_i)
        
      #Dissimilarity by Household
        hh$DissimHouse_g<-abs((hh$german/hh$german_t)-(hh$native_white/hh$native_white_t))
        SegTable$DissimHouse_g<-0.5*(sum(hh$DissimHouse_g))
        
        hh$DissimHouse_i<-abs((hh$irish/hh$irish_t)-(hh$native_white/hh$native_white_t))
        SegTable$DissimHouse_i<-0.5*(sum(hh$DissimHouse_i))

#########Neighbors by Race ##########
        build<-sumby(All[,c("german", "irish", "native_white", "Person")], All$Address)
        build<-rename(build, c("german"="g_build", "irish"="i_build", "native_white"="w_build", "Person"="t_build"))
        build<-merge(y=build, x=All[,c("Address","german_t", "irish_t","native_white_t", "Person")], by="Address", all.x=T)
        build<-subset(build, !duplicated(build$Address))
        build<-subset(build, !is.na(build$g_build))
        
        Neigh<-merge(y=build, x=All[,c("side", "Address", "segment_id", "stnum_id")], by="Address", all.x=T)
        Neigh<-subset(Neigh, !duplicated(Neigh$Address))

        #Divide by Side of Street
          sgright<-Neigh[which(Neigh$side=="R"),]
          sgright$side<-as.character(sgright$side)
          sgright$segment_id<-as.character(sgright$segment_id)
          sgleft<-Neigh[which(Neigh$side=="L"),]
          sgleft$side<-as.character(sgleft$side)
          sgleft$segment_id<-as.character(sgleft$segment_id)
          
        #Order
          sgright<-sgright[order(sgright$stnum_id),]
          sgleft<-sgleft[order(sgleft$stnum_id),]    
        
        #Position of observation
        #Create Neighbor Pairs
          sgright$N1<-shift(sgright$stnum_id, 1)
          sgright$N2<-shift(sgright$stnum_id, -1)
        #Create a sequence of numbers by segment id.  This code counts from 1 to N+1 for every unique segment id.
          sgright$Seq<-with(sgright, ave(sgright$segment_id,sgright$segment_id, FUN=seq_along))
        #Marks the maximum number in the sequence of numbers.  This is used to determine the last building on the segment
          sgright$Max<-with(sgright, ave(sgright$Seq, sgright$segment_id, FUN=function(x)
            seq_along(x)==which.max(x)))==1
        #Marking the first and last building of each segment
          sgright$N1<-ifelse(sgright$Max=="TRUE", "NA", sgright$N1)
          sgright$N2<-ifelse(sgright$Seq==1, "NA", sgright$N2)
        
        #Shift Building Totals;      
        sgright$N1gh<-shift(sgright$g_build,1)
          sgright$N1gh<-ifelse(sgright$Max=="TRUE", 0, sgright$N1gh)
        sgright$N1gh<-ifelse(is.na(sgright$N1gh), 1, sgright$N1gh)
          sgright$N2gh<-shift(sgright$g_build,-1)
        sgright$N2gh<-ifelse(sgright$Seq==1, 0, sgright$N2gh)
          sgright$N2gh<-ifelse(is.na(sgright$N2gh), 1, sgright$N2gh)   
        
        sgright$N1ih<-shift(sgright$i_build,1)
          sgright$N1ih<-ifelse(sgright$Max=="TRUE", 0, sgright$N1ih)
        sgright$N1ih<-ifelse(is.na(sgright$N1ih), 1, sgright$N1ih)
          sgright$N2ih<-shift(sgright$i_build, -1)
        sgright$N2ih<-ifelse(sgright$Seq==1, 0, sgright$N2ih)
          sgright$N2ih<-ifelse(is.na(sgright$N2ih), 1, sgright$N2ih)
        
        sgright$N1wh<-shift(sgright$w_build,1)
          sgright$N1wh<-ifelse(sgright$Max=="TRUE", 0, sgright$N1wh)
        sgright$N1wh<-ifelse(is.na(sgright$N1wh), 1, sgright$N1wh)
          sgright$N2wh<-shift(sgright$w_build, -1)
        sgright$N2wh<-ifelse(sgright$Seq==1, 0, sgright$N2wh)
          sgright$N2wh<-ifelse(is.na(sgright$N2wh), 1, sgright$N2wh)
        
        sgright$N1toth<-shift(sgright$t_build,1)
          sgright$N1toth<-ifelse(sgright$Max=="TRUE", 0, sgright$N1toth)
        sgright$N1toth<-ifelse(is.na(sgright$N1toth), 1, sgright$N1toth)
          sgright$N2toth<-shift(sgright$t_build,-1)
        sgright$N2toth<-ifelse(sgright$Seq==1, 0, sgright$N2toth)
          sgright$N2toth<-ifelse(is.na(sgright$N2toth), 1, sgright$N2toth)
        
        sgright$Ntotg<-as.numeric(sgright$N1gh + sgright$N2gh + sgright$g_build)
        sgright$Ntoti<-as.numeric(sgright$N1ih + sgright$N2ih + sgright$i_build)
        sgright$Ntotw<-as.numeric(sgright$N1wh + sgright$N2wh + sgright$w_build)
        sgright$Ntott<-as.numeric(sgright$N1toth + sgright$N2toth + sgright$t_build)
        
        sgright$Ntotg2<-as.numeric(sgright$N1gh + sgright$N2gh)
        sgright$Ntoti2<-as.numeric(sgright$N1ih + sgright$N2ih)
        sgright$Ntotw2<-as.numeric(sgright$N1wh + sgright$N2wh)
        sgright$Ntott2<-as.numeric(sgright$N1toth + sgright$N2toth)  
        
        #Shift Housing Totals;      
        #Position of observation - Create Neighbor Pairs
          sgleft$N1<-shift(sgleft$stnum_id, 1)
          sgleft$N2<-shift(sgleft$stnum_id, -1)
        #Create a sequence of numbers by segment id.  This code counts from 1 to N+1 for every unique segment id.
          sgleft$Seq<-with(sgleft, ave(sgleft$segment_id,sgleft$segment_id, FUN=seq_along))
        #Marks the maximum number in the sequence of numbers.  This is used to determine the last building on the segment
          sgleft$Max<-with(sgleft, ave(sgleft$Seq, sgleft$segment_id, FUN=function(x)
            seq_along(x)==which.max(x)))==1
        #Marking the first and last building of each segment
          sgleft$N1<-ifelse(sgleft$Max=="TRUE", "NA", sgleft$N1)
          sgleft$N2<-ifelse(sgleft$Seq==1, "NA", sgleft$N2)
        
        sgleft$N1gh<-shift(sgleft$g_build,1)
          sgleft$N1gh<-ifelse(sgleft$Max=="TRUE", 0, sgleft$N1gh)
        sgleft$N1gh<-ifelse(is.na(sgleft$N1gh), 1, sgleft$N1gh)
        sgleft$N2gh<-shift(sgleft$g_build,-1)
          sgleft$N2gh<-ifelse(sgleft$Seq==1, 0, sgleft$N2gh)
        sgleft$N2gh<-ifelse(is.na(sgleft$N2gh), 1, sgleft$N2gh)   
        
        sgleft$N1ih<-shift(sgleft$i_build,1)
          sgleft$N1ih<-ifelse(sgleft$Max=="TRUE", 0, sgleft$N1ih)
        sgleft$N1ih<-ifelse(is.na(sgleft$N1ih), 1, sgleft$N1ih)
        sgleft$N2ih<-shift(sgleft$i_build, -1)
          sgleft$N2ih<-ifelse(sgleft$Seq==1, 0, sgleft$N2ih)
        sgleft$N2ih<-ifelse(is.na(sgleft$N2ih), 1, sgleft$N2ih)
        
        sgleft$N1wh<-shift(sgleft$w_build,1)
          sgleft$N1wh<-ifelse(sgleft$Max=="TRUE", 0, sgleft$N1wh)
        sgleft$N1wh<-ifelse(is.na(sgleft$N1wh), 1, sgleft$N1wh)
          sgleft$N2wh<-shift(sgleft$w_build, -1)
        sgleft$N2wh<-ifelse(sgleft$Seq==1, 0, sgleft$N2wh)
          sgleft$N2wh<-ifelse(is.na(sgleft$N2wh), 1, sgleft$N2wh)
        
        sgleft$N1toth<-shift(sgleft$t_build,1)
          sgleft$N1toth<-ifelse(sgleft$Max=="TRUE", 0, sgleft$N1toth)
        sgleft$N1toth<-ifelse(is.na(sgleft$N1toth), 1, sgleft$N1toth)
        sgleft$N2toth<-shift(sgleft$t_build,-1)
          sgleft$N2toth<-ifelse(sgleft$Seq==1, 0, sgleft$N2toth)
        sgleft$N2toth<-ifelse(is.na(sgleft$N2toth), 1, sgleft$N2toth)
        
        sgleft$Ntotg<-as.numeric(sgleft$N1gh + sgleft$N2gh + sgleft$g_build)
        sgleft$Ntoti<-as.numeric(sgleft$N1ih + sgleft$N2ih + sgleft$i_build)
        sgleft$Ntotw<-as.numeric(sgleft$N1wh + sgleft$N2wh + sgleft$w_build)
        sgleft$Ntott<-as.numeric(sgleft$N1toth + sgleft$N2toth + sgleft$t_build)
        
        #Sans Self
          sgleft$Ntotg2<-as.numeric(sgleft$N1gh + sgleft$N2gh)
          sgleft$Ntoti2<-as.numeric(sgleft$N1ih + sgleft$N2ih)
          sgleft$Ntotw2<-as.numeric(sgleft$N1wh + sgleft$N2wh)
          sgleft$Ntott2<-as.numeric(sgleft$N1toth + sgleft$N2toth)  
          
          hhfinal<-rbind(sgright,sgleft)
          hhfinal<-hhfinal[!duplicated(hhfinal$stnum_id),]
        
        #SUM NUMERATORs
        #Drop Cases where person = NA.  This would occur if a building were geocoded on two segments.
          hhfinal<-hhfinal[which(!is.na(hhfinal$Person)),]
          hhfinal$Person<-as.numeric(hhfinal$Person)
          
          #Sum by Neighbor
          BLD<-sumby(hhfinal[,c("Ntotg", "Ntoti", "Ntotw", "Ntott")], hhfinal$Person)
          BLD<-rename(BLD, c("Ntotg"="g_neigh", "Ntoti"="i_neigh", "Ntotw"="w_neigh", "Ntott"="t_neigh"))
          
          #Sans Own Building       
          hhfinal2<-hhfinal[which(hhfinal$Ntott2!=0),]
          BLD2<-sumby(hhfinal2[,c("Ntotg2", "Ntoti2", "Ntotw2", "Ntott2")], hhfinal2$Person)
          BLD2<-rename(BLD2, c("Ntotg2"="g_neigh2", "Ntoti2"="i_neigh2", "Ntotw2"="w_neigh2", "Ntott2"="t_neigh2"))
          
        BLD<-merge(x=BLD, y=BLD2, by="Person", all.x=T)
        hhfinal<-merge(x=hhfinal, y=BLD, by="Person", all.x=T)
        hhfinal<-hhfinal[which(!is.na(hhfinal$Person)),]   
        
        hhfinal2<-merge(x=hhfinal2, y=BLD, by="Person", all.x=T)
        hhfinal2<-hhfinal2[which(!is.na(hhfinal2$Person)),]
        
      #Calculate Exposure - # WEIGHT IS BUILDING (E_build/ETHNICITY_t)
      #At Building Level
        hhfinal$IsoBLD_g<-((hhfinal$g_build/hhfinal$german_t)*(hhfinal$g_build/hhfinal$t_build))
        SegTable$IsoBLD_g<-sum(hhfinal$IsoBLD_g)
        hhfinal$IsoBLD_i<-((hhfinal$i_build/hhfinal$irish_t)*(hhfinal$i_build/hhfinal$t_build))
        SegTable$IsoBLD_i<-sum(hhfinal$IsoBLD_i)
        
      #Dissimilarity at Building Level
        hhfinal$DissimBLD_g<-abs((hhfinal$g_build/hhfinal$german_t)-(hhfinal$w_build/hhfinal$native_white_t))
        SegTable$DissimBLD_g<-0.5*(sum(hhfinal$DissimBLD_g))
        hhfinal$DissimBLD_i<-abs((hhfinal$i_build/hhfinal$irish_t)-(hhfinal$w_build/hhfinal$native_white_t))
        SegTable$DissimBLD_i<-0.5*(sum(hhfinal$DissimBLD_i))
          
          #Calculate Percent of Households that reside in Multi-Unit Buildings
            SegTable$PerMulti_H<-(as.numeric(length(unique(multi$serial))) / as.numeric(length(unique(All$serial))))
            SegTable$PerMulti_B<-(as.numeric(length(unique(multi$Address))) / as.numeric(length(unique(All$Address))))
            
          #Do Calculations
            MultiUnit<-sumby(multi[,c("german", "irish", "native_white", "Person")], multi$Address)
            MultiUnit<-rename(MultiUnit, c("german"="g_multi","irish"="i_multi","native_white"="w_multi", "Person"= "t_multi"))
            multi<-merge(x=multi, y=MultiUnit, by="Address")
          
          #ReCalculate German Totals in Subset
            t<-data.frame(table(multi$german))
            multi$german_t<-t[2,2]
          
            t<-data.frame(table(multi$irish))
            multi$irish_t<-t[2,2]
          
            t<-data.frame(table(multi$native_white))
            multi$native_white_t<-t[2,2]
          
          #Keep only one unique building
            multi<-multi[!duplicated(multi$Address),]
        
        #Isolation
          multi$IsoMultiBLD_g<-((multi$g_multi/multi$german_t)*(multi$g_multi/multi$t_multi))
          SegTable$IsoMultiBLD_g<-sum(multi$IsoMultiBLD_g)
          multi$IsoMultiBLD_i<-((multi$i_multi/multi$irish_t)*(multi$i_multi/multi$t_multi))
          SegTable$IsoMultiBLD_i<-sum(multi$IsoMultiBLD_i)
          
        #Dissimilarity
          multi$DissimMultiBLD_g<-abs((multi$g_multi/multi$german_t)-(multi$w_multi/multi$native_white_t))
          SegTable$DissimMultiBLD_g<-0.5*(sum(multi$DissimMultiBLD_g))
          multi$DissimMultiBLD_i<-abs((multi$i_multi/multi$irish_t)-(multi$w_multi/multi$native_white_t))
          SegTable$DissimMultiBLD_i<-0.5*(sum(multi$DissimMultiBLD_i))

      #At Building Group Level
        #Isolation
          hhfinal$IsoBLDGroup_g<-((hhfinal$g_build/hhfinal$german_t)*(hhfinal$Ntotg/hhfinal$Ntott))
          SegTable$IsoBLDGroup_g<-sum(hhfinal$IsoBLDGroup_g, na.rm=T)
          hhfinal$IsoBLDGroup_i<-((hhfinal$i_build/hhfinal$irish_t)*(hhfinal$Ntoti/hhfinal$Ntott))
          SegTable$IsoBLDGroup_i<-sum(hhfinal$IsoBLDGroup_i, na.rm=T)
        
      #At Building Level - SANS YOUR BUILDING
        #Isolation
        hhfinal2$IsoBLDGroup2_g<-((hhfinal2$g_build/hhfinal2$german_t)*(hhfinal2$Ntotg2/hhfinal2$Ntott2))
        SegTable$IsoBLDGroup2_g<-sum(hhfinal2$IsoBLDGroup2_g, na.rm=T)
        hhfinal2$IsoBLDGroup2_i<-((hhfinal2$i_build/hhfinal2$irish_t)*(hhfinal2$Ntoti2/hhfinal2$Ntott2))
        SegTable$IsoBLDGroup2_i<-sum(hhfinal2$IsoBLDGroup2_i, na.rm=T)
      
      ###### Segment Side #####
        Segside<-sumby(All[,c("german", "irish", "native_white", "Person")], All$segside)
        Segside<-rename(Segside, c("german"="g_side", "irish"="i_side", "native_white"="w_side", "Person"="t_side"))
        Segside<-merge(x=Segside, y=All[,c("segside", "german_t", "irish_t", "native_white_t")], by="segside", all.x=T)
        Segside<-subset(Segside, !duplicated(Segside$segside))
      
        #ISOLATION
        Segside$IssoSegside_g<-((Segside$g_side/Segside$german_t)*(Segside$g_side/Segside$t_side))
        SegTable$IsoSegside_g<-sum(Segside$IssoSegside_g)
        Segside$IssoSegside_i<-((Segside$i_side/Segside$irish_t)*(Segside$i_side/Segside$t_side))
        SegTable$IsoSegside_i<-sum(Segside$IssoSegside_i)
        
        #Dissimilarity
        Segside$DissimSegside_g<-abs((Segside$g_side/Segside$german_t)-(Segside$w_side/Segside$native_white_t))
        SegTable$DissimSegside_g<-0.5*(sum(Segside$DissimSegside_g))
        Segside$DissimSegside_i<-abs((Segside$i_side/Segside$irish_t)-(Segside$w_side/Segside$native_white_t))
        SegTable$DissimSegside_i<-0.5*(sum(Segside$DissimSegside_i))
          
      ##### Segment #####
        Segment<-sumby(All[,c("german", "irish", "native_white", "Person")], All$segment_id)
        Segment<-rename(Segment, c("german"="g_seg", "irish"="i_seg", "native_white"="w_seg", "Person"="t_seg"))
        Segment<-merge(x=Segment, y=All[,c("segment_id", "german_t", "irish_t", "native_white_t")], by="segment_id", all.x=TRUE)
        Segment<-subset(Segment, !duplicated(Segment$segment_id))
        
        #ISOLATION
        Segment$IssoSeg_g<-((Segment$g_seg/Segment$german_t)*(Segment$g_seg/Segment$t_seg))
        SegTable$IsoSeg_g<-sum(Segment$IssoSeg_g)
        Segment$IssoSeg_i<-((Segment$i_seg/Segment$irish_t)*(Segment$i_seg/Segment$t_seg))
        SegTable$IsoSeg_i<-sum(Segment$IssoSeg_i)
        
        #Dissimilarity
        Segment$Dissim_g<-abs((Segment$g_seg/Segment$german_t)-(Segment$w_seg/Segment$native_white_t))
        SegTable$Dissim_g<-0.5*sum(Segment$Dissim_g)
        Segment$Dissim_i<-abs((Segment$i_seg/Segment$irish_t)-(Segment$w_seg/Segment$native_white_t))
        SegTable$Dissim_i<-0.5*sum(Segment$Dissim_i)

  ##### Overlapping Segment Group - ISOLATION #####
      #Bring in Segment which is aggregated by segment and merge it to overlapping segment groups
        OverSeg<-merge(x=Segment, y=OverSeg_Join[,c("SegmentID", "SegGrID")], by.x="segment_id", by.y="SegmentID",all.x=TRUE)
      #Aggregate by Segment Group
        OverlappingSG<-sumby(OverSeg[,c("g_seg", "i_seg", "t_seg")], OverSeg$SegGrID)
        OverlappingSG<-rename(OverlappingSG, c("g_seg"="g_overseg", "i_seg"="i_overseg", "t_seg"="t_overseg", "SegGrID"="OverSegGr"))
        
      #Attach aggregated segment group information to OverSeg in order to do isolation calculations
        OverSeg<-merge(x=OverSeg, y=OverlappingSG, by.x="SegGrID", by.y="OverSegGr", all.x=TRUE)
      #Keep focal segments which are found if Seg_ID==SegGrID
        OverSeg<-OverSeg[which(OverSeg$SegGrID==OverSeg$segment_id),]

        #ISOLATION
        OverSeg$IssoOverSeg_g<-((OverSeg$g_seg/OverSeg$german_t)*(OverSeg$g_overseg/OverSeg$t_overseg))
        SegTable$IsoOverSeg_g<-sum(OverSeg$IssoOverSeg_g)
        OverSeg$IssoOverSeg_i<-((OverSeg$i_seg/OverSeg$irish_t)*(OverSeg$i_overseg/OverSeg$t_overseg))
        SegTable$IsoOverSeg_i<-sum(OverSeg$IssoOverSeg_i)
        
        ###### Dissimilarity Using Non-Overlapping Group IDs #########
        # Use "SegGroup_id", "ExtGroup_id" respectively
        SG<-sumby(All[,c("german", "irish", "native_white", "Person")], All$seggroup_id)
        SG<-rename(SG, c("german"="segg_g", "irish"="segg_i", "native_white"="segg_w", "Person"="segg_t"))
        SG<-merge(x=SG, y=All[,c("seggroup_id", "german_t", "irish_t", "native_white_t")], by="seggroup_id", all.x=TRUE)
        
        #Keep One Segment Group
        SG<-subset(SG, !duplicated(SG$seggroup_id))
        
        #DISSIMILARITY
          SG$DissimSegG_g<-abs((SG$segg_g/SG$german_t)-(SG$segg_w/SG$native_white_t))
          SegTable$DissimSegG_g<-0.5*(sum(SG$DissimSegG_g))
          SG$DissimSegG_i<-abs((SG$segg_i/SG$irish_t)-(SG$segg_w/SG$native_white_t))
          SegTable$DissimSegG_i<-0.5*(sum(SG$DissimSegG_i))
        
  ###### Overlapping Extended Segment Group - ISOLATION #####
      #Bring in Segment2 which is aggregated by segment and merge it to overlapping extended segment groups (Overlap2)
        ExtOverSeg<-merge(x=Segment, y=Overlap2[,c("SegmentID", "ExtSegGrID")], by.x="segment_id", by.y="SegmentID",all.x=TRUE)
      #Aggregate by Extended Segment Group
        g_overextseg<-tapply(ExtOverSeg$g_seg, INDEX=list(ExtOverSeg$ExtSegGrID), FUN=sum, na.rm=T)
        g_overextseg2<-data.frame(ExtOverSegGr=names(g_overextseg), g_overextseg=g_overextseg)
        i_overextseg<-tapply(ExtOverSeg$i_seg, INDEX=list(ExtOverSeg$ExtSegGrID), FUN=sum, na.rm=T)
        i_overextseg2<-data.frame(ExtOverSegGr=names(i_overextseg), i_overextseg=i_overextseg)
        t_overextseg<-tapply(ExtOverSeg$t_seg, INDEX=list(ExtOverSeg$ExtSegGrID), FUN=sum, na.rm=T)
        t_overextseg2<-data.frame(ExtOverSegGr=names(t_overextseg), t_overextseg=t_overextseg)

      #Merge aggregate data into one data frame (OverlappingExtSG)
        OverlappingExtSG<-merge(x=g_overextseg2, y=i_overextseg2, by="ExtOverSegGr", all.x=T)
        OverlappingExtSG<-merge(x=OverlappingExtSG, y=t_overextseg2, by="ExtOverSegGr", all.x=T)  
      #Attach aggregated segment group information to OverSeg in order to do isolation calculations
        ExtOverSeg<-merge(x=ExtOverSeg, y=OverlappingExtSG, by.x="ExtSegGrID", by.y="ExtOverSegGr", all.x=TRUE)
      #Keep focal segments which are found if Seg_ID==ExtSegGrID
        ExtOverSeg<-ExtOverSeg[which(ExtOverSeg$ExtSegGrID==ExtOverSeg$segment_id),]
        
        #ISOLATION
        ExtOverSeg$IssoOverSegEG_g<-((ExtOverSeg$g_seg/ExtOverSeg$german_t)*(ExtOverSeg$g_overextseg/ExtOverSeg$t_overextseg))
        SegTable$IsoOverEG_g<-sum(ExtOverSeg$IssoOverSegEG_g)
        ExtOverSeg$IssoOverSegEG_i<-((ExtOverSeg$i_seg/ExtOverSeg$irish_t)*(ExtOverSeg$i_overextseg/ExtOverSeg$t_overextseg))
        SegTable$IsoOverEG_i<-sum(ExtOverSeg$IssoOverSegEG_i)
        
      #Dissimilarity Using Non-Overlapping Extended Group ID
        ExSG<-sumby(All[,c("german","irish", "native_white", "Person")], All$extgroup_id)
        ExSG<-rename(ExSG, c("german"="exsegg_g", "irish"="exsegg_i", "native_white"="exsegg_w", "Person"="exsegg_t"))
        ExSG<-merge(x=ExSG, y=All[,c("extgroup_id", "german_t", "irish_t", "native_white_t")], by="extgroup_id", all.x=TRUE)
        ExSG<-subset(ExSG, !duplicated(ExSG$extgroup_id))
        
        #DISSIMILARITY
        ExSG$DissimExSegG_g<-abs((ExSG$exsegg_g/ExSG$german_t)-(ExSG$exsegg_w/ExSG$native_white_t))
        SegTable$DissimExSegG_g<-0.5*(sum(ExSG$DissimExSegG_g))
        ExSG$DissimExSegG_i<-abs((ExSG$exsegg_i/ExSG$irish_t)-(ExSG$exsegg_w/ExSG$native_white_t))
        SegTable$DissimExSegG_i<-0.5*(sum(ExSG$DissimExSegG_i))
        
      ##### ED #####
        ed<-sumby(All[,c("german", "irish", "native_white", "Person")], All$enumdist)
        ed<-rename(ed, c("german"="g_ed", "irish"="i_ed", "native_white"="w_ed", "Person"="t_ed"))
        ed<-merge(x=ed, y=All[,c("enumdist", "german_t", "irish_t", "native_white_t")], by="enumdist", all.x=TRUE)
        ed<-subset(ed, !duplicated(ed$enumdist))
        
        #Isolation
        ed$IsoED_g<-((ed$g_ed/ed$german_t)*(ed$g_ed/ed$t_ed))
        SegTable$IsoED_g<-sum(ed$IsoED_g)
        ed$IsoED_i<-((ed$i_ed/ed$irish_t)*(ed$i_ed/ed$t_ed))
        SegTable$IsoED_i<-sum(ed$IsoED_i)
        
        #Dissimilarity
        ed$DissimED_g<-abs((ed$g_ed/ed$german_t)-(ed$w_ed/ed$native_white_t))
        SegTable$DissimED_g<-0.5*sum(ed$DissimED_g)
        ed$DissimED_i<-abs((ed$i_ed/ed$irish_t)-(ed$w_ed/ed$native_white_t))
        SegTable$DissimED_i<-0.5*sum(ed$DissimED_i)
        
      #### City ####
        city<-sumby(All[,c("german", "irish", "native_white", "Person")], All$City)
        city<-rename(city, c("german"="g_city", "irish"="i_city", "native_white"="w_city", "Person"="t_city"))
        city<-merge(x=city, y=All[,c("City", "german_t", "irish_t", "native_white_t")], by="City", all.x=TRUE)
        city<-subset(city, !duplicated(city$City))
        
        #Isolation
        city$IsoCity_g<-((city$g_city/city$german_t)*(city$g_city/city$t_city))
        SegTable$IsoCity_g<-sum(city$IsoCity_g)
        city$IsoCity_i<-((city$i_city/city$irish_t)*(city$i_city/city$t_city))
        SegTable$IsoCity_i<-sum(city$IsoCity_i)
        
        #Dissimilarity
        city$DissimCity_g<-abs((city$g_city/city$german_t)-(city$w_city/city$native_white_t))
        SegTable$DissimCity_g<-0.5*sum(city$DissimCity_g)
        city$DissimCity_i<-abs((city$i_city/city$irish_t)-(city$w_city/city$native_white_t))
        SegTable$DissimCity_i<-0.5*sum(city$DissimCity_i)
        
        SegTable<-SegTable[,c(1,10,11,
                              2,6,12,16,18,20,24,28,32,36,40,
                              3,7,13,17,19,21,25,29,33,37,41,
                              4,8,14,22,26,30,34,38,42,
                              5,9,15,23,27,31,35,39,43)]
        
        AllSegTable<-rbind(AllSegTable, SegTable)
        
        print(paste("Finished ", Cityname[i], sep=""))
        
  }, error=function(e){cat(conditionMessage(e))})
}

  AllSegTable<-rename(AllSegTable, c("City"="City","PerMulti_H"="Percent Households in Multiunit Building","PerMulti_B"="Percent Buildings That Are MultUnit", 
                                     "IsoHouse_g"="Household Isolation (German)","IsoBLD_g"="Building Isolation (German)",
                                     "IsoMultiBLD_g"="Multi-Building Isolation (German)","IsoBLDGroup_g"="Building Group Isolation (German)",
                                     "IsoBLDGroup2_g"="Building Group Isolation - Sans Own Building (German)","IsoSegside_g"="Segment Side Isolation (German)",
                                     "IsoSeg_g"="Segment Isolation (German)", "IsoOverSeg_g"="Overlapping Segment Group (German)", 
                                     "IsoOverEG_g"="Overlapping Extended Segment Group (German)", "IsoED_g"="ED Isolation (German)", 
                                     "IsoCity_g"="Citywide Isolation (German)",
                                     "IsoHouse_i"="Household Isolation (Irish)","IsoBLD_i"="Building Isolation (Irish)", 
                                     "IsoMultiBLD_i"="Multi-Building Isolation (Irish)","IsoBLDGroup_i"="Building Group Isolation (Irish)",
                                     "IsoBLDGroup2_i"="Building Group Isolation - Sans Own Building (Irish)","IsoSegside_i"="Segment Side Isolation (Irish)",
                                     "IsoSeg_i"="Segment Isolation (Irish)", "IsoOverSeg_i"="Overlapping Segment Group (Irish)",
                                     "IsoOverEG_i"="Overlapping Extended Segment Group (Irish)","IsoED_i"="ED Isolation (Irish)",
                                     "IsoCity_i"="Citywide Isolation (Irish)",
                                    "DissimHouse_g"="Household Dissimilarity (German)", "DissimBLD_g"="Building Dissimilarity (German)",
                                    "DissimMultiBLD_g"="Multi-Building Dissimilarity (German)", "DissimSegside_g"="Segment Side Dissimilarity (German)",
                                    "Dissim_g"="Segment Dissimilarity (German)", "DissimSegG_g"="Non-Overlapping Segment Group Dissimilarity (German)",
                                    "DissimExSegG_g"="Non-Overlapping Extended Segment Group Dissimilarity (German)", "DissimED_g"="ED Dissimilarity (German)",
                                    "DissimCity_g"="Citywide Dissimilarity (German)",
                                    "DissimHouse_i"="Household Dissimilarity (Irish)", "DissimBLD_i"="Building Dissimilarity (Irish)",
                                    "DissimMultiBLD_i"="Multi-Building Dissimilarity (Irish)", "DissimSegside_i"="Segment Side Dissimilarity (Irish)",
                                    "Dissim_i"="Segment Dissimilarity (Irish)", "DissimSegG_i"="Non-Overlapping Segment Group Dissimilarity (Irish)",
                                    "DissimExSegG_i"="Non-Overlapping Extended Segment Group Dissimilarity (Irish)", "DissimED_i"="ED Dissimilarity (Irish)",
                                    "DissimCity_i"="Citywide Dissimilarity (Irish)"))
  
  AllSeg_New<-AllSegTable
  
  write.csv(AllSegTable, "C:/Users/mmarti24/Dropbox/Papers/SIS - Early Arriving/Segregation_Results.csv")
  write.xlsx(AllSegTable, file="C:/Users/mmarti24/Dropbox/Papers/SIS - Early Arriving/Segregation_Results.xlsx")
  
  #Bring in Nate's 1880 Data
  
  n_1880<-read.delim("Z:/Users/Matt/Segregation Project_Irish and Germans/revised_1880_extract.txt", sep = "|")
  