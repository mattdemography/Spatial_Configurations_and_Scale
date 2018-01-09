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

citylist<-read.csv(("Z:/Projects/Preparing 1880 Files/City Lists.csv"))
citylist <- data.frame(lapply(citylist, as.character), stringsAsFactors=FALSE)

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

##### This will create points to be geocoded #####
for(i in 1:40){
  tryCatch({
    Segment<-read.dta13(paste("Z:/Projects/Preparing 1880 Files/", Cityname[i],"/Match Address/", Cityname[i],".dta", sep=""))
    Points<-read.dta13(paste("Z:/Projects/1880Data/MicroData For Publication/", Cityname[i],".dta",sep=""))
    
    #Fix Names in Segment File
      names(Segment)<-tolower(names(Segment))
      Segment$serial<-as.character(Segment$serial)
      Segment<-subset(Segment, !duplicated(Segment$serial))
      ifelse(exists("street.1", where=Segment), (Segment$street_stnum=Segment$street.1), (Segment$street_stnum=Segment$street))
    
    #Fix names in Points file
      names(Points)<-tolower(names(Points))
      ifelse(exists("bpldet", where=Points), (Points$bpldet=Points$bpldet), (Points$bpldet=Points$bp))
      ifelse(exists("fbpldtus", where=Points), (Points$fbpldtus=Points$fbpldtus), (Points$fbpldtus=Points$fbp))
      ifelse(exists("mbpldtus", where=Points), (Points$mbpldtus=Points$mbpldtus), (Points$mbpldtus=Points$mbp))
      Points$serial<-as.character(Points$serial)
      Points$bpldet<-as.character(Points$bpldet)
      Points$fbpldtus<-as.character(Points$fbpldtus)
      Points$mbpldtus<-as.character(Points$mbpldtus)
    
    All<-merge(Points[,c("bpldet", "fbpldtus", "mbpldtus","race","relate","age","occ50us","labforce","serial","enumdist", "statfpus")],
               Segment[,c("serial", "side", "segment_id", "building_id", "house_number", "street_stnum")], by="serial")
    
    #Trim leading zeros to match with OffStreet File
      All$serial<-as.numeric(All$serial)
    #Create Addresses to Geocode
      All$Address<-paste(All$house_number, " ", All$street_stnum, sep="")
    #Tag Unique Sides and Segments
      All$segside<-id(All[c("segment_id","side")], drop=T)
    #Create City Name Field
      All$City<-Cityname[i]
      All$City<-ifelse(All$City=="St_Louis", "St. Louis", All$City)
      All$City<-ifelse(All$City=="St_Paul", "St. Paul", All$City)
    #Create State Field
      state<-read.csv("Z:/Projects/Preparing 1880 Files/State_Codes.csv", stringsAsFactors = F)
      All<-merge(All, state[,c("Code", "Label")], by.x="statfpus", by.y="Code", all.x=T)
      All<-rename(All, c("Label"="State"))
    #Create Person Counter for Aggregation  
      All$Person<-recode(All$serial,"\" \"=0; else=1")
      All<-subset(All, All$segment_id!=0)
    #Create Dummy of 18 Plus
      All$Above18<-recode(All$age,"c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,NA)=0; else=1")
    #Create Head of Household Variable
      All$hh<-recode(All$relate,"c(101)=1; else=0")
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
      All$native_p<-ifelse(All$mbpldtus<=9900 | All$fbpldtus<=9900, 1, 0)
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
      All$german<-ifelse(All$german_f==1 | All$german_s==1, 1, 0)
    #Irish (_f=first generation; _s=second generation; _t=total)
    #Disregard any person who has a mother that is German
      All$irish_f<-ifelse((All$bpldet==41400 | All$bpldet==41410),1,0)
      All$irish_s<-ifelse(All$native_b==1 & ((All$mbpldtus==41400 | All$mbpldtus==41410) |
                                             (All$fbpldtus==41400 | All$fbpldtus==41410)) &
                          (All$fbpldtus!=45300 | All$fbpldtus!=45362),1,0)
      All$irish<-ifelse(All$irish_f==1 | All$irish_s==1, 1, 0)
    #Keep only adults      
      All<-All[which(All$Above18==1),]
    
    #Create Categorical Variable for Marking Irish, German, and Other On the Map
      All$people<-ifelse(All$irish==1, "Irish", "Other")
      All$people<-ifelse(All$people=="Other" & All$german==1, "German", All$people)
      All$people<-ifelse(All$people=="Other" & All$native_white==1, "Native Born/Parent - White", All$people)
      All$people<-ifelse(All$people=="Other" & All$race=="black", "Black", All$people)
      
      #Create new dummy sequence based on All$people category
      All$german_new<-ifelse(All$people=="German", 1, 0)
      All$irish_new<-ifelse(All$people=="Irish", 1, 0)
      All$native_white_new<-ifelse(All$people=="Native Born/Parent - White", 1, 0)
      All$black_new<-ifelse(All$people=="Black", 1, 0)
      All$other_new<-ifelse(All$people=="Other", 1, 0)
      
    #Mark if multi-unit Building
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
      
      #Mark MultiUnit Households
        All$multi<-ifelse(All$hb_Max==All$Address_Max, 0, 1)
      
      #Count How many Households Make Up Multiunit
        num_multi<-sumby(All[,c("hh", "hh")], All$Address)
        num_multi<-num_multi[,1:2]
        num_multi<-rename(num_multi, c("hh"="num_multi"))
        All<-merge(x=All, y=num_multi, by="Address", all.x=T)
      
    #Calculate Segment Percent, Irish, German, and Native White  
      bld<-sumby(All[,c("german_new", "irish_new", "native_white_new", "Person")], All$Address)
      bld<-rename(bld, c("german_new"="bld_g", "irish_new"="bld_i", "native_white_new"="bld_w", "Person"="bld_t"))
      bld$bld_g_per<-round((bld$bld_g/bld$bld_t)*100, digits = 2)
      bld$bld_i_per<-round((bld$bld_i/bld$bld_t)*100, digits = 2)
      bld$bld_w_per<-round((bld$bld_w/bld$bld_t)*100, digits = 2)
      All<-merge(x=All, y=bld, by="Address", all.x=T)
        
    #Calculate Segment Percent, Irish, German, and Native White  
      seg<-sumby(All[,c("german_new", "irish_new", "native_white_new", "Person")], All$segment_id)
      seg<-rename(seg, c("german_new"="seg_g", "irish_new"="seg_i", "native_white_new"="seg_w", "Person"="seg_t"))
      seg$seg_g_per<-round((seg$seg_g/seg$seg_t)*100, digits = 2)
      seg$seg_i_per<-round((seg$seg_i/seg$seg_t)*100, digits = 2)
      seg$seg_w_per<-round((seg$seg_w/seg$seg_t)*100, digits = 2)
      All<-merge(x=All, y=seg, by="segment_id", all.x=T)
      
    #Calculate ED Percent Irish, German, and Native White
      ed<-sumby(All[,c("german_new", "irish_new", "native_white_new", "Person")], All$enumdist)
      ed<-rename(ed, c("german_new"="ed_g", "irish_new"="ed_i", "native_white_new"="ed_w", "Person"="ed_t"))
      ed$ed_g_per<-round((ed$ed_g/ed$ed_t)*100, digits=2)
      ed$ed_i_per<-round((ed$ed_i/ed$ed_t)*100, digits=2)
      ed$ed_w_per<-round((ed$ed_w/ed$ed_t)*100, digits=2)
      All<-merge(x=All, y=ed, by="enumdist", all.x=T)
      
    #Calculate City Percent Irish, German, and Native White
      city<-sumby(All[,c("german_new", "irish_new", "native_white_new", "Person")], All$City)
      city<-rename(city, c("german_new"="city_g", "irish_new"="city_i", "native_white_new"="city_w", "Person"="city_t"))
      city$city_g_per<-round((city$city_g/city$city_t)*100, digits=2)
      city$city_i_per<-round((city$city_i/city$city_t)*100, digits=2)
      city$city_w_per<-round((city$city_w/city$city_t)*100, digits=2)
      All<-merge(x=All, y=city, by="City", all.x=T)
        
    #Keep only Household
      All<-subset(All, (All$hh==1))
    #Write Out Points File to Be Geocoded
      write.csv(All, paste("Z:/Users/Matt/Segregation Project_Irish and Germans/Household Data/",Cityname[i],"_HH.csv", sep=""))
    #Write Out Only Single Unit Buildings
      All_Single<-subset(All, (All$multi==0))
      write.csv(All_Single, paste("Z:/Users/Matt/Segregation Project_Irish and Germans/Household Data/",Cityname[i],"_HH_Single.csv", sep=""))
    #Write Out Only Multi-Unit Buildings
      All_Multi<-subset(All, (All$multi==1))
      write.csv(All_Multi, paste("Z:/Users/Matt/Segregation Project_Irish and Germans/Household Data/",Cityname[i],"_HH_Multi.csv", sep=""))
      
  #Bring in Segment file to attach percent segment information
    library(shapefiles)
    setwd(paste("Z:/Projects/Preparing 1880 Files/", Cityname[i],"/Street Grid Without Ranges/",sep=""))
    Grid<-readOGR(dsn=getwd(), layer=paste(Cityname[i],"_StreetGrid", sep=""))
    #Must detach package after this line because package shapefiles affects how read.dbf works
    detach("package:shapefiles", unload=TRUE)
    
    #Keep only unique segment ids
      All_grid<-subset(All, !duplicated(All$segment_id))
    #Mege Files
      Grid<-merge(x=Grid, y=All_grid[,c("enumdist", "segment_id", "City", "seg_g", "seg_i", "seg_w", "seg_t",
                                        "seg_g_per", "seg_i_per", "seg_w_per")], 
                by.x="Seg_id", by.y="segment_id", all.x=T)
      Grid$seg_g<-as.numeric(Grid$seg_g)
      Grid$seg_g_per<-as.numeric(Grid$seg_g_per)
      Grid$seg_i<-as.numeric(Grid$seg_i)
      Grid$seg_i_per<-as.numeric(Grid$seg_i_per)
      Grid$seg_w<-as.numeric(Grid$seg_w)
      Grid$seg_w_per<-as.numeric(Grid$seg_w_per)
      Grid$seg_t<-as.numeric(Grid$seg_t)
    
  #Write Out New Grid with segment calculations
    library(shapefiles)
    setwd(paste("Z:/Users/Matt/Segregation Project_Irish and Germans/Shapefiles/",sep=""))
    writeOGR(Grid, dsn=getwd(), paste(Cityname[i],"_GridCalc",sep=""), check_exists = TRUE,
             overwrite_layer = TRUE, driver="ESRI Shapefile")
    detach("package:shapefiles", unload=TRUE)
    
  #Bring in ED File to attach percent ed information
    library(shapefiles)
    setwd(paste("Z:/Projects/Preparing 1880 Files/", Cityname[i],"/ED/",sep=""))
    ED<-readOGR(dsn=getwd(), layer=paste(Cityname[i],"_ED", sep=""))
    #Must detach package after this line because package shapefiles affects how read.dbf works
    detach("package:shapefiles", unload=TRUE)
    
    All_ed<-subset(All, !duplicated(All$enumdist))
    All_ed$enumdist<-as.numeric(All_ed$enumdist)
  #Merge Files
    ED<-merge(x=ED, y=All_ed[,c("enumdist", "City", "ed_g", "ed_i", "ed_w", "ed_t",
                                "ed_g_per", "ed_i_per", "ed_w_per")],
              by.x="ED", by.y="enumdist", all.x=T)
    ED$ed_g<-as.numeric(ED$ed_g)
    ED$ed_g_per<-as.numeric(ED$ed_g_per)
    ED$ed_i<-as.numeric(ED$ed_i)
    ED$ed_i_per<-as.numeric(ED$ed_i_per)
    ED$ed_w<-as.numeric(ED$ed_w)
    ED$ed_w_per<-as.numeric(ED$ed_w_per)
    ED$ed_t<-as.numeric(ED$ed_t)
    
    #Write Out New ED with ED calculations
    library(shapefiles)
    setwd(paste("Z:/Users/Matt/Segregation Project_Irish and Germans/Shapefiles/",sep=""))
    writeOGR(ED, dsn=getwd(), paste(Cityname[i],"_EDCalc",sep=""), check_exists = TRUE,
             overwrite_layer = TRUE, driver="ESRI Shapefile")
    detach("package:shapefiles", unload=TRUE)
    
    print(paste("Finished", Cityname[i], sep=""))
    
  }, error=function(e){cat(conditionMessage(e))})
}
