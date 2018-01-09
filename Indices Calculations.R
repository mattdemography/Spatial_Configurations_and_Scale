library(readstata13)
library(gmodels)
library(foreign)
library(car)
library(plyr)
library(seg)
#Cities
  #Atlanta 
  LongName<-"Atlanta"
  Shortname<-"Atlanta"
  Cityname<-"Atlanta"
  Ex<-read.dbf("Z:/Users/_Exchange/1880 Stuff/AllCities/Atlanta/Atlanta_point_seggr.dbf")
  Seg<-read.dbf("Z:/Users/_Exchange/1880 Stuff/AllCities/Atlanta/Atlanta_point_seggr.dbf")
  SG<-read.dbf("Z:/Users/_Exchange/1880 Stuff/AllCities/Atlanta/Atlanta_point_seggr.dbf")
  Pts<-read.dta13(paste("Z:/Projects/1880Data/1880Microdata/1880MicrodataSouthernCities_LatestAnalysesFiles/",Shortname,"_Jan23.dta", sep=""))
  hex150<-"hexid150"
  hex225<-"hexid225"
  Seg_ID<-"segid"
  
  #Baltimore  
  LongName<-"Baltimore"
  Shortname<-"Baltimore"
  Cityname<-"BAL"
  Seg<-read.dbf("Z:/Users/_Exchange/1880 Stuff/AllCities/Baltimore/BAL_point_seggr.dbf")
  SG<-read.dbf("Z:/Users/_Exchange/1880 Stuff/AllCities/Baltimore/BAL_point_seggr.dbf")
  Ex<-read.dbf("Z:/Users/_Exchange/1880 Stuff/AllCities/Baltimore/BAL_point_seggr.dbf")
  Pts<-read.dta13(paste("Z:/Projects/1880Data/1880Microdata/1880MicrodataSouthernCities_LatestAnalysesFiles/",Shortname,"_Jan23.dta", sep=""))
  hex150<-"hexid150"
  hex225<-"hexid225"
  Seg_ID<-"segid"
  serial<-"serial"
  Seg$serial<-Seg$Serial
  SG$serial<-SG$Serial
  Ex$serial<-Ex$Serial
  
  #Charleston
  LongName<-"Charleston"
  Shortname<-"Charleston"
  Cityname<-"Charleston"
  Seg<-read.dbf("Z:/Users/_Exchange/1880 Stuff/AllCities/Charleston/Charlestonst_point_seggr.dbf")
  SG<-read.dbf("Z:/Users/_Exchange/1880 Stuff/AllCities/Charleston/Charlestonst_point_seggr.dbf")
  Ex<-read.dbf("Z:/Users/_Exchange/1880 Stuff/AllCities/Charleston/Charlestonst_point_seggr.dbf")
  Pts<-read.dta13(paste("Z:/Projects/1880Data/1880Microdata/1880MicrodataSouthernCities_LatestAnalysesFiles/",Shortname,"_Jan23.dta", sep=""))
  hex150<-"hexid150"
  hex225<-"hexid225"
  Seg_ID<-"segid"
  serial<-"serial"
  Seg$serial<-Seg$Serial
  SG$serial<-SG$Serial
  Ex$serial<-Ex$Serial
  
  #Louisville  
  LongName<-"Louisville"
  Shortname<-"Louisville"
  Cityname<-"Louisville"
  hex150<-"hexid150"
  hex225<-"hexid225"
  Seg_ID<-"Seg_ID"
  
  #Memphis
  LongName<-"memphis"
  Shortname<-"memphis"
  Cityname<-"memphis" 
  hex150<-"hexid150"
  hex225<-"hexid225"
  Seg_ID<-"Seg_ID"
  
  #Mobile 
  LongName<-"Mobile"
  Shortname<-"Mobile"
  Cityname<-"Mobile"
  hex150<-"hexid150"
  hex225<-"hex225"
  Seg_ID<-"Seg_ID"
  Seg<-read.dbf(paste("Z:/Users/_Exchange/1880 Stuff/AllCities/",LongName,"/",Cityname,"_Points.dbf", sep=""))
  SG<-read.dbf(paste("Z:/Users/_Exchange/1880 Stuff/AllCities/",LongName,"/",Cityname,"_Points.dbf", sep=""))
  Ex<-read.dbf(paste("Z:/Users/_Exchange/1880 Stuff/AllCities/",LongName,"/",Cityname,"_join225_Points.dbf", sep=""))
  Pts<-read.dta13(paste("Z:/Projects/1880Data/1880Microdata/1880MicrodataSouthernCities_LatestAnalysesFiles/",Shortname,"_Jan23.dta", sep=""))
  
  #Nashville  
  LongName<-"Nashville"
  Shortname<-"Nashville"
  Cityname<-"Nashville"
  hex150<-"hex150"
  hex225<-"hex225"
  Seg_ID<-"Seg_ID"

  #New Orleans  
  LongName<-"New Orleans"
  Shortname<-"NewOrleans"
  Cityname<-"NOLA"
  hex150<-"hexid150"
  hex225<-"hex225"
  Seg_ID<-"Seg_ID"
  
  #Richmond
  LongName<-"Richmond"
  Shortname<-"Richmond"
  Cityname<-"Richmond"
  hex150<-"hexid150"
  hex225<-"hex225"
  Seg_ID<-"Seg_ID"
  Seg<-read.dbf(paste("Z:/Users/_Exchange/1880 Stuff/AllCities/",LongName,"/",Cityname,"_Segment_Points.dbf", sep=""))
  SG<-read.dbf(paste("Z:/Users/_Exchange/1880 Stuff/AllCities/",LongName,"/",Cityname,"_Segment_Points2.dbf", sep=""))
  Ex<-read.dbf(paste("Z:/Users/_Exchange/1880 Stuff/AllCities/",LongName,"/",Cityname,"_join225_Points.dbf", sep=""))
  Pts<-read.dta13(paste("Z:/Projects/1880Data/1880Microdata/1880MicrodataSouthernCities_LatestAnalysesFiles/",Shortname,"_Jan23.dta", sep=""))
  
  #Washington
  LongName<-"Washington"
  Shortname<-"WashingtonDC"
  Cityname<-"Washington"
  hex150<-"hexid150"
  hex225<-"hex225"
  Seg_ID<-"Seg_ID"
  Seg<-data.frame(read.dbf(paste("Z:/Users/_Exchange/1880 Stuff/AllCities/",LongName,"/",Cityname,"_join225_Points.dbf", sep="")))
  SG<-data.frame(read.dbf(paste("Z:/Users/_Exchange/1880 Stuff/AllCities/",LongName,"/",Cityname,"_Points.dbf", sep="")))
  Ex<-data.frame(read.dbf(paste("Z:/Users/_Exchange/1880 Stuff/AllCities/",LongName,"/",Cityname,"_join225_Points.dbf", sep="")))
  Pts<-data.frame(read.dta13(paste("Z:/Projects/1880Data/1880Microdata/1880MicrodataSouthernCities_LatestAnalysesFiles/",Shortname,"_Jan23.dta", sep="")))
  
    Seg<-read.dbf(paste("Z:/Users/_Exchange/1880 Stuff/AllCities/",LongName,"/",Cityname,"_Segment_Points.dbf", sep=""))
    SG<-read.dbf(paste("Z:/Users/_Exchange/1880 Stuff/AllCities/",LongName,"/",Cityname,"_join150_Points.dbf", sep=""))
    Ex<-read.dbf(paste("Z:/Users/_Exchange/1880 Stuff/AllCities/",LongName,"/",Cityname,"_join225_Points.dbf", sep=""))
    Pts<-read.dta13(paste("Z:/Projects/1880Data/1880Microdata/1880MicrodataSouthernCities_LatestAnalysesFiles/",Shortname,"_Jan23.dta", sep=""))
    
    merge1<-merge(x=SG[,c(hex150,"serial")],y=Ex[,c(hex225,"serial")], by="serial", all.x=TRUE)
    merge2<-merge(x=merge1,y=Seg[,c(Seg_ID, "serial")], by="serial", all.x=TRUE)
    merge2<-subset(merge2, !duplicated(merge2$serial))
    merge2$serial<-as.numeric(levels(merge2$serial))[merge2$serial]
    merge3<-merge(x=Pts[,c("race","relate","age","occ50us","labforce","serial")],y=merge2, by="serial", all.x=TRUE)
    merge3<-merge3[complete.cases(merge3[Seg_ID]),]
    merge3$Person<-recode(merge3$serial,"\" \"=0; else=1")
    merge3<-subset(merge3, (merge3$hexid150!=0))
    
    #Create Frequency Tables
    attach(merge3)
    mytable<-table(serial, race,relate)
    detach(merge3)
    margin.table(mytable,2)
    margin.table(mytable,3)
    
    attach(merge3)
    mytable2<-table(serial,occ50us, labforce)
    detach(merge3)
    margin.table(mytable2,2)
    margin.table(mytable2,3)
    
    #Create Dummy of 18 Plus
    merge3$Above18<-recode(merge3$age,"c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,NA)=0; else=1")
    #Create Head of Household Variable
    merge3$hh<-recode(merge3$relate,"c(101)=1; else=0")
    #Unrelated Individuals
    merge3$UnRelate<-recode(merge3$relate,"c(1200,1202,1203,1204,1205,1206,1221,1222,1223,1230,1239)=1; else=0")
    #Servants
    merge3$Serve<-recode(merge3$relate,"1210:1219=1; else=0")
    merge3$Serve2<-recode(merge3$occ50us, "700:720=1; else=0")
    #Unrelated and Over 18
    merge3$UnRelate18<-ifelse(merge3$UnRelate==1 & merge3$Above18==1,1,0)
    
    attach(merge3)
    mytable3<-table(serial, UnRelate, Serve, Serve2, UnRelate18)
    detach(merge3)
    margin.table(mytable3,2)
    margin.table(mytable3,3)
    margin.table(mytable3,4)
    CrossTable(merge3$UnRelate, merge3$Serve)
    CrossTable(merge3$UnRelate, merge3$Above18)
  
    #Whites
      merge3$whites<-recode(merge3$race,"100=1; else=0")
      merge3$WHH<-ifelse(merge3$whites==1 & merge3$hh==1,1,0)
    #Blacks 
      merge3$blacks<-recode(merge3$race,"c(200,210)=1; else=0")
      merge3$BHH<-ifelse(merge3$blacks==1 & merge3$hh==1,1,0)
    #In Laboor Force - 2 is the Code for being in the labor force
      merge3$labforce<-recode(merge3$labforce,"9=0; else=merge3$labforce")
    
    attach(merge3)
    mytable4<-table(serial, whites, blacks, WHH, BHH)
    detach(merge3)
    margin.table(mytable4,2)
    margin.table(mytable4,3)
    margin.table(mytable4,4)
    margin.table(mytable4,5)
    CrossTable(merge3$whites,merge3$hh)
    CrossTable(merge3$blacks,merge3$hh)
    
    #Black Servents
    merge3$bserve<-ifelse(merge3$blacks==1 & merge3$Serve==1,1,0)
    #Black Servents in White Households
    merge3$bserveWhite<-ifelse(merge3$bserve==1 & merge3$hh==0,1,0)
    
    attach(merge3)
    mytable5<-table(serial, bserve, bserveWhite)
    detach(merge3)
    margin.table(mytable5,2)
    margin.table(mytable5, 3)
    CrossTable(merge3$blacks, merge3$Serve)
    
    CrossTable(merge3$Serve, merge3$Serve2)
    CrossTable(merge3$Above18, merge3$Serve)
    CrossTable(merge3$occ50us, merge3$Serve)
    
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
    
  #Segments by Race - Pop Total
    bls<-tapply(merge3$blacks, INDEX=list(eval(parse(text=paste("merge3$",Seg_ID,sep="")))), FUN=sum)
    blseg<-data.frame(Seg_ID=names(bls), blseg=bls)
    whs<-tapply(merge3$whites, INDEX=list(eval(parse(text=paste("merge3$",Seg_ID,sep="")))), FUN=sum)
    whseg<-data.frame(Seg_ID=names(whs), whseg=whs)
    tots<-tapply(merge3$Person, INDEX=list(eval(parse(text=paste("merge3$",Seg_ID,sep="")))), FUN=sum)
    totseg<-data.frame(Seg_ID=names(tots), totseg=tots)
    
    Segment<-merge(x=blseg, y=whseg, by="Seg_ID", all.x=TRUE)
    Segment2<-merge(x=Segment, y=totseg, by="Seg_ID", all.x=TRUE)
    Segment2$Person<-recode(Segment2$Seg_ID,"\" \"=0; else=1")
    Segment2<-merge(y=Pop2, x=Segment2, by="Person", all.x=T)

    #DISSIMILARITY
    Segment2$DissimSeg<-abs((Segment2$blseg/Segment2$blktot)-(Segment2$whseg/Segment2$whtot))
    DissmSeg<-0.5*(sum(Segment2$DissimSeg))
 
    #ISOLATION
    Segment2$IssoSeg<-((Segment2$blseg/Segment2$blktot)*(Segment2$blseg/Segment2$totseg))
    IssoSeg<-sum(Segment2$IssoSeg)
    
    #Seg Package
    myvars<-c("blseg", "whseg")
    SegmentTest<-Segment2[myvars]
    dissim(data=SegmentTest)
    
  #Segment Group by Race - Pop Total
    blsg<-tapply(merge3$blacks, INDEX=list(eval(parse(text=paste("merge3$",hex150,sep="")))), FUN=sum)
    blsegG<-data.frame(hex150=names(blsg), blsegG=blsg)
    whsg<-tapply(merge3$whites, INDEX=list(eval(parse(text=paste("merge3$",hex150,sep="")))), FUN=sum)
    whsegG<-data.frame(hex150=names(whsg), whsegG=whsg)
    totsg<-tapply(merge3$Person, INDEX=list(eval(parse(text=paste("merge3$",hex150,sep="")))), FUN=sum)
    totsegG<-data.frame(hex150=names(totsg), totsegGroup=totsg)
    
    SegmentGroup<-merge(x=blsegG, y=whsegG, by="hex150", all.x=TRUE)
    SegmentGroup2<-merge(x=SegmentGroup, y=totsegG, by="hex150", all.x=TRUE)
    SegmentGroup2$Person<-recode(SegmentGroup2$hex150,"\" \"=0; else=1")
    SegmentGroup2<-merge(y=Pop2, x=SegmentGroup2, by="Person", all.x=T)
    
    #DISSIMILARITY
    SegmentGroup2$DissimSegG<-abs((SegmentGroup2$blsegG/SegmentGroup2$blktot)-(SegmentGroup2$whsegG/SegmentGroup2$whtot))
    DissmSegG<-0.5*(sum(SegmentGroup2$DissimSegG))
    
    #Seg Package
    myvars<-c("blsegG", "whsegG")
    SegmentTest2<-SegmentGroup2[myvars]
    dissim(data=SegmentTest2)
    
    #ISOLATION
    SegmentGroup2$IssoSegG<-((SegmentGroup2$blsegG/SegmentGroup2$blktot)*(SegmentGroup2$blsegG/SegmentGroup2$totsegG))
    IssoSegG<-sum(SegmentGroup2$IssoSegG)
    
  #Extended Segment Group by Race - Pop Total
    blsgE<-tapply(merge3$blacks, INDEX=list(eval(parse(text=paste("merge3$",hex225,sep="")))), FUN=sum)
    blsegEG<-data.frame(hex225=names(blsgE), blsegEG=blsgE)
    whsgE<-tapply(merge3$whites, INDEX=list(eval(parse(text=paste("merge3$",hex225,sep="")))), FUN=sum)
    whsegEG<-data.frame(hex225=names(whsgE), whsegEG=whsgE)
    totsgE<-tapply(merge3$Person, INDEX=list(eval(parse(text=paste("merge3$",hex225,sep="")))), FUN=sum)
    totsegEG<-data.frame(hex225=names(totsgE), totsegEG=totsgE)
    
    EXSegmentGroup<-merge(x=blsegEG, y=whsegEG, by="hex225", all.x=TRUE)
    EXSegmentGroup2<-merge(x=EXSegmentGroup, y=totsegEG, by="hex225", all.x=TRUE)
    EXSegmentGroup2$Person<-recode(EXSegmentGroup2$hex225,"\" \"=0; else=1")
    EXSegmentGroup2<-merge(y=Pop2, x=EXSegmentGroup2, by="Person", all.x=T)
    
    #DISSIMILARITY
    EXSegmentGroup2$DissimSegEG<-abs((EXSegmentGroup2$blsegEG/EXSegmentGroup2$blktot)-(EXSegmentGroup2$whsegEG/EXSegmentGroup2$whtot))
    DissmSegEG<-0.5*(sum(EXSegmentGroup2$DissimSegEG))
    
    #Seg Package
    myvars<-c("blsegEG", "whsegEG")
    SegmentTest3<-EXSegmentGroup2[myvars]
    dissim(data=SegmentTest3)
    
    #ISOLATION
    EXSegmentGroup2$IssoSegEG<-((EXSegmentGroup2$blsegEG/EXSegmentGroup2$blktot)*(EXSegmentGroup2$blsegEG/EXSegmentGroup2$totsegEG))
    IssoSegEG<-sum(EXSegmentGroup2$IssoSegEG)
    
 ###### Merge Calculations into Microdata ######
    firstmerge<-merge(x=merge3, y=Pop2, by="Person", all.x=TRUE)
    secondmerge<-merge(x=firstmerge, y=Segment2, by="Seg_ID", all.x=TRUE)
    thirdmerge<-merge(x=SegmentGroup2, y=secondmerge, by="hex150", all.x=TRUE)
    final<-merge(x=thirdmerge, y=EXSegmentGroup2, by="hex225", all.x=TRUE)
 