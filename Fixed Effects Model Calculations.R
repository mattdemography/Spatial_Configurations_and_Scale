library(readstata13)
library(gmodels)
library(foreign)
library(car)
library(plyr)
library(seg)
library(Hmisc)
RegSeg<-data.frame()

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

#Louisville  
LongName<-"Louisville"
Shortname<-"Louisville"
Cityname<-"Louisville"
hex150<-"hexid150"
hex225<-"hexid225"
Seg_ID<-"Seg_ID"
Seg<-read.dbf(paste("Z:/Users/_Exchange/1880 Stuff/AllCities/",LongName,"/",Cityname,"_Segment_Points.dbf", sep=""))
SG<-read.dbf(paste("Z:/Users/_Exchange/1880 Stuff/AllCities/",LongName,"/",Cityname,"_join150_Points.dbf", sep=""))
Ex<-read.dbf(paste("Z:/Users/_Exchange/1880 Stuff/AllCities/",LongName,"/",Cityname,"_join225_Points.dbf", sep=""))
Pts<-read.dta13(paste("Z:/Projects/1880Data/1880Microdata/1880MicrodataSouthernCities_LatestAnalysesFiles/",Shortname,"_Jan23.dta", sep=""))
Seg_ID<-"Seg_ID"

#Memphis
LongName<-"memphis"
Shortname<-"memphis"
Cityname<-"memphis" 
hex150<-"hexid150"
hex225<-"hexid225"
Seg_ID<-"Seg_ID"
Seg<-read.dbf(paste("Z:/Users/_Exchange/1880 Stuff/AllCities/",LongName,"/",Cityname,"_Segment_Points.dbf", sep=""))
SG<-read.dbf(paste("Z:/Users/_Exchange/1880 Stuff/AllCities/",LongName,"/",Cityname,"_join150_Points.dbf", sep=""))
Ex<-read.dbf(paste("Z:/Users/_Exchange/1880 Stuff/AllCities/",LongName,"/",Cityname,"_join225_Points.dbf", sep=""))
Pts<-read.dta13(paste("Z:/Projects/1880Data/1880Microdata/1880MicrodataSouthernCities_LatestAnalysesFiles/",Shortname,"_Jan23.dta", sep=""))

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
Seg<-read.dbf(paste("Z:/Users/_Exchange/1880 Stuff/AllCities/",LongName,"/",Cityname,"_Segment_Points.dbf", sep=""))
SG<-read.dbf(paste("Z:/Users/_Exchange/1880 Stuff/AllCities/",LongName,"/",Cityname,"_join150_Points.dbf", sep=""))
Ex<-read.dbf(paste("Z:/Users/_Exchange/1880 Stuff/AllCities/",LongName,"/",Cityname,"_join225_Points.dbf", sep=""))
Pts<-read.dta13(paste("Z:/Projects/1880Data/1880Microdata/1880MicrodataSouthernCities_LatestAnalysesFiles/",Shortname,"_Jan23.dta", sep=""))
hex150<-"hex150"
Seg_ID<-"Seg_ID"

#New Orleans  
LongName<-"New Orleans"
Shortname<-"NewOrleans"
Cityname<-"NOLA"
hex150<-"hexid150"
hex225<-"hex225"
Seg_ID<-"Seg_ID"
Seg<-read.dbf(paste("Z:/Users/_Exchange/1880 Stuff/AllCities/",LongName,"/",Cityname,"_Segment_Points.dbf", sep=""))
SG<-read.dbf(paste("Z:/Users/_Exchange/1880 Stuff/AllCities/",LongName,"/",Cityname,"_join150_Points.dbf", sep=""))
Ex<-read.dbf(paste("Z:/Users/_Exchange/1880 Stuff/AllCities/",LongName,"/",Cityname,"_join225_Points.dbf", sep=""))
Pts<-read.dta13(paste("Z:/Projects/1880Data/1880Microdata/1880MicrodataSouthernCities_LatestAnalysesFiles/",Shortname,"_Jan23.dta", sep=""))
hex150<-"hexid150"
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
hex150<-"hexid150"
Seg_ID<-"Seg_ID"

#Washington
LongName<-"Washington"
Shortname<-"WashingtonDC"
Cityname<-"Washington"
hex150<-"hexid150"
hex225<-"hex225"
Seg_ID<-"Seg_ID"
Seg<-read.dbf(paste("Z:/Users/_Exchange/1880 Stuff/AllCities/",LongName,"/",Cityname,"_join225_Points.dbf", sep=""))
SG<-read.dbf(paste("Z:/Users/_Exchange/1880 Stuff/AllCities/",LongName,"/",Cityname,"_Points.dbf", sep=""))
Ex<-read.dbf(paste("Z:/Users/_Exchange/1880 Stuff/AllCities/",LongName,"/",Cityname,"_join225_Points.dbf", sep=""))
Pts<-read.dta13(paste("Z:/Projects/1880Data/1880Microdata/1880MicrodataSouthernCities_LatestAnalysesFiles/",Shortname,"_Jan23.dta", sep=""))
hex150<-"hexid150"
Seg_ID<-"Seg_ID"


merge1<-merge(x=SG[,c(hex150,"serial")],y=Ex[,c(hex225,"serial")], by="serial", all.x=TRUE)
merge2<-merge(x=merge1,y=Seg[,c(Seg_ID, "serial")], by="serial", all.x=TRUE)
merge2<-subset(merge2, !duplicated(merge2$serial))
merge2$serial<-as.numeric(levels(merge2$serial))[merge2$serial]
merge3<-merge(x=Pts[,c("race","relate","age","sex","marst","occ50us","labforce", "numperhh", "bp_south",
                       "fam_maxsei", "bp","serial")],y=merge2, by="serial", all.x=TRUE)
merge3<-merge3[complete.cases(merge3[Seg_ID]),]
merge3$Person<-recode(merge3$serial,"\" \"=0; else=1")
merge3<-subset(merge3, (paste("merge3$",hex150)!=0))
merge3$City<-Cityname

#Creat Dummy Sex Variable
  merge3$female<-recode(merge3$sex,"1=0; else=1")
#Recode Marst
  merge3$marst2<-recode(merge3$marst, "1:2='Married'; 4='Divorced'; 5='Widowed'; 6='Single'; 7='Unknown'")
#Birthplace - Checked from bp code
  merge3$bp_south<-recode(merge3$bp_south, "2='Same_State'; 1='Southern_State';0='Other Place'")
#Relatives
  merge3$relative<-recode(merge3$relate, "100:1061='Relative'; 1100:1301='Non-Relative'; else=NA")
#Household Composition
  merge3$comp<-ifelse(merge3$relative=='Relative' & merge3$numperhh==1, 1,0)
  merge3$hhcomp<-ifelse(merge3$comp==1, "Lives_Alone",merge3$relative)
#Create Dummy of 18 Plus
  merge3$Above18<-recode(merge3$age,"c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,NA)=0; else=1")
#Create Head of Household Variable
  merge3$hh<-recode(merge3$relate,"c(101)=1; else=0")
#Unrelated Individuals
  merge3$UnRelate<-recode(merge3$relate,"c(1200,1201,1202,1203,1204,1205,1206,1221,1222,1223,1230)=1; else=0")
#Occupation Changes
  # 1 "Technical Professional" 2 "Farmers" 3 "Managers, officials, and proprietors" 4 "Clerical and kindred" 
  #5 "Sales workers" 6 "Craftsmen" 7 "Operatives" 8 "Household service workers" 9 "Service workers" 10 "Farm laborers" 
  #11 "Laborers" 99 "Non-occupational responses"
  merge3$occ_cat50<-recode(merge3$occ50us, "100:199=2; 200:299=3; 300:399=4; 400:499=5; 500:599=6;
                         600:699=7; 700:729=8; 730:799=9; 800:899=10; 900:975=11; 979:998=12; else=1")
#Unrelated and Over 18
  merge3$UnRelate18<-ifelse(merge3$UnRelate==1 & merge3$Above18==1,1,0)
#Unrealted, Over 18, and With Recorded Occupation
  merge3$UnRelate18Occ<-ifelse(merge3$UnRelate18==1 & merge3$occ_cat50>=2, 1, 0)
#Whites
  merge3$whites<-recode(merge3$race,"100=1; else=0")
  merge3$WHH<-ifelse(merge3$whites==1 & merge3$hh==1,1,0)
#Blacks 
  merge3$blacks<-recode(merge3$race,"c(200,210)=1; else=0")
  merge3$BHH<-ifelse(merge3$blacks==1 & merge3$hh==1,1,0)
#Mulatto
  merge3$mulatto<-recode(merge3$race, "210='Mulatto';200='Negro'; 999='NA'")

#Create Proportion Black
  merge3$Person<-recode(merge3$serial,"\" \"=0; else=1")
  #By City
    bltot<-tapply(merge3$blacks, INDEX=list(merge3$Person), FUN=sum)
    blktotCity<-data.frame(Person=names(bltot), blktotCity=bltot)
    tot<-tapply(merge3$Person, INDEX=list(merge3$Person), FUN=sum)
    totCity<-data.frame(Person=names(tot), totCity=tot)
    
    Pop<-merge(x=blktotCity, y=totCity, by="Person", all.x=TRUE)
    merge3<-merge(x=merge3, y=Pop, by="Person", all.x=TRUE)
    
  #By Segment
    bls<-tapply(merge3$blacks, INDEX=list(eval(parse(text=paste("merge3$",Seg_ID,sep="")))), FUN=sum)
    blseg<-data.frame(Seg_ID=names(bls), blseg=bls)
    tots<-tapply(merge3$Person, INDEX=list(eval(parse(text=paste("merge3$",Seg_ID,sep="")))), FUN=sum)
    totseg<-data.frame(Seg_ID=names(tots), totseg=tots)
  
    Segment<-merge(x=blseg, y=totseg, by="Seg_ID", all.x=TRUE)
    Segment$PropBSeg<-(Segment$blseg/Segment$totseg) 
    ifelse(exists("segid", where= merge3), (Segment$segid=Segment$Seg_ID), (Segment$Seg_ID=Segment$Seg_ID))
    merge3<-merge(x=merge3, y=Segment, by=Seg_ID)
  
  #By Segment Group
    blsg<-tapply(merge3$blacks, INDEX=list(eval(parse(text=paste("merge3$",hex150,sep="")))), FUN=sum)
    blsegG<-data.frame(hex150=names(blsg), blsegG=blsg)
    totsg<-tapply(merge3$Person, INDEX=list(eval(parse(text=paste("merge3$",hex150,sep="")))), FUN=sum)
    totsegG<-data.frame(hex150=names(totsg), totsegGroup=totsg)
    
    SegmentGroup<-merge(x=blsegG, y=totsegG, by="hex150", all.x=TRUE)
    SegmentGroup$PropBSegG<-(SegmentGroup$blsegG/SegmentGroup$totsegG)
    ifelse(exists("hexid150", where= merge3), (SegmentGroup$hexid150=SegmentGroup$hex150), (SegmentGroup$hex150=SegmentGroup$hex150))
    merge3<-merge(y=merge3, x=SegmentGroup, by=hex150, all.x=T)
    
  #Extended Segment Group by Race - Pop Total
    blsgE<-tapply(merge3$blacks, INDEX=list(eval(parse(text=paste("merge3$",hex225,sep="")))), FUN=sum)
    blsegEG<-data.frame(hex225=names(blsgE), blsegEG=blsgE)
    totsgE<-tapply(merge3$Person, INDEX=list(eval(parse(text=paste("merge3$",hex225,sep="")))), FUN=sum)
    totsegEG<-data.frame(hex225=names(totsgE), totsegEG=totsgE)
    
    EXSegmentGroup<-merge(x=blsegEG, y=totsegEG, by="hex225", all.x=TRUE)
    EXSegmentGroup$PropBSegEX<-(EXSegmentGroup$blsegEG/EXSegmentGroup$totsegEG)
    ifelse(exists("hexid225", where= merge3), (EXSegmentGroup$hexid225=EXSegmentGroup$hex225), (EXSegmentGroup$hex225=EXSegmentGroup$hex225))
    merge3<-merge(y=merge3, x=EXSegmentGroup, by=hex225, all.x=T)
    
#Create Separate file for White Headed Households
  WhiteHH<-subset(merge3, merge3$hh==1)
  myvars<-names(merge3) %in% c("WHH")
  merge3<-merge3[!myvars]
  FileWork<-merge(x=merge3,y=WhiteHH[,c("WHH", "serial")], by="serial", all.x=TRUE)
  
#Create Final Tables
FileWork$BWHH<-ifelse(FileWork$WHH==1 & FileWork$blacks==1,1,0)
FileWork$LiveServ<-ifelse(FileWork$BWHH==1 & FileWork$occ_cat50==8,1,0)
FileWork2<-FileWork[which(FileWork$blacks==1 & FileWork$LiveServ!=1),]
FileWork3<-FileWork2[which(FileWork2$hh==1 | FileWork2$UnRelate18Occ==1),]
ifelse(exists("hex225", where= FileWork3), (FileWork3$hex225=FileWork3$hex225), (FileWork3$hex225=FileWork3$hexid225))
ifelse(exists("hexid150", where= FileWork3), (FileWork3$hexid150=FileWork3$hexid150), (FileWork3$hexid150=FileWork3$hex150))
ifelse(exists("segid", where= FileWork3), (FileWork3$segid=FileWork3$segid), (FileWork3$segid=FileWork3$Seg_ID))

dropvar<-names(FileWork3) %in% c("hexid225", "hex150")
FileWork3<-FileWork3[!dropvar]
FileWork4<-FileWork3[,c("serial","segid", "hexid150", "hex225","mulatto", "female", "age", "marst2","hhcomp", "bp_south", "fam_maxsei",
                        "hh", "occ_cat50", "blacks", "BHH", "WHH", "BWHH", "LiveServ", "blktotCity", "totCity",
                        "blseg", "totseg", "blsegG", "totsegGroup", "blsegEG", "totsegEG",
                        "PropBSeg", "PropBSegG", "PropBSegEX","City")]

FileWork4<-FileWork4[which(FileWork4$segid!=0),]

rm(Ex,merge1,merge2, merge3, Pts, Seg,SG, WhiteHH, FileWork, FileWork2, FileWork3, myvars, Seg_ID, Shortname, Cityname, 
   hex150, hex225, LongName, dropvar, Pop, totCity, totseg, bls, tot, tots, blseg, blsgE, bltot,
   totsg, totsgE, blktotCity, blsegEG, blsegG, EXSegmentGroup, Segment, SegmentGroup, totsegEG, totsegG, blsg) 
RegSeg<-rbind(RegSeg,FileWork4)

rm(FileWork4)
describe(RegSeg$City)
RegSeg<-RegSeg[which(RegSeg$age>=18),]


#RECODES FOR USE WITH STATA

#Recode Marst
  RegSeg$marst2s<-recode(RegSeg$marst2, "'Married'=0; 'Divorced'=3; 'Widowed'=2; 'Single'=1; 'Unknown'=4")
  RegSeg$Mar<-recode(RegSeg$marst2s, "0=1; else=0")
  RegSeg$Div<-recode(RegSeg$marst2s, "3=1; else=0")
  RegSeg$Wid<-recode(RegSeg$marst2s, "2=1; else=0")
  RegSeg$Single<-recode(RegSeg$marst2s, "1=1; else=0")
  RegSeg$Unknown<-recode(RegSeg$marst2s, "4=1; else=0")
#Birthplace - Checked from bp code
  RegSeg$bp_souths<-recode(RegSeg$bp_south, "'Same_State'=0; 'Southern_State'=1; 'Other Place'=2")
  RegSeg$SameS<-recode(RegSeg$bp_souths, "0=1; else=0")
  RegSeg$South<-recode(RegSeg$bp_souths, "1=1; else=0")
  RegSeg$OtherP<-recode(RegSeg$bp_souths, "2=1; else=0")
#Household Composition
  RegSeg$hhcomps<-recode(RegSeg$hhcomp, "'Relative'=0; 'Lives_Alone'=1; 'Non-Relative'=2")
  RegSeg$Fam<-recode(RegSeg$hhcomps, "0=1; else=0")
  RegSeg$LiveAlone<-recode(RegSeg$hhcomps, "1=1; else=0")
  RegSeg$NonRelative<-recode(RegSeg$hhcomps, "2=1; else=0")
#Mulatto
  RegSeg$mulattos<-recode(RegSeg$mulatto, "'Negro'=0; 'Mulatto'=1;")
#CityName
  RegSeg$Citys<-recode(RegSeg$City, "'Atlanta'=0; 'BAL'=1; 'Charleston'=2; 'Louisville'=3;
                       'memphis'=4; 'Mobile'=5; 'Nashville'=6; 'NOLA'=7; 'Richmond'=8; 'Washington'=9")

#Write the RegSeg File
write.csv(RegSeg, file="C:/Users/mmarti24/Documents/Segregation Project/RegSeg.csv", row.names=F)
write.foreign(RegSeg, "C:/Users/mmarti24/Documents/Segregation Project/RegSeg.txt", "C:/Users/mmarti24/Documents/Segregation Project/RegSeg.sas",
                package="SAS")
write.dta(RegSeg, "c:/Users/mmarti24/Documents/Segregation Project/RegSeg.dta")

####### Models ######
RegSeg<-RegSeg
RegSeg$marst2b<-factor(RegSeg$marst2)
RegSeg$female2<-factor(RegSeg$female)
RegSeg$hhcomp2<-factor(RegSeg$hhcomp)
RegSeg$bp_south2<-factor(RegSeg$bp_south)

attach(Test)

fit.Seg<-lm(PropBSeg~I(mulatto=="Mulatto") + relevel(female2, "0") + age + relevel(marst2b, "Married") + 
              relevel(hhcomp2, "Relative") + relevel(bp_south2, "Same_State") + fam_maxsei + factor(City), data=Test)

fit.SegG<-lm(PropBSegG~I(mulatto=="Mulatto") + relevel(female2, "0") + age + relevel(marst2b, "Married") +
               relevel(hhcomp2, "Relative") + relevel(bp_south2, "Same_State") + fam_maxsei + factor(City), data=Test)

fit.SegEX<-lm(PropBSegEX~I(mulatto=="Mulatto") + relevel(female2, "0") + age + relevel(marst2b, "Married") + 
                relevel(hhcomp2, "Relative") + relevel(bp_south2, "Same_State") + fam_maxsei + factor(City), data=Test)

summary(fit.Seg)
summary(fit.SegG)
summary(fit.SegEX)

library(lme4)
multi.Seg<-glmer(PropBSeg~I(mulatto=="Mulatto") + relevel(female2, "0") + age + relevel(marst2b, "Married") + 
                 relevel(hhcomp2, "Relative") + relevel(bp_south2, "Same_State") + fam_maxsei + factor(City) +
               (1|City), family=gaussian, data=Test)

summary(multi.Seg)

