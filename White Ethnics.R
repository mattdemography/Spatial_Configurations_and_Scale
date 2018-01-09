d40<-read.csv("Z:/Users/Matt/SanAntonio/Data Files/SA_AutoClean40.csv")
d30<-read.csv("Z:/Users/Matt/SanAntonio/Data Files/SA_AutoCleaned30.csv")
names(d40)

#Look at race/ethnicity
myvars<-c("serial", "race", "hispan", "citizen", "bpl", "nativity")
d40s<-d40[myvars]

myvars<-c("serial", "self_empty_info_race", "self_birth_place_empty")
d30s<-sa30

#Cleaning Race/Ethnicity for 1930
trim <- function( x ) {
  gsub("(^[[:space:]]+|[[:space:]]+$)", "", x)
}
  d30s$self_empty_info_race<-sapply(d30s$self_empty_info_race, tolower)
  d30s$self_empty_info_race<-trim(d30s$self_empty_info_race)
  d30s$self_empty_info_race<-car::recode(d30s$self_empty_info_race,"c('negro','colored','black','quadroon','mulatto')='black';
                                           else=d30s$self_empty_info_race") 
  d30s$self_empty_info_race<-car::recode(d30s$self_empty_info_race,"c('white','w')='white';
                                           else=d30s$self_empty_info_race") 
  d30s$self_empty_info_race<-car::recode(d30s$self_empty_info_race,"c('m','mexican','spanish')='mexican';
                                           else=d30s$self_empty_info_race")
  d30s$self_empty_info_race<-car::recode(d30s$self_empty_info_race,"c('chinese','e','filipino','indian','italian','japanese','jew'
,'heb','syr','syrian','')='other';else=d30s$self_empty_info_race")
  
  table(d30s$self_empty_info_race)
  
  table(d30s$self_birth_place_empty)
  d30s$self_birth_place_empty<-sapply(d30s$self_birth_place_empty, tolower)
  
  d30s$g<-trim(ifelse(grepl("germany", d30s$self_birth_place_empty),1,0))
  d30s$r<-ifelse(d30s$self_empty_info_race=='white' & d30s$g==0, 1,0)
  d30s$r<-ifelse(d30s$self_empty_info_race=='mexican' & d30s$g==0, 2,d30s$r)
  d30s$r<-ifelse(d30s$self_empty_info_race=='black' & d30s$g==0, 3,d30s$r)
  d30s$r<-ifelse(d30s$self_empty_info_race=='other' & d30s$g==0, 4,d30s$r)
  
  table(d30s$r)
  
  sa30<-d30s
  
  write.csv(sa30, "Z:/Projects/1940Census/Block Creation/San Antonio/Add_30_2nd.csv")
  

#Race Variables
  #100 - White   #200 - Black/Negro  #210 - Mulatto  #300 - American Indian/Alaska Native  #400 - Chinese
  #500 - Japanese  #600 - Filipino

#Hispan Variable
  #100 - Mexican  #200 - Puerto Rican  #300 - Cuban  #411 - Costa Rican  #412 - Guatemalan  #413 - Honduran
  #414 - Nicaraguan  #415 - Panamanian  #416 - Salvadoran  #417 - Central American  #420 - Argentinean
  #422 - Chilean  #423 - Colombian  #424 - Ecuadorian  #426 - Peruvian  #428 - Venezuelan  #430 - Criollo
  #480 - Spanish

#Citizen Variable
  #0 - N/A
  #1 - Born abroad of American Parents
  #2 - Naturalized Citizen
  #3 - Not a Citizen
  #4 - Not a Citizen, but has received first papers
  #5 - Foreign born, citizenship not reported (Not in 1930 and 1940)

#Birthplace (bpl) Variable
  #UNITED STATES		
    #100	Alabama    #200	Alaska    #400	Arizona	    #500	Arkansas	    #600	California	    #800	Colorado
    #900	Connecticut    #1000	Delaware    #1100	District of Columbia    #1200	Florida    #1300	Georgia
    #1500	Hawaii    #1600	Idaho    #1610	Idaho Territory    #1700	Illinois    #1800	Indiana    #1900	Iowa
    #2000	Kansas    #2100	Kentucky    #2200	Louisiana    #2300	Maine    #2400	Maryland    #2500	Massachusetts
    #2600	Michigan    #2700	Minnesota    #2800	Mississippi    #2900	Missouri    #3000	Montana
    #3100	Nebraska    #3200	Nevada    #3300	New Hampshire    #3400	New Jersey    #3500	New Mexico    #3510	New Mexico Territory
    #3600	New York    #3700	North Carolina    #3800	North Dakota    #3900	Ohio    #4000	Oklahoma    #4010	Indian Territory
    #4100	Oregon    #4200	Pennsylvania    #4400	Rhode Island    #4500	South Carolina    #4600	South Dakota
    #4610	Dakota Territory    #4700	Tennessee    #4800	Texas    #4900	Utah
  #Early Arriving (Northern and Southern Europe)
    #45300 - Germany(#45210 - Behemia)   #British (#41200 - Wales; #41100 - Scotland; #41000 - England(#41010 - Channel ISlands;
    #41011 - Guernsey; #41012 - Jersey; #41020 - Isle of Man))   #41400 - Irish (#41410 - Northern Ireland)
  attach(d40s)
    eai<-ifelse((bpl==45300|bpl==41200|bpl==41100|bpl==41000|bpl==41010|bpl==41011|bpl==41012|bpl==41020|
                   bpl==41400|bpl==41410|bpl==45210), 1, 0)
  detach(d40s)



