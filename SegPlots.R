library(foreign)
library(ggplot2)
library(scales)
library(plyr)
library(dplyr)

setwd("C:/Users/mmarti24/Dropbox/Papers/SIS - Early Arriving/")
df <- read.csv("tabs_all_years.csv")

# Functions to properly capitalize city names
SentCase <- function(InputString){
  InputString <-
    paste(toupper(substring(InputString,1,1)),tolower(substring(InputString,2)),
          sep="")
}

ProperCase <- function(InputString){
  sapply(lapply(strsplit(InputString," "), SentCase), paste, collapse=" ")
}
s <- strsplit(as.character(df$citystate),",")
cities <- vector(mode="character",length=length(s))
for (i in 1:length(s)) {
  cities[i] <- paste(ProperCase(s[[i]][1]),",",s[[i]][2],sep="")
}
df$citystate <- factor(cities)

# Theme for plotting
theme_custom <- theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black"),
        legend.position="none",
        plot.title = element_text(hjust = 0.5),
        strip.text.x = element_text(size = 9),
        axis.text.y=element_text(size=7),
        axis.text.x=element_text(size=7,angle=90,vjust=0.4))

  names(df)
#Proportions (Change percent here)
  Eth_Name<-"German"
  df$eth<-df$pctgerman
  df$citystate <- factor(df$citystate,levels=unique(df$citystate))
  df<-df[order(df$citystate,df$year),]
#Create Percent Threshold
  perThresh <- 5
  df$eth<-ifelse(df$eth==0,1,df$eth)
  df$thresh <- ifelse(df$eth < perThresh/100,"red2","grey30")
#Create object to plot  
  pPctEth <- ggplot(data=df,aes(x=year,y=eth,group=citystate)) 
#Create Outputfile
  tiff(paste("Percent ",Eth_Name,".tif",sep=""), width=20, height=10, units='in',res=300)
  pPctEth + geom_bar(stat="identity",width=8,fill=df$thresh) + facet_wrap(~citystate,ncol=20) +
    labs(title = paste("% ",Eth_Name, "(with ",perThresh,"% threshhold)",sep=""), y=Eth_Name, "(%)", x="Year") +
    scale_y_continuous(labels=percent) + scale_x_continuous(breaks=c(1880,1890,1900,1910,1920,1930,1940)) +
    coord_cartesian(ylim=c(0,0.1)) + geom_hline(yintercept=perThresh/100,color="red2",size=1) + theme_custom

dev.off()


###### Ethnic Specific Population Size #####
  Eth_Name<-"German"
  df$eth<-df$german
  df$citystate <- factor(df$citystate,levels=unique(df$citystate))

  PopThresh <- 100
  df$eth <- ifelse(df$eth == 0, 1, df$eth) # Without this, having zero black people causes problems
  df$thresh <- ifelse(df$eth < PopThresh,"red2","grey30") 

  pPop <- ggplot(data=df,aes(x=year,y=log10(eth),group=citystate))

  tiff(paste("Population ",Eth_Name,".tif",sep=""), width=20, height=10, units='in',res=300)
  pPop + geom_bar(stat="identity",width=8,fill=df$thresh) + facet_wrap(~citystate,ncol=20) +
    labs(title = paste("Population", Eth_Name, "(with",as.character(PopThresh),"pop. threshhold)"), y="Population (log10)", x= "Year") + 
    scale_y_continuous(breaks=c(1,2,3,4),labels=c("10","100","1,000","10,000")) +
    scale_x_continuous(breaks=c(1880,1890,1900,1910,1920,1930,1940)) +
    coord_cartesian(ylim=c(1,4)) + geom_hline(yintercept=log10(PopThresh),color="red2",size=1) + theme_custom

dev.off()

###### Population size #####
PopThresh <- 25000
df$N <- ifelse(df$N == 0, 1, df$N) # Without this, having zero black people causes problems
df$PopThresh <- ifelse(df$N < PopThresh,"red2","grey30")

pN <- ggplot(data=df,aes(x=df$year,y=log10(df$N),group=df$citystate))

tiff("pN.tif", width=20, height=10, units='in',res=300)
pN + geom_bar(stat="identity",width=8,fill=df$PopThresh) + facet_wrap(~citystate,ncol=20) +
  labs(title = paste("City size (with",as.character(PopThresh),"pop. threshhold)"), y="Population (log10)", x= "Year") + 
  scale_y_continuous(breaks=c(3,4,5,6),labels=c("1,000","10,000","100,000","1,000,000")) +
  scale_x_continuous(breaks=c(1880,1890,1900,1910,1920,1930,1940)) +
  coord_cartesian(ylim=c(3,6)) + geom_hline(yintercept=log10(PopThresh),color="red2",size=1) + theme_custom

dev.off()
