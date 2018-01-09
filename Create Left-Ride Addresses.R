library(shapefiles)
library(maptools)
library(rgdal)
library(spdep)

richpoints<-readShapePoints("C:/Users/mmarti24/Documents/Segregation Project/Richmond/RichProject.shp",
                            proj4string=CRS("+proj=utm +zone=17"))
richpointsatt<-richpoints@data
richpoints<-richpoints[which(richpoints$X!=0),]
coord<-data.frame(x=c(richpoints$X), y=c(richpoints$Y))

names(richpoints)

richleft<-richpoints[which(richpoints$Side=='L'),]
richright<-richpoints[which(richpoints$Side=='R'),]

writeOGR(richleft, dsn='C:/Users/mmarti24/Documents/Segregation Project/Richmond', 
         layer='RichLeft', driver="ESRI Shapefile", overwrite_layer=T, coord)
writeOGR(richright, dsn='C:/Users/mmarti24/Documents/Segregation Project/Richmond', 
         layer='RichRight', driver="ESRI Shapefile", overwrite_layer=T, coord)

rightst<-readShapeLines("C:/Users/mmarti24/Documents/Segregation Project/Richmond/RightStreets.shp",
                            proj4string=CRS("+proj=utm +zone=17"))
leftst<-readShapeLines("C:/Users/mmarti24/Documents/Segregation Project/Richmond/LeftStreets.shp",
                        proj4string=CRS("+proj=utm +zone=17"))

rightsttable<-rightst@data
leftsttable<-leftst@data
write.csv(rightsttable,file="C:/Users/mmarti24/Documents/Segregation Project/Richmond/Rightst.csv")
write.csv(leftsttable,file="C:/Users/mmarti24/Documents/Segregation Project/Richmond/Leftst.csv")

#Code to find duplicate street lines#
####Richmond#####
lines<-data.frame(read.dbf("Z:/Projects/Southern Cities/Richmond/GIS/Richmondst_cln.dbf"))
write.csv(lines,file="C:/Users/mmarti24/Documents/Segregation Project/lines.csv")

####Atlanta#####
lines<-data.frame(read.dbf("Z:/Projects/Southern Cities/Atlanta/GIS/Atlantast_cln.dbf"))
write.csv(lines,file="C:/Users/mmarti24/Documents/Segregation Project/lines.csv")

####Washington DC#####
lines<-data.frame(read.dbf("Z:/Projects/Southern Cities/Washington DC/GIS/wash_cln_ss.dbf"))
write.csv(lines,file="C:/Users/mmarti24/Documents/Segregation Project/lines.csv")
