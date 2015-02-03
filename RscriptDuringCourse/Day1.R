## metadata file

library('adehabitatLT')
library('CircStats')
library('sp')
library('rgdal')
library('proj4')

metaData<- data.frame(Id =1, Species="BFAL")
metaData$Species<- as.character(metaData$Species)
metaData[2,] <- c(2, "BFAL")
metaData[3,] <- c(3, "MacP")

load(file="metaData.Rd")
metaData


plus <- function(a){
  print(a)
  print(b)
  return(a+b)
}

#### WARNING change the function
## function shown in course was not working properly
extractData <- function(metaData, fields,values  )
{
  ## metaData is the table
  individuals=list()
  for( i in 1:length(fields))
  {
    individuals[[i]] <- 1*(metaData[fields[i]]==values[i])
  }
  individuals <- which(Reduce("*", individuals)==1)
  return(metaData[individuals,])
}

extractData(metaData=metadata, fields=c('Species', 'Study_site'),values=c("Lion", "Nyathi"))


m1 <- extractData(metaData, fields=c("Sex", "Species"), values=c("F", "BFAL"))
m2 <- extractData(metaData, fields=c("Sex", "Species"), values=c("M", "BFAL"))
rbind(m1,m2)

traj1$datePOS <- as.POSIXct(strptime(traj1$dateTemps, format="%Y-%m-%d %H:%M:%S" ))





library('adehabitatLT')
setwd('/home/metienne/EnCours/2015-SA-MovementEcology/Data/Practicals')
load('metadata')
dataProv <- extractData(metadata, fields = "ID", values=18)
gpsData <- read.table(dataProv$gpsfile, header = T, sep=",")
gpsData$standardTime <- as.POSIXct( 
  strptime(
    paste(gpsData$Date, gpsData$Time, sep=" ")
    , 
    format="%Y/%m/%d %H:%M:%S"),  tz="GMT")
write.table(gpsData, file=dataProv$gpsfile, row.names=F, sep=",")

ex.coord <- list(gpsData$Longitude, gpsData$Latitude)
l1 <- as.data.frame(project(ex.coord,"+proj=utm +zone=35 +south +ellps=WGS84"))
l1$Time <- gpsData$standardTime
coordinates(l1) <- c("x","y")
BNG <- CRS("+proj=utm +zone=35 +south +ellps=WGS84")
proj4string(l1)<- BNG
p4s <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
l1.p4s <- spTransform(l1, CRS=p4s)
writeOGR(l1.p4s, dsn="text.kml", layer="l1.p4s", driver="KML" )

test <- SpatialPointsDataFrame(l1, proj4string=CRS("+proj=utm +zone=35 +south +ellps=WGS84"), bbox = NULL)
test.ll <- spTransform(test,CRS("+proj=utm +zone=35 +south +ellps=WGS84") )
writeOGR(test, driver="KML", dsn = "test.kml")
coordinates(l1) <- c("x", "y")
traj.ex <- as.ltraj(xy = l1, date=gpsData$standardTime, id=rep(1, nrow(gpsData)))
traj.ex[[1]]$speed <- traj.ex[[1]]$dist/traj.ex[[1]]$dt
plot(traj.ex)

###seychelle data
setwd('/home/metienne/Dropbox/EtatJErre/DataSeychelles/data')
traj1 <- read.table("../data/base-GPS-ew.txt", sep=";",row.names=1)
traj1$heure <-  floor(traj1$h.dec)
traj1$mn <-  floor((traj1$h.dec - traj1$heure)*60)
traj1$sec <-  floor((traj1$h.dec - traj1$heure -traj1$mn/60)*3600)
traj1$date <- paste(traj1$an, traj1$mois, traj1$jour, sep="-")
traj1$temps <- paste(traj1$heure, traj1$mn, traj1$sec, sep=":")


traj1$standardTime <- as.POSIXct( 
  strptime(
    paste(traj1$date, traj1$temps, sep=" "), 
    format="%Y-%m-%d %H:%M:%S"),  tz="GMT")

ex.coord <- list(traj1$lon.dec, traj1$lat.dec)
l1 <- as.data.frame(project(ex.coord,"+proj=utm +zone=40 +south +ellps=WGS84"))
l1$time <- traj1$standardTime
coordinates(l1) <- c("x","y")
BNG <-  CRS("+proj=utm +zone=40 +south +ellps=WGS84")
proj4string(l1)<- BNG
p4s <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
l1.p4s <- spTransform(l1, CRS=p4s)
writeOGR(l1.p4s, dsn="text.kml", layer="l1.p4s", driver="KML")
