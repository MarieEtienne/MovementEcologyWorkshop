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

setwd('/home/metienne/EnCours/2015-SA-MovementEcology/Data/Practicals')

gpsData <- read.table('BI_12_2010_gps_06_CAGA_018.csv', header = T, sep=",")
gpsData$standardTime <- as.POSIXct( 
  strptime(
    paste(gpsData$Date, gpsData$Time, sep=" ")
    , 
    format="%Y/%m/%d %H:%M:%S"),  tz="GMT")

xy <- project(list(x=gpsData$Longitude, y=gpsData$Latitude),"+proj=utm +zone=35 +south +ellps=WGS84")
gpsData$x <- xy[[1]]
gpsData$y <- xy[[2]]

coordinates(gpsData) <- c("x","y")
BNG <- CRS("+proj=utm +zone=35 +south +ellps=WGS84")
proj4string(gpsData)<- BNG
p4s <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
l1.p4s <- spTransform(gpsData, CRS=p4s)
writeOGR(gpsData, dsn="text.kml", layer="p4s", driver="KML" )

test <- SpatialPointsDataFrame(coords = gpsData[,c("Longitude", "Latitude")],data = gpsData, proj4string=CRS("+proj=utm +zone=35 +south +ellps=WGS84"), bbox = NULL)
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


## KML for google earth
traj2 <- traj1
traj2<- traj2[, c("lon.dec", "lat.dec", "standardTime")]
coordinates(traj1) <- c("lon.dec", "lat.dec")
p4s <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
coords <- SpatialPoints(traj2[,c("lon.dec", "lat.dec")], proj4string=p4s, bbox = NULL)
traj2.sp <- SpatialPointsDataFrame(coords=coords, data=traj2, 
                       proj4string = p4s)
writeOGR(traj2.sp, dsn="text.kml", layer="traj2.sp", driver="KML")


##### Two different scales on the same graph
set.seed(101)
x <- 1:10
y <- rnorm(10)
## second data set on a very different scale
z <- runif(10, min=1000, max=10000) 
par(mar = c(5, 4, 4, 4) + 0.3)  # Leave space for z axis
plot(x, y) # first plot
par(new = TRUE)
plot(x, z, type = "l", axes = FALSE, bty = "n", xlab = "", ylab = "")
axis(side=4, at = pretty(range(z)))
mtext("z", side=4, line=3)

