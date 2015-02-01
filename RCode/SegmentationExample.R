rm(list=ls())

## ---- Practical1
load(file="../Data/trajEx.Rd")
plot(traj.ex, addpoints = F)
legend("bottomleft",pch=c(2, 0), col=c(4,2),
       legend=c("Start", "End"), bty = "n",
       pt.lwd = c(1.5,1.5), pt.cex = c(1.5,1.5))

## ---- Practical2
# Movement description ----------------------------------------------------
## turning angle and speed
traj.ex[[1]]$speed <- traj.ex[[1]]$dist/traj.ex[[1]]$dt
par(mfcol=c(2,1))
plot(traj.ex[[1]]$date, traj.ex[[1]]$rel.angle, "l",)
plot(traj.ex[[1]]$date, traj.ex[[1]]$speed, "l")

## Persistent and normal velocity
traj.ex[[1]]$dP = traj.ex[[1]]$dist/traj.ex[[1]]$dt * cos(traj.ex[[1]]$rel.angle)
traj.ex[[1]]$dN = traj.ex[[1]]$dist/traj.ex[[1]]$dt * sin(traj.ex[[1]]$rel.angle)
par(mfcol=c(2,1))
plot(traj.ex[[1]]$date, traj.ex[[1]]$dP, "l",)
plot(traj.ex[[1]]$date, traj.ex[[1]]$dN, "l")
save("traj.ex", file="../Data/traj.Rd")

## ---- Practical3-adehabitatLT
## Segmentation:
library('adehabitatLT')
garde <- which(!is.na(traj.ex[[1]]$speed))
traj.lav <- lavielle(traj.ex, Lmin=10, Kmax=20) ## traj.ex[[1]]$dist is the series considered
traj.lav <- lavielle(traj.ex[[1]]$speed[garde] , Lmin=10, Kmax=100) ## traj.ex[[1]]$dist is the series considered
#traj.lav <- lavielle(traj.ex[[1]]$speed[garde] , Lmin=10, Kmax=100, type="var") ## traj.ex[[1]]$dist segmentation based on the variance
## choose the number of segments
par(mfrow=c(1,1))
K.opt <- chooseseg(traj.lav, output ="opt")
## There is a clear break in the
##
seg.traj.lav <- findpath(traj.lav, K.opt )
seg.traj.lav

## ---- Practical3
# Segmenation Only --------------------------------------------------------
library('cghseg') ## segmentation and clustering
traj.cgh  <- new("CGHdata",
                 Y=traj.ex[[1]]$speed[!is.na(traj.ex[[1]]$speed)])
CGHo <- new("CGHoptions")
calling(CGHo)=FALSE ## no clustering
seg.speed <- uniseg(.Object=traj.cgh, CGHo=CGHo)
seg.speed.profile <- getsegprofiles(seg.speed)  
plot(traj.ex[[1]]$speed)
lines(1:length(seg.speed.profile),seg.speed.profile, type="s", col=2)



## ---- Practical4
# Segmentation + classification -------------------------------------------
calling(CGHo)=TRUE ## clustering
CGHo@nblevels=2 ## number of different "states"
CGHo@alpha =0.5
seg.speed <- uniseg(.Object=traj.cgh, CGHo=CGHo)
seg.speed.profile <- getsegprofiles(seg.speed)  
plot(traj.ex[[1]]$speed)
lines(1:length(seg.speed.profile),seg.speed.profile, type="s", col=2)



 
