##AUto correlated random walk
N<- 1000
phi <- sapply(1:N, function(d){rnorm(1, mean=0, sd=0.5) })
hist(phi)
V <- rep(1, N)
phi.cum <- cumsum(phi)%%(2*pi)
x <- cumsum(c(0,cos(phi.cum)*V))
y <- cumsum(c(0,sin(phi.cum)*V))
plot(x,y,"l")


traj <- data.frame(x=x, y=y)
s=1
time <- seq(1,N,s)
hh <- floor(time/3600)
mn <- floor((time - hh*3600)/60)
ss <- (time - hh*3600-mn*60)

traj1 <- traj[time,]

ltraj1 <- as.ltraj(xy = traj1, 
                   typeII = TRUE,
                   date =as.POSIXct( strptime(paste(hh, mn, ss, sep=":"),   format="%H:%M:%S"),  tz="GMT"),
                   id = rep(1,length(time)))


l=2
reg.traj <- redisltraj(ltraj1,u = l,burst = 1, type = "space" )
cr <- mean(cos(reg.traj[[1]]$rel.angle), na.rm = TRUE)
lr <- l
SinIndex <- sqrt(2/lr*tan(pi/2*(1-cr)))

reg.traj[[1]]$speed <- reg.traj[[1]]$dist/reg.traj[[1]]$dt
meanSpeed <- mean(reg.traj[[1]]$speed, na.rm=T)
diffIndex <- meanSpeed/SinIndex^2

#class(reg.traj) <- class(reg.traj)[1]
plot(reg.traj)
