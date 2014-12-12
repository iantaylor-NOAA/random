# stuff from Ian

# data easily accessible at http://oceanadapt.rutgers.edu/

# plotting temperature from trawl survey
if(!exists("surv")){
  dir <- 'C:/github/random/survey_temp_figs'
  surv <- read.csv(file.path(dir,'wcann_haul.csv'))
  # remove duplicates (not sure why they are there)
  surv <- surv[1:(-1+which(surv$Trawl.Id==surv$Trawl.Id[1])[2]),]
  fish <- read.csv(file.path(dir,'wcann_fish.csv'))
}

if(!exists("bath")){
  bath <- read.table(file.path(dir,'etopo1.xyz'))
  names(bath) <- c("lon","lat","depth_m")
  lonvec <- sort(unique(bath$lon))
  latvec <- sort(unique(bath$lat))
  ## bathmat <- matrix(NA,nrow=length(latvec),ncol=length(lonvec))
  ## latmat <- lonmat <- bathmat
  bathmat <- latmat <- lonmat <- NULL
  for(ilat in 1:length(latvec)){
    if(ilat%%20==0){
      cat("ilat: ",ilat,"/",length(latvec),"\n")
    }
    lat <- latvec[ilat]
    sub <- bath$lat==lat
    bathmat <- rbind(bathmat, bath$depth_m[sub])
  }
  ##   #format of  etopo1.xyz
  ## -130                 50 -2282
  ## -129.98333333333332  50 -2315
  ## -129.966666666666669 50 -2330
  x <- lonvec
  y <- latvec
  z <- bathmat
  #z <- z[nrow(z):1,]     #why it needs to be flipped over, who knows?
  z <- t(as.matrix(z))  #or transposed for that matter....

  # obtain average of four corners of each square ... this is the basis for the color matrix
  z2 <- (z[-1, -1] + z[-1, -ncol(z)] + z[-nrow(z), -1] + z[-nrow(z), -ncol(z)])/4

  zmin=min(z2)
  zmax=max(z2)

  NwaterColors = round(abs(zmin))
  NlandColors = round(abs(zmax))

  #taking first 1/3 of topo.colors of length NwaterColors for water
  water.colors = topo.colors(NwaterColors*3)[1:NwaterColors]
  #taking terrain.colors of length NlandColors for land
  land.colors = terrain.colors(NlandColors)
  all.colors = c(water.colors, land.colors)

  z3 <- z2-min(z2)
  color.matrix <- all.colors[round(z3)]

  phi <- 60   # height angle
  theta <- 0  # degrees east (0 means looking due north)

  persp(x,y,z,col=color.matrix,
        phi=phi,theta=theta,ltheta=120,shade=0.5,bor=NA,
        xlab="Longitude", ylab="Latitude", zlab="Elevation (m)",
        expand=0.1)

  persp3d(x,y,z,col=all.colors[round(z-min(z))+1])

}

# rolling persp3d call into a function
persp3d.coast <- function(lat.range=c(31,50),
                          lon.range=c(-130,-115),
                          stretch=10){
  # subset data
  x.sub <- x[x > min(lon.range[1]) & x < max(lon.range[2])]
  y.sub <- y[y > min(lat.range[1]) & y < max(lat.range[2])]
  z.sub <- z[x > min(lon.range[1]) & x < max(lon.range[2]),
             y > min(lat.range[1]) & y < max(lat.range[2])]
  # calculate aspect ratio using stuff here:
  # http://wiki.cbr.washington.edu/qerm/index.php/R/Converting_Geographic_Coordinates_to_Kilometers
  lon0 <- mean(range(lon.range))
  lat0 <- mean(range(lat.range))
  rx <- 6371 * acos(sin(lat0 *pi/180)^2 + cos(lat0*pi/180)^2 * cos((lon0+.5)*pi/180 - (lon0-.5)*pi/180))
  ry <- 6371 * acos(sin((lat0 -.5)*pi/180) *  sin((lat0+.5)*pi/180) + cos((lat0-.5)*pi/180) * cos((lat0+.5)*pi/180))
  x.km <-(lon.range-lon0)*rx
  y.km <-(lat.range-lat0)*ry
  x.range.m <- 1000*diff(x.km)
  y.range.m <- 1000*diff(y.km)
  # draw stuff
  persp3d(x.sub,y.sub,z.sub,col=all.colors[round(z.sub-min(z))+1],
          xlab="",ylab="",zlab="")
  aspect3d(x=x.range.m,y=y.range.m,z=stretch*diff(range(z.sub)))
}

get.ranges <- function(Long=c(-122,-120),Lat=c(34,36)){
  lon0 <- mean(range(Long))
  lat0 <- mean(range(Lat))
  rx <- 6371 * acos(sin(lat0 *pi/180)^2 + cos(lat0*pi/180)^2 * cos((lon0+.5)*pi/180 - (lon0-.5)*pi/180))
  ry <- 6371 * acos(sin((lat0 -.5)*pi/180) *  sin((lat0+.5)*pi/180) + cos((lat0-.5)*pi/180) * cos((lat0+.5)*pi/180))
  x.km <-(Long-lon0)*rx
  y.km <-(Lat-lat0)*ry
  x.range.m <- 1000*diff(x.km)
  y.range.m <- 1000*diff(y.km)
}

range(surv$Temperature.At.the.Gear..degs.C.,na.rm=TRUE)
#[1]  2.91 14.70

## redyellowblue <- colorRampPalette(colors=c("blue","yellow","red"))
## colvec <- redyellowblue(100)
## colvec <- rev(rainbow(120)[1:100])
library(r4ss)
colvec <- rich.colors.short(100)
colfun <- function(x){
  colvec[round(100*(x/15)^.8)]
}

layout(mat=matrix(1:2,ncol=2),widths=c(10,2))
par(mar=c(5,5,1,1))
plot(surv$Best.Depth..m., surv$Best.Latitude..dd.,
     xlim=c(1300,0),pch=16,
     xaxs='i',
     col=colfun(surv$Temperature.At.the.Gear..degs.C.))
par(mar=c(5,.1,1,5))
tvec <- 0:15
image(x=0,y=tvec,matrix(tvec,nrow=1),col=colfun(tvec),
      axes=FALSE,xlab="",ylab="")
axis(4,las=1)

require(maps)
layout(mat=matrix(1:2,ncol=2),widths=c(6,2))
par(mar=c(5,5,1,1))
plot(surv$Best.Longitude..dd., surv$Best.Latitude..dd.,
     xlim=c(-128,-115),ylim=c(33,50),
     pch=16,
     xaxs='i',
     col=colfun(surv$Temperature.At.the.Gear..degs.C.))
par(mar=c(5,.1,1,5))
tvec <- 0:15
image(x=0,y=tvec,matrix(tvec,nrow=1),col=colfun(tvec),
      axes=FALSE,xlab="",ylab="")
axis(4,las=1)

require(maps)
layout(mat=matrix(1:2,ncol=2),widths=c(6,2))
par(mar=c(5,5,1,1))
plot(surv$Best.Longitude..dd., surv$Best.Latitude..dd.,
     xlim=c(-122,-120),ylim=c(34,35),
     pch=16,
     xaxs='i',
     col=colfun(surv$Temperature.At.the.Gear..degs.C.))
par(mar=c(5,.1,1,5))
tvec <- 0:15
image(x=0,y=tvec,matrix(tvec,nrow=1),col=colfun(tvec),
      axes=FALSE,xlab="",ylab="")
axis(4,las=1)

#install.packages('rgl')
n <- 2
#persp3d(x=seq(-122,-120,length=n),y=seq(34,35,length=n),
#        z=matrix(-50,n,n),col='white',
#        xlab="Longitude", ylab="Latitude",zlab="Depth (m)")


lat.range <- c(33,36); lon.range <- c(-122.5,-117)
lat.range <- c(46,49); lon.range <- c(-127,-122)
clear3d()
# make graph
persp3d.coast(lat.range=lat.range,lon.range=lon.range,stretch=5)
# subset tows
sub <- surv$Best.Latitude..dd. < max(lat.range) & surv$Best.Latitude..dd. > min(lat.range) &
  surv$Best.Longitude..dd. < max(lon.range) & surv$Best.Longitude..dd. > min(lon.range)
# add points
#points3d(x=surv$Best.Longitude..dd.[sub], y=surv$Best.Latitude..dd.[sub],
#         z=-surv$Best.Depth..m.[sub],
#         col=colfun(surv$Temperature.At.the.Gear..degs.C.[sub]))
## spheres3d(x=surv$Best.Longitude..dd.[sub], y=surv$Best.Latitude..dd.[sub],
##           z=-surv$Best.Depth..m.[sub],
##           col=colfun(surv$Temperature.At.the.Gear..degs.C.[sub]),
##           radius=2e6)
# get info on density of a particular fish
fish1 <- fish[fish$Species=="Anoplopoma fimbria",] # Sablefish
fish1 <- fish[fish$Species=="Sebastolobus alascanus",] # Shortspine Thornyhead
fish1 <- fish[fish$Species=="Sebastes crameri",] # Darkblotched Rockfish
surv2 <- surv
surv2$fish1.Haul.Weight..kg. <- 0
for(irow in 1:nrow(surv2)){
  Trawl.Id <- fish1$Trawl.Id[irow]
  row <- which(fish1$Trawl.Id==Trawl.Id)
  if(length(row)==1){
    surv2$fish1.Haul.Weight..kg.[irow] <- fish1$Haul.Weight..kg.[row]
  }
}
surv2$fish1.density <- surv2$fish1.Haul.Weight..kg./surv2$Area.Swept.by.the.Net..hectares.
spheres3d(x=surv$Best.Longitude..dd.[sub], y=surv$Best.Latitude..dd.[sub],
          z=-surv$Best.Depth..m.[sub],
          col=colfun(surv$Temperature.At.the.Gear..degs.C.[sub]),
          radius=1e5+1e6*(surv2$fish1.density^(1/3)))

light3d(specular="black") 
rgl.pop("lights") 



#aspect3d(x=1,y=2,z=.5)
stretch <- 50 # multiplication of depth in aspect ratio
aspect3d(x=x.range.m,y=y.range.m,z=stretch*1280)
title3d(xlab="Longitude", ylab="Latitude",zlab="Depth (m)")
axes3d()
box3d()
