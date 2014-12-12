# stuff from Ian

# data from http://oceanadapt.rutgers.edu/

# plotting temperature from trawl survey
if(!exists("surv")){
  dir <- 'C:/github/random/survey_temp_figs'
  surv <- read.csv(file.path(dir,'wcann_haul.csv'))
}
range(surv$Temperature.At.the.Gear..degs.C.,na.rm=TRUE)
#[1]  2.91 14.70

## redyellowblue <- colorRampPalette(colors=c("blue","yellow","red"))
## colvec <- redyellowblue(100)
## colvec <- rev(rainbow(120)[1:100])
library(r4ss)
colvec <- rich.colors.short(100)
colfun <- function(x){
  colvec[round(100*(x/13)^.8)]
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

#getting relative distances
#http://wiki.cbr.washington.edu/qerm/index.php/R/Converting_Geographic_Coordinates_to_Kilometers
Long <- c(-122,-120)
Lat <- c(34,36)
lon0 <- mean(range(Long))
lat0 <- mean(range(Lat))
rx <- 6371 * acos(sin(lat0 *pi/180)^2 + cos(lat0*pi/180)^2 * cos((lon0+.5)*pi/180 - (lon0-.5)*pi/180))
ry <- 6371 * acos(sin((lat0 -.5)*pi/180) *  sin((lat0+.5)*pi/180) + cos((lat0-.5)*pi/180) * cos((lat0+.5)*pi/180))
x.km <-(Long-lon0)*rx
y.km <-(Lat-lat0)*ry
x.range.m <- 1000*diff(x.km)
y.range.m <- 1000*diff(y.km)

clear3d()
sub <- surv$Best.Latitude..dd. < 36 & surv$Best.Latitude..dd. > 34
points3d(x=surv$Best.Longitude..dd.[sub], y=surv$Best.Latitude..dd.[sub],
         z=-surv$Best.Depth..m.[sub],
         col=colfun(surv$Temperature.At.the.Gear..degs.C.[sub]))
#aspect3d(x=1,y=2,z=.5)
stretch <- 50 # multiplication of depth in aspect ratio
aspect3d(x=x.range.m,y=y.range.m,z=stretch*1280)
title3d(xlab="Longitude", ylab="Latitude",zlab="Depth (m)")
axes3d()
box3d()

# next steps
# add bathymetric data following Eli Gurarie's approach:
# http://wiki.cbr.washington.edu/qerm/index.php/Bathymetry_data

#format of  etopo1.xyz
-130 50 -2282
-129.98333333333332 50 -2315
-129.966666666666669 50 -2330
-129.949999999999989 50 -2343
-129.933333333333337 50 -2367
-129.916666666666657 50 -2401
-129.900000000000006 50 -2446
