#5.8
#ethan
da1=scan("NOAAGlobalTemp.gridded.v4.0.1.201711.asc.gz")
#From Jan 1880 to Jan 2017
length(da1)
#[1] 4267130
da1[1:3]
#[1]    1.0 1880.0 -999.9 #means mon, year, temp
#data in 72 rows (2.5, ..., 357.5) and 
#data in 36 columns (-87.5, ..., 87.5)
tm1=seq(1,4267129, by=2594)
tm2=seq(2,4267130, by=2594)
length(tm1)
length(tm2)
mm1=da1[tm1] #Extract months
yy1=da1[tm2] #Extract years
head(mm1)
head(yy1)
length(mm1)
length(yy1)
rw1<-paste(yy1, sep="-", mm1) #Combine YYYY with MM
head(tm1)
head(tm2)
tm3=cbind(tm1,tm2)
tm4=as.vector(t(tm3))
head(tm4)
#[1]    1    2 2595 2596 5189 5190
da2<-da1[-tm4] #Remote the months and years data from the scanned data
length(da2)/(36*72)
#[1] 1645 #months, 137 yrs 1 mon: Jan 1880-Jan 2017
da3<-matrix(da2,ncol=1645) #Generate the space-time data
#2592 (=36*72) rows and 1645 months (=137 yrs 1 mon)
dim(da3)
#[1] 2592 1645

#Put space-time coordinates in the space-time data da3
colnames(da3)<-rw1
lat1=seq(-87.5, 87.5, length=36)
lon1=seq(2.5, 357.5,  length=72)
LAT=rep(lat1, each=72)
LON=rep(lon1,36)
gpcpst=cbind(LAT, LON, da3)
#head(gpcpst)
dim(gpcpst)
#[1] 2592 1647 #The first two columns are Lat and Lon
#-87.5 to 87.5 and then 2.5 to 375.5
#The first row for time is header, not counted as data.
gpcpst[1:3,1:6] #Part of the data
#       LAT  LON 1880-1 1880-2 1880-3 1880-4
#[1,] -87.5  2.5 -999.9 -999.9 -999.9 -999.9
#[2,] -87.5  7.5 -999.9 -999.9 -999.9 -999.9
#[3,] -87.5 12.5 -999.9 -999.9 -999.9 -999.9

write.csv(gpcpst,file="NOAAGlobalT.csv")
#Output the data as a csv file
#Student can start from here by reading the csv data

#(a) 
#If you read the NOAAGlobalT.csv file, you can start from here
gpcpst1 = read.csv("NOAAGlobalT.csv", header=TRUE) #Read the csv data
gpcpst1[1:3,1:4]
#  X   LAT  LON X1880.1
#1 1 -87.5  2.5  -999.9
#2 2 -87.5  7.5  -999.9
#3 3 -87.5 12.5  -999.9
gpcpst = gpcpst1[,-1] #Remove the first column caused by R reading
gpcpst[1:3,1:4]
#    LAT  LON X1880.1 X1880.2
#1 -87.5  2.5  -999.9  -999.9
#2 -87.5  7.5  -999.9  -999.9
#3 -87.5 12.5  -999.9  -999.9
#Now the first columb is latitude and the second is the longitude

#Select only the data for the tropical Pacific region
n2<-which(gpcpst[,1]>-20&gpcpst[,1]<20&gpcpst[,2]>160&gpcpst[,2]<260)
dim(gpcpst)
length(n2)
#[1] 160 (=8 latitude bends and 20 longitude bends)
pacificdat=gpcpst[n2,855:1454]
dim(pacificdat)
#[1] 160 600 
#160 = 8X20 grid boxes, 600 months = 50 years from Jan 1951-Dec 2000. 

max(pacificdat)
#[1] 3.6947
min(pacificdat)
#[1] -999.9  This means that there are some missing data. 
#We replace these missing data by 0. 
for (i in 1:160){
  for (j in 1:600) {if(pacificdat[i,j] < -800) pacificdat[i,j]=0}
}

min(pacificdat)
#[1] -2.6251   This missing data have been replaced by 0.

pacificdat[1:3,1:6]
#1951-1 1951-2  1951-3  1951-4  1951-5  1951-6
#[1,] 0.4194 0.0912 -0.1753 -0.4174 -0.4529 -0.6348
#[2,] 0.5007 0.1931  0.0420 -0.2839 -0.2665 -0.3898
#[3,] 0.6628 0.4094  0.2788 -0.1062  0.0167 -0.1304

#Make annual data from the July-June  12-month mean
pacificann = matrix(0, nrow=160, ncol=49)
for(k in 1:49){pacificann[,k] = rowMeans(pacificdat[,(k*12-5):(k*12 + 6)])}
dim(pacificann)
#[1] 160  49 #49 years of annual data 1951-1999: from July to next June for a year


pacificann[1:3,1:6]
#           [,1]        [,2]       [,3]       [,4]       [,5]         [,6]
#[1,] -0.4937833 -0.30995833 -0.2309667 -0.5114833 -0.0084250 -0.072525000
#[2,] -0.3395667 -0.19125000 -0.1963417 -0.3984750  0.1047417 -0.008958333
#[3,] -0.1903750  0.01785833 -0.1073167 -0.2726917  0.2298583  0.108666667

#SVD analysis of the annual data
svdP =svd(pacificann)
d=svdP$d
U=svdP$u
V=svdP$v
round(d[1:10],digits=2)
# [1] 457.14 235.76 179.46 149.91  33.39  14.41   9.96   5.65   5.42   4.44
#These are the first ten eigenvalues from the SVD analysis

#(b) Plot the first three eigenvectors U[,1:3]

#Plot EOF1
plot.new()
u=U[,1]
Lat=seq(-17.5,17.5, by=5)
Lon=seq(162.5, 257.5, by=5)
par(mar=c(4,5,3,0))
mapmat=matrix(u, nrow=20)
int=seq(-0.2,0.2,length.out=81)
rgb.palette=colorRampPalette(c('black','blue', 'darkgreen',
                               'green', 'yellow','pink','red','maroon'),interpolate='spline')
library(maps)
filled.contour(Lon, Lat, mapmat, color.palette=rgb.palette, levels=int,
               xlim=c(120,300),ylim=c(-40,40),
               plot.title=title(main="EOF1 of the Tropical Pacific Temp Data",
                                xlab="Latitude",ylab="Longitude", cex.lab=1.5),
               plot.axes={axis(1, cex.axis=1.5); axis(2, cex.axis=1.5);
                 map('world2', add=TRUE);grid()},
               key.title=title(main="[oC]"),
               key.axes={axis(4, cex.axis=1.5)})

#Plot EOF2
plot.new()
u=U[,2]
Lat=seq(-17.5,17.5, by=5)
Lon=seq(162.5, 257.5, by=5)
par(mar=c(4,5,3,0))
mapmat=matrix(u, nrow=20)
int=seq(-0.2,0.2,length.out=81)
rgb.palette=colorRampPalette(c('black','blue', 'darkgreen',
                               'green', 'yellow','pink','red','maroon'), interpolate='spline')
filled.contour(Lon, Lat, mapmat, color.palette=rgb.palette, levels=int,
               xlim=c(120,300),ylim=c(-40,40),
               plot.title=title(main="EOF2 of the Tropical Pacific Temp Data",
                                xlab="Latitude",ylab="Longitude", cex.lab=1.5),
               plot.axes={axis(1, cex.axis=1.5); axis(2, cex.axis=1.5);
                 map('world2', add=TRUE);grid()},
               key.title=title(main="[oC]"),
               key.axes={axis(4, cex.axis=1.5)})

#Plot EOF3
plot.new()
u=U[,3]
Lat=seq(-17.5,17.5, by=5)
Lon=seq(162.5, 257.5, by=5)
par(mar=c(4,5,3,0))
mapmat=matrix(u, nrow=20)
int=seq(-0.2,0.2,length.out=81)
rgb.palette=colorRampPalette(c('black','blue', 'darkgreen',
                               'green', 'yellow','pink','red','maroon'),interpolate='spline')
filled.contour(Lon, Lat, mapmat, color.palette=rgb.palette, levels=int,
               xlim=c(120,300),ylim=c(-40,40),
               plot.title=title(main="EOF3 of the Tropical Pacific Temp Data",
                                xlab="Latitude",ylab="Longitude", cex.lab=1.5),
               plot.axes={axis(1, cex.axis=1.5); axis(2, cex.axis=1.5);
                 map('world2', add=TRUE);grid()},
               key.title=title(main="[oC]"),
               key.axes={axis(4, cex.axis=1.5)})

#(c) Plot three PCs V[,1:3]
par(mar=c(4.5,4.5,2.5,1))
time=1951:1999
plot(time, V[,1], type="l", lwd=2, 
     xlab="Year", ylab="PC scale",
     main="The first three PCs", ylim=c(-0.4,0.4))
legend(1950,0.45, 'PC1',lwd=2, bty='n')
lines(time, V[,2], type="l", lwd=2, col="blue")
legend(1950,0.40, 'PC2',lwd=2, bty='n', col="blue")
lines(time, V[,3], type="l", lwd=2, col="red")
legend(1950,0.35, 'PC3',lwd=2, bty='n', col="red")
#5.9
prcprecon <- read.csv("PrcpRecon5degAnn.csv")
dim(prcprecon)


tropical <- which(prcprecon[,1]>-20&prcprecon[,1]<20&prcprecon[,2]>160&prcprecon[,2]<260)
dim(prcprecon)

length(tropical)
paccc = prcprecon[tropical, 0:50]
paccc[paccc< -900.00] <- 0
dim(paccc)


SVDppp = svd(paccc)
U = SVDppp$u
D = SVDppp$d
V = SVDppp$v

D[1:10]


#5.9b
paccc[,1]
paccc[,2]

plot.new()
u=U[,1]
EOflatone = seq(-17.5,17.5, by=5)
EOflongone = seq(162.5, 257.5, by=5)
par(mar=c(4,5,3,0))
matrixm = matrix(u, nrow=20, byrow=TRUE)
matrixm=pmax(pmin(matrixm,0.3),-0.3)
seqnum = seq(-0.3,0.3,length.out=75)
rgb.palette=colorRampPalette(c('black','blue', 'darkgreen','green', 'yellow','pink','red','maroon'),interpolate='spline')
filled.contour(EOflongone, EOflatone, matrixm, color.palette = rgb.palette, levels = seqnum, 
               plot.title = title(main="Annual tropical Pacific Precipitation Anomolies' EOF1", xlab ="Latitude",ylab ="Longitude", cex.lab=1.5, cex.main=.5),
               plot.axes={axis(1, cex.axis=1.5); axis(2, cex.axis=1.5);grid()}, key.title=title(main="oC"), key.axes={axis(4, cex.axis=1.5)})

plot.new()
u=U[,2]
EOflatone = seq(-17.5,17.5, by=5)
EOflongone = seq(162.5, 257.5, by=5)
par(mar=c(4,5,3,0))
matrixm = matrix(u, nrow=20, byrow=TRUE)
matrixm=pmax(pmin(matrixm,0.3),-0.1)
seqnum = seq(-0.3,0.3,length.out=75)
rgb.palette=colorRampPalette(c('black','blue', 'darkgreen','green', 'yellow','pink','red','maroon'),interpolate='spline')
filled.contour(EOflongone, EOflatone, matrixm, color.palette = rgb.palette, levels = seqnum,
               plot.title = title(main="Annual tropical Pacific Precipitation Anomolies' EOF2", xlab ="Latitude", cex.main =.5,ylab ="Longitude",
                                  cex.lab=1.5), plot.axes={axis(1, cex.axis=1.5); axis(2, cex.axis=1.5);grid()}, key.title=title(main="[oC]"), key.axes={axis(4, cex.axis=1.5)})

plot.new()
u=U[,3]
EOflatone = seq(-17.5,17.5, by=5)
EOflongone = seq(162.5, 257.5, by=5)
par(mar=c(4,5,3,0))
matrixm = matrix(u, nrow=20, byrow=TRUE)
matrixm=pmax(pmin(matrixm,0.1),-0.1)
seqnum = seq(-0.3,0.3,length.out=75)
rgb.palette=colorRampPalette(c('black','blue', 'darkgreen','green', 'yellow','pink','red','maroon'),interpolate='spline')
filled.contour(EOflongone, EOflatone, matrixm, color.palette = rgb.palette, levels = seqnum, plot.title = title(main="Annual Tropical Pacific Precipitation Anomolies' EOF3", xlab ="Latitude",ylab ="Longitude", cex.lab=1.5, cex.main=.5)
               , plot.axes={axis(1, cex.axis=1.5); axis(2, cex.axis=1.5);grid()}, key.title=title(main="[oC]"), key.axes={axis(4, cex.axis=1.5)})


#5.9c
par(mar=c(4,4,3,1))
t <- seq(1951,2000)
plot(t, V[,1], type="l", lwd=2, xlab="Year", ylab="PC Scale",main="First three PC (Principal Components) of Annual Tropical Pacific precipitation", ylim=c(-0.4,0.7), cex.main = .6)
legend(1970,0.6, 'PC1',lwd=2, bty='n')
lines(t, V[,2], type="l", lwd=2, col="blue")
legend(1970, 0.5, 'PC2',lwd=2, bty='n', col="blue")
lines(t, V[,3], type="l", lwd=2, col="red")
legend(1970, 0.4, 'PC3',lwd=2, bty='n', col="red")
#8 
#note that length and d are 1
numerator = 2
pie = 3.14
ps=numerator/pie
ps
#[1] 0.6369427


#9
N=100000
nn=8
x=matrix(runif(nn*N),ncol=nn)
k=0
for(i in 1:N){if((t(x[i,])%*%x[i,]) < 1) {k=k+1}}
k
ex = (k/N)*2^nn
ex
#4.07296 is volume. 
#The exact answer is 4.0587
#can use the general formula also as seen below
gen = pi^(nn/2)/gamma(nn/2 +1)
gen

#10
x=y=1:6
n=100000
v=0
for (i in 1:n) {if(sample(x,1)+sample(y,1) == 7) v=v+1}
v/n
# ran this code and got 0.16619 which is approximately 1/6.
# code will give a different k/n each time but will be close to 1/6
