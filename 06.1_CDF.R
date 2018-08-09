##############################################################################################
#       CALCULATE CUMULATIVE DISTRIBUTION FUNCTIONS FOR EACH SPECIES GROUP IN EACH HABITAT   #
##############################################################################################

#code by Jan Divisek (2018)

##CDFs are based on species distnaces from the centroid of native species in 3D trait space of each habitat

###RANDOMIZE INVASIVE AND NATURALIZED SPECIES---------------------------------------------------------------------------------
#To test the difference between natives and invasives and between natives and naturalized species
par(mar=c(4, 4, 2.5, 1.5))

xAxis <- list()
yAxis <- list()

###Grassland and heathland vegetation ---------------------------------------------------------------------------

#calculate centroid of native species
centr <- apply(traitT.3D[which(traitT.3D$INVASION.STATUS == "native"), c(1:3)], 2, mean)
centr <- rbind(centr, traitT.3D[,c(1:3)])

#calculate distances of species from observed native centroid
d.centr <- as.matrix(dist(centr))[,1]
d.centr <- d.centr[-1]

xAxis$T <- seq(from=0, to=max(d.centr), length=1000)

yAxis$T$obs$native <- vapply(xAxis$T, calcCDF, FUN.VALUE=1, d.centr[which(traitT.3D$INVASION.STATUS == "native")])
yAxis$T$obs$natur <- vapply(xAxis$T, calcCDF, FUN.VALUE=1, d.centr[which(traitT.3D$INVASION.STATUS == "naturalized")])
yAxis$T$obs$invasive <- vapply(xAxis$T, calcCDF, FUN.VALUE=1, d.centr[which(traitT.3D$INVASION.STATUS == "invasive")])

#randomize distances between natives and invasive
set.seed(1234)
yAxis$T$rand$inv <- dist.shuffler.3D(scores=traitT.3D[,1:3], status=traitT.3D$INVASION.STATUS, test="invasive", Nrand=999)

#randomize distances between natives and naturalized
set.seed(1234)
yAxis$T$rand$natur <- dist.shuffler.3D(scores=traitT.3D[,1:3], status=traitT.3D$INVASION.STATUS, test="naturalized", Nrand=999)

#CDF plot
plot(xAxis$T, yAxis$T$obs$native, type='l', axes=T, xlab="Distance from centroid", ylab="Proportion of species", main="Grassland and heathland vegetation", lwd=3,bty="n", col="blue", las=1, cex.lab=1.2, cex.axis=1.2, cex.main=1.2)
lines(xAxis$T, yAxis$T$obs$natur, type='l', lwd=3, col="yellow")
lines(xAxis$T, yAxis$T$obs$invasive, type='l', lwd=3, col="red3")

###Ruderal and weed vegetation ---------------------------------------------------------------------------

#calculate centroid of native species
centr <- apply(traitX.3D[which(traitX.3D$INVASION.STATUS == "native"), c(1:3)], 2, mean)
centr <- rbind(centr, traitX.3D[,c(1:3)])

#calculate distances of species from observed native centroid
d.centr <- as.matrix(dist(centr))[,1]
d.centr <- d.centr[-1]

xAxis$X <- seq(from=0, to=max(d.centr), length=1000)

yAxis$X$obs$native <- vapply(xAxis$X, calcCDF, FUN.VALUE=1, d.centr[which(traitX.3D$INVASION.STATUS == "native")])
yAxis$X$obs$natur <- vapply(xAxis$X, calcCDF, FUN.VALUE=1, d.centr[which(traitX.3D$INVASION.STATUS == "naturalized")])
yAxis$X$obs$invasive <- vapply(xAxis$X, calcCDF, FUN.VALUE=1, d.centr[which(traitX.3D$INVASION.STATUS == "invasive")])

#randomize distances between natives and invasive
set.seed(1234)
yAxis$X$rand$inv <- dist.shuffler.3D(scores=traitX.3D[,1:3], status=traitX.3D$INVASION.STATUS, test="invasive", Nrand=999)

#randomize distances between natives and naturalized
set.seed(1234)
yAxis$X$rand$natur <- dist.shuffler.3D(scores=traitX.3D[,1:3], status=traitX.3D$INVASION.STATUS, test="naturalized", Nrand=999)

#CDF plot
plot(xAxis$X, yAxis$X$obs$native, type='l', axes=T, xlab="Distance from centroid", ylab="Proportion of species", main="Ruderal and weed vegetation", lwd=3,bty="n", col="blue", las=1, cex.lab=1.2, cex.axis=1.2, cex.main=1.2)
lines(xAxis$X, yAxis$X$obs$natur, type='l', lwd=3, col="yellow")
lines(xAxis$X, yAxis$X$obs$invasive, type='l', lwd=3, col="red3")

###Rock and scree vegetation ---------------------------------------------------------------------------

#calculate centroid of native species
centr <- apply(traitS.3D[which(traitS.3D$INVASION.STATUS == "native"), c(1:3)], 2, mean)
centr <- rbind(centr, traitS.3D[,c(1:3)])

#calculate distances of species from observed native centroid
d.centr <- as.matrix(dist(centr))[,1]
d.centr <- d.centr[-1]

xAxis$S <- seq(from=0, to=max(d.centr), length=1000)

yAxis$S$obs$native <- vapply(xAxis$S, calcCDF, FUN.VALUE=1, d.centr[which(traitS.3D$INVASION.STATUS == "native")])
yAxis$S$obs$natur <- vapply(xAxis$S, calcCDF, FUN.VALUE=1, d.centr[which(traitS.3D$INVASION.STATUS == "naturalized")])
yAxis$S$obs$invasive <- vapply(xAxis$S, calcCDF, FUN.VALUE=1, d.centr[which(traitS.3D$INVASION.STATUS == "invasive")])

#randomize distances between natives and invasive
set.seed(1234)
yAxis$S$rand$inv <- dist.shuffler.3D(scores=traitS.3D[,1:3], status=traitS.3D$INVASION.STATUS, test="invasive", Nrand=999)

#randomize distances between natives and naturalized
set.seed(1234)
yAxis$S$rand$natur <- dist.shuffler.3D(scores=traitS.3D[,1:3], status=traitS.3D$INVASION.STATUS, test="naturalized", Nrand=999)

#CDF plot
plot(xAxis$S, yAxis$S$obs$native, type='l', axes=T, xlab="Distance from centroid", ylab="Proportion of species", main="Rock and scree vegetation", lwd=3,bty="n", col="blue", las=1, cex.lab=1.2, cex.axis=1.2, cex.main=1.2)
lines(xAxis$S, yAxis$S$obs$natur, type='l', lwd=3, col="yellow")
lines(xAxis$S, yAxis$S$obs$invasive, type='l', lwd=3, col="red3")

###Wetland vegetation ---------------------------------------------------------------------------

#calculate centroid of native species
centr <- apply(traitM.3D[which(traitM.3D$INVASION.STATUS == "native"), c(1:3)], 2, mean)
centr <- rbind(centr, traitM.3D[,c(1:3)])

#calculate distances of species from observed native centroid
d.centr <- as.matrix(dist(centr))[,1]
d.centr <- d.centr[-1]

xAxis$M <- seq(from=0, to=max(d.centr), length=1000)

yAxis$M$obs$native <- vapply(xAxis$M, calcCDF, FUN.VALUE=1, d.centr[which(traitM.3D$INVASION.STATUS == "native")])
yAxis$M$obs$natur <- vapply(xAxis$M, calcCDF, FUN.VALUE=1, d.centr[which(traitM.3D$INVASION.STATUS == "naturalized")])
yAxis$M$obs$invasive <- vapply(xAxis$M, calcCDF, FUN.VALUE=1, d.centr[which(traitM.3D$INVASION.STATUS == "invasive")])

#randomize distances between natives and invasive
set.seed(1234)
yAxis$M$rand$inv <- dist.shuffler.3D(scores=traitM.3D[,1:3], status=traitM.3D$INVASION.STATUS, test="invasive", Nrand=999)

#randomize distances between natives and naturalized
set.seed(1234)
yAxis$M$rand$natur <- dist.shuffler.3D(scores=traitM.3D[,1:3], status=traitM.3D$INVASION.STATUS, test="naturalized", Nrand=999)

#CDF plot
plot(xAxis$M, yAxis$M$obs$native, type='l', axes=T, xlab="Distance from centroid", ylab="Proportion of species", main="Wetland vegetation", lwd=3,bty="n", col="blue", las=1, cex.lab=1.2, cex.axis=1.2, cex.main=1.2)
lines(xAxis$M, yAxis$M$obs$natur, type='l', lwd=3, col="yellow")
lines(xAxis$M, yAxis$M$obs$invasive, type='l', lwd=3, col="red3")

###Scrub vegetation ---------------------------------------------------------------------------

#calculate centroid of native species
centr <- apply(traitK.3D[which(traitK.3D$INVASION.STATUS == "native"), c(1:3)], 2, mean)
centr <- rbind(centr, traitK.3D[,c(1:3)])

#calculate distances of species from observed native centroid
d.centr <- as.matrix(dist(centr))[,1]
d.centr <- d.centr[-1]

xAxis$K <- seq(from=0, to=max(d.centr), length=1000)

yAxis$K$obs$native <- vapply(xAxis$K, calcCDF, FUN.VALUE=1, d.centr[which(traitK.3D$INVASION.STATUS == "native")])
yAxis$K$obs$natur <- vapply(xAxis$K, calcCDF, FUN.VALUE=1, d.centr[which(traitK.3D$INVASION.STATUS == "naturalized")])
yAxis$K$obs$invasive <- vapply(xAxis$K, calcCDF, FUN.VALUE=1, d.centr[which(traitK.3D$INVASION.STATUS == "invasive")])

#randomize distances between natives and invasive
set.seed(1234)
yAxis$K$rand$inv <- dist.shuffler.3D(scores=traitK.3D[,1:3], status=traitK.3D$INVASION.STATUS, test="invasive", Nrand=999)

#randomize distances between natives and naturalized
set.seed(1234)
yAxis$K$rand$natur <- dist.shuffler.3D(scores=traitK.3D[,1:3], status=traitK.3D$INVASION.STATUS, test="naturalized", Nrand=999)

#CDF plot
plot(xAxis$K, yAxis$K$obs$native, type='l', axes=T, xlab="Distance from centroid", ylab="Proportion of species", main="Scrub vegetation", lwd=3,bty="n", col="blue", las=1, cex.lab=1.2, cex.axis=1.2, cex.main=1.2)
lines(xAxis$K, yAxis$K$obs$natur, type='l', lwd=3, col="yellow")
lines(xAxis$K, yAxis$K$obs$invasive, type='l', lwd=3, col="red3")

###Forest vegetation ---------------------------------------------------------------------------

#calculate centroid of native species
centr <- apply(traitL.3D[which(traitL.3D$INVASION.STATUS == "native"), c(1:3)], 2, mean)
centr <- rbind(centr, traitL.3D[,c(1:3)])

#calculate distances of species from observed native centroid
d.centr <- as.matrix(dist(centr))[,1]
d.centr <- d.centr[-1]

xAxis$L <- seq(from=0, to=max(d.centr), length=1000)

yAxis$L$obs$native <- vapply(xAxis$L, calcCDF, FUN.VALUE=1, d.centr[which(traitL.3D$INVASION.STATUS == "native")])
yAxis$L$obs$natur <- vapply(xAxis$L, calcCDF, FUN.VALUE=1, d.centr[which(traitL.3D$INVASION.STATUS == "naturalized")])
yAxis$L$obs$invasive <- vapply(xAxis$L, calcCDF, FUN.VALUE=1, d.centr[which(traitL.3D$INVASION.STATUS == "invasive")])

#randomize distances between natives and invasive
set.seed(1234)
yAxis$L$rand$inv <- dist.shuffler.3D(scores=traitL.3D[,1:3], status=traitL.3D$INVASION.STATUS, test="invasive", Nrand=999)

#randomize distances between natives and naturalized
set.seed(1234)
yAxis$L$rand$natur <- dist.shuffler.3D(scores=traitL.3D[,1:3], status=traitL.3D$INVASION.STATUS, test="naturalized", Nrand=999)

#CDF plot
plot(xAxis$L, yAxis$L$obs$native, type='l', axes=T, xlab="Distance from centroid", ylab="Proportion of species", main="Forest vegetation", lwd=3,bty="n", col="blue", las=1, cex.lab=1.2, cex.axis=1.2, cex.main=1.2)
lines(xAxis$L, yAxis$L$obs$natur, type='l', lwd=3, col="yellow")
lines(xAxis$L, yAxis$L$obs$invasive, type='l', lwd=3, col="red3")

#############################################################################################
###RANDOMIZE INVASIVE AND NATURALIZED SPECIES IN DATASET WITH IMPUTED SPECIES TRAITS---------
#To test the difference between natives and invasives and between natives and naturalized species
par(mar=c(4, 4, 2.5, 1.5))

xAxis.imp <- list()
yAxis.imp <- list()

###Grassland and heathland vegetation ---------------------------------------------------------------------------

#calculate centroid of native species
centr <- apply(traitT.3D.imp[which(traitT.3D.imp$INVASION.STATUS == "native"), c(1:3)], 2, mean)
centr <- rbind(centr, traitT.3D.imp[,c(1:3)])

#calculate distances of species from observed native centroid
d.centr <- as.matrix(dist(centr))[,1]
d.centr <- d.centr[-1]

xAxis.imp$T <- seq(from=0, to=max(d.centr), length=1000)

yAxis.imp$T$obs$native <- vapply(xAxis.imp$T, calcCDF, FUN.VALUE=1, d.centr[which(traitT.3D.imp$INVASION.STATUS == "native")])
yAxis.imp$T$obs$natur <- vapply(xAxis.imp$T, calcCDF, FUN.VALUE=1, d.centr[which(traitT.3D.imp$INVASION.STATUS == "naturalized")])
yAxis.imp$T$obs$invasive <- vapply(xAxis.imp$T, calcCDF, FUN.VALUE=1, d.centr[which(traitT.3D.imp$INVASION.STATUS == "invasive")])

#randomize distances between natives and invasive
set.seed(1234)
yAxis.imp$T$rand$inv <- dist.shuffler.3D(scores=traitT.3D.imp[,1:3], status=traitT.3D.imp$INVASION.STATUS, test="invasive", Nrand=999)

#randomize distances between natives and naturalized
set.seed(1234)
yAxis.imp$T$rand$natur <- dist.shuffler.3D(scores=traitT.3D.imp[,1:3], status=traitT.3D.imp$INVASION.STATUS, test="naturalized", Nrand=999)

#CDF plot
plot(xAxis.imp$T, yAxis.imp$T$obs$native, type='l', axes=T, xlab="Distance from centroid", ylab="Proportion of species", main="Grassland and heathland vegetation", lwd=3,bty="n", col="blue", las=1, cex.lab=1.2, cex.axis=1.2, cex.main=1.2)
lines(xAxis.imp$T, yAxis.imp$T$obs$natur, type='l', lwd=3, col="yellow")
lines(xAxis.imp$T, yAxis.imp$T$obs$invasive, type='l', lwd=3, col="red3")

###Ruderal and weed vegetation ---------------------------------------------------------------------------

#calculate centroid of native species
centr <- apply(traitX.3D.imp[which(traitX.3D.imp$INVASION.STATUS == "native"), c(1:3)], 2, mean)
centr <- rbind(centr, traitX.3D.imp[,c(1:3)])

#calculate distances of species from observed native centroid
d.centr <- as.matrix(dist(centr))[,1]
d.centr <- d.centr[-1]

xAxis.imp$X <- seq(from=0, to=max(d.centr), length=1000)

yAxis.imp$X$obs$native <- vapply(xAxis.imp$X, calcCDF, FUN.VALUE=1, d.centr[which(traitX.3D.imp$INVASION.STATUS == "native")])
yAxis.imp$X$obs$natur <- vapply(xAxis.imp$X, calcCDF, FUN.VALUE=1, d.centr[which(traitX.3D.imp$INVASION.STATUS == "naturalized")])
yAxis.imp$X$obs$invasive <- vapply(xAxis.imp$X, calcCDF, FUN.VALUE=1, d.centr[which(traitX.3D.imp$INVASION.STATUS == "invasive")])

#randomize distances between natives and invasive
set.seed(1234)
yAxis.imp$X$rand$inv <- dist.shuffler.3D(scores=traitX.3D.imp[,1:3], status=traitX.3D.imp$INVASION.STATUS, test="invasive", Nrand=999)

#randomize distances between natives and naturalized
set.seed(1234)
yAxis.imp$X$rand$natur <- dist.shuffler.3D(scores=traitX.3D.imp[,1:3], status=traitX.3D.imp$INVASION.STATUS, test="naturalized", Nrand=999)

#CDF plot
plot(xAxis.imp$X, yAxis.imp$X$obs$native, type='l', axes=T, xlab="Distance from centroid", ylab="Proportion of species", main="Ruderal and weed vegetation", lwd=3,bty="n", col="blue", las=1, cex.lab=1.2, cex.axis=1.2, cex.main=1.2)
lines(xAxis.imp$X, yAxis.imp$X$obs$natur, type='l', lwd=3, col="yellow")
lines(xAxis.imp$X, yAxis.imp$X$obs$invasive, type='l', lwd=3, col="red3")

###Rock and scree vegetation ---------------------------------------------------------------------------

#calculate centroid of native species
centr <- apply(traitS.3D.imp[which(traitS.3D.imp$INVASION.STATUS == "native"), c(1:3)], 2, mean)
centr <- rbind(centr, traitS.3D.imp[,c(1:3)])

#calculate distances of species from observed native centroid
d.centr <- as.matrix(dist(centr))[,1]
d.centr <- d.centr[-1]

xAxis.imp$S <- seq(from=0, to=max(d.centr), length=1000)

yAxis.imp$S$obs$native <- vapply(xAxis.imp$S, calcCDF, FUN.VALUE=1, d.centr[which(traitS.3D.imp$INVASION.STATUS == "native")])
yAxis.imp$S$obs$natur <- vapply(xAxis.imp$S, calcCDF, FUN.VALUE=1, d.centr[which(traitS.3D.imp$INVASION.STATUS == "naturalized")])
yAxis.imp$S$obs$invasive <- vapply(xAxis.imp$S, calcCDF, FUN.VALUE=1, d.centr[which(traitS.3D.imp$INVASION.STATUS == "invasive")])

#randomize distances between natives and invasive
set.seed(1234)
yAxis.imp$S$rand$inv <- dist.shuffler.3D(scores=traitS.3D.imp[,1:3], status=traitS.3D.imp$INVASION.STATUS, test="invasive", Nrand=999)

#randomize distances between natives and naturalized
set.seed(1234)
yAxis.imp$S$rand$natur <- dist.shuffler.3D(scores=traitS.3D.imp[,1:3], status=traitS.3D.imp$INVASION.STATUS, test="naturalized", Nrand=999)

#CDF plot
plot(xAxis.imp$S, yAxis.imp$S$obs$native, type='l', axes=T, xlab="Distance from centroid", ylab="Proportion of species", main="Rock and scree vegetation", lwd=3,bty="n", col="blue", las=1, cex.lab=1.2, cex.axis=1.2, cex.main=1.2)
lines(xAxis.imp$S, yAxis.imp$S$obs$natur, type='l', lwd=3, col="yellow")
lines(xAxis.imp$S, yAxis.imp$S$obs$invasive, type='l', lwd=3, col="red3")

###Wetland vegetation ---------------------------------------------------------------------------

#calculate centroid of native species
centr <- apply(traitM.3D.imp[which(traitM.3D.imp$INVASION.STATUS == "native"), c(1:3)], 2, mean)
centr <- rbind(centr, traitM.3D.imp[,c(1:3)])

#calculate distances of species from observed native centroid
d.centr <- as.matrix(dist(centr))[,1]
d.centr <- d.centr[-1]

xAxis.imp$M <- seq(from=0, to=max(d.centr), length=1000)

yAxis.imp$M$obs$native <- vapply(xAxis.imp$M, calcCDF, FUN.VALUE=1, d.centr[which(traitM.3D.imp$INVASION.STATUS == "native")])
yAxis.imp$M$obs$natur <- vapply(xAxis.imp$M, calcCDF, FUN.VALUE=1, d.centr[which(traitM.3D.imp$INVASION.STATUS == "naturalized")])
yAxis.imp$M$obs$invasive <- vapply(xAxis.imp$M, calcCDF, FUN.VALUE=1, d.centr[which(traitM.3D.imp$INVASION.STATUS == "invasive")])

#randomize distances between natives and invasive
set.seed(1234)
yAxis.imp$M$rand$inv <- dist.shuffler.3D(scores=traitM.3D.imp[,1:3], status=traitM.3D.imp$INVASION.STATUS, test="invasive", Nrand=999)

#randomize distances between natives and naturalized
set.seed(1234)
yAxis.imp$M$rand$natur <- dist.shuffler.3D(scores=traitM.3D.imp[,1:3], status=traitM.3D.imp$INVASION.STATUS, test="naturalized", Nrand=999)

#CDF plot
plot(xAxis.imp$M, yAxis.imp$M$obs$native, type='l', axes=T, xlab="Distance from centroid", ylab="Proportion of species", main="Wetland vegetation", lwd=3,bty="n", col="blue", las=1, cex.lab=1.2, cex.axis=1.2, cex.main=1.2)
lines(xAxis.imp$M, yAxis.imp$M$obs$natur, type='l', lwd=3, col="yellow")
lines(xAxis.imp$M, yAxis.imp$M$obs$invasive, type='l', lwd=3, col="red3")

###Scrub vegetation ---------------------------------------------------------------------------

#calculate centroid of native species
centr <- apply(traitK.3D.imp[which(traitK.3D.imp$INVASION.STATUS == "native"), c(1:3)], 2, mean)
centr <- rbind(centr, traitK.3D.imp[,c(1:3)])

#calculate distances of species from observed native centroid
d.centr <- as.matrix(dist(centr))[,1]
d.centr <- d.centr[-1]

xAxis.imp$K <- seq(from=0, to=max(d.centr), length=1000)

yAxis.imp$K$obs$native <- vapply(xAxis.imp$K, calcCDF, FUN.VALUE=1, d.centr[which(traitK.3D.imp$INVASION.STATUS == "native")])
yAxis.imp$K$obs$natur <- vapply(xAxis.imp$K, calcCDF, FUN.VALUE=1, d.centr[which(traitK.3D.imp$INVASION.STATUS == "naturalized")])
yAxis.imp$K$obs$invasive <- vapply(xAxis.imp$K, calcCDF, FUN.VALUE=1, d.centr[which(traitK.3D.imp$INVASION.STATUS == "invasive")])

#randomize distances between natives and invasive
set.seed(1234)
yAxis.imp$K$rand$inv <- dist.shuffler.3D(scores=traitK.3D.imp[,1:3], status=traitK.3D.imp$INVASION.STATUS, test="invasive", Nrand=999)

#randomize distances between natives and naturalized
set.seed(1234)
yAxis.imp$K$rand$natur <- dist.shuffler.3D(scores=traitK.3D.imp[,1:3], status=traitK.3D.imp$INVASION.STATUS, test="naturalized", Nrand=999)

#CDF plot
plot(xAxis.imp$K, yAxis.imp$K$obs$native, type='l', axes=T, xlab="Distance from centroid", ylab="Proportion of species", main="Scrub vegetation", lwd=3,bty="n", col="blue", las=1, cex.lab=1.2, cex.axis=1.2, cex.main=1.2)
lines(xAxis.imp$K, yAxis.imp$K$obs$natur, type='l', lwd=3, col="yellow")
lines(xAxis.imp$K, yAxis.imp$K$obs$invasive, type='l', lwd=3, col="red3")

###Forest vegetation ---------------------------------------------------------------------------

#calculate centroid of native species
centr <- apply(traitL.3D.imp[which(traitL.3D.imp$INVASION.STATUS == "native"), c(1:3)], 2, mean)
centr <- rbind(centr, traitL.3D.imp[,c(1:3)])

#calculate distances of species from observed native centroid
d.centr <- as.matrix(dist(centr))[,1]
d.centr <- d.centr[-1]

xAxis.imp$L <- seq(from=0, to=max(d.centr), length=1000)

yAxis.imp$L$obs$native <- vapply(xAxis.imp$L, calcCDF, FUN.VALUE=1, d.centr[which(traitL.3D.imp$INVASION.STATUS == "native")])
yAxis.imp$L$obs$natur <- vapply(xAxis.imp$L, calcCDF, FUN.VALUE=1, d.centr[which(traitL.3D.imp$INVASION.STATUS == "naturalized")])
yAxis.imp$L$obs$invasive <- vapply(xAxis.imp$L, calcCDF, FUN.VALUE=1, d.centr[which(traitL.3D.imp$INVASION.STATUS == "invasive")])

#randomize distances between natives and invasive
set.seed(1234)
yAxis.imp$L$rand$inv <- dist.shuffler.3D(scores=traitL.3D.imp[,1:3], status=traitL.3D.imp$INVASION.STATUS, test="invasive", Nrand=999)

#randomize distances between natives and naturalized
set.seed(1234)
yAxis.imp$L$rand$natur <- dist.shuffler.3D(scores=traitL.3D.imp[,1:3], status=traitL.3D.imp$INVASION.STATUS, test="naturalized", Nrand=999)

#CDF plot
plot(xAxis.imp$L, yAxis.imp$L$obs$native, type='l', axes=T, xlab="Distance from centroid", ylab="Proportion of species", main="Forest vegetation", lwd=3,bty="n", col="blue", las=1, cex.lab=1.2, cex.axis=1.2, cex.main=1.2)
lines(xAxis.imp$L, yAxis.imp$L$obs$natur, type='l', lwd=3, col="yellow")
lines(xAxis.imp$L, yAxis.imp$L$obs$invasive, type='l', lwd=3, col="red3")

##############################################################################################
#      CALCULATE CUMULATIVE DISTRIBUTION FUNCTIONS FOR PHYLOGENETICALLY CORRECTED DATA       #
##############################################################################################

###RANDOMIZE INVASIVE AND NATURALIZED SPECIES AFTER ACCOUNTING FOR PHYLOGENETIC SIGNAL-----------
par(mar=c(4, 4, 2.5, 1.5))

xAxis.cor <- list()
yAxis.cor <- list()

###Grassland and heathland vegetation ---------------------------------------------------------------------------

#calculate centroid of native species
centr <- apply(traitT.3Dcor[which(traitT.3Dcor$INVASION.STATUS == "native"), c(1:3)], 2, mean)
centr <- rbind(centr, traitT.3Dcor[,c(1:3)])

#calculate distances of species from observed native centroid
d.centr <- as.matrix(dist(centr))[,1]
d.centr <- d.centr[-1]

xAxis.cor$T <- seq(from=0, to=max(d.centr), length=1000)

yAxis.cor$T$obs$native <- vapply(xAxis.cor$T, calcCDF, FUN.VALUE=1, d.centr[which(traitT.3Dcor$INVASION.STATUS == "native")])
yAxis.cor$T$obs$natur <- vapply(xAxis.cor$T, calcCDF, FUN.VALUE=1, d.centr[which(traitT.3Dcor$INVASION.STATUS == "naturalized")])
yAxis.cor$T$obs$invasive <- vapply(xAxis.cor$T, calcCDF, FUN.VALUE=1, d.centr[which(traitT.3Dcor$INVASION.STATUS == "invasive")])

#randomize distances between natives and invasive
set.seed(1234)
yAxis.cor$T$rand$inv <- dist.shuffler.3D(scores=traitT.3Dcor[,1:3], status=traitT.3Dcor$INVASION.STATUS, test="invasive", Nrand=999)

#randomize distances between natives and naturalized
set.seed(1234)
yAxis.cor$T$rand$natur <- dist.shuffler.3D(scores=traitT.3Dcor[,1:3], status=traitT.3Dcor$INVASION.STATUS, test="naturalized", Nrand=999)

#CDF plot
plot(xAxis.cor$T, yAxis.cor$T$obs$native, type='l', axes=T, xlab="Distance from centroid", ylab="Proportion of species", main="Grassland and heathland vegetation", lwd=3,bty="n", col="blue", las=1, cex.lab=1.2, cex.axis=1.2, cex.main=1.2)
lines(xAxis.cor$T, yAxis.cor$T$obs$natur, type='l', lwd=3, col="yellow")
lines(xAxis.cor$T, yAxis.cor$T$obs$invasive, type='l', lwd=3, col="red3")

###Ruderal and weed vegetation ---------------------------------------------------------------------------

#calculate centroid of native species
centr <- apply(traitX.3Dcor[which(traitX.3Dcor$INVASION.STATUS == "native"), c(1:3)], 2, mean)
centr <- rbind(centr, traitX.3Dcor[,c(1:3)])

#calculate distances of species from observed native centroid
d.centr <- as.matrix(dist(centr))[,1]
d.centr <- d.centr[-1]

xAxis.cor$X <- seq(from=0, to=max(d.centr), length=1000)

yAxis.cor$X$obs$native <- vapply(xAxis.cor$X, calcCDF, FUN.VALUE=1, d.centr[which(traitX.3Dcor$INVASION.STATUS == "native")])
yAxis.cor$X$obs$natur <- vapply(xAxis.cor$X, calcCDF, FUN.VALUE=1, d.centr[which(traitX.3Dcor$INVASION.STATUS == "naturalized")])
yAxis.cor$X$obs$invasive <- vapply(xAxis.cor$X, calcCDF, FUN.VALUE=1, d.centr[which(traitX.3Dcor$INVASION.STATUS == "invasive")])

#randomize distances between natives and invasive
set.seed(1234)
yAxis.cor$X$rand$inv <- dist.shuffler.3D(scores=traitX.3Dcor[,1:3], status=traitX.3Dcor$INVASION.STATUS, test="invasive", Nrand=999)

#randomize distances between natives and naturalized
set.seed(1234)
yAxis.cor$X$rand$natur <- dist.shuffler.3D(scores=traitX.3Dcor[,1:3], status=traitX.3Dcor$INVASION.STATUS, test="naturalized", Nrand=999)

#CDF plot
plot(xAxis.cor$X, yAxis.cor$X$obs$native, type='l', axes=T, xlab="Distance from centroid", ylab="Proportion of species", main="Ruderal and weed vegetation", lwd=3,bty="n", col="blue", las=1, cex.lab=1.2, cex.axis=1.2, cex.main=1.2)
lines(xAxis.cor$X, yAxis.cor$X$obs$natur, type='l', lwd=3, col="yellow")
lines(xAxis.cor$X, yAxis.cor$X$obs$invasive, type='l', lwd=3, col="red3")

###Rock and scree vegetation ---------------------------------------------------------------------------

#calculate centroid of native species
centr <- apply(traitS.3Dcor[which(traitS.3Dcor$INVASION.STATUS == "native"), c(1:3)], 2, mean)
centr <- rbind(centr, traitS.3Dcor[,c(1:3)])

#calculate distances of species from observed native centroid
d.centr <- as.matrix(dist(centr))[,1]
d.centr <- d.centr[-1]

xAxis.cor$S <- seq(from=0, to=max(d.centr), length=1000)

yAxis.cor$S$obs$native <- vapply(xAxis.cor$S, calcCDF, FUN.VALUE=1, d.centr[which(traitS.3Dcor$INVASION.STATUS == "native")])
yAxis.cor$S$obs$natur <- vapply(xAxis.cor$S, calcCDF, FUN.VALUE=1, d.centr[which(traitS.3Dcor$INVASION.STATUS == "naturalized")])
yAxis.cor$S$obs$invasive <- vapply(xAxis.cor$S, calcCDF, FUN.VALUE=1, d.centr[which(traitS.3Dcor$INVASION.STATUS == "invasive")])

#randomize distances between natives and invasive
set.seed(1234)
yAxis.cor$S$rand$inv <- dist.shuffler.3D(scores=traitS.3Dcor[,1:3], status=traitS.3Dcor$INVASION.STATUS, test="invasive", Nrand=999)

#randomize distances between natives and naturalized
set.seed(1234)
yAxis.cor$S$rand$natur <- dist.shuffler.3D(scores=traitS.3Dcor[,1:3], status=traitS.3Dcor$INVASION.STATUS, test="naturalized", Nrand=999)

#CDF plot
plot(xAxis.cor$S, yAxis.cor$S$obs$native, type='l', axes=T, xlab="Distance from centroid", ylab="Proportion of species", main="Rock and scree vegetation", lwd=3,bty="n", col="blue", las=1, cex.lab=1.2, cex.axis=1.2, cex.main=1.2)
lines(xAxis.cor$S, yAxis.cor$S$obs$natur, type='l', lwd=3, col="yellow")
lines(xAxis.cor$S, yAxis.cor$S$obs$invasive, type='l', lwd=3, col="red3")

###Wetland vegetation ---------------------------------------------------------------------------

#calculate centroid of native species
centr <- apply(traitM.3Dcor[which(traitM.3Dcor$INVASION.STATUS == "native"), c(1:3)], 2, mean)
centr <- rbind(centr, traitM.3Dcor[,c(1:3)])

#calculate distances of species from observed native centroid
d.centr <- as.matrix(dist(centr))[,1]
d.centr <- d.centr[-1]

xAxis.cor$M <- seq(from=0, to=max(d.centr), length=1000)

yAxis.cor$M$obs$native <- vapply(xAxis.cor$M, calcCDF, FUN.VALUE=1, d.centr[which(traitM.3Dcor$INVASION.STATUS == "native")])
yAxis.cor$M$obs$natur <- vapply(xAxis.cor$M, calcCDF, FUN.VALUE=1, d.centr[which(traitM.3Dcor$INVASION.STATUS == "naturalized")])
yAxis.cor$M$obs$invasive <- vapply(xAxis.cor$M, calcCDF, FUN.VALUE=1, d.centr[which(traitM.3Dcor$INVASION.STATUS == "invasive")])

#randomize distances between natives and invasive
set.seed(1234)
yAxis.cor$M$rand$inv <- dist.shuffler.3D(scores=traitM.3Dcor[,1:3], status=traitM.3Dcor$INVASION.STATUS, test="invasive", Nrand=999)

#randomize distances between natives and naturalized
set.seed(1234)
yAxis.cor$M$rand$natur <- dist.shuffler.3D(scores=traitM.3Dcor[,1:3], status=traitM.3Dcor$INVASION.STATUS, test="naturalized", Nrand=999)

#CDF plot
plot(xAxis.cor$M, yAxis.cor$M$obs$native, type='l', axes=T, xlab="Distance from centroid", ylab="Proportion of species", main="Wetland vegetation", lwd=3,bty="n", col="blue", las=1, cex.lab=1.2, cex.axis=1.2, cex.main=1.2)
lines(xAxis.cor$M, yAxis.cor$M$obs$natur, type='l', lwd=3, col="yellow")
lines(xAxis.cor$M, yAxis.cor$M$obs$invasive, type='l', lwd=3, col="red3")

###Scrub vegetation ---------------------------------------------------------------------------

#calculate centroid of native species
centr <- apply(traitK.3Dcor[which(traitK.3Dcor$INVASION.STATUS == "native"), c(1:3)], 2, mean)
centr <- rbind(centr, traitK.3Dcor[,c(1:3)])

#calculate distances of species from observed native centroid
d.centr <- as.matrix(dist(centr))[,1]
d.centr <- d.centr[-1]

xAxis.cor$K <- seq(from=0, to=max(d.centr), length=1000)

yAxis.cor$K$obs$native <- vapply(xAxis.cor$K, calcCDF, FUN.VALUE=1, d.centr[which(traitK.3Dcor$INVASION.STATUS == "native")])
yAxis.cor$K$obs$natur <- vapply(xAxis.cor$K, calcCDF, FUN.VALUE=1, d.centr[which(traitK.3Dcor$INVASION.STATUS == "naturalized")])
yAxis.cor$K$obs$invasive <- vapply(xAxis.cor$K, calcCDF, FUN.VALUE=1, d.centr[which(traitK.3Dcor$INVASION.STATUS == "invasive")])

#randomize distances between natives and invasive
set.seed(1234)
yAxis.cor$K$rand$inv <- dist.shuffler.3D(scores=traitK.3Dcor[,1:3], status=traitK.3Dcor$INVASION.STATUS, test="invasive", Nrand=999)

#randomize distances between natives and naturalized
set.seed(1234)
yAxis.cor$K$rand$natur <- dist.shuffler.3D(scores=traitK.3Dcor[,1:3], status=traitK.3Dcor$INVASION.STATUS, test="naturalized", Nrand=999)

#CDF plot
plot(xAxis.cor$K, yAxis.cor$K$obs$native, type='l', axes=T, xlab="Distance from centroid", ylab="Proportion of species", main="Scrub vegetation", lwd=3,bty="n", col="blue", las=1, cex.lab=1.2, cex.axis=1.2, cex.main=1.2)
lines(xAxis.cor$K, yAxis.cor$K$obs$natur, type='l', lwd=3, col="yellow")
lines(xAxis.cor$K, yAxis.cor$K$obs$invasive, type='l', lwd=3, col="red3")

###Forest vegetation ---------------------------------------------------------------------------

#calculate centroid of native species
centr <- apply(traitL.3Dcor[which(traitL.3Dcor$INVASION.STATUS == "native"), c(1:3)], 2, mean)
centr <- rbind(centr, traitL.3Dcor[,c(1:3)])

#calculate distances of species from observed native centroid
d.centr <- as.matrix(dist(centr))[,1]
d.centr <- d.centr[-1]

xAxis.cor$L <- seq(from=0, to=max(d.centr), length=1000)

yAxis.cor$L$obs$native <- vapply(xAxis.cor$L, calcCDF, FUN.VALUE=1, d.centr[which(traitL.3Dcor$INVASION.STATUS == "native")])
yAxis.cor$L$obs$natur <- vapply(xAxis.cor$L, calcCDF, FUN.VALUE=1, d.centr[which(traitL.3Dcor$INVASION.STATUS == "naturalized")])
yAxis.cor$L$obs$invasive <- vapply(xAxis.cor$L, calcCDF, FUN.VALUE=1, d.centr[which(traitL.3Dcor$INVASION.STATUS == "invasive")])

#randomize distances between natives and invasive
set.seed(1234)
yAxis.cor$L$rand$inv <- dist.shuffler.3D(scores=traitL.3Dcor[,1:3], status=traitL.3Dcor$INVASION.STATUS, test="invasive", Nrand=999)

#randomize distances between natives and naturalized
set.seed(1234)
yAxis.cor$L$rand$natur <- dist.shuffler.3D(scores=traitL.3Dcor[,1:3], status=traitL.3Dcor$INVASION.STATUS, test="naturalized", Nrand=999)

#CDF plot
plot(xAxis.cor$L, yAxis.cor$L$obs$native, type='l', axes=T, xlab="Distance from centroid", ylab="Proportion of species", main="Forest vegetation", lwd=3,bty="n", col="blue", las=1, cex.lab=1.2, cex.axis=1.2, cex.main=1.2)
lines(xAxis.cor$L, yAxis.cor$L$obs$natur, type='l', lwd=3, col="yellow")
lines(xAxis.cor$L, yAxis.cor$L$obs$invasive, type='l', lwd=3, col="red3")

#############################################################################################
###RANDOMIZE INVASIVE AND NATURALIZED SPECIES IN DATASET WITH IMPUTED NAD CORRECTED SPECIES TRAITS---------
par(mar=c(4, 4, 2.5, 1.5))

xAxis.cor.imp <- list()
yAxis.cor.imp <- list()

###Grassland and heathland vegetation ---------------------------------------------------------------------------

#calculate centroid of native species
centr <- apply(traitT.3Dcor.imp[which(traitT.3Dcor.imp$INVASION.STATUS == "native"), c(1:3)], 2, mean)
centr <- rbind(centr, traitT.3Dcor.imp[,c(1:3)])

#calculate distances of species from observed native centroid
d.centr <- as.matrix(dist(centr))[,1]
d.centr <- d.centr[-1]

xAxis.cor.imp$T <- seq(from=0, to=max(d.centr), length=1000)

yAxis.cor.imp$T$obs$native <- vapply(xAxis.cor.imp$T, calcCDF, FUN.VALUE=1, d.centr[which(traitT.3Dcor.imp$INVASION.STATUS == "native")])
yAxis.cor.imp$T$obs$natur <- vapply(xAxis.cor.imp$T, calcCDF, FUN.VALUE=1, d.centr[which(traitT.3Dcor.imp$INVASION.STATUS == "naturalized")])
yAxis.cor.imp$T$obs$invasive <- vapply(xAxis.cor.imp$T, calcCDF, FUN.VALUE=1, d.centr[which(traitT.3Dcor.imp$INVASION.STATUS == "invasive")])

#randomize distances between natives and invasive
set.seed(1234)
yAxis.cor.imp$T$rand$inv <- dist.shuffler.3D(scores=traitT.3Dcor.imp[,1:3], status=traitT.3Dcor.imp$INVASION.STATUS, test="invasive", Nrand=999)

#randomize distances between natives and naturalized
set.seed(1234)
yAxis.cor.imp$T$rand$natur <- dist.shuffler.3D(scores=traitT.3Dcor.imp[,1:3], status=traitT.3Dcor.imp$INVASION.STATUS, test="naturalized", Nrand=999)

#CDF plot
plot(xAxis.cor.imp$T, yAxis.cor.imp$T$obs$native, type='l', axes=T, xlab="Distance from centroid", ylab="Proportion of species", main="Grassland and heathland vegetation", lwd=3,bty="n", col="blue", las=1, cex.lab=1.2, cex.axis=1.2, cex.main=1.2)
lines(xAxis.cor.imp$T, yAxis.cor.imp$T$obs$natur, type='l', lwd=3, col="yellow")
lines(xAxis.cor.imp$T, yAxis.cor.imp$T$obs$invasive, type='l', lwd=3, col="red3")

###Ruderal and weed vegetation ---------------------------------------------------------------------------

#calculate centroid of native species
centr <- apply(traitX.3Dcor.imp[which(traitX.3Dcor.imp$INVASION.STATUS == "native"), c(1:3)], 2, mean)
centr <- rbind(centr, traitX.3Dcor.imp[,c(1:3)])

#calculate distances of species from observed native centroid
d.centr <- as.matrix(dist(centr))[,1]
d.centr <- d.centr[-1]

xAxis.cor.imp$X <- seq(from=0, to=max(d.centr), length=1000)

yAxis.cor.imp$X$obs$native <- vapply(xAxis.cor.imp$X, calcCDF, FUN.VALUE=1, d.centr[which(traitX.3Dcor.imp$INVASION.STATUS == "native")])
yAxis.cor.imp$X$obs$natur <- vapply(xAxis.cor.imp$X, calcCDF, FUN.VALUE=1, d.centr[which(traitX.3Dcor.imp$INVASION.STATUS == "naturalized")])
yAxis.cor.imp$X$obs$invasive <- vapply(xAxis.cor.imp$X, calcCDF, FUN.VALUE=1, d.centr[which(traitX.3Dcor.imp$INVASION.STATUS == "invasive")])

#randomize distances between natives and invasive
set.seed(1234)
yAxis.cor.imp$X$rand$inv <- dist.shuffler.3D(scores=traitX.3Dcor.imp[,1:3], status=traitX.3Dcor.imp$INVASION.STATUS, test="invasive", Nrand=999)

#randomize distances between natives and naturalized
set.seed(1234)
yAxis.cor.imp$X$rand$natur <- dist.shuffler.3D(scores=traitX.3Dcor.imp[,1:3], status=traitX.3Dcor.imp$INVASION.STATUS, test="naturalized", Nrand=999)

#CDF plot
plot(xAxis.cor.imp$X, yAxis.cor.imp$X$obs$native, type='l', axes=T, xlab="Distance from centroid", ylab="Proportion of species", main="Ruderal and weed vegetation", lwd=3,bty="n", col="blue", las=1, cex.lab=1.2, cex.axis=1.2, cex.main=1.2)
lines(xAxis.cor.imp$X, yAxis.cor.imp$X$obs$natur, type='l', lwd=3, col="yellow")
lines(xAxis.cor.imp$X, yAxis.cor.imp$X$obs$invasive, type='l', lwd=3, col="red3")

###Rock and scree vegetation ---------------------------------------------------------------------------

#calculate centroid of native species
centr <- apply(traitS.3Dcor.imp[which(traitS.3Dcor.imp$INVASION.STATUS == "native"), c(1:3)], 2, mean)
centr <- rbind(centr, traitS.3Dcor.imp[,c(1:3)])

#calculate distances of species from observed native centroid
d.centr <- as.matrix(dist(centr))[,1]
d.centr <- d.centr[-1]

xAxis.cor.imp$S <- seq(from=0, to=max(d.centr), length=1000)

yAxis.cor.imp$S$obs$native <- vapply(xAxis.cor.imp$S, calcCDF, FUN.VALUE=1, d.centr[which(traitS.3Dcor.imp$INVASION.STATUS == "native")])
yAxis.cor.imp$S$obs$natur <- vapply(xAxis.cor.imp$S, calcCDF, FUN.VALUE=1, d.centr[which(traitS.3Dcor.imp$INVASION.STATUS == "naturalized")])
yAxis.cor.imp$S$obs$invasive <- vapply(xAxis.cor.imp$S, calcCDF, FUN.VALUE=1, d.centr[which(traitS.3Dcor.imp$INVASION.STATUS == "invasive")])

#randomize distances between natives and invasive
set.seed(1234)
yAxis.cor.imp$S$rand$inv <- dist.shuffler.3D(scores=traitS.3Dcor.imp[,1:3], status=traitS.3Dcor.imp$INVASION.STATUS, test="invasive", Nrand=999)

#randomize distances between natives and naturalized
set.seed(1234)
yAxis.cor.imp$S$rand$natur <- dist.shuffler.3D(scores=traitS.3Dcor.imp[,1:3], status=traitS.3Dcor.imp$INVASION.STATUS, test="naturalized", Nrand=999)

#CDF plot
plot(xAxis.cor.imp$S, yAxis.cor.imp$S$obs$native, type='l', axes=T, xlab="Distance from centroid", ylab="Proportion of species", main="Rock and scree vegetation", lwd=3,bty="n", col="blue", las=1, cex.lab=1.2, cex.axis=1.2, cex.main=1.2)
lines(xAxis.cor.imp$S, yAxis.cor.imp$S$obs$natur, type='l', lwd=3, col="yellow")
lines(xAxis.cor.imp$S, yAxis.cor.imp$S$obs$invasive, type='l', lwd=3, col="red3")

###Wetland vegetation ---------------------------------------------------------------------------

#calculate centroid of native species
centr <- apply(traitM.3Dcor.imp[which(traitM.3Dcor.imp$INVASION.STATUS == "native"), c(1:3)], 2, mean)
centr <- rbind(centr, traitM.3Dcor.imp[,c(1:3)])

#calculate distances of species from observed native centroid
d.centr <- as.matrix(dist(centr))[,1]
d.centr <- d.centr[-1]

xAxis.cor.imp$M <- seq(from=0, to=max(d.centr), length=1000)

yAxis.cor.imp$M$obs$native <- vapply(xAxis.cor.imp$M, calcCDF, FUN.VALUE=1, d.centr[which(traitM.3Dcor.imp$INVASION.STATUS == "native")])
yAxis.cor.imp$M$obs$natur <- vapply(xAxis.cor.imp$M, calcCDF, FUN.VALUE=1, d.centr[which(traitM.3Dcor.imp$INVASION.STATUS == "naturalized")])
yAxis.cor.imp$M$obs$invasive <- vapply(xAxis.cor.imp$M, calcCDF, FUN.VALUE=1, d.centr[which(traitM.3Dcor.imp$INVASION.STATUS == "invasive")])

#randomize distances between natives and invasive
set.seed(1234)
yAxis.cor.imp$M$rand$inv <- dist.shuffler.3D(scores=traitM.3Dcor.imp[,1:3], status=traitM.3Dcor.imp$INVASION.STATUS, test="invasive", Nrand=999)

#randomize distances between natives and naturalized
set.seed(1234)
yAxis.cor.imp$M$rand$natur <- dist.shuffler.3D(scores=traitM.3Dcor.imp[,1:3], status=traitM.3Dcor.imp$INVASION.STATUS, test="naturalized", Nrand=999)

#CDF plot
plot(xAxis.cor.imp$M, yAxis.cor.imp$M$obs$native, type='l', axes=T, xlab="Distance from centroid", ylab="Proportion of species", main="Wetland vegetation", lwd=3,bty="n", col="blue", las=1, cex.lab=1.2, cex.axis=1.2, cex.main=1.2)
lines(xAxis.cor.imp$M, yAxis.cor.imp$M$obs$natur, type='l', lwd=3, col="yellow")
lines(xAxis.cor.imp$M, yAxis.cor.imp$M$obs$invasive, type='l', lwd=3, col="red3")

###Scrub vegetation ---------------------------------------------------------------------------

#calculate centroid of native species
centr <- apply(traitK.3Dcor.imp[which(traitK.3Dcor.imp$INVASION.STATUS == "native"), c(1:3)], 2, mean)
centr <- rbind(centr, traitK.3Dcor.imp[,c(1:3)])

#calculate distances of species from observed native centroid
d.centr <- as.matrix(dist(centr))[,1]
d.centr <- d.centr[-1]

xAxis.cor.imp$K <- seq(from=0, to=max(d.centr), length=1000)

yAxis.cor.imp$K$obs$native <- vapply(xAxis.cor.imp$K, calcCDF, FUN.VALUE=1, d.centr[which(traitK.3Dcor.imp$INVASION.STATUS == "native")])
yAxis.cor.imp$K$obs$natur <- vapply(xAxis.cor.imp$K, calcCDF, FUN.VALUE=1, d.centr[which(traitK.3Dcor.imp$INVASION.STATUS == "naturalized")])
yAxis.cor.imp$K$obs$invasive <- vapply(xAxis.cor.imp$K, calcCDF, FUN.VALUE=1, d.centr[which(traitK.3Dcor.imp$INVASION.STATUS == "invasive")])

#randomize distances between natives and invasive
set.seed(1234)
yAxis.cor.imp$K$rand$inv <- dist.shuffler.3D(scores=traitK.3Dcor.imp[,1:3], status=traitK.3Dcor.imp$INVASION.STATUS, test="invasive", Nrand=999)

#randomize distances between natives and naturalized
set.seed(1234)
yAxis.cor.imp$K$rand$natur <- dist.shuffler.3D(scores=traitK.3Dcor.imp[,1:3], status=traitK.3Dcor.imp$INVASION.STATUS, test="naturalized", Nrand=999)

#CDF plot
plot(xAxis.cor.imp$K, yAxis.cor.imp$K$obs$native, type='l', axes=T, xlab="Distance from centroid", ylab="Proportion of species", main="Scrub vegetation", lwd=3,bty="n", col="blue", las=1, cex.lab=1.2, cex.axis=1.2, cex.main=1.2)
lines(xAxis.cor.imp$K, yAxis.cor.imp$K$obs$natur, type='l', lwd=3, col="yellow")
lines(xAxis.cor.imp$K, yAxis.cor.imp$K$obs$invasive, type='l', lwd=3, col="red3")

###Forest vegetation ---------------------------------------------------------------------------

#calculate centroid of native species
centr <- apply(traitL.3Dcor.imp[which(traitL.3Dcor.imp$INVASION.STATUS == "native"), c(1:3)], 2, mean)
centr <- rbind(centr, traitL.3Dcor.imp[,c(1:3)])

#calculate distances of species from observed native centroid
d.centr <- as.matrix(dist(centr))[,1]
d.centr <- d.centr[-1]

xAxis.cor.imp$L <- seq(from=0, to=max(d.centr), length=1000)

yAxis.cor.imp$L$obs$native <- vapply(xAxis.cor.imp$L, calcCDF, FUN.VALUE=1, d.centr[which(traitL.3Dcor.imp$INVASION.STATUS == "native")])
yAxis.cor.imp$L$obs$natur <- vapply(xAxis.cor.imp$L, calcCDF, FUN.VALUE=1, d.centr[which(traitL.3Dcor.imp$INVASION.STATUS == "naturalized")])
yAxis.cor.imp$L$obs$invasive <- vapply(xAxis.cor.imp$L, calcCDF, FUN.VALUE=1, d.centr[which(traitL.3Dcor.imp$INVASION.STATUS == "invasive")])

#randomize distances between natives and invasive
set.seed(1234)
yAxis.cor.imp$L$rand$inv <- dist.shuffler.3D(scores=traitL.3Dcor.imp[,1:3], status=traitL.3Dcor.imp$INVASION.STATUS, test="invasive", Nrand=999)

#randomize distances between natives and naturalized
set.seed(1234)
yAxis.cor.imp$L$rand$natur <- dist.shuffler.3D(scores=traitL.3Dcor.imp[,1:3], status=traitL.3Dcor.imp$INVASION.STATUS, test="naturalized", Nrand=999)

#CDF plot
plot(xAxis.cor.imp$L, yAxis.cor.imp$L$obs$native, type='l', axes=T, xlab="Distance from centroid", ylab="Proportion of species", main="Forest vegetation", lwd=3,bty="n", col="blue", las=1, cex.lab=1.2, cex.axis=1.2, cex.main=1.2)
lines(xAxis.cor.imp$L, yAxis.cor.imp$L$obs$natur, type='l', lwd=3, col="yellow")
lines(xAxis.cor.imp$L, yAxis.cor.imp$L$obs$invasive, type='l', lwd=3, col="red3")

