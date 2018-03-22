##############################################################################################
#       CALCULATE CUMULATIVE DISTRIBUTION FUNCTIONS FOR EACH SPECIES GROUP IN EACH HABITAT   #
##############################################################################################

#code by Jan Divisek (2015-2017)

##CDFs are based on species distances from the centroid of native species in the 3D trait space of each habitat

library(vegan)

#define step for CDF calculation
xAxis <- seq(from=0, to=1, length=1000)

###RANDOMIZE INVASIVE AND NATURALIZED SPECIES---------------------------------------------------------------------------------
#To test the difference between natives and invasives and between natives and naturalized species
par(mar=c(4, 4, 2.5, 1.5))

###Grassland and heathland vegetation ---------------------------------------------------------------------------
yAxisT.3D.rand <- list()

#randomize distances between natives and invasive
set.seed(1234)
yAxisT.3D.rand$inv <- dist.shuffler.3D(scores=traitT.3D[,1:3], status=traitT.3D$INVASION.STATUS, test="invasive", Nrand=999)

#randomize distances between natives and naturalized
set.seed(1234)
yAxisT.3D.rand$natur <- dist.shuffler.3D(scores=traitT.3D[,1:3], status=traitT.3D$INVASION.STATUS, test="naturalized", Nrand=999)

#calculate centroid of native species
centr <- apply(traitT.3D[which(traitT.3D$INVASION.STATUS == "native"), c(1:3)], 2, mean)
centr <- rbind(centr, traitT.3D[,c(1:3)])

#calculate distances of species from observed native centroid
d.centr <- as.matrix(dist(centr))[,1]
d.centr <- decostand(d.centr[-1], "range")

#observed CDF
yAxisT.3D.obs <- list()

yAxisT.3D.obs$native <- vapply(xAxis, calcCDF, FUN.VALUE=1, d.centr[which(traitT.3D$INVASION.STATUS == "native")])
yAxisT.3D.obs$natur <- vapply(xAxis, calcCDF, FUN.VALUE=1, d.centr[which(traitT.3D$INVASION.STATUS == "naturalized")])
yAxisT.3D.obs$invasive <- vapply(xAxis, calcCDF, FUN.VALUE=1, d.centr[which(traitT.3D$INVASION.STATUS == "invasive")])

#CDF plot
plot(xAxis, yAxisT.3D.obs$native, type='l', axes=T, xlab="Distance from centroid", ylab="Proportion of species", main="Grassland and heathland vegetation", lwd=3,bty="n", col="blue", las=1, cex.lab=1.2, cex.axis=1.2, cex.main=1.2)
lines(xAxis, yAxisT.3D.obs$natur, type='l', lwd=3, col="yellow")
lines(xAxis, yAxisT.3D.obs$invasive, type='l', lwd=3, col="red3")

###Ruderal and weed vegetation ---------------------------------------------------------------------------
yAxisX.3D.rand <- list()

#randomize distances between natives and invasive
set.seed(1234)
yAxisX.3D.rand$inv <- dist.shuffler.3D(scores=traitX.3D[,1:3], status=traitX.3D$INVASION.STATUS, test="invasive", Nrand=999)

#randomize distances between natives and naturalized
set.seed(1234)
yAxisX.3D.rand$natur <- dist.shuffler.3D(scores=traitX.3D[,1:3], status=traitX.3D$INVASION.STATUS, test="naturalized", Nrand=999)

#calculate centroid of native species
centr <- apply(traitX.3D[which(traitX.3D$INVASION.STATUS == "native"), c(1:3)], 2, mean)
centr <- rbind(centr, traitX.3D[,c(1:3)])

#calculate distances of species from observed native centroid
d.centr <- as.matrix(dist(centr))[,1]
d.centr <- decostand(d.centr[-1], "range")

#observed CDF
yAxisX.3D.obs <- list()

yAxisX.3D.obs$native <- vapply(xAxis, calcCDF, FUN.VALUE=1, d.centr[which(traitX.3D$INVASION.STATUS == "native")])
yAxisX.3D.obs$natur <- vapply(xAxis, calcCDF, FUN.VALUE=1, d.centr[which(traitX.3D$INVASION.STATUS == "naturalized")])
yAxisX.3D.obs$invasive <- vapply(xAxis, calcCDF, FUN.VALUE=1, d.centr[which(traitX.3D$INVASION.STATUS == "invasive")])

#CDF plot
plot(xAxis, yAxisX.3D.obs$native, type='l', axes=T, xlab="Distance from centroid", ylab="Proportion of species", main="Ruderal and weed vegetation", lwd=3,bty="n", col="blue", las=1, cex.lab=1.2, cex.axis=1.2, cex.main=1.2)
lines(xAxis, yAxisX.3D.obs$natur, type='l', lwd=3, col="yellow")
lines(xAxis, yAxisX.3D.obs$invasive, type='l', lwd=3, col="red3")

###Rock and scree vegetation ---------------------------------------------------------------------------
yAxisS.3D.rand <- list()

#randomize distances between natives and invasive
set.seed(1234)
yAxisS.3D.rand$inv <- dist.shuffler.3D(scores=traitS.3D[,1:3], status=traitS.3D$INVASION.STATUS, test="invasive", Nrand=999)

#randomize distances between natives and naturalized
set.seed(1234)
yAxisS.3D.rand$natur <- dist.shuffler.3D(scores=traitS.3D[,1:3], status=traitS.3D$INVASION.STATUS, test="naturalized", Nrand=999)

#calculate centroid of native species
centr <- apply(traitS.3D[which(traitS.3D$INVASION.STATUS == "native"), c(1:3)], 2, mean)
centr <- rbind(centr, traitS.3D[,c(1:3)])

#calculate distances of species from observed native centroid
d.centr <- as.matrix(dist(centr))[,1]
d.centr <- decostand(d.centr[-1], "range")

#observed CDF
yAxisS.3D.obs <- list()

yAxisS.3D.obs$native <- vapply(xAxis, calcCDF, FUN.VALUE=1, d.centr[which(traitS.3D$INVASION.STATUS == "native")])
yAxisS.3D.obs$natur <- vapply(xAxis, calcCDF, FUN.VALUE=1, d.centr[which(traitS.3D$INVASION.STATUS == "naturalized")])
yAxisS.3D.obs$invasive <- vapply(xAxis, calcCDF, FUN.VALUE=1, d.centr[which(traitS.3D$INVASION.STATUS == "invasive")])

#CDF plot
plot(xAxis, yAxisS.3D.obs$native, type='l', axes=T, xlab="Distance from centroid", ylab="Proportion of species", main="Rock and scree vegetation", lwd=3,bty="n", col="blue", las=1, cex.lab=1.2, cex.axis=1.2, cex.main=1.2)
lines(xAxis, yAxisS.3D.obs$natur, type='l', lwd=3, col="yellow")
lines(xAxis, yAxisS.3D.obs$invasive, type='l', lwd=3, col="red3")

###Wetland vegetation ---------------------------------------------------------------------------
yAxisM.3D.rand <- list()

#randomize distances between natives and invasive
set.seed(1234)
yAxisM.3D.rand$inv <- dist.shuffler.3D(scores=traitM.3D[,1:3], status=traitM.3D$INVASION.STATUS, test="invasive", Nrand=999)

#randomize distances between natives and naturalized
set.seed(1234)
yAxisM.3D.rand$natur <- dist.shuffler.3D(scores=traitM.3D[,1:3], status=traitM.3D$INVASION.STATUS, test="naturalized", Nrand=999)

#calculate centroid of native species
centr <- apply(traitM.3D[which(traitM.3D$INVASION.STATUS == "native"), c(1:3)], 2, mean)
centr <- rbind(centr, traitM.3D[,c(1:3)])

#calculate distances of species from observed native centroid
d.centr <- as.matrix(dist(centr))[,1]
d.centr <- decostand(d.centr[-1], "range")

#observed CDF
yAxisM.3D.obs <- list()

yAxisM.3D.obs$native <- vapply(xAxis, calcCDF, FUN.VALUE=1, d.centr[which(traitM.3D$INVASION.STATUS == "native")])
yAxisM.3D.obs$natur <- vapply(xAxis, calcCDF, FUN.VALUE=1, d.centr[which(traitM.3D$INVASION.STATUS == "naturalized")])
yAxisM.3D.obs$invasive <- vapply(xAxis, calcCDF, FUN.VALUE=1, d.centr[which(traitM.3D$INVASION.STATUS == "invasive")])

#CDF plot
plot(xAxis, yAxisM.3D.obs$native, type='l', axes=T, xlab="Distance from centroid", ylab="Proportion of species", main="Wetland vegetation", lwd=3,bty="n", col="blue", las=1, cex.lab=1.2, cex.axis=1.2, cex.main=1.2)
lines(xAxis, yAxisM.3D.obs$natur, type='l', lwd=3, col="yellow")
lines(xAxis, yAxisM.3D.obs$invasive, type='l', lwd=3, col="red3")

###Scrub vegetation ---------------------------------------------------------------------------
yAxisK.3D.rand <- list()

#randomize distances between natives and invasive
set.seed(1234)
yAxisK.3D.rand$inv <- dist.shuffler.3D(scores=traitK.3D[,1:3], status=traitK.3D$INVASION.STATUS, test="invasive", Nrand=999)

#randomize distances between natives and naturalized
set.seed(1234)
yAxisK.3D.rand$natur <- dist.shuffler.3D(scores=traitK.3D[,1:3], status=traitK.3D$INVASION.STATUS, test="naturalized", Nrand=999)

#calculate centroid of native species
centr <- apply(traitK.3D[which(traitK.3D$INVASION.STATUS == "native"), c(1:3)], 2, mean)
centr <- rbind(centr, traitK.3D[,c(1:3)])

#calculate distances of species from observed native centroid
d.centr <- as.matrix(dist(centr))[,1]
d.centr <- decostand(d.centr[-1], "range")

#observed CDF
yAxisK.3D.obs <- list()

yAxisK.3D.obs$native <- vapply(xAxis, calcCDF, FUN.VALUE=1, d.centr[which(traitK.3D$INVASION.STATUS == "native")])
yAxisK.3D.obs$natur <- vapply(xAxis, calcCDF, FUN.VALUE=1, d.centr[which(traitK.3D$INVASION.STATUS == "naturalized")])
yAxisK.3D.obs$invasive <- vapply(xAxis, calcCDF, FUN.VALUE=1, d.centr[which(traitK.3D$INVASION.STATUS == "invasive")])

#CDF plot
plot(xAxis, yAxisK.3D.obs$native, type='l', axes=T, xlab="Distance from centroid", ylab="Proportion of species", main="Scrub vegetation", lwd=3,bty="n", col="blue", las=1, cex.lab=1.2, cex.axis=1.2, cex.main=1.2)
lines(xAxis, yAxisK.3D.obs$natur, type='l', lwd=3, col="yellow")
lines(xAxis, yAxisK.3D.obs$invasive, type='l', lwd=3, col="red3")

###Forest vegetation ---------------------------------------------------------------------------
yAxisL.3D.rand <- list()

#randomize distances between natives and invasive
set.seed(1234)
yAxisL.3D.rand$inv <- dist.shuffler.3D(scores=traitL.3D[,1:3], status=traitL.3D$INVASION.STATUS, test="invasive", Nrand=999)

#randomize distances between natives and naturalized
set.seed(1234)
yAxisL.3D.rand$natur <- dist.shuffler.3D(scores=traitL.3D[,1:3], status=traitL.3D$INVASION.STATUS, test="naturalized", Nrand=999)

#calculate centroid of native species
centr <- apply(traitL.3D[which(traitL.3D$INVASION.STATUS == "native"), c(1:3)], 2, mean)
centr <- rbind(centr, traitL.3D[,c(1:3)])

#calculate distances of species from observed native centroid
d.centr <- as.matrix(dist(centr))[,1]
d.centr <- decostand(d.centr[-1], "range")

#observed CDF
yAxisL.3D.obs <- list()

yAxisL.3D.obs$native <- vapply(xAxis, calcCDF, FUN.VALUE=1, d.centr[which(traitL.3D$INVASION.STATUS == "native")])
yAxisL.3D.obs$natur <- vapply(xAxis, calcCDF, FUN.VALUE=1, d.centr[which(traitL.3D$INVASION.STATUS == "naturalized")])
yAxisL.3D.obs$invasive <- vapply(xAxis, calcCDF, FUN.VALUE=1, d.centr[which(traitL.3D$INVASION.STATUS == "invasive")])

#CDF plot
plot(xAxis, yAxisL.3D.obs$native, type='l', axes=T, xlab="Distance from centroid", ylab="Proportion of species", main="Forest vegetation", lwd=3,bty="n", col="blue", las=1, cex.lab=1.2, cex.axis=1.2, cex.main=1.2)
lines(xAxis, yAxisL.3D.obs$natur, type='l', lwd=3, col="yellow")
lines(xAxis, yAxisL.3D.obs$invasive, type='l', lwd=3, col="red3")

#############################################################################################
###RANDOMIZE INVASIVE AND NATURALIZED SPECIES IN DATASET WITH IMPUTED SPECIES TRAITS---------
par(mar=c(4, 4, 2.5, 1.5))

###Grassland and heathland vegetation ---------------------------------------------------------------------------
yAxisT.3D.full.rand <- list()

#randomize distances between natives and invasive
set.seed(1234)
yAxisT.3D.full.rand$inv <- dist.shuffler.3D(scores=traitT.3D.full[,1:3], status=traitT.3D.full$INVASION.STATUS, test="invasive", Nrand=999)

#randomize distances between natives and naturalized
set.seed(1234)
yAxisT.3D.full.rand$natur <- dist.shuffler.3D(scores=traitT.3D.full[,1:3], status=traitT.3D.full$INVASION.STATUS, test="naturalized", Nrand=999)

#calculate centroid of native species
centr <- apply(traitT.3D.full[which(traitT.3D.full$INVASION.STATUS == "native"), c(1:3)], 2, mean)
centr <- rbind(centr, traitT.3D.full[,c(1:3)])

#calculate distances of species from observed native centroid
d.centr <- as.matrix(dist(centr))[,1]
d.centr <- decostand(d.centr[-1], "range")

#observed CDF
yAxisT.3D.full.obs <- list()

yAxisT.3D.full.obs$native <- vapply(xAxis, calcCDF, FUN.VALUE=1, d.centr[which(traitT.3D.full$INVASION.STATUS == "native")])
yAxisT.3D.full.obs$natur <- vapply(xAxis, calcCDF, FUN.VALUE=1, d.centr[which(traitT.3D.full$INVASION.STATUS == "naturalized")])
yAxisT.3D.full.obs$invasive <- vapply(xAxis, calcCDF, FUN.VALUE=1, d.centr[which(traitT.3D.full$INVASION.STATUS == "invasive")])

#CDF plot
plot(xAxis, yAxisT.3D.full.obs$native, type='l', axes=T, xlab="Distance from centroid", ylab="Proportion of species", main="Grassland and heathland vegetation", lwd=3,bty="n", col="blue", las=1, cex.lab=1.2, cex.axis=1.2, cex.main=1.2)
lines(xAxis, yAxisT.3D.full.obs$natur, type='l', lwd=3, col="yellow")
lines(xAxis, yAxisT.3D.full.obs$invasive, type='l', lwd=3, col="red3")

###Ruderal and weed vegetation ---------------------------------------------------------------------------
yAxisX.3D.full.rand <- list()

#randomize distances between natives and invasive
set.seed(1234)
yAxisX.3D.full.rand$inv <- dist.shuffler.3D(scores=traitX.3D.full[,1:3], status=traitX.3D.full$INVASION.STATUS, test="invasive", Nrand=999)

#randomize distances between natives and naturalized
set.seed(1234)
yAxisX.3D.full.rand$natur <- dist.shuffler.3D(scores=traitX.3D.full[,1:3], status=traitX.3D.full$INVASION.STATUS, test="naturalized", Nrand=999)

#calculate centroid of native species
centr <- apply(traitX.3D.full[which(traitX.3D.full$INVASION.STATUS == "native"), c(1:3)], 2, mean)
centr <- rbind(centr, traitX.3D.full[,c(1:3)])

#calculate distances of species from observed native centroid
d.centr <- as.matrix(dist(centr))[,1]
d.centr <- decostand(d.centr[-1], "range")

#observed CDF
yAxisX.3D.full.obs <- list()

yAxisX.3D.full.obs$native <- vapply(xAxis, calcCDF, FUN.VALUE=1, d.centr[which(traitX.3D.full$INVASION.STATUS == "native")])
yAxisX.3D.full.obs$natur <- vapply(xAxis, calcCDF, FUN.VALUE=1, d.centr[which(traitX.3D.full$INVASION.STATUS == "naturalized")])
yAxisX.3D.full.obs$invasive <- vapply(xAxis, calcCDF, FUN.VALUE=1, d.centr[which(traitX.3D.full$INVASION.STATUS == "invasive")])

#CDF plot
plot(xAxis, yAxisX.3D.full.obs$native, type='l', axes=T, xlab="Distance from centroid", ylab="Proportion of species", main="Ruderal and weed vegetation", lwd=3,bty="n", col="blue", las=1, cex.lab=1.2, cex.axis=1.2, cex.main=1.2)
lines(xAxis, yAxisX.3D.full.obs$natur, type='l', lwd=3, col="yellow")
lines(xAxis, yAxisX.3D.full.obs$invasive, type='l', lwd=3, col="red3")

###Rock and scree vegetation ---------------------------------------------------------------------------
yAxisS.3D.full.rand <- list()

#randomize distances between natives and invasive
set.seed(1234)
yAxisS.3D.full.rand$inv <- dist.shuffler.3D(scores=traitS.3D.full[,1:3], status=traitS.3D.full$INVASION.STATUS, test="invasive", Nrand=999)

#randomize distances between natives and naturalized
set.seed(1234)
yAxisS.3D.full.rand$natur <- dist.shuffler.3D(scores=traitS.3D.full[,1:3], status=traitS.3D.full$INVASION.STATUS, test="naturalized", Nrand=999)

#calculate centroid of native species
centr <- apply(traitS.3D.full[which(traitS.3D.full$INVASION.STATUS == "native"), c(1:3)], 2, mean)
centr <- rbind(centr, traitS.3D.full[,c(1:3)])

#calculate distances of species from observed native centroid
d.centr <- as.matrix(dist(centr))[,1]
d.centr <- decostand(d.centr[-1], "range")

#observed CDF
yAxisS.3D.full.obs <- list()

yAxisS.3D.full.obs$native <- vapply(xAxis, calcCDF, FUN.VALUE=1, d.centr[which(traitS.3D.full$INVASION.STATUS == "native")])
yAxisS.3D.full.obs$natur <- vapply(xAxis, calcCDF, FUN.VALUE=1, d.centr[which(traitS.3D.full$INVASION.STATUS == "naturalized")])
yAxisS.3D.full.obs$invasive <- vapply(xAxis, calcCDF, FUN.VALUE=1, d.centr[which(traitS.3D.full$INVASION.STATUS == "invasive")])

#CDF plot
plot(xAxis, yAxisS.3D.full.obs$native, type='l', axes=T, xlab="Distance from centroid", ylab="Proportion of species", main="Rock and scree vegetation", lwd=3,bty="n", col="blue", las=1, cex.lab=1.2, cex.axis=1.2, cex.main=1.2)
lines(xAxis, yAxisS.3D.full.obs$natur, type='l', lwd=3, col="yellow")
lines(xAxis, yAxisS.3D.full.obs$invasive, type='l', lwd=3, col="red3")

###Wetland vegetation ---------------------------------------------------------------------------
yAxisM.3D.full.rand <- list()

#randomize distances between natives and invasive
set.seed(1234)
yAxisM.3D.full.rand$inv <- dist.shuffler.3D(scores=traitM.3D.full[,1:3], status=traitM.3D.full$INVASION.STATUS, test="invasive", Nrand=999)

#randomize distances between natives and naturalized
set.seed(1234)
yAxisM.3D.full.rand$natur <- dist.shuffler.3D(scores=traitM.3D.full[,1:3], status=traitM.3D.full$INVASION.STATUS, test="naturalized", Nrand=999)

#calculate centroid of native species
centr <- apply(traitM.3D.full[which(traitM.3D.full$INVASION.STATUS == "native"), c(1:3)], 2, mean)
centr <- rbind(centr, traitM.3D.full[,c(1:3)])

#calculate distances of species from observed native centroid
d.centr <- as.matrix(dist(centr))[,1]
d.centr <- decostand(d.centr[-1], "range")

#observed CDF
yAxisM.3D.full.obs <- list()

yAxisM.3D.full.obs$native <- vapply(xAxis, calcCDF, FUN.VALUE=1, d.centr[which(traitM.3D.full$INVASION.STATUS == "native")])
yAxisM.3D.full.obs$natur <- vapply(xAxis, calcCDF, FUN.VALUE=1, d.centr[which(traitM.3D.full$INVASION.STATUS == "naturalized")])
yAxisM.3D.full.obs$invasive <- vapply(xAxis, calcCDF, FUN.VALUE=1, d.centr[which(traitM.3D.full$INVASION.STATUS == "invasive")])

#CDF plot
plot(xAxis, yAxisM.3D.full.obs$native, type='l', axes=T, xlab="Distance from centroid", ylab="Proportion of species", main="Wetland vegetation", lwd=3,bty="n", col="blue", las=1, cex.lab=1.2, cex.axis=1.2, cex.main=1.2)
lines(xAxis, yAxisM.3D.full.obs$natur, type='l', lwd=3, col="yellow")
lines(xAxis, yAxisM.3D.full.obs$invasive, type='l', lwd=3, col="red3")

###Scrub vegetation ---------------------------------------------------------------------------
yAxisK.3D.full.rand <- list()

#randomize distances between natives and invasive
set.seed(1234)
yAxisK.3D.full.rand$inv <- dist.shuffler.3D(scores=traitK.3D.full[,1:3], status=traitK.3D.full$INVASION.STATUS, test="invasive", Nrand=999)

#randomize distances between natives and naturalized
set.seed(1234)
yAxisK.3D.full.rand$natur <- dist.shuffler.3D(scores=traitK.3D.full[,1:3], status=traitK.3D.full$INVASION.STATUS, test="naturalized", Nrand=999)

#calculate centroid of native species
centr <- apply(traitK.3D.full[which(traitK.3D.full$INVASION.STATUS == "native"), c(1:3)], 2, mean)
centr <- rbind(centr, traitK.3D.full[,c(1:3)])

#calculate distances of species from observed native centroid
d.centr <- as.matrix(dist(centr))[,1]
d.centr <- decostand(d.centr[-1], "range")

#observed CDF
yAxisK.3D.full.obs <- list()

yAxisK.3D.full.obs$native <- vapply(xAxis, calcCDF, FUN.VALUE=1, d.centr[which(traitK.3D.full$INVASION.STATUS == "native")])
yAxisK.3D.full.obs$natur <- vapply(xAxis, calcCDF, FUN.VALUE=1, d.centr[which(traitK.3D.full$INVASION.STATUS == "naturalized")])
yAxisK.3D.full.obs$invasive <- vapply(xAxis, calcCDF, FUN.VALUE=1, d.centr[which(traitK.3D.full$INVASION.STATUS == "invasive")])

#CDF plot
plot(xAxis, yAxisK.3D.full.obs$native, type='l', axes=T, xlab="Distance from centroid", ylab="Proportion of species", main="Scrub vegetation", lwd=3,bty="n", col="blue", las=1, cex.lab=1.2, cex.axis=1.2, cex.main=1.2)
lines(xAxis, yAxisK.3D.full.obs$natur, type='l', lwd=3, col="yellow")
lines(xAxis, yAxisK.3D.full.obs$invasive, type='l', lwd=3, col="red3")

###Forest vegetation ---------------------------------------------------------------------------
yAxisL.3D.full.rand <- list()

#randomize distances between natives and invasive
set.seed(1234)
yAxisL.3D.full.rand$inv <- dist.shuffler.3D(scores=traitL.3D.full[,1:3], status=traitL.3D.full$INVASION.STATUS, test="invasive", Nrand=999)

#randomize distances between natives and naturalized
set.seed(1234)
yAxisL.3D.full.rand$natur <- dist.shuffler.3D(scores=traitL.3D.full[,1:3], status=traitL.3D.full$INVASION.STATUS, test="naturalized", Nrand=999)

#calculate centroid of native species
centr <- apply(traitL.3D.full[which(traitL.3D.full$INVASION.STATUS == "native"), c(1:3)], 2, mean)
centr <- rbind(centr, traitL.3D.full[,c(1:3)])

#calculate distances of species from observed native centroid
d.centr <- as.matrix(dist(centr))[,1]
d.centr <- decostand(d.centr[-1], "range")

#observed CDF
yAxisL.3D.full.obs <- list()

yAxisL.3D.full.obs$native <- vapply(xAxis, calcCDF, FUN.VALUE=1, d.centr[which(traitL.3D.full$INVASION.STATUS == "native")])
yAxisL.3D.full.obs$natur <- vapply(xAxis, calcCDF, FUN.VALUE=1, d.centr[which(traitL.3D.full$INVASION.STATUS == "naturalized")])
yAxisL.3D.full.obs$invasive <- vapply(xAxis, calcCDF, FUN.VALUE=1, d.centr[which(traitL.3D.full$INVASION.STATUS == "invasive")])

#CDF plot
plot(xAxis, yAxisL.3D.full.obs$native, type='l', axes=T, xlab="Distance from centroid", ylab="Proportion of species", main="Forest vegetation", lwd=3,bty="n", col="blue", las=1, cex.lab=1.2, cex.axis=1.2, cex.main=1.2)
lines(xAxis, yAxisL.3D.full.obs$natur, type='l', lwd=3, col="yellow")
lines(xAxis, yAxisL.3D.full.obs$invasive, type='l', lwd=3, col="red3")


##############################################################################################
#      CALCULATE CUMULATIVE DISTRIBUTION FUNCTIONS FOR PHYLOGENETICALLY CORRECTED DATA       #
##############################################################################################

###RANDOMIZE INVASIVE AND NATURALIZED SPECIES AFTER ACCOUNTING FOR PHYLOGENETIC SIGNAL-----------
par(mar=c(4, 4, 2.5, 1.5))

###Grassland and heathland vegetation ---------------------------------------------------------------------------
yAxisT.3Dcor.rand <- list()

#randomize distances between natives and invasive
set.seed(1234)
yAxisT.3Dcor.rand$inv <- dist.shuffler.3D(scores=traitT.3Dcor[,1:3], status=traitT.3Dcor$INVASION.STATUS, test="invasive", Nrand=999)

#randomize distances between natives and naturalized
set.seed(1234)
yAxisT.3Dcor.rand$natur <- dist.shuffler.3D(scores=traitT.3Dcor[,1:3], status=traitT.3Dcor$INVASION.STATUS, test="naturalized", Nrand=999)

#calculate centroid of native species
centr <- apply(traitT.3Dcor[which(traitT.3Dcor$INVASION.STATUS == "native"), c(1:3)], 2, mean)
centr <- rbind(centr, traitT.3Dcor[,c(1:3)])

#calculate distances of species from observed native centroid
d.centr <- as.matrix(dist(centr))[,1]
d.centr <- decostand(d.centr[-1], "range")

#observed CDF
yAxisT.3Dcor.obs <- list()

yAxisT.3Dcor.obs$native <- vapply(xAxis, calcCDF, FUN.VALUE=1, d.centr[which(traitT.3Dcor$INVASION.STATUS == "native")])
yAxisT.3Dcor.obs$natur <- vapply(xAxis, calcCDF, FUN.VALUE=1, d.centr[which(traitT.3Dcor$INVASION.STATUS == "naturalized")])
yAxisT.3Dcor.obs$invasive <- vapply(xAxis, calcCDF, FUN.VALUE=1, d.centr[which(traitT.3Dcor$INVASION.STATUS == "invasive")])

#CDF plot
plot(xAxis, yAxisT.3Dcor.obs$native, type='l', axes=T, xlab="Distance from centroid", ylab="Proportion of species", main="Grassland and heathland vegetation", lwd=3,bty="n", col="blue", las=1, cex.lab=1.2, cex.axis=1.2, cex.main=1.2)
lines(xAxis, yAxisT.3Dcor.obs$natur, type='l', lwd=3, col="yellow")
lines(xAxis, yAxisT.3Dcor.obs$invasive, type='l', lwd=3, col="red3")

###Ruderal and weed vegetation ---------------------------------------------------------------------------
yAxisX.3Dcor.rand <- list()

#randomize distances between natives and invasive
set.seed(1234)
yAxisX.3Dcor.rand$inv <- dist.shuffler.3D(scores=traitX.3Dcor[,1:3], status=traitX.3Dcor$INVASION.STATUS, test="invasive", Nrand=999)

#randomize distances between natives and naturalized
set.seed(1234)
yAxisX.3Dcor.rand$natur <- dist.shuffler.3D(scores=traitX.3Dcor[,1:3], status=traitX.3Dcor$INVASION.STATUS, test="naturalized", Nrand=999)

#calculate centroid of native species
centr <- apply(traitX.3Dcor[which(traitX.3Dcor$INVASION.STATUS == "native"), c(1:3)], 2, mean)
centr <- rbind(centr, traitX.3Dcor[,c(1:3)])

#calculate distances of species from observed native centroid
d.centr <- as.matrix(dist(centr))[,1]
d.centr <- decostand(d.centr[-1], "range")

#observed CDF
yAxisX.3Dcor.obs <- list()

yAxisX.3Dcor.obs$native <- vapply(xAxis, calcCDF, FUN.VALUE=1, d.centr[which(traitX.3Dcor$INVASION.STATUS == "native")])
yAxisX.3Dcor.obs$natur <- vapply(xAxis, calcCDF, FUN.VALUE=1, d.centr[which(traitX.3Dcor$INVASION.STATUS == "naturalized")])
yAxisX.3Dcor.obs$invasive <- vapply(xAxis, calcCDF, FUN.VALUE=1, d.centr[which(traitX.3Dcor$INVASION.STATUS == "invasive")])

#CDF plot
plot(xAxis, yAxisX.3Dcor.obs$native, type='l', axes=T, xlab="Distance from centroid", ylab="Proportion of species", main="Ruderal and weed vegetation", lwd=3,bty="n", col="blue", las=1, cex.lab=1.2, cex.axis=1.2, cex.main=1.2)
lines(xAxis, yAxisX.3Dcor.obs$natur, type='l', lwd=3, col="yellow")
lines(xAxis, yAxisX.3Dcor.obs$invasive, type='l', lwd=3, col="red3")

###Rock and scree vegetation ---------------------------------------------------------------------------
yAxisS.3Dcor.rand <- list()

#randomize distances between natives and invasive
set.seed(1234)
yAxisS.3Dcor.rand$inv <- dist.shuffler.3D(scores=traitS.3Dcor[,1:3], status=traitS.3Dcor$INVASION.STATUS, test="invasive", Nrand=999)

#randomize distances between natives and naturalized
set.seed(1234)
yAxisS.3Dcor.rand$natur <- dist.shuffler.3D(scores=traitS.3Dcor[,1:3], status=traitS.3Dcor$INVASION.STATUS, test="naturalized", Nrand=999)

#calculate centroid of native species
centr <- apply(traitS.3Dcor[which(traitS.3Dcor$INVASION.STATUS == "native"), c(1:3)], 2, mean)
centr <- rbind(centr, traitS.3Dcor[,c(1:3)])

#calculate distances of species from observed native centroid
d.centr <- as.matrix(dist(centr))[,1]
d.centr <- decostand(d.centr[-1], "range")

#observed CDF
yAxisS.3Dcor.obs <- list()

yAxisS.3Dcor.obs$native <- vapply(xAxis, calcCDF, FUN.VALUE=1, d.centr[which(traitS.3Dcor$INVASION.STATUS == "native")])
yAxisS.3Dcor.obs$natur <- vapply(xAxis, calcCDF, FUN.VALUE=1, d.centr[which(traitS.3Dcor$INVASION.STATUS == "naturalized")])
yAxisS.3Dcor.obs$invasive <- vapply(xAxis, calcCDF, FUN.VALUE=1, d.centr[which(traitS.3Dcor$INVASION.STATUS == "invasive")])

#CDF plot
plot(xAxis, yAxisS.3Dcor.obs$native, type='l', axes=T, xlab="Distance from centroid", ylab="Proportion of species", main="Rock and scree vegetation", lwd=3,bty="n", col="blue", las=1, cex.lab=1.2, cex.axis=1.2, cex.main=1.2)
lines(xAxis, yAxisS.3Dcor.obs$natur, type='l', lwd=3, col="yellow")
lines(xAxis, yAxisS.3Dcor.obs$invasive, type='l', lwd=3, col="red3")

###Wetland vegetation ---------------------------------------------------------------------------
yAxisM.3Dcor.rand <- list()

#randomize distances between natives and invasive
set.seed(1234)
yAxisM.3Dcor.rand$inv <- dist.shuffler.3D(scores=traitM.3Dcor[,1:3], status=traitM.3Dcor$INVASION.STATUS, test="invasive", Nrand=999)

#randomize distances between natives and naturalized
set.seed(1234)
yAxisM.3Dcor.rand$natur <- dist.shuffler.3D(scores=traitM.3Dcor[,1:3], status=traitM.3Dcor$INVASION.STATUS, test="naturalized", Nrand=999)

#calculate centroid of native species
centr <- apply(traitM.3Dcor[which(traitM.3Dcor$INVASION.STATUS == "native"), c(1:3)], 2, mean)
centr <- rbind(centr, traitM.3Dcor[,c(1:3)])

#calculate distances of species from observed native centroid
d.centr <- as.matrix(dist(centr))[,1]
d.centr <- decostand(d.centr[-1], "range")

#observed CDF
yAxisM.3Dcor.obs <- list()

yAxisM.3Dcor.obs$native <- vapply(xAxis, calcCDF, FUN.VALUE=1, d.centr[which(traitM.3Dcor$INVASION.STATUS == "native")])
yAxisM.3Dcor.obs$natur <- vapply(xAxis, calcCDF, FUN.VALUE=1, d.centr[which(traitM.3Dcor$INVASION.STATUS == "naturalized")])
yAxisM.3Dcor.obs$invasive <- vapply(xAxis, calcCDF, FUN.VALUE=1, d.centr[which(traitM.3Dcor$INVASION.STATUS == "invasive")])

#CDF plot
plot(xAxis, yAxisM.3Dcor.obs$native, type='l', axes=T, xlab="Distance from centroid", ylab="Proportion of species", main="Wetland vegetation", lwd=3,bty="n", col="blue", las=1, cex.lab=1.2, cex.axis=1.2, cex.main=1.2)
lines(xAxis, yAxisM.3Dcor.obs$natur, type='l', lwd=3, col="yellow")
lines(xAxis, yAxisM.3Dcor.obs$invasive, type='l', lwd=3, col="red3")

###Scrub vegetation ---------------------------------------------------------------------------
yAxisK.3Dcor.rand <- list()

#randomize distances between natives and invasive
set.seed(1234)
yAxisK.3Dcor.rand$inv <- dist.shuffler.3D(scores=traitK.3Dcor[,1:3], status=traitK.3Dcor$INVASION.STATUS, test="invasive", Nrand=999)

#randomize distances between natives and naturalized
set.seed(1234)
yAxisK.3Dcor.rand$natur <- dist.shuffler.3D(scores=traitK.3Dcor[,1:3], status=traitK.3Dcor$INVASION.STATUS, test="naturalized", Nrand=999)

#calculate centroid of native species
centr <- apply(traitK.3Dcor[which(traitK.3Dcor$INVASION.STATUS == "native"), c(1:3)], 2, mean)
centr <- rbind(centr, traitK.3Dcor[,c(1:3)])

#calculate distances of species from observed native centroid
d.centr <- as.matrix(dist(centr))[,1]
d.centr <- decostand(d.centr[-1], "range")

#observed CDF
yAxisK.3Dcor.obs <- list()

yAxisK.3Dcor.obs$native <- vapply(xAxis, calcCDF, FUN.VALUE=1, d.centr[which(traitK.3Dcor$INVASION.STATUS == "native")])
yAxisK.3Dcor.obs$natur <- vapply(xAxis, calcCDF, FUN.VALUE=1, d.centr[which(traitK.3Dcor$INVASION.STATUS == "naturalized")])
yAxisK.3Dcor.obs$invasive <- vapply(xAxis, calcCDF, FUN.VALUE=1, d.centr[which(traitK.3Dcor$INVASION.STATUS == "invasive")])

#CDF plot
plot(xAxis, yAxisK.3Dcor.obs$native, type='l', axes=T, xlab="Distance from centroid", ylab="Proportion of species", main="Scrub vegetation", lwd=3,bty="n", col="blue", las=1, cex.lab=1.2, cex.axis=1.2, cex.main=1.2)
lines(xAxis, yAxisK.3Dcor.obs$natur, type='l', lwd=3, col="yellow")
lines(xAxis, yAxisK.3Dcor.obs$invasive, type='l', lwd=3, col="red3")

###Forest vegetation ---------------------------------------------------------------------------
yAxisL.3Dcor.rand <- list()

#randomize distances between natives and invasive
set.seed(1234)
yAxisL.3Dcor.rand$inv <- dist.shuffler.3D(scores=traitL.3Dcor[,1:3], status=traitL.3Dcor$INVASION.STATUS, test="invasive", Nrand=999)

#randomize distances between natives and naturalized
set.seed(1234)
yAxisL.3Dcor.rand$natur <- dist.shuffler.3D(scores=traitL.3Dcor[,1:3], status=traitL.3Dcor$INVASION.STATUS, test="naturalized", Nrand=999)

#calculate centroid of native species
centr <- apply(traitL.3Dcor[which(traitL.3Dcor$INVASION.STATUS == "native"), c(1:3)], 2, mean)
centr <- rbind(centr, traitL.3Dcor[,c(1:3)])

#calculate distances of species from observed native centroid
d.centr <- as.matrix(dist(centr))[,1]
d.centr <- decostand(d.centr[-1], "range")

#observed CDF
yAxisL.3Dcor.obs <- list()

yAxisL.3Dcor.obs$native <- vapply(xAxis, calcCDF, FUN.VALUE=1, d.centr[which(traitL.3Dcor$INVASION.STATUS == "native")])
yAxisL.3Dcor.obs$natur <- vapply(xAxis, calcCDF, FUN.VALUE=1, d.centr[which(traitL.3Dcor$INVASION.STATUS == "naturalized")])
yAxisL.3Dcor.obs$invasive <- vapply(xAxis, calcCDF, FUN.VALUE=1, d.centr[which(traitL.3Dcor$INVASION.STATUS == "invasive")])

#CDF plot
plot(xAxis, yAxisL.3Dcor.obs$native, type='l', axes=T, xlab="Distance from centroid", ylab="Proportion of species", main="Forest vegetation", lwd=3,bty="n", col="blue", las=1, cex.lab=1.2, cex.axis=1.2, cex.main=1.2)
lines(xAxis, yAxisL.3Dcor.obs$natur, type='l', lwd=3, col="yellow")
lines(xAxis, yAxisL.3Dcor.obs$invasive, type='l', lwd=3, col="red3")

#############################################################################################
###RANDOMIZE INVASIVE AND NATURALIZED SPECIES IN DATASET WITH IMPUTED SPECIES TRAITS---------
par(mar=c(4, 4, 2.5, 1.5))

###Grassland and heathland vegetation ---------------------------------------------------------------------------
yAxisT.3Dcor.full.rand <- list()

#randomize distances between natives and invasive
set.seed(1234)
yAxisT.3Dcor.full.rand$inv <- dist.shuffler.3D(scores=traitT.3Dcor.full[,1:3], status=traitT.3Dcor.full$INVASION.STATUS, test="invasive", Nrand=999)

#randomize distances between natives and naturalized
set.seed(1234)
yAxisT.3Dcor.full.rand$natur <- dist.shuffler.3D(scores=traitT.3Dcor.full[,1:3], status=traitT.3Dcor.full$INVASION.STATUS, test="naturalized", Nrand=999)

#calculate centroid of native species
centr <- apply(traitT.3Dcor.full[which(traitT.3Dcor.full$INVASION.STATUS == "native"), c(1:3)], 2, mean)
centr <- rbind(centr, traitT.3Dcor.full[,c(1:3)])

#calculate distances of species from observed native centroid
d.centr <- as.matrix(dist(centr))[,1]
d.centr <- decostand(d.centr[-1], "range")

#observed CDF
yAxisT.3Dcor.full.obs <- list()

yAxisT.3Dcor.full.obs$native <- vapply(xAxis, calcCDF, FUN.VALUE=1, d.centr[which(traitT.3Dcor.full$INVASION.STATUS == "native")])
yAxisT.3Dcor.full.obs$natur <- vapply(xAxis, calcCDF, FUN.VALUE=1, d.centr[which(traitT.3Dcor.full$INVASION.STATUS == "naturalized")])
yAxisT.3Dcor.full.obs$invasive <- vapply(xAxis, calcCDF, FUN.VALUE=1, d.centr[which(traitT.3Dcor.full$INVASION.STATUS == "invasive")])

#CDF plot
plot(xAxis, yAxisT.3Dcor.full.obs$native, type='l', axes=T, xlab="Distance from centroid", ylab="Proportion of species", main="Grassland and heathland vegetation", lwd=3,bty="n", col="blue", las=1, cex.lab=1.2, cex.axis=1.2, cex.main=1.2)
lines(xAxis, yAxisT.3Dcor.full.obs$natur, type='l', lwd=3, col="yellow")
lines(xAxis, yAxisT.3Dcor.full.obs$invasive, type='l', lwd=3, col="red3")

###Ruderal and weed vegetation ---------------------------------------------------------------------------
yAxisX.3Dcor.full.rand <- list()

#randomize distances between natives and invasive
set.seed(1234)
yAxisX.3Dcor.full.rand$inv <- dist.shuffler.3D(scores=traitX.3Dcor.full[,1:3], status=traitX.3Dcor.full$INVASION.STATUS, test="invasive", Nrand=999)

#randomize distances between natives and naturalized
set.seed(1234)
yAxisX.3Dcor.full.rand$natur <- dist.shuffler.3D(scores=traitX.3Dcor.full[,1:3], status=traitX.3Dcor.full$INVASION.STATUS, test="naturalized", Nrand=999)

#calculate centroid of native species
centr <- apply(traitX.3Dcor.full[which(traitX.3Dcor.full$INVASION.STATUS == "native"), c(1:3)], 2, mean)
centr <- rbind(centr, traitX.3Dcor.full[,c(1:3)])

#calculate distances of species from observed native centroid
d.centr <- as.matrix(dist(centr))[,1]
d.centr <- decostand(d.centr[-1], "range")

#observed CDF
yAxisX.3Dcor.full.obs <- list()

yAxisX.3Dcor.full.obs$native <- vapply(xAxis, calcCDF, FUN.VALUE=1, d.centr[which(traitX.3Dcor.full$INVASION.STATUS == "native")])
yAxisX.3Dcor.full.obs$natur <- vapply(xAxis, calcCDF, FUN.VALUE=1, d.centr[which(traitX.3Dcor.full$INVASION.STATUS == "naturalized")])
yAxisX.3Dcor.full.obs$invasive <- vapply(xAxis, calcCDF, FUN.VALUE=1, d.centr[which(traitX.3Dcor.full$INVASION.STATUS == "invasive")])

#CDF plot
plot(xAxis, yAxisX.3Dcor.full.obs$native, type='l', axes=T, xlab="Distance from centroid", ylab="Proportion of species", main="Ruderal and weed vegetation", lwd=3,bty="n", col="blue", las=1, cex.lab=1.2, cex.axis=1.2, cex.main=1.2)
lines(xAxis, yAxisX.3Dcor.full.obs$natur, type='l', lwd=3, col="yellow")
lines(xAxis, yAxisX.3Dcor.full.obs$invasive, type='l', lwd=3, col="red3")

###Rock and scree vegetation ---------------------------------------------------------------------------
yAxisS.3Dcor.full.rand <- list()

#randomize distances between natives and invasive
set.seed(1234)
yAxisS.3Dcor.full.rand$inv <- dist.shuffler.3D(scores=traitS.3Dcor.full[,1:3], status=traitS.3Dcor.full$INVASION.STATUS, test="invasive", Nrand=999)

#randomize distances between natives and naturalized
set.seed(1234)
yAxisS.3Dcor.full.rand$natur <- dist.shuffler.3D(scores=traitS.3Dcor.full[,1:3], status=traitS.3Dcor.full$INVASION.STATUS, test="naturalized", Nrand=999)

#calculate centroid of native species
centr <- apply(traitS.3Dcor.full[which(traitS.3Dcor.full$INVASION.STATUS == "native"), c(1:3)], 2, mean)
centr <- rbind(centr, traitS.3Dcor.full[,c(1:3)])

#calculate distances of species from observed native centroid
d.centr <- as.matrix(dist(centr))[,1]
d.centr <- decostand(d.centr[-1], "range")

#observed CDF
yAxisS.3Dcor.full.obs <- list()

yAxisS.3Dcor.full.obs$native <- vapply(xAxis, calcCDF, FUN.VALUE=1, d.centr[which(traitS.3Dcor.full$INVASION.STATUS == "native")])
yAxisS.3Dcor.full.obs$natur <- vapply(xAxis, calcCDF, FUN.VALUE=1, d.centr[which(traitS.3Dcor.full$INVASION.STATUS == "naturalized")])
yAxisS.3Dcor.full.obs$invasive <- vapply(xAxis, calcCDF, FUN.VALUE=1, d.centr[which(traitS.3Dcor.full$INVASION.STATUS == "invasive")])

#CDF plot
plot(xAxis, yAxisS.3Dcor.full.obs$native, type='l', axes=T, xlab="Distance from centroid", ylab="Proportion of species", main="Rock and scree vegetation", lwd=3,bty="n", col="blue", las=1, cex.lab=1.2, cex.axis=1.2, cex.main=1.2)
lines(xAxis, yAxisS.3Dcor.full.obs$natur, type='l', lwd=3, col="yellow")
lines(xAxis, yAxisS.3Dcor.full.obs$invasive, type='l', lwd=3, col="red3")

###Wetland vegetation ---------------------------------------------------------------------------
yAxisM.3Dcor.full.rand <- list()

#randomize distances between natives and invasive
set.seed(1234)
yAxisM.3Dcor.full.rand$inv <- dist.shuffler.3D(scores=traitM.3Dcor.full[,1:3], status=traitM.3Dcor.full$INVASION.STATUS, test="invasive", Nrand=999)

#randomize distances between natives and naturalized
set.seed(1234)
yAxisM.3Dcor.full.rand$natur <- dist.shuffler.3D(scores=traitM.3Dcor.full[,1:3], status=traitM.3Dcor.full$INVASION.STATUS, test="naturalized", Nrand=999)

#calculate centroid of native species
centr <- apply(traitM.3Dcor.full[which(traitM.3Dcor.full$INVASION.STATUS == "native"), c(1:3)], 2, mean)
centr <- rbind(centr, traitM.3Dcor.full[,c(1:3)])

#calculate distances of species from observed native centroid
d.centr <- as.matrix(dist(centr))[,1]
d.centr <- decostand(d.centr[-1], "range")

#observed CDF
yAxisM.3Dcor.full.obs <- list()

yAxisM.3Dcor.full.obs$native <- vapply(xAxis, calcCDF, FUN.VALUE=1, d.centr[which(traitM.3Dcor.full$INVASION.STATUS == "native")])
yAxisM.3Dcor.full.obs$natur <- vapply(xAxis, calcCDF, FUN.VALUE=1, d.centr[which(traitM.3Dcor.full$INVASION.STATUS == "naturalized")])
yAxisM.3Dcor.full.obs$invasive <- vapply(xAxis, calcCDF, FUN.VALUE=1, d.centr[which(traitM.3Dcor.full$INVASION.STATUS == "invasive")])

#CDF plot
plot(xAxis, yAxisM.3Dcor.full.obs$native, type='l', axes=T, xlab="Distance from centroid", ylab="Proportion of species", main="Wetland vegetation", lwd=3,bty="n", col="blue", las=1, cex.lab=1.2, cex.axis=1.2, cex.main=1.2)
lines(xAxis, yAxisM.3Dcor.full.obs$natur, type='l', lwd=3, col="yellow")
lines(xAxis, yAxisM.3Dcor.full.obs$invasive, type='l', lwd=3, col="red3")

###Scrub vegetation ---------------------------------------------------------------------------
yAxisK.3Dcor.full.rand <- list()

#randomize distances between natives and invasive
set.seed(1234)
yAxisK.3Dcor.full.rand$inv <- dist.shuffler.3D(scores=traitK.3Dcor.full[,1:3], status=traitK.3Dcor.full$INVASION.STATUS, test="invasive", Nrand=999)

#randomize distances between natives and naturalized
set.seed(1234)
yAxisK.3Dcor.full.rand$natur <- dist.shuffler.3D(scores=traitK.3Dcor.full[,1:3], status=traitK.3Dcor.full$INVASION.STATUS, test="naturalized", Nrand=999)

#calculate centroid of native species
centr <- apply(traitK.3Dcor.full[which(traitK.3Dcor.full$INVASION.STATUS == "native"), c(1:3)], 2, mean)
centr <- rbind(centr, traitK.3Dcor.full[,c(1:3)])

#calculate distances of species from observed native centroid
d.centr <- as.matrix(dist(centr))[,1]
d.centr <- decostand(d.centr[-1], "range")

#observed CDF
yAxisK.3Dcor.full.obs <- list()

yAxisK.3Dcor.full.obs$native <- vapply(xAxis, calcCDF, FUN.VALUE=1, d.centr[which(traitK.3Dcor.full$INVASION.STATUS == "native")])
yAxisK.3Dcor.full.obs$natur <- vapply(xAxis, calcCDF, FUN.VALUE=1, d.centr[which(traitK.3Dcor.full$INVASION.STATUS == "naturalized")])
yAxisK.3Dcor.full.obs$invasive <- vapply(xAxis, calcCDF, FUN.VALUE=1, d.centr[which(traitK.3Dcor.full$INVASION.STATUS == "invasive")])

#CDF plot
plot(xAxis, yAxisK.3Dcor.full.obs$native, type='l', axes=T, xlab="Distance from centroid", ylab="Proportion of species", main="Scrub vegetation", lwd=3,bty="n", col="blue", las=1, cex.lab=1.2, cex.axis=1.2, cex.main=1.2)
lines(xAxis, yAxisK.3Dcor.full.obs$natur, type='l', lwd=3, col="yellow")
lines(xAxis, yAxisK.3Dcor.full.obs$invasive, type='l', lwd=3, col="red3")

###Forest vegetation ---------------------------------------------------------------------------
yAxisL.3Dcor.full.rand <- list()

#randomize distances between natives and invasive
set.seed(1234)
yAxisL.3Dcor.full.rand$inv <- dist.shuffler.3D(scores=traitL.3Dcor.full[,1:3], status=traitL.3Dcor.full$INVASION.STATUS, test="invasive", Nrand=999)

#randomize distances between natives and naturalized
set.seed(1234)
yAxisL.3Dcor.full.rand$natur <- dist.shuffler.3D(scores=traitL.3Dcor.full[,1:3], status=traitL.3Dcor.full$INVASION.STATUS, test="naturalized", Nrand=999)

#calculate centroid of native species
centr <- apply(traitL.3Dcor.full[which(traitL.3Dcor.full$INVASION.STATUS == "native"), c(1:3)], 2, mean)
centr <- rbind(centr, traitL.3Dcor.full[,c(1:3)])

#calculate distances of species from observed native centroid
d.centr <- as.matrix(dist(centr))[,1]
d.centr <- decostand(d.centr[-1], "range")

#observed CDF
yAxisL.3Dcor.full.obs <- list()

yAxisL.3Dcor.full.obs$native <- vapply(xAxis, calcCDF, FUN.VALUE=1, d.centr[which(traitL.3Dcor.full$INVASION.STATUS == "native")])
yAxisL.3Dcor.full.obs$natur <- vapply(xAxis, calcCDF, FUN.VALUE=1, d.centr[which(traitL.3Dcor.full$INVASION.STATUS == "naturalized")])
yAxisL.3Dcor.full.obs$invasive <- vapply(xAxis, calcCDF, FUN.VALUE=1, d.centr[which(traitL.3Dcor.full$INVASION.STATUS == "invasive")])

#CDF plot
plot(xAxis, yAxisL.3Dcor.full.obs$native, type='l', axes=T, xlab="Distance from centroid", ylab="Proportion of species", main="Forest vegetation", lwd=3,bty="n", col="blue", las=1, cex.lab=1.2, cex.axis=1.2, cex.main=1.2)
lines(xAxis, yAxisL.3Dcor.full.obs$natur, type='l', lwd=3, col="yellow")
lines(xAxis, yAxisL.3Dcor.full.obs$invasive, type='l', lwd=3, col="red3")
