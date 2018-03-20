#####################################################################################
#                                       READ DATA                                   #
#####################################################################################

#code by Jan Divisek (2015-2017)

#read list of species with traits
trait <- read.delim("SpecList.txt", header=T, row.names = 1)
head(trait)
dim(trait)

trait$INVASION.STATUS <- factor(trait$INVASION.STATUS, levels=c("native", "casual", "naturalized", "invasive"))

#read list of species - NAs in traits were replaced by average for genus or family
trait.full <- read.delim("SpecList_FullTraits.txt", header=T, row.names = 1)
head(trait.full)
dim(trait.full)

trait.full$INVASION.STATUS <- factor(trait.full$INVASION.STATUS, levels=c("native", "casual", "naturalized", "invasive"))

#read phylogenetic tree (Durka & Michalski, 2012)
tmp.env <- new.env()
load("C:/Users/Geonika/Documents/R Working Directory/Trait_statistics_for_sharing/phy_tree.RData", envir=tmp.env)
phy.tree <- get("phy.tree", pos=tmp.env)
rm(tmp.env)

length(phy.tree$tip.label) #check the number of species in the tree

#transform and scale species traits
trait.s <- trait
trait.s[,3:5] <- scale(log10(trait[,3:5]))
head(trait.s)

trait.full.s <- trait.full 
trait.full.s[,3:5] <- scale(log10(trait.full.s[,3:5]))
head(trait.full.s)

#separate individual habitats
traitT <- trait[trait$T == 1, ] #Grassland and heathland vegetation below the timberline
traitX <- trait[trait$X == 1, ] #Ruderal and weed vegetation
traitS <- trait[trait$S == 1, ] #Rock and scree vegetation
traitM <- trait[trait$M == 1, ] #Wetland vegetation
traitK <- trait[trait$K == 1, ] #Scrub vegetation
traitL <- trait[trait$L == 1, ] #Forest vegetation

#some statistics
table(traitT$INVASION.STATUS); summary(traitT[,3:5])
table(traitX$INVASION.STATUS); summary(traitX[,3:5])
table(traitS$INVASION.STATUS); summary(traitS[,3:5])
table(traitM$INVASION.STATUS); summary(traitM[,3:5])
table(traitK$INVASION.STATUS); summary(traitK[,3:5])
table(traitL$INVASION.STATUS); summary(traitL[,3:5])

traitT.full <- trait.full[trait.full$T == 1, ] #Grassland and heathland vegetation below the timberline
traitX.full <- trait.full[trait.full$X == 1, ] #Ruderal and weed vegetation
traitS.full <- trait.full[trait.full$S == 1, ] #Rock and scree vegetation
traitM.full <- trait.full[trait.full$M == 1, ] #Wetland vegetation
traitK.full <- trait.full[trait.full$K == 1, ] #Scrub vegetation
traitL.full <- trait.full[trait.full$L == 1, ] #Forest vegetation

#some statistics
table(traitT.full$INVASION.STATUS); summary(traitT.full[,3:5])
table(traitX.full$INVASION.STATUS); summary(traitX.full[,3:5])
table(traitS.full$INVASION.STATUS); summary(traitS.full[,3:5])
table(traitM.full$INVASION.STATUS); summary(traitM.full[,3:5])
table(traitK.full$INVASION.STATUS); summary(traitK.full[,3:5])
table(traitL.full$INVASION.STATUS); summary(traitL.full[,3:5])

# source required functions
myfunctions <- list.files(paste(getwd(), "/MyFunctions", sep=""), full.names=TRUE)
for (i in 1:length(myfunctions))
{
  source(myfunctions[i])
}

####################################################################################################
#            ACCOUNT FOR PHYLOGENETIC SIGNAL IN INDIVIDUAL SPECIES TRAITS                          #
####################################################################################################

library(ade4)
library(spdep)
library(adephylo)

#calculate phylogenetic distances between each pair of species
dist.Ab.all <- as.matrix(distTips(phy.tree, method = "Abouheif"))
dist.Ab.all[1:5,1:5]
dim(dist.Ab.all)

###ASSEMBLE DATA FOR PHYLOGENETIC ANALYSIS
trait.phy <- list()

#Plant height
trait.phy$Plant.height <- trait[phy.tree$tip.label, c(6:11, 2, 3)]
nrow(trait.phy$Plant.height)
trait.phy$Plant.height <- trait.phy$Plant.height[complete.cases(trait.phy$Plant.height), ] #remove NAs
nrow(trait.phy$Plant.height)
trait.phy$Plant.height$HEIGHT.MAX.log <- log10(trait.phy$Plant.height$HEIGHT.MAX)

#SLA
trait.phy$SLA <- trait[phy.tree$tip.label, c(6:11, 2, 4)]
nrow(trait.phy$SLA)
trait.phy$SLA <- trait.phy$SLA[complete.cases(trait.phy$SLA), ]
nrow(trait.phy$SLA)
trait.phy$SLA$sla.avg.mm2mg.log <- log10(trait.phy$SLA$sla.avg.mm2mg.)

#Germinule
trait.phy$Germinule <- trait[phy.tree$tip.label, c(6:11, 2, 5)]
nrow(trait.phy$Germinule)
trait.phy$Germinule <- trait.phy$Germinule[complete.cases(trait.phy$Germinule), ]
nrow(trait.phy$Germinule)
trait.phy$Germinule$Germinule.log <- log10(trait.phy$Germinule$Germinule)

###############################################################################################
###SELECT PHYLOGENETIC EIGENVECTORS TO FILTER OUT PYLOGENETIC SIGNAL IN EACH TRAIT-------------

ME.single <- list()

#NOTE!!!!!!!!!!!!!!!!!!!!!!
#SELECTION OF EIGENVECTORS TAKES VERY, VERY LONG TIME (ABOUT THREE DAYS!!!)
#INSTEAD IMPORT DATASET WITH ALREADY SELECTED EIGENVECTORS

# ###Plant Height
# #Calculate Moran eigenvetors to filter out phylogenetic autocorrelation
# D.sel <- dist.Ab.all[rownames(trait.phy$Plant.height),rownames(trait.phy$Plant.height)]
# prox <- (1/D.sel)^1 #convert distances to proximities
# diag(prox) <- 0
# prox <- prop.table(prox, 1) #normalize by row
# prox <- 0.5 * (prox + t(prox)) #make matrix symmetric
# ME <- me.phylo(prox = prox) #here Abouheif matrix without diagonal is used
# head(ME)
# 
# #create oriAbouheif proximity matrix (with diagonal) to test phylogenetic autocorrelation using Abouheif's Cmean
# D.sel <- dist.Ab.all[rownames(trait.phy$Plant.height),rownames(trait.phy$Plant.height)]
# prox <- (1/D.sel)^1 #convert distances to proximities
# diag(prox) <- 0
# sumMarg <- apply(prox, 1, sum) #make oriAbouheif diagonal
# diag(prox) <- (1 - sumMarg)
# 
# #Test phylogenetic autocorrelation in trait data
# abouheif.moran(x = trait.phy$Plant.height$HEIGHT.MAX.log, W = prox, method = "oriAbouheif", nrepet = 999)
# 
# #select eigenvectors
# ME.single$Plant.height <- phylo.fwd(tr = trait.phy$Plant.height$HEIGHT.MAX.log, prox = prox, 
#                                     ME=ME, method="oriAbouheif", Nperm = 999)
# head(ME.single$Plant.height)
# 
# ###SLA
# #Calculate Moran eigenvetors to filter out phylogenetic autocorrelation
# D.sel <- dist.Ab.all[rownames(trait.phy$SLA),rownames(trait.phy$SLA)]
# prox <- (1/D.sel)^1 #convert distances to proximities
# diag(prox) <- 0
# prox <- prop.table(prox, 1) #normalize by row
# prox <- 0.5 * (prox + t(prox)) #make matrix symmetric
# ME <- me.phylo(prox = prox) #here Abouheif matrix without diagonal is used
# head(ME)
# 
# #create oriAbouheif proximity matrix (with diagonal) to test phylogenetic autocorrelation using Abouheif's Cmean
# D.sel <- dist.Ab.all[rownames(trait.phy$SLA),rownames(trait.phy$SLA)]
# prox <- (1/D.sel)^1 #convert distances to proximities
# diag(prox) <- 0
# sumMarg <- apply(prox, 1, sum) #make oriAbouheif diagonal
# diag(prox) <- (1 - sumMarg)
# 
# #Test phylogenetic autocorrelation in trait data
# abouheif.moran(x = trait.phy$SLA$sla.avg.mm2mg.log, W = prox, method = "oriAbouheif", nrepet = 999)
# 
# #select eigenvectors
# ME.single$SLA <- phylo.fwd(tr = trait.phy$SLA$sla.avg.mm2mg.log, prox = prox, 
#                            ME=ME, method="oriAbouheif", Nperm = 999)
# head(ME.single$SLA)
# 
# ###Germinule
# #Calculate Moran eigenvetors to filter out phylogenetic autocorrelation
# D.sel <- dist.Ab.all[rownames(trait.phy$Germinule),rownames(trait.phy$Germinule)]
# prox <- (1/D.sel)^1 #convert distances to proximities
# diag(prox) <- 0
# prox <- prop.table(prox, 1) #normalize by row
# prox <- 0.5 * (prox + t(prox)) #make matrix symmetric
# ME <- me.phylo(prox = prox) #here Abouheif matrix without diagonal is used
# head(ME)
# 
# #create oriAbouheif proximity matrix (with diagonal) to test phylogenetic autocorrelation using Abouheif's Cmean
# D.sel <- dist.Ab.all[rownames(trait.phy$Germinule),rownames(trait.phy$Germinule)]
# prox <- (1/D.sel)^1 #convert distances to proximities
# diag(prox) <- 0
# sumMarg <- apply(prox, 1, sum) #make oriAbouheif diagonal
# diag(prox) <- (1 - sumMarg)
# 
# #Test phylogenetic autocorrelation in trait data
# abouheif.moran(x = trait.phy$Germinule$Germinule.log, W = prox, method = "oriAbouheif", nrepet = 999)
# 
# #select eigenvectors
# ME.single$Germinule <- phylo.fwd(tr = trait.phy$Germinule$Germinule.log, prox = prox, 
#                                  ME=ME, method="oriAbouheif", Nperm = 999)
# head(ME.single$Germinule)

ME.single$Plant.height <- read.delim("ME.single_Plant.height.txt", header=T, row.names = 1)
ME.single$SLA <- read.delim("ME.single_SLA.txt", header=T, row.names = 1)
ME.single$Germinule <- read.delim("ME.single_Germinule.txt", header=T, row.names = 1)

###Filter out phylogenetic autocorrelation

#Plant height
trait.phy$Plant.height$HEIGHT.MAX.cor <- resid(lm(trait.phy$Plant.height$HEIGHT.MAX.log ~., data=ME.single$Plant.height))
head(trait.phy$Plant.height)

#SLA
trait.phy$SLA$sla.avg.mm2mg.cor <- resid(lm(trait.phy$SLA$sla.avg.mm2mg.log ~., data=ME.single$SLA))
head(trait.phy$SLA)

#Germinule
trait.phy$Germinule$Germinule.cor <- resid(lm(trait.phy$Germinule$Germinule.log ~., data=ME.single$Germinule))
head(trait.phy$Germinule)

#divide dataset according to habitats
traitT.phy <- list()
traitT.phy$PlantHeight <- trait.phy$Plant.height[trait.phy$Plant.height$T == 1, ]
traitT.phy$SLA <- trait.phy$SLA[trait.phy$SLA$T == 1, ]
traitT.phy$Germinule <- trait.phy$Germinule[trait.phy$Germinule$T == 1, ]

traitX.phy <- list()
traitX.phy$PlantHeight <- trait.phy$Plant.height[trait.phy$Plant.height$X == 1, ]
traitX.phy$SLA <- trait.phy$SLA[trait.phy$SLA$X == 1, ]
traitX.phy$Germinule <- trait.phy$Germinule[trait.phy$Germinule$X == 1, ]

traitS.phy <- list()
traitS.phy$PlantHeight <- trait.phy$Plant.height[trait.phy$Plant.height$S == 1, ]
traitS.phy$SLA <- trait.phy$SLA[trait.phy$SLA$S == 1, ]
traitS.phy$Germinule <- trait.phy$Germinule[trait.phy$Germinule$S == 1, ]

traitM.phy <- list()
traitM.phy$PlantHeight <- trait.phy$Plant.height[trait.phy$Plant.height$M == 1, ]
traitM.phy$SLA <- trait.phy$SLA[trait.phy$SLA$M == 1, ]
traitM.phy$Germinule <- trait.phy$Germinule[trait.phy$Germinule$M == 1, ]

traitK.phy <- list()
traitK.phy$PlantHeight <- trait.phy$Plant.height[trait.phy$Plant.height$K == 1, ]
traitK.phy$SLA <- trait.phy$SLA[trait.phy$SLA$K == 1, ]
traitK.phy$Germinule <- trait.phy$Germinule[trait.phy$Germinule$K == 1, ]

traitL.phy <- list()
traitL.phy$PlantHeight <- trait.phy$Plant.height[trait.phy$Plant.height$L == 1, ]
traitL.phy$SLA <- trait.phy$SLA[trait.phy$SLA$L == 1, ]
traitL.phy$Germinule <- trait.phy$Germinule[trait.phy$Germinule$L == 1, ]

###############################################################################################
###SELECT PHYLOGENETIC EIGENVECTORS TO FILTER OUT PYLOGENETIC SIGNAL IN IMPUTED TRAITS---------
#only SLA and Germinule

ME.single.full <- list()
ME.single.full$Plant.height <- ME.single$Plant.height

###assemble data
trait.full.phy <- list()

trait.full.phy$Plant.height <- trait.phy$Plant.height
head(trait.full.phy$Plant.height)

trait.full.phy$SLA <- trait.full[phy.tree$tip.label, c(6:11, 2, 4)]
nrow(trait.full.phy$SLA)
summary(trait.full.phy$SLA$sla.avg.mm2mg.)
trait.full.phy$SLA$sla.avg.mm2mg.log <- log10(trait.full.phy$SLA$sla.avg.mm2mg.)

trait.full.phy$Germinule <- trait.full[phy.tree$tip.label, c(6:11, 2, 5)]
nrow(trait.full.phy$Germinule)
summary(trait.full.phy$Germinule$Germinule)
trait.full.phy$Germinule$Germinule.log <- log10(trait.full.phy$Germinule$Germinule)

#NOTE!!!!!!!!!!!!!!!!!!!!!!
#SELECTION OF EIGENVECTORS TAKES VERY, VERY LONG TIME (ABOUT THREE DAYS!!!)
#INSTEAD IMPORT DATASET WITH ALREADY SELECTED EIGENVECTORS

# ###SLA
# #Calculate Moran eigenvetors to filter out phylogenetic autocorrelation
# D.sel <- dist.Ab.all[rownames(trait.full.phy$SLA),rownames(trait.full.phy$SLA)]
# prox <- (1/D.sel)^1 #convert distances to proximities
# diag(prox) <- 0
# prox <- prop.table(prox, 1) #normalize by row
# prox <- 0.5 * (prox + t(prox)) #make matrix symmetric
# ME <- me.phylo(prox = prox) #here Abouheif matrix without diagonal is used
# head(ME)
# 
# #create oriAbouheif proximity matrix (with diagonal) to test phylogenetic autocorrelation using Abouheif's Cmean
# D.sel <- dist.Ab.all[rownames(trait.full.phy$SLA),rownames(trait.full.phy$SLA)]
# prox <- (1/D.sel)^1 #convert distances to proximities
# diag(prox) <- 0
# sumMarg <- apply(prox, 1, sum) #make oriAbouheif diagonal
# diag(prox) <- (1 - sumMarg)
# 
# #Test phylogenetic autocorrelation in trait data
# abouheif.moran(x = trait.full.phy$SLA$sla.avg.mm2mg.log, W = prox, method = "oriAbouheif", nrepet = 999)
# 
# #select eigenvectors
# ME.single.full$SLA <- phylo.fwd(tr = trait.full.phy$SLA$sla.avg.mm2mg.log, prox = prox, 
#                                 ME=ME, method="oriAbouheif", Nperm = 999)
# head(ME.single.full$SLA)
# 
# ###Germinule
# #Calculate Moran eigenvetors to filter out phylogenetic autocorrelation
# D.sel <- dist.Ab.all[rownames(trait.full.phy$Germinule),rownames(trait.full.phy$Germinule)]
# prox <- (1/D.sel)^1 #convert distances to proximities
# diag(prox) <- 0
# prox <- prop.table(prox, 1) #normalize by row
# prox <- 0.5 * (prox + t(prox)) #make matrix symmetric
# ME <- me.phylo(prox = prox) #here Abouheif matrix without diagonal is used
# head(ME)
# 
# #create oriAbouheif proximity matrix (with diagonal) to test phylogenetic autocorrelation using Abouheif's Cmean
# D.sel <- dist.Ab.all[rownames(trait.full.phy$Germinule),rownames(trait.full.phy$Germinule)]
# prox <- (1/D.sel)^1 #convert distances to proximities
# diag(prox) <- 0
# sumMarg <- apply(prox, 1, sum) #make oriAbouheif diagonal
# diag(prox) <- (1 - sumMarg)
# 
# #Test phylogenetic autocorrelation in trait data
# abouheif.moran(x = trait.full.phy$Germinule$Germinule.log, W = prox, method = "oriAbouheif", nrepet = 999)
# 
# #select eigenvectors
# ME.single.full$Germinule <- phylo.fwd(tr = trait.full.phy$Germinule$Germinule.log, prox = prox, 
#                                       ME=ME, method="oriAbouheif", Nperm = 999)
# head(ME.single.full$Germinule)

ME.single.full$SLA <- read.delim("ME.single.full_SLA.txt", header=T, row.names=1)
ME.single.full$Germinule <- read.delim("ME.single.full_Germinule.txt", header=T, row.names=1)

###Filter out phylogenetic autocorrelation

#Plant height
trait.full.phy$Plant.height$HEIGHT.MAX.cor <- resid(lm(trait.full.phy$Plant.height$HEIGHT.MAX.log ~., data=ME.single.full$Plant.height))
head(trait.full.phy$Plant.height)

#SLA
trait.full.phy$SLA$sla.avg.mm2mg.cor <- resid(lm(trait.full.phy$SLA$sla.avg.mm2mg.log ~., data=ME.single.full$SLA))
head(trait.full.phy$SLA)

#Germinule
trait.full.phy$Germinule$Germinule.cor <- resid(lm(trait.full.phy$Germinule$Germinule.log ~., data=ME.single.full$Germinule))
head(trait.full.phy$Germinule)

#divide dataset according to habitats
traitT.full.phy <- list()
traitT.full.phy$PlantHeight <- trait.full.phy$Plant.height[trait.full.phy$Plant.height$T == 1, ]
traitT.full.phy$SLA <- trait.full.phy$SLA[trait.full.phy$SLA$T == 1, ]
traitT.full.phy$Germinule <- trait.full.phy$Germinule[trait.full.phy$Germinule$T == 1, ]

traitX.full.phy <- list()
traitX.full.phy$PlantHeight <- trait.full.phy$Plant.height[trait.full.phy$Plant.height$X == 1, ]
traitX.full.phy$SLA <- trait.full.phy$SLA[trait.full.phy$SLA$X == 1, ]
traitX.full.phy$Germinule <- trait.full.phy$Germinule[trait.full.phy$Germinule$X == 1, ]

traitS.full.phy <- list()
traitS.full.phy$PlantHeight <- trait.full.phy$Plant.height[trait.full.phy$Plant.height$S == 1, ]
traitS.full.phy$SLA <- trait.full.phy$SLA[trait.full.phy$SLA$S == 1, ]
traitS.full.phy$Germinule <- trait.full.phy$Germinule[trait.full.phy$Germinule$S == 1, ]

traitM.full.phy <- list()
traitM.full.phy$PlantHeight <- trait.full.phy$Plant.height[trait.full.phy$Plant.height$M == 1, ]
traitM.full.phy$SLA <- trait.full.phy$SLA[trait.full.phy$SLA$M == 1, ]
traitM.full.phy$Germinule <- trait.full.phy$Germinule[trait.full.phy$Germinule$M == 1, ]

traitK.full.phy <- list()
traitK.full.phy$PlantHeight <- trait.full.phy$Plant.height[trait.full.phy$Plant.height$K == 1, ]
traitK.full.phy$SLA <- trait.full.phy$SLA[trait.full.phy$SLA$K == 1, ]
traitK.full.phy$Germinule <- trait.full.phy$Germinule[trait.full.phy$Germinule$K == 1, ]

traitL.full.phy <- list()
traitL.full.phy$PlantHeight <- trait.full.phy$Plant.height[trait.full.phy$Plant.height$L == 1, ]
traitL.full.phy$SLA <- trait.full.phy$SLA[trait.full.phy$SLA$L == 1, ]
traitL.full.phy$Germinule <- trait.full.phy$Germinule[trait.full.phy$Germinule$L == 1, ]


####################################################################################################
#              ACCOUNT FOR PHYLOGENETIC SIGNAL IN MULTIVARIATE TRAIT DATA                          #
####################################################################################################

#assemble data
trait.3D <- trait.s[phy.tree$tip.label, c(3:5, 2, 6:11)]
head(trait.3D)

#complete cases
trait.3D <- trait.3D[complete.cases(trait.3D), ]
nrow(trait.3D)

#create phylogenetic proximity table
prox.Ab.all <- proxTips(phy.tree, method = "Abouheif", normalize="none")
prox.Ab.all[1:5,1:5]
dim(prox.Ab.all)

#NOTE!!!!!!!!!!!!!!!!!!!!!!
#SELECTION OF EIGENVECTORS TAKES VERY LONG TIME
#INSTEAD IMPORT DATASET WITH ALREADY SELECTED EIGENVECTORS

# #test phylogenetic correlation
# prox <- prox.Ab.all[rownames(trait.3D), rownames(trait.3D)] #select species in given habitat
# dim(prox)
# w <- mat2listw(prox, style="W") #normalize weights by row and create listw object
# 
# dpca <- dudi.pca(trait.3D[,1:3], center=FALSE, scale=FALSE, scannf=FALSE) #dudi.pca
# biplot(dpca)
# 
# multispati.randtest(dpca, w, nrepet = 999) #rest multivariate phylogenetic autocorrelation
# 
# #Calculate Moran eigenvetors to filter out phylogenetic autocorrelation
# prox <- prop.table(prox, 1) #standardize by row
# prox <- 0.5 * (prox + t(prox)) #make matrix symetric
# prox[1:5,1:5]
# 
# ME <- me.phylo(prox = prox) 
# dim(ME)
# 
# #Select Moran's eigenvectors
# ME.3D <- phylo.multifwd2(tr = trait.3D[,1:3], w = w, ME = ME, nperm=999)
# dim(ME.3D)

ME.3D <- read.delim("ME.3D.txt", header=T, row.names = 1)

###FILTER OUT PHYLOGENETIC AUTOCORRELATION IN MULTIVARIATE TRAIT DATA

trait.3Dcor <- trait.3D
trait.3Dcor[,1:3] <- resid(lm(as.matrix(trait.3D[,1:3]) ~., data=ME.3D))
head(trait.3Dcor)

#remove casuals
trait.3Dcor <- trait.3Dcor[trait.3Dcor$INVASION.STATUS != "casual", ]
nrow(trait.3Dcor)

###########################################################
####PHYLOGENETIC CORRECTION FOR DATASET WITH IMPUTED TRAITS

#assemble data
trait.3D.full <- trait.full.s[phy.tree$tip.label, c(3:5, 2, 6:11)]
head(trait.3D.full)
nrow(trait.3D.full)

#check complete cases
trait.3D.full <- trait.3D.full[complete.cases(trait.3D.full), ]
nrow(trait.3D.full)

#!!!!!!!!!!!!!!!!!!!!!!
#DO NOT RUN THE FOLLOWING CODE - SELECTION OF EIGENVECTORS TAKES VERY LONG TIME
#INSTEAD IMPORT DATASET WITH ALREADY SELECTED EIGENVECTORS

# #test phylogenetic correlation
# prox <- prox.Ab.all[rownames(trait.3D.full), rownames(trait.3D.full)] #select species in given habitat
# dim(prox)
# w <- mat2listw(prox, style="W") #normalize weights by row and create listw object
# 
# dpca <- dudi.pca(trait.3D.full[,1:3], center=FALSE, scale=FALSE, scannf=FALSE) #dudi.pca
# biplot(dpca)
# 
# multispati.randtest(dpca, w, nrepet = 999) #rest multivariate phylogenetic autocorrelation
# 
# #Calculate Moran eigenvetors to filter out phylogenetic autocorrelation
# prox <- prop.table(prox, 1) #standardize by row
# prox <- 0.5 * (prox + t(prox)) #make matrix symetric
# prox[1:5,1:5]
# 
# ME <- me.phylo(prox = prox) 
# dim(ME)
# 
# #Select Moran's eigenvectors
# ME.3D.full <- phylo.multifwd2(tr = trait.3D.full[,1:3], w = w, ME = ME, nperm=999)
# dim(ME.3D.full)

ME.3D.full <- read.delim("ME.3D.full.txt", header = T, row.names = 1)

###FILTER OUT PHYLOGENETIC AUTOCORRELATION IN MULTIVARIATE TRAIT DATA

trait.3Dcor.full <- trait.3D.full
trait.3Dcor.full[,1:3] <- resid(lm(as.matrix(trait.3D.full[,1:3]) ~., data=ME.3D.full))
head(trait.3Dcor.full)

#remove casuals
trait.3Dcor.full <- trait.3Dcor.full[trait.3Dcor.full$INVASION.STATUS != "casual", ]
nrow(trait.3Dcor.full)

