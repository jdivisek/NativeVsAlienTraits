#####################################################################################
#                                       READ DATA                                   #
#####################################################################################

#code by Jan Divisek (2018)

#read list of species with traits
trait <- read.delim("https://www.dropbox.com/s/th2mozf98o6nrda/SpecList.txt?raw=1", header=T, row.names = 1)
head(trait)
dim(trait)

trait$INVASION.STATUS <- factor(trait$INVASION.STATUS, levels=c("native", "casual", "naturalized", "invasive"))

#read phylogenetic tree (Durka & Michalski, 2012)
tmp.env <- new.env()
load("https://www.dropbox.com/s/ry8qbnepd8vhob7/phy_tree.RData?raw=1", envir=tmp.env)
phy.tree <- get("phy.tree", pos=tmp.env)
rm(tmp.env)

length(phy.tree$tip.label) #check the number of species in the tree

#reorder species according to phylogenetic tree
trait <- trait[phy.tree$tip.label, ]
head(trait)

#transform and scale species traits
trait.s <- trait
trait.s[,3:5] <- scale(log10(trait[,3:5]))
head(trait.s)

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

###IMPUTE MISSING TRAIT VALUES---------------------------------------------------------------
#Imputation is based on correlations among traits and species phylogenetic relatedness

library(missForest)
library(adephylo)

#create phylogenetic proximity table
prox.Ab.all <- proxTips(phy.tree, method = "Abouheif", normalize="none")
prox.Ab.all[1:5,1:5]
dim(prox.Ab.all)

#Calculate Moran eigenvetors to filter out phylogenetic autocorrelation
prox <- prop.table(prox.Ab.all, 1) #standardize by row
prox <- 0.5 * (prox + t(prox)) #make matrix symetric
prox[1:5,1:5]

ME <- me.phylo(prox = prox)
dim(ME)
head(ME)

ME <- ME[rownames(trait),]
dim(ME)
head(ME)

##impute missing trait values using missForest function with default settings
set.seed(1234)
trait.imp <- missForest(cbind(log10(trait[,3:5]), ME[,1:10]))

(imp.error <- trait.imp$OOBerror)#check OOB rerror
trait.imp <- as.data.frame(10^trait.imp$ximp[,1:3])

trait.imp <- cbind(trait[,1:2], trait.imp, trait[,6:11])
head(trait.imp)

trait.imp <- trait.imp[phy.tree$tip.label, ]

trait.imp$INVASION.STATUS <- factor(trait.imp$INVASION.STATUS, levels=c("native", "casual", "naturalized", "invasive"))

trait.imp.s <- trait.imp 
trait.imp.s[,3:5] <- scale(log10(trait.imp.s[,3:5]))
head(trait.imp.s)

traitT.imp <- trait.imp[trait.imp$T == 1, ] #Grassland and heathland vegetation below the timberline
traitX.imp <- trait.imp[trait.imp$X == 1, ] #Ruderal and weed vegetation
traitS.imp <- trait.imp[trait.imp$S == 1, ] #Rock and scree vegetation
traitM.imp <- trait.imp[trait.imp$M == 1, ] #Wetland vegetation
traitK.imp <- trait.imp[trait.imp$K == 1, ] #Scrub vegetation
traitL.imp <- trait.imp[trait.imp$L == 1, ] #Forest vegetation

#some statistics
table(traitT.imp$INVASION.STATUS); summary(traitT.imp[,3:5])
table(traitX.imp$INVASION.STATUS); summary(traitX.imp[,3:5])
table(traitS.imp$INVASION.STATUS); summary(traitS.imp[,3:5])
table(traitM.imp$INVASION.STATUS); summary(traitM.imp[,3:5])
table(traitK.imp$INVASION.STATUS); summary(traitK.imp[,3:5])
table(traitL.imp$INVASION.STATUS); summary(traitL.imp[,3:5])

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
trait.phy$Plant.height <- trait[, c(6:11, 2, 3)]
nrow(trait.phy$Plant.height)
trait.phy$Plant.height <- trait.phy$Plant.height[complete.cases(trait.phy$Plant.height), ] #remove NAs
nrow(trait.phy$Plant.height)
trait.phy$Plant.height$HEIGHT.MAX.log <- log10(trait.phy$Plant.height$HEIGHT.MAX)

#SLA
trait.phy$SLA <- trait[, c(6:11, 2, 4)]
nrow(trait.phy$SLA)
trait.phy$SLA <- trait.phy$SLA[complete.cases(trait.phy$SLA), ]
nrow(trait.phy$SLA)
trait.phy$SLA$sla.avg.mm2mg.log <- log10(trait.phy$SLA$sla.avg.mm2mg.)

#Germinule
trait.phy$Germinule <- trait[, c(6:11, 2, 5)]
nrow(trait.phy$Germinule)
trait.phy$Germinule <- trait.phy$Germinule[complete.cases(trait.phy$Germinule), ]
nrow(trait.phy$Germinule)
trait.phy$Germinule$Germinule.log <- log10(trait.phy$Germinule$Germinule)

###############################################################################################
###SELECT PHYLOGENETIC EIGENVECTORS TO FILTER OUT PYLOGENETIC SIGNAL IN EACH TRAIT-------------

ME.single <- list()

#NOTE!!!!!!!!!!!!!!!!!!!!!!
#SELECTION OF EIGENVECTORS TAKES VERY LONG TIME
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
# abouheif.moran(x = trait.phy$Plant.height$HEIGHT.MAX.cor, W = prox, method = "oriAbouheif", nrepet = 999)
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
# set.seed(4321)
# abouheif.moran(x = trait.phy$Germinule$Germinule.cor, W = prox, method = "oriAbouheif", nrepet = 999)
# 
# #select eigenvectors
# ME.single$Germinule <- phylo.fwd(tr = trait.phy$Germinule$Germinule.log, prox = prox,
#                                  ME=ME, method="oriAbouheif", Nperm = 999)
# head(ME.single$Germinule)

ME.single$Plant.height <- read.delim("https://www.dropbox.com/s/xlwmixjwun3xyxo/ME.single_Plant.height.txt?raw=1", header=T, row.names = 1)
ME.single$SLA <- read.delim("https://www.dropbox.com/s/22h58n2sq9eht7q/ME.single_SLA.txt?raw=1", header=T, row.names = 1)
ME.single$Germinule <- read.delim("https://www.dropbox.com/s/9mcptejbeodkuuf/ME.single_Germinule.txt?raw=1", header=T, row.names = 1)

#germinule: 141
#SLA: 70 
#plant height: 160

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

ME.single.imp <- list()
ME.single.imp$Plant.height <- ME.single$Plant.height

###assemble data
trait.imp.phy <- list()

trait.imp.phy$Plant.height <- trait.phy$Plant.height
head(trait.imp.phy$Plant.height)

trait.imp.phy$SLA <- trait.imp[, c(6:11, 2, 4)]
nrow(trait.imp.phy$SLA)
summary(trait.imp.phy$SLA$sla.avg.mm2mg.)
trait.imp.phy$SLA$sla.avg.mm2mg.log <- log10(trait.imp.phy$SLA$sla.avg.mm2mg.)

trait.imp.phy$Germinule <- trait.imp[, c(6:11, 2, 5)]
nrow(trait.imp.phy$Germinule)
summary(trait.imp.phy$Germinule$Germinule)
trait.imp.phy$Germinule$Germinule.log <- log10(trait.imp.phy$Germinule$Germinule)

#NOTE!!!!!!!!!!!!!!!!!!!!!!
#SELECTION OF EIGENVECTORS TAKES VERY LONG TIME
#INSTEAD IMPORT DATASET WITH ALREADY SELECTED EIGENVECTORS

# ###SLA
# #Calculate Moran eigenvetors to filter out phylogenetic autocorrelation
# D.sel <- dist.Ab.all[rownames(trait.imp.phy$SLA),rownames(trait.imp.phy$SLA)]
# prox <- (1/D.sel)^1 #convert distances to proximities
# diag(prox) <- 0
# prox <- prop.table(prox, 1) #normalize by row
# prox <- 0.5 * (prox + t(prox)) #make matrix symmetric
# ME <- me.phylo(prox = prox) #here Abouheif matrix without diagonal is used
# head(ME)
# 
# #create oriAbouheif proximity matrix (with diagonal) to test phylogenetic autocorrelation using Abouheif's Cmean
# D.sel <- dist.Ab.all[rownames(trait.imp.phy$SLA),rownames(trait.imp.phy$SLA)]
# prox <- (1/D.sel)^1 #convert distances to proximities
# diag(prox) <- 0
# sumMarg <- apply(prox, 1, sum) #make oriAbouheif diagonal
# diag(prox) <- (1 - sumMarg)
# 
# #Test phylogenetic autocorrelation in trait data
# abouheif.moran(x = trait.imp.phy$SLA$sla.avg.mm2mg.log, W = prox, method = "oriAbouheif", nrepet = 999)
# 
# #select eigenvectors
# ME.single.imp$SLA <- phylo.fwd(tr = trait.imp.phy$SLA$sla.avg.mm2mg.log, prox = prox,
#                                 ME=ME, method="oriAbouheif", Nperm = 999)
# head(ME.single.imp$SLA)
# 
# ###Germinule
# #Calculate Moran eigenvetors to filter out phylogenetic autocorrelation
# D.sel <- dist.Ab.all[rownames(trait.imp.phy$Germinule),rownames(trait.imp.phy$Germinule)]
# prox <- (1/D.sel)^1 #convert distances to proximities
# diag(prox) <- 0
# prox <- prop.table(prox, 1) #normalize by row
# prox <- 0.5 * (prox + t(prox)) #make matrix symmetric
# ME <- me.phylo(prox = prox) #here Abouheif matrix without diagonal is used
# head(ME)
# 
# #create oriAbouheif proximity matrix (with diagonal) to test phylogenetic autocorrelation using Abouheif's Cmean
# D.sel <- dist.Ab.all[rownames(trait.imp.phy$Germinule),rownames(trait.imp.phy$Germinule)]
# prox <- (1/D.sel)^1 #convert distances to proximities
# diag(prox) <- 0
# sumMarg <- apply(prox, 1, sum) #make oriAbouheif diagonal
# diag(prox) <- (1 - sumMarg)
# 
# #Test phylogenetic autocorrelation in trait data
# abouheif.moran(x = trait.imp.phy$Germinule$Germinule.log, W = prox, method = "oriAbouheif", nrepet = 999)
# 
# #select eigenvectors
# ME.single.imp$Germinule <- phylo.fwd(tr = trait.imp.phy$Germinule$Germinule.log, prox = prox,
#                                       ME=ME, method="oriAbouheif", Nperm = 999)
# head(ME.single.imp$Germinule)

ME.single.imp$SLA <- read.delim("https://www.dropbox.com/s/ifmjdirs3rb7y95/ME.single.imp_SLA.txt?raw=1", header=T, row.names=1)
ME.single.imp$Germinule <- read.delim("https://www.dropbox.com/s/pqelv5nnq5pi4no/ME.single.imp_Germinule.txt?raw=1", header=T, row.names=1)


###Filter out phylogenetic autocorrelation

# #Plant height
# trait.imp.phy$Plant.height$HEIGHT.MAX.cor <- resid(lm(trait.imp.phy$Plant.height$HEIGHT.MAX.log ~., data=ME.single.imp$Plant.height))
# head(trait.imp.phy$Plant.height)

#SLA
trait.imp.phy$SLA$sla.avg.mm2mg.cor <- resid(lm(trait.imp.phy$SLA$sla.avg.mm2mg.log ~., data=ME.single.imp$SLA))
head(trait.imp.phy$SLA)

#Germinule
trait.imp.phy$Germinule$Germinule.cor <- resid(lm(trait.imp.phy$Germinule$Germinule.log ~., data=ME.single.imp$Germinule))
head(trait.imp.phy$Germinule)

#divide dataset according to habitats
traitT.imp.phy <- list()
traitT.imp.phy$PlantHeight <- trait.imp.phy$Plant.height[trait.imp.phy$Plant.height$T == 1, ]
traitT.imp.phy$SLA <- trait.imp.phy$SLA[trait.imp.phy$SLA$T == 1, ]
traitT.imp.phy$Germinule <- trait.imp.phy$Germinule[trait.imp.phy$Germinule$T == 1, ]

traitX.imp.phy <- list()
traitX.imp.phy$PlantHeight <- trait.imp.phy$Plant.height[trait.imp.phy$Plant.height$X == 1, ]
traitX.imp.phy$SLA <- trait.imp.phy$SLA[trait.imp.phy$SLA$X == 1, ]
traitX.imp.phy$Germinule <- trait.imp.phy$Germinule[trait.imp.phy$Germinule$X == 1, ]

traitS.imp.phy <- list()
traitS.imp.phy$PlantHeight <- trait.imp.phy$Plant.height[trait.imp.phy$Plant.height$S == 1, ]
traitS.imp.phy$SLA <- trait.imp.phy$SLA[trait.imp.phy$SLA$S == 1, ]
traitS.imp.phy$Germinule <- trait.imp.phy$Germinule[trait.imp.phy$Germinule$S == 1, ]

traitM.imp.phy <- list()
traitM.imp.phy$PlantHeight <- trait.imp.phy$Plant.height[trait.imp.phy$Plant.height$M == 1, ]
traitM.imp.phy$SLA <- trait.imp.phy$SLA[trait.imp.phy$SLA$M == 1, ]
traitM.imp.phy$Germinule <- trait.imp.phy$Germinule[trait.imp.phy$Germinule$M == 1, ]

traitK.imp.phy <- list()
traitK.imp.phy$PlantHeight <- trait.imp.phy$Plant.height[trait.imp.phy$Plant.height$K == 1, ]
traitK.imp.phy$SLA <- trait.imp.phy$SLA[trait.imp.phy$SLA$K == 1, ]
traitK.imp.phy$Germinule <- trait.imp.phy$Germinule[trait.imp.phy$Germinule$K == 1, ]

traitL.imp.phy <- list()
traitL.imp.phy$PlantHeight <- trait.imp.phy$Plant.height[trait.imp.phy$Plant.height$L == 1, ]
traitL.imp.phy$SLA <- trait.imp.phy$SLA[trait.imp.phy$SLA$L == 1, ]
traitL.imp.phy$Germinule <- trait.imp.phy$Germinule[trait.imp.phy$Germinule$L == 1, ]


####################################################################################################
#              ACCOUNT FOR PHYLOGENETIC SIGNAL IN MULTIVARIATE TRAIT DATA                          #
####################################################################################################

#assemble data
trait.3D <- trait.s[, c(3:5, 2, 6:11)]
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
trait.3D.imp <- trait.imp.s[, c(3:5, 2, 6:11)]
head(trait.3D.imp)
nrow(trait.3D.imp)

#check complete cases
trait.3D.imp <- trait.3D.imp[complete.cases(trait.3D.imp), ]
nrow(trait.3D.imp)

# #!!!!!!!!!!!!!!!!!!!!!!
# #DO NOT RUN THE FOLLOWING CODE - SELECTION OF EIGENVECTORS TAKES VERY LONG TIME
# #INSTEAD impORT DATASET WITH ALREADY SELECTED EIGENVECTORS
# 
# #test phylogenetic correlation
# prox <- prox.Ab.all[rownames(trait.3D.imp), rownames(trait.3D.imp)] #select species in given habitat
# dim(prox)
# w <- mat2listw(prox, style="W") #normalize weights by row and create listw object
# 
# dpca <- dudi.pca(trait.3Dcor.imp[,1:3], center=FALSE, scale=FALSE, scannf=FALSE) #dudi.pca
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
# ME.3D.imp <- phylo.multifwd2(tr = trait.3D.imp[,1:3], w = w, ME = ME, nperm=999)
# dim(ME.3D.imp)
# write.table(ME.3D.imp, file="ME.3D.imp.txt", sep="\t", dec=".")

ME.3D.imp <- read.delim("ME.3D.imp.txt", header = T, row.names = 1)

###FILTER OUT PHYLOGENETIC AUTOCORRELATION IN MULTIVARIATE TRAIT DATA

trait.3Dcor.imp <- trait.3D.imp
trait.3Dcor.imp[,1:3] <- resid(lm(as.matrix(trait.3D.imp[,1:3]) ~., data=ME.3D.imp))
head(trait.3Dcor.imp)

#remove casuals
trait.3Dcor.imp <- trait.3Dcor.imp[trait.3Dcor.imp$INVASION.STATUS != "casual", ]
nrow(trait.3Dcor.imp)
