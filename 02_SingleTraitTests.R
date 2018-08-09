#############################################################################################
#        RANDOMIZATION TESTS OF THE TRAIT DIFFERENCES BETWEEN SPECIES GROUPS                #
#############################################################################################

#code by Jan Divisek (2018)

#load required packages
library(ade4)

###########################################################################
##INVASIVE VS NATIVE SPECIES COMPARISON------------------------------------

#create table for results
Diff.mediansNI.TEST <- as.data.frame(matrix(data=NA, nrow=6, ncol=9))
rownames(Diff.mediansNI.TEST) <- c("Grassland and heathland vegetation",
                                   "Ruderal and weed vegetation",
                                   "Rock and scree vegetation",
                                   "Wetland vegetation",
                                   "Scrub vegetation",
                                   "Forest vegetation")
colnames(Diff.mediansNI.TEST) <- c("SLA.obs", "SLA.P", "SLA.Padj",
                                   "PH.obs", "PH.P", "PH.Padj",
                                   "Germ.obs", "Germ.P", "Germ.Padj")

#assemble data
h <- list(traitT, traitX, traitS, traitM, traitK, traitL)

for(i in seq(1, length(h)))
{
  h1 <- h[[i]]
  sel <- h1[which(h1$INVASION.STATUS == "native" | h1$INVASION.STATUS == "invasive"), 2:5]
  
  #Plant heigh
  set.seed(1234)
  sim <- RAND.diff.of.medians(tr=sel$HEIGHT.MAX, stat=sel$INVASION.STATUS, target="invasive", Nrand=999)
  obs <- diff.of.medians(tr=sel$HEIGHT.MAX, stat=sel$INVASION.STATUS, target="invasive")
  
  Diff.mediansNI.TEST[i, "PH.obs"] <- obs
  Diff.mediansNI.TEST[i, "PH.P"] <- as.randtest(sim, obs, alter="two-sided")$pvalue
  
  #SLA
  set.seed(1234)
  sim <- RAND.diff.of.medians(tr=sel$sla.avg.mm2mg., stat=sel$INVASION.STATUS, target="invasive", Nrand=999)
  obs <- diff.of.medians(tr=sel$sla.avg.mm2mg., stat=sel$INVASION.STATUS, target="invasive")
  
  Diff.mediansNI.TEST[i, "SLA.obs"] <- obs
  Diff.mediansNI.TEST[i, "SLA.P"] <- as.randtest(sim, obs, alter="two-sided")$pvalue
  
  #Geminule weight
  set.seed(1234)
  sim <- RAND.diff.of.medians(tr=sel$Germinule, stat=sel$INVASION.STATUS, target="invasive", Nrand=999)
  obs <- diff.of.medians(tr=sel$Germinule, stat=sel$INVASION.STATUS, target="invasive")
  
  Diff.mediansNI.TEST[i, "Germ.obs"] <- obs
  Diff.mediansNI.TEST[i, "Germ.P"] <- as.randtest(sim, obs, alter="two-sided")$pvalue
}

#adjust p-values
Diff.mediansNI.TEST$PH.Padj <- p.adjust(Diff.mediansNI.TEST$PH.P, method="fdr")
Diff.mediansNI.TEST$SLA.Padj <- p.adjust(Diff.mediansNI.TEST$SLA.P, method="fdr")
Diff.mediansNI.TEST$Germ.Padj <- p.adjust(Diff.mediansNI.TEST$Germ.P, method="fdr")

round(Diff.mediansNI.TEST,3)

###########################################################################
##NATURALIZED VS NATIVE SPECIES COMPARISON------------------------------------

#create table for results
Diff.mediansNN.TEST <- as.data.frame(matrix(data=NA, nrow=6, ncol=9))
rownames(Diff.mediansNN.TEST) <- c("Grassland and heathland vegetation",
                                   "Ruderal and weed vegetation",
                                   "Rock and scree vegetation",
                                   "Wetland vegetation",
                                   "Scrub vegetation",
                                   "Forest vegetation")
colnames(Diff.mediansNN.TEST) <- c("SLA.obs", "SLA.P", "SLA.Padj",
                                   "PH.obs", "PH.P", "PH.Padj",
                                   "Germ.obs", "Germ.P", "Germ.Padj")
#assemble data
h <- list(traitT, traitX, traitS, traitM, traitK, traitL)

for(i in seq(1, length(h)))
{
  h1 <- h[[i]]
  sel <- h1[which(h1$INVASION.STATUS == "native" | h1$INVASION.STATUS == "naturalized"), 2:5]
  
  #Plant heigh
  set.seed(1234)
  sim <- RAND.diff.of.medians(tr=sel$HEIGHT.MAX, stat=sel$INVASION.STATUS, target="naturalized", Nrand=999)
  obs <- diff.of.medians(tr=sel$HEIGHT.MAX, stat=sel$INVASION.STATUS, target="naturalized")
  
  Diff.mediansNN.TEST[i, "PH.obs"] <- obs
  Diff.mediansNN.TEST[i, "PH.P"] <- as.randtest(sim, obs, alter="two-sided")$pvalue
  
  #SLA
  set.seed(1234)
  sim <- RAND.diff.of.medians(tr=sel$sla.avg.mm2mg., stat=sel$INVASION.STATUS, target="naturalized", Nrand=999)
  obs <- diff.of.medians(tr=sel$sla.avg.mm2mg., stat=sel$INVASION.STATUS, target="naturalized")
  
  Diff.mediansNN.TEST[i, "SLA.obs"] <- obs
  Diff.mediansNN.TEST[i, "SLA.P"] <- as.randtest(sim, obs, alter="two-sided")$pvalue
  
  #Geminule weight
  set.seed(1234)
  sim <- RAND.diff.of.medians(tr=sel$Germinule, stat=sel$INVASION.STATUS, target="naturalized", Nrand=999)
  obs <- diff.of.medians(tr=sel$Germinule, stat=sel$INVASION.STATUS, target="naturalized")
  
  Diff.mediansNN.TEST[i, "Germ.obs"] <- obs
  Diff.mediansNN.TEST[i, "Germ.P"] <- as.randtest(sim, obs, alter="two-sided")$pvalue
}

#adjust p-values
Diff.mediansNN.TEST$PH.Padj <- p.adjust(Diff.mediansNN.TEST$PH.P, method="fdr")
Diff.mediansNN.TEST$SLA.Padj <- p.adjust(Diff.mediansNN.TEST$SLA.P, method="fdr")
Diff.mediansNN.TEST$Germ.Padj <- p.adjust(Diff.mediansNN.TEST$Germ.P, method="fdr")

round(Diff.mediansNN.TEST,3)

###########################################################################
##INVASIVE VS NATIVE SPECIES COMPARISON IN DATASET WITH IMPUTED TRAITS-----

#create table for results
Diff.mediansNI.imp.TEST <- as.data.frame(matrix(data=NA, nrow=6, ncol=9))
rownames(Diff.mediansNI.imp.TEST) <- c("Grassland and heathland vegetation",
                                   "Ruderal and weed vegetation",
                                   "Rock and scree vegetation",
                                   "Wetland vegetation",
                                   "Scrub vegetation",
                                   "Forest vegetation")
colnames(Diff.mediansNI.imp.TEST) <- c("SLA.obs", "SLA.P", "SLA.Padj",
                                   "PH.obs", "PH.P", "PH.Padj",
                                   "Germ.obs", "Germ.P", "Germ.Padj")

#assemble data
h <- list(traitT.imp, traitX.imp, traitS.imp, traitM.imp, traitK.imp, traitL.imp)

for(i in seq(1, length(h)))
{
  h1 <- h[[i]]
  sel <- h1[which(h1$INVASION.STATUS == "native" | h1$INVASION.STATUS == "invasive"), 2:5]
  
  #Plant heigh
  set.seed(1234)
  sim <- RAND.diff.of.medians(tr=sel$HEIGHT.MAX, stat=sel$INVASION.STATUS, target="invasive", Nrand=999)
  obs <- diff.of.medians(tr=sel$HEIGHT.MAX, stat=sel$INVASION.STATUS, target="invasive")
  
  Diff.mediansNI.imp.TEST[i, "PH.obs"] <- obs
  Diff.mediansNI.imp.TEST[i, "PH.P"] <- as.randtest(sim, obs, alter="two-sided")$pvalue
  
  #SLA
  set.seed(1234)
  sim <- RAND.diff.of.medians(tr=sel$sla.avg.mm2mg., stat=sel$INVASION.STATUS, target="invasive", Nrand=999)
  obs <- diff.of.medians(tr=sel$sla.avg.mm2mg., stat=sel$INVASION.STATUS, target="invasive")
  
  Diff.mediansNI.imp.TEST[i, "SLA.obs"] <- obs
  Diff.mediansNI.imp.TEST[i, "SLA.P"] <- as.randtest(sim, obs, alter="two-sided")$pvalue
  
  #Geminule weight
  set.seed(1234)
  sim <- RAND.diff.of.medians(tr=sel$Germinule, stat=sel$INVASION.STATUS, target="invasive", Nrand=999)
  obs <- diff.of.medians(tr=sel$Germinule, stat=sel$INVASION.STATUS, target="invasive")
  
  Diff.mediansNI.imp.TEST[i, "Germ.obs"] <- obs
  Diff.mediansNI.imp.TEST[i, "Germ.P"] <- as.randtest(sim, obs, alter="two-sided")$pvalue
}

#adjust p-values
Diff.mediansNI.imp.TEST$PH.Padj <- p.adjust(Diff.mediansNI.imp.TEST$PH.P, method="fdr")
Diff.mediansNI.imp.TEST$SLA.Padj <- p.adjust(Diff.mediansNI.imp.TEST$SLA.P, method="fdr")
Diff.mediansNI.imp.TEST$Germ.Padj <- p.adjust(Diff.mediansNI.imp.TEST$Germ.P, method="fdr")

round(Diff.mediansNI.imp.TEST,3)

##############################################################################
##NATURALIZED VS NATIVE SPECIES COMPARISON IN DATASET WITH IMPUTED TRAITS-----

#create table for results
Diff.mediansNN.imp.TEST <- as.data.frame(matrix(data=NA, nrow=6, ncol=9))
rownames(Diff.mediansNN.imp.TEST) <- c("Grassland and heathland vegetation",
                                   "Ruderal and weed vegetation",
                                   "Rock and scree vegetation",
                                   "Wetland vegetation",
                                   "Scrub vegetation",
                                   "Forest vegetation")
colnames(Diff.mediansNN.imp.TEST) <- c("SLA.obs", "SLA.P", "SLA.Padj",
                                   "PH.obs", "PH.P", "PH.Padj",
                                   "Germ.obs", "Germ.P", "Germ.Padj")

#assemble data
h <- list(traitT.imp, traitX.imp, traitS.imp, traitM.imp, traitK.imp, traitL.imp)

for(i in seq(1, length(h)))
{
  h1 <- h[[i]]
  sel <- h1[which(h1$INVASION.STATUS == "native" | h1$INVASION.STATUS == "naturalized"), 2:5]
  
  #Plant heigh
  set.seed(1234)
  sim <- RAND.diff.of.medians(tr=sel$HEIGHT.MAX, stat=sel$INVASION.STATUS, target="naturalized", Nrand=999)
  obs <- diff.of.medians(tr=sel$HEIGHT.MAX, stat=sel$INVASION.STATUS, target="naturalized")
  
  Diff.mediansNN.imp.TEST[i, "PH.obs"] <- obs
  Diff.mediansNN.imp.TEST[i, "PH.P"] <- as.randtest(sim, obs, alter="two-sided")$pvalue
  
  #SLA
  set.seed(1234)
  sim <- RAND.diff.of.medians(tr=sel$sla.avg.mm2mg., stat=sel$INVASION.STATUS, target="naturalized", Nrand=999)
  obs <- diff.of.medians(tr=sel$sla.avg.mm2mg., stat=sel$INVASION.STATUS, target="naturalized")
  
  Diff.mediansNN.imp.TEST[i, "SLA.obs"] <- obs
  Diff.mediansNN.imp.TEST[i, "SLA.P"] <- as.randtest(sim, obs, alter="two-sided")$pvalue
  
  #Geminule weight
  set.seed(1234)
  sim <- RAND.diff.of.medians(tr=sel$Germinule, stat=sel$INVASION.STATUS, target="naturalized", Nrand=999)
  obs <- diff.of.medians(tr=sel$Germinule, stat=sel$INVASION.STATUS, target="naturalized")
  
  Diff.mediansNN.imp.TEST[i, "Germ.obs"] <- obs
  Diff.mediansNN.imp.TEST[i, "Germ.P"] <- as.randtest(sim, obs, alter="two-sided")$pvalue
}

#adjust p-values
Diff.mediansNN.imp.TEST$PH.Padj <- p.adjust(Diff.mediansNN.imp.TEST$PH.P, method="fdr")
Diff.mediansNN.imp.TEST$SLA.Padj <- p.adjust(Diff.mediansNN.imp.TEST$SLA.P, method="fdr")
Diff.mediansNN.imp.TEST$Germ.Padj <- p.adjust(Diff.mediansNN.imp.TEST$Germ.P, method="fdr")

round(Diff.mediansNN.imp.TEST,3)

#####################################################################################
#             SINGLE TRAIT TESTS FOR PHYLOGENETICALLY CORRECTED DATA                #
#####################################################################################

##Phylogenetic singal was filtered out using Moran's egenvectors
##Trait values are residuals from linear regression of log10(trait) and selected Moran's eigenvectors

####################################################################################
##INVASIVE VS NATIVE SPECIES COMPARISON FOR CORRECTED DATASET-----------------------

#create table for results
Diff.mediansNI.cor.TEST <- as.data.frame(matrix(data=NA, nrow=6, ncol=9))
rownames(Diff.mediansNI.cor.TEST) <- c("Grassland and heathland vegetation",
                                       "Ruderal and weed vegetation",
                                       "Rock and scree vegetation",
                                       "Wetland vegetation",
                                       "Scrub vegetation",
                                       "Forest vegetation")
colnames(Diff.mediansNI.cor.TEST) <- c("SLA.obs", "SLA.P", "SLA.Padj",
                                       "PH.obs", "PH.P", "PH.Padj",
                                       "Germ.obs", "Germ.P", "Germ.Padj")

#assemble data
h <- list(traitT.phy, traitX.phy, traitS.phy, traitM.phy, traitK.phy, traitL.phy)

for(i in seq(1, length(h)))
{
  h1 <- h[[i]]
  sel <- h1$PlantHeight[which(h1$PlantHeight$INVASION.STATUS == "native" | h1$PlantHeight$INVASION.STATUS == "invasive"),]
  
  #Plant heigh
  set.seed(1234)
  sim <- RAND.diff.of.medians(tr=sel$HEIGHT.MAX.cor, stat=sel$INVASION.STATUS, target="invasive", Nrand=999)
  obs <- diff.of.medians(tr=sel$HEIGHT.MAX.cor, stat=sel$INVASION.STATUS, target="invasive")
  
  Diff.mediansNI.cor.TEST[i, "PH.obs"] <- obs
  Diff.mediansNI.cor.TEST[i, "PH.P"] <- as.randtest(sim, obs, alter="two-sided")$pvalue
  
  #SLA
  sel <- h1$SLA[which(h1$SLA$INVASION.STATUS == "native" | h1$SLA$INVASION.STATUS == "invasive"),]
  
  set.seed(1234)
  sim <- RAND.diff.of.medians(tr=sel$sla.avg.mm2mg.cor, stat=sel$INVASION.STATUS, target="invasive", Nrand=999)
  obs <- diff.of.medians(tr=sel$sla.avg.mm2mg.cor, stat=sel$INVASION.STATUS, target="invasive")
  
  Diff.mediansNI.cor.TEST[i, "SLA.obs"] <- obs
  Diff.mediansNI.cor.TEST[i, "SLA.P"] <- as.randtest(sim, obs, alter="two-sided")$pvalue
  
  #Geminule weight
  sel <- h1$Germ[which(h1$Germinule$INVASION.STATUS == "native" | h1$Germinule$INVASION.STATUS == "invasive"),]
  
  set.seed(1234)
  sim <- RAND.diff.of.medians(tr=sel$Germinule.cor, stat=sel$INVASION.STATUS, target="invasive", Nrand=999)
  obs <- diff.of.medians(tr=sel$Germinule.cor, stat=sel$INVASION.STATUS, target="invasive")
  
  Diff.mediansNI.cor.TEST[i, "Germ.obs"] <- obs
  Diff.mediansNI.cor.TEST[i, "Germ.P"] <- as.randtest(sim, obs, alter="two-sided")$pvalue
}

#adjust p-values
Diff.mediansNI.cor.TEST$PH.Padj <- p.adjust(Diff.mediansNI.cor.TEST$PH.P, method="fdr")
Diff.mediansNI.cor.TEST$SLA.Padj <- p.adjust(Diff.mediansNI.cor.TEST$SLA.P, method="fdr")
Diff.mediansNI.cor.TEST$Germ.Padj <- p.adjust(Diff.mediansNI.cor.TEST$Germ.P, method="fdr")

round(Diff.mediansNI.cor.TEST,3)

##############################################################################
##NATURALIZED VS NATIVE SPECIES COMPARISON FOR CORRECTED DATASET-----------------

#create table for results
Diff.mediansNN.cor.TEST <- as.data.frame(matrix(data=NA, nrow=6, ncol=9))
rownames(Diff.mediansNN.cor.TEST) <- c("Grassland and heathland vegetation",
                                       "Ruderal and weed vegetation",
                                       "Rock and scree vegetation",
                                       "Wetland vegetation",
                                       "Scrub vegetation",
                                       "Forest vegetation")
colnames(Diff.mediansNN.cor.TEST) <- c("SLA.obs", "SLA.P", "SLA.Padj",
                                       "PH.obs", "PH.P", "PH.Padj",
                                       "Germ.obs", "Germ.P", "Germ.Padj")

#assemble data
h <- list(traitT.phy, traitX.phy, traitS.phy, traitM.phy, traitK.phy, traitL.phy)

for(i in seq(1, length(h)))
{
  h1 <- h[[i]]
  sel <- h1$PlantHeight[which(h1$PlantHeight$INVASION.STATUS == "native" | h1$PlantHeight$INVASION.STATUS == "naturalized"),]
  
  #Plant heigh
  set.seed(1234)
  sim <- RAND.diff.of.medians(tr=sel$HEIGHT.MAX.cor, stat=sel$INVASION.STATUS, target="naturalized", Nrand=999)
  obs <- diff.of.medians(tr=sel$HEIGHT.MAX.cor, stat=sel$INVASION.STATUS, target="naturalized")
  
  Diff.mediansNN.cor.TEST[i, "PH.obs"] <- obs
  Diff.mediansNN.cor.TEST[i, "PH.P"] <- as.randtest(sim, obs, alter="two-sided")$pvalue
  
  #SLA
  sel <- h1$SLA[which(h1$SLA$INVASION.STATUS == "native" | h1$SLA$INVASION.STATUS == "naturalized"),]
  
  set.seed(1234)
  sim <- RAND.diff.of.medians(tr=sel$sla.avg.mm2mg.cor, stat=sel$INVASION.STATUS, target="naturalized", Nrand=999)
  obs <- diff.of.medians(tr=sel$sla.avg.mm2mg.cor, stat=sel$INVASION.STATUS, target="naturalized")
  
  Diff.mediansNN.cor.TEST[i, "SLA.obs"] <- obs
  Diff.mediansNN.cor.TEST[i, "SLA.P"] <- as.randtest(sim, obs, alter="two-sided")$pvalue
  
  #Geminule weight
  sel <- h1$Germ[which(h1$Germinule$INVASION.STATUS == "native" | h1$Germinule$INVASION.STATUS == "naturalized"),]
  
  set.seed(1234)
  sim <- RAND.diff.of.medians(tr=sel$Germinule.cor, stat=sel$INVASION.STATUS, target="naturalized", Nrand=999)
  obs <- diff.of.medians(tr=sel$Germinule.cor, stat=sel$INVASION.STATUS, target="naturalized")
  
  Diff.mediansNN.cor.TEST[i, "Germ.obs"] <- obs
  Diff.mediansNN.cor.TEST[i, "Germ.P"] <- as.randtest(sim, obs, alter="two-sided")$pvalue
}

#adjust p-values
Diff.mediansNN.cor.TEST$PH.Padj <- p.adjust(Diff.mediansNN.cor.TEST$PH.P, method="fdr")
Diff.mediansNN.cor.TEST$SLA.Padj <- p.adjust(Diff.mediansNN.cor.TEST$SLA.P, method="fdr")
Diff.mediansNN.cor.TEST$Germ.Padj <- p.adjust(Diff.mediansNN.cor.TEST$Germ.P, method="fdr")

round(Diff.mediansNN.cor.TEST,3)

##############################################################################
##INVASIVE VS NATIVE SPECIES COMPARISON FOR IMPUTED AND CORRECTED DATASET-----

#create table for results
Diff.mediansNI.cor.imp.TEST <- as.data.frame(matrix(data=NA, nrow=6, ncol=9))
rownames(Diff.mediansNI.cor.imp.TEST) <- c("Grassland and heathland vegetation",
                                       "Ruderal and weed vegetation",
                                       "Rock and scree vegetation",
                                       "Wetland vegetation",
                                       "Scrub vegetation",
                                       "Forest vegetation")
colnames(Diff.mediansNI.cor.imp.TEST) <- c("SLA.obs", "SLA.P", "SLA.Padj",
                                       "PH.obs", "PH.P", "PH.Padj",
                                       "Germ.obs", "Germ.P", "Germ.Padj")

#assemble data
h <- list(traitT.imp.phy, traitX.imp.phy, traitS.imp.phy, traitM.imp.phy, traitK.imp.phy, traitL.imp.phy)

for(i in seq(1, length(h)))
{
  h1 <- h[[i]]
  sel <- h1$PlantHeight[which(h1$PlantHeight$INVASION.STATUS == "native" | h1$PlantHeight$INVASION.STATUS == "invasive"),]
  
  #Plant heigh
  set.seed(1234)
  sim <- RAND.diff.of.medians(tr=sel$HEIGHT.MAX.cor, stat=sel$INVASION.STATUS, target="invasive", Nrand=999)
  obs <- diff.of.medians(tr=sel$HEIGHT.MAX.cor, stat=sel$INVASION.STATUS, target="invasive")
  
  Diff.mediansNI.cor.imp.TEST[i, "PH.obs"] <- obs
  Diff.mediansNI.cor.imp.TEST[i, "PH.P"] <- as.randtest(sim, obs, alter="two-sided")$pvalue
  
  #SLA
  sel <- h1$SLA[which(h1$SLA$INVASION.STATUS == "native" | h1$SLA$INVASION.STATUS == "invasive"),]
  
  set.seed(1234)
  sim <- RAND.diff.of.medians(tr=sel$sla.avg.mm2mg.cor, stat=sel$INVASION.STATUS, target="invasive", Nrand=999)
  obs <- diff.of.medians(tr=sel$sla.avg.mm2mg.cor, stat=sel$INVASION.STATUS, target="invasive")
  
  Diff.mediansNI.cor.imp.TEST[i, "SLA.obs"] <- obs
  Diff.mediansNI.cor.imp.TEST[i, "SLA.P"] <- as.randtest(sim, obs, alter="two-sided")$pvalue
  
  #Geminule weight
  sel <- h1$Germ[which(h1$Germinule$INVASION.STATUS == "native" | h1$Germinule$INVASION.STATUS == "invasive"),]
  
  set.seed(1234)
  sim <- RAND.diff.of.medians(tr=sel$Germinule.cor, stat=sel$INVASION.STATUS, target="invasive", Nrand=999)
  obs <- diff.of.medians(tr=sel$Germinule.cor, stat=sel$INVASION.STATUS, target="invasive")
  
  Diff.mediansNI.cor.imp.TEST[i, "Germ.obs"] <- obs
  Diff.mediansNI.cor.imp.TEST[i, "Germ.P"] <- as.randtest(sim, obs, alter="two-sided")$pvalue
}

#adjust p-values
Diff.mediansNI.cor.imp.TEST$PH.Padj <- p.adjust(Diff.mediansNI.cor.imp.TEST$PH.P, method="fdr")
Diff.mediansNI.cor.imp.TEST$SLA.Padj <- p.adjust(Diff.mediansNI.cor.imp.TEST$SLA.P, method="fdr")
Diff.mediansNI.cor.imp.TEST$Germ.Padj <- p.adjust(Diff.mediansNI.cor.imp.TEST$Germ.P, method="fdr")

round(Diff.mediansNI.cor.imp.TEST, 3)

##############################################################################
##NATURALIZED VS NATIVE SPECIES COMPARISON FOR IMPUTED AND CORRECTED DATASET-----

#create table for results
Diff.mediansNN.cor.imp.TEST <- as.data.frame(matrix(data=NA, nrow=6, ncol=9))
rownames(Diff.mediansNN.cor.imp.TEST) <- c("Grassland and heathland vegetation",
                                            "Ruderal and weed vegetation",
                                            "Rock and scree vegetation",
                                            "Wetland vegetation",
                                            "Scrub vegetation",
                                            "Forest vegetation")
colnames(Diff.mediansNN.cor.imp.TEST) <- c("SLA.obs", "SLA.P", "SLA.Padj",
                                            "PH.obs", "PH.P", "PH.Padj",
                                            "Germ.obs", "Germ.P", "Germ.Padj")

#assemble data
h <- list(traitT.imp.phy, traitX.imp.phy, traitS.imp.phy, traitM.imp.phy, traitK.imp.phy, traitL.imp.phy)

for(i in seq(1, length(h)))
{
  h1 <- h[[i]]
  sel <- h1$PlantHeight[which(h1$PlantHeight$INVASION.STATUS == "native" | h1$PlantHeight$INVASION.STATUS == "naturalized"),]
  
  #Plant heigh
  set.seed(1234)
  sim <- RAND.diff.of.medians(tr=sel$HEIGHT.MAX.cor, stat=sel$INVASION.STATUS, target="naturalized", Nrand=999)
  obs <- diff.of.medians(tr=sel$HEIGHT.MAX.cor, stat=sel$INVASION.STATUS, target="naturalized")
  
  Diff.mediansNN.cor.imp.TEST[i, "PH.obs"] <- obs
  Diff.mediansNN.cor.imp.TEST[i, "PH.P"] <- as.randtest(sim, obs, alter="two-sided")$pvalue
  
  #SLA
  sel <- h1$SLA[which(h1$SLA$INVASION.STATUS == "native" | h1$SLA$INVASION.STATUS == "naturalized"),]
  
  set.seed(1234)
  sim <- RAND.diff.of.medians(tr=sel$sla.avg.mm2mg.cor, stat=sel$INVASION.STATUS, target="naturalized", Nrand=999)
  obs <- diff.of.medians(tr=sel$sla.avg.mm2mg.cor, stat=sel$INVASION.STATUS, target="naturalized")
  
  Diff.mediansNN.cor.imp.TEST[i, "SLA.obs"] <- obs
  Diff.mediansNN.cor.imp.TEST[i, "SLA.P"] <- as.randtest(sim, obs, alter="two-sided")$pvalue
  
  #Geminule weight
  sel <- h1$Germ[which(h1$Germinule$INVASION.STATUS == "native" | h1$Germinule$INVASION.STATUS == "naturalized"),]
  
  set.seed(1234)
  sim <- RAND.diff.of.medians(tr=sel$Germinule.cor, stat=sel$INVASION.STATUS, target="naturalized", Nrand=999)
  obs <- diff.of.medians(tr=sel$Germinule.cor, stat=sel$INVASION.STATUS, target="naturalized")
  
  Diff.mediansNN.cor.imp.TEST[i, "Germ.obs"] <- obs
  Diff.mediansNN.cor.imp.TEST[i, "Germ.P"] <- as.randtest(sim, obs, alter="two-sided")$pvalue
}

#adjust p-values
Diff.mediansNN.cor.imp.TEST$PH.Padj <- p.adjust(Diff.mediansNN.cor.imp.TEST$PH.P, method="fdr")
Diff.mediansNN.cor.imp.TEST$SLA.Padj <- p.adjust(Diff.mediansNN.cor.imp.TEST$SLA.P, method="fdr")
Diff.mediansNN.cor.imp.TEST$Germ.Padj <- p.adjust(Diff.mediansNN.cor.imp.TEST$Germ.P, method="fdr")

round(Diff.mediansNN.cor.imp.TEST,3)
