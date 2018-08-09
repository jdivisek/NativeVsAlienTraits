#################################################################################################
#           TEST DIFFERENCES BETWEEN THE CDF CURVES FOR NATIVE AND ALIEN SPECIES                #
#################################################################################################

#code by Jan Divisek (2018)

##CDFs are based on species distnaces from the centroid of native species in 3D trait space
##Here are significance tests of the difference between the CDFs

library(ade4)

###NATIVE VS INVASIVE SPECIES TEST------------------------------------------------------------
Diff.CDF.NI <- as.data.frame(matrix(data=NA, nrow=6, ncol=3))
rownames(Diff.CDF.NI) <- c("Grassland and heathland vegetation",
                           "Ruderal and weed vegetation",
                           "Rock and scree vegetation",
                           "Wetland vegetation",
                           "Scrub vegetation",
                           "Forest vegetation")
colnames(Diff.CDF.NI) <- c("dCDF", "P", "Padj")

CDFx <- xAxis
CDFy.obs <- list(yAxis$T$obs, yAxis$X$obs, yAxis$S$obs, yAxis$M$obs, yAxis$K$obs, yAxis$L$obs)
CDFy.rand <- list(yAxis$T$rand, yAxis$X$rand, yAxis$S$rand, yAxis$M$rand, yAxis$K$rand, yAxis$L$rand)

stat <- list()

for(i in 1:6)
{
  #calculate vector of simulated differences between the CDFs
  sim <- abc.loop(obs.yAxis = CDFy.obs[[i]]$native, rand.yAxes = CDFy.rand[[i]]$inv, xAxis=CDFx[[i]]/max(CDFx[[i]]))
  #observed area between the curves
  obs <- area.between.curves.2side(CDFy.obs[[i]]$native, CDFy.obs[[i]]$invasive, xAxis=CDFx[[i]]/max(CDFx[[i]]))[[1]]
  
  stat[[i]] <- area.between.curves.2side(CDFy.obs[[i]]$native, CDFy.obs[[i]]$invasive, xAxis=CDFx[[i]]/max(CDFx[[i]]))[[2]]
  
  Diff.CDF.NI[i, "dCDF"] <- obs
  Diff.CDF.NI[i, "P"] <- as.randtest(sim=sim, obs=obs, alter = "two-sided")$ pvalue
}

#adjust p-values
Diff.CDF.NI$Padj <- p.adjust(Diff.CDF.NI$P, method = "fdr")

round(Diff.CDF.NI, 3)

unlist(lapply(stat, FUN = function(x){return(max(x))}))
unlist(lapply(stat, FUN = function(x){return(min(x))}))

###NATIVE VS NATURALIZED SPECIES TEST------------------------------------------------------------
Diff.CDF.NN <- as.data.frame(matrix(data=NA, nrow=6, ncol=3))
rownames(Diff.CDF.NN) <- c("Grassland and heathland vegetation",
                           "Ruderal and weed vegetation",
                           "Rock and scree vegetation",
                           "Wetland vegetation",
                           "Scrub vegetation",
                           "Forest vegetation")
colnames(Diff.CDF.NN) <- c("dCDF", "P", "Padj")

CDFy.obs <- list(yAxis$T$obs, yAxis$X$obs, yAxis$S$obs, yAxis$M$obs, yAxis$K$obs, yAxis$L$obs)
CDFy.rand <- list(yAxis$T$rand, yAxis$X$rand, yAxis$S$rand, yAxis$M$rand, yAxis$K$rand, yAxis$L$rand)

stat <- list()

for(i in 1:6)
{
  #calculate vector of simulated differences between the CDFs
  sim <- abc.loop(obs.yAxis = CDFy.obs[[i]]$native, rand.yAxes = CDFy.rand[[i]]$natur, xAxis=CDFx[[i]]/max(CDFx[[i]]))
  #observed area between the curves
  obs <- area.between.curves.2side(CDFy.obs[[i]]$native, CDFy.obs[[i]]$natur, xAxis=CDFx[[i]]/max(CDFx[[i]]))[[1]]
  
  stat[[i]] <- area.between.curves.2side(CDFy.obs[[i]]$native, CDFy.obs[[i]]$natur, xAxis=CDFx[[i]]/max(CDFx[[i]]))[[2]]
  
  Diff.CDF.NN[i, "dCDF"] <- obs
  Diff.CDF.NN[i, "P"] <- as.randtest(sim=sim, obs=obs, alter = "two-sided")$ pvalue
}

#adjust p-values
Diff.CDF.NN$Padj <- p.adjust(Diff.CDF.NN$P, method = "fdr")

round(Diff.CDF.NN, 3)

unlist(lapply(stat, FUN = function(x){return(max(x))}))
unlist(lapply(stat, FUN = function(x){return(min(x))}))

###NATIVE VS INVASIVE SPECIES TEST FOR DATASET WITH IMPUTED TRAITS--------------------------
Diff.CDF.imp.NI <- as.data.frame(matrix(data=NA, nrow=6, ncol=3))
rownames(Diff.CDF.imp.NI) <- c("Grassland and heathland vegetation",
                           "Ruderal and weed vegetation",
                           "Rock and scree vegetation",
                           "Wetland vegetation",
                           "Scrub vegetation",
                           "Forest vegetation")
colnames(Diff.CDF.imp.NI) <- c("dCDF", "P", "Padj")

CDFx <- xAxis.imp
CDFy.obs <- list(yAxis.imp$T$obs, yAxis.imp$X$obs, yAxis.imp$S$obs, yAxis.imp$M$obs, yAxis.imp$K$obs, yAxis.imp$L$obs)
CDFy.rand <- list(yAxis.imp$T$rand, yAxis.imp$X$rand, yAxis.imp$S$rand, yAxis.imp$M$rand, yAxis.imp$K$rand, yAxis.imp$L$rand)

for(i in 1:6)
{
  #calculate vector of simulated differences between the CDFs
  sim <- abc.loop(obs.yAxis = CDFy.obs[[i]]$native, rand.yAxes = CDFy.rand[[i]]$inv, xAxis=CDFx[[i]]/max(CDFx[[i]]))
  #observed area between the curves
  obs <- area.between.curves.2side(CDFy.obs[[i]]$native, CDFy.obs[[i]]$invasive, xAxis=CDFx[[i]]/max(CDFx[[i]]))[[1]]
  
  Diff.CDF.imp.NI[i, "dCDF"] <- obs
  Diff.CDF.imp.NI[i, "P"] <- as.randtest(sim=sim, obs=obs, alter = "two-sided")$ pvalue
}

#adjust p-values
Diff.CDF.imp.NI$Padj <- p.adjust(Diff.CDF.imp.NI$P, method = "fdr")

round(Diff.CDF.imp.NI, 3)

###NATIVE VS NATURALIZED SPECIES TEST FOR DATASET WITH impUTED TRAITS--------------------------
Diff.CDF.imp.NN <- as.data.frame(matrix(data=NA, nrow=6, ncol=3))
rownames(Diff.CDF.imp.NN) <- c("Grassland and heathland vegetation",
                           "Ruderal and weed vegetation",
                           "Rock and scree vegetation",
                           "Wetland vegetation",
                           "Scrub vegetation",
                           "Forest vegetation")
colnames(Diff.CDF.imp.NN) <- c("dCDF", "P", "Padj")

CDFy.obs <- list(yAxis.imp$T$obs, yAxis.imp$X$obs, yAxis.imp$S$obs, yAxis.imp$M$obs, yAxis.imp$K$obs, yAxis.imp$L$obs)
CDFy.rand <- list(yAxis.imp$T$rand, yAxis.imp$X$rand, yAxis.imp$S$rand, yAxis.imp$M$rand, yAxis.imp$K$rand, yAxis.imp$L$rand)

for(i in 1:6)
{
  #calculate vector of simulated differences between the CDFs
  sim <- abc.loop(obs.yAxis = CDFy.obs[[i]]$native, rand.yAxes = CDFy.rand[[i]]$natur, xAxis=CDFx[[i]]/max(CDFx[[i]]))
  #observed area between the curves
  obs <- area.between.curves.2side(CDFy.obs[[i]]$native, CDFy.obs[[i]]$natur, xAxis=CDFx[[i]]/max(CDFx[[i]]))[[1]]
  
  Diff.CDF.imp.NN[i, "dCDF"] <- obs
  Diff.CDF.imp.NN[i, "P"] <- as.randtest(sim=sim, obs=obs, alter = "two-sided")$ pvalue
}

#adjust p-values
Diff.CDF.imp.NN$Padj <- p.adjust(Diff.CDF.imp.NN$P, method = "fdr")

round(Diff.CDF.imp.NN, 3)

##########################################################################################
#                     CDF TEST FOR PHYLOGENETICALLY CORRECTED DATA                       #
##########################################################################################

##Phylogenetic signal was filtered out using Moran's egenvectors
##Trait values are residuals from linear regression of log10(trait) and selected Moran's eigenvectors

###NATIVE VS INVASIVE SPECIES TEST------------------------------------------------------------
Diff.CDFcor.NI <- as.data.frame(matrix(data=NA, nrow=6, ncol=3))
rownames(Diff.CDFcor.NI) <- c("Grassland and heathland vegetation",
                           "Ruderal and weed vegetation",
                           "Rock and scree vegetation",
                           "Wetland vegetation",
                           "Scrub vegetation",
                           "Forest vegetation")
colnames(Diff.CDFcor.NI) <- c("dCDF", "P", "Padj")

CDFx <- xAxis.cor
CDFy.obs <- list(yAxis.cor$T$obs, yAxis.cor$X$obs, yAxis.cor$S$obs, yAxis.cor$M$obs, yAxis.cor$K$obs, yAxis.cor$L$obs)
CDFy.rand <- list(yAxis.cor$T$rand, yAxis.cor$X$rand, yAxis.cor$S$rand, yAxis.cor$M$rand, yAxis.cor$K$rand, yAxis.cor$L$rand)

for(i in 1:6)
{
  #calculate vector of simulated differences between the CDFs
  sim <- abc.loop(obs.yAxis = CDFy.obs[[i]]$native, rand.yAxes = CDFy.rand[[i]]$inv, xAxis=CDFx[[i]]/max(CDFx[[i]]))
  #observed area between the curves
  obs <- area.between.curves.2side(CDFy.obs[[i]]$native, CDFy.obs[[i]]$invasive, xAxis=CDFx[[i]]/max(CDFx[[i]]))[[1]]
  
  Diff.CDFcor.NI[i, "dCDF"] <- obs
  Diff.CDFcor.NI[i, "P"] <- as.randtest(sim=sim, obs=obs, alter = "two-sided")$ pvalue
}

#adjust p-values
Diff.CDFcor.NI$Padj <- p.adjust(Diff.CDFcor.NI$P, method = "fdr")

round(Diff.CDFcor.NI, 3)

###NATIVE VS NATURALIZED SPECIES TEST------------------------------------------------------------
Diff.CDFcor.NN <- as.data.frame(matrix(data=NA, nrow=6, ncol=3))
rownames(Diff.CDFcor.NN) <- c("Grassland and heathland vegetation",
                           "Ruderal and weed vegetation",
                           "Rock and scree vegetation",
                           "Wetland vegetation",
                           "Scrub vegetation",
                           "Forest vegetation")
colnames(Diff.CDFcor.NN) <- c("dCDF", "P", "Padj")

CDFy.obs <- list(yAxis.cor$T$obs, yAxis.cor$X$obs, yAxis.cor$S$obs, yAxis.cor$M$obs, yAxis.cor$K$obs, yAxis.cor$L$obs)
CDFy.rand <- list(yAxis.cor$T$rand, yAxis.cor$X$rand, yAxis.cor$S$rand, yAxis.cor$M$rand, yAxis.cor$K$rand, yAxis.cor$L$rand)

for(i in 1:6)
{
  #calculate vector of simulated differences between the CDFs
  sim <- abc.loop(obs.yAxis = CDFy.obs[[i]]$native, rand.yAxes = CDFy.rand[[i]]$natur, xAxis=CDFx[[i]]/max(CDFx[[i]]))
  #observed area between the curves
  obs <- area.between.curves.2side(CDFy.obs[[i]]$native, CDFy.obs[[i]]$natur, xAxis=CDFx[[i]]/max(CDFx[[i]]))[[1]]
  
  Diff.CDFcor.NN[i, "dCDF"] <- obs
  Diff.CDFcor.NN[i, "P"] <- as.randtest(sim=sim, obs=obs, alter = "two-sided")$ pvalue
}

#adjust p-values
Diff.CDFcor.NN$Padj <- p.adjust(Diff.CDFcor.NN$P, method = "fdr")

round(Diff.CDFcor.NN, 3)

###NATIVE VS INVASIVE SPECIES TEST FOR DATASET WITH IMPUTED TRAITS--------------------------

Diff.CDFcor.imp.NI <- as.data.frame(matrix(data=NA, nrow=6, ncol=3))
rownames(Diff.CDFcor.imp.NI) <- c("Grassland and heathland vegetation",
                              "Ruderal and weed vegetation",
                              "Rock and scree vegetation",
                              "Wetland vegetation",
                              "Scrub vegetation",
                              "Forest vegetation")
colnames(Diff.CDFcor.imp.NI) <- c("dCDF", "P", "Padj")

CDFx <- xAxis.cor.imp
CDFy.obs <- list(yAxis.cor.imp$T$obs, yAxis.cor.imp$X$obs, yAxis.cor.imp$S$obs, yAxis.cor.imp$M$obs, yAxis.cor.imp$K$obs, yAxis.cor.imp$L$obs)
CDFy.rand <- list(yAxis.cor.imp$T$rand, yAxis.cor.imp$X$rand, yAxis.cor.imp$S$rand, yAxis.cor.imp$M$rand, yAxis.cor.imp$K$rand, yAxis.cor.imp$L$rand)

for(i in 1:6)
{
  #calculate vector of simulated differences between the CDFs
  sim <- abc.loop(obs.yAxis = CDFy.obs[[i]]$native, rand.yAxes = CDFy.rand[[i]]$inv, xAxis=CDFx[[i]]/max(CDFx[[i]]))
  #observed area between the curves
  obs <- area.between.curves.2side(CDFy.obs[[i]]$native, CDFy.obs[[i]]$invasive, xAxis=CDFx[[i]]/max(CDFx[[i]]))[[1]]
  
  Diff.CDFcor.imp.NI[i, "dCDF"] <- obs
  Diff.CDFcor.imp.NI[i, "P"] <- as.randtest(sim=sim, obs=obs, alter = "two-sided")$ pvalue
}

#adjust p-values
Diff.CDFcor.imp.NI$Padj <- p.adjust(Diff.CDFcor.imp.NI$P, method = "fdr")

round(Diff.CDFcor.imp.NI, 3)

###NATIVE VS NATURALIZED SPECIES TEST FOR DATASET WITH IMPUTED TRAITS--------------------------
Diff.CDFcor.imp.NN <- as.data.frame(matrix(data=NA, nrow=6, ncol=3))
rownames(Diff.CDFcor.imp.NN) <- c("Grassland and heathland vegetation",
                              "Ruderal and weed vegetation",
                              "Rock and scree vegetation",
                              "Wetland vegetation",
                              "Scrub vegetation",
                              "Forest vegetation")
colnames(Diff.CDFcor.imp.NN) <- c("dCDF", "P", "Padj")

CDFy.obs <- list(yAxis.cor.imp$T$obs, yAxis.cor.imp$X$obs, yAxis.cor.imp$S$obs, yAxis.cor.imp$M$obs, yAxis.cor.imp$K$obs, yAxis.cor.imp$L$obs)
CDFy.rand <- list(yAxis.cor.imp$T$rand, yAxis.cor.imp$X$rand, yAxis.cor.imp$S$rand, yAxis.cor.imp$M$rand, yAxis.cor.imp$K$rand, yAxis.cor.imp$L$rand)

for(i in 1:6)
{
  #calculate vector of simulated differences between the CDFs
  sim <- abc.loop(obs.yAxis = CDFy.obs[[i]]$native, rand.yAxes = CDFy.rand[[i]]$natur, xAxis=CDFx[[i]]/max(CDFx[[i]]))
  #observed area between the curves
  obs <- area.between.curves.2side(CDFy.obs[[i]]$native, CDFy.obs[[i]]$natur, xAxis=CDFx[[i]]/max(CDFx[[i]]))[[1]]
  
  Diff.CDFcor.imp.NN[i, "dCDF"] <- obs
  Diff.CDFcor.imp.NN[i, "P"] <- as.randtest(sim=sim, obs=obs, alter = "two-sided")$ pvalue
}

#adjust p-values
Diff.CDFcor.imp.NN$Padj <- p.adjust(Diff.CDFcor.imp.NN$P, method = "fdr")

round(Diff.CDFcor.imp.NN, 3)
