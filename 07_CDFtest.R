#################################################################################################
#           TEST DIFFERENCES BETWEEN THE CDF CURVES FOR NATIVE AND ALIEN SPECIES                #
#################################################################################################

#code by Jan Divisek (2015-2017)

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

CDF.obs <- list(yAxisT.3D.obs, yAxisX.3D.obs, yAxisS.3D.obs, yAxisM.3D.obs, yAxisK.3D.obs, yAxisL.3D.obs)
CDF.rand <- list(yAxisT.3D.rand, yAxisX.3D.rand, yAxisS.3D.rand, yAxisM.3D.rand, yAxisK.3D.rand, yAxisL.3D.rand)

for(i in 1:6)
{
  #calculate vector of simulated differences between the CDFs
  sim <- abc.loop(obs.yAxis = CDF.obs[[i]]$native, rand.yAxes = CDF.rand[[i]]$inv, xAxis = xAxis)
  #observed area between the curves
  obs <- area.between.curves.2side(CDF.obs[[i]]$native, CDF.obs[[i]]$invasive, xAxis)
  
  Diff.CDF.NI[i, "dCDF"] <- obs
  Diff.CDF.NI[i, "P"] <- as.randtest(sim=sim, obs=obs, alter = "two-sided")$ pvalue
}

#adjust p-values
Diff.CDF.NI$Padj <- p.adjust(Diff.CDF.NI$P, method = "fdr")

Diff.CDF.NI

###NATIVE VS NATURALIZED SPECIES TEST------------------------------------------------------------
Diff.CDF.NN <- as.data.frame(matrix(data=NA, nrow=6, ncol=3))
rownames(Diff.CDF.NN) <- c("Grassland and heathland vegetation",
                           "Ruderal and weed vegetation",
                           "Rock and scree vegetation",
                           "Wetland vegetation",
                           "Scrub vegetation",
                           "Forest vegetation")
colnames(Diff.CDF.NN) <- c("dCDF", "P", "Padj")

CDF.obs <- list(yAxisT.3D.obs, yAxisX.3D.obs, yAxisS.3D.obs, yAxisM.3D.obs, yAxisK.3D.obs, yAxisL.3D.obs)
CDF.rand <- list(yAxisT.3D.rand, yAxisX.3D.rand, yAxisS.3D.rand, yAxisM.3D.rand, yAxisK.3D.rand, yAxisL.3D.rand)

for(i in 1:6)
{
  #calculate vector of simulated differences between the CDFs
  sim <- abc.loop(obs.yAxis = CDF.obs[[i]]$native, rand.yAxes = CDF.rand[[i]]$natur, xAxis = xAxis)
  #observed area between the curves
  obs <- area.between.curves.2side(CDF.obs[[i]]$native, CDF.obs[[i]]$natur, xAxis)
  
  Diff.CDF.NN[i, "dCDF"] <- obs
  Diff.CDF.NN[i, "P"] <- as.randtest(sim=sim, obs=obs, alter = "two-sided")$ pvalue
}

#adjust p-values
Diff.CDF.NN$Padj <- p.adjust(Diff.CDF.NN$P, method = "fdr")

Diff.CDF.NN

###NATIVE VS INVASIVE SPECIES TEST FOR DATASET WITH IMPUTED TRAITS--------------------------
Diff.CDF.NI.full <- as.data.frame(matrix(data=NA, nrow=6, ncol=3))
rownames(Diff.CDF.NI.full) <- c("Grassland and heathland vegetation",
                           "Ruderal and weed vegetation",
                           "Rock and scree vegetation",
                           "Wetland vegetation",
                           "Scrub vegetation",
                           "Forest vegetation")
colnames(Diff.CDF.NI.full) <- c("dCDF", "P", "Padj")

CDF.obs <- list(yAxisT.3D.full.obs, yAxisX.3D.full.obs, yAxisS.3D.full.obs, yAxisM.3D.full.obs, yAxisK.3D.full.obs, yAxisL.3D.full.obs)
CDF.rand <- list(yAxisT.3D.full.rand, yAxisX.3D.full.rand, yAxisS.3D.full.rand, yAxisM.3D.full.rand, yAxisK.3D.full.rand, yAxisL.3D.full.rand)

for(i in 1:6)
{
  #calculate vector of simulated differences between the CDFs
  sim <- abc.loop(obs.yAxis = CDF.obs[[i]]$native, rand.yAxes = CDF.rand[[i]]$inv, xAxis = xAxis)
  #observed area between the curves
  obs <- area.between.curves.2side(CDF.obs[[i]]$native, CDF.obs[[i]]$invasive, xAxis)
  
  Diff.CDF.NI.full[i, "dCDF"] <- obs
  Diff.CDF.NI.full[i, "P"] <- as.randtest(sim=sim, obs=obs, alter = "two-sided")$ pvalue
}

#adjust p-values
Diff.CDF.NI.full$Padj <- p.adjust(Diff.CDF.NI.full$P, method = "fdr")

Diff.CDF.NI.full

###NATIVE VS NATURALIZED SPECIES TEST FOR DATASET WITH IMPUTED TRAITS--------------------------
Diff.CDF.NN.full <- as.data.frame(matrix(data=NA, nrow=6, ncol=3))
rownames(Diff.CDF.NN.full) <- c("Grassland and heathland vegetation",
                                "Ruderal and weed vegetation",
                                "Rock and scree vegetation",
                                "Wetland vegetation",
                                "Scrub vegetation",
                                "Forest vegetation")
colnames(Diff.CDF.NN.full) <- c("dCDF", "P", "Padj")

CDF.obs <- list(yAxisT.3D.full.obs, yAxisX.3D.full.obs, yAxisS.3D.full.obs, yAxisM.3D.full.obs, yAxisK.3D.full.obs, yAxisL.3D.full.obs)
CDF.rand <- list(yAxisT.3D.full.rand, yAxisX.3D.full.rand, yAxisS.3D.full.rand, yAxisM.3D.full.rand, yAxisK.3D.full.rand, yAxisL.3D.full.rand)

for(i in 1:6)
{
  #calculate vector of simulated differences between the CDFs
  sim <- abc.loop(obs.yAxis = CDF.obs[[i]]$native, rand.yAxes = CDF.rand[[i]]$natur, xAxis = xAxis)
  #observed area between the curves
  obs <- area.between.curves.2side(CDF.obs[[i]]$native, CDF.obs[[i]]$natur, xAxis)
  
  Diff.CDF.NN.full[i, "dCDF"] <- obs
  Diff.CDF.NN.full[i, "P"] <- as.randtest(sim=sim, obs=obs, alter = "two-sided")$ pvalue
}

#adjust p-values
Diff.CDF.NN.full$Padj <- p.adjust(Diff.CDF.NN.full$P, method = "fdr")

Diff.CDF.NN.full

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

CDF.obs <- list(yAxisT.3Dcor.obs, yAxisX.3Dcor.obs, yAxisS.3Dcor.obs, yAxisM.3Dcor.obs, yAxisK.3Dcor.obs, yAxisL.3Dcor.obs)
CDF.rand <- list(yAxisT.3Dcor.rand, yAxisX.3Dcor.rand, yAxisS.3Dcor.rand, yAxisM.3Dcor.rand, yAxisK.3Dcor.rand, yAxisL.3Dcor.rand)

for(i in 1:6)
{
  #calculate vector of simulated differences between the CDFs
  sim <- abc.loop(obs.yAxis = CDF.obs[[i]]$native, rand.yAxes = CDF.rand[[i]]$inv, xAxis = xAxis)
  #observed area between the curves
  obs <- area.between.curves.2side(CDF.obs[[i]]$native, CDF.obs[[i]]$invasive, xAxis)
  
  Diff.CDFcor.NI[i, "dCDF"] <- obs
  Diff.CDFcor.NI[i, "P"] <- as.randtest(sim=sim, obs=obs, alter = "two-sided")$ pvalue
}

#adjust p-values
Diff.CDFcor.NI$Padj <- p.adjust(Diff.CDFcor.NI$P, method = "fdr")

Diff.CDFcor.NI

###NATIVE VS NATURALIZED SPECIES TEST------------------------------------------------------------
Diff.CDFcor.NN <- as.data.frame(matrix(data=NA, nrow=6, ncol=3))
rownames(Diff.CDFcor.NN) <- c("Grassland and heathland vegetation",
                              "Ruderal and weed vegetation",
                              "Rock and scree vegetation",
                              "Wetland vegetation",
                              "Scrub vegetation",
                              "Forest vegetation")
colnames(Diff.CDFcor.NN) <- c("dCDF", "P", "Padj")

CDF.obs <- list(yAxisT.3Dcor.obs, yAxisX.3Dcor.obs, yAxisS.3Dcor.obs, yAxisM.3Dcor.obs, yAxisK.3Dcor.obs, yAxisL.3Dcor.obs)
CDF.rand <- list(yAxisT.3Dcor.rand, yAxisX.3Dcor.rand, yAxisS.3Dcor.rand, yAxisM.3Dcor.rand, yAxisK.3Dcor.rand, yAxisL.3Dcor.rand)

for(i in 1:6)
{
  #calculate vector of simulated differences between the CDFs
  sim <- abc.loop(obs.yAxis = CDF.obs[[i]]$native, rand.yAxes = CDF.rand[[i]]$natur, xAxis = xAxis)
  #observed area between the curves
  obs <- area.between.curves.2side(CDF.obs[[i]]$native, CDF.obs[[i]]$natur, xAxis)
  
  Diff.CDFcor.NN[i, "dCDF"] <- obs
  Diff.CDFcor.NN[i, "P"] <- as.randtest(sim=sim, obs=obs, alter = "two-sided")$ pvalue
}

#adjust p-values
Diff.CDFcor.NN$Padj <- p.adjust(Diff.CDFcor.NN$P, method = "fdr")

Diff.CDFcor.NN

###NATIVE VS INVASIVE SPECIES TEST FOR DATASET WITH IMPUTED TRAITS--------------------------
Diff.CDFcor.NI.full <- as.data.frame(matrix(data=NA, nrow=6, ncol=3))
rownames(Diff.CDFcor.NI.full) <- c("Grassland and heathland vegetation",
                                "Ruderal and weed vegetation",
                                "Rock and scree vegetation",
                                "Wetland vegetation",
                                "Scrub vegetation",
                                "Forest vegetation")
colnames(Diff.CDFcor.NI.full) <- c("dCDF", "P", "Padj")

CDF.obs <- list(yAxisT.3Dcor.full.obs, yAxisX.3Dcor.full.obs, yAxisS.3Dcor.full.obs, yAxisM.3Dcor.full.obs, yAxisK.3Dcor.full.obs, yAxisL.3Dcor.full.obs)
CDF.rand <- list(yAxisT.3Dcor.full.rand, yAxisX.3Dcor.full.rand, yAxisS.3Dcor.full.rand, yAxisM.3Dcor.full.rand, yAxisK.3Dcor.full.rand, yAxisL.3Dcor.full.rand)

for(i in 1:6)
{
  #calculate vector of simulated differences between the CDFs
  sim <- abc.loop(obs.yAxis = CDF.obs[[i]]$native, rand.yAxes = CDF.rand[[i]]$inv, xAxis = xAxis)
  #observed area between the curves
  obs <- area.between.curves.2side(CDF.obs[[i]]$native, CDF.obs[[i]]$invasive, xAxis)
  
  Diff.CDFcor.NI.full[i, "dCDF"] <- obs
  Diff.CDFcor.NI.full[i, "P"] <- as.randtest(sim=sim, obs=obs, alter = "two-sided")$ pvalue
}

#adjust p-values
Diff.CDFcor.NI.full$Padj <- p.adjust(Diff.CDFcor.NI.full$P, method = "fdr")

Diff.CDFcor.NI.full

###NATIVE VS NATURALIZED SPECIES TEST FOR DATASET WITH IMPUTED TRAITS--------------------------
Diff.CDFcor.NN.full <- as.data.frame(matrix(data=NA, nrow=6, ncol=3))
rownames(Diff.CDFcor.NN.full) <- c("Grassland and heathland vegetation",
                                   "Ruderal and weed vegetation",
                                   "Rock and scree vegetation",
                                   "Wetland vegetation",
                                   "Scrub vegetation",
                                   "Forest vegetation")
colnames(Diff.CDFcor.NN.full) <- c("dCDF", "P", "Padj")

CDF.obs <- list(yAxisT.3Dcor.full.obs, yAxisX.3Dcor.full.obs, yAxisS.3Dcor.full.obs, yAxisM.3Dcor.full.obs, yAxisK.3Dcor.full.obs, yAxisL.3Dcor.full.obs)
CDF.rand <- list(yAxisT.3Dcor.full.rand, yAxisX.3Dcor.full.rand, yAxisS.3Dcor.full.rand, yAxisM.3Dcor.full.rand, yAxisK.3Dcor.full.rand, yAxisL.3Dcor.full.rand)

for(i in 1:6)
{
  #calculate vector of simulated differences between the CDFs
  sim <- abc.loop(obs.yAxis = CDF.obs[[i]]$native, rand.yAxes = CDF.rand[[i]]$natur, xAxis = xAxis)
  #observed area between the curves
  obs <- area.between.curves.2side(CDF.obs[[i]]$native, CDF.obs[[i]]$natur, xAxis)
  
  Diff.CDFcor.NN.full[i, "dCDF"] <- obs
  Diff.CDFcor.NN.full[i, "P"] <- as.randtest(sim=sim, obs=obs, alter = "two-sided")$ pvalue
}

#adjust p-values
Diff.CDFcor.NN.full$Padj <- p.adjust(Diff.CDFcor.NN.full$P, method = "fdr")

Diff.CDFcor.NN.full
