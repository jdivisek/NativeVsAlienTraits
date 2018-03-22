###########################################################################################
#                               PLOT CDFs FOR ALL HABITATS                                #
###########################################################################################

#code by Jan Divisek (2015-2017)

#cairo_pdf("CDF1.tif", width=5.3, height = 7)
tiff("CDF1.tif", width = 5.3, height = 7, units = "in", res=500, compression = "lzw")
layout(matrix(1:6,ncol=2, byrow=T))
par(mar=c(0.5, 0.5, 1.5, 0.2), oma=c(3, 3.5, 0.5, 0.5), mgp=c(3,0.6,0), las=1, tck=-0.027)

CDF.obs <- list(yAxisT.3D.obs, yAxisX.3D.obs, yAxisS.3D.obs, yAxisM.3D.obs, yAxisK.3D.obs, yAxisL.3D.obs)
CDF.rand <- list(yAxisT.3D.rand, yAxisX.3D.rand, yAxisS.3D.rand, yAxisM.3D.rand, yAxisK.3D.rand, yAxisL.3D.rand)

No.sp <- list(table(traitT.3D$INVASION.STATUS), 
              table(traitX.3D$INVASION.STATUS), 
              table(traitS.3D$INVASION.STATUS), 
              table(traitM.3D$INVASION.STATUS), 
              table(traitK.3D$INVASION.STATUS), 
              table(traitL.3D$INVASION.STATUS))

habitat <- c("Grassland and heathland vegetation",
              "Ruderal and weed vegetation",
              "Rock and scree vegetation",
              "Wetland vegetation",
              "Scrub vegetation",
              "Forest vegetation")

for(i in 1:6)
{
  plot(xAxis, CDF.obs[[i]]$native*100, type='l', axes=F, xlab="", ylab="",  bty="n", col=NA, las=1, cex.axis=0.9)
  title(habitat[i], cex.main=1.1, line=0.5) 
  box(lwd=0.75)
  
  if(any(c(1,3,5) == i))  {axis(2, lwd=0.75)}
  if(any(5:6 == i))  {axis(1, lwd=0.75)}
  
  #calculate extent of randomized CDFs for invasive species
  rand.plot <- matrix(data=NA, ncol=1000, nrow=999)
  for(q in seq(1,999))
  {
    rand.plot[q,] <- CDF.rand[[i]]$inv[[q]]*100
  }
  rand.max <- apply(rand.plot, 2, FUN=quantile, probs = 0.975)
  rand.min <- apply(rand.plot, 2, FUN=quantile, probs = 0.025)
  
  #plot 95% interval of randomized CDFs for invasive species
  polygon(x=c(xAxis, rev(xAxis)), y=c(rand.min, rev(rand.max)), col="gray93", border = NA)
  
  #calculate extent of randomized CDFs for naturalized species
  rand.plot <- matrix(data=NA, ncol=1000, nrow=999)
  for(q in seq(1,999))
  {
    rand.plot[q,] <- CDF.rand[[i]]$natur[[q]]*100
  }
  rand.max <- apply(rand.plot, 2, FUN=quantile, probs = 0.975)
  rand.min <- apply(rand.plot, 2, FUN=quantile, probs = 0.025)
  
  #plot 95% interval of randomized CDFs for invasive species
  polygon(x=c(xAxis, rev(xAxis)), y=c(rand.min, rev(rand.max)), col="gray75", border = NA)
  
  #plot observed CDFs
  lines(xAxis, CDF.obs[[i]]$native*100, type='l', lwd=1, col="deepskyblue3")
  lines(xAxis, CDF.obs[[i]]$natur*100, type='l', lwd=1, col="yellow1")
  lines(xAxis, CDF.obs[[i]]$invasive*100, type='l', lwd=1, col="red2")
  
  #add statistics for native species
  text(0.50, 62, labels = "Native species", pos=4, cex=0.9, col="deepskyblue3")
  text(0.55,54, labels = paste("N = ", No.sp[[i]][1], sep=""), pos=4, cex=0.9, col="deepskyblue3")
  
  #add statistics for naturalized species
  obs <- round(Diff.CDF.NN[i, "dCDF"], 3)
  p <- sig.code(Diff.CDF.NN[i, "Padj"])
  
  polygon(c(0.50, 1, 1, 0.50), c(25, 25, 50, 50), border = F, col="gray75")
  text(0.50, 46, labels = "Naturalized species", pos=4, cex=0.9, col="yellow1")
  text(0.55,38, labels = paste("N = ", No.sp[[i]][3], sep=""), pos=4, cex=0.9, col="yellow1")
  text(0.55,30, labels = paste("ΔCDF = ", obs, sep=""), pos=4, cex=0.9, col="yellow1")
  text(0.855,30, labels = p, pos=4, cex=1.5, col="yellow1")
  
  #add statistics for invasive species
  obs <- round(Diff.CDF.NI[i, "dCDF"], 3)
  p <- sig.code(Diff.CDF.NI[i, "Padj"])
  
  polygon(c(0.50, 1, 1, 0.50), c(2, 2, 26, 26), border = F, col="gray93")
  text(0.50, 22, labels = "Invasive species", pos=4, cex=0.9, col="red2")
  text(0.55,14, labels = paste("N = ", No.sp[[i]][4], sep=""), pos=4, cex=0.9, col="red2")
  text(0.55,6, labels = paste("ΔCDF = ", obs, sep=""), pos=4, cex=0.9, col="red2")
  text(0.87,6, labels = p, pos=4, cex=1.5, col="red2")
}

mtext("Distance from the centroid of native species in 3D trait space", 1, 1.5, outer=T, cex=0.7)
mtext("Proportion of species (%)", 2, 2, outer=T, las=0, cex=0.7)

dev.off()

##############################################################################################
###PLOT CDFs FOR DATASET WITH IMPUTED TRAITS--------------------------------------------------

#cairo_pdf("CDF2.tif", width=5.3, height = 7)
tiff("CDF2.tif", width = 5.3, height = 7, units = "in", res=500, compression = "lzw")
layout(matrix(1:6,ncol=2, byrow=T))
par(mar=c(0.5, 0.5, 1.5, 0.2), oma=c(3, 3.5, 0.5, 0.5), mgp=c(3,0.6,0), las=1, tck=-0.027)

CDF.obs <- list(yAxisT.3D.full.obs, yAxisX.3D.full.obs, yAxisS.3D.full.obs, yAxisM.3D.full.obs, yAxisK.3D.full.obs, yAxisL.3D.full.obs)
CDF.rand <- list(yAxisT.3D.full.rand, yAxisX.3D.full.rand, yAxisS.3D.full.rand, yAxisM.3D.full.rand, yAxisK.3D.full.rand, yAxisL.3D.full.rand)

No.sp <- list(table(traitT.3D.full$INVASION.STATUS), 
              table(traitX.3D.full$INVASION.STATUS), 
              table(traitS.3D.full$INVASION.STATUS), 
              table(traitM.3D.full$INVASION.STATUS), 
              table(traitK.3D.full$INVASION.STATUS), 
              table(traitL.3D.full$INVASION.STATUS))

habitat <- c("Grassland and heathland vegetation",
             "Ruderal and weed vegetation",
             "Rock and scree vegetation",
             "Wetland vegetation",
             "Scrub vegetation",
             "Forest vegetation")

for(i in 1:6)
{
  plot(xAxis, CDF.obs[[i]]$native*100, type='l', axes=F, xlab="", ylab="",  bty="n", col=NA, las=1, cex.axis=0.9)
  title(habitat[i], cex.main=1.1, line=0.5) 
  box(lwd=0.75)
  
  if(any(c(1,3,5) == i))  {axis(2, lwd=0.75)}
  if(any(5:6 == i))  {axis(1, lwd=0.75)}
  
  #calculate extent of randomized CDFs for invasive species
  rand.plot <- matrix(data=NA, ncol=1000, nrow=999)
  for(q in seq(1,999))
  {
    rand.plot[q,] <- CDF.rand[[i]]$inv[[q]]*100
  }
  rand.max <- apply(rand.plot, 2, FUN=quantile, probs = 0.975)
  rand.min <- apply(rand.plot, 2, FUN=quantile, probs = 0.025)
  
  #plot 95% interval of randomized CDFs for invasive species
  polygon(x=c(xAxis, rev(xAxis)), y=c(rand.min, rev(rand.max)), col="gray93", border = NA)
  
  #calculate extent of randomized CDFs for naturalized species
  rand.plot <- matrix(data=NA, ncol=1000, nrow=999)
  for(q in seq(1,999))
  {
    rand.plot[q,] <- CDF.rand[[i]]$natur[[q]]*100
  }
  rand.max <- apply(rand.plot, 2, FUN=quantile, probs = 0.975)
  rand.min <- apply(rand.plot, 2, FUN=quantile, probs = 0.025)
  
  #plot 95% interval of randomized CDFs for invasive species
  polygon(x=c(xAxis, rev(xAxis)), y=c(rand.min, rev(rand.max)), col="gray75", border = NA)
  
  #plot observed CDFs
  lines(xAxis, CDF.obs[[i]]$native*100, type='l', lwd=1, col="deepskyblue3")
  lines(xAxis, CDF.obs[[i]]$natur*100, type='l', lwd=1, col="yellow1")
  lines(xAxis, CDF.obs[[i]]$invasive*100, type='l', lwd=1, col="red2")
  
  #add statistics for native species
  text(0.50, 62, labels = "Native species", pos=4, cex=0.9, col="deepskyblue3")
  text(0.55,54, labels = paste("N = ", No.sp[[i]][1], sep=""), pos=4, cex=0.9, col="deepskyblue3")
  
  #add statistics for naturalized species
  obs <- round(Diff.CDF.NN.full[i, "dCDF"], 3)
  p <- sig.code(Diff.CDF.NN.full[i, "Padj"])
  
  polygon(c(0.50, 1, 1, 0.50), c(25, 25, 50, 50), border = F, col="gray75")
  text(0.50, 46, labels = "Naturalized species", pos=4, cex=0.9, col="yellow1")
  text(0.55,38, labels = paste("N = ", No.sp[[i]][3], sep=""), pos=4, cex=0.9, col="yellow1")
  text(0.55,30, labels = paste("ΔCDF = ", obs, sep=""), pos=4, cex=0.9, col="yellow1")
  text(0.855,30, labels = p, pos=4, cex=1.5, col="yellow1")
  
  #add statistics for invasive species
  obs <- round(Diff.CDF.NI.full[i, "dCDF"], 3)
  p <- sig.code(Diff.CDF.NI.full[i, "Padj"])
  
  polygon(c(0.50, 1, 1, 0.50), c(2, 2, 26, 26), border = F, col="gray93")
  text(0.50, 22, labels = "Invasive species", pos=4, cex=0.9, col="red2")
  text(0.55,14, labels = paste("N = ", No.sp[[i]][4], sep=""), pos=4, cex=0.9, col="red2")
  text(0.55,6, labels = paste("ΔCDF = ", obs, sep=""), pos=4, cex=0.9, col="red2")
  text(0.87,6, labels = p, pos=4, cex=1.5, col="red2")
}

mtext("Distance from the centroid of native species in 3D trait space", 1, 1.5, outer=T, cex=0.7)
mtext("Proportion of species (%)", 2, 2, outer=T, las=0, cex=0.7)

dev.off()

###########################################################################################
#                  PLOT CDFs FOR PHYLOGENETICALLY CORRECTED DATA                          #
###########################################################################################

#cairo_pdf("CDF3.tif", width=5.3, height = 7)
tiff("CDF3.tif", width = 5.3, height = 7, units = "in", res=500, compression = "lzw")
layout(matrix(1:6,ncol=2, byrow=T))
par(mar=c(0.5, 0.5, 1.5, 0.2), oma=c(3, 3.5, 0.5, 0.5), mgp=c(3,0.6,0), las=1, tck=-0.027)

CDF.obs <- list(yAxisT.3Dcor.obs, yAxisX.3Dcor.obs, yAxisS.3Dcor.obs, yAxisM.3Dcor.obs, yAxisK.3Dcor.obs, yAxisL.3Dcor.obs)
CDF.rand <- list(yAxisT.3Dcor.rand, yAxisX.3Dcor.rand, yAxisS.3Dcor.rand, yAxisM.3Dcor.rand, yAxisK.3Dcor.rand, yAxisL.3Dcor.rand)

No.sp <- list(table(traitT.3Dcor$INVASION.STATUS), 
              table(traitX.3Dcor$INVASION.STATUS), 
              table(traitS.3Dcor$INVASION.STATUS), 
              table(traitM.3Dcor$INVASION.STATUS), 
              table(traitK.3Dcor$INVASION.STATUS), 
              table(traitL.3Dcor$INVASION.STATUS))

habitat <- c("Grassland and heathland vegetation",
             "Ruderal and weed vegetation",
             "Rock and scree vegetation",
             "Wetland vegetation",
             "Scrub vegetation",
             "Forest vegetation")

for(i in 1:6)
{
  plot(xAxis, CDF.obs[[i]]$native*100, type='l', axes=F, xlab="", ylab="",  bty="n", col=NA, las=1, cex.axis=0.9)
  title(habitat[i], cex.main=1.1, line=0.5) 
  box(lwd=0.75)
  
  if(any(c(1,3,5) == i))  {axis(2, lwd=0.75)}
  if(any(5:6 == i))  {axis(1, lwd=0.75)}
  
  #calculate extent of randomized CDFs for invasive species
  rand.plot <- matrix(data=NA, ncol=1000, nrow=999)
  for(q in seq(1,999))
  {
    rand.plot[q,] <- CDF.rand[[i]]$inv[[q]]*100
  }
  rand.max <- apply(rand.plot, 2, FUN=quantile, probs = 0.975)
  rand.min <- apply(rand.plot, 2, FUN=quantile, probs = 0.025)
  
  #plot 95% interval of randomized CDFs for invasive species
  polygon(x=c(xAxis, rev(xAxis)), y=c(rand.min, rev(rand.max)), col="gray93", border = NA)
  
  #calculate extent of randomized CDFs for naturalized species
  rand.plot <- matrix(data=NA, ncol=1000, nrow=999)
  for(q in seq(1,999))
  {
    rand.plot[q,] <- CDF.rand[[i]]$natur[[q]]*100
  }
  rand.max <- apply(rand.plot, 2, FUN=quantile, probs = 0.975)
  rand.min <- apply(rand.plot, 2, FUN=quantile, probs = 0.025)
  
  #plot 95% interval of randomized CDFs for invasive species
  polygon(x=c(xAxis, rev(xAxis)), y=c(rand.min, rev(rand.max)), col="gray75", border = NA)
  
  #plot observed CDFs
  lines(xAxis, CDF.obs[[i]]$native*100, type='l', lwd=1, col="deepskyblue3")
  lines(xAxis, CDF.obs[[i]]$natur*100, type='l', lwd=1, col="yellow1")
  lines(xAxis, CDF.obs[[i]]$invasive*100, type='l', lwd=1, col="red2")
  
  #add statistics for native species
  text(0.50, 62, labels = "Native species", pos=4, cex=0.9, col="deepskyblue3")
  text(0.55,54, labels = paste("N = ", No.sp[[i]][1], sep=""), pos=4, cex=0.9, col="deepskyblue3")
  
  #add statistics for naturalized species
  obs <- round(Diff.CDFcor.NN[i, "dCDF"], 3)
  p <- sig.code(Diff.CDFcor.NN[i, "Padj"])
  
  polygon(c(0.50, 1, 1, 0.50), c(25, 25, 50, 50), border = F, col="gray75")
  text(0.50, 46, labels = "Naturalized species", pos=4, cex=0.9, col="yellow1")
  text(0.55,38, labels = paste("N = ", No.sp[[i]][3], sep=""), pos=4, cex=0.9, col="yellow1")
  text(0.55,30, labels = paste("ΔCDF = ", obs, sep=""), pos=4, cex=0.9, col="yellow1")
  text(0.855,30, labels = p, pos=4, cex=1.5, col="yellow1")
  
  #add statistics for invasive species
  obs <- round(Diff.CDFcor.NI[i, "dCDF"], 3)
  p <- sig.code(Diff.CDFcor.NI[i, "Padj"])
  
  polygon(c(0.50, 1, 1, 0.50), c(2, 2, 26, 26), border = F, col="gray93")
  text(0.50, 22, labels = "Invasive species", pos=4, cex=0.9, col="red2")
  text(0.55,14, labels = paste("N = ", No.sp[[i]][4], sep=""), pos=4, cex=0.9, col="red2")
  text(0.55,6, labels = paste("ΔCDF = ", obs, sep=""), pos=4, cex=0.9, col="red2")
  text(0.87,6, labels = p, pos=4, cex=1.5, col="red2")
}

mtext("Distance from the centroid of native species in 3D trait space", 1, 1.5, outer=T, cex=0.7)
mtext("Proportion of species (%)", 2, 2, outer=T, las=0, cex=0.7)

dev.off()

##############################################################################################
###PLOT PHYLOGENETICALLY CORRECTED CDFs FOR DATASET WITH IMPUTED TRAITS-----------------------

#cairo_pdf("CDF4.tif", width=5.3, height = 7)
tiff("CDF4.tif", width = 5.3, height = 7, units = "in", res=500, compression = "lzw")
layout(matrix(1:6,ncol=2, byrow=T))
par(mar=c(0.5, 0.5, 1.5, 0.2), oma=c(3, 3.5, 0.5, 0.5), mgp=c(3,0.6,0), las=1, tck=-0.027)

CDF.obs <- list(yAxisT.3Dcor.full.obs, yAxisX.3Dcor.full.obs, yAxisS.3Dcor.full.obs, yAxisM.3Dcor.full.obs, yAxisK.3Dcor.full.obs, yAxisL.3Dcor.full.obs)
CDF.rand <- list(yAxisT.3Dcor.full.rand, yAxisX.3Dcor.full.rand, yAxisS.3Dcor.full.rand, yAxisM.3Dcor.full.rand, yAxisK.3Dcor.full.rand, yAxisL.3Dcor.full.rand)

No.sp <- list(table(traitT.3Dcor.full$INVASION.STATUS), 
              table(traitX.3Dcor.full$INVASION.STATUS), 
              table(traitS.3Dcor.full$INVASION.STATUS), 
              table(traitM.3Dcor.full$INVASION.STATUS), 
              table(traitK.3Dcor.full$INVASION.STATUS), 
              table(traitL.3Dcor.full$INVASION.STATUS))

habitat <- c("Grassland and heathland vegetation",
             "Ruderal and weed vegetation",
             "Rock and scree vegetation",
             "Wetland vegetation",
             "Scrub vegetation",
             "Forest vegetation")

for(i in 1:6)
{
  plot(xAxis, CDF.obs[[i]]$native*100, type='l', axes=F, xlab="", ylab="",  bty="n", col=NA, las=1, cex.axis=0.9)
  title(habitat[i], cex.main=1.1, line=0.5) 
  box(lwd=0.75)
  
  if(any(c(1,3,5) == i))  {axis(2, lwd=0.75)}
  if(any(5:6 == i))  {axis(1, lwd=0.75)}
  
  #calculate extent of randomized CDFs for invasive species
  rand.plot <- matrix(data=NA, ncol=1000, nrow=999)
  for(q in seq(1,999))
  {
    rand.plot[q,] <- CDF.rand[[i]]$inv[[q]]*100
  }
  rand.max <- apply(rand.plot, 2, FUN=quantile, probs = 0.975)
  rand.min <- apply(rand.plot, 2, FUN=quantile, probs = 0.025)
  
  #plot 95% interval of randomized CDFs for invasive species
  polygon(x=c(xAxis, rev(xAxis)), y=c(rand.min, rev(rand.max)), col="gray93", border = NA)
  
  #calculate extent of randomized CDFs for naturalized species
  rand.plot <- matrix(data=NA, ncol=1000, nrow=999)
  for(q in seq(1,999))
  {
    rand.plot[q,] <- CDF.rand[[i]]$natur[[q]]*100
  }
  rand.max <- apply(rand.plot, 2, FUN=quantile, probs = 0.975)
  rand.min <- apply(rand.plot, 2, FUN=quantile, probs = 0.025)
  
  #plot 95% interval of randomized CDFs for invasive species
  polygon(x=c(xAxis, rev(xAxis)), y=c(rand.min, rev(rand.max)), col="gray75", border = NA)
  
  #plot observed CDFs
  lines(xAxis, CDF.obs[[i]]$native*100, type='l', lwd=1, col="deepskyblue3")
  lines(xAxis, CDF.obs[[i]]$natur*100, type='l', lwd=1, col="yellow1")
  lines(xAxis, CDF.obs[[i]]$invasive*100, type='l', lwd=1, col="red2")
  
  #add statistics for native species
  text(0.50, 62, labels = "Native species", pos=4, cex=0.9, col="deepskyblue3")
  text(0.55,54, labels = paste("N = ", No.sp[[i]][1], sep=""), pos=4, cex=0.9, col="deepskyblue3")
  
  #add statistics for naturalized species
  obs <- round(Diff.CDFcor.NN.full[i, "dCDF"], 3)
  p <- sig.code(Diff.CDFcor.NN.full[i, "Padj"])
  
  polygon(c(0.50, 1, 1, 0.50), c(25, 25, 50, 50), border = F, col="gray75")
  text(0.50, 46, labels = "Naturalized species", pos=4, cex=0.9, col="yellow1")
  text(0.55,38, labels = paste("N = ", No.sp[[i]][3], sep=""), pos=4, cex=0.9, col="yellow1")
  text(0.55,30, labels = paste("ΔCDF = ", obs, sep=""), pos=4, cex=0.9, col="yellow1")
  text(0.855,30, labels = p, pos=4, cex=1.5, col="yellow1")
  
  #add statistics for invasive species
  obs <- round(Diff.CDFcor.NI.full[i, "dCDF"], 3)
  p <- sig.code(Diff.CDFcor.NI.full[i, "Padj"])
  
  polygon(c(0.50, 1, 1, 0.50), c(2, 2, 26, 26), border = F, col="gray93")
  text(0.50, 22, labels = "Invasive species", pos=4, cex=0.9, col="red2")
  text(0.55,14, labels = paste("N = ", No.sp[[i]][4], sep=""), pos=4, cex=0.9, col="red2")
  text(0.55,6, labels = paste("ΔCDF = ", obs, sep=""), pos=4, cex=0.9, col="red2")
  text(0.87,6, labels = p, pos=4, cex=1.5, col="red2")
}

mtext("Distance from the centroid of native species in 3D trait space", 1, 1.5, outer=T, cex=0.7)
mtext("Proportion of species (%)", 2, 2, outer=T, las=0, cex=0.7)

dev.off()
