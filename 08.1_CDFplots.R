###########################################################################################
#                               PLOT CDFs FOR ALL HABITATS                                #
###########################################################################################

#code by Jan Divisek (2018)

#cairo_pdf("CDF1.pdf", width=5, height = 7.2)
#tiff("CDF1.tif", width = 5, height = 7.2, units = "in", res=300, compression = "lzw")

pdf("CDF1.pdf", width=7, height = 5.1)
#tiff("CDF1.tif", width = 7, height = 5.1, units = "in", res=400, compression = "lzw")
#windows(width=7.3, height = 5.2)
layout(matrix(1:6,ncol=3, byrow=T))
par(mar=c(2, 0.5, 1.5, 0.4), oma=c(3, 3.5, 0.5, 0.5), mgp=c(3,0.6,0), las=1, tck=-0.027)

CDFx <- xAxis
CDFy.obs <- list(yAxis$T$obs, yAxis$X$obs, yAxis$S$obs, yAxis$M$obs, yAxis$K$obs, yAxis$L$obs)
CDFy.rand <- list(yAxis$T$rand, yAxis$X$rand, yAxis$S$rand, yAxis$M$rand, yAxis$K$rand, yAxis$L$rand)

test <- list()
test$natur <- Diff.CDF.NN
test$inv <- Diff.CDF.NI

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
  plot(CDFx[[i]], CDFy.obs[[i]]$native*100, ylim=c(-10, 100), type='l', axes=F, xlab="", ylab="",  bty="n", col=NA, las=1, cex.axis=0.9)
  title(habitat[i], cex.main=1.1, line=0.5) 
  box(lwd=0.75)
  axis(1, lwd=0.75, at=quantile(CDFx[[i]], seq(0,1, by=0.2)), labels=as.character(seq(0,100, by=20)))
  d.lab <- axis(1, lwd=0.75, tck=0.02, labels=F, tick = F)
  if(max(CDFx[[i]])< max(d.lab)) d.lab <- d.lab[-c(length(d.lab))]
  axis(1, at=d.lab, lwd=0.75, tck=0.02, labels=F)
  text(d.lab, -7, d.lab)
  
  if(any(c(1,4) == i))#c(1,3,5)
  {
    axis(2, at=seq(0,100, by=20), labels=seq(0,100, by=20) ,lwd=0.75)
  }
  
  #calculate extent of randomized CDFs for invasive species
  rand.plot <- matrix(data=NA, ncol=1000, nrow=999)
  for(q in seq(1,999))
  {
    rand.plot[q,] <- CDFy.rand[[i]]$inv[[q]]*100
  }
  rand.max <- apply(rand.plot, 2, FUN=quantile, probs = 0.975)
  rand.min <- apply(rand.plot, 2, FUN=quantile, probs = 0.025)
  
  #plot 95% interval of randomized CDFs for invasive species
  polygon(x=c(CDFx[[i]], rev(CDFx[[i]])), y=c(rand.min, rev(rand.max)), col="gray93", border = NA)
  
  #calculate extent of randomized CDFs for naturalized species
  rand.plot <- matrix(data=NA, ncol=1000, nrow=999)
  for(q in seq(1,999))
  {
    rand.plot[q,] <- CDFy.rand[[i]]$natur[[q]]*100
  }
  rand.max <- apply(rand.plot, 2, FUN=quantile, probs = 0.975)
  rand.min <- apply(rand.plot, 2, FUN=quantile, probs = 0.025)
  
  #plot 95% interval of randomized CDFs for invasive species
  polygon(x=c(CDFx[[i]], rev(CDFx[[i]])), y=c(rand.min, rev(rand.max)), col="gray75", border = NA)
  
  #plot observed CDFs
  lines(CDFx[[i]], CDFy.obs[[i]]$native*100, type='l', lwd=1, col="deepskyblue3")
  lines(CDFx[[i]], CDFy.obs[[i]]$natur*100, type='l', lwd=1, col="yellow1")
  lines(CDFx[[i]], CDFy.obs[[i]]$invasive*100, type='l', lwd=1, col="red2")
  
  #add statistics for native species
  text(0.50*max(CDFx[[i]]), 62, labels = "Native species", pos=4, cex=0.9, col="deepskyblue3")
  text(0.55*max(CDFx[[i]]),54, labels = paste("N = ", No.sp[[i]][1], sep=""), pos=4, cex=0.9, col="deepskyblue3")
  
  #add statistics for naturalized species
  obs <- round(test$natur[i, "dCDF"], 3)
  p <- sig.code(test$natur[i, "Padj"])
  
  polygon(c(0.50, 1, 1, 0.50)*max(CDFx[[i]]), c(25, 25, 50, 50), border = F, col="gray75")
  text(0.50*max(CDFx[[i]]), 46, labels = "Naturalized species", pos=4, cex=0.9, col="yellow1")
  text(0.55*max(CDFx[[i]]),38, labels = paste("N = ", No.sp[[i]][3], sep=""), pos=4, cex=0.9, col="yellow1")
  text(0.55*max(CDFx[[i]]),30, labels = bquote(paste(Delta, "CDF = ", .(obs), sep="")), pos=4, cex=0.9, col="yellow1")
  text(0.87*max(CDFx[[i]]),30, labels = p, pos=4, cex=1.5, col="yellow1")
  
  #add statistics for invasive species
  obs <- round(test$inv[i, "dCDF"], 3)
  p <- sig.code(test$inv[i, "Padj"])
  
  polygon(c(0.50, 1, 1, 0.50)*max(CDFx[[i]]), c(2, 2, 26, 26), border = F, col="gray93")
  text(0.50*max(CDFx[[i]]), 22, labels = "Invasive species", pos=4, cex=0.9, col="red2")
  text(0.55*max(CDFx[[i]]),14, labels = paste("N = ", No.sp[[i]][4], sep=""), pos=4, cex=0.9, col="red2")
  text(0.55*max(CDFx[[i]]),6, labels = bquote(paste(Delta, "CDF = ", .(obs), sep="")), pos=4, cex=0.9, col="red2")
  text(0.905*max(CDFx[[i]]),6, labels = p, pos=4, cex=1.5, col="red2")
}

mtext(expression(paste("Distance from the centroid of native species in 3D trait space ", 
                       bgroup("(", frac("SD","%"),")")), sep=""), 1, 1.8, outer=T, cex=0.7)
mtext("Proportion of species (%)", 2, 2, outer=T, las=0, cex=0.7)

dev.off()

##############################################################################################
###PLOT CDFs FOR DATASET WITH IMPUTED TRAITS--------------------------------------------------

#cairo_pdf("CDF2.pdf", width=5, height = 7.2)
#tiff("CDF2.tif", width = 5, height = 7.2, units = "in", res=300, compression = "lzw")

windows(width=7.3, height = 5.2)
#cairo_pdf("CDF2.pdf", width=7, height = 5.1)
#tiff("CDF2.tif", width = 7, height = 5.1, units = "in", res=400, compression = "lzw")
layout(matrix(1:6,ncol=3, byrow=T))
par(mar=c(2, 0.5, 1.5, 0.4), oma=c(3, 3.5, 0.5, 0.5), mgp=c(3,0.6,0), las=1, tck=-0.027)

CDFx <- xAxis.imp
CDFy.obs <- list(yAxis.imp$T$obs, yAxis.imp$X$obs, yAxis.imp$S$obs, yAxis.imp$M$obs, yAxis.imp$K$obs, yAxis.imp$L$obs)
CDFy.rand <- list(yAxis.imp$T$rand, yAxis.imp$X$rand, yAxis.imp$S$rand, yAxis.imp$M$rand, yAxis.imp$K$rand, yAxis.imp$L$rand)

test <- list()
test$natur <- Diff.CDF.imp.NN
test$inv <- Diff.CDF.imp.NI

No.sp <- list(table(traitT.3D.imp$INVASION.STATUS), 
              table(traitX.3D.imp$INVASION.STATUS), 
              table(traitS.3D.imp$INVASION.STATUS), 
              table(traitM.3D.imp$INVASION.STATUS), 
              table(traitK.3D.imp$INVASION.STATUS), 
              table(traitL.3D.imp$INVASION.STATUS))

habitat <- c("Grassland and heathland vegetation",
             "Ruderal and weed vegetation",
             "Rock and scree vegetation",
             "Wetland vegetation",
             "Scrub vegetation",
             "Forest vegetation")

for(i in 1:6)
{
  plot(CDFx[[i]], CDFy.obs[[i]]$native*100, ylim=c(-10, 100), type='l', axes=F, xlab="", ylab="",  bty="n", col=NA, las=1, cex.axis=0.9)
  title(habitat[i], cex.main=1.1, line=0.5) 
  box(lwd=0.75)
  axis(1, lwd=0.75, at=quantile(CDFx[[i]], seq(0,1, by=0.2)), labels=as.character(seq(0,100, by=20)))
  d.lab <- axis(1, lwd=0.75, tck=0.02, labels=F, tick = F)
  if(max(CDFx[[i]])< max(d.lab)) d.lab <- d.lab[-c(length(d.lab))]
  axis(1, at=d.lab, lwd=0.75, tck=0.02, labels=F)
  text(d.lab, -7, d.lab)
  
  if(any(c(1,4) == i))#
  {
    axis(2, at=seq(0,100, by=20), labels=seq(0,100, by=20) ,lwd=0.75)
  }
  
  #calculate extent of randomized CDFs for invasive species
  rand.plot <- matrix(data=NA, ncol=1000, nrow=999)
  for(q in seq(1,999))
  {
    rand.plot[q,] <- CDFy.rand[[i]]$inv[[q]]*100
  }
  rand.max <- apply(rand.plot, 2, FUN=quantile, probs = 0.975)
  rand.min <- apply(rand.plot, 2, FUN=quantile, probs = 0.025)
  
  #plot 95% interval of randomized CDFs for invasive species
  polygon(x=c(CDFx[[i]], rev(CDFx[[i]])), y=c(rand.min, rev(rand.max)), col="gray93", border = NA)
  
  #calculate extent of randomized CDFs for naturalized species
  rand.plot <- matrix(data=NA, ncol=1000, nrow=999)
  for(q in seq(1,999))
  {
    rand.plot[q,] <- CDFy.rand[[i]]$natur[[q]]*100
  }
  rand.max <- apply(rand.plot, 2, FUN=quantile, probs = 0.975)
  rand.min <- apply(rand.plot, 2, FUN=quantile, probs = 0.025)
  
  #plot 95% interval of randomized CDFs for invasive species
  polygon(x=c(CDFx[[i]], rev(CDFx[[i]])), y=c(rand.min, rev(rand.max)), col="gray75", border = NA)
  
  #plot observed CDFs
  lines(CDFx[[i]], CDFy.obs[[i]]$native*100, type='l', lwd=1, col="deepskyblue3")
  lines(CDFx[[i]], CDFy.obs[[i]]$natur*100, type='l', lwd=1, col="yellow1")
  lines(CDFx[[i]], CDFy.obs[[i]]$invasive*100, type='l', lwd=1, col="red2")
  
  #add statistics for native species
  text(0.50*max(CDFx[[i]]), 62, labels = "Native species", pos=4, cex=0.9, col="deepskyblue3")
  text(0.55*max(CDFx[[i]]),54, labels = paste("N = ", No.sp[[i]][1], sep=""), pos=4, cex=0.9, col="deepskyblue3")
  
  #add statistics for naturalized species
  obs <- round(test$natur[i, "dCDF"], 3)
  p <- sig.code(test$natur[i, "Padj"])
  
  polygon(c(0.50, 1, 1, 0.50)*max(CDFx[[i]]), c(25, 25, 50, 50), border = F, col="gray75")
  text(0.50*max(CDFx[[i]]), 46, labels = "Naturalized species", pos=4, cex=0.9, col="yellow1")
  text(0.55*max(CDFx[[i]]),38, labels = paste("N = ", No.sp[[i]][3], sep=""), pos=4, cex=0.9, col="yellow1")
  text(0.55*max(CDFx[[i]]),30, labels = bquote(paste(Delta, "CDF = ", .(obs), sep="")), pos=4, cex=0.9, col="yellow1")
  text(0.88*max(CDFx[[i]]),30, labels = p, pos=4, cex=1.5, col="yellow1")
  
  #add statistics for invasive species
  obs <- round(test$inv[i, "dCDF"], 3)
  p <- sig.code(test$inv[i, "Padj"])
  
  polygon(c(0.50, 1, 1, 0.50)*max(CDFx[[i]]), c(2, 2, 26, 26), border = F, col="gray93")
  text(0.50*max(CDFx[[i]]), 22, labels = "Invasive species", pos=4, cex=0.9, col="red2")
  text(0.55*max(CDFx[[i]]),14, labels = paste("N = ", No.sp[[i]][4], sep=""), pos=4, cex=0.9, col="red2")
  text(0.55*max(CDFx[[i]]),6, labels = bquote(paste(Delta, "CDF = ", .(obs), sep="")), pos=4, cex=0.9, col="red2")
  text(0.905*max(CDFx[[i]]),6, labels = p, pos=4, cex=1.5, col="red2")
}

mtext(expression(paste("Distance from the centroid of native species in 3D trait space ", 
                       bgroup("(", frac("SD","%"),")")), sep=""), 1, 1.8, outer=T, cex=0.7)
mtext("Proportion of species (%)", 2, 2, outer=T, las=0, cex=0.7)

dev.off()

###########################################################################################
#                  PLOT CDFs FOR PHYLOGENETICALLY CORRECTED DATA                          #
###########################################################################################

#cairo_pdf("CDF3.pdf", width=5, height = 7.2)
#tiff("CDF3.tif", width = 5, height = 7.2, units = "in", res=300, compression = "lzw")

windows(width=7.3, height = 5.2)
#cairo_pdf("CDF3.pdf", width=7, height = 5.1)
#tiff("CDF3.tif", width = 7, height = 5.1, units = "in", res=400, compression = "lzw")
layout(matrix(1:6,ncol=3, byrow=T))
par(mar=c(2, 0.5, 1.5, 0.4), oma=c(3, 3.5, 0.5, 0.5), mgp=c(3,0.6,0), las=1, tck=-0.027)

CDFx <- xAxis.cor
CDFy.obs <- list(yAxis.cor$T$obs, yAxis.cor$X$obs, yAxis.cor$S$obs, yAxis.cor$M$obs, yAxis.cor$K$obs, yAxis.cor$L$obs)
CDFy.rand <- list(yAxis.cor$T$rand, yAxis.cor$X$rand, yAxis.cor$S$rand, yAxis.cor$M$rand, yAxis.cor$K$rand, yAxis.cor$L$rand)

test <- list()
test$natur <- Diff.CDFcor.NN
test$inv <- Diff.CDFcor.NI

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
  plot(CDFx[[i]], CDFy.obs[[i]]$native*100, ylim=c(-10, 100), type='l', axes=F, xlab="", ylab="",  bty="n", col=NA, las=1, cex.axis=0.9)
  title(habitat[i], cex.main=1.1, line=0.5) 
  box(lwd=0.75)
  axis(1, lwd=0.75, at=quantile(CDFx[[i]], seq(0,1, by=0.2)), labels=as.character(seq(0,100, by=20)))
  d.lab <- axis(1, lwd=0.75, tck=0.02, labels=F, tick = F)
  if(max(CDFx[[i]])< max(d.lab)) d.lab <- d.lab[-c(length(d.lab))]
  axis(1, at=d.lab, lwd=0.75, tck=0.02, labels=F)
  text(d.lab, -7, d.lab)
  
  if(any(c(1,4) == i))#c(1,3,5)
  {
    axis(2, at=seq(0,100, by=20), labels=seq(0,100, by=20) ,lwd=0.75)
  }
  
  #calculate extent of randomized CDFs for invasive species
  rand.plot <- matrix(data=NA, ncol=1000, nrow=999)
  for(q in seq(1,999))
  {
    rand.plot[q,] <- CDFy.rand[[i]]$inv[[q]]*100
  }
  rand.max <- apply(rand.plot, 2, FUN=quantile, probs = 0.975)
  rand.min <- apply(rand.plot, 2, FUN=quantile, probs = 0.025)
  
  #plot 95% interval of randomized CDFs for invasive species
  polygon(x=c(CDFx[[i]], rev(CDFx[[i]])), y=c(rand.min, rev(rand.max)), col="gray93", border = NA)
  
  #calculate extent of randomized CDFs for naturalized species
  rand.plot <- matrix(data=NA, ncol=1000, nrow=999)
  for(q in seq(1,999))
  {
    rand.plot[q,] <- CDFy.rand[[i]]$natur[[q]]*100
  }
  rand.max <- apply(rand.plot, 2, FUN=quantile, probs = 0.975)
  rand.min <- apply(rand.plot, 2, FUN=quantile, probs = 0.025)
  
  #plot 95% interval of randomized CDFs for invasive species
  polygon(x=c(CDFx[[i]], rev(CDFx[[i]])), y=c(rand.min, rev(rand.max)), col="gray75", border = NA)
  
  #plot observed CDFs
  lines(CDFx[[i]], CDFy.obs[[i]]$native*100, type='l', lwd=1, col="deepskyblue3")
  lines(CDFx[[i]], CDFy.obs[[i]]$natur*100, type='l', lwd=1, col="yellow1")
  lines(CDFx[[i]], CDFy.obs[[i]]$invasive*100, type='l', lwd=1, col="red2")
  
  #add statistics for native species
  text(0.50*max(CDFx[[i]]), 62, labels = "Native species", pos=4, cex=0.9, col="deepskyblue3")
  text(0.55*max(CDFx[[i]]),54, labels = paste("N = ", No.sp[[i]][1], sep=""), pos=4, cex=0.9, col="deepskyblue3")
  
  #add statistics for naturalized species
  obs <- round(test$natur[i, "dCDF"], 3)
  p <- sig.code(test$natur[i, "Padj"])
  
  polygon(c(0.50, 1, 1, 0.50)*max(CDFx[[i]]), c(25, 25, 50, 50), border = F, col="gray75")
  text(0.50*max(CDFx[[i]]), 46, labels = "Naturalized species", pos=4, cex=0.9, col="yellow1")
  text(0.55*max(CDFx[[i]]),38, labels = paste("N = ", No.sp[[i]][3], sep=""), pos=4, cex=0.9, col="yellow1")
  text(0.55*max(CDFx[[i]]),30, labels = bquote(paste(Delta, "CDF = ", .(obs), sep="")), pos=4, cex=0.9, col="yellow1")
  text(0.88*max(CDFx[[i]]),30, labels = p, pos=4, cex=1.5, col="yellow1")
  
  #add statistics for invasive species
  obs <- round(test$inv[i, "dCDF"], 3)
  p <- sig.code(test$inv[i, "Padj"])
  
  polygon(c(0.50, 1, 1, 0.50)*max(CDFx[[i]]), c(2, 2, 26, 26), border = F, col="gray93")
  text(0.50*max(CDFx[[i]]), 22, labels = "Invasive species", pos=4, cex=0.9, col="red2")
  text(0.55*max(CDFx[[i]]),14, labels = paste("N = ", No.sp[[i]][4], sep=""), pos=4, cex=0.9, col="red2")
  text(0.55*max(CDFx[[i]]),6, labels = bquote(paste(Delta, "CDF = ", .(obs), sep="")), pos=4, cex=0.9, col="red2")
  text(0.905*max(CDFx[[i]]),6, labels = p, pos=4, cex=1.5, col="red2")
}

mtext(expression(paste("Distance from the centroid of native species in 3D trait space ", 
                       bgroup("(", frac("SD","%"),")")), sep=""), 1, 1.8, outer=T, cex=0.7)
mtext("Proportion of species (%)", 2, 2, outer=T, las=0, cex=0.7)

dev.off()

##############################################################################################
###PLOT PHYLOGENETICALLY CORRECTED CDFs FOR DATASET WITH IMPUTED TRAITS-----------------------

#cairo_pdf("CDF4.pdf", width=5, height = 7.2)
#tiff("CDF4.tif", width = 5, height = 7.2, units = "in", res=300, compression = "lzw")

windows(width=7.3, height = 5.2)
#cairo_pdf("CDF4.pdf", width=7, height = 5.1)
#tiff("CDF4.tif", width = 7, height = 5.1, units = "in", res=400, compression = "lzw")
layout(matrix(1:6,ncol=3, byrow=T))
par(mar=c(2, 0.5, 1.5, 0.4), oma=c(3, 3.5, 0.5, 0.5), mgp=c(3,0.6,0), las=1, tck=-0.027)

CDFx <- xAxis.cor.imp
CDFy.obs <- list(yAxis.cor.imp$T$obs, yAxis.cor.imp$X$obs, yAxis.cor.imp$S$obs, yAxis.cor.imp$M$obs, yAxis.cor.imp$K$obs, yAxis.cor.imp$L$obs)
CDFy.rand <- list(yAxis.cor.imp$T$rand, yAxis.cor.imp$X$rand, yAxis.cor.imp$S$rand, yAxis.cor.imp$M$rand, yAxis.cor.imp$K$rand, yAxis.cor.imp$L$rand)

test <- list()
test$natur <- Diff.CDFcor.imp.NN
test$inv <- Diff.CDFcor.imp.NI

No.sp <- list(table(traitT.3Dcor.imp$INVASION.STATUS), 
              table(traitX.3Dcor.imp$INVASION.STATUS), 
              table(traitS.3Dcor.imp$INVASION.STATUS), 
              table(traitM.3Dcor.imp$INVASION.STATUS), 
              table(traitK.3Dcor.imp$INVASION.STATUS), 
              table(traitL.3Dcor.imp$INVASION.STATUS))

habitat <- c("Grassland and heathland vegetation",
             "Ruderal and weed vegetation",
             "Rock and scree vegetation",
             "Wetland vegetation",
             "Scrub vegetation",
             "Forest vegetation")

for(i in 1:6)
{
  plot(CDFx[[i]], CDFy.obs[[i]]$native*100, ylim=c(-10, 100), type='l', axes=F, xlab="", ylab="",  bty="n", col=NA, las=1, cex.axis=0.9)
  title(habitat[i], cex.main=1.1, line=0.5) 
  box(lwd=0.75)
  axis(1, lwd=0.75, at=quantile(CDFx[[i]], seq(0,1, by=0.2)), labels=as.character(seq(0,100, by=20)))
  d.lab <- axis(1, lwd=0.75, tck=0.02, labels=F, tick = F)
  if(max(CDFx[[i]])< max(d.lab)) d.lab <- d.lab[-c(length(d.lab))]
  axis(1, at=d.lab, lwd=0.75, tck=0.02, labels=F)
  text(d.lab, -7, d.lab)
  
  if(any(c(1,4) == i))#c(1,3,5)
  {
    axis(2, at=seq(0,100, by=20), labels=seq(0,100, by=20) ,lwd=0.75)
  }
  
  #calculate extent of randomized CDFs for invasive species
  rand.plot <- matrix(data=NA, ncol=1000, nrow=999)
  for(q in seq(1,999))
  {
    rand.plot[q,] <- CDFy.rand[[i]]$inv[[q]]*100
  }
  rand.max <- apply(rand.plot, 2, FUN=quantile, probs = 0.975)
  rand.min <- apply(rand.plot, 2, FUN=quantile, probs = 0.025)
  
  #plot 95% interval of randomized CDFs for invasive species
  polygon(x=c(CDFx[[i]], rev(CDFx[[i]])), y=c(rand.min, rev(rand.max)), col="gray93", border = NA)
  
  #calculate extent of randomized CDFs for naturalized species
  rand.plot <- matrix(data=NA, ncol=1000, nrow=999)
  for(q in seq(1,999))
  {
    rand.plot[q,] <- CDFy.rand[[i]]$natur[[q]]*100
  }
  rand.max <- apply(rand.plot, 2, FUN=quantile, probs = 0.975)
  rand.min <- apply(rand.plot, 2, FUN=quantile, probs = 0.025)
  
  #plot 95% interval of randomized CDFs for invasive species
  polygon(x=c(CDFx[[i]], rev(CDFx[[i]])), y=c(rand.min, rev(rand.max)), col="gray75", border = NA)
  
  #plot observed CDFs
  lines(CDFx[[i]], CDFy.obs[[i]]$native*100, type='l', lwd=1, col="deepskyblue3")
  lines(CDFx[[i]], CDFy.obs[[i]]$natur*100, type='l', lwd=1, col="yellow1")
  lines(CDFx[[i]], CDFy.obs[[i]]$invasive*100, type='l', lwd=1, col="red2")
  
  #add statistics for native species
  text(0.50*max(CDFx[[i]]), 62, labels = "Native species", pos=4, cex=0.9, col="deepskyblue3")
  text(0.55*max(CDFx[[i]]),54, labels = paste("N = ", No.sp[[i]][1], sep=""), pos=4, cex=0.9, col="deepskyblue3")
  
  #add statistics for naturalized species
  obs <- round(test$natur[i, "dCDF"], 3)
  p <- sig.code(test$natur[i, "Padj"])
  
  polygon(c(0.50, 1, 1, 0.50)*max(CDFx[[i]]), c(25, 25, 50, 50), border = F, col="gray75")
  text(0.50*max(CDFx[[i]]), 46, labels = "Naturalized species", pos=4, cex=0.9, col="yellow1")
  text(0.55*max(CDFx[[i]]),38, labels = paste("N = ", No.sp[[i]][3], sep=""), pos=4, cex=0.9, col="yellow1")
  text(0.55*max(CDFx[[i]]),30, labels = bquote(paste(Delta, "CDF = ", .(obs), sep="")), pos=4, cex=0.9, col="yellow1")
  text(0.88*max(CDFx[[i]]),30, labels = p, pos=4, cex=1.5, col="yellow1")
  
  #add statistics for invasive species
  obs <- round(test$inv[i, "dCDF"], 3)
  p <- sig.code(test$inv[i, "Padj"])
  
  polygon(c(0.50, 1, 1, 0.50)*max(CDFx[[i]]), c(2, 2, 26, 26), border = F, col="gray93")
  text(0.50*max(CDFx[[i]]), 22, labels = "Invasive species", pos=4, cex=0.9, col="red2")
  text(0.55*max(CDFx[[i]]),14, labels = paste("N = ", No.sp[[i]][4], sep=""), pos=4, cex=0.9, col="red2")
  text(0.55*max(CDFx[[i]]),6, labels = bquote(paste(Delta, "CDF = ", .(obs), sep="")), pos=4, cex=0.9, col="red2")
  text(0.905*max(CDFx[[i]]),6, labels = p, pos=4, cex=1.5, col="red2")
}

mtext(expression(paste("Distance from the centroid of native species in 3D trait space ", 
                       bgroup("(", frac("SD","%"),")")), sep=""), 1, 1.8, outer=T, cex=0.7)
mtext("Proportion of species (%)", 2, 2, outer=T, las=0, cex=0.7)

dev.off()