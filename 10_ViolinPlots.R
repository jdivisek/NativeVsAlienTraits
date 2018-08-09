###########################################################################################
#                   VIOLIN PLOTS WITH RAW VALUES FOR ALL HABITATS                         #
###########################################################################################

#code by Jan Divisek (2018)

#library(vegan)
library(vioplot)

###VIOLIN PLOTS---------------------------------------------------------------------------

#cairo_pdf("Violin1.pdf", width=5.3, height = 7)
#tiff("Violin1.tif", width = 5.3, height = 7, units = "in", res=300, compression = "lzw")

#windows(7,4.8,  rescale = "fixed")
svg("Violin1.svg", width=7, height = 4.8)
#cairo_pdf("Violin1.pdf", width=7, height = 4.8)
#tiff("Violin1.tif", width = 7, height = 4.8, units = "in", res=400, compression = "lzw")
layout(matrix(1:6,ncol=3, byrow=T))
par(mar=c(0.5, 0.5, 1.5, 0.2), oma=c(3, 3.5, 0.5, 0.5), mgp=c(3,0.6,0), las=1, tck=-0.027)

PlotData <- list(traitT.3D, traitX.3D, traitS.3D, traitM.3D, traitK.3D, traitL.3D)

# test <- list()
# test$natur <- Wilcox.NN
# test$inv <- Wilcox.NI

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

stat <- list()

for(i in 1:6)
{
  if(any(1:4 == i))
  {
    plot(1, 1, xlim=c(0.5,3.5), ylim=c(0,5), type='n', axes=F, xlab="", ylab="", las=1, cex.axis=0.9)
  }
  if(any(5:6 == i))
  {
    plot(1, 1, xlim=c(0.5,3.5), ylim=c(0,6.5), type='n', axes=F, xlab="", ylab="", las=1, cex.axis=0.9)
  }
  
  title(habitat[i], cex.main=1.1, line=0.5) 
  box(lwd=0.75)
  
  if(any(c(1,4) == i))#c(1,3,5)
  {
    axis(2, lwd=0.75)
  }
  
  if(any(4:6 == i))#5:6
  {
    axis(1, 1:3, c("native", "naturalized", "invasive"), lwd=0.75)
  }
  
  #calculate centroid of native species
  centr <- apply(PlotData[[i]][which(PlotData[[i]]$INVASION.STATUS == "native"), c(1:3)], 2, mean)
  centr <- rbind(centr, PlotData[[i]][,c(1:3)])
  
  #calculate distances of species from observed native centroid
  d.centr <- as.matrix(dist(centr))[,1]
  d.centr <- d.centr[-1]#/max(d.centr[-1])
  
  #violin plots
  vioplot(d.centr[which(PlotData[[i]]$INVASION.STATUS == "native")], at = 1, add = T, border = 'deepskyblue3', col=box.cols2[1], lwd=1.5)
  vioplot(d.centr[which(PlotData[[i]]$INVASION.STATUS == "naturalized")], at = 2, add = T, border = 'gold', col=box.cols2[2], lwd=1.5)
  vioplot(d.centr[which(PlotData[[i]]$INVASION.STATUS == "invasive")], at = 3, add = T, border = 'red2', col=box.cols2[3], lwd=1.5)
  
  stat[[i]] <- list(summary(d.centr[which(PlotData[[i]]$INVASION.STATUS == "native")]),
                    summary(d.centr[which(PlotData[[i]]$INVASION.STATUS == "naturalized")]),
                    summary(d.centr[which(PlotData[[i]]$INVASION.STATUS == "invasive")]))
  names(stat[[i]]) <- c("native", "naturalized", "invasive")  

  
  if(any(1:4 == i))
  {
    #add numbers of species
    text(1,4.9, paste("N = ", No.sp[[i]][1], sep=""), cex=1)
    text(2,4.9, paste("N = ", No.sp[[i]][3], sep=""), cex=1)
    text(3,4.9, paste("N = ", No.sp[[i]][4], sep=""), cex=1)
    
    # #add results of Wilcoxon Rank Sum test
    # text(2,4.5, sig.code(test$natur$Padj[i]), cex=2)
    # text(3,4.5, sig.code(test$inv$Padj[i]), cex=2)
  }
  
  if(any(5:6 == i))
  {
    #add numbers of species
    text(1,6.4, paste("N = ", No.sp[[i]][1], sep=""), cex=1)
    text(2,6.4, paste("N = ", No.sp[[i]][3], sep=""), cex=1)
    text(3,6.4, paste("N = ", No.sp[[i]][4], sep=""), cex=1)
    
    # #add results of Wilcoxon Rank Sum test
    # text(2,5.9, sig.code(test$natur$Padj[i]), cex=2)
    # text(3,5.9, sig.code(test$inv$Padj[i]), cex=2)
  }
  
}

mtext("Distance from the centroid of native species in 3D trait space (SD)", 2, 2, outer=T, las=0, cex=0.7)
mtext("Invasion status", 1, 1.5, outer=T, las=0, cex=0.65)

dev.off()

Dist.stats <- stat
Dist.stats

###VIOLIN PLOTS FOR DATASET WITH IMPUTED TRAITS--------------------------------------------------------

#cairo_pdf("Violin2.pdf", width=5.3, height = 7)
#tiff("Violin2.tif", width = 5.3, height = 7, units = "in", res=300, compression = "lzw")

#windows(7,4.8, rescale = "fixed")
svg("Violin2.svg", width=7, height = 4.8)
#cairo_pdf("Violin2.pdf", width=7, height = 4.8)
#tiff("Violin2.tif", width = 7, height = 4.8, units = "in", res=400, compression = "lzw")
layout(matrix(1:6,ncol=3, byrow=T))
par(mar=c(0.5, 0.5, 1.5, 0.2), oma=c(3, 3.5, 0.5, 0.5), mgp=c(3,0.6,0), las=1, tck=-0.027)

PlotData <- list(traitT.3D.imp, traitX.3D.imp, traitS.3D.imp, traitM.3D.imp, traitK.3D.imp, traitL.3D.imp)

# test <- list()
# test$natur <- Wilcox.imp.NN
# test$inv <- Wilcox.imp.NI

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

stat <- list()

for(i in 1:6)
{
  if(any(1:4 == i))
  {
    plot(1, 1, xlim=c(0.5,3.5), ylim=c(0,5.5), type='n', axes=F, xlab="", ylab="", las=1, cex.axis=0.9)
  }
  if(any(5:6 == i))
  {
    plot(1, 1, xlim=c(0.5,3.5), ylim=c(0,6.5), type='n', axes=F, xlab="", ylab="", las=1, cex.axis=0.9)
  }
  
  title(habitat[i], cex.main=1.1, line=0.5) 
  box(lwd=0.75)
  
  if(any(c(1,4) == i))#c(1,3,5)
  {
    axis(2, lwd=0.75)
  }
  
  if(any(4:6 == i))#5:6
  {
    axis(1, 1:3, c("native", "naturalized", "invasive"), lwd=0.75)
  }
  
  #calculate centroid of native species
  centr <- apply(PlotData[[i]][which(PlotData[[i]]$INVASION.STATUS == "native"), c(1:3)], 2, mean)
  centr <- rbind(centr, PlotData[[i]][,c(1:3)])
  
  #calculate distances of species from observed native centroid
  d.centr <- as.matrix(dist(centr))[,1]
  d.centr <- d.centr[-1]
  
  #violin plots
  vioplot(d.centr[which(PlotData[[i]]$INVASION.STATUS == "native")], at = 1, add = T, border = 'deepskyblue3', col=box.cols2[1], lwd=1.5)
  vioplot(d.centr[which(PlotData[[i]]$INVASION.STATUS == "naturalized")], at = 2, add = T, border = 'gold', col=box.cols2[2], lwd=1.5)
  vioplot(d.centr[which(PlotData[[i]]$INVASION.STATUS == "invasive")], at = 3, add = T, border = 'red2', col=box.cols2[3], lwd=1.5)
  
  stat[[i]] <- list(summary(d.centr[which(PlotData[[i]]$INVASION.STATUS == "native")]),
                    summary(d.centr[which(PlotData[[i]]$INVASION.STATUS == "naturalized")]),
                    summary(d.centr[which(PlotData[[i]]$INVASION.STATUS == "invasive")]))
  names(stat[[i]]) <- c("native", "naturalized", "invasive")  
  
  if(any(1:4 == i))
  {
    #add numbers of species
    text(1,5.4, paste("N = ", No.sp[[i]][1], sep=""), cex=1)
    text(2,5.4, paste("N = ", No.sp[[i]][3], sep=""), cex=1)
    text(3,5.4, paste("N = ", No.sp[[i]][4], sep=""), cex=1)
    
    # #add results of Wilcoxon Rank Sum test
    # text(2,5, sig.code(test$natur$Padj[i]), cex=2)
    # text(3,5, sig.code(test$inv$Padj[i]), cex=2)
  }
  
  if(any(5:6 == i))
  {
    #add numbers of species
    text(1,6.4, paste("N = ", No.sp[[i]][1], sep=""), cex=1)
    text(2,6.4, paste("N = ", No.sp[[i]][3], sep=""), cex=1)
    text(3,6.4, paste("N = ", No.sp[[i]][4], sep=""), cex=1)
    
    # #add results of Wilcoxon Rank Sum test
    # text(2,5.9, sig.code(test$natur$Padj[i]), cex=2)
    # text(3,5.9, sig.code(test$inv$Padj[i]), cex=2)
  }
  
}

mtext("Distance from the centroid of native species in 3D trait space (SD)", 2, 2, outer=T, las=0, cex=0.7)
mtext("Invasion status", 1, 1.5, outer=T, las=0, cex=0.65)

dev.off()

Dist.stats.imp <- stat
Dist.stats.imp

###VIOLIN PLOTS FOR PHYLOGENETICALLY CORRECTED TRAITS---------------------------------------------------------

#cairo_pdf("Violin3.pdf", width=5.3, height = 7)
#tiff("Violin3.tif", width = 5.3, height = 7, units = "in", res=300, compression = "lzw")

#windows(7,4.8, rescale = "fixed")
svg("Violin3.svg", width=7, height = 4.8)
#cairo_pdf("Violin3.pdf", width=7, height = 4.8)
#tiff("Violin3.tif", width = 7, height = 4.8, units = "in", res=400, compression = "lzw")
layout(matrix(1:6,ncol=3, byrow=T))
par(mar=c(0.5, 0.5, 1.5, 0.2), oma=c(3, 3.5, 0.5, 0.5), mgp=c(3,0.6,0), las=1, tck=-0.027)

PlotData <- list(traitT.3Dcor, traitX.3Dcor, traitS.3Dcor, traitM.3Dcor, traitK.3Dcor, traitL.3Dcor)

# test <- list()
# test$natur <- Wilcox.cor.NN
# test$inv <- Wilcox.cor.NI

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

stat <- list()

for(i in 1:6)
{
  if(any(1:4 == i))
  {
    plot(1, 1, xlim=c(0.5,3.5), ylim=c(0,4), type='n', axes=F, xlab="", ylab="", las=1, cex.axis=0.9)
  }
  if(any(5:6 == i))
  {
    plot(1, 1, xlim=c(0.5,3.5), ylim=c(0,5), type='n', axes=F, xlab="", ylab="", las=1, cex.axis=0.9)
  }
  
  title(habitat[i], cex.main=1.1, line=0.5) 
  box(lwd=0.75)
  
  if(any(c(1,4) == i))#c(1,3,5)
  {
    axis(2, lwd=0.75)
  }
  
  if(any(4:6 == i))#5:6
  {
    axis(1, 1:3, c("native", "naturalized", "invasive"), lwd=0.75)
  }
  
  #calculate centroid of native species
  centr <- apply(PlotData[[i]][which(PlotData[[i]]$INVASION.STATUS == "native"), c(1:3)], 2, mean)
  centr <- rbind(centr, PlotData[[i]][,c(1:3)])
  
  #calculate distances of species from observed native centroid
  d.centr <- as.matrix(dist(centr))[,1]
  d.centr <- d.centr[-1]#/max(d.centr[-1])
  
  #violin plots
  vioplot(d.centr[which(PlotData[[i]]$INVASION.STATUS == "native")], at = 1, add = T, border = 'deepskyblue3', col=box.cols2[1], lwd=1.5)
  vioplot(d.centr[which(PlotData[[i]]$INVASION.STATUS == "naturalized")], at = 2, add = T, border = 'gold', col=box.cols2[2], lwd=1.5)
  vioplot(d.centr[which(PlotData[[i]]$INVASION.STATUS == "invasive")], at = 3, add = T, border = 'red2', col=box.cols2[3], lwd=1.5)
  
  stat[[i]] <- list(summary(d.centr[which(PlotData[[i]]$INVASION.STATUS == "native")]),
                    summary(d.centr[which(PlotData[[i]]$INVASION.STATUS == "naturalized")]),
                    summary(d.centr[which(PlotData[[i]]$INVASION.STATUS == "invasive")]))
  names(stat[[i]]) <- c("native", "naturalized", "invasive")  
  
  if(any(1:4 == i))
  {
    #add numbers of species
    text(1,3.9, paste("N = ", No.sp[[i]][1], sep=""), cex=1)
    text(2,3.9, paste("N = ", No.sp[[i]][3], sep=""), cex=1)
    text(3,3.9, paste("N = ", No.sp[[i]][4], sep=""), cex=1)
    
    # #add results of Wilcoxon Rank Sum test
    # text(2,3.4, sig.code(test$natur$Padj[i]), cex=2)
    # text(3,3.4, sig.code(test$inv$Padj[i]), cex=2)
  }
  
  if(any(5:6 == i))
  {
    #add numbers of species
    text(1,4.9, paste("N = ", No.sp[[i]][1], sep=""), cex=1)
    text(2,4.9, paste("N = ", No.sp[[i]][3], sep=""), cex=1)
    text(3,4.9, paste("N = ", No.sp[[i]][4], sep=""), cex=1)
    
    # #add results of Wilcoxon Rank Sum test
    # text(2,4.4, sig.code(test$natur$Padj[i]), cex=2)
    # text(3,4.4, sig.code(test$inv$Padj[i]), cex=2)
  }
  
}

mtext("Distance from the centroid of native species in 3D trait space (SD)", 2, 2, outer=T, las=0, cex=0.7)
mtext("Invasion status", 1, 1.5, outer=T, las=0, cex=0.65)

dev.off()

Dist.stats.cor <- stat
Dist.stats.cor

###VIOLIN PLOTS FOR IMPUTED AND PHYLOGENETICALLY CORRECTED TRAITS---------------------------------------------------------

#cairo_pdf("Violin4.pdf", width=5.3, height = 7)
#tiff("Violin4.tif", width = 5.3, height = 7, units = "in", res=300, compression = "lzw")

#windows(7,4.8, rescale = "fixed")
svg("Violin4.svg", width=7, height = 4.8)
#cairo_pdf("Violin4.pdf", width=7, height = 4.8)
#tiff("Violin4.tif", width = 7, height = 4.8, units = "in", res=400, compression = "lzw")
layout(matrix(1:6,ncol=3, byrow=T))
par(mar=c(0.5, 0.5, 1.5, 0.2), oma=c(3, 3.5, 0.5, 0.5), mgp=c(3,0.6,0), las=1, tck=-0.027)

PlotData <- list(traitT.3Dcor.imp, traitX.3Dcor.imp, traitS.3Dcor.imp, traitM.3Dcor.imp, traitK.3Dcor.imp, traitL.3Dcor.imp)

# test <- list()
# test$natur <- Wilcox.cor.imp.NN
# test$inv <- Wilcox.cor.imp.NI

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

stat <- list()

for(i in 1:6)
{
  if(any(1:4 == i))
  {
    plot(1, 1, xlim=c(0.5,3.5), ylim=c(0,5), type='n', axes=F, xlab="", ylab="", las=1, cex.axis=0.9)
  }
  if(any(5:6 == i))
  {
    plot(1, 1, xlim=c(0.5,3.5), ylim=c(0,5), type='n', axes=F, xlab="", ylab="", las=1, cex.axis=0.9)
  }
  
  title(habitat[i], cex.main=1.1, line=0.5) 
  box(lwd=0.75)
  
  if(any(c(1,4) == i))#c(1,3,5)
  {
    axis(2, lwd=0.75)
  }
  
  if(any(4:6 == i))#5:6
  {
    axis(1, 1:3, c("native", "naturalized", "invasive"), lwd=0.75)
  }
  
  #calculate centroid of native species
  centr <- apply(PlotData[[i]][which(PlotData[[i]]$INVASION.STATUS == "native"), c(1:3)], 2, mean)
  centr <- rbind(centr, PlotData[[i]][,c(1:3)])
  
  #calculate distances of species from observed native centroid
  d.centr <- as.matrix(dist(centr))[,1]
  d.centr <- d.centr[-1]
  
  #violin plots
  vioplot(d.centr[which(PlotData[[i]]$INVASION.STATUS == "native")], at = 1, add = T, border = 'deepskyblue3', col=box.cols2[1], lwd=1.5)
  vioplot(d.centr[which(PlotData[[i]]$INVASION.STATUS == "naturalized")], at = 2, add = T, border = 'gold', col=box.cols2[2], lwd=1.5)
  vioplot(d.centr[which(PlotData[[i]]$INVASION.STATUS == "invasive")], at = 3, add = T, border = 'red2', col=box.cols2[3], lwd=1.5)
  
  stat[[i]] <- list(summary(d.centr[which(PlotData[[i]]$INVASION.STATUS == "native")]),
                    summary(d.centr[which(PlotData[[i]]$INVASION.STATUS == "naturalized")]),
                    summary(d.centr[which(PlotData[[i]]$INVASION.STATUS == "invasive")]))
  names(stat[[i]]) <- c("native", "naturalized", "invasive")  
  
  if(any(1:4 == i))
  {
    #add numbers of species
    text(1,4.9, paste("N = ", No.sp[[i]][1], sep=""), cex=1)
    text(2,4.9, paste("N = ", No.sp[[i]][3], sep=""), cex=1)
    text(3,4.9, paste("N = ", No.sp[[i]][4], sep=""), cex=1)
    
    # #add results of Wilcoxon Rank Sum test
    # text(2,4.4, sig.code(test$natur$Padj[i]), cex=2)
    # text(3,4.4, sig.code(test$inv$Padj[i]), cex=2)
  }
  
  if(any(5:6 == i))
  {
    #add numbers of species
    text(1,4.9, paste("N = ", No.sp[[i]][1], sep=""), cex=1)
    text(2,4.9, paste("N = ", No.sp[[i]][3], sep=""), cex=1)
    text(3,4.9, paste("N = ", No.sp[[i]][4], sep=""), cex=1)
    
    # #add results of Wilcoxon Rank Sum test
    # text(2,4.4, sig.code(test$natur$Padj[i]), cex=2)
    # text(3,4.4, sig.code(test$inv$Padj[i]), cex=2)
  }
  
}

mtext("Distance from the centroid of native species in 3D trait space (SD)", 2, 2, outer=T, las=0, cex=0.7)
mtext("Invasion status", 1, 1.5, outer=T, las=0, cex=0.65)

dev.off()

Dist.stats.cor.imp <- stat
Dist.stats.cor.imp

###SOME STATISTICS---------------------------------------------------------------------------
#observed traits (NAs removed)
unlist(lapply(Dist.stats, FUN = function(x){return(x$native["Mean"])}))
unlist(lapply(Dist.stats, FUN = function(x){return(x$naturalized["Mean"])}))
unlist(lapply(Dist.stats, FUN = function(x){return(x$invasive["Mean"])}))

unlist(lapply(Dist.stats, FUN = function(x){return(x$native["Mean"])})) - unlist(lapply(Dist.stats, FUN = function(x){return(x$naturalized["Mean"])}))
unlist(lapply(Dist.stats, FUN = function(x){return(x$native["Mean"])})) - unlist(lapply(Dist.stats, FUN = function(x){return(x$invasive["Mean"])}))

#residuals (NAs removed)
max(unlist(lapply(Dist.stats.cor, FUN = function(x){return(x$native["Mean"])})))
max(unlist(lapply(Dist.stats.cor, FUN = function(x){return(x$naturalized["Mean"])})))
max(unlist(lapply(Dist.stats.cor, FUN = function(x){return(x$invasive["Mean"])})))

unlist(lapply(Dist.stats.cor, FUN = function(x){return(x$native["Mean"])})) - unlist(lapply(Dist.stats.cor, FUN = function(x){return(x$naturalized["Mean"])}))
unlist(lapply(Dist.stats.cor, FUN = function(x){return(x$native["Mean"])})) - unlist(lapply(Dist.stats.cor, FUN = function(x){return(x$invasive["Mean"])}))
