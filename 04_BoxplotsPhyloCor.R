############################################################################################
#      PLOT PHYLOGENETICALLY CORRECTED TRAITS FOR SPECIES GROUPS IN EACH HABITAT           #
############################################################################################

#code by Jan Divisek (2018)

library(dplyr)

#no transparency for emf files
box.cols2 <- c(rgb(155,229,255,255, maxColorValue = 255),
               rgb(255,255,155,255, maxColorValue = 255),
               rgb(248,155,155,255, maxColorValue = 255))

point.cols2 <- c(rgb(64,205,255,255, maxColorValue = 255),
                 rgb(255,240,64,255, maxColorValue = 255),
                 rgb(242,64,64,255, maxColorValue = 255))

NN.TEST <- Diff.mediansNN.cor.TEST
NI.TEST <- Diff.mediansNI.cor.TEST

#windows(7,9)
emf("Boxplots3.emf", width=7, height = 9)
#svg("Boxplots3.svg", width=7, height = 9)
#cairo_pdf("Boxplots3.pdf", width=7, height = 9)
#tiff("Boxplots3.tif", width=7, height = 9, units="in", res=300, compression = "lzw")
layout.matrix <- matrix(c(rep(1:3,2),
                          rep(4:6,2),
                          rep(7:9,2),
                          rep(19:21,1),
                          rep(10:12,2),
                          rep(13:15,2),
                          rep(16:18,2)), ncol=3, nrow=13 ,byrow = T)

layout(layout.matrix)
par(mar=c(0.3, 0.3, 0.3, 0.3), oma=c(3,5,1.5,0.2), mgp=c(3,0.5,0))

###PLOT FIRST GROUP OF HABITATS (T, X, S)--------------------------------------------------------
PlotData <- list()

PlotData[[1]] <- bind_rows(traitT.phy$PlantHeight[which(traitT.phy$PlantHeight$INVASION.STATUS != "casual"),c(7,10)], 
                           traitT.phy$SLA[which(traitT.phy$SLA$INVASION.STATUS != "casual"),c(7,10)], 
                           traitT.phy$Germinule[which(traitT.phy$Germinule$INVASION.STATUS != "casual"),c(7,10)])

PlotData[[2]] <- bind_rows(traitX.phy$PlantHeight[which(traitX.phy$PlantHeight$INVASION.STATUS != "casual"),c(7,10)], 
                           traitX.phy$SLA[which(traitX.phy$SLA$INVASION.STATUS != "casual"),c(7,10)], 
                           traitX.phy$Germinule[which(traitX.phy$Germinule$INVASION.STATUS != "casual"),c(7,10)])

PlotData[[3]] <- bind_rows(traitS.phy$PlantHeight[which(traitS.phy$PlantHeight$INVASION.STATUS != "casual"),c(7,10)], 
                           traitS.phy$SLA[which(traitS.phy$SLA$INVASION.STATUS != "casual"),c(7,10)], 
                           traitS.phy$Germinule[which(traitS.phy$Germinule$INVASION.STATUS != "casual"),c(7,10)])

PlotData[[1]][, "INVASION.STATUS"] <- droplevels(PlotData[[1]][, "INVASION.STATUS"])
PlotData[[2]][, "INVASION.STATUS"] <- droplevels(PlotData[[2]][, "INVASION.STATUS"])
PlotData[[3]][, "INVASION.STATUS"] <- droplevels(PlotData[[3]][, "INVASION.STATUS"])

habitat <- c("Grassland and heathland vegetation",
             "Ruderal and weed vegetation",
             "Rock and scree vegetation")

for(q in c("sla.avg.mm2mg.cor", "HEIGHT.MAX.cor", "Germinule.cor"))
{
  y.range <- range(do.call(rbind.data.frame, PlotData)[, q], na.rm = T)
  
  for(i in 1:3)
  {
    sel <- complete.cases(PlotData[[i]][, q])
    tr <- PlotData[[i]][sel, q]
    st <- PlotData[[i]]$INVASION.STATUS[sel]
    
    boxplot(tr ~ st, xaxt="n", yaxt="n", border=F, ylim=y.range, axes=F)
    box(lwd=0.75)
    
    boxplot(tr ~ st, outline=FALSE, add=T, axes=F, col =box.cols2, lwd=0.5, border=NA)
    
    x.point <- seq(0.62, 1.38, length.out = sum(st == "native"))
    points(sample(x.point), tr[st == "native"], pch=16, cex=0.3, col=point.cols2[1])
    
    x.point <- seq(1.62, 2.38, length.out = sum(st == "naturalized"))
    points(sample(x.point), tr[st == "naturalized"], pch=16, cex=0.3, col=point.cols2[2])
    
    x.point <- seq(2.62, 3.38, length.out = sum(st == "invasive"))
    points(sample(x.point), tr[st == "invasive"], pch=16, cex=0.3, col=point.cols2[3])
    
    boxplot(tr ~ st, outline=FALSE, add=T, axes=F, col =NA, lwd=0.5)
    
    if(q == "sla.avg.mm2mg.cor")
    {
      mtext(habitat[i], side=3, line=0.3, outer=F, font=2, cex=0.72)
    }
    
    if(i == 1)
    {
      axis(2, las=1, cex.axis=1, tck=-0.04, lwd=0.75)
      
      if(q == "sla.avg.mm2mg.cor")
      {
        mtext("SLA", side=2, line=2.3, outer=F, cex=0.7)
      }
      if(q == "HEIGHT.MAX.cor")
      {
        mtext("Plant height", side=2, line=2.3, outer=F, cex=0.7)
        mtext("Residuals of phylogenetic models", side=2, line=3.9, outer=F, cex=0.7)
      }
      if(q == "Germinule.cor")
      {
        mtext("Seed weight", side=2, line=2.3, outer=F, cex=0.7)
      }
    }
    
    
    if(q == "Germinule.cor")
    {
      axis(1, at=1:3, labels = c("native", "naturalized", "invasive"), tck=-0.05, cex.axis=1, lwd=0.75)
      
      if(i == 2)
      {
        mtext("Invasion status", side=1, line=1.7, outer=F, cex=0.7)
      }
    }
    
    text(1, y.range[2]-(diff(y.range)*0.05), paste("N =", sum(st == "native"), sep=" "), cex=1)
    text(2, y.range[2]-(diff(y.range)*0.05), paste("N =", sum(st == "naturalized"), sep=" "), cex=1)
    text(3, y.range[2]-(diff(y.range)*0.05), paste("N =", sum(st == "invasive"), sep=" "), cex=1)
    
    
    ##add significance levels
    p <- vector("character")
    if(q == "sla.avg.mm2mg.cor")
    {
      p[1] <- sig.code(NN.TEST[habitat[i], "SLA.P"])
      p[2] <- sig.code(NN.TEST[habitat[i], "SLA.Padj"])
      p[3] <- sig.code(NI.TEST[habitat[i], "SLA.P"])
      p[4] <- sig.code(NI.TEST[habitat[i], "SLA.Padj"])
    }
    if(q == "HEIGHT.MAX.cor")
    {
      p[1] <- sig.code(NN.TEST[habitat[i], "PH.P"])
      p[2] <- sig.code(NN.TEST[habitat[i], "PH.Padj"])
      p[3] <- sig.code(NI.TEST[habitat[i], "PH.P"])
      p[4] <- sig.code(NI.TEST[habitat[i], "PH.Padj"])
    }
    if(q == "Germinule.cor")
    {
      p[1] <- sig.code(NN.TEST[habitat[i], "Germ.P"])
      p[2] <- sig.code(NN.TEST[habitat[i], "Germ.Padj"])
      p[3] <- sig.code(NI.TEST[habitat[i], "Germ.P"])
      p[4] <- sig.code(NI.TEST[habitat[i], "Germ.Padj"])
    }
    
    
    #text(2, y.range[1]+(diff(y.range)*0.1), labels = p[1], col="gray", font=2, cex=2)
    text(2, y.range[1]+(diff(y.range)*0.05), labels = p[2], col="black", font=2, cex=2)
    
    #text(3, y.range[1]+(diff(y.range)*0.1), labels = p[3], col="gray", font=2, cex=2)
    text(3, y.range[1]+(diff(y.range)*0.05), labels = p[4], col="black", font=2, cex=2)
    
  }
  
}

###PLOT SECOND GROUP OF HABITATS (M, K, L)--------------------------------------------------------
PlotData <- list()

PlotData[[1]] <- bind_rows(traitM.phy$PlantHeight[which(traitM.phy$PlantHeight$INVASION.STATUS != "casual"),c(7,10)], 
                           traitM.phy$SLA[which(traitM.phy$SLA$INVASION.STATUS != "casual"),c(7,10)], 
                           traitM.phy$Germinule[which(traitM.phy$Germinule$INVASION.STATUS != "casual"),c(7,10)])

PlotData[[2]] <- bind_rows(traitK.phy$PlantHeight[which(traitK.phy$PlantHeight$INVASION.STATUS != "casual"),c(7,10)], 
                           traitK.phy$SLA[which(traitK.phy$SLA$INVASION.STATUS != "casual"),c(7,10)], 
                           traitK.phy$Germinule[which(traitK.phy$Germinule$INVASION.STATUS != "casual"),c(7,10)])

PlotData[[3]] <- bind_rows(traitL.phy$PlantHeight[which(traitL.phy$PlantHeight$INVASION.STATUS != "casual"),c(7,10)], 
                           traitL.phy$SLA[which(traitL.phy$SLA$INVASION.STATUS != "casual"),c(7,10)], 
                           traitL.phy$Germinule[which(traitL.phy$Germinule$INVASION.STATUS != "casual"),c(7,10)])

PlotData[[1]][, "INVASION.STATUS"] <- droplevels(PlotData[[1]][, "INVASION.STATUS"])
PlotData[[2]][, "INVASION.STATUS"] <- droplevels(PlotData[[2]][, "INVASION.STATUS"])
PlotData[[3]][, "INVASION.STATUS"] <- droplevels(PlotData[[3]][, "INVASION.STATUS"])

habitat <- c("Wetland vegetation",
             "Scrub vegetation",
             "Forest vegetation")

for(q in c("sla.avg.mm2mg.cor", "HEIGHT.MAX.cor", "Germinule.cor"))
{
  y.range <- range(do.call(rbind.data.frame, PlotData)[, q], na.rm = T)
  
  for(i in 1:3)
  {
    sel <- complete.cases(PlotData[[i]][, q])
    tr <- PlotData[[i]][sel, q]
    st <- PlotData[[i]]$INVASION.STATUS[sel]
    
    boxplot(tr ~ st, xaxt="n", yaxt="n", border=F, ylim=y.range, axes=F)
    box(lwd=0.75)
    
    boxplot(tr ~ st, outline=FALSE, add=T, axes=F, col =box.cols2, lwd=0.5, border=NA)
    
    x.point <- seq(0.62, 1.38, length.out = sum(st == "native"))
    points(sample(x.point), tr[st == "native"], pch=16, cex=0.3, col=point.cols2[1])
    
    x.point <- seq(1.62, 2.38, length.out = sum(st == "naturalized"))
    points(sample(x.point), tr[st == "naturalized"], pch=16, cex=0.3, col=point.cols2[2])
    
    x.point <- seq(2.62, 3.38, length.out = sum(st == "invasive"))
    points(sample(x.point), tr[st == "invasive"], pch=16, cex=0.3, col=point.cols2[3])
    
    boxplot(tr ~ st, outline=FALSE, add=T, axes=F, col =NA, lwd=0.5)
    
    if(q == "sla.avg.mm2mg.cor")
    {
      mtext(habitat[i], side=3, line=0.3, outer=F, font=2, cex=0.72)
    }
    
    if(i == 1)
    {
      axis(2, las=1, cex.axis=1, tck=-0.04, lwd=0.75)
      
      if(q == "sla.avg.mm2mg.cor")
      {
        mtext("SLA", side=2, line=2.3, outer=F, cex=0.7)
      }
      if(q == "HEIGHT.MAX.cor")
      {
        mtext("Plant height", side=2, line=2.3, outer=F, cex=0.7)
        mtext("Residuals of phylogenetic models", side=2, line=3.9, outer=F, cex=0.7)
      }
      if(q == "Germinule.cor")
      {
        mtext("Seed weight", side=2, line=2.3, outer=F, cex=0.7)
      }
    }
    
    
    if(q == "Germinule.cor")
    {
      axis(1, at=1:3, labels = c("native", "naturalized", "invasive"), tck=-0.05, cex.axis=1, lwd=0.75)
      
      if(i == 2)
      {
        mtext("Invasion status", side=1, line=1.7, outer=F, cex=0.7)
      }
    }
    
    text(1, y.range[2]-(diff(y.range)*0.05), paste("N =", sum(st == "native"), sep=" "), cex=1)
    text(2, y.range[2]-(diff(y.range)*0.05), paste("N =", sum(st == "naturalized"), sep=" "), cex=1)
    text(3, y.range[2]-(diff(y.range)*0.05), paste("N =", sum(st == "invasive"), sep=" "), cex=1)
    
    ##add significance levels
    p <- vector("character")
    if(q == "sla.avg.mm2mg.cor")
    {
      p[1] <- sig.code(NN.TEST[habitat[i], "SLA.P"])
      p[2] <- sig.code(NN.TEST[habitat[i], "SLA.Padj"])
      p[3] <- sig.code(NI.TEST[habitat[i], "SLA.P"])
      p[4] <- sig.code(NI.TEST[habitat[i], "SLA.Padj"])
    }
    if(q == "HEIGHT.MAX.cor")
    {
      p[1] <- sig.code(NN.TEST[habitat[i], "PH.P"])
      p[2] <- sig.code(NN.TEST[habitat[i], "PH.Padj"])
      p[3] <- sig.code(NI.TEST[habitat[i], "PH.P"])
      p[4] <- sig.code(NI.TEST[habitat[i], "PH.Padj"])
    }
    if(q == "Germinule.cor")
    {
      p[1] <- sig.code(NN.TEST[habitat[i], "Germ.P"])
      p[2] <- sig.code(NN.TEST[habitat[i], "Germ.Padj"])
      p[3] <- sig.code(NI.TEST[habitat[i], "Germ.P"])
      p[4] <- sig.code(NI.TEST[habitat[i], "Germ.Padj"])
    }
    
    
    #text(2, y.range[1]+(diff(y.range)*0.1), labels = p[1], col="gray", font=2, cex=2)
    text(2, y.range[1]+(diff(y.range)*0.05), labels = p[2], col="black", font=2, cex=2)
    
    #text(3, y.range[1]+(diff(y.range)*0.1), labels = p[3], col="gray", font=2, cex=2)
    text(3, y.range[1]+(diff(y.range)*0.05), labels = p[4], col="black", font=2, cex=2)
  }
  
}

dev.off()

############################################################################################
#      PLOT PHYLOGENETICALLY CORRECTED TRAITS FOR SPECIES GROUPS IN EACH HABITAT           #
#                             DATASET WITH IMPUTED TRAITS                                  #
############################################################################################

NN.TEST <- Diff.mediansNN.cor.imp.TEST
NI.TEST <- Diff.mediansNI.cor.imp.TEST

#windows(7,9)
emf("Boxplots4.emf", width=7, height = 9)
#svg("Boxplots4.svg", width=7, height = 9)
#cairo_pdf("Boxplots4.pdf", width=7, height = 9)
#tiff("Boxplots4.tif", width=7, height = 9, units="in", res=300, compression = "lzw")
layout.matrix <- matrix(c(rep(1:3,2),
                          rep(4:6,2),
                          rep(7:9,2),
                          rep(19:21,1),
                          rep(10:12,2),
                          rep(13:15,2),
                          rep(16:18,2)), ncol=3, nrow=13 ,byrow = T)

layout(layout.matrix)
par(mar=c(0.3, 0.3, 0.3, 0.3), oma=c(3,5,1.5,0.2), mgp=c(3,0.5,0))

###PLOT FIRST GROUP OF HABITATS (T, X, S)--------------------------------------------------------
PlotData <- list()

PlotData[[1]] <- bind_rows(traitT.imp.phy$PlantHeight[which(traitT.imp.phy$PlantHeight$INVASION.STATUS != "casual"),c(7,10)], 
                           traitT.imp.phy$SLA[which(traitT.imp.phy$SLA$INVASION.STATUS != "casual"),c(7,10)], 
                           traitT.imp.phy$Germinule[which(traitT.imp.phy$Germinule$INVASION.STATUS != "casual"),c(7,10)])

PlotData[[2]] <- bind_rows(traitX.imp.phy$PlantHeight[which(traitX.imp.phy$PlantHeight$INVASION.STATUS != "casual"),c(7,10)], 
                           traitX.imp.phy$SLA[which(traitX.imp.phy$SLA$INVASION.STATUS != "casual"),c(7,10)], 
                           traitX.imp.phy$Germinule[which(traitX.imp.phy$Germinule$INVASION.STATUS != "casual"),c(7,10)])

PlotData[[3]] <- bind_rows(traitS.imp.phy$PlantHeight[which(traitS.imp.phy$PlantHeight$INVASION.STATUS != "casual"),c(7,10)], 
                           traitS.imp.phy$SLA[which(traitS.imp.phy$SLA$INVASION.STATUS != "casual"),c(7,10)], 
                           traitS.imp.phy$Germinule[which(traitS.imp.phy$Germinule$INVASION.STATUS != "casual"),c(7,10)])

PlotData[[1]][, "INVASION.STATUS"] <- droplevels(PlotData[[1]][, "INVASION.STATUS"])
PlotData[[2]][, "INVASION.STATUS"] <- droplevels(PlotData[[2]][, "INVASION.STATUS"])
PlotData[[3]][, "INVASION.STATUS"] <- droplevels(PlotData[[3]][, "INVASION.STATUS"])

habitat <- c("Grassland and heathland vegetation",
             "Ruderal and weed vegetation",
             "Rock and scree vegetation")

for(q in c("sla.avg.mm2mg.cor", "HEIGHT.MAX.cor", "Germinule.cor"))
{
  y.range <- range(do.call(rbind.data.frame, PlotData)[, q], na.rm = T)
  
  for(i in 1:3)
  {
    sel <- complete.cases(PlotData[[i]][, q])
    tr <- PlotData[[i]][sel, q]
    st <- PlotData[[i]]$INVASION.STATUS[sel]
    
    boxplot(tr ~ st, xaxt="n", yaxt="n", border=F, ylim=y.range, axes=F)
    box(lwd=0.75)
    
    boxplot(tr ~ st, outline=FALSE, add=T, axes=F, col =box.cols2, lwd=0.5, border=NA)
    
    x.point <- seq(0.62, 1.38, length.out = sum(st == "native"))
    points(sample(x.point), tr[st == "native"], pch=16, cex=0.3, col=point.cols2[1])
    
    x.point <- seq(1.62, 2.38, length.out = sum(st == "naturalized"))
    points(sample(x.point), tr[st == "naturalized"], pch=16, cex=0.3, col=point.cols2[2])
    
    x.point <- seq(2.62, 3.38, length.out = sum(st == "invasive"))
    points(sample(x.point), tr[st == "invasive"], pch=16, cex=0.3, col=point.cols2[3])
    
    boxplot(tr ~ st, outline=FALSE, add=T, axes=F, col =NA, lwd=0.5)
    
    if(q == "sla.avg.mm2mg.cor")
    {
      mtext(habitat[i], side=3, line=0.3, outer=F, font=2, cex=0.72)
    }
    
    if(i == 1)
    {
      axis(2, las=1, cex.axis=1, tck=-0.04, lwd=0.75)
      
      if(q == "sla.avg.mm2mg.cor")
      {
        mtext("SLA", side=2, line=2.3, outer=F, cex=0.7)
      }
      if(q == "HEIGHT.MAX.cor")
      {
        mtext("Plant height", side=2, line=2.3, outer=F, cex=0.7)
        mtext("Residuals of phylogenetic models", side=2, line=3.9, outer=F, cex=0.7)
      }
      if(q == "Germinule.cor")
      {
        mtext("Seed weight", side=2, line=2.3, outer=F, cex=0.7)
      }
    }
    
    
    if(q == "Germinule.cor")
    {
      axis(1, at=1:3, labels = c("native", "naturalized", "invasive"), tck=-0.05, cex.axis=1, lwd=0.75)
      
      if(i == 2)
      {
        mtext("Invasion status", side=1, line=1.7, outer=F, cex=0.7)
      }
    }
    
    text(1, y.range[2]-(diff(y.range)*0.05), paste("N =", sum(st == "native"), sep=" "), cex=1)
    text(2, y.range[2]-(diff(y.range)*0.05), paste("N =", sum(st == "naturalized"), sep=" "), cex=1)
    text(3, y.range[2]-(diff(y.range)*0.05), paste("N =", sum(st == "invasive"), sep=" "), cex=1)
    
    
    ##add significance levels
    p <- vector("character")
    if(q == "sla.avg.mm2mg.cor")
    {
      p[1] <- sig.code(NN.TEST[habitat[i], "SLA.P"])
      p[2] <- sig.code(NN.TEST[habitat[i], "SLA.Padj"])
      p[3] <- sig.code(NI.TEST[habitat[i], "SLA.P"])
      p[4] <- sig.code(NI.TEST[habitat[i], "SLA.Padj"])
    }
    if(q == "HEIGHT.MAX.cor")
    {
      p[1] <- sig.code(NN.TEST[habitat[i], "PH.P"])
      p[2] <- sig.code(NN.TEST[habitat[i], "PH.Padj"])
      p[3] <- sig.code(NI.TEST[habitat[i], "PH.P"])
      p[4] <- sig.code(NI.TEST[habitat[i], "PH.Padj"])
    }
    if(q == "Germinule.cor")
    {
      p[1] <- sig.code(NN.TEST[habitat[i], "Germ.P"])
      p[2] <- sig.code(NN.TEST[habitat[i], "Germ.Padj"])
      p[3] <- sig.code(NI.TEST[habitat[i], "Germ.P"])
      p[4] <- sig.code(NI.TEST[habitat[i], "Germ.Padj"])
    }
    
    
    #text(2, y.range[1]+(diff(y.range)*0.1), labels = p[1], col="gray", font=2, cex=2)
    text(2, y.range[1]+(diff(y.range)*0.05), labels = p[2], col="black", font=2, cex=2)
    
    #text(3, y.range[1]+(diff(y.range)*0.1), labels = p[3], col="gray", font=2, cex=2)
    text(3, y.range[1]+(diff(y.range)*0.05), labels = p[4], col="black", font=2, cex=2)
    
  }
  
}

###PLOT SECOND GROUP OF HABITATS (M, K, L)--------------------------------------------------------
PlotData <- list()

PlotData[[1]] <- bind_rows(traitM.imp.phy$PlantHeight[which(traitM.imp.phy$PlantHeight$INVASION.STATUS != "casual"),c(7,10)], 
                           traitM.imp.phy$SLA[which(traitM.imp.phy$SLA$INVASION.STATUS != "casual"),c(7,10)], 
                           traitM.imp.phy$Germinule[which(traitM.imp.phy$Germinule$INVASION.STATUS != "casual"),c(7,10)])

PlotData[[2]] <- bind_rows(traitK.imp.phy$PlantHeight[which(traitK.imp.phy$PlantHeight$INVASION.STATUS != "casual"),c(7,10)], 
                           traitK.imp.phy$SLA[which(traitK.imp.phy$SLA$INVASION.STATUS != "casual"),c(7,10)], 
                           traitK.imp.phy$Germinule[which(traitK.imp.phy$Germinule$INVASION.STATUS != "casual"),c(7,10)])

PlotData[[3]] <- bind_rows(traitL.imp.phy$PlantHeight[which(traitL.imp.phy$PlantHeight$INVASION.STATUS != "casual"),c(7,10)], 
                           traitL.imp.phy$SLA[which(traitL.imp.phy$SLA$INVASION.STATUS != "casual"),c(7,10)], 
                           traitL.imp.phy$Germinule[which(traitL.imp.phy$Germinule$INVASION.STATUS != "casual"),c(7,10)])

PlotData[[1]][, "INVASION.STATUS"] <- droplevels(PlotData[[1]][, "INVASION.STATUS"])
PlotData[[2]][, "INVASION.STATUS"] <- droplevels(PlotData[[2]][, "INVASION.STATUS"])
PlotData[[3]][, "INVASION.STATUS"] <- droplevels(PlotData[[3]][, "INVASION.STATUS"])

habitat <- c("Wetland vegetation",
             "Scrub vegetation",
             "Forest vegetation")

for(q in c("sla.avg.mm2mg.cor", "HEIGHT.MAX.cor", "Germinule.cor"))
{
  y.range <- range(do.call(rbind.data.frame, PlotData)[, q], na.rm = T)
  
  for(i in 1:3)
  {
    sel <- complete.cases(PlotData[[i]][, q])
    tr <- PlotData[[i]][sel, q]
    st <- PlotData[[i]]$INVASION.STATUS[sel]
    
    boxplot(tr ~ st, xaxt="n", yaxt="n", border=F, ylim=y.range, axes=F)
    box(lwd=0.75)
    
    boxplot(tr ~ st, outline=FALSE, add=T, axes=F, col =box.cols2, lwd=0.5, border=NA)
    
    x.point <- seq(0.62, 1.38, length.out = sum(st == "native"))
    points(sample(x.point), tr[st == "native"], pch=16, cex=0.3, col=point.cols2[1])
    
    x.point <- seq(1.62, 2.38, length.out = sum(st == "naturalized"))
    points(sample(x.point), tr[st == "naturalized"], pch=16, cex=0.3, col=point.cols2[2])
    
    x.point <- seq(2.62, 3.38, length.out = sum(st == "invasive"))
    points(sample(x.point), tr[st == "invasive"], pch=16, cex=0.3, col=point.cols2[3])
    
    boxplot(tr ~ st, outline=FALSE, add=T, axes=F, col =NA, lwd=0.5)
    
    if(q == "sla.avg.mm2mg.cor")
    {
      mtext(habitat[i], side=3, line=0.3, outer=F, font=2, cex=0.72)
    }
    
    if(i == 1)
    {
      axis(2, las=1, cex.axis=1, tck=-0.04, lwd=0.75)
      
      if(q == "sla.avg.mm2mg.cor")
      {
        mtext("SLA", side=2, line=2.3, outer=F, cex=0.7)
      }
      if(q == "HEIGHT.MAX.cor")
      {
        mtext("Plant height", side=2, line=2.3, outer=F, cex=0.7)
        mtext("Residuals of phylogenetic models", side=2, line=3.9, outer=F, cex=0.7)
      }
      if(q == "Germinule.cor")
      {
        mtext("Seed weight", side=2, line=2.3, outer=F, cex=0.7)
      }
    }
    
    
    if(q == "Germinule.cor")
    {
      axis(1, at=1:3, labels = c("native", "naturalized", "invasive"), tck=-0.05, cex.axis=1, lwd=0.75)
      
      if(i == 2)
      {
        mtext("Invasion status", side=1, line=1.7, outer=F, cex=0.7)
      }
    }
    
    text(1, y.range[2]-(diff(y.range)*0.05), paste("N =", sum(st == "native"), sep=" "), cex=1)
    text(2, y.range[2]-(diff(y.range)*0.05), paste("N =", sum(st == "naturalized"), sep=" "), cex=1)
    text(3, y.range[2]-(diff(y.range)*0.05), paste("N =", sum(st == "invasive"), sep=" "), cex=1)
    
    ##add significance levels
    p <- vector("character")
    if(q == "sla.avg.mm2mg.cor")
    {
      p[1] <- sig.code(NN.TEST[habitat[i], "SLA.P"])
      p[2] <- sig.code(NN.TEST[habitat[i], "SLA.Padj"])
      p[3] <- sig.code(NI.TEST[habitat[i], "SLA.P"])
      p[4] <- sig.code(NI.TEST[habitat[i], "SLA.Padj"])
    }
    if(q == "HEIGHT.MAX.cor")
    {
      p[1] <- sig.code(NN.TEST[habitat[i], "PH.P"])
      p[2] <- sig.code(NN.TEST[habitat[i], "PH.Padj"])
      p[3] <- sig.code(NI.TEST[habitat[i], "PH.P"])
      p[4] <- sig.code(NI.TEST[habitat[i], "PH.Padj"])
    }
    if(q == "Germinule.cor")
    {
      p[1] <- sig.code(NN.TEST[habitat[i], "Germ.P"])
      p[2] <- sig.code(NN.TEST[habitat[i], "Germ.Padj"])
      p[3] <- sig.code(NI.TEST[habitat[i], "Germ.P"])
      p[4] <- sig.code(NI.TEST[habitat[i], "Germ.Padj"])
    }
    
    
    #text(2, y.range[1]+(diff(y.range)*0.1), labels = p[1], col="gray", font=2, cex=2)
    text(2, y.range[1]+(diff(y.range)*0.05), labels = p[2], col="black", font=2, cex=2)
    
    #text(3, y.range[1]+(diff(y.range)*0.1), labels = p[3], col="gray", font=2, cex=2)
    text(3, y.range[1]+(diff(y.range)*0.05), labels = p[4], col="black", font=2, cex=2)
  }
  
}

dev.off()
