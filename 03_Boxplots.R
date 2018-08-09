############################################################################################
#                     PLOT TRAITS FOR SPECIES GROUPS IN EACH HABITAT                       #
############################################################################################

#code by Jan Divisek (2018)

#with transparency
box.cols <- c(rgb(0,191,255,100, maxColorValue = 255),
               rgb(255,255,0,100, maxColorValue = 255),
               rgb(238,0,0,100, maxColorValue = 255))

#no transparency for emf files
box.cols2 <- c(rgb(155,229,255,255, maxColorValue = 255),
              rgb(255,255,155,255, maxColorValue = 255),
              rgb(248,155,155,255, maxColorValue = 255))

point.cols2 <- c(rgb(64,205,255,255, maxColorValue = 255),
               rgb(255,240,64,255, maxColorValue = 255),
               rgb(242,64,64,255, maxColorValue = 255))

NN.TEST <- Diff.mediansNN.TEST
NI.TEST <- Diff.mediansNI.TEST

#windows(7,9)
cairo_pdf("Boxplots1.pdf", width=7, height = 9)
#tiff("Boxplots1.tif", width=7, height = 9, units="in", res=300, compression = "lzw")
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
PlotData <- list(traitT[which(traitT$INVASION.STATUS != "casual"),c(3:5,2)], 
                 traitX[which(traitX$INVASION.STATUS != "casual"),c(3:5,2)], 
                 traitS[which(traitS$INVASION.STATUS != "casual"),c(3:5,2)])

PlotData[[1]][, "INVASION.STATUS"] <- droplevels(PlotData[[1]][, "INVASION.STATUS"])
PlotData[[2]][, "INVASION.STATUS"] <- droplevels(PlotData[[2]][, "INVASION.STATUS"])
PlotData[[3]][, "INVASION.STATUS"] <- droplevels(PlotData[[3]][, "INVASION.STATUS"])

habitat <- c("Grassland and heathland vegetation",
             "Ruderal and weed vegetation",
             "Rock and scree vegetation")

for(q in c("sla.avg.mm2mg.", "HEIGHT.MAX", "Germinule"))
{
  y.range <- range(do.call(rbind.data.frame, PlotData)[, q], na.rm = T)
  y.range <- log10(y.range)
  
  for(i in 1:3)
  {
    sel <- complete.cases(PlotData[[i]][, q])
    tr <- log10(PlotData[[i]][sel, q])
    st <- PlotData[[i]]$INVASION.STATUS[sel]
    
    boxplot(tr ~ st, xaxt="n", yaxt="n", border=F, ylim=y.range, axes=F)
    box(lwd=0.75)
    
    x.point <- seq(0.62, 1.38, length.out = sum(st == "native"))
    points(sample(x.point), tr[st == "native"], pch=16, cex=0.3, col=rgb(0,191,255,150, maxColorValue = 255))
    
    x.point <- seq(1.62, 2.38, length.out = sum(st == "naturalized"))
    points(sample(x.point), tr[st == "naturalized"], pch=16, cex=0.3, col=rgb(255,215,0,150, maxColorValue = 255))
    
    x.point <- seq(2.62, 3.38, length.out = sum(st == "invasive"))
    points(sample(x.point), tr[st == "invasive"], pch=16, cex=0.3, col=rgb(238,0,0,150, maxColorValue = 255))
    
    
    boxplot(tr ~ st, outline=FALSE, add=T, axes=F, col =box.cols, lwd=0.5)
    
    if(q == "sla.avg.mm2mg.")
    {
      mtext(habitat[i], side=3, line=0.3, outer=F, font=2, cex=0.72)
    }
    
    if(i == 1)
    {
      axis(2, at=log10(c(0.01, 0.1, 1, 10, 100, 1000)), labels = c(0.01, 0.1, 1, 10, 100, 1000), 
           las=1, cex.axis=1, tck=-0.04, lwd = 0.75)
      
      if(q == "sla.avg.mm2mg.")
      {
        mtext(expression(SLA~(mm^2~mg^-1)), side=2, line=2.3, outer=F, cex=0.7)
      }
      if(q == "HEIGHT.MAX")
      {
        mtext(expression(Plant~height~(m)), side=2, line=2.3, outer=F, cex=0.7)
        mtext(expression(Trait~(log[10]~scale)), side=2, line=3.9, outer=F, cex=0.7)
      }
      if(q == "Germinule")
      {
        mtext(expression(Seed~weight~(mg)), side=2, line=2.3, outer=F, cex=0.7)
      }
    }
    
    
    if(q == "Germinule")
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
    if(q == "sla.avg.mm2mg.")
    {
      p[1] <- sig.code(NN.TEST[habitat[i], "SLA.P"])
      p[2] <- sig.code(NN.TEST[habitat[i], "SLA.Padj"])
      p[3] <- sig.code(NI.TEST[habitat[i], "SLA.P"])
      p[4] <- sig.code(NI.TEST[habitat[i], "SLA.Padj"])
    }
    if(q == "HEIGHT.MAX")
    {
      p[1] <- sig.code(NN.TEST[habitat[i], "PH.P"])
      p[2] <- sig.code(NN.TEST[habitat[i], "PH.Padj"])
      p[3] <- sig.code(NI.TEST[habitat[i], "PH.P"])
      p[4] <- sig.code(NI.TEST[habitat[i], "PH.Padj"])
    }
    if(q == "Germinule")
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
PlotData <- list(traitM[which(traitM$INVASION.STATUS != "casual"),c(3:5,2)], 
                 traitK[which(traitK$INVASION.STATUS != "casual"),c(3:5,2)], 
                 traitL[which(traitL$INVASION.STATUS != "casual"),c(3:5,2)])

PlotData[[1]][, "INVASION.STATUS"] <- droplevels(PlotData[[1]][, "INVASION.STATUS"])
PlotData[[2]][, "INVASION.STATUS"] <- droplevels(PlotData[[2]][, "INVASION.STATUS"])
PlotData[[3]][, "INVASION.STATUS"] <- droplevels(PlotData[[3]][, "INVASION.STATUS"])

habitat <- c("Wetland vegetation",
             "Scrub vegetation",
             "Forest vegetation")

for(q in c("sla.avg.mm2mg.", "HEIGHT.MAX", "Germinule"))
{
  y.range <- range(do.call(rbind.data.frame, PlotData)[, q], na.rm = T)
  y.range <- log10(y.range)
  
  if(q == "HEIGHT.MAX")
  {
    y.range[2] <- y.range[2] + (diff(y.range)*0.07)
  }
  
  for(i in 1:3)
  {
    sel <- complete.cases(PlotData[[i]][, q])
    tr <- log10(PlotData[[i]][sel, q])
    st <- PlotData[[i]]$INVASION.STATUS[sel]
    
    boxplot(tr ~ st, xaxt="n", yaxt="n", border=F, ylim=y.range, axes=F)
    box(lwd=0.75)
    
    x.point <- seq(0.62, 1.38, length.out = sum(st == "native"))
    points(sample(x.point), tr[st == "native"], pch=16, cex=0.3, col=rgb(0,191,255,150, maxColorValue = 255))
    
    x.point <- seq(1.62, 2.38, length.out = sum(st == "naturalized"))
    points(sample(x.point), tr[st == "naturalized"], pch=16, cex=0.3, col=rgb(255,215,0,150, maxColorValue = 255))
    
    x.point <- seq(2.62, 3.38, length.out = sum(st == "invasive"))
    points(sample(x.point), tr[st == "invasive"], pch=16, cex=0.3, col=rgb(238,0,0,150, maxColorValue = 255))
    
    
    boxplot(tr ~ st, outline=FALSE, add=T, axes=F, col =box.cols, lwd=0.5)
    
    if(q == "sla.avg.mm2mg.")
    {
      mtext(habitat[i], side=3, line=0.3, outer=F, font=2, cex=0.72)
    }
    
    if(i == 1)
    {
      axis(2, at=log10(c(0.01, 0.1, 1, 10, 100, 1000)), labels = c(0.01, 0.1, 1, 10, 100, 1000), 
           las=1, cex.axis=1, tck=-0.04, lwd=0.75)
      
      if(q == "sla.avg.mm2mg.")
      {
        mtext(expression(SLA~(mm^2~mg^-1)), side=2, line=2.3, outer=F, cex=0.7)
      }
      if(q == "HEIGHT.MAX")
      {
        mtext(expression(Plant~height~(m)), side=2, line=2.3, outer=F, cex=0.7)
        mtext(expression(Trait~(log[10]~scale)), side=2, line=3.9, outer=F, cex=0.7)
      }
      if(q == "Germinule")
      {
        mtext(expression(Seed~weight~(mg)), side=2, line=2.3, outer=F, cex=0.7)
      }
    }
    
    
    if(q == "Germinule")
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
    if(q == "sla.avg.mm2mg.")
    {
      p[1] <- sig.code(NN.TEST[habitat[i], "SLA.P"])
      p[2] <- sig.code(NN.TEST[habitat[i], "SLA.Padj"])
      p[3] <- sig.code(NI.TEST[habitat[i], "SLA.P"])
      p[4] <- sig.code(NI.TEST[habitat[i], "SLA.Padj"])
    }
    if(q == "HEIGHT.MAX")
    {
      p[1] <- sig.code(NN.TEST[habitat[i], "PH.P"])
      p[2] <- sig.code(NN.TEST[habitat[i], "PH.Padj"])
      p[3] <- sig.code(NI.TEST[habitat[i], "PH.P"])
      p[4] <- sig.code(NI.TEST[habitat[i], "PH.Padj"])
    }
    if(q == "Germinule")
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
#   PLOT TRAITS FOR SPECIES GROUPS IN EACH HABITAT - DATASET WITH IMPUTED TRAITS           #
############################################################################################

NN.TEST <- Diff.mediansNN.imp.TEST
NI.TEST <- Diff.mediansNI.imp.TEST

#windows(7,9, rescale = "fixed")
emf("Boxplots2.emf", width=7, height = 9)
#svg("Boxplots2.svg", width=7, height = 9)
#cairo_pdf("Boxplots2.pdf", width=7, height = 9)
#tiff("Boxplots2.tif", width=7, height = 9, units="in", res=300, compression = "lzw")
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
PlotData <- list(traitT.imp[which(traitT.imp$INVASION.STATUS != "casual"),c(3:5,2)], 
                 traitX.imp[which(traitX.imp$INVASION.STATUS != "casual"),c(3:5,2)], 
                 traitS.imp[which(traitS.imp$INVASION.STATUS != "casual"),c(3:5,2)])

PlotData[[1]][, "INVASION.STATUS"] <- droplevels(PlotData[[1]][, "INVASION.STATUS"])
PlotData[[2]][, "INVASION.STATUS"] <- droplevels(PlotData[[2]][, "INVASION.STATUS"])
PlotData[[3]][, "INVASION.STATUS"] <- droplevels(PlotData[[3]][, "INVASION.STATUS"])

habitat <- c("Grassland and heathland vegetation",
             "Ruderal and weed vegetation",
             "Rock and scree vegetation")

for(q in c("sla.avg.mm2mg.", "HEIGHT.MAX", "Germinule"))
{
  y.range <- range(do.call(rbind.data.frame, PlotData)[, q], na.rm = T)
  y.range <- log10(y.range)
  
  for(i in 1:3)
  {
    sel <- complete.cases(PlotData[[i]][, q])
    tr <- log10(PlotData[[i]][sel, q])
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
    
    if(q == "sla.avg.mm2mg.")
    {
      mtext(habitat[i], side=3, line=0.3, outer=F, font=2, cex=0.72)
    }
    
    if(i == 1)
    {
      axis(2, at=log10(c(0.01, 0.1, 1, 10, 100, 1000)), labels = c(0.01, 0.1, 1, 10, 100, 1000), 
           las=1, cex.axis=1, tck=-0.04, lwd = 0.75)
      
      if(q == "sla.avg.mm2mg.")
      {
        mtext(expression(SLA~(mm^2~mg^-1)), side=2, line=2.3, outer=F, cex=0.7)
      }
      if(q == "HEIGHT.MAX")
      {
        mtext(expression(Plant~height~(m)), side=2, line=2.3, outer=F, cex=0.7)
        mtext(expression(Trait~(log[10]~scale)), side=2, line=3.9, outer=F, cex=0.7)
      }
      if(q == "Germinule")
      {
        mtext(expression(Seed~weight~(mg)), side=2, line=2.3, outer=F, cex=0.7)
      }
    }
    
    
    if(q == "Germinule")
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
    if(q == "sla.avg.mm2mg.")
    {
      p[1] <- sig.code(NN.TEST[habitat[i], "SLA.P"])
      p[2] <- sig.code(NN.TEST[habitat[i], "SLA.Padj"])
      p[3] <- sig.code(NI.TEST[habitat[i], "SLA.P"])
      p[4] <- sig.code(NI.TEST[habitat[i], "SLA.Padj"])
    }
    if(q == "HEIGHT.MAX")
    {
      p[1] <- sig.code(NN.TEST[habitat[i], "PH.P"])
      p[2] <- sig.code(NN.TEST[habitat[i], "PH.Padj"])
      p[3] <- sig.code(NI.TEST[habitat[i], "PH.P"])
      p[4] <- sig.code(NI.TEST[habitat[i], "PH.Padj"])
    }
    if(q == "Germinule")
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
PlotData <- list(traitM.imp[which(traitM.imp$INVASION.STATUS != "casual"),c(3:5,2)], 
                 traitK.imp[which(traitK.imp$INVASION.STATUS != "casual"),c(3:5,2)], 
                 traitL.imp[which(traitL.imp$INVASION.STATUS != "casual"),c(3:5,2)])

PlotData[[1]][, "INVASION.STATUS"] <- droplevels(PlotData[[1]][, "INVASION.STATUS"])
PlotData[[2]][, "INVASION.STATUS"] <- droplevels(PlotData[[2]][, "INVASION.STATUS"])
PlotData[[3]][, "INVASION.STATUS"] <- droplevels(PlotData[[3]][, "INVASION.STATUS"])

habitat <- c("Wetland vegetation",
             "Scrub vegetation",
             "Forest vegetation")

for(q in c("sla.avg.mm2mg.", "HEIGHT.MAX", "Germinule"))
{
  y.range <- range(do.call(rbind.data.frame, PlotData)[, q], na.rm = T)
  y.range <- log10(y.range)
  
  if(q == "HEIGHT.MAX")
  {
    y.range[2] <- y.range[2] + (diff(y.range)*0.07)
  }
  
  for(i in 1:3)
  {
    sel <- complete.cases(PlotData[[i]][, q])
    tr <- log10(PlotData[[i]][sel, q])
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
    
    if(q == "sla.avg.mm2mg.")
    {
      mtext(habitat[i], side=3, line=0.3, outer=F, font=2, cex=0.72)
    }
    
    if(i == 1)
    {
      axis(2, at=log10(c(0.01, 0.1, 1, 10, 100, 1000)), labels = c(0.01, 0.1, 1, 10, 100, 1000), 
           las=1, cex.axis=1, tck=-0.04, lwd=0.75)
      
      if(q == "sla.avg.mm2mg.")
      {
        mtext(expression(SLA~(mm^2~mg^-1)), side=2, line=2.3, outer=F, cex=0.7)
      }
      if(q == "HEIGHT.MAX")
      {
        mtext(expression(Plant~height~(m)), side=2, line=2.3, outer=F, cex=0.7)
        mtext(expression(Trait~(log[10]~scale)), side=2, line=3.9, outer=F, cex=0.7)
      }
      if(q == "Germinule")
      {
        mtext(expression(Seed~weight~(mg)), side=2, line=2.3, outer=F, cex=0.7)
      }
    }
    
    
    if(q == "Germinule")
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
    if(q == "sla.avg.mm2mg.")
    {
      p[1] <- sig.code(NN.TEST[habitat[i], "SLA.P"])
      p[2] <- sig.code(NN.TEST[habitat[i], "SLA.Padj"])
      p[3] <- sig.code(NI.TEST[habitat[i], "SLA.P"])
      p[4] <- sig.code(NI.TEST[habitat[i], "SLA.Padj"])
    }
    if(q == "HEIGHT.MAX")
    {
      p[1] <- sig.code(NN.TEST[habitat[i], "PH.P"])
      p[2] <- sig.code(NN.TEST[habitat[i], "PH.Padj"])
      p[3] <- sig.code(NI.TEST[habitat[i], "PH.P"])
      p[4] <- sig.code(NI.TEST[habitat[i], "PH.Padj"])
    }
    if(q == "Germinule")
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

