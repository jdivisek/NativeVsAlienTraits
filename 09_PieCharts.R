######################################################################################################
#                   PLOT NUMBERS OF SPECIES IN OCTANTS OF THE TRAIT SPACE                            #
######################################################################################################

#read coordinates of cube vertices
cube <- read.delim("https://www.dropbox.com/s/ujb7lwqtdfmaibn/cube_vertices.txt?raw=1", header=T)

habitat <- c("Grassland and heathland vegetation",
             "Ruderal and weed vegetation",
             "Rock and scree vegetation",
             "Wetland vegetation",
             "Scrub vegetation",
             "Forest vegetation")

col2rgb("red2")

pie.cols <- c(rgb(0,154,205, maxColorValue = 255), 
              rgb(255,255,0, maxColorValue = 255),
              rgb(238,0,0, maxColorValue = 255))

###PIE CHARTS----------------------------------------------------------------------------

##assemble data
PlotData <- list(traitT.3D, traitX.3D, traitS.3D, traitM.3D, traitK.3D, traitL.3D)

#svg("Pie1.svg", width=7, height = 4.8)
cairo_pdf("Pie1.pdf", width=7, height = 4.8)
#tiff("Pie1.tif", width = 7, height = 4.8, units = "in", res=400, compression = "lzw")

# split.screen(rbind(c(0, 0.495, 0.66667, 0.97),
#                    c(0.505, 1, 0.66667, 0.97),
#                    c(0, 0.495, 0.33334, 0.63667),
#                    c(0.505, 1, 0.33334, 0.63667),
#                    c(0, 0.495, 0.00001, 0.30334),
#                    c(0.505, 1, 0.00001, 0.30334)), erase = T)

split.screen(rbind(c(0.0001, 0.3334, 0.515, 0.97),
                   c(0.3334, 0.6667, 0.515, 0.97),
                   c(0.6667, 1, 0.515, 0.97),
                   c(0, 0.3334, 0, 0.455),
                   c(0.3334, 0.6667, 0, 0.455),
                   c(0.6667, 1, 0, 0.455)), erase = T)

stat <- list()

for(i in 1:6)
{
  #calculate centroid of native species
  
  centr <- apply(PlotData[[i]][which(PlotData[[i]]$INVASION.STATUS == "native"), c(1:3)], 2, mean)
  
  coord <- PlotData[[i]]
  pie.data <- list()
  
  for(q in 1:8)
  {
    #X axis = SLA
    #Y axis = Plant height
    #Z axis = Seed weight
    
    if(q == 1)
    {
      pie.data[[q]] <- table(coord[which(coord$SLA > centr["SLA"] & coord$`Plant height` > centr["Plant height"] & coord$`Germinule weight` > centr["Germinule weight"]), 4])[-2]
    }
    if(q == 2)
    {
      pie.data[[q]] <- table(coord[which(coord$SLA > centr["SLA"] & coord$`Plant height` > centr["Plant height"] & coord$`Germinule weight` < centr["Germinule weight"]), 4])[-2]
    }
    if(q == 3)
    {
      pie.data[[q]] <- table(coord[which(coord$SLA < centr["SLA"] & coord$`Plant height` > centr["Plant height"] & coord$`Germinule weight` < centr["Germinule weight"]), 4])[-2]
    }
    if(q == 4)
    {
      pie.data[[q]] <- table(coord[which(coord$SLA < centr["SLA"] & coord$`Plant height` > centr["Plant height"] & coord$`Germinule weight` > centr["Germinule weight"]), 4])[-2]
    }
    if(q == 5)
    {      
      pie.data[[q]] <- table(coord[which(coord$SLA > centr["SLA"] & coord$`Plant height` < centr["Plant height"] & coord$`Germinule weight` > centr["Germinule weight"]), 4])[-2]
    }
    if(q == 6)
    {      
      pie.data[[q]] <- table(coord[which(coord$SLA > centr["SLA"] & coord$`Plant height` < centr["Plant height"] & coord$`Germinule weight` < centr["Germinule weight"]), 4])[-2]
    }
    if(q == 7)
    {
      pie.data[[q]] <- table(coord[which(coord$SLA < centr["SLA"] & coord$`Plant height` < centr["Plant height"] & coord$`Germinule weight` < centr["Germinule weight"]), 4])[-2]
    }
    if(q == 8)
    {
      pie.data[[q]] <- table(coord[which(coord$SLA < centr["SLA"] & coord$`Plant height` < centr["Plant height"] & coord$`Germinule weight` > centr["Germinule weight"]), 4])[-2]
    }
    
  }
  
  pie.max <- max(unlist(lapply(pie.data, FUN=sum)))
  
  screen(i, new=F)
  par(mar=c(0,0,0,0))
  
  plot.new()
  plot.window(xlim=c(-33,385), ylim=c(-360,40), xaxs="i", yaxs="i")
  #box()
  
  #darkest
  polygon(cube$X[c(4,1,10)], cube$Y[c(4,1,10)], col="gray74", border = NA)
  polygon(cube$X[c(6,8,5)], cube$Y[c(6,8,5)], col="gray74", border = NA)
  #middle
  polygon(cube$X[c(2,5,7,1)], cube$Y[c(2,5,7,1)], col="gray96", border=NA)
  #dark right side
  polygon(cube$X[c(5,8,9,7)], cube$Y[c(5,8,9,7)], col="gray79", border=NA)
  #top side
  polygon(cube$X[c(1,7,9,10)], cube$Y[c(1,7,9,10)], col="gray84", border=NA)
  #left side
  polygon(cube$X[c(3,2,1,4)], cube$Y[c(3,2,1,4)], col="gray89", border=NA)
  #bottom side
  polygon(cube$X[c(3,6,5,2)], cube$Y[c(3,6,5,2)], col="gray93", border=NA)
  
  
  arrows(x0=10, y0=-237, x1=10, y1=-147, lwd=1, length = 0.1, angle = 15)
  arrows(x0=75, y0=-325, x1=170, y1=-335, lwd=1, length = 0.1, angle = 15)
  arrows(x0=280, y0=-310, x1=315, y1=-280, lwd=1, length = 0.1, angle = 15)
  
  text(122.5, -345, "SLA", cex=0.6, srt=-4)
  text(-10, -190, "Plant height", cex=0.6, srt=90)
  text(310, -310, "Seed", cex=0.6, srt=40)
  text(325, -325, "weight", cex = 0.6, srt=40)
  
  mtext(habitat[i], side=3, line=-0.15, outer=F, cex = 0.7, font=2)
  
  split.screen(rbind(c(0.70, 1, 0.66, 0.96),#1
                     c(0.48, 0.78, 0.46, 0.76),#2
                     c(0, 0.3, 0.52, 0.82),#3
                     c(0.23, 0.53, 0.7, 1),#4
                     c(0.7, 1, 0.15, 0.45),#5
                     c(0.48, 0.78, 0, 0.3),#6
                     c(0, 0.3, 0.02, 0.32),#7
                     c(0.23, 0.53, 0.2, 0.5)),#8 
               screen=i, erase = F)
  
  
  for(q in 1:8)
  {
    screen(q+6, new=F)
    pie(pie.data[[q]], radius=(sum(pie.data[[q]])/pie.max)*0.95, labels = NA, clockwise = T, border="white", col=pie.cols)
  }
  
  stat[[i]] <- as.data.frame(do.call("rbind", pie.data))
  stat[[i]]$sum <- rowSums(stat[[i]][,1:3])
  stat[[i]] <- t(stat[[i]])
  
  close.screen(c(i, 7:14))
}

Octants <- stat
Octants

dev.off()

do.call(rbind, Octants)

###PIE CHARTS FOR DATASET WITH IMPUTED TRAITS----------------------------------------------------------------------------

##assemble data
PlotData <- list(traitT.3D.imp, traitX.3D.imp, traitS.3D.imp, traitM.3D.imp, traitK.3D.imp, traitL.3D.imp)

#windows(7,4.8)
svg("Pie2.svg", width=7, height = 4.8)
#cairo_pdf("Pie2.pdf", width=7, height = 4.8)
#tiff("Pie2.tif", width = 7, height = 4.8, units = "in", res=400, compression = "lzw")

# split.screen(rbind(c(0, 0.495, 0.66667, 0.97),
#                    c(0.505, 1, 0.66667, 0.97),
#                    c(0, 0.495, 0.33334, 0.63667),
#                    c(0.505, 1, 0.33334, 0.63667),
#                    c(0, 0.495, 0.00001, 0.30334),
#                    c(0.505, 1, 0.00001, 0.30334)), erase = T)

split.screen(rbind(c(0.0001, 0.3334, 0.515, 0.97),
                   c(0.3334, 0.6667, 0.515, 0.97),
                   c(0.6667, 1, 0.515, 0.97),
                   c(0, 0.3334, 0, 0.455),
                   c(0.3334, 0.6667, 0, 0.455),
                   c(0.6667, 1, 0, 0.455)), erase = T)

stat <- list()

for(i in 1:6)
{
  #calculate centroid of native species
  
  centr <- apply(PlotData[[i]][which(PlotData[[i]]$INVASION.STATUS == "native"), c(1:3)], 2, mean)
  
  coord <- PlotData[[i]]
  pie.data <- list()
  
  for(q in 1:8)
  {
    #X axis = SLA
    #Y axis = Plant height
    #Z axis = Seed weight
    
    if(q == 1)
    {
      pie.data[[q]] <- table(coord[which(coord$SLA > centr["SLA"] & coord$`Plant height` > centr["Plant height"] & coord$`Germinule weight` > centr["Germinule weight"]), 4])[-2]
    }
    if(q == 2)
    {
      pie.data[[q]] <- table(coord[which(coord$SLA > centr["SLA"] & coord$`Plant height` > centr["Plant height"] & coord$`Germinule weight` < centr["Germinule weight"]), 4])[-2]
    }
    if(q == 3)
    {
      pie.data[[q]] <- table(coord[which(coord$SLA < centr["SLA"] & coord$`Plant height` > centr["Plant height"] & coord$`Germinule weight` < centr["Germinule weight"]), 4])[-2]
    }
    if(q == 4)
    {
      pie.data[[q]] <- table(coord[which(coord$SLA < centr["SLA"] & coord$`Plant height` > centr["Plant height"] & coord$`Germinule weight` > centr["Germinule weight"]), 4])[-2]
    }
    if(q == 5)
    {
      pie.data[[q]] <- table(coord[which(coord$SLA > centr["SLA"] & coord$`Plant height` < centr["Plant height"] & coord$`Germinule weight` > centr["Germinule weight"]), 4])[-2]
    }
    if(q == 6)
    {
      pie.data[[q]] <- table(coord[which(coord$SLA > centr["SLA"] & coord$`Plant height` < centr["Plant height"] & coord$`Germinule weight` < centr["Germinule weight"]), 4])[-2]
    }
    if(q == 7)
    {
      pie.data[[q]] <- table(coord[which(coord$SLA < centr["SLA"] & coord$`Plant height` < centr["Plant height"] & coord$`Germinule weight` < centr["Germinule weight"]), 4])[-2]
    }
    if(q == 8)
    {
      pie.data[[q]] <- table(coord[which(coord$SLA < centr["SLA"] & coord$`Plant height` < centr["Plant height"] & coord$`Germinule weight` > centr["Germinule weight"]), 4])[-2]
    }
    
  }
  
  pie.max <- max(unlist(lapply(pie.data, FUN=sum)))
  
  screen(i, new=F)
  par(mar=c(0,0,0,0))
  
  plot.new()
  plot.window(xlim=c(-33,385), ylim=c(-360,40), xaxs="i", yaxs="i")
  #box()
  
  #darkest
  polygon(cube$X[c(4,1,10)], cube$Y[c(4,1,10)], col="gray74", border = NA)
  polygon(cube$X[c(6,8,5)], cube$Y[c(6,8,5)], col="gray74", border = NA)
  #middle
  polygon(cube$X[c(2,5,7,1)], cube$Y[c(2,5,7,1)], col="gray96", border=NA)
  #dark right side
  polygon(cube$X[c(5,8,9,7)], cube$Y[c(5,8,9,7)], col="gray79", border=NA)
  #top side
  polygon(cube$X[c(1,7,9,10)], cube$Y[c(1,7,9,10)], col="gray84", border=NA)
  #left side
  polygon(cube$X[c(3,2,1,4)], cube$Y[c(3,2,1,4)], col="gray89", border=NA)
  #bottom side
  polygon(cube$X[c(3,6,5,2)], cube$Y[c(3,6,5,2)], col="gray93", border=NA)
  
  
  arrows(x0=10, y0=-237, x1=10, y1=-147, lwd=1, length = 0.1, angle = 15)
  arrows(x0=75, y0=-325, x1=170, y1=-335, lwd=1, length = 0.1, angle = 15)
  arrows(x0=280, y0=-310, x1=315, y1=-280, lwd=1, length = 0.1, angle = 15)
  
  text(122.5, -345, "SLA", cex=0.6, srt=-4)
  text(-10, -190, "Plant height", cex=0.6, srt=90)
  text(310, -310, "Seed", cex=0.6, srt=40)
  text(325, -325, "weight", cex = 0.6, srt=40)
  
  mtext(habitat[i], side=3, line=-0.15, outer=F, cex = 0.7, font=2)
  
  split.screen(rbind(c(0.70, 1, 0.66, 0.96),#1
                     c(0.48, 0.78, 0.46, 0.76),#2
                     c(0, 0.3, 0.52, 0.82),#3
                     c(0.23, 0.53, 0.7, 1),#4
                     c(0.7, 1, 0.15, 0.45),#5
                     c(0.48, 0.78, 0, 0.3),#6
                     c(0, 0.3, 0.02, 0.32),#7
                     c(0.23, 0.53, 0.2, 0.5)),#8 
               screen=i, erase = F)
  
  for(q in 1:8)
  {
    screen(q+6, new=F)
    pie(pie.data[[q]], radius=(sum(pie.data[[q]])/pie.max)*0.95, labels = NA, clockwise = T, border="white", col=pie.cols)
  }
  
  stat[[i]] <- as.data.frame(do.call("rbind", pie.data))
  stat[[i]]$sum <- rowSums(stat[[i]][,1:3])
  stat[[i]] <- t(stat[[i]])
  
  close.screen(c(i, 7:14))
}

Octants.imp <- stat
Octants.imp

dev.off()

do.call(rbind, Octants.imp)

###########################################################################################
#                  PIE CHARTS FOR PHYLOGENETICALLY CORRECTED DATA                         #
###########################################################################################

###PIE CHARTS FOR PHYLOGENETICALLY CORRECTED DATA-------------------------------------------

##assemble data
PlotData <- list(traitT.3Dcor, traitX.3Dcor, traitS.3Dcor, traitM.3Dcor, traitK.3Dcor, traitL.3Dcor)

svg("Pie3.svg", width=7, height = 4.8)
#cairo_pdf("Pie3.pdf", width=7, height = 4.8)
#tiff("Pie3.tif", width = 7, height = 4.8, units = "in", res=400, compression = "lzw")

# split.screen(rbind(c(0, 0.495, 0.66667, 0.97),
#                    c(0.505, 1, 0.66667, 0.97),
#                    c(0, 0.495, 0.33334, 0.63667),
#                    c(0.505, 1, 0.33334, 0.63667),
#                    c(0, 0.495, 0.00001, 0.30334),
#                    c(0.505, 1, 0.00001, 0.30334)), erase = T)

split.screen(rbind(c(0.0001, 0.3334, 0.515, 0.97),
                   c(0.3334, 0.6667, 0.515, 0.97),
                   c(0.6667, 1, 0.515, 0.97),
                   c(0, 0.3334, 0, 0.455),
                   c(0.3334, 0.6667, 0, 0.455),
                   c(0.6667, 1, 0, 0.455)), erase = T)

stat <- list()

for(i in 1:6)
{
  #calculate centroid of native species
  
  centr <- apply(PlotData[[i]][which(PlotData[[i]]$INVASION.STATUS == "native"), c(1:3)], 2, mean)
  
  coord <- PlotData[[i]]
  pie.data <- list()
  
  for(q in 1:8)
  {
    #X axis = SLA
    #Y axis = Plant height
    #Z axis = Seed weight
    
    if(q == 1)
    {
      pie.data[[q]] <- table(coord[which(coord$sla.avg.mm2mg. > centr["sla.avg.mm2mg."] & coord$HEIGHT.MAX > centr["HEIGHT.MAX"] & coord$Germinule > centr["Germinule"]), 4])[-2]
    }
    if(q == 2)
    {
      pie.data[[q]] <- table(coord[which(coord$sla.avg.mm2mg. > centr["sla.avg.mm2mg."] & coord$HEIGHT.MAX > centr["HEIGHT.MAX"] & coord$Germinule < centr["Germinule"]), 4])[-2]
    }
    if(q == 3)
    {
      pie.data[[q]] <- table(coord[which(coord$sla.avg.mm2mg. < centr["sla.avg.mm2mg."] & coord$HEIGHT.MAX > centr["HEIGHT.MAX"] & coord$Germinule < centr["Germinule"]), 4])[-2]
    }
    if(q == 4)
    {
      pie.data[[q]] <- table(coord[which(coord$sla.avg.mm2mg. < centr["sla.avg.mm2mg."] & coord$HEIGHT.MAX > centr["HEIGHT.MAX"] & coord$Germinule > centr["Germinule"]), 4])[-2]
    }
    if(q == 5)
    {
      pie.data[[q]] <- table(coord[which(coord$sla.avg.mm2mg. > centr["sla.avg.mm2mg."] & coord$HEIGHT.MAX < centr["HEIGHT.MAX"] & coord$Germinule > centr["Germinule"]), 4])[-2]
    }
    if(q == 6)
    {
      pie.data[[q]] <- table(coord[which(coord$sla.avg.mm2mg. > centr["sla.avg.mm2mg."] & coord$HEIGHT.MAX < centr["HEIGHT.MAX"] & coord$Germinule < centr["Germinule"]), 4])[-2]
    }
    if(q == 7)
    {
      pie.data[[q]] <- table(coord[which(coord$sla.avg.mm2mg. < centr["sla.avg.mm2mg."] & coord$HEIGHT.MAX < centr["HEIGHT.MAX"] & coord$Germinule < centr["Germinule"]), 4])[-2]
    }
    if(q == 8)
    {
      pie.data[[q]] <- table(coord[which(coord$sla.avg.mm2mg. < centr["sla.avg.mm2mg."] & coord$HEIGHT.MAX < centr["HEIGHT.MAX"] & coord$Germinule > centr["Germinule"]), 4])[-2]
    }
    
  }
  
  pie.max <- max(unlist(lapply(pie.data, FUN=sum)))
  
  screen(i, new=F)
  par(mar=c(0,0,0,0))
  
  plot.new()
  plot.window(xlim=c(-33,385), ylim=c(-360,40), xaxs="i", yaxs="i")
  #box()
  
  #darkest
  polygon(cube$X[c(4,1,10)], cube$Y[c(4,1,10)], col="gray74", border = NA)
  polygon(cube$X[c(6,8,5)], cube$Y[c(6,8,5)], col="gray74", border = NA)
  #middle
  polygon(cube$X[c(2,5,7,1)], cube$Y[c(2,5,7,1)], col="gray96", border=NA)
  #dark right side
  polygon(cube$X[c(5,8,9,7)], cube$Y[c(5,8,9,7)], col="gray79", border=NA)
  #top side
  polygon(cube$X[c(1,7,9,10)], cube$Y[c(1,7,9,10)], col="gray84", border=NA)
  #left side
  polygon(cube$X[c(3,2,1,4)], cube$Y[c(3,2,1,4)], col="gray89", border=NA)
  #bottom side
  polygon(cube$X[c(3,6,5,2)], cube$Y[c(3,6,5,2)], col="gray93", border=NA)
  
  
  arrows(x0=10, y0=-237, x1=10, y1=-147, lwd=1, length = 0.1, angle = 15)
  arrows(x0=75, y0=-325, x1=170, y1=-335, lwd=1, length = 0.1, angle = 15)
  arrows(x0=280, y0=-310, x1=315, y1=-280, lwd=1, length = 0.1, angle = 15)
  
  text(122.5, -345, "SLA", cex=0.6, srt=-4)
  text(-10, -190, "Plant height", cex=0.6, srt=90)
  text(310, -310, "Seed", cex=0.6, srt=40)
  text(325, -325, "weight", cex = 0.6, srt=40)
  
  mtext(habitat[i], side=3, line=-0.15, outer=F, cex = 0.7, font=2)
  
  split.screen(rbind(c(0.70, 1, 0.66, 0.96),#1
                     c(0.48, 0.78, 0.46, 0.76),#2
                     c(0, 0.3, 0.52, 0.82),#3
                     c(0.23, 0.53, 0.7, 1),#4
                     c(0.7, 1, 0.15, 0.45),#5
                     c(0.48, 0.78, 0, 0.3),#6
                     c(0, 0.3, 0.02, 0.32),#7
                     c(0.23, 0.53, 0.2, 0.5)),#8 
               screen=i, erase = F)
  
  for(q in 1:8)
  {
    screen(q+6, new=F)
    pie(pie.data[[q]], radius=(sum(pie.data[[q]])/pie.max)*0.95, labels = NA, clockwise = T, border="white", col=pie.cols)
  }
  
  stat[[i]] <- as.data.frame(do.call("rbind", pie.data))
  stat[[i]]$sum <- rowSums(stat[[i]][,1:3])
  stat[[i]] <- t(stat[[i]])
  
  close.screen(c(i, 7:14))
}

Octants.cor <- stat
Octants.cor

dev.off()

do.call(rbind, Octants.cor)

###PIE CHARTS FOR PHYLOGENETICALLY CORRECTED DATA WITH IMPUTED TRAITS-------------------------------------------

##assemble data
PlotData <- list(traitT.3Dcor.imp, traitX.3Dcor.imp, traitS.3Dcor.imp, traitM.3Dcor.imp, traitK.3Dcor.imp, traitL.3Dcor.imp)

svg("Pie4.svg", width=7, height = 4.8)
#cairo_pdf("Pie4.pdf", width=7, height = 4.8)
#tiff("Pie4.tif", width = 7, height = 4.8, units = "in", res=400, compression = "lzw")

# split.screen(rbind(c(0, 0.495, 0.66667, 0.97),
#                    c(0.505, 1, 0.66667, 0.97),
#                    c(0, 0.495, 0.33334, 0.63667),
#                    c(0.505, 1, 0.33334, 0.63667),
#                    c(0, 0.495, 0.00001, 0.30334),
#                    c(0.505, 1, 0.00001, 0.30334)), erase = T)

split.screen(rbind(c(0.0001, 0.3334, 0.515, 0.97),
                   c(0.3334, 0.6667, 0.515, 0.97),
                   c(0.6667, 1, 0.515, 0.97),
                   c(0, 0.3334, 0, 0.455),
                   c(0.3334, 0.6667, 0, 0.455),
                   c(0.6667, 1, 0, 0.455)), erase = T)

stat <- list()

for(i in 1:6)
{
  #calculate centroid of native species
  
  centr <- apply(PlotData[[i]][which(PlotData[[i]]$INVASION.STATUS == "native"), c(1:3)], 2, mean)
  
  coord <- PlotData[[i]]
  pie.data <- list()
  
  for(q in 1:8)
  {
    #X axis = SLA
    #Y axis = Plant height
    #Z axis = Seed weight
    
    if(q == 1)
    {
      pie.data[[q]] <- table(coord[which(coord$sla.avg.mm2mg. > centr["sla.avg.mm2mg."] & coord$HEIGHT.MAX > centr["HEIGHT.MAX"] & coord$Germinule > centr["Germinule"]), 4])[-2]
    }
    if(q == 2)
    {
      pie.data[[q]] <- table(coord[which(coord$sla.avg.mm2mg. > centr["sla.avg.mm2mg."] & coord$HEIGHT.MAX > centr["HEIGHT.MAX"] & coord$Germinule < centr["Germinule"]), 4])[-2]
    }
    if(q == 3)
    {
      pie.data[[q]] <- table(coord[which(coord$sla.avg.mm2mg. < centr["sla.avg.mm2mg."] & coord$HEIGHT.MAX > centr["HEIGHT.MAX"] & coord$Germinule < centr["Germinule"]), 4])[-2]
    }
    if(q == 4)
    {
      pie.data[[q]] <- table(coord[which(coord$sla.avg.mm2mg. < centr["sla.avg.mm2mg."] & coord$HEIGHT.MAX > centr["HEIGHT.MAX"] & coord$Germinule > centr["Germinule"]), 4])[-2]
    }
    if(q == 5)
    {
      pie.data[[q]] <- table(coord[which(coord$sla.avg.mm2mg. > centr["sla.avg.mm2mg."] & coord$HEIGHT.MAX < centr["HEIGHT.MAX"] & coord$Germinule > centr["Germinule"]), 4])[-2]
    }
    if(q == 6)
    {
      pie.data[[q]] <- table(coord[which(coord$sla.avg.mm2mg. > centr["sla.avg.mm2mg."] & coord$HEIGHT.MAX < centr["HEIGHT.MAX"] & coord$Germinule < centr["Germinule"]), 4])[-2]
    }
    if(q == 7)
    {
      pie.data[[q]] <- table(coord[which(coord$sla.avg.mm2mg. < centr["sla.avg.mm2mg."] & coord$HEIGHT.MAX < centr["HEIGHT.MAX"] & coord$Germinule < centr["Germinule"]), 4])[-2]
    }
    if(q == 8)
    {
      pie.data[[q]] <- table(coord[which(coord$sla.avg.mm2mg. < centr["sla.avg.mm2mg."] & coord$HEIGHT.MAX < centr["HEIGHT.MAX"] & coord$Germinule > centr["Germinule"]), 4])[-2]
    }
    
  }
  
  pie.max <- max(unlist(lapply(pie.data, FUN=sum)))
  
  screen(i, new=F)
  par(mar=c(0,0,0,0))
  
  plot.new()
  plot.window(xlim=c(-33,385), ylim=c(-360,40), xaxs="i", yaxs="i")
  #box()
  
  #darkest
  polygon(cube$X[c(4,1,10)], cube$Y[c(4,1,10)], col="gray74", border = NA)
  polygon(cube$X[c(6,8,5)], cube$Y[c(6,8,5)], col="gray74", border = NA)
  #middle
  polygon(cube$X[c(2,5,7,1)], cube$Y[c(2,5,7,1)], col="gray96", border=NA)
  #dark right side
  polygon(cube$X[c(5,8,9,7)], cube$Y[c(5,8,9,7)], col="gray79", border=NA)
  #top side
  polygon(cube$X[c(1,7,9,10)], cube$Y[c(1,7,9,10)], col="gray84", border=NA)
  #left side
  polygon(cube$X[c(3,2,1,4)], cube$Y[c(3,2,1,4)], col="gray89", border=NA)
  #bottom side
  polygon(cube$X[c(3,6,5,2)], cube$Y[c(3,6,5,2)], col="gray93", border=NA)
  
  
  arrows(x0=10, y0=-237, x1=10, y1=-147, lwd=1, length = 0.1, angle = 15)
  arrows(x0=75, y0=-325, x1=170, y1=-335, lwd=1, length = 0.1, angle = 15)
  arrows(x0=280, y0=-310, x1=315, y1=-280, lwd=1, length = 0.1, angle = 15)
  
  text(122.5, -345, "SLA", cex=0.6, srt=-4)
  text(-10, -190, "Plant height", cex=0.6, srt=90)
  text(310, -310, "Seed", cex=0.6, srt=40)
  text(325, -325, "weight", cex = 0.6, srt=40)
  
  mtext(habitat[i], side=3, line=-0.15, outer=F, cex = 0.7, font=2)
  
  split.screen(rbind(c(0.70, 1, 0.66, 0.96),#1
                     c(0.48, 0.78, 0.46, 0.76),#2
                     c(0, 0.3, 0.52, 0.82),#3
                     c(0.23, 0.53, 0.7, 1),#4
                     c(0.7, 1, 0.15, 0.45),#5
                     c(0.48, 0.78, 0, 0.3),#6
                     c(0, 0.3, 0.02, 0.32),#7
                     c(0.23, 0.53, 0.2, 0.5)),#8 
               screen=i, erase = F)
  
  for(q in 1:8)
  {
    screen(q+6, new=F)
    pie(pie.data[[q]], radius=(sum(pie.data[[q]])/pie.max)*0.95, labels = NA, clockwise = T, border="white", col=pie.cols)
  }
  
  stat[[i]] <- as.data.frame(do.call("rbind", pie.data))
  stat[[i]]$sum <- rowSums(stat[[i]][,1:3])
  stat[[i]] <- t(stat[[i]])
  
  close.screen(c(i, 7:14))
}

Octants.cor.imp <- stat
Octants.cor.imp

dev.off()

do.call(rbind, Octants.cor.imp)
