###############################################################################
#         CALCULATE SIMULATED CUMULATIVE DISTRIBUTION FUNCTION                #
###############################################################################

#define funtion to randomize species and calculate distnaces from the centroid
dist.shuffler.3D <- function(scores, status, test="invasive", Nrand=999)
{
  require(vegan)
  yAxis.rand <- list()
  
  #calculate centroid of native species
  centr.nat <- apply(scores[which(status == "native"),], 2, mean)
  
  #calculate number of invasive species
  No.spe <- sum(status == test)
  
  #calculate distances of species to centroid
  centr <- rbind(centr.nat, scores[c(which(status == "native" | status == test)), ])
  d.centr <- as.matrix(dist(centr, "euclidean"))[,1]
  d.centr <- decostand(d.centr[-1], "range") #scale distances between 0 and 1
  
  #create x axis
  xAxis <- seq(from=0, to=ceiling(max(d.centr)),length=1000)
  
  for(i in seq(1, Nrand))
  {
    #randomly sample distances
    d.centr.sel <- sample(d.centr, size=No.spe, replace=FALSE)
    
    #calculate randomized cdf curve
    yAxis.rand[[i]] <- vapply(xAxis,calcCDF,FUN.VALUE=1,d.centr.sel) 
  }
  return(yAxis.rand)
}