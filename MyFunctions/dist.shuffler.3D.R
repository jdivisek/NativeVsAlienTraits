###############################################################################
#         CALCULATE SIMULATED CUMULATIVE DISTRIBUTION FUNCTION                #
###############################################################################

#define funtion to randomize species and calculate distances from the centroid
dist.shuffler.3D <- function(scores, status, test="invasive", Nrand=999)
{
  yAxis.rand <- list()
  
  #calculate centroid of native species
  centr.nat <- apply(scores[which(status == "native"),], 2, mean)
  
  #calculate number of invasive species
  No.spe <- sum(status == test)
  
  #calculate distances of species to centroid
  centr <- rbind(centr.nat, scores)
  d.centr <- as.matrix(dist(centr, "euclidean"))[,1]
  d.centr <- d.centr[-1]
  
  #create x axis
  xAxis <- seq(from=0, to=max(d.centr),length=1000)
  
  d.centr <- d.centr[c(which(status == "native" | status == test))]
  
  for(i in seq(1, Nrand))
  {
    #randomly sample distances
    d.centr.sel <- sample(d.centr, size=No.spe, replace=FALSE)
    
    #calculate randomized cdf curve
    yAxis.rand[[i]] <- vapply(xAxis,calcCDF,FUN.VALUE=1,d.centr.sel) 
  }
  return(yAxis.rand)
}
