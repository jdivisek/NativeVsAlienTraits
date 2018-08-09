################################################################
#    LOOP FOR CALCULATION OF THE AREA BETWEEN THE TWO CDFs     #
################################################################

#loop for area.between.curves.2side
abc.loop <- function(obs.yAxis, rand.yAxes, xAxis)
{
  area <- vector("numeric")
  n <- length(rand.yAxes)
  
  for(i in seq(1,n))
  {
    area[i] <- area.between.curves.2side(yAxis1 = obs.yAxis, yAxis2 = rand.yAxes[[i]], xAxis=xAxis)[[1]] 
  }
  return(area)
}