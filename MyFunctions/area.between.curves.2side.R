#############################################################
#           CALCULATE AREA BETWEEN THE TWO CDFs             #
#############################################################

#define function to calculate area between the curves
area.between.curves.2side <- function(yAxis1, yAxis2, xAxis)
{
  d <- c(yAxis2 - yAxis1)[-1]
  
  d <- d * diff(xAxis)
  
  neg <- sum(abs(d[which(d < 0)]))
  pos <- sum(d[which(d > 0)])
  
  abc <- list()
  
  if(neg > pos) abc[[1]] <- neg*-1
  if(pos > neg) abc[[1]] <- pos
  
  abc[[2]] <- c(pos, neg)/(pos+abs(neg))
  
  return(abc)
}