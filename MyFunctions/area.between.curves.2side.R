#############################################################
#           CALCULATE AREA BETWEEN THE TWO CDFs             #
#############################################################

#define function to calculate area between the curves
area.between.curves.2side <- function(yAxis1, yAxis2, xAxis)
{
  require(splancs)
  
  abc.neg <- 0
  abc.pos <- 0
  
  #calculate negative area
  if(all(yAxis2 <= yAxis1))
  {
    dat <- cbind(c(xAxis, rev(xAxis)), c(yAxis2, rev(yAxis1)))
    abc.neg <- areapl(dat)*-1
  }
  
  if(any(yAxis2 > yAxis1))
  {
    sel <- which(yAxis2 > yAxis1)
    
    y <- yAxis2
    y[sel] <- yAxis1[sel]
    
    if(identical(y, yAxis1) == FALSE)
    {
      dat <- cbind(c(xAxis, rev(xAxis)), c(y, rev(yAxis1)))
      
      abc.neg <- areapl(dat)*-1
    }
  }
  
  #calculate positive area
  if(all(yAxis2 >= yAxis1))
  {
    dat <- cbind(c(xAxis, rev(xAxis)), c(yAxis2, rev(yAxis1)))
    abc.pos <- areapl(dat)
  }
  
  if(any(yAxis2 < yAxis1))
  {
    sel <- which(yAxis2 < yAxis1)
    
    y <- yAxis2
    y[sel] <- yAxis1[sel]
    
    if(identical(y, yAxis1) == FALSE)
    {
      dat <- cbind(c(xAxis, rev(xAxis)), c(y, rev(yAxis1)))
      
      abc.pos <- areapl(dat)
    }
  }
  
  res <- if(abc.pos > abs(abc.neg)) abc.pos else abc.neg
  
  return(res)
}