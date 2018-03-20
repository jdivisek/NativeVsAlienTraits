###################################################
#   CALCULATE CUMULATIVE DISTRIBUTION FUNCTION    #
###################################################

#code by Brian Beckage (brian.beckage@uvm.edu)
calcCDF<-function(threshold,dataVect){
  length(dataVect[abs(dataVect)<=threshold])/length(dataVect)}