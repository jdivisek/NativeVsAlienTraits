############################################################################################
#               PHYLOGENETIC FORWARD SELECTION FOR MULTIVARIATE DATA                       #
############################################################################################

#The selection is based on an iterative search for the eigenvector that reduces the largest 
#amount of phylogenetic autocorrelation in the residuals of multivariate trait table. As new 
#eigenvectors are added to the model, residuals are updated and autocorrelation re-estimated. 
#The search stops when residual autocorrelation is reduced to a level that is statistically 
#non-significant (P ≥ 0.05).

phylo.multifwd2 <- function(tr, w, ME, nperm=999)
{
  #load required packages
  require(ade4)
  require(adephylo)
  
  #test each eigenvector separately
  test <- vector("numeric")
  
  print("Selection of 1. eigenvector")
  for(i in seq(1,ncol(ME)))
  {
    res <- as.data.frame(resid(lm(as.matrix(tr) ~., as.data.frame(ME[,i]))))
    d.pca <- dudi.pca(res, center=FALSE, scale=FALSE, scannf=FALSE)
    test[i] <- multispati.randtest(d.pca, w, nrepet = 1)$obs
  }
  
  #model with ME eigenvector that most decreases phylogentic autocorrelation
  sel <- which.min(test)
  
  #calculate significance
  res <- as.data.frame(resid(lm(as.matrix(tr) ~., as.data.frame(ME[,sel]))))
  d.pca <- dudi.pca(res, center=FALSE, scale=FALSE, scannf=FALSE)
  sig <- multispati.randtest(d.pca, w, nrepet = nperm)$pvalue
  
  #select eigenvector
  ME.sel <- as.data.frame(ME[, sel])
  colnames(ME.sel) <- colnames(ME)[sel]
  rownames(ME.sel) <- rownames(ME)
  ME <- ME[, -c(sel)]
  
  #test the rest of eigenvectors
  for(q in seq(1, ncol(ME)))
  {
    if(sig < 0.05) #stop if phylogenetic autocorrelation in model residuals is not significant
    {
      test <- vector("numeric")
      
      #test eigenvectors
      print(paste("Selection of ", q+1, ". eigenvector", sep = ""))
      
      for(i in seq(1, ncol(ME)))
      {
        res <- as.data.frame(resid(lm(as.matrix(tr) ~., data=as.data.frame(cbind(ME.sel, ME[,i])))))
        d.pca <- dudi.pca(res, center=FALSE, scale=FALSE, scannf=FALSE)
        test[i] <- multispati.randtest(d.pca, w, nrepet = 1)$obs
      }
      
      #model with ME eigenvector that most decraeses phylogentic autocorrelation
      sel <- which.min(test)
      
      #calculate significance
      res <- as.data.frame(resid(lm(as.matrix(tr) ~., data=as.data.frame(cbind(ME.sel, ME[, sel])))))
      d.pca <- dudi.pca(res, center=FALSE, scale=FALSE, scannf=FALSE)
      sig <- multispati.randtest(d.pca, w, nrepet = nperm)$pvalue
      
      #select eigenvector
      ME.sel <- as.data.frame(cbind(ME.sel, ME[, sel]))
      colnames(ME.sel)[ncol(ME.sel)] <- colnames(ME)[sel]
      ME <- ME[, -c(sel)]
    }
  }
  print(paste("Forward selection stopped with p =", sig, sep=" "))
  return(ME.sel)
}
