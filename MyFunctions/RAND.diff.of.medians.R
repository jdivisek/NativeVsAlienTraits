#########################################################################################
#                         SIMULATED DIFFERENCES OF MEDIANS                              #
#########################################################################################

#randomize data and calculate difference of medians
RAND.diff.of.medians <- function(tr, stat, target=c("invasive", "naturalized"), Nrand=999)
{
  res <- vector("numeric")
  
  df <- as.data.frame(tr)
  df$stat <- stat
  
  df <- df[complete.cases(df),] #remove NAs if necessary
  
  for(i in seq(1,Nrand))
  {
    stat.r <- sample(df$stat)#reshuffle labels
    res[i] <- diff.of.medians(tr=df$tr, stat=stat.r, target=target)#calculate difference of medians
  }
  return(res)
}
