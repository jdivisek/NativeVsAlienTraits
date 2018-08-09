########################################################################
#                       DIFFERENCE OF MEDIANS                          #
########################################################################

#measure difference of median trait values for selected species groups
diff.of.medians <- function(tr, stat, target=c("invasive", "naturalized"))
{
  df <- as.data.frame(tr)
  df$stat <- stat
  
  df <- df[complete.cases(df),] #remove NAs if necessary
  
  m <- c(median(df$tr[which(df$stat == "native")]),
         median(df$tr[which(df$stat == target[1])]))
  v <- diff(m)
  return(v)
}