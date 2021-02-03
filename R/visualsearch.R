
# use the first (or alternatively the last) of several files for the same participant?
usefirst <- TRUE

# how many lines should be in a data file? (there may be multiple correct options)
nlines <- c(165)

visualsearch <- function(filename) {
  
  # first we read the data file:
  df <- read.csv(filename, stringsAsFactors=F)
  
  # remove lines for breaks:
  df[which(!is.na(df$trialResp.corr)),]
  
  
  
  # we remove the trials without a correct response:
  df[which(df$trialResp.corr == 1),]
  
  
  
  return(c('rows'=dim(df)[1],'cols'=dim(df)[2]))
  
}