
# use the first (or alternatively the last) of several files for the same participant?
usefirst <- TRUE

# how many lines should be in a data file? (there may be multiple correct options)
nlines <- c(303)

gonogo <- function(filename) {
  
  print(filename)
  
  # first we read the data file:
  df <- read.csv(filename, stringsAsFactors=F)
  
  # get a more useful column with trialtype:
  df$trialtype <- NA
  df$trialtype[which(df$colorName == 'blue')] <- 'go'
  df$trialtype[which(df$colorName == 'orange')] <- 'nogo'
  
  # set up a vector for output, must be a named vector:
  output <- c()
  
  # remove lines for breaks:
  df <- df[-which(is.na(df$trialResp.corr)),]
  
  # remove lines with very low RTs:
  df <- df[which(df$trialResp.rt > 0.1 | is.na(df$trialResp.rt)),]
  
  # for counting trials
  df$counttrials <- 1
  
  # get proportion correct scores to data:
  correct <- aggregate(trialResp.corr ~ trialtype, data=df, FUN=mean)
  # remove people who are less then 66.7% correct in any condition
  if (any(correct$trialResp.corr < 0.8)) {
    use = FALSE
  }
  
  print(correct)
  
}
