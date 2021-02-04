
# use the first (or alternatively the last) of several files for the same participant?
usefirst <- TRUE

# how many lines should be in a data file? (there may be multiple correct options)
nlines <- c(165)

visualsearch <- function(filename) {
  
  # first we read the data file:
  df <- read.csv(filename, stringsAsFactors=F)
  
  # set up a vector for output, must be a named vector:
  output <- c()
  use = TRUE
  
  # remove lines for breaks:
  df <- df[-which(is.na(df$trialResp.corr)),]
  
  # remove lines with very low RTs:
  df <- df[which(df$trialResp.rt > 0.1),]
  
  # get target presence as a variable:
  df$counttrials <- 1
  df$targetpresent <- NA
  df$targetpresent[which(df$trialResp.keys == 'm' & df$trialResp.corr == 1)] <- 'absent'
  df$targetpresent[which(df$trialResp.keys == 'x' & df$trialResp.corr == 1)] <- 'present'
  df$targetpresent[which(df$trialResp.keys == 'm' & df$trialResp.corr == 0)] <- 'present'
  df$targetpresent[which(df$trialResp.keys == 'x' & df$trialResp.corr == 0)] <- 'absent'
  
  # check if there are enough good trials left
  counttrials <- aggregate(counttrials ~ arraysize + targetpresent, data=df, FUN=sum)
  if (any(counttrials$counttrials < 18)) {
    #print(filename)
    use = FALSE
  }
  
  # get proportion correct scores to data:
  correct <- aggregate(trialResp.corr ~ arraysize + targetpresent, data=df, FUN=mean)
  # remove people who are less then 66.7% correct in any condition
  if (any(correct$trialResp.corr < (2/3))) {
    use = FALSE
  }
  correctOutput <- as.vector(unlist(correct$trialResp.corr))
  names(correctOutput) <- sprintf('propcorrect_%d_%s',correct$arraysize,correct$targetpresent)
  
  # we remove the trials without a correct response:
  df[which(df$trialResp.corr == 1),]
  
  # now get the average RTs:
  RTs <- aggregate(trialResp.rt ~ arraysize + targetpresent, data=df, FUN=mean)
  
  RToutput <- as.vector(unlist(RTs$trialResp.rt))
  names(RToutput) <- sprintf('RT_%d_%s',RTs$arraysize,RTs$targetpresent)
  
  # fit 2 linear models:
  for (target in c('absent','present')) {
    
    model <- lm(trialResp.rt ~ arraysize, data=RTs[which(RTs$targetpresent==target),])
    lmoutput <- as.vector(unlist(coef(model)))
    names(lmoutput) <- sprintf('lm_%s_%s',c('intercept','slope'),target)
    output <- c(output, lmoutput)
    
  }
  
  # create named output vector
  output <- as.list(c(output, correctOutput, RToutput))
  if (!use) {
    output[1:length(output)] <- NA
  }
  output[['participant']] <-  df$participant[1]
  output[['totaltime']] <- df$cumulativetime[dim(df)[1]]
  output[['OS']] <- df$OS[1]
  
  # return to caller:
  return(output)
  
}