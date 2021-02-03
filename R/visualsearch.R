
# use the first (or alternatively the last) of several files for the same participant?
usefirst <- TRUE

# how many lines should be in a data file? (there may be multiple correct options)
nlines <- c(165)

visualsearch <- function(filename) {
  
  # first we read the data file:
  df <- read.csv(filename, stringsAsFactors=F)
  
  # set up a vector for output, must be a named vector:
  output <- c()
  
  # get some basic descriptors of the participant:
  descriptors <- as.vector(unlist(df[dim(df)[1],c('participant','OS','cumulativetime')]))
  names(descriptors) <- c('participant', 'OS', 'totaltime')
  
  # add descriptors to output:
  #output <- c(output,  descriptors)
  
  # remove lines for breaks:
  df <- df[-which(is.na(df$trialResp.corr)),]
  
  # get target presence as a variable:
  df$targetpresent <- NA
  df$targetpresent[which(df$trialResp.keys == 'm' & df$trialResp.corr == 1)] <- 'absent'
  df$targetpresent[which(df$trialResp.keys == 'x' & df$trialResp.corr == 1)] <- 'present'
  df$targetpresent[which(df$trialResp.keys == 'm' & df$trialResp.corr == 0)] <- 'present'
  df$targetpresent[which(df$trialResp.keys == 'x' & df$trialResp.corr == 0)] <- 'absent'
  
  # add proportion correct scores to data:
  correct <- aggregate(trialResp.corr ~ arraysize + targetpresent, data=df, FUN=mean)
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
  output <- c(output, correctOutput, RToutput)
  
  # return to caller:
  return(output)
  
}