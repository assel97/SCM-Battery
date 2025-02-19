# use the first (or alternatively the last) of several files for the same participant?
usefirst <- TRUE

# how many lines should be in a data file? (there may be multiple correct options)
nlines <- c(77)

taskswitching <- function(filename) {
  
  use <- TRUE
  
  # first we read the data file:
  df <- read.csv(filename, stringsAsFactors=F)
  
  thisparticipant <- as.character(df$participant[1])
  thistotaltime <- df$cumulativetime[dim(df)[1]]
  thisOS <- df$OS[1]
  
  block <- c(rep(1,13), rep(2,13), rep(3,50))
  
  df <- cbind(block,df)
  
 
  
  
  # if ('key_resp.rt' %in% names(df)) {
  #   df <- df[which(is.na(df$key_resp.rt) | df$key_resp.rt > 0.1),]
  # } else {
  #   "NA"
  
  # remove very low RTs:
    df$key_resp.rt[df$key_resp.rt < 0.1] <- NA
    df$key_resp.rt[df$key_resp.rt == ""] <- NA
  
  # get a column indicating whether the response is correct 
  apply_correct <- function(answer_response){
    if (answer_response[1]== answer_response[2])
      return(1)
    else 
      return(0)
  }
  
  df$correctResponse <- apply(df[,c('correctAnswer','key_resp.keys')], 1, FUN=apply_correct)
  
  # Switch vs. non-switch trials 
  # given a string of numbers from pavlovia experiment, makes an array and ONLY RETURNS THE SECOND VALUE
  online_string_to_array <- function(str_array){
    
    x <- str_sub(str_array, 2, str_length(str_array)-1) # subset string to get rid of beginning and end
    x <- simplify(lapply(str_split(x, ','), as.double)) # make this a list of doubles (simplify gets rid of unnecessary levels)
    
    x <- x[2] # we only need the second value
    return(x)
  }
  
 
  library(tidyverse)
  
  #finding switch vs. non-switch trials 
  df$grid_loc_y <- apply(df[,'gridLocation', drop=F], 
                         1, FUN = online_string_to_array)
  
  df <- df %>% 
    mutate(grid_diff = lag(grid_loc_y) + grid_loc_y) %>%
    mutate(switch = recode(grid_diff, 
                           "0" = "1",
                           .default = "0")) %>% select(-grid_diff,  -contains("phase"))
  
  
  
  # get proportion correct scores to data for switch trials:
  correct <- aggregate(correctResponse ~ switch, data=df, FUN=mean)
  correct$correctResponse <- round(correct$correctResponse, digits = 3)
  
  #### which percent threshold for error to keep 
    if (any(correct$correctResponse[which(correct$switch == 1)] < 0.60)) {
   use <- FALSE
  #cat('too many errors\n')
   }
  
  #correct response output for switch vs.non-switch trials  
  correctOutput <- as.vector(unlist(correct$correctResponse))
  names(correctOutput) <- sprintf('switch_%s_prop.correct',correct$switch)
  
  # data frame for single trials 
  
 
  singledf <- df[c(4:12, 17:25 ),]
  
  print (singledf)
  # get proportion correct scores to data for single block trials:
  correctsingle <- aggregate(correctResponse ~ block, data = singledf, FUN = mean)
  
  correctsingle$correctResponse <- round(correctsingle$correctResponse, digits = 3)
  
   if (any(correctsingle$correctResponse[which(correctsingle$block == 1 | correctsingle$block == 2)] < 0.65)) {
     use <- FALSE
     #cat('too many errors\n')
   }
  
  #correct response output for single block trials  
  correctOutputSingle <- as.vector(unlist(correctsingle$correctResponse))
  names(correctOutputSingle) <- sprintf('block_%s_prop.correct',correctsingle$block)
  
  
  RToutput <- c()

  
  
  #get RTs for single block 1
  singleBlock1_idx <- which(singledf$correctResponse == 1  & singledf$block == 1) 
  if (length(singleBlock1_idx) > 0) {
    singleBlock1_RT <- mean(singledf$key_resp.rt[singleBlock1_idx], na.rm=TRUE)
  } else {singleBlock1_idx <- NA}
  
  singleBlock1_RT <- round(singleBlock1_RT, digits = 3)
 
   
  
  #get RTs for single block 2
  singleBlock2_idx <- which(singledf$correctResponse == 1  &  singledf$block==2) 
  if (length(singleBlock2_idx) > 0) {
    singleBlock2_RT <- mean(singledf$key_resp.rt[singleBlock2_idx], na.rm=TRUE)
  } else {singleBlock2_idx <- NA}
  
  singleBlock2_RT <- round(singleBlock2_RT, digits = 3)
  
  
  # get RTs for switch trials
  switch_idx <- which(df$correctResponse == 1  & df$switch == 1 & df$block == 3) 
  if (length(switch_idx) > 0) {
    switch_RT <- mean(df$key_resp.rt[switch_idx], na.rm=TRUE)
  } else {switch_idx <- NA}
  
  switch_RT <- round(switch_RT, digits = 3)
  

  
  
  # get RTs for non-switch trials
  nonswitch_idx <- which(df$correctResponse == 1  & df$switch == 0 & df$block == 3) 
  if (length(nonswitch_idx) > 0) {
    nonswitch_RT <- mean(df$key_resp.rt[nonswitch_idx], na.rm=TRUE)
  } else {nonswitch_idx <- NA}
  
  nonswitch_RT <- round(nonswitch_RT, digits = 3)

 
  newRToutput <- c(singleBlock1_RT,singleBlock2_RT, switch_RT, nonswitch_RT)
  names(newRToutput) <- sprintf(c('singleblock_1_RT','singleblock_2_RT','switch_RT', 'nonswitch_RT' ))
  RToutput <- c(RToutput, newRToutput)
  
  
  # remove break lines:
  df <- df[which(!is.na(df$trials.thisRepN)),]
  
  
  # create named output vector
  output <- as.list(c(correctOutput, correctOutputSingle,RToutput))
  if (!use) {
    output[1:length(output)] <- NA
  }
  
  
  output[['participant']]     <- thisparticipant
  output[['totaltime']]       <- thistotaltime
  output[['OS']]              <- thisOS
  output[['passedscreening']] <- use
  
  return(output)
  
}
  
  
  