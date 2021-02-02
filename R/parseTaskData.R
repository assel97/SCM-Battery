


# we want to run functions named as a string over a bunch of participants

getGroupPerformance <- function(year, semester, task) {
  
  # get list of participants / file names
  
  
  
  source(paste0('R/',task,'.R'))
  
  print(usefirst)
  
  # now we can use do.call, to run a named function on some data:
  
  # do.call(task, DF)
  
  # f <- match.fun(task)
  
  # and use lapply to run stuff on all participants with simplify = 'matrix' ?
  
  # return a data frame
  
}