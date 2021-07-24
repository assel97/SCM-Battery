library('osfr')

getOSFdata <- function(year, semester, task, overwrite=FALSE, removezip=FALSE) {
  
  # check the OSF repository
  OSFnode <- osfr::osf_retrieve_node("q8kda")
  
  # get a list of files for the year and semester that is requested:
  files <- osfr::osf_ls_files(OSFnode, path=sprintf('%s/%s',year, semester))
  
  # find which line corresponds to the task:
  idx <- which(files$name == sprintf('%s.zip', task))
  
  # check that the task / semester / year combination exists on OSF, and is unique:
  # if not: abort
  if (length(idx) > 1) {
    # no unique file found: aborting this one
    if (length(idx) == 0) {
      return(FALSE)
    }
    return(NULL)
  }
  
  # download the zip file:
  if (!file.exists(sprintf('data/%s',files$name[idx])) | overwrite) {
    osfr::osf_download(files[idx,], 'data/')
  }
  
  # determine where the contents should go:
  path <- sprintf('./data/%s/%s/%s/',year,semester,task)
  
  # and unzip it there:
  unzip(sprintf('data/%s',files$name[idx]), exdir=path)
  
  if (removezip) {
    file.remove(sprintf('data/%s',files$name[idx]))
  }
  
  return(TRUE)
  
}