library('osfr')

getOSFdata <- function() {
  
  OSFnode <- osfr::osf_retrieve_node("q8kda")
  
  osfr::osf_ls_files(OSFnode, path='2020_fall')
  
  
}