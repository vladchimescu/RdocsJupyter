# source("~/Documents/embl/Rnotebooks/RdocsJupyter/R/replace.R")
processTex <- function(filename, removeFigs = TRUE) {
  replaceEnv(filename)
  replaceMacros(filename)
  if(removeFigs) {
    removeFigures(filename)
  }
}
