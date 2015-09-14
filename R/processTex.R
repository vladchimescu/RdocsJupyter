processTex <- function(filename, removeFigs = TRUE) {
  replaceEnv(filename)
  replaceMacros(filename)
  if(removeFigs) {
    removeFigures(filename)
  }
}
