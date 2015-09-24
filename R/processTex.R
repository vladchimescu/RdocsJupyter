processTex <- function(filename, removeFigs = TRUE) {
  replaceEnv(filename)
  replaceMacros(filename)
  if(removeFigs) {
    removeFigures(filename)
  }
}

processVignette <- function(filename, removeFigs = TRUE) {
  ## so first need to figure out if the vignette is Rnw or Rmd
  ## depending on this branch into two action modes for these formats
  ## use file_ext() function from 'tools' package (which has to be loaded)
  if(file_ext(filename) == "Rmd") {
    ## extract all code chunk headers with figure dimension settings
    # grep("^[\t >]*```+\\s*\\{[.]?([a-zA-Z]+.*)(fig+.*)\\}\\s*$", 
         # readLines(filename), value=TRUE)
    text <- gsub("(^[\t >]*```+\\s*\\{[.]?)([a-zA-Z]+.*)(fig+.*)\\}\\s*$", 
                 "\\1\\2\\3\\}\n# \\2\\3", readLines(filename))
    writeLines(text, con = filename, sep = "\n")
    ## now need to add comments in code chunks with figure parameters
  }
  
  else if(file_ext(filename) == "Rnw") {
    ## extract all code chunk headers with figure dimension settings
    # grep("^\\s*<<(.*)(fig+.*)>>=.*$", readLines(filename), value = TRUE)
    text <- gsub("(^\\s*<<)(.*)(fig+.*)>>=.*$", 
                 "\\1\\2\\3>>=\n# \\2\\3", readLines(filename))
    writeLines(text, con = filename, sep = "\n")
  }
  
  else {
    ## do nothing and print an error, saying that only
    ## Rmd and Rnw vignettes can be processed

  }
