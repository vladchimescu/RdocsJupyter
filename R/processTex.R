processTex <- function(filename, removeFigs = TRUE) {
  replaceEnv(filename)
  replaceMacros(filename)
  if(removeFigs) {
    removeFigures(filename)
  }
}

processVignette <- function(filename, removeFigs = TRUE) {
  if(file_ext(filename) == "Rmd") {
    ## extract all code chunk headers with figure dimension settings
    ## and put them in comments
    if (length(grep("IRkernel", readLines(filename), fixed = TRUE)) == 0) {
      text <- gsub("(^[\t >]*```+\\s*\\{[.]?)([a-zA-Z]+.*)(fig+.*)\\}\\s*$", 
                   "IRkernel::set_plot_options\\(\\2\\3\\)\n\\1\\2\\3\\}", 
                   readLines(filename))
      text <- gsub("(IRkernel::set_plot_options\\()([a-zA-Z]+.*)(width+.*|height+.*)fig.(height+.*|width+.*)\\)", 
                   "```{r}\n\\1\\3\\4\\)\n```", text)
      writeLines(text, con = filename, sep = "\n")
    }

  }
  
  else if(file_ext(filename) == "Rnw") {
    ## extract all code chunk headers with figure dimension settings
    ## and put them in comments
    if (length(grep("IRkernel", readLines(filename), fixed = TRUE)) == 0) {
      text <- gsub("(^\\s*<<)(.*)(fig+.*)>>=.*$", 
                   "IRkernel::set_plot_options\\(\\2\\3\\)\n\\1\\2\\3>>=", readLines(filename))
      text <- gsub("(IRkernel::set_plot_options\\()([a-zA-Z]+.*)(width+.*|height+.*)fig.(height+.*|width+.*)\\)", 
                   "<<\\3\\4>>=\n\\1\\3\\4\\)\n@", text)
      writeLines(text, con = filename, sep = "\n")
    }
  }
  
  else {
    cat("Only Rmd and Rnw formats are supported!")
  }
}
