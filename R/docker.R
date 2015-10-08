get.dependencies <- function(filename) {
  quotes <- length(grep(".*library\\(\\\\\"(.*?)\\\\\"\\).*", readLines(filename), value = TRUE)) != 0
  if(quotes) {
    return (sub(".*library\\(\\\\\"(.*?)\\\\\"\\).*","\\1",
              grep("library\\(",readLines(filename),value=TRUE)))
  } ## end if(quotes)
  
  else {
    return (sub(".*library\\((.*?)\\).*","\\1",
                grep("library\\(",readLines(filename),value=TRUE)))
  } ## end else
} ## end get.dependencies(filename)

produce.dockerfile <- function(vignette) {
  pkgs <- get.dependencies(vignette)
  text <- paste("source('http://bioconductor.org/biocLite.R')\npkgs <- c(", 
      paste(shQuote(pkgs, type="cmd"), collapse=", "), ")\nbiocLite(pkgs)")
  writeLines(text, con = paste(dirname(vignette), "/packages.R", sep = ""))
  text <- paste("FROM vladkim/ipynb:latest",
                "\nCOPY packages.R ./",
                "\nRUN R < packages.R --no-save && rm packages.R",
                "\nCOPY ", basename(vignette), " ./", sep="")
  writeLines(text, con = paste(dirname(vignette), "/Dockerfile", sep = ""))
} ## end produce.dockerfile()


