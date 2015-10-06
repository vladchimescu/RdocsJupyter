getDependencies <- function(filename) {
  quotes <- length(grep(".*library\\(\\\\\"(.*?)\\\\\"\\).*", readLines(filename), value = TRUE)) != 0
  if(quotes) {
    return (sub(".*library\\(\\\\\"(.*?)\\\\\"\\).*","\\1",
              grep("library\\(",readLines(filename),value=TRUE)))
  }
  
  else {
    return (sub(".*library\\((.*?)\\).*","\\1",
                grep("library\\(",readLines(filename),value=TRUE)))
  }
}

produceDockerfile <- function(vignette) {
  pkgs <- getDependencies(vignette)
  text <- paste("source('http://bioconductor.org/biocLite.R')\npkgs <- c(", 
      paste(shQuote(pkgs, type="cmd"), collapse=", "), ")\nbiocLite(pkgs)")
  writeLines(text, con = paste(dirname(vignette), "/packages.R", sep = ""))
  text <- paste("FROM vladkim/ipynb:latest",
                "\nCOPY packages.R ./",
                "\nRUN R < packages.R --no-save && rm packages.R",
                "\nCOPY ", basename(vignette), " ./", sep="")
  writeLines(text, con = paste(dirname(vignette), "/Dockerfile", sep = ""))
}


