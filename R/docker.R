# error message
e <- "No DESCRIPTION file in current directory\n"
w <- "You need to provide a valid path of the directory 
with the DESCRIPTION file of the package"
openFile <- function(dir) {
  if(file.exists(file.path(dir, "DESCRIPTION"))) {
    deps <- read.dcf(file = file.path(dir, "DESCRIPTION"), 
                     fields = c("Depends", "Suggests",
                                "Imports", "LinkingTo", "Package"))
    return(deps)
  }

  else {
    stop(e)
  }
}

getDependencies <- function(dir) {
 message(paste("Searching for DESCRIPTION file in\n", dir, "\n"))

 tryCatch(
    { deps <- openFile(dir)
      dim(deps) <- NULL
      deps <- paste(deps[!is.na(deps)], seps = "", collapse = ",")
      deps <- gsub("\\n", " ", deps)
      deps <- gsub(",", "", deps)
      # remove all version numbers in parentheses
      regmatches(deps, gregexpr("(?=\\().*?(?<=\\))", deps, perl=T))[[1]] <- ""
      deps <- unlist(strsplit(deps, " "))
      deps[deps!="R" & deps!="R " & deps!=""]
    },
    
    error = function(e){
      print(e)
      message(w)

    }
  ) # end of tryCatch()
} # end of packageList() function


produceDockerfile <- function(dirc = getwd(), vignette) {
  if(missing(dirc)) {
    dirc <- getwd()
  }
  if(missing(vignette)) {
    stop("Error: please check the provided arguments 'dirc' and 'vignette'.
         'dirc' has to be the path to the DESCRIPTION file and the vignette
         'vignette' is the name of the vignette file")
  }
  pkgs <- getDependencies(dir = dirc)
  text <- paste("source('http://bioconductor.org/biocLite.R')\npkgs <- c(", 
      paste(shQuote(pkgs, type="cmd"), collapse=", "), ")\nbiocLite(pkgs)")
  write(text, file=paste(dirc, "packages.R", sep = ""))
  text <- paste("FROM vladkim/ipynb:latest",
                "\nCOPY packages.R ./",
                "\nRUN R < packages.R --no-save && rm packages.R",
                "\nCOPY ", vignette, " ./", sep="")
  write(text, file=paste(dirc, "Dockerfile", sep = ""))
  
}


