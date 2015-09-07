# error message
e <- "No DESCRIPTION file in current directory\n"
w <- "You need to provide a valid path of the directory 
with the DESCRIPTION file or provide the name 
of the package installed on your system"
openFile <- function(pkg, dir) {
  if(file.exists(file.path(dir, "DESCRIPTION"))) {
    deps <- read.dcf(file = file.path(dir, "DESCRIPTION"), 
                     fields = c("Depends", "Suggests",
                                "Imports", "LinkingTo"))
    return(deps)
  }
  else if (file.exists(system.file("DESCRIPTION", package = pkg))) {
    deps <- read.dcf(file = system.file("DESCRIPTION", package = toString(pkg)), 
                     fields = c("Depends", "Suggests",
                                "Imports", "LinkingTo"))
    return(deps)
  }
  else {
    stop(e)
  }
}

getDependencies <- function(pkg, dir=getwd()) {
 if(missing(pkg)) {
     message(paste("Searching for DESCRIPTION file in\n", dir, "\n"))
 }
 if(missing(dir) & !missing(pkg)) {
    message(paste("Searching DESCRIPTION file in\n", pkg, "package lib directory\n"))
 }
 tryCatch(
    { deps <- openFile(pkg, dir)
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


produceDockerfile <- function(dirc, vignette) {
  pkgs <- getDependencies(dir = dirc)
  text <- paste("source('http://bioconductor.org/biocLite.R')\npkgs <- c(", 
      paste(shQuote(pkgs, type="cmd"), collapse=", "), ")\nbiocLite(pkgs)")
  write(text, file="packages.R")
  text <- paste("FROM vladkim/ipynb:latest",
                "\nCOPY packages.R ./",
                "\nRUN R < packages.R --no-save && rm packages.R",
                "\nCOPY ", vignette, " ./", sep="")
  write(text, file="Dockerfile")
  
}


