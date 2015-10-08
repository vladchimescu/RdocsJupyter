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

#' @title Produce a Docker image for Jupyter notebook.
#' @description \code{produce.dockerfile} generates a Dockerfile
#' which can be used to build a Docker image for running Jupyter 
#' notebook in Docker without the local installation of Jupyter notebook
#' @param vignette Name of the Jupyter notebook file. Absolute path has to
#' be prepended if the notebook is not in R working directory.
#' @details The function scans the Jupyter notebook for R package dependencies and
#' produces a script \emph{packages.R} for installing these R packages in the Docker 
#' image.
#' \cr\cr The Docker image is built from the generated
#' Dockerfile, which is written in the same directory as the Jupyter notebook.
#' In order to build a Docker image for running the vignette you don't need 
#' IPython Notebook installed on your machine. If you installed Docker,
#' simply run in command line \cr\cr
#' \code{$ docker build -t imagename .} 
#'  \cr\cr in the same directory as the Dockerfile. After building the Docker image for
#' your Jupyter notebook, check the operating system-specific commands for running 
#' Jupyter notebooks in Docker containers on the following
#'  web page \url{https://hub.docker.com/r/vladkim/rnaseq/}
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


#' @title Extract R package dependencies from DESCRIPTION file
#' @description \code{get.deps.from.description} extracts R package dependencies
#' from the package DESCRIPTION file and writes \emph{packages.R} script to
#' install R package dependencies in the Docker image. See \strong{Details} for exact
#' usage
#' @param dir Directory where DESCRIPTION file is located
#' @details The function is used to generate \emph{packages.R} script that lists
#' R package dependencies that need to be installed in Docker image for running the
#' Jupyter notebook under Docker. \cr\cr
#' The user should use this function, only when the automatically generated \emph{packages.R}
#' script is incomplete or malformatted.
get.deps.from.description <- function(dir) {
  message(paste("Searching for DESCRIPTION file in\n", dir, "\n"))
  
  # error message
  e <- "No DESCRIPTION file in current directory\n"
  if(file.exists(file.path(dir, "DESCRIPTION"))) {
    deps <- read.dcf(file = file.path(dir, "DESCRIPTION"), 
                     fields = c("Depends", "Suggests",
                                "Imports", "LinkingTo"))
  }
  
  else {
    stop(e)
  }
  
  dim(deps) <- NULL
  deps <- paste(deps[!is.na(deps)], seps = "", collapse = ",")
  deps <- gsub("\\n", " ", deps)
  deps <- gsub(",", "", deps)
  # remove all version numbers in parentheses
  regmatches(deps, gregexpr("(?=\\().*?(?<=\\))", deps, perl=T))[[1]] <- ""
  deps <- unlist(strsplit(deps, " "))
  deps <- deps[deps!="R" & deps!="R " & deps!=""]
  text <- paste("source('http://bioconductor.org/biocLite.R')\npkgs <- c(", 
                paste(shQuote(deps, type="cmd"), collapse=", "), ")\nbiocLite(pkgs)")
  writeLines(text, con = paste(dir, "/packages.R", sep = ""))
  message(paste("Wrote 'packages.R' in", dir))
} # end of getDependencies() function


