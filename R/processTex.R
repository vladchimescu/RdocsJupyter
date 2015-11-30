process.tex <- function(filename) {
  replace.environment(filename)
  replace.macros(filename)
  remove.figures(filename)
  
} ## end process.tex()

knit.vignette <- function(filename) {
  ## works for now but this is dangerous...
  opts_chunk$set(eval = TRUE)
  ## Also need to catch that warning from knitr (twice)
  suppressWarnings(knit(filename))
  opts_chunk$set(results = "hide", highlight = FALSE, eval = FALSE)
  suppressWarnings(knit(filename))
  if(dirname(filename) != ".") {
    system2('mv', paste(getwd(), '/', 
                        sub("^([^.]*).*", "\\1", basename(filename)), 
                        ".tex ", dirname(filename), "/", sep = ""))
  } ## end if(dirname(filename != "."))
} ## end knit.vignette()

sweave.vignette <- function(filename) {
  ## not sure if it's such a good idea to use try on Sweave...
    options(prompt=" ", continue=" ")
    try(Sweave(filename, eval = TRUE))
    try(Sweave(filename, eval = FALSE))

    if(dirname(filename) != ".") {
      system2('mv', paste(getwd(), '/', 
                          sub("^([^.]*).*", "\\1", basename(filename)), 
                          ".tex ", dirname(filename), "/", sep = ""))
    } ## end if(dirname(filename) != ".")
    ## this is dangerous...
    options(prompt = "> ", continue = "+ ")
} ## end sweave.vignette

render.markdown <- function(filename) {
  opts_chunk$set(results = "hide", fig.show = "hide")
  render(filename, md_document(variant = "markdown_github+tex_math_dollars"))
} ## end render.markdown


#' @title Extract figure parameters from code chunks.
#' @description \code{extract.fig.params} extracts figure width and height from
#' code chunks
#' @param filename File name of the vignette. Absolute path has to be prepended
#' if the document is not in R working directory
#' @param type Vignette type. Valid values are "Rmd" for R markdown and "Rnw"
#' for Sweave and knitr vignette
#' @details The function extracts figure parameters from code chunks and 
#' places these figure options right before the code chunk, from which the
#' figure dimensions were read out. 
#' \cr\cr Suppose, there is a code chunk
#' in which a figure is plotted and the figure's width and height are set as follows:
#' \cr\cr\code{<<someplot, fig.width=6, fig.height=4.5>>=\cr
#' plot(x, f(x))\cr
#' @@ }
#' \cr\cr The function \code{extract.fig.params} would extract width and height
#' and pass them to \code{set_plot_options} function from package \pkg{IRkernel}.
#' Thus the former code chunk will be preceded by the following : \cr\cr
#' \code{<<>>=\cr
#' IRkernel::set_plot_options(width = 6, height = 4.5)\cr
#' @@}
#' \cr\cr The purpose of this code chunk is to make sure the aspect ratio of the
#' figure is communicated to Jupyter Notebook via R kernel (\pkg{IRkernel})
extract.fig.params <- function(filename, type) {
  if(type == "Rmd") {
    if (length(grep("IRkernel", readLines(filename), fixed = TRUE)) == 0) {
      text <- gsub("(^[\t >]*```+\\s*\\{[.]?)([a-zA-Z]+.*)(fig+.*)\\}\\s*$", 
                   "IRkernel::set_plot_options\\(\\2\\3\\)\n\\1\\2\\3\\}", 
                   readLines(filename))
      text <- gsub("(IRkernel::set_plot_options\\()([a-zA-Z]+.*)(width+.*|height+.*)fig.(height+.*|width+.*)\\)", 
                   "```{r, eval=FALSE}\n\\1\\3\\4\\)\n```", text)
      writeLines(text, con = filename, sep = "\n")
    } ## end if (length(...))
  } ## end if (type == "Rmd")
  
  else {
    if (length(grep("IRkernel", readLines(filename), fixed = TRUE)) == 0) {
      text <- gsub("(^\\s*<<)(.*)(fig+.*)>>=.*$", 
                   "IRkernel::set_plot_options\\(\\2\\3\\)\n\\1\\2\\3>>=",
                   readLines(filename))
      text <- gsub("(IRkernel::set_plot_options\\()([a-zA-Z]+.*)(width+.*|height+.*)fig.(height+.*|width+.*)\\)", 
                   "<<\\3\\4>>=\n\\1\\3\\4\\)\n@", text)
      writeLines(text, con = filename, sep = "\n")
    } ## end if(length...)
  } ## end else 
} ## end extract.fig.params() function

run.notedown <- function(filename) {
  ## check if notedown is installed
  notedown.installed <- system2('notedown', '--version', stdout = FALSE)==0
  if(notedown.installed) {
    
    if (dirname(filename) == ".") {
      out_name <- paste(getwd(), "/", sub("^([^.]*).*", "\\1", basename(filename)),
                        sep = "")
      in_name <- paste(getwd(), "/", sub("^([^.]*).*", "\\1", 
                                         basename(filename)), ".md", sep = "")
    }
    
    else {
      out_name <- paste(dirname(filename), "/", sub("^([^.]*).*", "\\1", basename(filename)),
                        sep = "")
      in_name <- paste(dirname(filename), "/",
                       sub("^([^.]*).*", "\\1", basename(filename)), 
                       ".md", sep = "")
    }
    
    
    ## also test if system(...) command was successful and if yes
    ## print a message(): created a file dirname/file.ipynb
    if(system2('notedown', paste('-o ', out_name, '.ipynb --nomagic ',
                                 in_name, sep = "")) == 0)
      if(dirname(filename) == ".") {
        message(paste("Wrote ", basename(out_name), ".ipynb in ", getwd(), sep = ""))
      }
      else {
      message(paste("Wrote ", basename(out_name), ".ipynb in ", dirname(filename), sep = ""))
      } ## end else (dirname != ".")
    else {
      message("Error: notedown exited with non-zero status")
    } ## end else (notedown not successful)
    
    vignette <- paste(out_name, ".ipynb", sep = "")
    # produce Dockerfile and packages.R script
    produce.dockerfile(vignette)
    if(dirname(filename) == ".") {
      message(paste("Produced Dockerfile and packages.R in", getwd()))
    }
    else {
      message(paste("Produced Dockerfile and packages.R in", dirname(filename)))
    }
  } ## end if(notedown.status)
  
  
  else {
    message("notedown executable not found.\n You need notedown utility for md -> ipynb conversion")
  } ## end else ...
  
} ## end run.notedown() function

run.pandoc <- function(filename) {
  pandoc.installed <- system2('pandoc', '--version', stdout = FALSE) == 0
  if(pandoc.installed) {
    if (dirname(filename) == ".") {
      in_name <- paste(getwd(), "/", sub("^([^.]*).*", "\\1", 
                                         basename(filename)), ".tex", sep = "")
      out_name <- paste(getwd(), "/", sub("^([^.]*).*", "\\1", basename(filename)),
                        sep = "")
    }
    
    else {
      out_name <- paste(dirname(filename), "/", sub("^([^.]*).*", "\\1", basename(filename)),
                        sep = "")
      in_name <- paste(dirname(filename), "/",
                       sub("^([^.]*).*", "\\1", basename(filename)), 
                       ".tex", sep = "")
    }
    
    if(system2('pandoc', paste(' -s --toc --to=markdown_github+tex_math_dollars ',
                               in_name, ' -o ',
                               out_name, '.md', sep = "")) == 0)
      if(dirname(filename) == ".") {
        message(paste("Wrote ", basename(out_name), ".md in ", getwd(), sep = ""))
      }
      else {
        message(paste("Wrote ", basename(out_name), ".md in ", dirname(filename), sep = ""))
      }
    else {
      message("Error: pandoc exited with non-zero status")
    }
    
  } ## end if (pandoc.installed)
  
  
  else {
    message("pandoc executable not found. For TeX -> Md conversion pandoc has to be installed!")
  } ## end else (!pandoc.installed)
  
} ## end run.pandoc() function

#' @title Convert vignettes into Jupyter notebooks.
#' @description \code{process.vignette} converts R markdown, Sweave and knitr vignettes
#' into Jupyter notebook format and automatically produces a Dockerfile for running
#' the notebook in Docker virtual environment.
#' @param filename  File name of the R documentation (.Rmd or .Rnw) to be converted. 
#' Absolute path has to be prepended if the file is not in the working directory.
#' The file extension must be included in the file name.
#' @details The function expects as input the file name of the .Rmd or .Rnw documentation 
#' that needs to be converted to Jupyter notebook format. Absolute path has to be prepended
#'  if the file is not in the working directory.
#' The file extension must be included in the file name.
#' \cr\cr R markdown (.Rmd) vignettes are first rendered to Markdown Github and 
#' subsequently converted with the \strong{notedown}
#' command line utility to .ipynb format.
#' The produced Jupyter notebook is scanned for dependencies using \code{get.dependencies}
#' function and an R script \emph{packages.R} is produced. \emph{packages.R} and the
#' Jupyter notebook are used to make a Docker image for the vignette.
#' \cr\cr Note, however, that the Docker image has to be built first from the produced
#' Dockerfile. All generated files, i.e. Jupyter notebook, packages.R and Dockerfile 
#' are written in the \strong{same} directory where the input file is located.
#' In order to build a Docker image for running the vignette you don't need 
#' IPython Notebook installed on your machine. If you installed Docker,
#' simply run in command line \cr\cr
#' \code{$ docker build -t imagename .} 
#'  \cr\cr in the same directory as the Dockerfile. After building the Docker image for
#' your Jupyter notebook, check the operating system-specific commands for running 
#' Jupyter notebooks in Docker containers on the following
#'  web page \url{https://hub.docker.com/r/vladkim/rnaseq/}
#'  \cr\cr Similarly, Sweave and knitr vignettes are compiled to a TeX file, which
#'  is in turn converted to Markdown with \strong{pandoc}. The remaining processing
#'  steps are identical to those for R markdown vignettes.

process.vignette <- function(filename) {
  if(file_ext(filename) == "Rmd") {
    
    ## extract figure parameters from code chunks
    extract.fig.params(filename, "Rmd")
    
    ## render Markdown file
    render.markdown(filename)
    
    ## use notedown utility to convert the md file to ipynb
    run.notedown(filename)
    
  } ## end if (Rmd)
  
  else if(file_ext(filename) == "Rnw") {
    
    extract.fig.params(filename, "Rnw")
    
    ## here process Sweave and knitr vignettes separately
    repeat {
    vignette_type <- readline("Is this a knitr or Sweave vignette? \nknitr  --- enter 1\nSweave --- enter 2\n ")
    if(vignette_type == 1 || vignette_type == 2) {
      break
    }
    message("Please enter 1 or 2 to choose knitr or Sweave!")
    } ## end repeat
    
    if(as.character(vignette_type) == 1) {
      knit.vignette(filename)
    } ## end if (knitr)
    
    else if(as.character(vignette_type) == 2) {
      ## Sweave the vignette
      sweave.vignette(filename)
      
    } ## end else if (Sweave)
    
    ## after generating a TeX file, process it
    process.tex(paste(dirname(filename), "/",
                     sub("^([^.]*).*", "\\1", basename(filename)), ".tex", sep = ""))
    run.pandoc(filename)    
    
    ## right now this is not strictly speaking right
    run.notedown(filename)
    
    
  } ## end else if (Rnw)
  
  else {
    message("Only Rmd and Rnw formats are supported!")
  } ## end else (if !"Rmd" && !"Rnw")
} ## end process.vignette()
