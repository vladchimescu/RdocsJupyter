process.tex <- function(filename) {
  replaceEnv(filename)
  replaceMacros(filename)
  removeFigures(filename)
  
} ## end process.tex()

knit.vignette <- function(filename) {
  ## need to add some exception handling
  opts_chunk$set(eval = TRUE)
  ## Also need to catch that warning from knitr (twice)
  knit(filename)
  opts_chunk$set(results = "hide", highlight = FALSE, eval = FALSE)
  knit(filename)
  if(dirname(filename) != ".") {
    system2('mv', paste(getwd(), '/', 
                        sub("^([^.]*).*", "\\1", basename(filename)), 
                        ".tex ", dirname(filename), "/", sep = ""))
  } ## end if(dirname(filename != "."))
} ## end knit.vignette()

sweave.vignette <- function(filename) {
  ## need to add some error handling
  options(prompt=" ", continue=" ")
  Sweave(filename, eval = TRUE)
  Sweave(filename, eval = FALSE)
  if(dirname(filename) != ".") {
    system2('mv', paste(getwd(), '/', 
                        sub("^([^.]*).*", "\\1", basename(filename)), 
                        ".tex ", dirname(filename), "/", sep = ""))
  } ## end if(dirname(filename) != ".")
  options(prompt = "> ", continue = "+ ")
} ## end sweave.vignette

render.markdown(filename) {
  ## need to add some error handling
  opts_chunk$set(eval = TRUE)
  render(filename, md_document(variant = "markdown_github+tex_math_dollars"))
  opts_chunk$set(eval = FALSE)
  render(filename, md_document(variant = "markdown_github+tex_math_dollars"))
} ## end render.markdown

extract.fig.params <- function(filename, type) {
  if(type == "Rmd") {
    if (length(grep("IRkernel", readLines(filename), fixed = TRUE)) == 0) {
      text <- gsub("(^[\t >]*```+\\s*\\{[.]?)([a-zA-Z]+.*)(fig+.*)\\}\\s*$", 
                   "IRkernel::set_plot_options\\(\\2\\3\\)\n\\1\\2\\3\\}", 
                   readLines(filename))
      text <- gsub("(IRkernel::set_plot_options\\()([a-zA-Z]+.*)(width+.*|height+.*)fig.(height+.*|width+.*)\\)", 
                   "```{r}\n\\1\\3\\4\\)\n```", text)
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
    message(paste("Produced Dockerfile and packages.R in", dirname(filename)))
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
      print(in_name)
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
