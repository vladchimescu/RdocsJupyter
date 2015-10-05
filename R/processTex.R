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
    } ## end if (length(...))
    
    ## check if opts_chunk$set(..., eval = TRUE)  
    ## normally it is. then render once with eval = TRUE and 
    ## the second time with eval = FALSE
    opts_chunk$set(eval = TRUE)
    render(filename, md_document(variant = "markdown_github+tex_math_dollars"))
    opts_chunk$set(eval = FALSE)
    render(filename, md_document(variant = "markdown_github+tex_math_dollars"))
    
    ## check if notedown is installed
    notedown.status <- system('notedown --version')==0
    if(notedown.status) {
      out_name <- sub("^([^.]*).*", "\\1", basename(filename))
      if (dirname(filename) == ".") {
        in_name <- paste(getwd(), "/", sub("^([^.]*).*", "\\1", basename(filename)), ".md", sep = "")
      }
      
      else {
        in_name <- paste(dirname(filename), "/", sub("^([^.]*).*", "\\1", basename(filename)), ".md", sep = "")
      }
      
      
      ## also test if system(...) command was successful and if yes
      ## print a message(): created a file dirname/file.ipynb
      if(system2('notedown', paste('-o ', out_name, '.ipynb --nomagic ', in_name, sep = "")) == 0)
        message(paste("Wrote ", out_name, ".ipynb in ", getwd(), sep = ""))
      else {
        message("Error: notedown exited with non-zero status")
      }
    } ## end if(notedown.status)
    
    
    else {
      message("notedown executable not found.\n You need notedown utility for md -> ipynb conversion")
    } ## end else ...
    

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
    message("Only Rmd and Rnw formats are supported!")
  }
}
