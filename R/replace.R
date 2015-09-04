## replace LaTeX environments (usually have to be removed
## or simplified)
replaceEnv <- function(filename) {
  text <- readLines(file.path(getwd(), filename))
  text <- gsub("\\usepackage{Sweave}", "\\usepackage{listings}\n",
               text, fixed=TRUE)
  text <- gsub("\\usepackage{alltt}", "\\usepackage{listings}\n", text, fixed=TRUE)
  if(length(grep("%\\usepackage{listings}", text, fixed=TRUE))!=0) {
    text <- gsub("%\\usepackage{listings}", "\\usepackage{listings}", text, 
         fixed=TRUE)
  }
  if(length(grep("\\usepackage{listings}", text, fixed=TRUE))==0) {
    text <- gsub("\\begin{document}", "\\usepackage{listings}\n\\begin{document}",
                 text, fixed=TRUE)
  }
  # replace environments
  text <- gsub("Sinput", "lstlisting", text)
  text <- gsub("\\begin{Schunk}", "", text, fixed=TRUE)
  text <- gsub("\\end{Schunk}", "", text, fixed=TRUE)
  text <- gsub("\\begin{knitrout}", "", text, fixed=TRUE)
  text <- gsub("\\end{knitrout}", "", text, fixed=TRUE)
  text <- gsub("\\begin{kframe}", "", text, fixed=TRUE)
  text <- gsub("\\end{kframe}", "", text, fixed=TRUE)
  # right now don't know how to distinguish simple verbatim from
  # verbatim-enclosed code
  text <- gsub("{verbatim}", "{lstlisting}", text, fixed=TRUE)
  text <- gsub("\bioccoment", "", text, fixed=TRUE)
  text <- gsub("\warning", "", text, fixed=TRUE)
  text <- gsub("\fixme", "", text, fixed=TRUE)
  text <- gsub("\prefix", "", text, fixed=TRUE)
  writeLines(text, con=filename, sep = "\n")
}

## replace Bioconductor.sty macros (such as \Biocpkg{})
replaceMacros <- function(filename) {
  text <- readLines(file.path(getwd(), filename))
  # replace macros
  text <- gsub("\\R{}~", "\\emph{R} ", text, fixed=TRUE)
  text <- gsub("\\R~", "R ", text, fixed=TRUE)
  text <- gsub("\\R{}", "R ", text, fixed=TRUE)
  text <- gsub("Biocpkg", "emph", text, fixed=TRUE)
  text <- gsub("CRANpkg", "emph", text, fixed = TRUE)
  text <- gsub("Githubpkg", "emph", text, fixed=TRUE)
  text <- gsub("Biocannopkg", "emph", text, fixed=TRUE)
  text <- gsub("Biocexptpkg", "emph", text, fixed=TRUE)
  text <- gsub("Robject", "texttt", text)
  text <- gsub("Rfunction", "texttt", text)
  text <- gsub("Rclass", "textbf", text)
  # text <- gsub("\\includegraphics", "%\\includegraphics", text, fixed=TRUE)
  text <- gsub("\\software", "\\textbf", text, fixed=TRUE)
  text <- gsub("\\warning", "\\emph", text, fixed=TRUE)
  text <- gsub("\\Bioconductor{}", "Biconductor", text, fixed=TRUE)
  writeLines(text, con=filename, sep = "\n")
}

## remove all figures
removeFigures <- function(filename) {
  text <- readLines(file.path(getwd(), filename))
  text <- deleteBetween("\\\\begin\\{figure\\}", "\\\\end\\{figure\\}", text)
  # text <- gsub("(\\\\begin\\{figure\\})(.*?)(\\\\end\\{figure\\})", "", text)
  text <- gsub("\\incfig((.|\n)*+)", "\\emph\\{Caption:", text, perl = TRUE)
  writeLines(text, con=filename, sep = "\n")
}

# deletes everything between start and end points
# of the TeX environment. Start and end have to be
# regexp that can be passed to grep() function
deleteBetween <- function(start, end, text) {
  start_indices <- grep(start, text)
  end_indices <- grep(end, text)
  
  two <- function(start, end) {
    (start:end)
  }
  vecTwo <- Vectorize(two)
  toRemove <- unlist(vecTwo(start_indices, end_indices))
  if(length(toRemove!=0)) {
    return (text[-toRemove])
  }
  else {
    return (text)
  }
}