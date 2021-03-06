% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/processTex.R
\name{extract.fig.params}
\alias{extract.fig.params}
\title{Extract figure parameters from code chunks.}
\usage{
extract.fig.params(filename, type)
}
\arguments{
\item{filename}{File name of the vignette. Absolute path has to be prepended
if the document is not in R working directory}

\item{type}{Vignette type. Valid values are "Rmd" for R markdown and "Rnw"
for Sweave and knitr vignette}
}
\description{
\code{extract.fig.params} extracts figure width and height from
code chunks
}
\details{
The function extracts figure parameters from code chunks and
places these figure options right before the code chunk, from which the
figure dimensions were read out.
\cr\cr Suppose, there is a code chunk
in which a figure is plotted and the figure's width and height are set as follows:
\cr\cr\code{<<someplot, fig.width=6, fig.height=4.5>>=\cr
plot(x, f(x))\cr
@ }
\cr\cr The function \code{extract.fig.params} would extract width and height
and pass them to \code{set_plot_options} function from package \pkg{IRkernel}.
Thus the former code chunk will be preceded by the following : \cr\cr
\code{<<>>=\cr
IRkernel::set_plot_options(width = 6, height = 4.5)\cr
@}
\cr\cr The purpose of this code chunk is to make sure the aspect ratio of the
figure is communicated to Jupyter Notebook via R kernel (\pkg{IRkernel})
}

