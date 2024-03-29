% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_variables.R
\name{plot_categorical}
\alias{plot_categorical}
\title{Plot categorical variables in a data frame}
\usage{
plot_categorical(
  x,
  vars = colnames(x),
  by = NULL,
  dictionary = NULL,
  col = c("#4F81BD", "#FAAB18", "#868686FF", "#CD534CFF"),
  title = "",
  xlab = NULL,
  wrap_length = 10
)
}
\arguments{
\item{x}{A data frame to plot variable(s) from}

\item{vars}{A vector of variables to plot frequency of}

\item{by}{Split bars by another variable in the data}

\item{dictionary}{Optional data dictionary to enumerate all choices (even those
not in the data).}

\item{col}{A vector of colors (or list of vectors, for multiple vars) to use.}

\item{title}{A title for the plot(s)}

\item{xlab}{A label for the x axis (or vector of labels if multiple vars). Default
is the \code{\link[labelled:var_label]{labelled::var_label()}} or \code{vars} otherwise.}

\item{wrap_length}{The length of string to use for the x axis (wrapped to this length).}
}
\value{
A \code{\link[ggplot2:ggplot]{ggplot2::ggplot()}} representing the categorical counts of variables.
}
\description{
Plot categorical variables in a data frame
}
\examples{
\dontrun{
plot_categorical(ToothGrowth, vars=c("dose"), by="supp")
}
}
