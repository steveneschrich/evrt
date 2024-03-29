% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summarize_categorical.R
\name{summarize_categorical}
\alias{summarize_categorical}
\alias{summarize_categorical_as_flextable}
\alias{summarize_categorical_as_gtsummary}
\alias{summarize_categorical_as_rmarkdown}
\title{Create a basic categorical variable breakdown table}
\usage{
summarize_categorical(
  x,
  vars,
  header = "",
  by = NULL,
  include_overall = FALSE,
  output = c("gtsummary", "flextable", "rmarkdown")
)

summarize_categorical_as_flextable(...)

summarize_categorical_as_gtsummary(...)

summarize_categorical_as_rmarkdown(...)
}
\arguments{
\item{x}{The data frame to summarize}

\item{vars}{(default: dplyr::everything) Columns of the data frame
that contain categorical fields}

\item{header}{(default: "\strong{Characteristic}") The summary table header}

\item{by}{Variable to stratify summary by.}

\item{include_overall}{Include a column of overall summary. Note this only
works (and makes sense) when the \code{by} variable is used.}

\item{output}{A string representing the desired output. Choices include:
\itemize{
\item "gtsummary" A \link[gtsummary:gtsummary-package]{gtsummary::gtsummary} object.
\item "flextable" A \link[flextable:flextable]{flextable::flextable} table with formatting applied to a gtsummary table.
\item "rmarkdown" Raw flextable output for use in rmarkdown (see \code{\link[flextable:flextable_to_rmd]{flextable::flextable_to_rmd()}}
for details). This is most suitable for running summarize in a loop or as list component.
}}
}
\value{
A \code{\link[gtsummary:tbl_summary]{gtsummary::tbl_summary()}} object representing the categorical
breakdown of \code{vars} from \code{x}.
}
\description{
Extract variables from the table and format the variables
by value within a summary table.
}
\details{
This function combines (potentially) multiple columns of a
data frame into a single output summary table. This table has the
variable name (or column name if there is no \code{\link[labelled:var_label]{labelled::var_label()}})
followed by a summarization of levels for this variable. If your
variable is numeric, please see \code{\link[=summarize_numeric]{summarize_numeric()}}.
}
\section{Functions}{
\itemize{
\item \code{summarize_categorical_as_flextable()}: Flextable output of summarized categorical table

\item \code{summarize_categorical_as_gtsummary()}: gtsummary output of summarized categorical table

\item \code{summarize_categorical_as_rmarkdown()}: rmarkdown output of summarized categorical table

}}
\examples{
\dontrun{
summarize_categorical(ToothGrowth, vars =c("dose"),by="supp")
}

}
\seealso{
style_categorical_flextable()
}
