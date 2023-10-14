#' Generate paired table and graph for categorical data with followup
#'
#' @description This is a high-level function wrapping a number of different
#'  activities: summarize categorical variables in a data frame and generate a
#'  summary table and summary graph as a [grid::grob()]. A followup is free text
#'  included as a followup question.
#'
#'  @details This function is a high-level summarization of a great deal of complex
#'  processing code. As such, it is currently likely to be somewhat unstable. Briefly,
#'  this function summarizes categaorical data elements within a data frame. Categorical
#'  elements are listed by level. This function first builds a summary table of the
#'  field(s). A followup question (free text) is included below the summarization
#'  counts. Then a table of these summaries is generated. Additionally, the
#'  fields are then graphed as bar plots. The table and figure are assembled into an
#'  overall figure suitable for plotting. Note that this makes a very large
#'  [patchwork::patchwork()] grob so please take that into consideration when using
#'  a markdown/officedown format.
#'
#'  @note The table output is always a [flextable::flextable].
#'
#' @param x
#' @param vars
#' @param header
#' @param by
#' @param include_overall
#' @param dictionary
#' @param col
#' @param title
#' @param fit
#' @param followup
#' @param xlab
#' @param combine
#'
#' @return
#' @export
#'
#' @examples
gvsummary_categorical_followup <- function(x,
                               vars = colnames(x),
                               followup,
                               followup_statistic = "{n}",

                               by=NULL,
                               group_headers =  c("",""),
                               header = "**Characteristic**",

                               include_overall = FALSE ,
                               dictionary = NULL,
                               col = c("#4F81BD", "#FAAB18","#868686FF","#CD534CFF"),
                               title = "",
                               fit = "fixed",
                               xlab = NULL,
                               combine = FALSE
                               ) {
  if ( is.null(xlab) )  xlab <- default_labels(x, vars)

  ft_tbl <- summarize_categorical_followup_as_flextable(x = x, vars=vars,followup=followup,
                               followup_statistic = followup_statistic,
                                  by = by, header=header,
                               group_headers=group_headers,
                                  include_overall = include_overall
                               )

  nm_plot <- plot_categorical(x = x, vars = vars, by = by, dictionary = dictionary,
                              col = col, title = title, xlab = xlab
  )

  if ( combine ) {
    res <- patchwork::wrap_plots(
      flextable::gen_grob(ft_tbl,fit=fit,scaling="min",just="center"),
      nm_plot, ncol = 1, nrow = 2, guides = "keep"
    )
  } else {
    res <- list(table = ft_tbl, plot = nm_plot)
  }

  res
}



#' Generate paired table and graph for categorical data
#'
#' @description This is a high-level function wrapping a number of different
#'  activities: summarize categorical variables in a data frame and generate a
#'  summary table and summary graph as a [grid::grob()].
#'
#'  @details This function is a high-level summarization of a great deal of complex
#'  processing code. As such, it is currently likely to be somewhat unstable. Briefly,
#'  this function summarizes categaorical data elements within a data frame. Categorical
#'  elements are listed by level. This function first builds a summary table of the
#'  field(s). Then a table of these summaries is generated. Additionally, the
#'  fields are then graphed as bar plots. The table and figure are assembled into an
#'  overall figure suitable for plotting. Note that this makes a very large
#'  [patchwork::patchwork()] grob so please take that into consideration when using
#'  a markdown/officedown format.
#'
#'
#' @param x
#' @param vars
#' @param header
#' @param by
#' @param include_overall
#' @param dictionary
#' @param col
#' @param title
#' @param fit
#' @param xlab A label for the x axis (or vector of labels if multiple vars). Default
#'              is the [labelled::var_label()] or `vars` otherwise.
#' @param combine (FALSE) Should the table and plot be combined into a
#'  single plot output?
#'
#' @return Depending on the value of `combine` either: a list consisting
#'  of two elements: `plot` and `table`; or a single plot combining the
#'  two elements.
#' @export
#'
#' @examples
#' \dontrun{
#' gvsummary_categorical(ToothGrowth, vars="dose",by="supp")
#' }
gvsummary_categorical <- function(x,
                              vars = colnames(x),
                              header = "**Characteristic**",
                              by=NULL,
                              include_overall = FALSE ,
                              dictionary = NULL,
                              col = c("#4F81BD", "#FAAB18","#868686FF","#CD534CFF"),
                              title = "",
                              fit = "fixed",
                              xlab = NULL,
                              combine = FALSE) {

  if ( is.null(xlab) )  xlab <- default_labels(x, vars)

  ft_tbl <- summarize_categorical_as_flextable(x = x, vars=vars,
                                  by = by, header=header,
                                  include_overall = include_overall)

  nm_plot <- plot_categorical(x = x, vars = vars, by = by, dictionary = dictionary,
                              col = col, title = title, xlab = xlab
  )

  if ( combine ) {
    res <- patchwork::wrap_plots(
      flextable::gen_grob(ft_tbl,fit=fit,scaling="min",just="center"),
      nm_plot, ncol = 1, nrow = 2
    )
  } else {
    res <- list(table = ft_tbl, plot = nm_plot)
  }

  res
}



#' Generate paired table and graph for numeric data
#'
#' @description This is a high-level function wrapping a number of different
#'  activities: summarize numeric variables in a data frame and generate a
#'  summary table and summary graph as a [grid::grob()].
#'
#'  @details This function is a high-level summarization of a great deal of complex
#'  processing code. As such, it is currently likely to be somewhat unstable. Briefly,
#'  this function summarizes numeric data elements within a data frame. Numeric data
#'  elements can be averaged. This function first builds a summary table of the numeric
#'  field(s). Then a table of these summaries is generated. Additionally, the numeric
#'  fields are then graphed as bar plots. The table and figure are assembled into an
#'  overall figure suitable for plotting. Note that this makes a very large
#'  [patchwork::patchwork()] grob so please take that into consideration when using
#'  a markdown/officedown format.
#'
#'
#' @param x A data frame to summarize
#' @param vars Variable (column) names in table
#' @param dictionary
#' @param by
#' @param header
#' @param summary
#' @param default_wrap_length
#' @param threshold
#' @param title
#' @param col
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' gvsummary_numeric(iris, vars=c("Sepal.Length","Sepal.Width"), by="Species")
#' }
gvsummary_numeric <- function(x, vars,
                              dictionary = NULL, by = NULL,
                              header = "**Characteristic**",
                              summary = "mean", wrap_length=35,
                              threshold = 3, title = "",
                              col=c("#4F81BD", "#FAAB18", "#868686FF", "#CD534CFF"),
                              fit = "fixed",
                              xlab = NULL,
                              combine = FALSE
) {
  if ( is.null(xlab) )  xlab <- default_labels(x, vars)

  ft_tbl <- summarize_numeric_as_flextable(
    x = x, vars=vars, by = by,
    dictionary = dictionary, header=header, summary=summary
  )

  nm_plot <- plot_numeric(
    x = x, vars = vars, by = by,
    dictionary = dictionary, wrap_length= wrap_length,
    threshold = threshold, title = title, col=col
  )

  if ( combine ) {
    res <- patchwork::wrap_plots(flextable::gen_grob(ft_tbl), nm_plot, ncol = 1, nrow = 2)
  } else {
    res <- list(table = ft_tbl, plot = nm_plot)
  }
  res
}
