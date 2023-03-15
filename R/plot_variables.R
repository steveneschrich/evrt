#' Plot categorical variables in a data frame
#'
#' @param x A data frame to plot variable(s) from
#' @param vars A vector of variables to plot frequency of
#' @param by Split bars by another variable in the data
#' @param dictionary Optional data dictionary to enumerate all choices (even those
#'   not in the data).
#' @param col A vector of colors (or list of vectors, for multiple vars) to use.
#' @param title A title for the plot(s)
#' @param xlab A label for the x axis (or vector of labels if multiple vars). Default
#'              is the [labelled::var_label()] or `vars` otherwise.
#' @param wrap_length The length of string to use for the x axis (wrapped to this length).
#' @return A [ggplot2::ggplot()] representing the categorical counts of variables.
#' @export
#'
#' @examples
#' \dontrun{
#' plot_categorical(ToothGrowth, vars=c("dose"), by="supp")
#' }
plot_categorical <- function(x, vars = colnames(x),
                             by = NULL, dictionary = NULL,
                             col = c("#4F81BD", "#FAAB18","#868686FF","#CD534CFF"),
                             title = "",
                             xlab = NULL,
                             wrap_length=10
                             ) {
  if ( is.null(xlab) )  xlab <- default_labels(x, vars)

  # Force vars to factor for plotting
  x <- dplyr::mutate(
    x,
    dplyr::across(dplyr::all_of(vars), as.factor),
    dplyr::across(dplyr::all_of(vars), \(.x) {fct_wrap(.x,width=wrap_length)})
  )

  # NB: Future. If length(vars)>1 and col is not a list of vectors, replicate the
  # col across length(vars), then we can use purrr::map2 to have unique colors per
  # variable.

  # The plots are different if there is a by option
  plot_list <- purrr::imap(vars, \(v, i) {
    if ( is.null(by) )
      fill_var <- v
    else
      fill_var <- by
    if (is.list(col) && length(col) == length(vars))
      pcol <- col[[i]]
    else
      pcol <- col
    if ( length(xlab) > 1)
      xlabel <- xlab[i]
    else
      xlabel <- xlab

    ggplot2::ggplot(x, ggplot2::aes(x = !!rlang::sym(v), fill = !!rlang::sym(fill_var))) +
      # Barplot with narrower bars. Note the position dodge is required
      # with grouping variables. Also, the preserve=single keeps the
      # width of a single bar the same (not expanding to the full width
      # on missing data).
      ggplot2::geom_bar(
        width = 0.5,
        position = ggplot2::position_dodge(0.5, preserve="single")
      ) +
      ggplot2::ggtitle(title) +
      ggplot2::ylab("Count") +
      # The fill scale is to specifiy colors. Note that we should
      # not drop empty factors in the fill or the x axis bars.
      ggplot2::scale_fill_manual(values = pcol, drop = FALSE) +
      ggplot2::scale_x_discrete(drop=FALSE) +
      ggplot2::xlab(xlabel) +
      style_ggplot()

  })

  # Patchwork the plots together (one under the other)
  p <- patchwork::wrap_plots(plot_list, ncol = 1)

  p

}


#' Plot numerical variables in a data frame
#'
#' @param x A data frame to plot variable(s) from
#' @param vars
#' @param by
#'
#' @return
#' @export
#'
#' @examples
plot_numeric <- function(x, vars, by=NULL, dictionary=NULL,
                         wrap_length = 35,
                         threshold = 3,
                         title = "",
                         col = c("#4F81BD", "#FAAB18","#868686FF","#CD534CFF"),
                         xlab = NULL) {

  if ( is.null(xlab) )  xlab <- default_labels(x, vars)


  calculate_numerical_summaries(x,vars = vars,by = by) |>
    plot_variables_as_numeric(by=by, dictionary=dictionary,
                              wrap_length=wrap_length,
                              threshold=threshold, title=title, col=col,xlab= xlab)
}
#' Title
#'
#' @param x A data frame to plot variable(s) from
#' @param dictionary
#' @param by
#' @param default_wrap_length
#' @param threshold
#' @param title A title for the plot
#' @param col Colors for the plot
#' @return
#' @export
#'
#' @examples
plot_variables_as_numeric <- function(x,
                                        dictionary=NULL,
                                        by=NULL,
                                        wrap_length = 35,
                                        threshold = 3,
                                        title = "",
                                        col = c("#4F81BD", "#FAAB18","#868686FF","#CD534CFF"),
                                      xlab = NULL
) {


  if (!is.null(by)) by <- rlang::ensym(by)
  # Turn the label into a factor to keep it from getting out of order. Probably
  # should also use the variable name if the label isn't present.
  x <- dplyr::mutate(x, label = forcats::fct_inorder(.data$label))

  # Wrap the labels to a reasonable width for printing
  x <- dplyr::mutate(x, label = fct_wrap(label, width = wrap_length))
  #default_fill <- "#1380A1"
  #default_fill <- "#4F81BD"

  # From the dictionary, get all factor levels of the variable(s).
  factor_levels <- NULL
  if (!is.null(dictionary))
    factor_levels<-get_factor_levels(
      x,
      vars = x$variable,
      dictionary = dictionary
    )

  g <- ggplot2::ggplot(x, ggplot2::aes(y=forcats::fct_rev(label), x=mean,
                                       xmin = mean - se, xmax = mean + se))

  # There are a variety of settings that are different if the "by" parameter was used,
  # so special-case the plot for these two conditions.
  if (!is.null(by)) {
    g <- g +
      ggplot2::aes(fill = forcats::fct_rev(!!by)) +
      ggplot2::geom_errorbar(width=0.2, size =1, position=ggplot2::position_dodge(width=0.9)) +
      ggplot2::geom_bar(stat="identity",position=ggplot2::position_dodge(width=0.9))
  }  else {
    g <- g +
      ggplot2::geom_errorbar(width=0.2, size = 1) +
      ggplot2::geom_bar(stat = "identity", fill = col[1])
  }
#c("#4F81BD", "#FAAB18","#868686FF","#CD534CFF")
  g<- g +
    ggplot2::geom_hline(yintercept = 0, size = 1, colour="#333333") +
    ggplot2::scale_fill_manual(values =  col) +#,
#                               guide = ggplot2::guide_legend(reverse=TRUE)) +
    ggplot2::geom_vline(xintercept = threshold, linetype = "dashed") +
    ggplot2::labs(
      caption= stringr::str_replace_all(factor_levels, pattern="; ", replacement = "\n"),
      title = title) +
    ggplot2::ylab("") +
    ggplot2::xlab("") +

    #    ggplot2::coord_cartesian(xlim=c(1,5)) +

    # Figure out how to make ranges on the axis that agree with the scales of the var, f.
    # Also, we need the labels!
    #ggplot2::xlim(c(0,6)) +
    style_ggplot()

  g

}


#' Plot variable counts
#'
#' @param x A data frame to plot variable(s) from
#' @param dictionary
#' @param by
#' @param threshold
#' @param wrap_length
#' @param title A title for the plot
#' @param col Colors for the plot
#' @return
#' @export
#'
#' @examples
plot_threshold<- function(x, vars,
                                       dictionary = NULL,
                                       by=NULL,
                                       wrap_length = 35,
                                       threshold = "",
                                       title = "Response",
                          xlab = NULL,
                                       col = c("#4F81BD", "#FAAB18","#868686FF","#CD534CFF")
) {

  # Set xlab to values if labels not given
  if ( is.null(xlab) )  xlab <- default_labels(x, vars)
  f<-get_factor_levels(x, vars, dictionary)

  xs <- evrt::calculate_counts_at_or_above_threshold(
    x,
    vars = vars,
    by = by,
    threshold = threshold
  )

  # This is magic to turn string to expression, or leave as expression.
  if (!is.null(by)) by <- rlang::ensym(by)

  # Turn the label into a factor to keep it from getting out of order. Probably
  # should also use the variable name if the label isn't present.
  xs <- dplyr::mutate(xs, label = forcats::fct_inorder(.data$label))

  # x has count, n and percent. label gives the info.
  # NOTENOTENOTE:::: The N is not guaranteed to be constant, how to fix this for
  ## the label????
  n <- xs |>
    dplyr::select(n) |>
    dplyr::distinct() |>
    dplyr::pull("n")
  # Wrap the labels to a reasonable width for printing. Note that this
  # bashes the factor so we need to do this intelligently.
  xs <- dplyr::mutate(xs, label = fct_wrap(.data$label, wrap_length))

  default_fill <- "#1380A1"
  default_fill <- "#4F81BD"
  #if (is.null(by)) by <- default_fill
  #fill = "#1380A1
  g<- ggplot2::ggplot(xs, ggplot2::aes(y = forcats::fct_rev(label), x=percent,
                                      fill = forcats::fct_rev(!!by)))

  # ggplot2::geom_bar(stat="identity",position=position_dodge(width=0.9))
  if (is.null(by)) {
    g <- g +
      ggplot2::geom_bar(stat = "identity",  fill=col[1])
  }  else {
    g <- g +
      ggplot2::geom_bar(stat="identity",position=ggplot2::position_dodge(width=0.9))
  }
  g<- g +
    ggplot2::geom_hline(yintercept = 0, size = 1, colour="#333333") +
    ggplot2::scale_fill_manual(values = col)+#,
#                               guide = ggplot2::guide_legend(reverse=TRUE)) +
    # Set x scale as percentages (0-100%).
    ggplot2::scale_x_continuous(labels = scales::percent, limits = c(0,1)) +

    # Turn off x/y axes labels
    ggplot2::ylab("") +
    ggplot2::xlab("") +

    # Annotate the plot: this includes
    #  - description of scale in subtitle
    #  - scale in the caption
    ggplot2::labs(caption=stringr::str_replace_all(f, pattern="; ", replacement = "\n"),
                  title = stringr::str_wrap(title, wrap_length),
                  subtitle = stringr::str_wrap(
                    glue::glue("% Respondents Exceeding '{threshold}' Threshold (N={n})"),
                    wrap_length)) +

    # Apply standard style for the library
    style_ggplot()

  g

}


#' Plot yes/no variable frequency of yes
#'
#' @param x A data frame to plot variable(s) from
#' @param vars A list of column names to plot
#' @param by A grouping column
#' @param wrap_length Length of factor level to wrap strings at
#' @param title A title for the plot
#' @param col Colors for the plot
#' @return
#' @export
#'
#' @examples
plot_yesno<- function(x, vars,
                                   by=NULL,
                                   wrap_length = 35,
                                   xlab = NULL,
                                   title = "",
                                   col = c("#4F81BD", "#FAAB18","#868686FF","#CD534CFF")
) {

  # Set xlab to values if labels not given
  if ( is.null(xlab) )  xlab <- default_labels(x, vars)

  # Calculate summary information for plot
  xs <- calculate_counts_yesno(x, vars = vars, by = by)

  # This is magic to turn string to expression, or leave as expression.
  if (!is.null(by)) by <- ensym(by)

  # Turn the label into a factor to keep it from getting out of order. Probably
  # should also use the variable name if the label isn't present.
  xs <- dplyr::mutate(xs, label = forcats::fct_inorder(.data$label))

  # For plotting, everything starts at the wrong direction.
  # xs has count, n and percent. label gives the info.
  n <- xs |>
    dplyr::select(n) |>
    dplyr::distinct() |>
    dplyr::pull("n")

  # Wrap the labels to a reasonable width for printing
  xs <- xs |>
    dplyr::mutate(label = fct_wrap(label, wrap_length))
  default_fill <- "#1380A1"
  default_fill <- "#4F81BD"
  #if (is.null(by)) by <- default_fill
  #fill = "#1380A1
  g<- ggplot2::ggplot(xs, ggplot2::aes(y=forcats::fct_rev(label), x=percent,
                                      fill = forcats::fct_rev({{by}})))

  # ggplot2::geom_bar(stat="identity",position=position_dodge(width=0.9))
  if (is.null(by)) {
    g <- g +
      ggplot2::geom_bar(stat = "identity",  fill=col[1])
  }  else {
    g <- g +
      ggplot2::geom_bar(stat="identity",position=ggplot2::position_dodge(width=0.9))
  }
  g<- g +
    ggplot2::geom_hline(yintercept = 0, linewidth = 1, colour="#333333") +
    ggplot2::scale_fill_manual(values =  col) + #,
  #                             guide = ggplot2::guide_legend(reverse=TRUE)) +
    ggplot2::scale_x_continuous(labels = scales::percent) +
    # Dont do this, put in the top the percent repsondents exceeding threhsold (somewhere)
    ggplot2::labs(title = title,
                  subtitle = glue::glue("% Respondents Answering 'Yes' (N={n})")) +
    ggplot2::ylab("") +
    ggplot2::xlab("") +

    # ggplot2::coord_cartesian(xlim=c(1,5)) +

    # Figure out how to make ranges on the axis that agree with the scales of the var, f.
    # Also, we need the labels!
    #ggplot2::xlim(c(0,6)) +
    style_ggplot()

  g

}







