#' Title
#'
#' @param x
#' @param vars
#' @param by
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
numeric_plot <- function(x, vars, by=NULL, ...) {
  evrt::calculate_numerical_summaries(x,vars = vars,by = by) |>
    evrt::plot_variables_as_numeric(x, by=by, ...)
}
#' Title
#'
#' @param x A table of variables
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
                                        default_wrap_length = 35,
                                        threshold = 3,
                                        title = "",
                                        col = c("#4F81BD", "#FAAB18","#868686FF","#CD534CFF")
) {

  if (!is.null(by)) by <- rlang::ensym(by)
  # Turn the label into a factor to keep it from getting out of order. Probably
  # should also use the variable name if the label isn't present.
  x <- dplyr::mutate(x, label = forcats::fct_inorder(.data$label))

  # Wrap the labels to a reasonable width for printing
  x<-x %>% dplyr::mutate(label = fct_wrap(label, 35))
  #default_fill <- "#1380A1"
  #default_fill <- "#4F81BD"

  # From the dictionary, get all factor levels of the variable(s).
  factor_levels <- NULL
  if (!is.null(dictionary))
    factor_levels<-get_factor_levels(dictionary, x %>% dplyr::pull("variable"))

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

#' Title
#'
#' @param x
#' @param vars
#' @param threshold
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
threshold_plot <- function(x, vars, by=NULL, threshold, ...) {
  evrt::calculate_counts_at_or_above_threshold(
    x,
    vars = vars,
    by = by,
    threshold = threshold
  ) |>
    evrt::plot_variables_as_threshold(threshold = threshold, by=by, ...)
}
#' Plot variable counts
#'
#' @param x A table of variables
#' @param dictionary
#' @param by
#' @param default_wrap_length
#' @param title A title for the plot
#' @param col Colors for the plot
#' @return
#' @export
#'
#' @examples
plot_variables_as_threshold<- function(x,
                                       dictionary,
                                       by=NULL,
                                       default_wrap_length = 35,
                                       threshold = "",
                                       title = "Response",
                                       col = c("#4F81BD", "#FAAB18","#868686FF","#CD534CFF")
) {
  # This is magic to turn string to expression, or leave as expression.
  if (!is.null(by)) by <- rlang::ensym(by)

  # Turn the label into a factor to keep it from getting out of order. Probably
  # should also use the variable name if the label isn't present.
  x <- dplyr::mutate(x, label = forcats::fct_inorder(.data$label))
  # x has count, n and percent. label gives the info.
  # NOTENOTENOTE:::: The N is not guaranteed to be constant, how to fix this for
  ## the label????
  n <- x %>% dplyr::select(n) %>% dplyr::distinct() %>% dplyr::pull("n")
  # Wrap the labels to a reasonable width for printing. Note that this
  # bashes the factor so we need to do this intelligently.
  x<-x %>% dplyr::mutate(label = fct_wrap(.data$label, default_wrap_length))
  default_fill <- "#1380A1"
  default_fill <- "#4F81BD"
  #if (is.null(by)) by <- default_fill
  #fill = "#1380A1
  f<-get_factor_levels(dictionary, x %>% dplyr::pull("variable"))
  g<- ggplot2::ggplot(x, ggplot2::aes(y = forcats::fct_rev(label), x=percent,
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
                  title = stringr::str_wrap(title, default_wrap_length),
                  subtitle = stringr::str_wrap(
                    glue::glue("% Respondents Exceeding '{threshold}' Threshold (N={n})"),
                    default_wrap_length)) +

    # Apply standard style for the library
    style_ggplot()

  g

}


#' Title
#'
#' @param x
#' @param vars
#' @param by
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
yesno_plot <- function(x, vars, by = NULL, ...) {
  evrt::calculate_counts_yesno(x, vars = vars, by = by) |>
    evrt::plot_variables_as_yesno(by=by, ...)
}

#' Plot variable counts
#'
#' @param x A table of variables
#' @param dictionary
#' @param by
#' @param default_wrap_length
#' @param title A title for the plot
#' @param col Colors for the plot
#' @return
#' @export
#'
#' @examples
plot_variables_as_yesno<- function(x,
                                       dictionary,
                                       by=NULL,
                                       default_wrap_length = 35,
                                       threshold = "",
                                       title = "",
                                       col = c("#4F81BD", "#FAAB18","#868686FF","#CD534CFF")
) {
  # This is magic to turn string to expression, or leave as expression.
  if (!is.null(by)) by <- ensym(by)
  # Turn the label into a factor to keep it from getting out of order. Probably
  # should also use the variable name if the label isn't present.
  x <- dplyr::mutate(x, label = forcats::fct_inorder(.data$label))
  # For plotting, everything starts at the wrong direction.
  # x has count, n and percent. label gives the info.
  n <- x %>% dplyr::select(n) %>% dplyr::distinct() %>% dplyr::pull("n")
  # Wrap the labels to a reasonable width for printing
  x<-x %>% dplyr::mutate(label = fct_wrap(label, 35))
  default_fill <- "#1380A1"
  default_fill <- "#4F81BD"
  #if (is.null(by)) by <- default_fill
  #fill = "#1380A1
  g<- ggplot2::ggplot(x, ggplot2::aes(y=forcats::fct_rev(label), x=percent,
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
    ggplot2::geom_hline(yintercept = 0, size = 1, colour="#333333") +
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







