#' Title
#'
#' @param x
#' @param vars
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
combo_numeric <- function(x, vars, dictionary, by = NULL, header = "**Characteristic**",
                          summary = "mean", default_wrap_length=35,
                          threshold = 3, title = "",
                          col=c("#4F81BD", "#FAAB18", "#868686FF", "#CD534CFF")) {

  ft_tbl <- numeric_flextable(x = x, vars=vars, by = by, dictionary = dictionary, header=header, summary=summary)

  nm_plot <- numeric_plot(x = x, vars = vars, by = by, dictionary = dictionary, default_wrap_length= default_wrap_length,
            threshold = threshold, title = title, col=col
  )

  patchwork::wrap_plots(flextable::gen_grob(ft_tbl), nm_plot, ncol = 1, nrow = 2)
}
