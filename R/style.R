#' Default styling for a flextable
#'
#' @description This is the package default for styling a flextable. This adds color in the
#' header and uses the booktabs theme.
#'
#' @param ft A flextable to style.
#'
#' @return A flextable, with stylign applied.
#' @export
#'
#' @examples
style_flextable <- function(ft) {
  ft %>%
    flextable::theme_booktabs() %>%
    flextable::bg(part = "header", bg="#4F81BD") %>%
    flextable::color(color = "#FFFFFF", part = "header")

}


#' This was adapted from the BBC template
#'
#' @return
#' @export
#'
#' @examples
style_ggplot<-function () {
  font <- "Helvetica"

  #    axis.text.x = ggplot2::element_text(margin = ggplot2::margin(5,

  ggplot2::theme(
    plot.title = ggplot2::element_text(family = font, face = "bold", color = "#222222"),
    plot.subtitle = ggplot2::element_text(family = font,
                                          margin = ggplot2::margin(9, 0, 9, 0)),
    legend.position = "top",
    legend.text.align = 0,
    legend.background = ggplot2::element_blank(),
    legend.title = ggplot2::element_blank(),
    legend.key = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(family = font,
                                        color = "#222222"),
    #axis.title = ggplot2::element_blank(),
    axis.text = ggplot2::element_text(family = font,
                                      color = "#222222"),
    axis.line = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_line(color = "#cbcbcb"),
    panel.grid.major.x = ggplot2::element_blank(),
    panel.background = ggplot2::element_blank(),
    strip.background = ggplot2::element_rect(fill = "white"),
    strip.text = ggplot2::element_text(size = 22, hjust = 0)
  )
}




