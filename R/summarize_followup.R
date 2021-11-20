#' Create a categorical table with followup comments
#'
#' @param x
#' @param var
#' @param followup
#' @param header
#'
#' @return
#' @export
#' @importFrom magrittr %>%
#'
#' @examples
summarize_followup <- function(x, var, followup, header="**Characteristic**") {

  cattable<-dplyr::select(x, dplyr::all_of(var)) %>%
    gtsummary::tbl_summary(type=list(gtsummary::everything() ~ "categorical"))
  ftable <- dplyr::select(x, dplyr::all_of(followup)) %>%
    dplyr::na_if("") %>%
    gtsummary::tbl_summary(statistic = list(gtsummary::all_categorical() ~ ""),
                           missing = "no")

  gtsummary::tbl_stack(list(cattable, ftable), quiet = TRUE,
                       group_header = c("QUESTION", "FOLLOWUP")) %>%
    gtsummary::modify_header(update = list(label ~ header, groupname_col ~ "")) %>%
    gtsummary::modify_footnote(update = gtsummary::everything() ~ NA)

}

#' Convert followup table to flextable output
#'
#' @param x A gtsummary table
#'
#' @return A flextable object representing x.
#' @export
#'
#' @importFrom magrittr %>%
#' @examples
style_followup_as_flextable <- function(x) {
  x %>%
    gtsummary::as_flex_table() %>%
    flextable::width(j=1:3,width=c(1,5,1)) %>%
    style_flextable()
}
