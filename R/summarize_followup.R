#' Title
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
followup_flextable <- function(x, ...) {
  summarize_followup(x, ...) |>
    style_followup_as_flextable()
}

#' Create a categorical table with followup comments
#'
#' @param x Labelled data frame containing all data in survey
#' @param vars Variable to include as frequency table (character)
#' @param followup Variable to include as follow-up field (character)
#' @param header
#' @param group_headers A vector of strings of header names for the questions (default
#'  QUESTION) and followup (default FOLLOWUP).
#' @param followup_statistic A statistic (see [gtsummary::tbl_summary()]). Default
#'   is `"{n}"`, meaning include counts.
#'
#' @return A [gtsummary::tbl_summary()] representing the frequency of `var`, with
#'  any additional free text responses in `followup` listed below the table.
#'
#' @export
#' @importFrom magrittr %>%
#'
#' @examples
summarize_followup <- function(x, vars, followup, header="**Characteristic**",
                               group_headers = c("QUESTION","FOLLOWUP"),
                               followup_statistic = "{n}") {

  # If embedded lists are present, expand them here.
  cattable <- dplyr::select(x, dplyr::all_of(vars)) |>
    expand_embedded_list()


  cattable<-cattable |>
    gtsummary::tbl_summary(type=list(gtsummary::everything() ~ "categorical"))
  ftable <- dplyr::select(x, dplyr::all_of(followup)) %>%
    dplyr::na_if("") %>%
    gtsummary::tbl_summary(
      statistic = list(gtsummary::all_categorical() ~ followup_statistic),
      missing = "no"
    )

  gtsummary::tbl_stack(list(cattable, ftable), quiet = TRUE,
                       group_header = group_headers) %>%
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
