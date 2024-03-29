#' Collapse Value/Label pairs in a tibble to a string.
#'
#' @description The data dictionary provides a mapping of values to labels for a variable. This function
#' converts such a table to a string representing the mapping.
#'
#' @details This function assumes as input a `Levels` tibble which can be converted into string representation.
#'
#' @note To convert all levels in the dictionary, see \code{\link{format_levels}}. This function converts only the levels
#' for a single variable.
#'
#' @param x A tibble representing a variable's levels (or empty).
#'
#' @return A string representing the values/labels for a variable.
#' @export
#' @importFrom rlang .data
#' @examples
#' \dontrun{
#' collapse_levels(tibble::tibble(Value=c(1,2,3), Label=c("One","Two","Three")))
#'
#'}
collapse_levels<- function(x) {
  checkmate::assert_data_frame(x)
  checkmate::assert_subset(colnames(x), c("Value","Label"))
  x |>
    dplyr::mutate(text = stringr::str_c(.data$Value,":", .data$Label)) |>
    dplyr::pull("text") |>
    stringr::str_trim() |>
    stringr::str_c(collapse="; ")
}

#' Convert data dictionary levels to strings
#'
#' @description A function to convert the data dictionary levels into string representations.
#'
#' @details The data dictionary contains information about specific variables in a redcap dataset. This
#' routine will take as input the Levels variable of the dictionary and turn it into a vector of strings.
#'
#' The idiom should be:
#' ```
#' dictionary |> mutate(levels_as_string = format_levels(Levels))
#' ```
#'
#' @param x A dictionary column representing levels
#'
#' @return
#' @export
#'
#' @examples
format_levels<- function(x) {
  checkmate::assert_list(x, types = "data.frame")

  purrr::map_chr(x, collapse_levels)
}

#' Extract formatted factor levels from a dictionary for a
#' list of variables.
#'
#' @param dictionary Data dictionary
#' @param vars Variable list to extract factor levels
#'
#' @return A unique list of strings (ideally one) representing the levels.
#' @export
#'
#' @importFrom rlang .data
#' @examples
get_factor_levels <-function(x, vars = colnames(x),dictionary = NULL) {
  if ( is.null(dictionary) ) {
    res <- levels_as_string(x, vars)
  } else {
    res <- dictionary |>
      dplyr::filter(.data$`Variable / Field Name` %in% {{ vars }}) |>
      dplyr::mutate( label_text = format_levels(.data$Levels)) |>
      dplyr::select(.data$`Variable / Field Name`, label_text) |>
      tibble::deframe() |>
      unique()
  }
  res
}

#' Extract levels of variables as a string
#'
#' @description Extract from a data frame a subset of variables, then
#'  create single-string descriptions of the levels of each of these
#'  variables.
#'
#' @param x A data frame of factors
#' @param vars A list of variables to choose from data frame
#'
#' @return A vector of strings where each string represents a concatenated
#'  set of factor levels.
#' @export
#'
#' @examples
#' \dontrun{
#' levels_as_string(iris, vars="Species")
#' }
levels_as_string <- function(x, vars = colnames(x)) {
  xsub <- dplyr::select(x, dplyr::all_of(vars))

  #checkmate::assert_data_frame(xsub, types="factor")

  strlevel <- purrr::map_chr(xsub, \(x) {
    if ( is.numeric(x) ) {
      res <- "Continuous"
    } else if ( is.factor(x) ) {
      res <- paste(1:nlevels(x), levels(x), sep=":", collapse="; ")
    } else {
      res <- "Discrete"
    }
    res
  })

  strlevel
}
#' Title
#'
#' @param levels Table of levels (Value and Label) for a variable.
#'
#' @return
#' @export
#' @importFrom rlang .data
#' @examples
get_min_levels<-function(levels) {
  purrr::map_chr(levels, ~.x  %>%
                   dplyr::summarize(min=min(.data$Value)) %>%
                   dplyr::pull("min")
  )
}



#' Title
#'
#' @param levels Table of levels (Value and Label) for a variable.
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @examples
get_max_levels<-function(levels) {
  purrr::map_chr(levels, ~.x  %>%
                   dplyr::summarize(max=max(.data$Value)) %>%
                   dplyr::pull("max")
  )
}


#' Title
#'
#' @param dictionary A data dictionary (table).
#' @param vars A list of variables to extract ranges for.
#'
#' @return
#' @export
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#'
#' @examples
get_variable_range <- function(dictionary, vars) {
  dictionary %>%
    dplyr::filter(.data$`Variable / Field Name` %in% !!vars) %>%
    dplyr::mutate(
      min = get_min_levels(.data$Levels),
      max= get_max_levels(.data$Levels)
    )

}
