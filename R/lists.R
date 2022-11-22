count_lists <- function(x) {
  sum(unlist(purrr::map(x, is.list)))
}
expand_embedded_list <- function(x, by = NULL) {

  num_lists <- count_lists(x)
  if ( num_lists == 0 ) return(x)

  if ( num_lists > 1 )
    stop("The data to summarize contains more than one list, this will not work
         in the same table. Instead, summarize each list separately.")
  if ( ncol(x) > 1 && is.null(by))
    stop("The data to summarize contains both list and non-list elements. This will not
    work since it will overcount the non-list elements. Instead, summarize data separately."
    )

  x|>
    tidyr::unnest(cols = setdiff(colnames(x),by)) |>
    labelled::set_variable_labels(.labels = labelled::var_label(x)) |>
    dplyr::filter(dplyr::if_any(c(dplyr::everything(),-by), ~!is.na(.)))

}
