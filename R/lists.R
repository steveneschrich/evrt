count_lists <- function(x) {
  sum(unlist(purrr::map(x, is.list)))
}
expand_embedded_list <- function(x) {

  num_lists <- count_lists(x)
  if ( num_lists == 0 ) return(x)

  if ( num_lists > 1 )
    stop("The data to summarize contains more than one list, this will not work
         in the same table. Instead, summarize each list separately.")
  if ( ncol(x) > 1 )
    stop("The data to summarize contains both list and non-list elements. This will not
    work since it will overcount the non-list elements. Instead, summarize data separately."
    )

  x|>
    tidyr::unnest(cols = colnames(x)) |>
    labelled::set_variable_labels(.labels = labelled::var_label(x)) |>
    dplyr::filter(dplyr::if_any(dplyr::everything(), ~!is.na(.)))

}
