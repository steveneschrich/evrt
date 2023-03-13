#' Short-hand to add new line in word document
#'
#' @description When manipulating a word document, adding a blank line
#' as a paragraph is often needed. This function inserts a blank paragraph
#' into the given document.
#'
#' @details One can construct a word document through a series of `officer` commands. One
#' particular use that is often needed is to insert a new paragraph, resulting in a blank
#' line. This function is a short-hand to do that quickly.
#'
#' @param doc A officer word document
#'
#' @return A document with a blank paragraph added
#' @export
#'
#' @examples
#' \dontrun{
#' officer::body_add_par("A paragraph of text") %>% nl() %>%
#' officer::body_add_par("Another paragraph")
#' }
nl <- function(doc) {
  officer::body_add_par(doc, "\n\n")
}
#' Add content to word document
#'
#' @description Adds content to a word document (including header).
#'
#' @param doc Word document object.
#' @param header Header text to include in document.
#' @param text Any text to add to in the paragraph below the header.
#' @param level The Heading level to use (default 1).
#'
#' @return Word document object
#' @export
#'
docx_add <- function(doc, header = "", text = NULL, fig = NULL, ft = NULL,
                     level = 1, pre_page_break = FALSE, post_page_break= FALSE,
                     fig.height=5, fig.width=6) {

    if (pre_page_break)
      doc <- officer::body_add_break(doc)
    else
      doc <- officer::body_add_par(doc, "\n")

    doc <- doc |>
      officer::body_add_par(header, style=glue::glue("heading {level}"))
    if (!is.null(text)) {
      doc <- doc |>
        officer::body_add_par(text) |>
        officer::body_add_par("\n\n")
    }
    if (!is.null(ft))
      doc <- flextable::body_add_flextable(doc, ft)
    if (!is.null(fig))
      doc <- officer::body_add_gg(doc, fig, height=fig.height, width=fig.width)
    if (post_page_break)
      doc <- officer::body_add_break(doc)
    else
      doc <- officer::body_add_par(doc, "\n")

    doc
}

#' Add Heading 1 word content
#'
#' @param doc Word document object.
#' @param ... Other parameters to \code{\link{docx_add}}
#'
#' @return Word document object
#' @export
#'
docx_add_section<-function(doc, ...) {
  docx_add(doc,level = 1,...)
}


#' Add Heading 2 word content
#'
#' @param doc Word document object.
#' @param ... Other parameters to \code{\link{docx_add}}
#'
#' @return Word document object
#' @export
#'
docx_add_subsection<-function(doc, header = "", text="", ft) {
  docx_add(doc, level = 2, ...)
}


