
#' Add section text to a word document
#'
#' @description Adds section text to a word document (including header).
#'
#' @param doc Word document object.
#' @param header Header text to include in document.
#' @param text Any text to add to in the paragraph below the header.
#' @param level The Heading level to use (default 1).
#'
#' @return Word document object
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @examples
docx_add_section <- function(doc, header = "", text = "", level = 1, page_break= TRUE) {

    if (page_break) doc <- officer::body_add_break(doc)
    doc %>%
      officer::body_add_par(header, style=glue::glue("heading {level}")) %>%
      officer::body_add_par("\n") %>%
      officer::body_add_par(text) %>%
      officer::body_add_par("\n\n")
}

#' Add flextable at Heading 1
#'
#' @param doc Word document object.
#' @param header Header text to include in document.
#' @param text Any text to add to in the paragraph below the header.
#' @param ft Flextable object to add to word document.
#'
#' @return Word document object
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @examples
docx_add_flextable_section<-function(doc, header = "", text="", ft) {
  docx_add_section(doc, header, text, level = 1) %>%
    flextable::body_add_flextable(ft)
}

#' Add flextable at Heading 2
#'
#' @param doc Word document object.
#' @param header Header text to include in document.
#' @param text Any text to add to in the paragraph below the header.
#' @param ft Flextable object to add to word document.
#'
#' @return Word document object
#' @export
#'
#' @importFrom magrittr %>%
#' @examples
docx_add_flextable_subsection<-function(doc, header = "", text="", ft) {
  docx_add_section(doc, header, text, level = 2) %>%
    flextable::body_add_flextable(ft)
}
