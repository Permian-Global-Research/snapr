#' Print a message with a yellow underline
#' @param x character message
#' @keywords internal
#' @noRd
#' @details This function is used frequently in printing methods.
pheader <- function(x) {
  cli::style_underline(cli::col_yellow(x))
}

#' clean up ugly snap operator names
#' @param x character vector
#' @keywords internal
#' @noRd
clean_param_names <- function(x, prefix = "") {
  # convert x to snake case
  s <- stringr::str_replace_all(x, " ", "") |>
    stringr::str_replace_all("-", "_") |>
    stringr::str_replace_all("([a-z])([A-Z])", "\\1_\\2") |>
    stringr::str_to_lower()

  return(glue("{prefix}{s}"))
}

#' function for checking if the xml graph can be read
#' @param x character xml graph
#' @return logical if the xml graph can be read
#' @keywords internal
#' @noRd
check_xml_read <- function(x) {
  result <- tryCatch(
    xml2::read_xml(x),
    error = function(e) NULL
  )

  # Check if an error occurred
  if (is.null(result)) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}
