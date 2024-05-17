#' Print a message with a yellow underline
#' @param x character message
#' @keywords internal
#' @noRd
#' @details This function is used frequently in printing methods.
pheader <- function(x) {
  cli::style_underline(cli::col_yellow(x))
}

#' Print the xml graph for a snap_operator or snap_operator_help object
#' @param x snap_operator or snap_operator_help object
#' @keywords internal
#' @noRd
xml_printer <- function(x) {
  cli::style_bold("XML process graph: \n\n") |>
    cli::col_yellow() |>
    cat()
  as.character(paste0(x@xml_graph, "\n")) |>
    cli::style_italic() |>
    cli::col_green() |>
    cat()
}

#' clean up ugly snap operator names
#' @param x character vector
#' @keywords internal
#' @noRd
clean_param_names <- function(x, prefix = "") {
  # convert x to snake case
  s <- stringr::str_replace_all(
    x,
    c(
      " " = "",
      "-" = "_",
      "\\." = "_",
      "([a-z])([A-Z])" = "\\1_\\2"
    )
  ) |>
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
