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
clean_param_names <- function(x) {
  # convert x to snake case
  stringr::str_replace_all(x, " ", "") |>
    stringr::str_replace_all("-", "_") |>
    stringr::str_replace_all("([a-z])([A-Z])", "\\1_\\2") |>
    stringr::str_to_lower()
}
