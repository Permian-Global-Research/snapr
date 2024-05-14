#' function to join multi-line text
#' @param vec character vector
#' @return character vector
#' @keywords internal
#' @noRd
#' @details used to concatonate multi-line text - used to covert the gpt ouputs
#' into single line text.
join_multilines <- function(vec) {
  i <- 1
  while (i < length(vec)) {
    # Check if the next element starts with more than two spaces
    if (substr(vec[i + 1], 1, 3) == "   ") {
      # Join the current element with the next one
      vec[i] <- paste(vec[i], vec[i + 1])
      # Remove the next element from the vector
      vec <- vec[-(i + 1)]
    } else {
      # Move to the next element
      i <- i + 1
    }
  }
  vec[nzchar(vec)]
}

#' function to get the operators from the gpt executable
#' @param gpt_path character path to the gpt executable
#' @return tibble of operators and descriptions
#' @export
get_operators <- function(gpt_path = "/home/hugh/esa-snap/bin/gpt") {
  gpt_help <- system2(gpt_path,
    args = c("-h", "2>/dev/null"),
    stdout = TRUE, stderr = FALSE
  )

  op_n <- which(gpt_help == "Operators:")
  operators <- join_multilines(gpt_help[(op_n + 1):(length(gpt_help) - 1)])

  operators <- operators |>
    trimws() |>
    tibble::as_tibble() |>
    dplyr::mutate(
      operator = stringr::str_extract(value, "^[^\\s]+"),
      description = stringr::str_squish(stringr::str_extract(value, "\\s.*$"))
    ) |>
    dplyr::select(operator, description)

  return(operators)
}
