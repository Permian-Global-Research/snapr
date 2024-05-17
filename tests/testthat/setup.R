all_nodes <- function(x, drop = c("source", "sourceProduct")) {
  drop <- rlang::arg_match(drop)
  xml2::xml_find_all(x, "//*") |>
    xml2::xml_name() |>
    setdiff(drop)
}
