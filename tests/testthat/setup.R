all_nodes <- function(
    x,
    drop = c("source", "sourceProduct", "sources", "sourceProducts", "Source")) {
  drop <- rlang::arg_match(drop, multiple = TRUE)
  xml2::xml_find_all(x, "//*") |>
    xml2::xml_name() |>
    setdiff(drop) |>
    sort()
}
