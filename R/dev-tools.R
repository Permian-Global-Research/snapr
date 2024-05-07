# Create a function to join neighboring elements
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

get_operator_help <- function(
    operator,
    gpt_path = "/home/hugh/esa-snap/bin/gpt",
    check_operator = TRUE,
    node = TRUE) {
  if (check_operator) {
    operator <- rlang::arg_match(operator, get_operators()$operator)
  }
  gpt_help <- system2(gpt_path, args = c("-h", operator, "2>/dev/null"), stdout = TRUE, stderr = FALSE)

  param_n <- which(gpt_help == "Parameter Options:")
  graph_xml_n <- which(gpt_help == "Graph XML Format:")
  desc_n <- which(gpt_help == "Description:")

  params <- join_multilines(gpt_help[(param_n + 1):(graph_xml_n - 1)])

  param_descr <- gpt_help[(desc_n + 1):(param_n - 1)] |>
    paste(collapse = "\n") |>
    stringr::str_squish()

  paramstib <- params |>
    tibble::as_tibble() |>
    dplyr::mutate(
      param = stringr::str_extract(value, "(?<=-)[^\\s]*"),
      class = stringr::str_extract(param, "(?<=<)[^>]*"),
      param = stringr::str_extract(param, "^[^=]*"),
      description = stringr::str_squish(
        stringr::str_extract(value, "(?<=\\>\\s).*")
      )
    ) |>
    dplyr::select(param, class, description)


  xml_graph <- gpt_help[(graph_xml_n + 1):(length(gpt_help))] |>
    paste(collapse = "\n") |>
    xml2::read_xml()

  if (isTRUE(node)) {
    xml_graph <- xml2::xml_find_all(xml_graph, "//node")
  }

  operator_helper <- list(
    operator = operator,
    description = param_descr,
    parameters = paramstib,
    xml_graph = xml_graph
  )

  class(operator_helper) <- c("snap.operator_help", "list")

  return(operator_helper)
}


operators <- get_operators()
operators$operator

reader <- get_operator_help("Read", node = FALSE)
writer <- get_operator_help("Write")
speck_filt <- get_operator_help("Speckle-Filter")

new_graph <- reader$xml_graph
op_child <- xml2::xml_find_first(speck_filt$xml_graph, "//node")
write_chile <- xml2::xml_find_first(writer$xml_graph, "//node")

xml2::xml_add_child(new_graph, op_child)

xml2::xml_add_child(new_graph, write_chile)




xml2::write_xml(new_graph, "new_graph.xml")

# microbenchmark::microbenchmark(
#   get_operator_help("Read"),
#   get_operator_help("Read", check_operator = FALSE),
#   times = 1
# )
