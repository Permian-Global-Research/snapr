#' function for creating a new snap_operator_help object
#' @param operator character name of the operator
#' @param gpt_path character path to the gpt executable
#' @param check_operator logical check if the operator is valid
#' @param node logical return the node or the full xml graph
#' @return snap_operator_help S7 object with the following slots:
#' operator, description, parameters, and xml_graph
#' @name snap_operator_help
#' @import S7
#' @export
#'
snap_operator_help <- new_class("snap_operator_help",
  properties = list(
    operator = class_character,
    description = class_character,
    sources = class_data.frame,
    parameters = class_data.frame,
    xml_graph = class_character
  ),
  package = "snapr",
  validator = function(self) {
    if (length(self@operator) != 1) {
      return("@operator must be a character of length 1")
    }
    if (length(self@description) != 1) {
      return("@description must be a character of length 1")
    }
    if (ncol(self@sources) != 3) {
      return("@sources must be a data.frame with 3 columns")
    }
    if (ncol(self@parameters) != 3) {
      return("@parameters must be a data.frame with 3 columns")
    }
    if (nrow(self@parameters) < 1) {
      return("@parameters must have at least 1 row")
    }
    if (!check_xml_read(self@xml_graph)) {
      return("@xml_graph must contain valid XML content")
    }
  },
  constructor = function(operator,
                         gpt_path = "/home/hugh/esa-snap/bin/gpt",
                         check_operator = TRUE,
                         node = TRUE) {
    op_help <- get_operator_help(
      operator,
      gpt_path = gpt_path,
      check_operator = check_operator,
      node = node
    )
    new_object(
      S7_object(),
      operator = op_help[[1]],
      description = op_help[[2]],
      sources = op_help[[3]],
      parameters = op_help[[4]],
      xml_graph = as.character(op_help[[5]])
    )
  }
)

#' print method for snap operator helper object
#' @param x snap_operator_help object
#' @param xml logical print the xml graph
#' @param ... not used.
#' @name snap_operator_help
#' @export
#' @noRd
method(print, snap_operator_help) <- function(x, xml = FALSE, ...) {
  cat("<snap_operator_help>\n")
  cat("\n")
  cat(paste(
    pheader("Opreator:"),
    cli::style_bold(
      paste(x@operator, "\n\n")
    )
  ))
  cat(paste(
    pheader("Description:"),
    cli::col_magenta(
      cli::style_italic(paste0(x@description, "\n\n"))
    )
  ))

  cat(pheader("Sources:\n\n"))
  print(x@sources, n = nrow(x@sources))

  cat(pheader("Parameters:\n\n"))
  print(x@parameters, n = nrow(x@parameters))

  if (xml) {
    cat("\n")
    show_xml(x)
  }
  invisible()
}

#' create xml_document object from snap_operator_help
#' @name as_xml
#' @param x snap_operator_help object
#' @export
method(as_xml, snap_operator_help) <- function(x) {
  xml2::read_xml(x@xml_graph)
}

#' show xml graph method for snap_operator_help object
#' @name show_xml
#' @param x snap_operator_help object
#' @export
method(show_xml, snap_operator_help) <- function(x) {
  xml_printer(x)
  invisible()
}



#' function for constrcuting a snap operator helper object
#' @param operator character name of the operator
#' @param gpt_path character path to the gpt executable
#' @param check_operator logical check if the operator is valid
#' @param node logical return the node or the full xml graph
#' @return list of operator, description, parameters, and xml graph
#' @keywords internal
#' @noRd
get_operator_help <- function(
    operator,
    gpt_path = "/home/hugh/esa-snap/bin/gpt",
    check_operator = TRUE,
    node = TRUE) {
  if (check_operator) {
    operator <- rlang::arg_match(operator, get_operators()$operator)
  }

  suppressWarnings(gpt_help <- system2(gpt_path,
    args = c("-h", operator, "2>/dev/null"),
    stdout = TRUE, stderr = FALSE
  ))

  if (length(gpt_help) == 0) {
    cli::cli_abort(
      c(
        "x" = "There is an issue with the snap gpt executable, operator name or operator.",
        "i" = "To reproduce this error run: ",
        cli::code_highlight('system2("{gpt_path}", c("{operator}", "-h"))')
      )
    )
  }

  src_opts_n <- which(gpt_help == "Source Options:")
  param_n <- which(gpt_help == "Parameter Options:")
  graph_xml_n <- which(gpt_help == "Graph XML Format:")
  desc_n <- which(gpt_help == "Description:")

  xml_graph <- gpt_help[(graph_xml_n + 1):(length(gpt_help))] |>
    paste(collapse = "\n") |>
    xml2::read_xml()

  if (isTRUE(node)) {
    xml_graph <- xml2::xml_find_all(xml_graph, "//node")
  }


  gpt_xml_src_names <- xml_graph |>
    xml2::xml_find_all("//sources") |>
    xml_children() |>
    purrr::map_chr(xml2::xml_name)

  if (length(src_opts_n) == 1 || length(gpt_xml_src_names) > 0) {
    poss_srcs <- c(
      "source", "sources", "Source",
      "sourceProduct", "sourceProducts"
    )


    default_src_opts <- tibble::tibble(
      sources = "sourceProduct",
      class = "string/file",
      description = "The source product(s) as input to the operator"
    )

    if (length(src_opts_n) == 1) {
      src_end <- ifelse(length(param_n) == 0, graph_xml_n, param_n)
      src_opts <- join_multilines(gpt_help[(src_opts_n + 1):(src_end - 1)])

      src_opts_tib <- src_opts |>
        tibble::as_tibble() |>
        dplyr::mutate(
          sources = stringr::str_extract(value, "(?<=-S)[^\\s]*"),
          class = stringr::str_extract(sources, "(?<=<)[^>]*"),
          sources = stringr::str_extract(sources, "^[^=]*"),
          description = stringr::str_squish(
            stringr::str_extract(value, "(?<=\\>\\s).*")
          )
        ) |>
        dplyr::select(sources, class, description) |>
        dplyr::mutate(
          sources =
            dplyr::case_when(
              sources %in% poss_srcs
              ~ "sourceProduct",
              TRUE ~ sources
            ),
          description = dplyr::case_when(
            sources == "sourceProduct" ~ "The source product(s) as input to the operator",
            TRUE ~ description
          ),
          class = dplyr::case_when(
            sources == "sourceProduct" ~ "string/file",
            TRUE ~ class
          )
        ) |>
        dplyr::distinct(sources, .keep_all = TRUE)
    } else {
      src_opts_tib <- tibble::tibble(sources = NA, class = NA, description = NA)
    }

    if (any(poss_srcs %in% gpt_xml_src_names) &&
      !any(poss_srcs %in% src_opts_tib$sources)) {
      src_opts_tib <- dplyr::bind_rows(src_opts_tib, default_src_opts)
    }

    if ("sourceProduct" %in% src_opts_tib$sources) {
      src_opts_tib <- src_opts_tib |>
        dplyr::arrange(dplyr::desc(sources == "sourceProduct")) |>
        na.omit()
    }
  } else if (length(src_opts_n) == 0) {
    src_opts_tib <- tibble::tibble(sources = NA, class = NA, description = NA)
  } else {
    cli::cli_abort(
      c(
        "x" = "Multiple 'Source Options' lines found in: `gpt {operator} -h output`"
      )
    )
  }


  if (length(param_n) == 1) {
    params <- join_multilines(gpt_help[(param_n + 1):(graph_xml_n - 1)])

    param_end <- if (length(src_opts_n) == 0) graph_xml_n else src_opts_n
    param_descr <- gpt_help[(desc_n + 1):(param_end - 1)] |>
      paste(collapse = "\n") |>
      stringr::str_squish()

    paramstib <- params |>
      tibble::as_tibble() |>
      dplyr::mutate(
        param = stringr::str_extract(value, "(?<=-P)[^\\s]*"),
        class = stringr::str_extract(param, "(?<=<)[^>]*"),
        param = stringr::str_extract(param, "^[^=]*"),
        description = stringr::str_squish(
          stringr::str_extract(value, "(?<=\\>\\s).*")
        )
      ) |>
      dplyr::select(param, class, description) |>
      # for some reason this borks the reader unless removed.
      dplyr::filter(param != "useAdvancedOptions")
  } else if (length(param_n) == 0) {
    paramstib <- tibble::tibble(param = NA, class = NA, description = NA)
    param_descr <- gpt_help[(desc_n + 1):(graph_xml_n - 1)] |>
      paste(collapse = "\n") |>
      stringr::str_squish()
  } else {
    cli::cli_abort(
      c("x" = "Multiple Parameter tile lines found in gpt {operator} -h output")
    )
  }



  list(operator, param_descr, src_opts_tib, paramstib, xml_graph)
}
