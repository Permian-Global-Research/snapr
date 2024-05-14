#' A function to write R functions for given xml graphs.
#' This function creates a function that takes the arguments and xml from
#' a snap_operator_helper_bject to create a function that can then be used to
#' build gpt graphs.
#' @import glue
#' @keywords internal
#' @export
build_xml_engine <- function(operator, node = TRUE, null_src = FALSE) {
  param_tib <- get_param_defaults(operator)
  op <- snap_operator_helper(operator, check_operator = FALSE, node = node)

  xml_graph <- op@xml_graph
  params <- param_tib$param
  defaults <- param_tib$default
  defaults[is.na(defaults)] <- "NULL"
  descriptions <- param_tib$description

  op_name <- clean_param_names(op@operator)

  func_file <- file.path("R", glue("{op_name}.R"))

  file.create(func_file)

  param_string <- paste(
    "\n   ",
    paste(params, defaults, sep = " = ", collapse = ",\n    ")
  )

  param_string <- glue("operator_id,\n operator_sources,{param_string}")

  src <- if (isFALSE(null_src)) {
    ""
  } else {
    "xml_add_child(sourceProduct, 'source', id = 'null')"
  }

  operator_sources <- c("reader")
  op_src_id <- sub("\\.0", "", paste0(".", seq_along(operator_sources) - 1))

  purrr::map2_chr(
    op_src_id,
    operator_sources,
    function(.x, .y) {
      glue("xml_add_child(sourceProduct{.x}, refid = {.y})\n")
    }
  )

  writeLines(
    c(
      glue(
        "#' {operator}: snap operator function\n"
      ),
      "#' @param operator_id character operator id",
      chr_80_split(paste(
        "#' @param operator_sources character vector of",
        "operator sources that match upstream `operator_id` values"
      )),
      purrr::map2_chr(
        params, descriptions,
        \(.x, .y) {
          chr_80_split(glue("#' @param {.x} {.y}\n"))
        }
      ),
      "#' @details",
      chr_80_split(glue(
        "#' Descrscription from `gpt {operator}` -h`: {op@description} "
      )),
      "#' @import xml2",
      "#' @return xml2 xml graph",
      "#' @export",
      glue(
        "{op_name} <- function(\n{param_string}) {{\n  ",
        "args <- as.list(rlang::current_env())\n  ",
        'gpt_args <- purrr::map(args, function(x) {{
          if (is.null(x)) {{ "" }}
          else if (is.logical(x)) {{ tolower(x) }}
          else {{ x }}
        }})\n',
        "op_xml <- xml2::xml_new_document()\n  ",
        'node <- xml_add_child(op_xml, "node", id = operator_id)\n  ',
        'xml_add_child(node, "operator", "{op@operator}")\n  ',
        'sources <- xml_add_child(node, "sources")\n  ',
        'op_src_id <- sub(".0", "", paste0(".", seq_along(operator_sources) - 1))\n  ',
        'purrr::walk2(
    op_src_id,
    operator_sources,
    function(.x, .y){{
      xml_add_child(sources, paste0("sourceProduct", .x), refid = .y)
    }}
  )\n',
        'parameters <- xml_add_child(node, "parameters")\n  ',
        paste(purrr::map_chr(
          params,
          function(.x) {
            glue('xml_add_child(parameters, "{.x}", gpt_args${.x})')
          }
        ), collapse = "\n  "),
        "\n  ",
        "return(op_xml)\n",
        "}}"
      )
    ),
    con = func_file
  )

  styler::style_file(func_file)
}

#' helper to split long strings at 80 characters
#' @param x character string
#' @param collapse character string to join lines
#' @return character string
#' @keywords internal
#' @noRd
chr_80_split <- function(x, collapse = "\n#' ") {
  lines <- strwrap(x, width = 80)
  paste(lines, collapse = collapse)
}
