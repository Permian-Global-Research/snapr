#' A function to write R functions for given xml graphs.
#' This function creates a function that takes the arguments and xml from
#' a snap_operator_help_bject to create a function that can then be used to
#' build gpt graphs.
#' @import glue
#' @keywords internal
#' @export
build_xml_engine <- function(operator, node = TRUE, null_src = FALSE) {
  if (!rlang::is_installed("styler")) {
    cli::cli_abort(
      c("x" = "The {{styler}} package is required to format the output")
    )
  }

  param_tib <- get_param_defaults(operator)
  op <- snap_operator_help(operator, check_operator = FALSE, node = node)

  xml_graph <- op@xml_graph
  params <- param_tib$param
  defaults <- param_tib$default
  defaults[is.na(defaults)] <- "NULL"
  descriptions <- param_tib$description

  op_name <- clean_param_names(op@operator, prefix = "op_")

  func_file <- file.path("R", glue("{op_name}.R"))

  file.create(func_file)

  param_string <- if (is.null(params)) {
    ""
  } else {
    paste(
      ",\n   ",
      paste(params, defaults, sep = " = ", collapse = ",\n    ")
    )
  }

  op_src_param <- if (isTRUE(null_src)) "" else ",\n operator_sources"

  param_string <- glue("operator_id {op_src_param} {param_string}")

  source_builder <- if (isTRUE(null_src)) {
    ""
  } else {
    'op_src_id <- sub(".0", "", paste0(".", seq_along(operator_sources) - 1))
          purrr::walk2(
    op_src_id,
    operator_sources,
    function(.x, .y){{
      xml_add_child(sources, paste0("sourceProduct", .x), refid = .y)
    }}
  )\n'
  }
  op_src_docs <- if (isTRUE(null_src)) {
    ""
  } else {
    chr_80_split(paste(
      "#' @param operator_sources character vector of",
      "operator sources that match upstream `operator_id` values"
    ))
  }

  param_builder <- if (is.null(param_tib)) {
    ""
  } else {
    paste(purrr::map_chr(
      params,
      function(.x) {
        glue('xml_add_child(parameters,
            "{.x}",
            gpt_args${.x})')
      }
    ), collapse = "\n  ")
  }

  class_builder <- if (isTRUE(null_src)) {
    glue(
      "snap_{op_name} <- S7::new_class(
          'snap_{op_name}',
          parent=snap_read_operator)\n  ",
      "snap_{op_name}(
          operator = '{op@operator}',
          operator_id = operator_id,
          created_with = rlang::current_fn(),
          xml_graph = as.character(op_xml))\n "
    )
  } else {
    glue(
      "snap_{op_name} <- S7::new_class(
          'snap_{op_name}',
          parent=snap_operator)\n  ",
      "snap_{op_name}(
          operator = '{op@operator}',
          operator_id = operator_id,
          operator_sources = operator_sources,
          created_with = rlang::current_fn(),
          xml_graph = as.character(op_xml))\n "
    )
  }

  writeLines(
    c(
      glue(
        "#' {operator}: snap operator function\n"
      ),
      "#' @param operator_id character operator id",
      op_src_docs,
      purrr::map2_chr(
        params, descriptions,
        \(.x, .y) {
          chr_80_split(glue("#' @param {.x} {.y}\n"))
        }
      ),
      "#' @details",
      "#' Descrscription from '`gpt {operator} -h`':\n#'",
      chr_80_split(glue("#' \"{op@description}\" ")),
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
        source_builder,
        'parameters <- xml_add_child(node, "parameters")\n  ',
        param_builder,
        "\n  ",
        class_builder,
        "}}"
      )
    ),
    con = func_file
  )

  styler::style_file(func_file)

  write_op_test(op_name, op, null_src, node)
  invisible()
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

#' function to write tests for functions created in `build_xml_engine`
#' @import glue
#' @keywords internal
#' @export
write_op_test <- function(op_name, gpt_op, null_src, node) {
  test_file <- file.path("tests", "testthat", glue("test-{op_name}.R"))

  file.create(test_file)

  rop_caller <- if (isTRUE(null_src)) {
    'r_op <- {op_name}("testnode1")\n'
  } else {
    'r_op <- {op_name}("testnode1", "testsrc1")\n'
  }

  writeLines(
    glue(
      'test_that("{op_name} matches gpt xml", {{\n   ',
      rop_caller,
      'gpt_op <- snap_operator_help("{gpt_op@operator}",
      check_operator = FALSE,
      node = {node})\n',
      "snapr_xml_nodes <- all_nodes(as_xml_document(r_op))\n ",
      "gpt_example_nodes <- all_nodes(as_xml_document(gpt_op))\n",
      "testthat::expect_equal(snapr_xml_nodes, gpt_example_nodes)",
      "}})"
    ),
    con = test_file
  )
  styler::style_file(test_file)

  invisible()
}
