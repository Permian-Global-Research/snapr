#' A function to write R functions for given xml graphs.
#' This function creates a function that takes the arguments and xml from
#' a snap_operator_help_bject to create a function that can then be used to
#' build gpt graphs.
#' @param operator character name of the operator
#' @param node logical indicating if the operator is a node
#' @import glue xml2
#' @keywords internal
#' @export
build_xml_engine <- function(operator, node = TRUE) {
  if (!rlang::is_installed("styler")) {
    cli::cli_abort(
      c("x" = "The {{styler}} package is required to format the output")
    )
  }


  param_tib <- get_param_defaults(operator)
  op <- snap_operator_help(operator, check_operator = FALSE, node = node)

  xml_graph <- op@xml_graph
  inputs <- op@sources$sources
  input_description <- op@sources$description
  params <- param_tib$param
  defaults <- param_tib$default
  defaults[is.na(defaults)] <- "NULL"
  descriptions <- param_tib$description

  null_src <- all(is.na(inputs))

  op_name <- clean_param_names(op@operator, prefix = "op_")

  func_file <- file.path("R", glue("{op_name}.R"))

  file.create(func_file)

  input_string <- build_input_string(inputs, null_src)

  param_string <- build_param_string(params, defaults, input_string)

  source_string <- build_source_string(inputs, null_src)

  op_src_docs <- build_source_docs(inputs, input_description, null_src)

  op_param_docs <- build_param_docs(params, descriptions)

  xml_param_string <- xml_param_builder(param_tib, params)

  class_string <- build_class_string(op_name, op, null_src)

  write_op_file(
    operator, op, params, descriptions, op_name, param_string, op_src_docs,
    op_param_docs, source_string, xml_param_string, class_string, func_file,
    inputs
  )
  write_op_test(op_name, op, null_src, node)
  invisible()
}


#' function to write R functions to file for given xml graphs
#' @import glue
#' @keywords internal
#' @noRd
write_op_file <- function(
    operator, op, params, descriptions, op_name, param_string, op_src_docs,
    op_param_docs, source_string, xml_param_string, class_string, func_file,
    inputs) {
  writeLines(
    c(
      glue(
        "#' {operator}: snap operator function\n"
      ),
      "#' @param operator_id character operator id",
      op_src_docs,
      op_param_docs,
      "#' @details",
      "#' Descrscription from '`gpt {operator} -h`':\n#'",
      clean_docs(chr_80_split(glue("#' \"{op@description}\" "))),
      "#' @import xml2",
      glue("#' @return snap_{op_name} object"),
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
        source_string,
        'parameters <- xml_add_child(node, "parameters")\n  ',
        xml_param_string,
        "\n  ",
        'operator_sources <- gpt_args[c({paste(paste0(\'"\', inputs, \'"\'), collapse=",\n")})]\n',
        class_string,
        "}}"
      )
    ),
    con = func_file
  )

  styler::style_file(func_file)
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
      "snapr_xml_nodes <- all_nodes(as_xml(r_op))\n ",
      "gpt_example_nodes <- all_nodes(as_xml(gpt_op))\n",
      "testthat::expect_equal(snapr_xml_nodes, gpt_example_nodes)",
      "}})"
    ),
    con = test_file
  )
  styler::style_file(test_file)
}


build_input_string <- function(inputs, null_src) {
  if (isTRUE(null_src)) {
    s <- ""
  } else {
    s <- dplyr::case_when(
      inputs == "sourceProduct" ~ inputs,
      TRUE ~ paste(inputs, "NULL", sep = " = ")
    )
  }

  return(s)
}



#' function to build the parameter string for the snap operator function
#' @param params character vector of parameter names
#' @param defaults character vector of default values
#' @param null_src logical indicating if operator_sources are NULL
#' @keywords internal
#' @noRd
build_param_string <- function(params, defaults, input_str) {
  pstring <- if (is.null(params)) {
    ""
  } else {
    paste(params, defaults, sep = " = ")
  }

  if (all(pstring == "") && all(input_str == "")) {
    return("operator_id")
  } else if (all(pstring == "")) {
    return(paste(c("operator_id", input_str), collapse = ",\n    "))
  } else if (all(input_str == "")) {
    return(paste(c("operator_id", pstring), collapse = ",\n    "))
  } else {
    return(paste(c("operator_id", input_str, pstring), collapse = ",\n    "))
  }
}

#' function to build the source string for the snap operator function
#' @param null_src logical indicating if operator_sources are NULL
#' @param input_srcs character vector of source names
#' @keywords internal
#' @noRd
build_source_string <- function(input_srcs, null_src) {
  if (isTRUE(null_src)) {
    return("")
  } else {
    sprod <- if ("sourceProduct" %in% input_srcs) {
      paste(
        'op_src_id <- sub(".0", "", paste0(".", seq_along(sourceProduct) - 1))',
        "purrr::walk2(",
        "op_src_id,",
        "sourceProduct,",
        "function(.x, .y){{",
        'xml_add_child(sources, paste0("sourceProduct", .x), refid = .y)',
        "}}",
        ")\n",
        sep = "\n"
      )
    } else {
      ""
    }
    add_src <- if (any(input_srcs != "sourceProduct")) {
      paste(purrr::map_chr(
        setdiff(input_srcs, "sourceProduct"),
        function(.x) {
          glue('xml_add_child(sources,
            "{.x}",
            gpt_args${.x})')
        }
      ), collapse = "\n  ")
    } else {
      ""
    }
    return(paste(sprod, add_src, "\n"))
  }
}

#' function to build the source docs for the snap operator function
#' @param null_src logical indicating if operator_sources are NULL
#' @keywords internal
#' @noRd
build_source_docs <- function(input_srcs, input_descrip, null_src) {
  if (isTRUE(null_src)) {
    "#'"
  } else {
    purrr::map2_chr(
      input_srcs, input_descrip,
      \(.x, .y) {
        chr_80_split(glue("#' @param {.x} {.y}\n")) |>
          clean_docs()
      }
    )
  }
}

#' function to build the parameter docs for the snap operator function
#' @param params character vector of parameter names
#' @param descriptions character vector of parameter descriptions
#' @keywords internal
#' @noRd
build_param_docs <- function(params, descriptions) {
  purrr::map2_chr(
    params, descriptions,
    \(.x, .y) {
      chr_80_split(glue("#' @param {.x} {.y}\n")) |>
        clean_docs()
    }
  )
}

#' function to build the xml parameter string for the snap operator function
#' @param param_tib tibble of parameter names, defaults, and descriptions
#' @param params character vector of parameter names
#' @keywords internal
#' @noRd
xml_param_builder <- function(param_tib, params) {
  if (is.null(param_tib)) {
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
}

#' function to build the class string for the snap operator function
#' @param op_name character operator name
#' @param op snap_operator_help object
#' @param null_src logical indicating if operator_sources are NULL
#' @keywords internal
#' @noRd
build_class_string <- function(op_name, op, null_src) {
  if (isTRUE(null_src)) {
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
}
