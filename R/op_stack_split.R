#' Stack-Split: snap operator function
#' @param operator_id character operator id
#' @param sourceProduct The source product(s) as input to the operator
#' @param targetFolder The output folder to which the data product is written.
#' Default value is 'target'.
#' @param formatName The name of the output file format. Default value is
#' 'BEAM-DIMAP'.
#' @details
#' Descrscription from '`gpt {operator} -h`':
#'
#' "Writes all bands to files."
#' @import xml2
#' @return snap_op_stack_split object
#' @export
op_stack_split <- function(
    operator_id,
    sourceProduct,
    targetFolder = "target",
    formatName = "BEAM-DIMAP") {
  args <- as.list(rlang::current_env())
  gpt_args <- purrr::map(args, function(x) {
    if (is.null(x)) {
      ""
    } else if (is.logical(x)) {
      tolower(x)
    } else {
      x
    }
  })
  op_xml <- xml2::xml_new_document()
  node <- xml_add_child(op_xml, "node", id = operator_id)
  xml_add_child(node, "operator", "Stack-Split")
  sources <- xml_add_child(node, "sources")
  op_src_id <- sub(".0", "", paste0(".", seq_along(sourceProduct) - 1))
  purrr::walk2(
    op_src_id,
    sourceProduct,
    function(.x, .y) {
      xml_add_child(sources, paste0("sourceProduct", .x), refid = .y)
    }
  )

  parameters <- xml_add_child(node, "parameters")
  xml_add_child(
    parameters,
    "targetFolder",
    gpt_args$targetFolder
  )
  xml_add_child(
    parameters,
    "formatName",
    gpt_args$formatName
  )
  operator_sources <- gpt_args[c("sourceProduct")]
  snap_op_stack_split <- S7::new_class(
    "snap_op_stack_split",
    parent = snap_operator
  )
  snap_op_stack_split(
    operator = "Stack-Split",
    operator_id = operator_id,
    operator_sources = operator_sources,
    created_with = rlang::current_fn(),
    xml_graph = as.character(op_xml)
  )
}
