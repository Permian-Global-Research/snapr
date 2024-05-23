#' BatchSnaphuUnwrapOp: snap operator function
#' @param operator_id character operator id
#' @param sourceProduct The source product(s) as input to the operator
#' @param snaphuProcessingLocation Directory that the Snaphu Export operator
#' wrote to
#' @param snaphuInstallLocation Directory to install the SNAPHU binary to
#' @details
#' Descrscription from '`gpt {operator} -h`':
#'
#' "Downloads and executes SNAPHU on a set of two or more interferograms."
#' @import xml2
#' @return snap_op_batch_snaphu_unwrap_op object
#' @export
op_batch_snaphu_unwrap_op <- function(
    operator_id,
    sourceProduct,
    snaphuProcessingLocation = NULL,
    snaphuInstallLocation = NULL) {
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
  xml_add_child(node, "operator", "BatchSnaphuUnwrapOp")
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
    "snaphuProcessingLocation",
    gpt_args$snaphuProcessingLocation
  )
  xml_add_child(
    parameters,
    "snaphuInstallLocation",
    gpt_args$snaphuInstallLocation
  )
  operator_sources <- gpt_args[c("sourceProduct")]
  snap_op_batch_snaphu_unwrap_op <- S7::new_class(
    "snap_op_batch_snaphu_unwrap_op",
    parent = snap_operator
  )
  snap_op_batch_snaphu_unwrap_op(
    operator = "BatchSnaphuUnwrapOp",
    operator_id = operator_id,
    operator_sources = operator_sources,
    created_with = rlang::current_fn(),
    xml_graph = as.character(op_xml)
  )
}
