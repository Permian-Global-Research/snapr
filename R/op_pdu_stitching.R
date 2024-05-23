#' PduStitching: snap operator function
#' @param operator_id character operator id
#' @param sourceProduct The source product(s) as input to the operator
#' @param sourceProductPaths A comma-separated list of file paths specifying
#' the product dissemination units. Each path may contain the wildcards '**'
#' (matches recursively any directory), '*' (matches any character sequence in
#' path names) and '?' (matches any single character). If not given, the parameter
#' 'sourceProducts' must be provided.
#' @param targetDir The directory to which the stitched product shall be
#' written. Within this directory, a folder of the SLSTR L1B naming format will be
#' created. If no target directory is given, the product will be written to the
#' user directory.
#' @details
#' Descrscription from '`gpt {operator} -h`':
#'
#' "Stitches multiple SLSTR L1B product dissemination units (PDUs) of the same
#' orbit to a single product. Parameter Options:
#' -PsourceProductPaths=<string,string,string,...> A comma-separated list of file
#' paths specifying the product dissemination units. Each path may contain the
#' wildcards '**' (matches recursively any directory), '*' (matches any character
#' sequence in path names) and '?' (matches any single character). If not given,
#' the parameter 'sourceProducts' must be provided. -PtargetDir=<file> The
#' directory to which the stitched product shall be written. Within this
#' directory, a folder of the SLSTR L1B naming format will be created. If no
#' target directory is given, the product will be written to the user directory."
#' @import xml2
#' @return snap_op_pdu_stitching object
#' @export
op_pdu_stitching <- function(
    operator_id,
    sourceProduct,
    sourceProductPaths = NULL,
    targetDir = NULL) {
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
  xml_add_child(node, "operator", "PduStitching")
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
    "sourceProductPaths",
    gpt_args$sourceProductPaths
  )
  xml_add_child(
    parameters,
    "targetDir",
    gpt_args$targetDir
  )
  operator_sources <- gpt_args[c("sourceProduct")]
  snap_op_pdu_stitching <- S7::new_class(
    "snap_op_pdu_stitching",
    parent = snap_operator
  )
  snap_op_pdu_stitching(
    operator = "PduStitching",
    operator_id = operator_id,
    operator_sources = operator_sources,
    created_with = rlang::current_fn(),
    xml_graph = as.character(op_xml)
  )
}
