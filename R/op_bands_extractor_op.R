#' BandsExtractorOp: snap operator function
#' @param operator_id character operator id
#' @param sourceProduct The source product(s) as input to the operator
#' @param sourceMaskNames The source masks for the computation.
#' @param sourceBandNames The source bands for the computation.
#' @details
#' Descrscription from '`gpt {operator} -h`':
#'
#' "Creates a new product out of the source product containing only the indexes
#' bands given"
#' @import xml2
#' @return snap_op_bands_extractor_op object
#' @export
op_bands_extractor_op <- function(
    operator_id,
    sourceProduct,
    sourceMaskNames = NULL,
    sourceBandNames = NULL) {
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
  xml_add_child(node, "operator", "BandsExtractorOp")
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
    "sourceMaskNames",
    gpt_args$sourceMaskNames
  )
  xml_add_child(
    parameters,
    "sourceBandNames",
    gpt_args$sourceBandNames
  )
  operator_sources <- gpt_args[c("sourceProduct")]
  snap_op_bands_extractor_op <- S7::new_class(
    "snap_op_bands_extractor_op",
    parent = snap_operator
  )
  snap_op_bands_extractor_op(
    operator = "BandsExtractorOp",
    operator_id = operator_id,
    operator_sources = operator_sources,
    created_with = rlang::current_fn(),
    xml_graph = as.character(op_xml)
  )
}
