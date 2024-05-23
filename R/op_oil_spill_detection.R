#' Oil-Spill-Detection: snap operator function
#' @param operator_id character operator id
#' @param sourceProduct The source product(s) as input to the operator
#' @param sourceBands The list of source bands.
#' @param backgroundWindowDim Background window dimension (km) Default value is
#' '0.5'.
#' @param k Threshold shift from background mean Default value is '2.0'.
#' @details
#' Descrscription from '`gpt {operator} -h`':
#'
#' "Detect oil spill."
#' @import xml2
#' @return snap_op_oil_spill_detection object
#' @export
op_oil_spill_detection <- function(
    operator_id,
    sourceProduct,
    sourceBands = NULL,
    backgroundWindowDim = 0.5,
    k = 2.0) {
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
  xml_add_child(node, "operator", "Oil-Spill-Detection")
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
    "sourceBands",
    gpt_args$sourceBands
  )
  xml_add_child(
    parameters,
    "backgroundWindowDim",
    gpt_args$backgroundWindowDim
  )
  xml_add_child(
    parameters,
    "k",
    gpt_args$k
  )
  operator_sources <- gpt_args[c("sourceProduct")]
  snap_op_oil_spill_detection <- S7::new_class(
    "snap_op_oil_spill_detection",
    parent = snap_operator
  )
  snap_op_oil_spill_detection(
    operator = "Oil-Spill-Detection",
    operator_id = operator_id,
    operator_sources = operator_sources,
    created_with = rlang::current_fn(),
    xml_graph = as.character(op_xml)
  )
}
