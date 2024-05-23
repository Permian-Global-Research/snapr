#' GenericRegionMergingOp: snap operator function
#' @param operator_id character operator id
#' @param sourceProduct The source product(s) as input to the operator
#' @param mergingCostCriterion The method to compute the region merging. Value
#' must be one of 'Spring', 'Baatz & Schape', 'Full Lamda Schedule'. Default value
#' is 'Baatz & Schape'.
#' @param regionMergingCriterion The method to check the region merging. Value
#' must be one of 'Best Fitting', 'Local Mutual Best Fitting'. Default value is
#' 'Local Mutual Best Fitting'.
#' @param totalIterationsForSecondSegmentation The total number of iterations.
#' Default value is '50'.
#' @param threshold The threshold. Default value is '100.0'.
#' @param spectralWeight The spectral weight. Default value is '0.5'.
#' @param shapeWeight The shape weight. Default value is '0.5'.
#' @param sourceBandNames The source bands for the computation.
#' @details
#' Descrscription from '`gpt {operator} -h`':
#'
#' "The 'Generic Region Merging' operator computes the distinct regions from a
#' product"
#' @import xml2
#' @return snap_op_generic_region_merging_op object
#' @export
op_generic_region_merging_op <- function(
    operator_id,
    sourceProduct,
    mergingCostCriterion = "Baatz & Schape",
    regionMergingCriterion = "Local Mutual Best Fitting",
    totalIterationsForSecondSegmentation = 50,
    threshold = 100.0,
    spectralWeight = 0.5,
    shapeWeight = 0.5,
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
  xml_add_child(node, "operator", "GenericRegionMergingOp")
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
    "mergingCostCriterion",
    gpt_args$mergingCostCriterion
  )
  xml_add_child(
    parameters,
    "regionMergingCriterion",
    gpt_args$regionMergingCriterion
  )
  xml_add_child(
    parameters,
    "totalIterationsForSecondSegmentation",
    gpt_args$totalIterationsForSecondSegmentation
  )
  xml_add_child(
    parameters,
    "threshold",
    gpt_args$threshold
  )
  xml_add_child(
    parameters,
    "spectralWeight",
    gpt_args$spectralWeight
  )
  xml_add_child(
    parameters,
    "shapeWeight",
    gpt_args$shapeWeight
  )
  xml_add_child(
    parameters,
    "sourceBandNames",
    gpt_args$sourceBandNames
  )
  operator_sources <- gpt_args[c("sourceProduct")]
  snap_op_generic_region_merging_op <- S7::new_class(
    "snap_op_generic_region_merging_op",
    parent = snap_operator
  )
  snap_op_generic_region_merging_op(
    operator = "GenericRegionMergingOp",
    operator_id = operator_id,
    operator_sources = operator_sources,
    created_with = rlang::current_fn(),
    xml_graph = as.character(op_xml)
  )
}
