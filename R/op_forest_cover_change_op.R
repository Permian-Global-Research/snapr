#' ForestCoverChangeOp: snap operator function
#' @param operator_id character operator id
#' @param previousProduct The source product to be modified. This is a
#' mandatory source.
#' @param recentProduct The source product to be modified. This is a mandatory
#' source.
#' @param forestCoverPercentage Specifies the percentage of forest cover per
#' segment Default value is '95.0'.
#' @param landCoverName Sets parameter 'landCoverName' to <string>. Default
#' value is 'CCILandCover-2015'.
#' @param landCoverMapIndices The indices of forest color from the new added
#' land cover map Default value is '40, 50, 60, 61, 62, 70, 71, 72, 80, 81, 82,
#' 90, 100, 110, 160, 170'.
#' @param mergingCostCriterion The method to compute the region merging. Value
#' must be one of 'Spring', 'Baatz & Schape', 'Full Lamda Schedule'. Default value
#' is 'Spring'.
#' @param regionMergingCriterion The method to check the region merging. Value
#' must be one of 'Best Fitting', 'Local Mutual Best Fitting'. Default value is
#' 'Local Mutual Best Fitting'.
#' @param totalIterationsForSecondSegmentation The total number of iterations.
#' Default value is '10'.
#' @param threshold The threshold. Default value is '5.0'.
#' @param spectralWeight The spectral weight. Default value is '0.5'.
#' @param shapeWeight The shape weight. Default value is '0.5'.
#' @param degreesOfFreedom Degrees of freedom used for the Chi distribution
#' trimming process Default value is '3.3'.
#' @param currentProductSourceMaskFile A binary raster file to be added as mask
#' to the output product
#' @param previousProductSourceMaskFile A binary raster file to be added as
#' mask to the output product
#' @details
#' Descrscription from '`gpt {operator} -h`':
#'
#' "Creates forest change masks out of two source products"
#' @import xml2
#' @return snap_op_forest_cover_change_op object
#' @export
op_forest_cover_change_op <- function(
    operator_id,
    previousProduct = NULL,
    recentProduct = NULL,
    forestCoverPercentage = 95.0,
    landCoverName = "CCILandCover-2015",
    landCoverMapIndices = "40, 50, 60, 61, 62, 70, 71, 72, 80, 81, 82, 90, 100, 110, 160, 170",
    mergingCostCriterion = "Spring",
    regionMergingCriterion = "Local Mutual Best Fitting",
    totalIterationsForSecondSegmentation = 10,
    threshold = 5.0,
    spectralWeight = 0.5,
    shapeWeight = 0.5,
    degreesOfFreedom = 3.3,
    currentProductSourceMaskFile = NULL,
    previousProductSourceMaskFile = NULL) {
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
  xml_add_child(node, "operator", "ForestCoverChangeOp")
  sources <- xml_add_child(node, "sources")
  xml_add_child(
    sources,
    "previousProduct",
    gpt_args$previousProduct
  )
  xml_add_child(
    sources,
    "recentProduct",
    gpt_args$recentProduct
  )
  parameters <- xml_add_child(node, "parameters")
  xml_add_child(
    parameters,
    "forestCoverPercentage",
    gpt_args$forestCoverPercentage
  )
  xml_add_child(
    parameters,
    "landCoverName",
    gpt_args$landCoverName
  )
  xml_add_child(
    parameters,
    "landCoverMapIndices",
    gpt_args$landCoverMapIndices
  )
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
    "degreesOfFreedom",
    gpt_args$degreesOfFreedom
  )
  xml_add_child(
    parameters,
    "currentProductSourceMaskFile",
    gpt_args$currentProductSourceMaskFile
  )
  xml_add_child(
    parameters,
    "previousProductSourceMaskFile",
    gpt_args$previousProductSourceMaskFile
  )
  operator_sources <- gpt_args[c(
    "previousProduct",
    "recentProduct"
  )]
  snap_op_forest_cover_change_op <- S7::new_class(
    "snap_op_forest_cover_change_op",
    parent = snap_operator
  )
  snap_op_forest_cover_change_op(
    operator = "ForestCoverChangeOp",
    operator_id = operator_id,
    operator_sources = operator_sources,
    created_with = rlang::current_fn(),
    xml_graph = as.character(op_xml)
  )
}
