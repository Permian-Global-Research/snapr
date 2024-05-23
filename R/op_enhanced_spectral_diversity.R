#' Enhanced-Spectral-Diversity: snap operator function
#' @param operator_id character operator id
#' @param sourceProduct The source product(s) as input to the operator
#' @param fineWinWidthStr Sets parameter 'fineWinWidthStr' to <string>. Value
#' must be one of '32', '64', '128', '256', '512', '1024', '2048'. Default value
#' is '512'.
#' @param fineWinHeightStr Sets parameter 'fineWinHeightStr' to <string>. Value
#' must be one of '32', '64', '128', '256', '512', '1024', '2048'. Default value
#' is '512'.
#' @param fineWinAccAzimuth Sets parameter 'fineWinAccAzimuth' to <string>.
#' Value must be one of '2', '4', '8', '16', '32', '64'. Default value is '16'.
#' @param fineWinAccRange Sets parameter 'fineWinAccRange' to <string>. Value
#' must be one of '2', '4', '8', '16', '32', '64'. Default value is '16'.
#' @param fineWinOversampling Sets parameter 'fineWinOversampling' to <string>.
#' Value must be one of '32', '64', '128', '256'. Default value is '128'.
#' @param xCorrThreshold The peak cross-correlation threshold Valid interval is
#' (0, *). Default value is '0.1'.
#' @param cohThreshold The coherence threshold for outlier removal Valid
#' interval is (0, 1\]. Default value is '0.3'.
#' @param numBlocksPerOverlap The number of windows per overlap for ESD Valid
#' interval is \[1, 20\]. Default value is '10'.
#' @param esdEstimator ESD estimator used for azimuth shift computation Value
#' must be one of 'Average', 'Periodogram'. Default value is 'Periodogram'.
#' @param weightFunc Weight function of the coherence to use for azimuth shift
#' estimation Value must be one of 'None', 'Linear', 'Quadratic', 'Inv Quadratic'.
#' Default value is 'Inv Quadratic'.
#' @param temporalBaselineType Baseline type for building the integration
#' network Value must be one of 'Number of images', 'Number of days'. Default
#' value is 'Number of images'.
#' @param maxTemporalBaseline Maximum temporal baseline (in days or number of
#' images depending on the Temporal baseline type) between pairs of images to
#' construct the network. Any number < 1 will generate a network with all of the
#' possible pairs. Default value is '4'.
#' @param integrationMethod Method used for integrating the shifts network.
#' Value must be one of 'L1', 'L2', 'L1 and L2'. Default value is 'L1 and L2'.
#' @param doNotWriteTargetBands Do not write target bands Default value is
#' 'false'.
#' @param useSuppliedRangeShift Use user supplied range shift Default value is
#' 'false'.
#' @param overallRangeShift The overall range shift Default value is '0.0'.
#' @param useSuppliedAzimuthShift Use user supplied azimuth shift Default value
#' is 'false'.
#' @param overallAzimuthShift The overall azimuth shift Default value is '0.0'.
#' @details
#' Descrscription from '`gpt {operator} -h`':
#'
#' "Estimate constant range and azimuth offsets for a stack of images"
#' @import xml2
#' @return snap_op_enhanced_spectral_diversity object
#' @export
op_enhanced_spectral_diversity <- function(
    operator_id,
    sourceProduct,
    fineWinWidthStr = 512,
    fineWinHeightStr = 512,
    fineWinAccAzimuth = 16,
    fineWinAccRange = 16,
    fineWinOversampling = 128,
    xCorrThreshold = 0.1,
    cohThreshold = 0.3,
    numBlocksPerOverlap = 10,
    esdEstimator = "Periodogram",
    weightFunc = "Inv Quadratic",
    temporalBaselineType = "Number of images",
    maxTemporalBaseline = 4,
    integrationMethod = "L1 and L2",
    doNotWriteTargetBands = FALSE,
    useSuppliedRangeShift = FALSE,
    overallRangeShift = 0.0,
    useSuppliedAzimuthShift = FALSE,
    overallAzimuthShift = 0.0) {
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
  xml_add_child(node, "operator", "Enhanced-Spectral-Diversity")
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
    "fineWinWidthStr",
    gpt_args$fineWinWidthStr
  )
  xml_add_child(
    parameters,
    "fineWinHeightStr",
    gpt_args$fineWinHeightStr
  )
  xml_add_child(
    parameters,
    "fineWinAccAzimuth",
    gpt_args$fineWinAccAzimuth
  )
  xml_add_child(
    parameters,
    "fineWinAccRange",
    gpt_args$fineWinAccRange
  )
  xml_add_child(
    parameters,
    "fineWinOversampling",
    gpt_args$fineWinOversampling
  )
  xml_add_child(
    parameters,
    "xCorrThreshold",
    gpt_args$xCorrThreshold
  )
  xml_add_child(
    parameters,
    "cohThreshold",
    gpt_args$cohThreshold
  )
  xml_add_child(
    parameters,
    "numBlocksPerOverlap",
    gpt_args$numBlocksPerOverlap
  )
  xml_add_child(
    parameters,
    "esdEstimator",
    gpt_args$esdEstimator
  )
  xml_add_child(
    parameters,
    "weightFunc",
    gpt_args$weightFunc
  )
  xml_add_child(
    parameters,
    "temporalBaselineType",
    gpt_args$temporalBaselineType
  )
  xml_add_child(
    parameters,
    "maxTemporalBaseline",
    gpt_args$maxTemporalBaseline
  )
  xml_add_child(
    parameters,
    "integrationMethod",
    gpt_args$integrationMethod
  )
  xml_add_child(
    parameters,
    "doNotWriteTargetBands",
    gpt_args$doNotWriteTargetBands
  )
  xml_add_child(
    parameters,
    "useSuppliedRangeShift",
    gpt_args$useSuppliedRangeShift
  )
  xml_add_child(
    parameters,
    "overallRangeShift",
    gpt_args$overallRangeShift
  )
  xml_add_child(
    parameters,
    "useSuppliedAzimuthShift",
    gpt_args$useSuppliedAzimuthShift
  )
  xml_add_child(
    parameters,
    "overallAzimuthShift",
    gpt_args$overallAzimuthShift
  )
  operator_sources <- gpt_args[c("sourceProduct")]
  snap_op_enhanced_spectral_diversity <- S7::new_class(
    "snap_op_enhanced_spectral_diversity",
    parent = snap_operator
  )
  snap_op_enhanced_spectral_diversity(
    operator = "Enhanced-Spectral-Diversity",
    operator_id = operator_id,
    operator_sources = operator_sources,
    created_with = rlang::current_fn(),
    xml_graph = as.character(op_xml)
  )
}
