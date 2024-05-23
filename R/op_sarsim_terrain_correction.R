#' SARSim-Terrain-Correction: snap operator function
#' @param operator_id character operator id
#' @param sourceProduct The source product(s) as input to the operator
#' @param rmsThreshold The RMS threshold for eliminating invalid GCPs Valid
#' interval is (0, *). Default value is '1.0'.
#' @param warpPolynomialOrder The order of WARP polynomial function Value must
#' be one of '1', '2', '3'. Default value is '1'.
#' @param imgResamplingMethod Sets parameter 'imgResamplingMethod' to <string>.
#' Value must be one of 'NEAREST_NEIGHBOUR', 'BILINEAR_INTERPOLATION',
#' 'CUBIC_CONVOLUTION'. Default value is 'BILINEAR_INTERPOLATION'.
#' @param pixelSpacingInMeter The pixel spacing in meters Default value is '0'.
#' @param pixelSpacingInDegree The pixel spacing in degrees Default value is
#' '0'.
#' @param mapProjection The coordinate reference system in well known text
#' format
#' @param alignToStandardGrid Force the image grid to be aligned with a
#' specific point Default value is 'false'.
#' @param standardGridOriginX x-coordinate of the standard grid's origin point
#' Default value is '0'.
#' @param standardGridOriginY y-coordinate of the standard grid's origin point
#' Default value is '0'.
#' @param saveDEM Sets parameter 'saveDEM' to <boolean>. Default value is
#' 'false'.
#' @param saveLatLon Sets parameter 'saveLatLon' to <boolean>. Default value is
#' 'false'.
#' @param saveLocalIncidenceAngle Sets parameter 'saveLocalIncidenceAngle' to
#' <boolean>. Default value is 'false'.
#' @param saveProjectedLocalIncidenceAngle Sets parameter
#' 'saveProjectedLocalIncidenceAngle' to <boolean>. Default value is 'false'.
#' @param saveSelectedSourceBand Sets parameter 'saveSelectedSourceBand' to
#' <boolean>. Default value is 'true'.
#' @param outputComplex Sets parameter 'outputComplex' to <boolean>. Default
#' value is 'false'.
#' @param applyRadiometricNormalization Sets parameter
#' 'applyRadiometricNormalization' to <boolean>. Default value is 'false'.
#' @param saveSigmaNought Sets parameter 'saveSigmaNought' to <boolean>.
#' Default value is 'false'.
#' @param saveGammaNought Sets parameter 'saveGammaNought' to <boolean>.
#' Default value is 'false'.
#' @param saveBetaNought Sets parameter 'saveBetaNought' to <boolean>. Default
#' value is 'false'.
#' @param incidenceAngleForSigma0 Sets parameter 'incidenceAngleForSigma0' to
#' <string>. Value must be one of 'Use incidence angle from Ellipsoid', 'Use
#' projected local incidence angle from DEM', 'Use local incidence angle from
#' DEM'. Default value is 'Use projected local incidence angle from DEM'.
#' @param incidenceAngleForGamma0 Sets parameter 'incidenceAngleForGamma0' to
#' <string>. Value must be one of 'Use incidence angle from Ellipsoid', 'Use
#' projected local incidence angle from DEM', 'Use local incidence angle from
#' DEM'. Default value is 'Use projected local incidence angle from DEM'.
#' @param auxFile The auxiliary file Value must be one of 'Latest Auxiliary
#' File', 'Product Auxiliary File', 'External Auxiliary File'. Default value is
#' 'Latest Auxiliary File'.
#' @param externalAuxFile The antenne elevation pattern gain auxiliary data
#' file.
#' @param openShiftsFile Show range and azimuth shifts file in a text viewer
#' Default value is 'false'.
#' @param openResidualsFile Show the Residuals file in a text viewer Default
#' value is 'false'.
#' @details
#' Descrscription from '`gpt {operator} -h`':
#'
#' "Orthorectification with SAR simulation"
#' @import xml2
#' @return snap_op_sarsim_terrain_correction object
#' @export
op_sarsim_terrain_correction <- function(
    operator_id,
    sourceProduct,
    rmsThreshold = 1.0,
    warpPolynomialOrder = 1,
    imgResamplingMethod = "BILINEAR_INTERPOLATION",
    pixelSpacingInMeter = 0,
    pixelSpacingInDegree = 0,
    mapProjection = NULL,
    alignToStandardGrid = FALSE,
    standardGridOriginX = 0,
    standardGridOriginY = 0,
    saveDEM = FALSE,
    saveLatLon = FALSE,
    saveLocalIncidenceAngle = FALSE,
    saveProjectedLocalIncidenceAngle = FALSE,
    saveSelectedSourceBand = TRUE,
    outputComplex = FALSE,
    applyRadiometricNormalization = FALSE,
    saveSigmaNought = FALSE,
    saveGammaNought = FALSE,
    saveBetaNought = FALSE,
    incidenceAngleForSigma0 = "Use projected local incidence angle from DEM",
    incidenceAngleForGamma0 = "Use projected local incidence angle from DEM",
    auxFile = "Latest Auxiliary File",
    externalAuxFile = NULL,
    openShiftsFile = FALSE,
    openResidualsFile = FALSE) {
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
  xml_add_child(node, "operator", "SARSim-Terrain-Correction")
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
    "rmsThreshold",
    gpt_args$rmsThreshold
  )
  xml_add_child(
    parameters,
    "warpPolynomialOrder",
    gpt_args$warpPolynomialOrder
  )
  xml_add_child(
    parameters,
    "imgResamplingMethod",
    gpt_args$imgResamplingMethod
  )
  xml_add_child(
    parameters,
    "pixelSpacingInMeter",
    gpt_args$pixelSpacingInMeter
  )
  xml_add_child(
    parameters,
    "pixelSpacingInDegree",
    gpt_args$pixelSpacingInDegree
  )
  xml_add_child(
    parameters,
    "mapProjection",
    gpt_args$mapProjection
  )
  xml_add_child(
    parameters,
    "alignToStandardGrid",
    gpt_args$alignToStandardGrid
  )
  xml_add_child(
    parameters,
    "standardGridOriginX",
    gpt_args$standardGridOriginX
  )
  xml_add_child(
    parameters,
    "standardGridOriginY",
    gpt_args$standardGridOriginY
  )
  xml_add_child(
    parameters,
    "saveDEM",
    gpt_args$saveDEM
  )
  xml_add_child(
    parameters,
    "saveLatLon",
    gpt_args$saveLatLon
  )
  xml_add_child(
    parameters,
    "saveLocalIncidenceAngle",
    gpt_args$saveLocalIncidenceAngle
  )
  xml_add_child(
    parameters,
    "saveProjectedLocalIncidenceAngle",
    gpt_args$saveProjectedLocalIncidenceAngle
  )
  xml_add_child(
    parameters,
    "saveSelectedSourceBand",
    gpt_args$saveSelectedSourceBand
  )
  xml_add_child(
    parameters,
    "outputComplex",
    gpt_args$outputComplex
  )
  xml_add_child(
    parameters,
    "applyRadiometricNormalization",
    gpt_args$applyRadiometricNormalization
  )
  xml_add_child(
    parameters,
    "saveSigmaNought",
    gpt_args$saveSigmaNought
  )
  xml_add_child(
    parameters,
    "saveGammaNought",
    gpt_args$saveGammaNought
  )
  xml_add_child(
    parameters,
    "saveBetaNought",
    gpt_args$saveBetaNought
  )
  xml_add_child(
    parameters,
    "incidenceAngleForSigma0",
    gpt_args$incidenceAngleForSigma0
  )
  xml_add_child(
    parameters,
    "incidenceAngleForGamma0",
    gpt_args$incidenceAngleForGamma0
  )
  xml_add_child(
    parameters,
    "auxFile",
    gpt_args$auxFile
  )
  xml_add_child(
    parameters,
    "externalAuxFile",
    gpt_args$externalAuxFile
  )
  xml_add_child(
    parameters,
    "openShiftsFile",
    gpt_args$openShiftsFile
  )
  xml_add_child(
    parameters,
    "openResidualsFile",
    gpt_args$openResidualsFile
  )
  operator_sources <- gpt_args[c("sourceProduct")]
  snap_op_sarsim_terrain_correction <- S7::new_class(
    "snap_op_sarsim_terrain_correction",
    parent = snap_operator
  )
  snap_op_sarsim_terrain_correction(
    operator = "SARSim-Terrain-Correction",
    operator_id = operator_id,
    operator_sources = operator_sources,
    created_with = rlang::current_fn(),
    xml_graph = as.character(op_xml)
  )
}
