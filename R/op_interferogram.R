#' Interferogram: snap operator function
#' @param operator_id character operator id
#' @param sourceProduct The source product(s) as input to the operator
#' @param subtractFlatEarthPhase Sets parameter 'subtractFlatEarthPhase' to
#' <boolean>. Default value is 'true'.
#' @param srpPolynomialDegree Order of 'Flat earth phase' polynomial Value must
#' be one of '1', '2', '3', '4', '5', '6', '7', '8'. Default value is '5'.
#' @param srpNumberPoints Number of points for the 'flat earth phase'
#' polynomial estimation Value must be one of '301', '401', '501', '601', '701',
#' '801', '901', '1001'. Default value is '501'.
#' @param orbitDegree Degree of orbit (polynomial) interpolator Value must be
#' one of '1', '2', '3', '4', '5'. Default value is '3'.
#' @param includeCoherence Sets parameter 'includeCoherence' to <boolean>.
#' Default value is 'true'.
#' @param cohWinAz Size of coherence estimation window in Azimuth direction
#' Default value is '10'.
#' @param cohWinRg Size of coherence estimation window in Range direction
#' Default value is '10'.
#' @param squarePixel Use ground square pixel Default value is 'true'.
#' @param subtractTopographicPhase Sets parameter 'subtractTopographicPhase' to
#' <boolean>. Default value is 'false'.
#' @param demName The digital elevation model. Default value is 'SRTM 3Sec'.
#' @param externalDEMFile Sets parameter 'externalDEMFile' to <file>.
#' @param externalDEMNoDataValue Sets parameter 'externalDEMNoDataValue' to
#' <double>. Default value is '0'.
#' @param externalDEMApplyEGM Sets parameter 'externalDEMApplyEGM' to
#' <boolean>. Default value is 'true'.
#' @param tileExtensionPercent Define extension of tile for DEM simulation
#' (optimization parameter). Default value is '100'.
#' @param outputElevation Sets parameter 'outputElevation' to <boolean>.
#' Default value is 'false'.
#' @param outputLatLon Sets parameter 'outputLatLon' to <boolean>. Default
#' value is 'false'.
#' @details
#' Descrscription from '`gpt {operator} -h`':
#'
#' "Compute interferograms from stack of coregistered S-1 images"
#' @import xml2
#' @return snap_op_interferogram object
#' @export
op_interferogram <- function(
    operator_id,
    sourceProduct,
    subtractFlatEarthPhase = TRUE,
    srpPolynomialDegree = 5,
    srpNumberPoints = 501,
    orbitDegree = 3,
    includeCoherence = TRUE,
    cohWinAz = 10,
    cohWinRg = 10,
    squarePixel = TRUE,
    subtractTopographicPhase = FALSE,
    demName = "SRTM 3Sec",
    externalDEMFile = NULL,
    externalDEMNoDataValue = 0,
    externalDEMApplyEGM = TRUE,
    tileExtensionPercent = 100,
    outputElevation = FALSE,
    outputLatLon = FALSE) {
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
  xml_add_child(node, "operator", "Interferogram")
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
    "subtractFlatEarthPhase",
    gpt_args$subtractFlatEarthPhase
  )
  xml_add_child(
    parameters,
    "srpPolynomialDegree",
    gpt_args$srpPolynomialDegree
  )
  xml_add_child(
    parameters,
    "srpNumberPoints",
    gpt_args$srpNumberPoints
  )
  xml_add_child(
    parameters,
    "orbitDegree",
    gpt_args$orbitDegree
  )
  xml_add_child(
    parameters,
    "includeCoherence",
    gpt_args$includeCoherence
  )
  xml_add_child(
    parameters,
    "cohWinAz",
    gpt_args$cohWinAz
  )
  xml_add_child(
    parameters,
    "cohWinRg",
    gpt_args$cohWinRg
  )
  xml_add_child(
    parameters,
    "squarePixel",
    gpt_args$squarePixel
  )
  xml_add_child(
    parameters,
    "subtractTopographicPhase",
    gpt_args$subtractTopographicPhase
  )
  xml_add_child(
    parameters,
    "demName",
    gpt_args$demName
  )
  xml_add_child(
    parameters,
    "externalDEMFile",
    gpt_args$externalDEMFile
  )
  xml_add_child(
    parameters,
    "externalDEMNoDataValue",
    gpt_args$externalDEMNoDataValue
  )
  xml_add_child(
    parameters,
    "externalDEMApplyEGM",
    gpt_args$externalDEMApplyEGM
  )
  xml_add_child(
    parameters,
    "tileExtensionPercent",
    gpt_args$tileExtensionPercent
  )
  xml_add_child(
    parameters,
    "outputElevation",
    gpt_args$outputElevation
  )
  xml_add_child(
    parameters,
    "outputLatLon",
    gpt_args$outputLatLon
  )
  operator_sources <- gpt_args[c("sourceProduct")]
  snap_op_interferogram <- S7::new_class(
    "snap_op_interferogram",
    parent = snap_operator
  )
  snap_op_interferogram(
    operator = "Interferogram",
    operator_id = operator_id,
    operator_sources = operator_sources,
    created_with = rlang::current_fn(),
    xml_graph = as.character(op_xml)
  )
}
