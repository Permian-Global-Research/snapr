#' TopoPhaseRemoval: snap operator function
#' @param operator_id character operator id
#' @param sourceProduct The source product(s) as input to the operator
#' @param orbitDegree Degree of orbit interpolation polynomial Valid interval
#' is (1, 10\]. Default value is '3'.
#' @param demName The digital elevation model. Default value is 'SRTM 3Sec'.
#' @param externalDEMFile Sets parameter 'externalDEMFile' to <file>.
#' @param externalDEMNoDataValue Sets parameter 'externalDEMNoDataValue' to
#' <double>. Default value is '0'.
#' @param tileExtensionPercent Define extension of tile for DEM simulation
#' (optimization parameter). Default value is '100'.
#' @param outputTopoPhaseBand Output topographic phase band. Default value is
#' 'false'.
#' @param outputElevationBand Output elevation band. Default value is 'false'.
#' @param outputLatLonBands Output lat/lon bands. Default value is 'false'.
#' @details
#' Descrscription from '`gpt {operator} -h`':
#'
#' "Compute and subtract TOPO phase"
#' @import xml2
#' @return snap_op_topo_phase_removal object
#' @export
op_topo_phase_removal <- function(
    operator_id,
    sourceProduct,
    orbitDegree = 3,
    demName = "SRTM 3Sec",
    externalDEMFile = NULL,
    externalDEMNoDataValue = 0,
    tileExtensionPercent = 100,
    outputTopoPhaseBand = FALSE,
    outputElevationBand = FALSE,
    outputLatLonBands = FALSE) {
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
  xml_add_child(node, "operator", "TopoPhaseRemoval")
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
    "orbitDegree",
    gpt_args$orbitDegree
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
    "tileExtensionPercent",
    gpt_args$tileExtensionPercent
  )
  xml_add_child(
    parameters,
    "outputTopoPhaseBand",
    gpt_args$outputTopoPhaseBand
  )
  xml_add_child(
    parameters,
    "outputElevationBand",
    gpt_args$outputElevationBand
  )
  xml_add_child(
    parameters,
    "outputLatLonBands",
    gpt_args$outputLatLonBands
  )
  operator_sources <- gpt_args[c("sourceProduct")]
  snap_op_topo_phase_removal <- S7::new_class(
    "snap_op_topo_phase_removal",
    parent = snap_operator
  )
  snap_op_topo_phase_removal(
    operator = "TopoPhaseRemoval",
    operator_id = operator_id,
    operator_sources = operator_sources,
    created_with = rlang::current_fn(),
    xml_graph = as.character(op_xml)
  )
}
