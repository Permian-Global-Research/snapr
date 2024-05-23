#' RayleighCorrection: snap operator function
#' @param operator_id character operator id
#' @param sourceProduct The source product(s) as input to the operator
#' @param computeTaur Sets parameter 'computeTaur' to <boolean>. Default value
#' is 'false'.
#' @param computeRBrr Sets parameter 'computeRBrr' to <boolean>. Default value
#' is 'true'.
#' @param computeRtoaNg Sets parameter 'computeRtoaNg' to <boolean>. Default
#' value is 'false'.
#' @param computeRtoa Sets parameter 'computeRtoa' to <boolean>. Default value
#' is 'false'.
#' @param addAirMass Sets parameter 'addAirMass' to <boolean>. Default value is
#' 'false'.
#' @param s2MsiTargetResolution Sets parameter 's2MsiTargetResolution' to
#' <int>. Value must be one of '10', '20', '60'. Default value is '20'.
#' @param s2MsiSeaLevelPressure Sets parameter 's2MsiSeaLevelPressure' to
#' <double>. Default value is '1013.25'.
#' @param s2MsiOzone Sets parameter 's2MsiOzone' to <double>. Default value is
#' '300.0'.
#' @param sourceBandNames The source bands for the computation.
#' @details
#' Descrscription from '`gpt {operator} -h`':
#'
#' "Performs radiometric corrections on OLCI, MERIS L1B and S2 MSI L1C data
#' products."
#' @import xml2
#' @return snap_op_rayleigh_correction object
#' @export
op_rayleigh_correction <- function(
    operator_id,
    sourceProduct,
    computeTaur = FALSE,
    computeRBrr = TRUE,
    computeRtoaNg = FALSE,
    computeRtoa = FALSE,
    addAirMass = FALSE,
    s2MsiTargetResolution = 20,
    s2MsiSeaLevelPressure = 1013.25,
    s2MsiOzone = 300.0,
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
  xml_add_child(node, "operator", "RayleighCorrection")
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
    "computeTaur",
    gpt_args$computeTaur
  )
  xml_add_child(
    parameters,
    "computeRBrr",
    gpt_args$computeRBrr
  )
  xml_add_child(
    parameters,
    "computeRtoaNg",
    gpt_args$computeRtoaNg
  )
  xml_add_child(
    parameters,
    "computeRtoa",
    gpt_args$computeRtoa
  )
  xml_add_child(
    parameters,
    "addAirMass",
    gpt_args$addAirMass
  )
  xml_add_child(
    parameters,
    "s2MsiTargetResolution",
    gpt_args$s2MsiTargetResolution
  )
  xml_add_child(
    parameters,
    "s2MsiSeaLevelPressure",
    gpt_args$s2MsiSeaLevelPressure
  )
  xml_add_child(
    parameters,
    "s2MsiOzone",
    gpt_args$s2MsiOzone
  )
  xml_add_child(
    parameters,
    "sourceBandNames",
    gpt_args$sourceBandNames
  )
  operator_sources <- gpt_args[c("sourceProduct")]
  snap_op_rayleigh_correction <- S7::new_class(
    "snap_op_rayleigh_correction",
    parent = snap_operator
  )
  snap_op_rayleigh_correction(
    operator = "RayleighCorrection",
    operator_id = operator_id,
    operator_sources = operator_sources,
    created_with = rlang::current_fn(),
    xml_graph = as.character(op_xml)
  )
}
