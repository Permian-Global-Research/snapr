#' PhaseToElevation: snap operator function
#' @param operator_id character operator id
#' @param sourceProduct The source product(s) as input to the operator
#' @param demName The digital elevation model. Default value is 'SRTM 3Sec'.
#' @param demResamplingMethod Sets parameter 'demResamplingMethod' to <string>.
#' Default value is 'BILINEAR_INTERPOLATION'.
#' @param externalDEMFile Sets parameter 'externalDEMFile' to <file>.
#' @param externalDEMNoDataValue Sets parameter 'externalDEMNoDataValue' to
#' <double>. Default value is '0'.
#' @details
#' Descrscription from '`gpt {operator} -h`':
#'
#' "DEM Generation"
#' @import xml2
#' @return snap_op_phase_to_elevation object
#' @export
op_phase_to_elevation <- function(
    operator_id,
    sourceProduct,
    demName = "SRTM 3Sec",
    demResamplingMethod = "BILINEAR_INTERPOLATION",
    externalDEMFile = NULL,
    externalDEMNoDataValue = 0) {
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
  xml_add_child(node, "operator", "PhaseToElevation")
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
    "demName",
    gpt_args$demName
  )
  xml_add_child(
    parameters,
    "demResamplingMethod",
    gpt_args$demResamplingMethod
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
  operator_sources <- gpt_args[c("sourceProduct")]
  snap_op_phase_to_elevation <- S7::new_class(
    "snap_op_phase_to_elevation",
    parent = snap_operator
  )
  snap_op_phase_to_elevation(
    operator = "PhaseToElevation",
    operator_id = operator_id,
    operator_sources = operator_sources,
    created_with = rlang::current_fn(),
    xml_graph = as.character(op_xml)
  )
}
