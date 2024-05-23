#' Terrain-Mask: snap operator function
#' @param operator_id character operator id
#' @param sourceProduct The source product(s) as input to the operator
#' @param demName The digital elevation model. Value must be one of 'ACE',
#' 'ASTER 1sec GDEM', 'GETASSE30', 'SRTM 1Sec HGT', 'SRTM 3Sec'. Default value is
#' 'SRTM 3Sec'.
#' @param demResamplingMethod Sets parameter 'demResamplingMethod' to <string>.
#' Value must be one of 'NEAREST_NEIGHBOUR', 'BILINEAR_INTERPOLATION',
#' 'CUBIC_CONVOLUTION', 'BICUBIC_INTERPOLATION', 'BISINC_5_POINT_INTERPOLATION'.
#' Default value is 'NEAREST_NEIGHBOUR'.
#' @param externalDEMFile Sets parameter 'externalDEMFile' to <file>.
#' @param externalDEMNoDataValue Sets parameter 'externalDEMNoDataValue' to
#' <double>. Default value is '0'.
#' @param windowSizeStr Sets parameter 'windowSizeStr' to <string>. Value must
#' be one of '5x5', '7x7', '9x9', '11x11', '13x13', '15x15', '17x17'. Default
#' value is '15x15'.
#' @param thresholdInMeter Threshold for detection Valid interval is (0, *).
#' Default value is '40.0'.
#' @details
#' Descrscription from '`gpt {operator} -h`':
#'
#' "Terrain Mask Generation"
#' @import xml2
#' @return snap_op_terrain_mask object
#' @export
op_terrain_mask <- function(
    operator_id,
    sourceProduct,
    demName = "SRTM 3Sec",
    demResamplingMethod = "NEAREST_NEIGHBOUR",
    externalDEMFile = NULL,
    externalDEMNoDataValue = 0,
    windowSizeStr = "15x15",
    thresholdInMeter = 40.0) {
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
  xml_add_child(node, "operator", "Terrain-Mask")
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
  xml_add_child(
    parameters,
    "windowSizeStr",
    gpt_args$windowSizeStr
  )
  xml_add_child(
    parameters,
    "thresholdInMeter",
    gpt_args$thresholdInMeter
  )
  operator_sources <- gpt_args[c("sourceProduct")]
  snap_op_terrain_mask <- S7::new_class(
    "snap_op_terrain_mask",
    parent = snap_operator
  )
  snap_op_terrain_mask(
    operator = "Terrain-Mask",
    operator_id = operator_id,
    operator_sources = operator_sources,
    created_with = rlang::current_fn(),
    xml_graph = as.character(op_xml)
  )
}
