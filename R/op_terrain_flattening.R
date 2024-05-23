#' Terrain-Flattening: snap operator function
#' @param operator_id character operator id
#' @param sourceProduct The source product(s) as input to the operator
#' @param sourceBands The list of source bands.
#' @param demName The digital elevation model. Default value is 'SRTM 1Sec
#' HGT'.
#' @param demResamplingMethod Sets parameter 'demResamplingMethod' to <string>.
#' Default value is 'BILINEAR_INTERPOLATION'.
#' @param externalDEMFile Sets parameter 'externalDEMFile' to <file>.
#' @param externalDEMNoDataValue Sets parameter 'externalDEMNoDataValue' to
#' <double>. Default value is '0'.
#' @param externalDEMApplyEGM Sets parameter 'externalDEMApplyEGM' to
#' <boolean>. Default value is 'false'.
#' @param outputSimulatedImage Sets parameter 'outputSimulatedImage' to
#' <boolean>. Default value is 'false'.
#' @param outputSigma0 Sets parameter 'outputSigma0' to <boolean>. Default
#' value is 'false'.
#' @param nodataValueAtSea Mask the sea with no data value (faster) Default
#' value is 'true'.
#' @param additionalOverlap The additional overlap percentage Valid interval is
#' \[0, 1\]. Default value is '0.1'.
#' @param oversamplingMultiple The oversampling factor Valid interval is \[1,
#' 4\]. Default value is '1.0'.
#' @details
#' Descrscription from '`gpt {operator} -h`':
#'
#' "Terrain Flattening"
#' @import xml2
#' @return snap_op_terrain_flattening object
#' @export
op_terrain_flattening <- function(
    operator_id,
    sourceProduct,
    sourceBands = NULL,
    demName = "SRTM 1Sec HGT",
    demResamplingMethod = "BILINEAR_INTERPOLATION",
    externalDEMFile = NULL,
    externalDEMNoDataValue = 0,
    externalDEMApplyEGM = FALSE,
    outputSimulatedImage = FALSE,
    outputSigma0 = FALSE,
    nodataValueAtSea = TRUE,
    additionalOverlap = 0.1,
    oversamplingMultiple = 1.0) {
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
  xml_add_child(node, "operator", "Terrain-Flattening")
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
    "externalDEMApplyEGM",
    gpt_args$externalDEMApplyEGM
  )
  xml_add_child(
    parameters,
    "outputSimulatedImage",
    gpt_args$outputSimulatedImage
  )
  xml_add_child(
    parameters,
    "outputSigma0",
    gpt_args$outputSigma0
  )
  xml_add_child(
    parameters,
    "nodataValueAtSea",
    gpt_args$nodataValueAtSea
  )
  xml_add_child(
    parameters,
    "additionalOverlap",
    gpt_args$additionalOverlap
  )
  xml_add_child(
    parameters,
    "oversamplingMultiple",
    gpt_args$oversamplingMultiple
  )
  operator_sources <- gpt_args[c("sourceProduct")]
  snap_op_terrain_flattening <- S7::new_class(
    "snap_op_terrain_flattening",
    parent = snap_operator
  )
  snap_op_terrain_flattening(
    operator = "Terrain-Flattening",
    operator_id = operator_id,
    operator_sources = operator_sources,
    created_with = rlang::current_fn(),
    xml_graph = as.character(op_xml)
  )
}
