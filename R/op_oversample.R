#' Oversample: snap operator function
#' @param operator_id character operator id
#' @param sourceProduct The source product(s) as input to the operator
#' @param sourceBands The list of source bands.
#' @param outputImageBy Sets parameter 'outputImageBy' to <string>. Value must
#' be one of 'Image Size', 'Ratio', 'Pixel Spacing'. Default value is 'Ratio'.
#' @param targetImageHeight The row dimension of the output image Default value
#' is '1000'.
#' @param targetImageWidth The col dimension of the output image Default value
#' is '1000'.
#' @param widthRatio The width ratio of the output/input images Default value
#' is '2.0'.
#' @param heightRatio The height ratio of the output/input images Default value
#' is '2.0'.
#' @param rangeSpacing The range pixel spacing Default value is '12.5'.
#' @param azimuthSpacing The azimuth pixel spacing Default value is '12.5'.
#' @param usePRFTileSize use PRF as azimuth tile size and range line as range
#' tile size Default value is 'false'.
#' @details
#' Descrscription from '`gpt {operator} -h`':
#'
#' "Oversample the datset"
#' @import xml2
#' @return snap_op_oversample object
#' @export
op_oversample <- function(
    operator_id,
    sourceProduct,
    sourceBands = NULL,
    outputImageBy = "Ratio",
    targetImageHeight = 1000,
    targetImageWidth = 1000,
    widthRatio = 2.0,
    heightRatio = 2.0,
    rangeSpacing = 12.5,
    azimuthSpacing = 12.5,
    usePRFTileSize = FALSE) {
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
  xml_add_child(node, "operator", "Oversample")
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
    "outputImageBy",
    gpt_args$outputImageBy
  )
  xml_add_child(
    parameters,
    "targetImageHeight",
    gpt_args$targetImageHeight
  )
  xml_add_child(
    parameters,
    "targetImageWidth",
    gpt_args$targetImageWidth
  )
  xml_add_child(
    parameters,
    "widthRatio",
    gpt_args$widthRatio
  )
  xml_add_child(
    parameters,
    "heightRatio",
    gpt_args$heightRatio
  )
  xml_add_child(
    parameters,
    "rangeSpacing",
    gpt_args$rangeSpacing
  )
  xml_add_child(
    parameters,
    "azimuthSpacing",
    gpt_args$azimuthSpacing
  )
  xml_add_child(
    parameters,
    "usePRFTileSize",
    gpt_args$usePRFTileSize
  )
  operator_sources <- gpt_args[c("sourceProduct")]
  snap_op_oversample <- S7::new_class(
    "snap_op_oversample",
    parent = snap_operator
  )
  snap_op_oversample(
    operator = "Oversample",
    operator_id = operator_id,
    operator_sources = operator_sources,
    created_with = rlang::current_fn(),
    xml_graph = as.character(op_xml)
  )
}
