#' Warp: snap operator function
#' @param operator_id character operator id
#' @param sourceProduct The source product(s) as input to the operator
#' @param rmsThreshold Confidence level for outlier detection procedure, lower
#' value accepts more outliers Value must be one of '0.001', '0.05', '0.1', '0.5',
#' '1.0'. Default value is '0.05'.
#' @param warpPolynomialOrder The order of WARP polynomial function Value must
#' be one of '1', '2', '3'. Default value is '2'.
#' @param interpolationMethod Sets parameter 'interpolationMethod' to <string>.
#' Value must be one of 'Nearest-neighbor interpolation', 'Bilinear
#' interpolation', 'Bicubic interpolation', 'Bicubic2 interpolation', 'Linear
#' interpolation', 'Cubic convolution (4 points)', 'Cubic convolution (6 points)',
#' 'Truncated sinc (6 points)', 'Truncated sinc (8 points)', 'Truncated sinc (16
#' points)'. Default value is 'Cubic convolution (6 points)'.
#' @param demRefinement Refine estimated offsets using a-priori DEM Default
#' value is 'false'.
#' @param demName The digital elevation model. Default value is 'SRTM 3Sec'.
#' @param excludeMaster Sets parameter 'excludeMaster' to <boolean>. Default
#' value is 'false'.
#' @param openResidualsFile Show the Residuals file in a text viewer Default
#' value is 'false'.
#' @details
#' Descrscription from '`gpt {operator} -h`':
#'
#' "Create Warp Function And Get Co-registrated Images"
#' @import xml2
#' @return snap_op_warp object
#' @export
op_warp <- function(
    operator_id,
    sourceProduct,
    rmsThreshold = 0.05,
    warpPolynomialOrder = 2,
    interpolationMethod = "Cubic convolution (6 points)",
    demRefinement = FALSE,
    demName = "SRTM 3Sec",
    excludeMaster = FALSE,
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
  xml_add_child(node, "operator", "Warp")
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
    "interpolationMethod",
    gpt_args$interpolationMethod
  )
  xml_add_child(
    parameters,
    "demRefinement",
    gpt_args$demRefinement
  )
  xml_add_child(
    parameters,
    "demName",
    gpt_args$demName
  )
  xml_add_child(
    parameters,
    "excludeMaster",
    gpt_args$excludeMaster
  )
  xml_add_child(
    parameters,
    "openResidualsFile",
    gpt_args$openResidualsFile
  )
  operator_sources <- gpt_args[c("sourceProduct")]
  snap_op_warp <- S7::new_class(
    "snap_op_warp",
    parent = snap_operator
  )
  snap_op_warp(
    operator = "Warp",
    operator_id = operator_id,
    operator_sources = operator_sources,
    created_with = rlang::current_fn(),
    xml_graph = as.character(op_xml)
  )
}
