#' CrossResampling: snap operator function
#' @param operator_id character operator id
#' @param sourceProduct The source product(s) as input to the operator
#' @param warpPolynomialOrder The order of polynomial function Value must be
#' one of '1', '2', '3'. Default value is '2'.
#' @param interpolationMethod Sets parameter 'interpolationMethod' to <string>.
#' Value must be one of 'Cubic convolution (4 points)', 'Cubic convolution (6
#' points)', 'Truncated sinc (6 points)', 'Truncated sinc (8 points)', 'Truncated
#' sinc (16 points)'. Default value is 'Cubic convolution (6 points)'.
#' @param targetGeometry Sets parameter 'targetGeometry' to <string>. Value
#' must be one of 'ERS', 'Envisat ASAR'. Default value is 'ERS'.
#' @details
#' Descrscription from '`gpt {operator} -h`':
#'
#' "Estimate Resampling Polynomial using SAR Image Geometry, and Resample Input
#' Images"
#' @import xml2
#' @return snap_op_cross_resampling object
#' @export
op_cross_resampling <- function(
    operator_id,
    sourceProduct,
    warpPolynomialOrder = 2,
    interpolationMethod = "Cubic convolution (6 points)",
    targetGeometry = "ERS") {
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
  xml_add_child(node, "operator", "CrossResampling")
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
    "targetGeometry",
    gpt_args$targetGeometry
  )
  operator_sources <- gpt_args[c("sourceProduct")]
  snap_op_cross_resampling <- S7::new_class(
    "snap_op_cross_resampling",
    parent = snap_operator
  )
  snap_op_cross_resampling(
    operator = "CrossResampling",
    operator_id = operator_id,
    operator_sources = operator_sources,
    created_with = rlang::current_fn(),
    xml_graph = as.character(op_xml)
  )
}
