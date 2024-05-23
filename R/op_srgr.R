#' SRGR: snap operator function
#' @param operator_id character operator id
#' @param sourceProduct The source product(s) as input to the operator
#' @param sourceBands The list of source bands.
#' @param warpPolynomialOrder The order of WARP polynomial function Value must
#' be one of '1', '2', '3', '4'. Default value is '4'.
#' @param interpolationMethod Sets parameter 'interpolationMethod' to <string>.
#' Value must be one of 'Nearest-neighbor interpolation', 'Linear interpolation',
#' 'Cubic interpolation', 'Cubic2 interpolation', 'Sinc interpolation'. Default
#' value is 'Linear interpolation'.
#' @details
#' Descrscription from '`gpt {operator} -h`':
#'
#' "Converts Slant Range to Ground Range"
#' @import xml2
#' @return snap_op_srgr object
#' @export
op_srgr <- function(
    operator_id,
    sourceProduct,
    sourceBands = NULL,
    warpPolynomialOrder = 4,
    interpolationMethod = "Linear interpolation") {
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
  xml_add_child(node, "operator", "SRGR")
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
    "warpPolynomialOrder",
    gpt_args$warpPolynomialOrder
  )
  xml_add_child(
    parameters,
    "interpolationMethod",
    gpt_args$interpolationMethod
  )
  operator_sources <- gpt_args[c("sourceProduct")]
  snap_op_srgr <- S7::new_class(
    "snap_op_srgr",
    parent = snap_operator
  )
  snap_op_srgr(
    operator = "SRGR",
    operator_id = operator_id,
    operator_sources = operator_sources,
    created_with = rlang::current_fn(),
    xml_graph = as.character(op_xml)
  )
}
