#' Ellipsoid-Correction-GG: snap operator function
#' @param operator_id character operator id
#' @param sourceProduct The source product(s) as input to the operator
#' @param sourceBands The list of source bands.
#' @param imgResamplingMethod Sets parameter 'imgResamplingMethod' to <string>.
#' Value must be one of 'NEAREST_NEIGHBOUR', 'BILINEAR_INTERPOLATION',
#' 'CUBIC_CONVOLUTION'. Default value is 'BILINEAR_INTERPOLATION'.
#' @param mapProjection The coordinate reference system in well known text
#' format Default value is 'WGS84(DD)'.
#' @details
#' Descrscription from '`gpt {operator} -h`':
#'
#' "GG method for orthorectification"
#' @import xml2
#' @return snap_op_ellipsoid_correction_gg object
#' @export
op_ellipsoid_correction_gg <- function(
    operator_id,
    sourceProduct,
    sourceBands = NULL,
    imgResamplingMethod = "BILINEAR_INTERPOLATION",
    mapProjection = "WGS84(DD)") {
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
  xml_add_child(node, "operator", "Ellipsoid-Correction-GG")
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
    "imgResamplingMethod",
    gpt_args$imgResamplingMethod
  )
  xml_add_child(
    parameters,
    "mapProjection",
    gpt_args$mapProjection
  )
  operator_sources <- gpt_args[c("sourceProduct")]
  snap_op_ellipsoid_correction_gg <- S7::new_class(
    "snap_op_ellipsoid_correction_gg",
    parent = snap_operator
  )
  snap_op_ellipsoid_correction_gg(
    operator = "Ellipsoid-Correction-GG",
    operator_id = operator_id,
    operator_sources = operator_sources,
    created_with = rlang::current_fn(),
    xml_graph = as.character(op_xml)
  )
}
