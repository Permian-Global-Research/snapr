#' LandWaterMask: snap operator function
#' @param operator_id character operator id
#' @param sourceProduct The source product(s) as input to the operator
#' @param resolution Specifies on which resolution the water mask shall be
#' based. Value must be one of '50', '150', '1000'. Default value is '50'.
#' Parameter unit is 'm/pixel'.
#' @param subSamplingFactorX Specifies the factor between the resolution of the
#' source product and the watermask in x direction. A value of '1' means no
#' subsampling at all. Default value is '1'. This is a mandatory parameter.
#' @param subSamplingFactorY Specifies the factor between the resolution of the
#' source product and the watermask iny direction. A value of '1' means no
#' subsampling at all. Default value is '1'. This is a mandatory parameter.
#' @details
#' Descrscription from '`gpt {operator} -h`':
#'
#' "Operator creating a target product with a single band containing a
#' land/water-mask."
#' @import xml2
#' @return snap_op_land_water_mask object
#' @export
op_land_water_mask <- function(
    operator_id,
    sourceProduct,
    resolution = 50,
    subSamplingFactorX = 1,
    subSamplingFactorY = 1) {
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
  xml_add_child(node, "operator", "LandWaterMask")
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
    "resolution",
    gpt_args$resolution
  )
  xml_add_child(
    parameters,
    "subSamplingFactorX",
    gpt_args$subSamplingFactorX
  )
  xml_add_child(
    parameters,
    "subSamplingFactorY",
    gpt_args$subSamplingFactorY
  )
  operator_sources <- gpt_args[c("sourceProduct")]
  snap_op_land_water_mask <- S7::new_class(
    "snap_op_land_water_mask",
    parent = snap_operator
  )
  snap_op_land_water_mask(
    operator = "LandWaterMask",
    operator_id = operator_id,
    operator_sources = operator_sources,
    created_with = rlang::current_fn(),
    xml_graph = as.character(op_xml)
  )
}
