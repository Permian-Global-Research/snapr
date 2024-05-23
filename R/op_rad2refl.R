#' Rad2Refl: snap operator function
#' @param operator_id character operator id
#' @param sourceProduct The source product(s) as input to the operator
#' @param sensor The sensor Value must be one of 'MERIS', 'OLCI', 'SLSTR_500m'.
#' Default value is 'OLCI'.
#' @param conversionMode Conversion mode: from rad to refl, or backwards Value
#' must be one of 'RAD_TO_REFL', 'REFL_TO_RAD'. Default value is 'RAD_TO_REFL'.
#' @param copyTiePointGrids If set, all tie point grids from source product are
#' written to target product Default value is 'false'.
#' @param copyFlagBandsAndMasks If set, all flag bands and masks from source
#' product are written to target product Default value is 'false'.
#' @param copyNonSpectralBands If set, all other non-spectral bands from source
#' product are written to target product Default value is 'false'.
#' @details
#' Descrscription from '`gpt {operator} -h`':
#'
#' "Provides conversion from radiances to reflectances or backwards."
#' @import xml2
#' @return snap_op_rad2refl object
#' @export
op_rad2refl <- function(
    operator_id,
    sourceProduct,
    sensor = "OLCI",
    conversionMode = "RAD_TO_REFL",
    copyTiePointGrids = FALSE,
    copyFlagBandsAndMasks = FALSE,
    copyNonSpectralBands = FALSE) {
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
  xml_add_child(node, "operator", "Rad2Refl")
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
    "sensor",
    gpt_args$sensor
  )
  xml_add_child(
    parameters,
    "conversionMode",
    gpt_args$conversionMode
  )
  xml_add_child(
    parameters,
    "copyTiePointGrids",
    gpt_args$copyTiePointGrids
  )
  xml_add_child(
    parameters,
    "copyFlagBandsAndMasks",
    gpt_args$copyFlagBandsAndMasks
  )
  xml_add_child(
    parameters,
    "copyNonSpectralBands",
    gpt_args$copyNonSpectralBands
  )
  operator_sources <- gpt_args[c("sourceProduct")]
  snap_op_rad2refl <- S7::new_class(
    "snap_op_rad2refl",
    parent = snap_operator
  )
  snap_op_rad2refl(
    operator = "Rad2Refl",
    operator_id = operator_id,
    operator_sources = operator_sources,
    created_with = rlang::current_fn(),
    xml_graph = as.character(op_xml)
  )
}
