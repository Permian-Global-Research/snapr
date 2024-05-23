#' c2rcc.viirs: snap operator function
#' @param operator_id character operator id
#' @param sourceProduct The source product(s) as input to the operator
#' @param ncepEndProduct The second product providing air pressure values for
#' pressure interpolation. Use either the TOMSOMI and NCEP products or the
#' atmosphericAuxdataPath to as source for ozone and air pressure. This is an
#' optional source.
#' @param ncepStartProduct The first product providing air pressure values for
#' pressure interpolation. Use either the TOMSOMI and NCEP products or the
#' atmosphericAuxdataPath to as source for ozone and air pressure. This is an
#' optional source.
#' @param tomsomiEndProduct The second product providing ozone values for ozone
#' interpolation. Use either the TOMSOMI and NCEP products or the
#' atmosphericAuxdataPath to as source for ozone and air pressure. This is an
#' optional source.
#' @param tomsomiStartProduct The first product providing ozone values for
#' ozone interpolation. Use either the TOMSOMI and NCEP products or the
#' atmosphericAuxdataPath to as source for ozone and air pressure. This is an
#' optional source.
#' @param validPixelExpression Defines the pixels which are valid for
#' processing Default value is '!(l2_flags.LAND || rhot_862 > 0.25)'.
#' @param salinity The value used as salinity for the scene Valid interval is
#' (0.000028, 43). Default value is '35.0'. Parameter unit is 'PSU'.
#' @param temperature The value used as temperature for the scene Valid
#' interval is (0.000111, 36). Default value is '15.0'. Parameter unit is 'C'.
#' @param ozone The value used as ozone if not provided by auxiliary data Valid
#' interval is (0, 1000). Default value is '330'. Parameter unit is 'DU'.
#' @param press The surface air pressure at sea level if not provided by
#' auxiliary data Valid interval is (800, 1040). Default value is '1000'.
#' Parameter unit is 'hPa'.
#' @param atmosphericAuxDataPath Path to the atmospheric auxiliary data
#' directory. Use either this or tomsomiStartProduct, tomsomiEndProduct,
#' ncepStartProduct and ncepEndProduct to use ozone and air pressure aux data for
#' calculations. If the auxiliary data needed for interpolation not available in
#' this path, the data will automatically downloaded.
#' @param outputRtosa Sets parameter 'outputRtosa' to <boolean>. Default value
#' is 'false'.
#' @param outputAsRrs Reflectance values in the target product shall be either
#' written as remote sensing or water leaving reflectances Default value is
#' 'false'.
#' @details
#' Descrscription from '`gpt {operator} -h`':
#'
#' "Performs atmospheric correction and IOP retrieval on Viirs L1C data
#' products."
#' @import xml2
#' @return snap_op_c2rcc_viirs object
#' @export
op_c2rcc_viirs <- function(
    operator_id,
    sourceProduct,
    ncepEndProduct = NULL,
    ncepStartProduct = NULL,
    tomsomiEndProduct = NULL,
    tomsomiStartProduct = NULL,
    validPixelExpression = "!(l2_flags.LAND || rhot_862 > 0.25)",
    salinity = 35.0,
    temperature = 15.0,
    ozone = 330,
    press = 1000,
    atmosphericAuxDataPath = NULL,
    outputRtosa = FALSE,
    outputAsRrs = FALSE) {
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
  xml_add_child(node, "operator", "c2rcc.viirs")
  sources <- xml_add_child(node, "sources")
  op_src_id <- sub(".0", "", paste0(".", seq_along(sourceProduct) - 1))
  purrr::walk2(
    op_src_id,
    sourceProduct,
    function(.x, .y) {
      xml_add_child(sources, paste0("sourceProduct", .x), refid = .y)
    }
  )
  xml_add_child(
    sources,
    "ncepEndProduct",
    gpt_args$ncepEndProduct
  )
  xml_add_child(
    sources,
    "ncepStartProduct",
    gpt_args$ncepStartProduct
  )
  xml_add_child(
    sources,
    "tomsomiEndProduct",
    gpt_args$tomsomiEndProduct
  )
  xml_add_child(
    sources,
    "tomsomiStartProduct",
    gpt_args$tomsomiStartProduct
  )
  parameters <- xml_add_child(node, "parameters")
  xml_add_child(
    parameters,
    "validPixelExpression",
    gpt_args$validPixelExpression
  )
  xml_add_child(
    parameters,
    "salinity",
    gpt_args$salinity
  )
  xml_add_child(
    parameters,
    "temperature",
    gpt_args$temperature
  )
  xml_add_child(
    parameters,
    "ozone",
    gpt_args$ozone
  )
  xml_add_child(
    parameters,
    "press",
    gpt_args$press
  )
  xml_add_child(
    parameters,
    "atmosphericAuxDataPath",
    gpt_args$atmosphericAuxDataPath
  )
  xml_add_child(
    parameters,
    "outputRtosa",
    gpt_args$outputRtosa
  )
  xml_add_child(
    parameters,
    "outputAsRrs",
    gpt_args$outputAsRrs
  )
  operator_sources <- gpt_args[c(
    "sourceProduct",
    "ncepEndProduct",
    "ncepStartProduct",
    "tomsomiEndProduct",
    "tomsomiStartProduct"
  )]
  snap_op_c2rcc_viirs <- S7::new_class(
    "snap_op_c2rcc_viirs",
    parent = snap_operator
  )
  snap_op_c2rcc_viirs(
    operator = "c2rcc.viirs",
    operator_id = operator_id,
    operator_sources = operator_sources,
    created_with = rlang::current_fn(),
    xml_graph = as.character(op_xml)
  )
}
