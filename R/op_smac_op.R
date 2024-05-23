#' SmacOp: snap operator function
#' @param operator_id character operator id
#' @param sourceProduct The source product(s) as input to the operator
#' @param tauAero550 Aerosol optical depth Default value is '0.2'.
#' @param uH2o Relative humidity Default value is '3.0'. Parameter unit is
#' 'g/cm²'.
#' @param uO3 Ozone content Default value is '0.15'. Parameter unit is 'cm *
#' atm'.
#' @param surfPress Surface pressure Default value is '1013.0'. Parameter unit
#' is 'hPa'.
#' @param useMerisADS Use ECMWF data in the MERIS ADS Default value is 'false'.
#' @param aerosolType Aerosol type Default value is 'CONTINENTAL'. This is a
#' mandatory parameter.
#' @param invalidPixel Default reflectance for invalid pixel Default value is
#' '0.0'.
#' @param maskExpression Mask expression for the whole view (MERIS) or the
#' nadir view (AATSR)
#' @param maskExpressionForward Mask expression for the forward view (AATSR
#' only)
#' @param bandNames Bands to process This is a mandatory parameter.
#' @details
#' Descrscription from '`gpt {operator} -h`':
#'
#' "Applies the Simplified Method for Atmospheric Corrections of Envisat
#' MERIS/(A)ATSR measurements."
#' @import xml2
#' @return snap_op_smac_op object
#' @export
op_smac_op <- function(
    operator_id,
    sourceProduct,
    tauAero550 = 0.2,
    uH2o = 3.0,
    uO3 = 0.15,
    surfPress = 1013.0,
    useMerisADS = FALSE,
    aerosolType = "CONTINENTAL",
    invalidPixel = 0.0,
    maskExpression = NULL,
    maskExpressionForward = NULL,
    bandNames = NULL) {
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
  xml_add_child(node, "operator", "SmacOp")
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
    "tauAero550",
    gpt_args$tauAero550
  )
  xml_add_child(
    parameters,
    "uH2o",
    gpt_args$uH2o
  )
  xml_add_child(
    parameters,
    "uO3",
    gpt_args$uO3
  )
  xml_add_child(
    parameters,
    "surfPress",
    gpt_args$surfPress
  )
  xml_add_child(
    parameters,
    "useMerisADS",
    gpt_args$useMerisADS
  )
  xml_add_child(
    parameters,
    "aerosolType",
    gpt_args$aerosolType
  )
  xml_add_child(
    parameters,
    "invalidPixel",
    gpt_args$invalidPixel
  )
  xml_add_child(
    parameters,
    "maskExpression",
    gpt_args$maskExpression
  )
  xml_add_child(
    parameters,
    "maskExpressionForward",
    gpt_args$maskExpressionForward
  )
  xml_add_child(
    parameters,
    "bandNames",
    gpt_args$bandNames
  )
  operator_sources <- gpt_args[c("sourceProduct")]
  snap_op_smac_op <- S7::new_class(
    "snap_op_smac_op",
    parent = snap_operator
  )
  snap_op_smac_op(
    operator = "SmacOp",
    operator_id = operator_id,
    operator_sources = operator_sources,
    created_with = rlang::current_fn(),
    xml_graph = as.character(op_xml)
  )
}
