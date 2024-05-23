#' c2rcc.meris4: snap operator function
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
#' processing. Default value is '!quality_flags.invalid && (!quality_flags.land ||
#' quality_flags.fresh_inland_water)'.
#' @param salinity The value used as salinity for the scene. Valid interval is
#' (0.000028, 43). Default value is '35.0'. Parameter unit is 'PSU'.
#' @param temperature The value used as temperature for the scene. Valid
#' interval is (0.000111, 36). Default value is '15.0'. Parameter unit is 'C'.
#' @param ozone The value used as ozone if not provided by auxiliary data.
#' Valid interval is (0, 1000). Default value is '330'. Parameter unit is 'DU'.
#' @param press The surface air pressure at sea level if not provided by
#' auxiliary data. Valid interval is (800, 1040). Default value is '1000'.
#' Parameter unit is 'hPa'.
#' @param CHLexp Chlorophyll exponent ( CHL = iop_apig^CHLexp * CHLfac).
#' Default value is '1.04'.
#' @param thresholdRtosaOOS Threshold for out of scope of nn training dataset
#' flag for gas corrected top-of-atmosphere reflectances. Default value is
#' '0.003'.
#' @param thresholdAcReflecOos Threshold for out of scope of nn training
#' dataset flag for atmospherically corrected reflectances. Default value is
#' '0.1'.
#' @param thresholdCloudTDown865 Threshold for cloud test based on downwelling
#' transmittance @865. Default value is '0.955'.
#' @param atmosphericAuxDataPath Path to the atmospheric auxiliary data
#' directory. Use either this or the specified products on the I/O Parameters tab.
#' If the auxiliary data is not available at this path, the data will
#' automatically be downloaded.
#' @param alternativeNNPath Path to an alternative set of neuronal nets. Use
#' this to replace the standard set of neuronal nets with the ones in the given
#' directory.
#' @param netSet Set of neuronal nets for algorithm. Value must be one of
#' 'C2RCC-Nets', 'C2X-Nets'. Default value is 'C2RCC-Nets'.
#' @param outputAsRrs Write remote sensing reflectances instead of water
#' leaving reflectances. Default value is 'false'.
#' @param deriveRwFromPathAndTransmittance Alternative way of calculating water
#' reflectance. Still experimental. Default value is 'false'.
#' @param useEcmwfAuxData If selected, the ECMWF auxiliary data (total_ozone,
#' sea_level_pressure) of the source product is used Default value is 'true'.
#' @param outputRtoa Sets parameter 'outputRtoa' to <boolean>. Default value is
#' 'true'.
#' @param outputRtosaGc Sets parameter 'outputRtosaGc' to <boolean>. Default
#' value is 'false'.
#' @param outputRtosaGcAann Sets parameter 'outputRtosaGcAann' to <boolean>.
#' Default value is 'false'.
#' @param outputRpath Sets parameter 'outputRpath' to <boolean>. Default value
#' is 'false'.
#' @param outputTdown Sets parameter 'outputTdown' to <boolean>. Default value
#' is 'false'.
#' @param outputTup Sets parameter 'outputTup' to <boolean>. Default value is
#' 'false'.
#' @param outputAcReflectance Sets parameter 'outputAcReflectance' to
#' <boolean>. Default value is 'true'.
#' @param outputRhown Sets parameter 'outputRhown' to <boolean>. Default value
#' is 'true'.
#' @param outputOos Sets parameter 'outputOos' to <boolean>. Default value is
#' 'false'.
#' @param outputKd Sets parameter 'outputKd' to <boolean>. Default value is
#' 'true'.
#' @param outputUncertainties Sets parameter 'outputUncertainties' to
#' <boolean>. Default value is 'true'.
#' @param CHLfac Chlorophyll factor ( CHL = iop_apig^CHLexp * CHLfac). Default
#' value is '21.0'.
#' @param TSMexp TSM exponent (TSM = TSMfac * iop_btot^TSMexp). Default value
#' is '3.1'.
#' @param TSMfac TSM factor (TSM = TSMfac * iop_btot^TSMexp). Default value is
#' '1.72'.
#' @details
#' Descrscription from '`gpt {operator} -h`':
#'
#' "Performs atmospheric correction and IOP retrieval with uncertainties on
#' MERIS L1b data products from the 4th reprocessing."
#' @import xml2
#' @return snap_op_c2rcc_meris4 object
#' @export
op_c2rcc_meris4 <- function(
    operator_id,
    sourceProduct,
    ncepEndProduct = NULL,
    ncepStartProduct = NULL,
    tomsomiEndProduct = NULL,
    tomsomiStartProduct = NULL,
    validPixelExpression = "!quality_flags.invalid && (!quality_flags.land || quality_flags.fresh_inland_water)",
    salinity = 35.0,
    temperature = 15.0,
    ozone = 330,
    press = 1000,
    CHLexp = 1.04,
    thresholdRtosaOOS = 0.003,
    thresholdAcReflecOos = 0.1,
    thresholdCloudTDown865 = 0.955,
    atmosphericAuxDataPath = NULL,
    alternativeNNPath = NULL,
    netSet = "C2RCC-Nets",
    outputAsRrs = FALSE,
    deriveRwFromPathAndTransmittance = FALSE,
    useEcmwfAuxData = TRUE,
    outputRtoa = TRUE,
    outputRtosaGc = FALSE,
    outputRtosaGcAann = FALSE,
    outputRpath = FALSE,
    outputTdown = FALSE,
    outputTup = FALSE,
    outputAcReflectance = TRUE,
    outputRhown = TRUE,
    outputOos = FALSE,
    outputKd = TRUE,
    outputUncertainties = TRUE,
    CHLfac = NULL,
    TSMexp = NULL,
    TSMfac = NULL) {
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
  xml_add_child(node, "operator", "c2rcc.meris4")
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
    "CHLexp",
    gpt_args$CHLexp
  )
  xml_add_child(
    parameters,
    "thresholdRtosaOOS",
    gpt_args$thresholdRtosaOOS
  )
  xml_add_child(
    parameters,
    "thresholdAcReflecOos",
    gpt_args$thresholdAcReflecOos
  )
  xml_add_child(
    parameters,
    "thresholdCloudTDown865",
    gpt_args$thresholdCloudTDown865
  )
  xml_add_child(
    parameters,
    "atmosphericAuxDataPath",
    gpt_args$atmosphericAuxDataPath
  )
  xml_add_child(
    parameters,
    "alternativeNNPath",
    gpt_args$alternativeNNPath
  )
  xml_add_child(
    parameters,
    "netSet",
    gpt_args$netSet
  )
  xml_add_child(
    parameters,
    "outputAsRrs",
    gpt_args$outputAsRrs
  )
  xml_add_child(
    parameters,
    "deriveRwFromPathAndTransmittance",
    gpt_args$deriveRwFromPathAndTransmittance
  )
  xml_add_child(
    parameters,
    "useEcmwfAuxData",
    gpt_args$useEcmwfAuxData
  )
  xml_add_child(
    parameters,
    "outputRtoa",
    gpt_args$outputRtoa
  )
  xml_add_child(
    parameters,
    "outputRtosaGc",
    gpt_args$outputRtosaGc
  )
  xml_add_child(
    parameters,
    "outputRtosaGcAann",
    gpt_args$outputRtosaGcAann
  )
  xml_add_child(
    parameters,
    "outputRpath",
    gpt_args$outputRpath
  )
  xml_add_child(
    parameters,
    "outputTdown",
    gpt_args$outputTdown
  )
  xml_add_child(
    parameters,
    "outputTup",
    gpt_args$outputTup
  )
  xml_add_child(
    parameters,
    "outputAcReflectance",
    gpt_args$outputAcReflectance
  )
  xml_add_child(
    parameters,
    "outputRhown",
    gpt_args$outputRhown
  )
  xml_add_child(
    parameters,
    "outputOos",
    gpt_args$outputOos
  )
  xml_add_child(
    parameters,
    "outputKd",
    gpt_args$outputKd
  )
  xml_add_child(
    parameters,
    "outputUncertainties",
    gpt_args$outputUncertainties
  )
  xml_add_child(
    parameters,
    "CHLfac",
    gpt_args$CHLfac
  )
  xml_add_child(
    parameters,
    "TSMexp",
    gpt_args$TSMexp
  )
  xml_add_child(
    parameters,
    "TSMfac",
    gpt_args$TSMfac
  )
  operator_sources <- gpt_args[c(
    "sourceProduct",
    "ncepEndProduct",
    "ncepStartProduct",
    "tomsomiEndProduct",
    "tomsomiStartProduct"
  )]
  snap_op_c2rcc_meris4 <- S7::new_class(
    "snap_op_c2rcc_meris4",
    parent = snap_operator
  )
  snap_op_c2rcc_meris4(
    operator = "c2rcc.meris4",
    operator_id = operator_id,
    operator_sources = operator_sources,
    created_with = rlang::current_fn(),
    xml_graph = as.character(op_xml)
  )
}
