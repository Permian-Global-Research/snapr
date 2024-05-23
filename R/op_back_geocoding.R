#' Back-Geocoding: snap operator function
#' @param operator_id character operator id
#' @param sourceProduct The source product(s) as input to the operator
#' @param demName The digital elevation model. Default value is 'SRTM 3Sec'.
#' @param demResamplingMethod Sets parameter 'demResamplingMethod' to <string>.
#' Default value is 'BICUBIC_INTERPOLATION'.
#' @param externalDEMFile Sets parameter 'externalDEMFile' to <file>.
#' @param externalDEMNoDataValue Sets parameter 'externalDEMNoDataValue' to
#' <double>. Default value is '0'.
#' @param resamplingType The method to be used when resampling the slave grid
#' onto the master grid. Default value is 'BISINC_5_POINT_INTERPOLATION'.
#' @param maskOutAreaWithoutElevation Sets parameter
#' 'maskOutAreaWithoutElevation' to <boolean>. Default value is 'true'.
#' @param outputRangeAzimuthOffset Sets parameter 'outputRangeAzimuthOffset' to
#' <boolean>. Default value is 'false'.
#' @param outputDerampDemodPhase Sets parameter 'outputDerampDemodPhase' to
#' <boolean>. Default value is 'false'.
#' @param disableReramp Sets parameter 'disableReramp' to <boolean>. Default
#' value is 'false'.
#' @details
#' Descrscription from '`gpt {operator} -h`':
#'
#' "Bursts co-registration using orbit and DEM Parameter Options:
#' -PdemName=<string> The digital elevation model. Default value is 'SRTM 3Sec'.
#' -PdemResamplingMethod=<string> Sets parameter 'demResamplingMethod' to
#' <string>. Default value is 'BICUBIC_INTERPOLATION'. -PdisableReramp=<boolean>
#' Sets parameter 'disableReramp' to <boolean>. Default value is 'false'.
#' -PexternalDEMFile=<file> Sets parameter 'externalDEMFile' to <file>.
#' -PexternalDEMNoDataValue=<double> Sets parameter 'externalDEMNoDataValue' to
#' <double>. Default value is '0'. -PmaskOutAreaWithoutElevation=<boolean> Sets
#' parameter 'maskOutAreaWithoutElevation' to <boolean>. Default value is 'true'.
#' -PoutputDerampDemodPhase=<boolean> Sets parameter 'outputDerampDemodPhase' to
#' <boolean>. Default value is 'false'. -PoutputRangeAzimuthOffset=<boolean> Sets
#' parameter 'outputRangeAzimuthOffset' to <boolean>. Default value is 'false'.
#' -PresamplingType=<string> The method to be used when resampling the slave grid
#' onto the master grid. Default value is 'BISINC_5_POINT_INTERPOLATION'."
#' @import xml2
#' @return snap_op_back_geocoding object
#' @export
op_back_geocoding <- function(
    operator_id,
    sourceProduct,
    demName = "SRTM 3Sec",
    demResamplingMethod = "BICUBIC_INTERPOLATION",
    externalDEMFile = NULL,
    externalDEMNoDataValue = 0,
    resamplingType = "BISINC_5_POINT_INTERPOLATION",
    maskOutAreaWithoutElevation = TRUE,
    outputRangeAzimuthOffset = FALSE,
    outputDerampDemodPhase = FALSE,
    disableReramp = FALSE) {
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
  xml_add_child(node, "operator", "Back-Geocoding")
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
    "demName",
    gpt_args$demName
  )
  xml_add_child(
    parameters,
    "demResamplingMethod",
    gpt_args$demResamplingMethod
  )
  xml_add_child(
    parameters,
    "externalDEMFile",
    gpt_args$externalDEMFile
  )
  xml_add_child(
    parameters,
    "externalDEMNoDataValue",
    gpt_args$externalDEMNoDataValue
  )
  xml_add_child(
    parameters,
    "resamplingType",
    gpt_args$resamplingType
  )
  xml_add_child(
    parameters,
    "maskOutAreaWithoutElevation",
    gpt_args$maskOutAreaWithoutElevation
  )
  xml_add_child(
    parameters,
    "outputRangeAzimuthOffset",
    gpt_args$outputRangeAzimuthOffset
  )
  xml_add_child(
    parameters,
    "outputDerampDemodPhase",
    gpt_args$outputDerampDemodPhase
  )
  xml_add_child(
    parameters,
    "disableReramp",
    gpt_args$disableReramp
  )
  operator_sources <- gpt_args[c("sourceProduct")]
  snap_op_back_geocoding <- S7::new_class(
    "snap_op_back_geocoding",
    parent = snap_operator
  )
  snap_op_back_geocoding(
    operator = "Back-Geocoding",
    operator_id = operator_id,
    operator_sources = operator_sources,
    created_with = rlang::current_fn(),
    xml_graph = as.character(op_xml)
  )
}
