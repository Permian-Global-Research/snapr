#' Polarimetric-Parameters: snap operator function
#' @param operator_id character operator id
#' @param sourceProduct The source product(s) as input to the operator
#' @param useMeanMatrix Use mean coherency or covariance matrix Default value
#' is 'true'.
#' @param windowSizeXStr Sets parameter 'windowSizeXStr' to <string>. Value
#' must be one of '3', '5', '7', '9', '11', '13', '15', '17', '19'. Default value
#' is '5'.
#' @param windowSizeYStr Sets parameter 'windowSizeYStr' to <string>. Value
#' must be one of '3', '5', '7', '9', '11', '13', '15', '17', '19'. Default value
#' is '5'.
#' @param outputSpan Output Span Default value is 'true'.
#' @param outputPedestalHeight Output pedestal height Default value is 'false'.
#' @param outputRVI Output RVI Default value is 'false'.
#' @param outputRFDI Output RFDI Default value is 'false'.
#' @param outputCSI Output CSI Default value is 'false'.
#' @param outputVSI Output VSI Default value is 'false'.
#' @param outputBMI Output BMI Default value is 'false'.
#' @param outputITI Output ITI Default value is 'false'.
#' @param outputHHVVRatio Output Co-Pol HH/VV Default value is 'false'.
#' @param outputHHHVRatio Output Cross-Pol HH/HV Default value is 'false'.
#' @param outputVVVHRatio Output Cross-Pol VV/VH Default value is 'false'.
#' @details
#' Descrscription from '`gpt {operator} -h`':
#'
#' "Compute general polarimetric parameters"
#' @import xml2
#' @return snap_op_polarimetric_parameters object
#' @export
op_polarimetric_parameters <- function(
    operator_id,
    sourceProduct,
    useMeanMatrix = TRUE,
    windowSizeXStr = 5,
    windowSizeYStr = 5,
    outputSpan = TRUE,
    outputPedestalHeight = FALSE,
    outputRVI = FALSE,
    outputRFDI = FALSE,
    outputCSI = FALSE,
    outputVSI = FALSE,
    outputBMI = FALSE,
    outputITI = FALSE,
    outputHHVVRatio = FALSE,
    outputHHHVRatio = FALSE,
    outputVVVHRatio = FALSE) {
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
  xml_add_child(node, "operator", "Polarimetric-Parameters")
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
    "useMeanMatrix",
    gpt_args$useMeanMatrix
  )
  xml_add_child(
    parameters,
    "windowSizeXStr",
    gpt_args$windowSizeXStr
  )
  xml_add_child(
    parameters,
    "windowSizeYStr",
    gpt_args$windowSizeYStr
  )
  xml_add_child(
    parameters,
    "outputSpan",
    gpt_args$outputSpan
  )
  xml_add_child(
    parameters,
    "outputPedestalHeight",
    gpt_args$outputPedestalHeight
  )
  xml_add_child(
    parameters,
    "outputRVI",
    gpt_args$outputRVI
  )
  xml_add_child(
    parameters,
    "outputRFDI",
    gpt_args$outputRFDI
  )
  xml_add_child(
    parameters,
    "outputCSI",
    gpt_args$outputCSI
  )
  xml_add_child(
    parameters,
    "outputVSI",
    gpt_args$outputVSI
  )
  xml_add_child(
    parameters,
    "outputBMI",
    gpt_args$outputBMI
  )
  xml_add_child(
    parameters,
    "outputITI",
    gpt_args$outputITI
  )
  xml_add_child(
    parameters,
    "outputHHVVRatio",
    gpt_args$outputHHVVRatio
  )
  xml_add_child(
    parameters,
    "outputHHHVRatio",
    gpt_args$outputHHHVRatio
  )
  xml_add_child(
    parameters,
    "outputVVVHRatio",
    gpt_args$outputVVVHRatio
  )
  operator_sources <- gpt_args[c("sourceProduct")]
  snap_op_polarimetric_parameters <- S7::new_class(
    "snap_op_polarimetric_parameters",
    parent = snap_operator
  )
  snap_op_polarimetric_parameters(
    operator = "Polarimetric-Parameters",
    operator_id = operator_id,
    operator_sources = operator_sources,
    created_with = rlang::current_fn(),
    xml_graph = as.character(op_xml)
  )
}
