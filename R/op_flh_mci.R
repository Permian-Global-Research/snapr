#' FlhMci: snap operator function
#' @param operator_id character operator id
#' @param sourceProduct The source product(s) as input to the operator
#' @param preset Sets default values according to the selected preset. The
#' specific parameters have precedence and override the ones from the preset
#' Default value is 'NONE'.
#' @param lowerBaselineBandName The name for the lower wavelength band defining
#' the baseline
#' @param upperBaselineBandName The name of the upper wavelength band defining
#' the baseline
#' @param signalBandName The name of the signal band, i.e. the band for which
#' the baseline height is calculated
#' @param lineHeightBandName The name of the line height band in the target
#' product
#' @param slope Activates or deactivates calculating the slope parameter
#' Default value is 'true'.
#' @param slopeBandName The name of the slope band in the target product
#' @param maskExpression A ROI-mask expression used to identify pixels of
#' interest
#' @param cloudCorrectionFactor The cloud correction factor used during
#' calculation Default value is '1.005'.
#' @param invalidFlhMciValue Value used to fill invalid FLH/MCI pixels Default
#' value is 'NaN'.
#' @details
#' Descrscription from '`gpt {operator} -h`':
#'
#' "Computes fluorescence line height (FLH) or maximum chlorophyll index
#' (MCI)."
#' @import xml2
#' @return snap_op_flh_mci object
#' @export
op_flh_mci <- function(
    operator_id,
    sourceProduct,
    preset = "NONE",
    lowerBaselineBandName = NULL,
    upperBaselineBandName = NULL,
    signalBandName = NULL,
    lineHeightBandName = NULL,
    slope = TRUE,
    slopeBandName = NULL,
    maskExpression = NULL,
    cloudCorrectionFactor = 1.005,
    invalidFlhMciValue = "NaN") {
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
  xml_add_child(node, "operator", "FlhMci")
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
    "preset",
    gpt_args$preset
  )
  xml_add_child(
    parameters,
    "lowerBaselineBandName",
    gpt_args$lowerBaselineBandName
  )
  xml_add_child(
    parameters,
    "upperBaselineBandName",
    gpt_args$upperBaselineBandName
  )
  xml_add_child(
    parameters,
    "signalBandName",
    gpt_args$signalBandName
  )
  xml_add_child(
    parameters,
    "lineHeightBandName",
    gpt_args$lineHeightBandName
  )
  xml_add_child(
    parameters,
    "slope",
    gpt_args$slope
  )
  xml_add_child(
    parameters,
    "slopeBandName",
    gpt_args$slopeBandName
  )
  xml_add_child(
    parameters,
    "maskExpression",
    gpt_args$maskExpression
  )
  xml_add_child(
    parameters,
    "cloudCorrectionFactor",
    gpt_args$cloudCorrectionFactor
  )
  xml_add_child(
    parameters,
    "invalidFlhMciValue",
    gpt_args$invalidFlhMciValue
  )
  operator_sources <- gpt_args[c("sourceProduct")]
  snap_op_flh_mci <- S7::new_class(
    "snap_op_flh_mci",
    parent = snap_operator
  )
  snap_op_flh_mci(
    operator = "FlhMci",
    operator_id = operator_id,
    operator_sources = operator_sources,
    created_with = rlang::current_fn(),
    xml_graph = as.character(op_xml)
  )
}
