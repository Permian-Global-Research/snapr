#' MphChl: snap operator function
#' @param operator_id character operator id
#' @param sourceProduct The source product(s) as input to the operator
#' @param validPixelExpression Expression defining pixels considered for
#' processing. If not set, all valid pixels over water are processed.
#' @param cyanoMaxValue Maximum chlorophyll, arithmetically higher values are
#' capped. Default value is '1000.0'.
#' @param chlThreshForFloatFlag Chlorophyll threshold, above which all
#' cyanobacteria dominated waters are 'float'. Default value is '500.0'.
#' @param exportMph Switch to true to write 'mph' band. Default value is
#' 'false'.
#' @param applyLowPassFilter Switch to true to apply a 3x3 low-pass filter on
#' the result. Default value is 'false'.
#' @details
#' Descrscription from '`gpt {operator} -h`':
#'
#' "This operator computes maximum peak height of chlorophyll (MPH/CHL)."
#' @import xml2
#' @return snap_op_mph_chl object
#' @export
op_mph_chl <- function(
    operator_id,
    sourceProduct,
    validPixelExpression = NULL,
    cyanoMaxValue = 1000.0,
    chlThreshForFloatFlag = 500.0,
    exportMph = FALSE,
    applyLowPassFilter = FALSE) {
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
  xml_add_child(node, "operator", "MphChl")
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
    "validPixelExpression",
    gpt_args$validPixelExpression
  )
  xml_add_child(
    parameters,
    "cyanoMaxValue",
    gpt_args$cyanoMaxValue
  )
  xml_add_child(
    parameters,
    "chlThreshForFloatFlag",
    gpt_args$chlThreshForFloatFlag
  )
  xml_add_child(
    parameters,
    "exportMph",
    gpt_args$exportMph
  )
  xml_add_child(
    parameters,
    "applyLowPassFilter",
    gpt_args$applyLowPassFilter
  )
  operator_sources <- gpt_args[c("sourceProduct")]
  snap_op_mph_chl <- S7::new_class(
    "snap_op_mph_chl",
    parent = snap_operator
  )
  snap_op_mph_chl(
    operator = "MphChl",
    operator_id = operator_id,
    operator_sources = operator_sources,
    created_with = rlang::current_fn(),
    xml_graph = as.character(op_xml)
  )
}
