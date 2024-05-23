#' RangeFilter: snap operator function
#' @param operator_id character operator id
#' @param sourceProduct The source product(s) as input to the operator
#' @param fftLength Length of filtering window Value must be one of '8', '16',
#' '32', '64', '128', '256', '512', '1024'. Default value is '8'.
#' @param alphaHamming Weight for Hamming filter (1 is rectangular window)
#' Value must be one of '0.5', '0.75', '0.8', '0.9', '1'. Default value is '0.75'.
#' @param nlMean Input value for (walking) mean averaging to reduce noise.
#' Value must be one of '5', '10', '15', '20', '25'. Default value is '15'.
#' @param snrThresh Threshold on SNR for peak estimation Value must be one of
#' '3', '4', '5', '6', '7'. Default value is '5'.
#' @param ovsmpFactor Oversampling factor (in range only). Value must be one of
#' '1', '2', '4'. Default value is '1'.
#' @param doWeightCorrel Use weight values to bias higher frequencies Value
#' must be one of 'true', 'false'. Default value is 'off'.
#' @details
#' Descrscription from '`gpt {operator} -h`':
#'
#' "Range Filter"
#' @import xml2
#' @return snap_op_range_filter object
#' @export
op_range_filter <- function(
    operator_id,
    sourceProduct,
    fftLength = 8,
    alphaHamming = 0.75,
    nlMean = 15,
    snrThresh = 5,
    ovsmpFactor = 1,
    doWeightCorrel = FALSE) {
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
  xml_add_child(node, "operator", "RangeFilter")
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
    "fftLength",
    gpt_args$fftLength
  )
  xml_add_child(
    parameters,
    "alphaHamming",
    gpt_args$alphaHamming
  )
  xml_add_child(
    parameters,
    "nlMean",
    gpt_args$nlMean
  )
  xml_add_child(
    parameters,
    "snrThresh",
    gpt_args$snrThresh
  )
  xml_add_child(
    parameters,
    "ovsmpFactor",
    gpt_args$ovsmpFactor
  )
  xml_add_child(
    parameters,
    "doWeightCorrel",
    gpt_args$doWeightCorrel
  )
  operator_sources <- gpt_args[c("sourceProduct")]
  snap_op_range_filter <- S7::new_class(
    "snap_op_range_filter",
    parent = snap_operator
  )
  snap_op_range_filter(
    operator = "RangeFilter",
    operator_id = operator_id,
    operator_sources = operator_sources,
    created_with = rlang::current_fn(),
    xml_graph = as.character(op_xml)
  )
}
