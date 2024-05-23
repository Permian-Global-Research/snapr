#' AdaptiveThresholding: snap operator function
#' @param operator_id character operator id
#' @param sourceProduct The source product(s) as input to the operator
#' @param targetWindowSizeInMeter Target window size Default value is '50'.
#' @param guardWindowSizeInMeter Guard window size Default value is '500.0'.
#' @param backgroundWindowSizeInMeter Background window size Default value is
#' '800.0'.
#' @param pfa Probability of false alarm Default value is '6.5'.
#' @param estimateBackground Rough estimation of background threshold for
#' quicker processing Default value is 'false'.
#' @details
#' Descrscription from '`gpt {operator} -h`':
#'
#' "Detect ships using Constant False Alarm Rate detector."
#' @import xml2
#' @return snap_op_adaptive_thresholding object
#' @export
op_adaptive_thresholding <- function(
    operator_id,
    sourceProduct,
    targetWindowSizeInMeter = 50,
    guardWindowSizeInMeter = 500.0,
    backgroundWindowSizeInMeter = 800.0,
    pfa = 6.5,
    estimateBackground = FALSE) {
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
  xml_add_child(node, "operator", "AdaptiveThresholding")
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
    "targetWindowSizeInMeter",
    gpt_args$targetWindowSizeInMeter
  )
  xml_add_child(
    parameters,
    "guardWindowSizeInMeter",
    gpt_args$guardWindowSizeInMeter
  )
  xml_add_child(
    parameters,
    "backgroundWindowSizeInMeter",
    gpt_args$backgroundWindowSizeInMeter
  )
  xml_add_child(
    parameters,
    "pfa",
    gpt_args$pfa
  )
  xml_add_child(
    parameters,
    "estimateBackground",
    gpt_args$estimateBackground
  )
  operator_sources <- gpt_args[c("sourceProduct")]
  snap_op_adaptive_thresholding <- S7::new_class(
    "snap_op_adaptive_thresholding",
    parent = snap_operator
  )
  snap_op_adaptive_thresholding(
    operator = "AdaptiveThresholding",
    operator_id = operator_id,
    operator_sources = operator_sources,
    created_with = rlang::current_fn(),
    xml_graph = as.character(op_xml)
  )
}
