#' Change-Detection: snap operator function
#' @param operator_id character operator id
#' @param sourceProduct The source product(s) as input to the operator
#' @param sourceBands The list of source bands.
#' @param maskUpperThreshold Mask upper threshold Default value is '2.0'.
#' @param maskLowerThreshold Mask lower threshold Default value is '-2.0'.
#' @param includeSourceBands Include source bands Default value is 'false'.
#' @param outputLogRatio Output Log Ratio Default value is 'false'.
#' @details
#' Descrscription from '`gpt {operator} -h`':
#'
#' "Change Detection."
#' @import xml2
#' @return snap_op_change_detection object
#' @export
op_change_detection <- function(
    operator_id,
    sourceProduct,
    sourceBands = NULL,
    maskUpperThreshold = 2.0,
    maskLowerThreshold = -2.0,
    includeSourceBands = FALSE,
    outputLogRatio = FALSE) {
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
  xml_add_child(node, "operator", "Change-Detection")
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
    "sourceBands",
    gpt_args$sourceBands
  )
  xml_add_child(
    parameters,
    "maskUpperThreshold",
    gpt_args$maskUpperThreshold
  )
  xml_add_child(
    parameters,
    "maskLowerThreshold",
    gpt_args$maskLowerThreshold
  )
  xml_add_child(
    parameters,
    "includeSourceBands",
    gpt_args$includeSourceBands
  )
  xml_add_child(
    parameters,
    "outputLogRatio",
    gpt_args$outputLogRatio
  )
  operator_sources <- gpt_args[c("sourceProduct")]
  snap_op_change_detection <- S7::new_class(
    "snap_op_change_detection",
    parent = snap_operator
  )
  snap_op_change_detection(
    operator = "Change-Detection",
    operator_id = operator_id,
    operator_sources = operator_sources,
    created_with = rlang::current_fn(),
    xml_graph = as.character(op_xml)
  )
}
