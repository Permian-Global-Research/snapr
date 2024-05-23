#' OlciAnomalyFlagging: snap operator function
#' @param operator_id character operator id
#' @param l1bProduct OLCI L1b or fully compatible product. This is a mandatory
#' source.
#' @param writeSlopeInformation If set to true, the operator adds two bands
#' containing the maximal spectral slope and the band index where the peak is
#' observed. Default value is 'false'.
#' @details
#' Descrscription from '`gpt {operator} -h`':
#'
#' "Adds a flagging band indicating saturated pixels and altitude data
#' overflows"
#' @import xml2
#' @return snap_op_olci_anomaly_flagging object
#' @export
op_olci_anomaly_flagging <- function(
    operator_id,
    l1bProduct = NULL,
    writeSlopeInformation = FALSE) {
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
  xml_add_child(node, "operator", "OlciAnomalyFlagging")
  sources <- xml_add_child(node, "sources")
  xml_add_child(
    sources,
    "l1bProduct",
    gpt_args$l1bProduct
  )
  parameters <- xml_add_child(node, "parameters")
  xml_add_child(
    parameters,
    "writeSlopeInformation",
    gpt_args$writeSlopeInformation
  )
  operator_sources <- gpt_args[c("l1bProduct")]
  snap_op_olci_anomaly_flagging <- S7::new_class(
    "snap_op_olci_anomaly_flagging",
    parent = snap_operator
  )
  snap_op_olci_anomaly_flagging(
    operator = "OlciAnomalyFlagging",
    operator_id = operator_id,
    operator_sources = operator_sources,
    created_with = rlang::current_fn(),
    xml_graph = as.character(op_xml)
  )
}
