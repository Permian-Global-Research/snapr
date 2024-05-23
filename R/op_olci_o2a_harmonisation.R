#' OlciO2aHarmonisation: snap operator function
#' @param operator_id character operator id
#' @param l1bProduct OLCI L1b or fully compatible product. May contain an
#' optional altitude band, i.e. introduced from an external DEM. This is a
#' mandatory source.
#' @param alternativeAltitudeBandName Name of alternative altitude band in
#' source product (i.e. introduced from an external DEM). Altitude is expected in
#' meters.
#' @param processOnlyBand13 If set to true, only band 13 needed for cloud
#' detection will be processed, otherwise bands 13-15. Default value is 'true'.
#' @param writeHarmonisedRadiances If set to true, harmonised radiances of
#' processed band(s) will be written to target product. Default value is 'true'.
#' @details
#' Descrscription from '`gpt {operator} -h`':
#'
#' "Performs O2A band harmonisation on OLCI L1b product. Implements update v4
#' of R.Preusker, June 2020."
#' @import xml2
#' @return snap_op_olci_o2a_harmonisation object
#' @export
op_olci_o2a_harmonisation <- function(
    operator_id,
    l1bProduct = NULL,
    alternativeAltitudeBandName = NULL,
    processOnlyBand13 = TRUE,
    writeHarmonisedRadiances = TRUE) {
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
  xml_add_child(node, "operator", "OlciO2aHarmonisation")
  sources <- xml_add_child(node, "sources")
  xml_add_child(
    sources,
    "l1bProduct",
    gpt_args$l1bProduct
  )
  parameters <- xml_add_child(node, "parameters")
  xml_add_child(
    parameters,
    "alternativeAltitudeBandName",
    gpt_args$alternativeAltitudeBandName
  )
  xml_add_child(
    parameters,
    "processOnlyBand13",
    gpt_args$processOnlyBand13
  )
  xml_add_child(
    parameters,
    "writeHarmonisedRadiances",
    gpt_args$writeHarmonisedRadiances
  )
  operator_sources <- gpt_args[c("l1bProduct")]
  snap_op_olci_o2a_harmonisation <- S7::new_class(
    "snap_op_olci_o2a_harmonisation",
    parent = snap_operator
  )
  snap_op_olci_o2a_harmonisation(
    operator = "OlciO2aHarmonisation",
    operator_id = operator_id,
    operator_sources = operator_sources,
    created_with = rlang::current_fn(),
    xml_graph = as.character(op_xml)
  )
}
