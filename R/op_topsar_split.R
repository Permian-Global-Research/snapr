#' TOPSAR-Split: snap operator function
#' @param operator_id character operator id
#' @param sourceProduct The source product(s) as input to the operator
#' @param subswath The list of source bands.
#' @param selectedPolarisations The list of polarisations
#' @param firstBurstIndex The first burst index Valid interval is \[1, *).
#' Default value is '1'.
#' @param lastBurstIndex The last burst index Valid interval is \[1, *). Default
#' value is '9999'.
#' @param wktAoi WKT polygon to be used for selecting bursts
#' @details
#' Descrscription from '`gpt {operator} -h`':
#'
#' "Creates a new product with only the selected subswath"
#' @import xml2
#' @return snap_op_topsar_split object
#' @export
op_topsar_split <- function(
    operator_id,
    sourceProduct,
    subswath = NULL,
    selectedPolarisations = NULL,
    firstBurstIndex = 1,
    lastBurstIndex = 9999,
    wktAoi = NULL) {
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
  xml_add_child(node, "operator", "TOPSAR-Split")
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
    "subswath",
    gpt_args$subswath
  )
  xml_add_child(
    parameters,
    "selectedPolarisations",
    gpt_args$selectedPolarisations
  )
  xml_add_child(
    parameters,
    "firstBurstIndex",
    gpt_args$firstBurstIndex
  )
  xml_add_child(
    parameters,
    "lastBurstIndex",
    gpt_args$lastBurstIndex
  )
  xml_add_child(
    parameters,
    "wktAoi",
    gpt_args$wktAoi
  )
  operator_sources <- gpt_args[c("sourceProduct")]
  snap_op_topsar_split <- S7::new_class(
    "snap_op_topsar_split",
    parent = snap_operator
  )
  snap_op_topsar_split(
    operator = "TOPSAR-Split",
    operator_id = operator_id,
    operator_sources = operator_sources,
    created_with = rlang::current_fn(),
    xml_graph = as.character(op_xml)
  )
}
