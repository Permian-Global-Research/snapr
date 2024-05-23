#' Oil-Spill-Clustering: snap operator function
#' @param operator_id character operator id
#' @param sourceProduct The source product(s) as input to the operator
#' @param minClusterSizeInKm2 Minimum cluster size Default value is '0.1'.
#' @details
#' Descrscription from '`gpt {operator} -h`':
#'
#' "Remove small clusters from detected area."
#' @import xml2
#' @return snap_op_oil_spill_clustering object
#' @export
op_oil_spill_clustering <- function(
    operator_id,
    sourceProduct,
    minClusterSizeInKm2 = 0.1) {
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
  xml_add_child(node, "operator", "Oil-Spill-Clustering")
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
    "minClusterSizeInKm2",
    gpt_args$minClusterSizeInKm2
  )
  operator_sources <- gpt_args[c("sourceProduct")]
  snap_op_oil_spill_clustering <- S7::new_class(
    "snap_op_oil_spill_clustering",
    parent = snap_operator
  )
  snap_op_oil_spill_clustering(
    operator = "Oil-Spill-Clustering",
    operator_id = operator_id,
    operator_sources = operator_sources,
    created_with = rlang::current_fn(),
    xml_graph = as.character(op_xml)
  )
}
