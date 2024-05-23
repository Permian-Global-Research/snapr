#' SubGraph: snap operator function
#' @param operator_id character operator id
#'
#' @param graphFile Sets parameter 'graphFile' to <file>.
#' @details
#' Descrscription from '`gpt {operator} -h`':
#'
#' "Encapsulates a graph within a graph. Parameter Options: -PgraphFile=<file>
#' Sets parameter 'graphFile' to <file>."
#' @import xml2
#' @return snap_op_sub_graph object
#' @export
op_sub_graph <- function(
    operator_id,
    graphFile = NULL) {
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
  xml_add_child(node, "operator", "SubGraph")
  sources <- xml_add_child(node, "sources")
  parameters <- xml_add_child(node, "parameters")
  xml_add_child(
    parameters,
    "graphFile",
    gpt_args$graphFile
  )
  operator_sources <- gpt_args[c("NA")]
  snap_op_sub_graph <- S7::new_class(
    "snap_op_sub_graph",
    parent = snap_read_operator
  )
  snap_op_sub_graph(
    operator = "SubGraph",
    operator_id = operator_id,
    created_with = rlang::current_fn(),
    xml_graph = as.character(op_xml)
  )
}
