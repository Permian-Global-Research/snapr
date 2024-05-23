#' ToolAdapterOp: snap operator function
#' @param operator_id character operator id
#'
#' @details
#' Descrscription from '`gpt {operator} -h`':
#'
#' "Tool Adapter Operator"
#' @import xml2
#' @return snap_op_tool_adapter_op object
#' @export
op_tool_adapter_op <- function(
    operator_id) {
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
  xml_add_child(node, "operator", "ToolAdapterOp")
  sources <- xml_add_child(node, "sources")
  parameters <- xml_add_child(node, "parameters")

  operator_sources <- gpt_args[c("NA")]
  snap_op_tool_adapter_op <- S7::new_class(
    "snap_op_tool_adapter_op",
    parent = snap_read_operator
  )
  snap_op_tool_adapter_op(
    operator = "ToolAdapterOp",
    operator_id = operator_id,
    created_with = rlang::current_fn(),
    xml_graph = as.character(op_xml)
  )
}
