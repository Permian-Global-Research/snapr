#' ProductSet-Reader: snap operator function
#' @param operator_id character operator id
#'
#' @param fileList Sets parameter 'fileList' to <string,string,string,...>.
#' @details
#' Descrscription from '`gpt {operator} -h`':
#'
#' "Adds a list of sources Parameter Options:
#' -PfileList=<string,string,string,...> Sets parameter 'fileList' to
#' <string,string,string,...>."
#' @import xml2
#' @return snap_op_product_set_reader object
#' @export
op_product_set_reader <- function(
    operator_id,
    fileList = NULL) {
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
  xml_add_child(node, "operator", "ProductSet-Reader")
  sources <- xml_add_child(node, "sources")
  parameters <- xml_add_child(node, "parameters")
  xml_add_child(
    parameters,
    "fileList",
    gpt_args$fileList
  )
  operator_sources <- gpt_args[c("NA")]
  snap_op_product_set_reader <- S7::new_class(
    "snap_op_product_set_reader",
    parent = snap_read_operator
  )
  snap_op_product_set_reader(
    operator = "ProductSet-Reader",
    operator_id = operator_id,
    created_with = rlang::current_fn(),
    xml_graph = as.character(op_xml)
  )
}
