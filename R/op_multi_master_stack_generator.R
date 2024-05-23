#' MultiMasterStackGenerator: snap operator function
#' @param operator_id character operator id
#' @param sourceProduct The source product(s) as input to the operator
#' @param outputFolder Output folder
#' @details
#' Descrscription from '`gpt {operator} -h`':
#'
#' "Generates a set of master-slave pairs from a coregistered stack for use in
#' SBAS processing"
#' @import xml2
#' @return snap_op_multi_master_stack_generator object
#' @export
op_multi_master_stack_generator <- function(
    operator_id,
    sourceProduct,
    outputFolder = NULL) {
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
  xml_add_child(node, "operator", "MultiMasterStackGenerator")
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
    "outputFolder",
    gpt_args$outputFolder
  )
  operator_sources <- gpt_args[c("sourceProduct")]
  snap_op_multi_master_stack_generator <- S7::new_class(
    "snap_op_multi_master_stack_generator",
    parent = snap_operator
  )
  snap_op_multi_master_stack_generator(
    operator = "MultiMasterStackGenerator",
    operator_id = operator_id,
    operator_sources = operator_sources,
    created_with = rlang::current_fn(),
    xml_graph = as.character(op_xml)
  )
}
