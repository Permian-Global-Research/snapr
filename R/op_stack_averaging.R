#' Stack-Averaging: snap operator function
#' @param operator_id character operator id
#' @param sourceProduct The source product(s) as input to the operator
#' @param statistic Sets parameter 'statistic' to <string>. Value must be one
#' of 'Mean Average', 'Minimum', 'Maximum', 'Standard Deviation', 'Coefficient of
#' Variation'. Default value is 'Mean Average'.
#' @details
#' Descrscription from '`gpt {operator} -h`':
#'
#' "Averaging multi-temporal images"
#' @import xml2
#' @return snap_op_stack_averaging object
#' @export
op_stack_averaging <- function(
    operator_id,
    sourceProduct,
    statistic = "Mean Average") {
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
  xml_add_child(node, "operator", "Stack-Averaging")
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
    "statistic",
    gpt_args$statistic
  )
  operator_sources <- gpt_args[c("sourceProduct")]
  snap_op_stack_averaging <- S7::new_class(
    "snap_op_stack_averaging",
    parent = snap_operator
  )
  snap_op_stack_averaging(
    operator = "Stack-Averaging",
    operator_id = operator_id,
    operator_sources = operator_sources,
    created_with = rlang::current_fn(),
    xml_graph = as.character(op_xml)
  )
}
