#' Three-passDInSAR: snap operator function
#' @param operator_id character operator id
#' @param sourceProduct The source product(s) as input to the operator
#' @param orbitDegree Degree of orbit interpolation polynomial Valid interval
#' is (1, 10\]. Default value is '3'.
#' @details
#' Descrscription from '`gpt {operator} -h`':
#'
#' "Differential Interferometry Parameter Options: -PorbitDegree=<int> Degree
#' of orbit interpolation polynomial Valid interval is (1, 10\]. Default value is
#' '3'."
#' @import xml2
#' @return snap_op_three_pass_din_sar object
#' @export
op_three_pass_din_sar <- function(
    operator_id,
    sourceProduct,
    orbitDegree = 3) {
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
  xml_add_child(node, "operator", "Three-passDInSAR")
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
    "orbitDegree",
    gpt_args$orbitDegree
  )
  operator_sources <- gpt_args[c("sourceProduct")]
  snap_op_three_pass_din_sar <- S7::new_class(
    "snap_op_three_pass_din_sar",
    parent = snap_operator
  )
  snap_op_three_pass_din_sar(
    operator = "Three-passDInSAR",
    operator_id = operator_id,
    operator_sources = operator_sources,
    created_with = rlang::current_fn(),
    xml_graph = as.character(op_xml)
  )
}
