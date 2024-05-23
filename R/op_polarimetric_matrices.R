#' Polarimetric-Matrices: snap operator function
#' @param operator_id character operator id
#' @param sourceProduct The source product(s) as input to the operator
#' @param matrix The covariance or coherency matrix Value must be one of 'C2',
#' 'C3', 'C4', 'T3', 'T4'. Default value is 'T3'.
#' @details
#' Descrscription from '`gpt {operator} -h`':
#'
#' "Generates covariance or coherency matrix for given product"
#' @import xml2
#' @return snap_op_polarimetric_matrices object
#' @export
op_polarimetric_matrices <- function(
    operator_id,
    sourceProduct,
    matrix = "T3") {
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
  xml_add_child(node, "operator", "Polarimetric-Matrices")
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
    "matrix",
    gpt_args$matrix
  )
  operator_sources <- gpt_args[c("sourceProduct")]
  snap_op_polarimetric_matrices <- S7::new_class(
    "snap_op_polarimetric_matrices",
    parent = snap_operator
  )
  snap_op_polarimetric_matrices(
    operator = "Polarimetric-Matrices",
    operator_id = operator_id,
    operator_sources = operator_sources,
    created_with = rlang::current_fn(),
    xml_graph = as.character(op_xml)
  )
}
