#' ChangeVectorAnalysisOp: snap operator function
#' @param operator_id character operator id
#' @param sourceProduct1 The sources product at the first date. This is a
#' mandatory source.
#' @param sourceProduct2 The sources product at the second date. This is a
#' mandatory source.
#' @param sourceBand1 Band 1 at the same date
#' @param sourceBand2 Band 2 at the same date
#' @param magnitudeThreshold No change detection magnitude threshold Default
#' value is '0'.
#' @details
#' Descrscription from '`gpt {operator} -h`':
#'
#' "The 'Change Vector Analysis' between two dual bands at two differents
#' dates."
#' @import xml2
#' @return snap_op_change_vector_analysis_op object
#' @export
op_change_vector_analysis_op <- function(
    operator_id,
    sourceProduct1 = NULL,
    sourceProduct2 = NULL,
    sourceBand1 = NULL,
    sourceBand2 = NULL,
    magnitudeThreshold = 0) {
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
  xml_add_child(node, "operator", "ChangeVectorAnalysisOp")
  sources <- xml_add_child(node, "sources")
  xml_add_child(
    sources,
    "sourceProduct1",
    gpt_args$sourceProduct1
  )
  xml_add_child(
    sources,
    "sourceProduct2",
    gpt_args$sourceProduct2
  )
  parameters <- xml_add_child(node, "parameters")
  xml_add_child(
    parameters,
    "sourceBand1",
    gpt_args$sourceBand1
  )
  xml_add_child(
    parameters,
    "sourceBand2",
    gpt_args$sourceBand2
  )
  xml_add_child(
    parameters,
    "magnitudeThreshold",
    gpt_args$magnitudeThreshold
  )
  operator_sources <- gpt_args[c(
    "sourceProduct1",
    "sourceProduct2"
  )]
  snap_op_change_vector_analysis_op <- S7::new_class(
    "snap_op_change_vector_analysis_op",
    parent = snap_operator
  )
  snap_op_change_vector_analysis_op(
    operator = "ChangeVectorAnalysisOp",
    operator_id = operator_id,
    operator_sources = operator_sources,
    created_with = rlang::current_fn(),
    xml_graph = as.character(op_xml)
  )
}
