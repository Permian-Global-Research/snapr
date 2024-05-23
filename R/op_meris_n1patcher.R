#' Meris.N1Patcher: snap operator function
#' @param operator_id character operator id
#' @param input The source product provides the data to be written into the
#' patched file. This is a mandatory source.
#' @param n1 The N1 file which is used as a template. This is a mandatory
#' source.
#' @param copyAllTiePoints If set to 'false' only the lat and lon tie-points
#' will be copied to the target product Default value is 'false'.
#' @param patchedFile The file to which the patched L1b product is written.
#' This is a mandatory parameter. Value must not be empty.
#' @details
#' Descrscription from '`gpt {operator} -h`':
#'
#' "Copies an existing N1 file and replaces the data for the radiance bands"
#' @import xml2
#' @return snap_op_meris_n1patcher object
#' @export
op_meris_n1patcher <- function(
    operator_id,
    input = NULL,
    n1 = NULL,
    copyAllTiePoints = FALSE,
    patchedFile = NULL) {
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
  xml_add_child(node, "operator", "Meris.N1Patcher")
  sources <- xml_add_child(node, "sources")
  xml_add_child(
    sources,
    "input",
    gpt_args$input
  )
  xml_add_child(
    sources,
    "n1",
    gpt_args$n1
  )
  parameters <- xml_add_child(node, "parameters")
  xml_add_child(
    parameters,
    "copyAllTiePoints",
    gpt_args$copyAllTiePoints
  )
  xml_add_child(
    parameters,
    "patchedFile",
    gpt_args$patchedFile
  )
  operator_sources <- gpt_args[c(
    "input",
    "n1"
  )]
  snap_op_meris_n1patcher <- S7::new_class(
    "snap_op_meris_n1patcher",
    parent = snap_operator
  )
  snap_op_meris_n1patcher(
    operator = "Meris.N1Patcher",
    operator_id = operator_id,
    operator_sources = operator_sources,
    created_with = rlang::current_fn(),
    xml_graph = as.character(op_xml)
  )
}
