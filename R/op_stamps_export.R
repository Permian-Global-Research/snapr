#' StampsExport: snap operator function
#' @param operator_id character operator id
#' @param sourceProduct The source product(s) as input to the operator
#' @param targetFolder The output folder to which the data product is written.
#' @param psiFormat Format for PSI or SBAS Default value is 'true'.
#' @details
#' Descrscription from '`gpt {operator} -h`':
#'
#' "Export data for StaMPS processing Parameter Options: -PpsiFormat=<boolean>
#' Format for PSI or SBAS Default value is 'true'. -PtargetFolder=<file> The
#' output folder to which the data product is written."
#' @import xml2
#' @return snap_op_stamps_export object
#' @export
op_stamps_export <- function(
    operator_id,
    sourceProduct,
    targetFolder = NULL,
    psiFormat = TRUE) {
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
  xml_add_child(node, "operator", "StampsExport")
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
    "targetFolder",
    gpt_args$targetFolder
  )
  xml_add_child(
    parameters,
    "psiFormat",
    gpt_args$psiFormat
  )
  operator_sources <- gpt_args[c("sourceProduct")]
  snap_op_stamps_export <- S7::new_class(
    "snap_op_stamps_export",
    parent = snap_operator
  )
  snap_op_stamps_export(
    operator = "StampsExport",
    operator_id = operator_id,
    operator_sources = operator_sources,
    created_with = rlang::current_fn(),
    xml_graph = as.character(op_xml)
  )
}
