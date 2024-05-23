#' Apply-Orbit-File: snap operator function
#' @param operator_id character operator id
#' @param sourceProduct The source product(s) as input to the operator
#' @param orbitType Sets parameter 'orbitType' to <string>. Value must be one
#' of 'Sentinel Precise (Auto Download)', 'Sentinel Restituted (Auto Download)',
#' 'DORIS Preliminary POR (ENVISAT)', 'DORIS Precise VOR (ENVISAT) (Auto
#' Download)', 'DELFT Precise (ENVISAT, ERS1&2) (Auto Download)', 'PRARE Precise
#' (ERS1&2) (Auto Download)', 'Kompsat5 Precise'. Default value is 'Sentinel
#' Precise (Auto Download)'.
#' @param polyDegree Sets parameter 'polyDegree' to <int>. Default value is
#' '3'.
#' @param continueOnFail Sets parameter 'continueOnFail' to <boolean>. Default
#' value is 'false'.
#' @details
#' Descrscription from '`gpt {operator} -h`':
#'
#' "Apply orbit file"
#' @import xml2
#' @return snap_op_apply_orbit_file object
#' @export
op_apply_orbit_file <- function(
    operator_id,
    sourceProduct,
    orbitType = "Sentinel Precise (Auto Download)",
    polyDegree = 3,
    continueOnFail = FALSE) {
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
  xml_add_child(node, "operator", "Apply-Orbit-File")
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
    "orbitType",
    gpt_args$orbitType
  )
  xml_add_child(
    parameters,
    "polyDegree",
    gpt_args$polyDegree
  )
  xml_add_child(
    parameters,
    "continueOnFail",
    gpt_args$continueOnFail
  )
  operator_sources <- gpt_args[c("sourceProduct")]
  snap_op_apply_orbit_file <- S7::new_class(
    "snap_op_apply_orbit_file",
    parent = snap_operator
  )
  snap_op_apply_orbit_file(
    operator = "Apply-Orbit-File",
    operator_id = operator_id,
    operator_sources = operator_sources,
    created_with = rlang::current_fn(),
    xml_graph = as.character(op_xml)
  )
}
