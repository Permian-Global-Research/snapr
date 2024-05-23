#' MultiMasterInSAR: snap operator function
#' @param operator_id character operator id
#' @param sourceProduct The source product(s) as input to the operator
#' @param orbitDegree Degree of orbit (polynomial) interpolator Value must be
#' one of '1', '2', '3', '4', '5'. Default value is '4'.
#' @param pairs List of interferometric pairs
#' @param includeWavenumber Sets parameter 'includeWavenumber' to <boolean>.
#' Default value is 'true'.
#' @param includeIncidenceAngle Sets parameter 'includeIncidenceAngle' to
#' <boolean>. Default value is 'true'.
#' @param includeLatLon Sets parameter 'includeLatLon' to <boolean>. Default
#' value is 'true'.
#' @param cohWindowAz Size of coherence estimation window in azimuth Default
#' value is '10'.
#' @param cohWindowRg Size of coherence estimation window in range Default
#' value is '10'.
#' @details
#' Descrscription from '`gpt {operator} -h`':
#'
#' "Multi-master InSAR processing"
#' @import xml2
#' @return snap_op_multi_master_in_sar object
#' @export
op_multi_master_in_sar <- function(
    operator_id,
    sourceProduct,
    orbitDegree = 4,
    pairs = NULL,
    includeWavenumber = TRUE,
    includeIncidenceAngle = TRUE,
    includeLatLon = TRUE,
    cohWindowAz = 10,
    cohWindowRg = 10) {
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
  xml_add_child(node, "operator", "MultiMasterInSAR")
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
  xml_add_child(
    parameters,
    "pairs",
    gpt_args$pairs
  )
  xml_add_child(
    parameters,
    "includeWavenumber",
    gpt_args$includeWavenumber
  )
  xml_add_child(
    parameters,
    "includeIncidenceAngle",
    gpt_args$includeIncidenceAngle
  )
  xml_add_child(
    parameters,
    "includeLatLon",
    gpt_args$includeLatLon
  )
  xml_add_child(
    parameters,
    "cohWindowAz",
    gpt_args$cohWindowAz
  )
  xml_add_child(
    parameters,
    "cohWindowRg",
    gpt_args$cohWindowRg
  )
  operator_sources <- gpt_args[c("sourceProduct")]
  snap_op_multi_master_in_sar <- S7::new_class(
    "snap_op_multi_master_in_sar",
    parent = snap_operator
  )
  snap_op_multi_master_in_sar(
    operator = "MultiMasterInSAR",
    operator_id = operator_id,
    operator_sources = operator_sources,
    created_with = rlang::current_fn(),
    xml_graph = as.character(op_xml)
  )
}
