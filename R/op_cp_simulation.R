#' CP-Simulation: snap operator function
#' @param operator_id character operator id
#' @param sourceProduct The source product(s) as input to the operator
#' @param compactMode The compact mode Value must be one of 'Right Circular
#' Hybrid Mode', 'Left Circular Hybrid Mode'. Default value is 'Right Circular
#' Hybrid Mode'.
#' @param outputFormat The output simulated compact pol data format Value must
#' be one of 'Covariance Matrix C2', 'Scatter Vector S2'. Default value is
#' 'Covariance Matrix C2'.
#' @param noisePower The noise power Valid interval is (-35, -15). Default
#' value is '-25'.
#' @param simulateNoiseFloor Simulate noise floor Default value is 'false'.
#' @details
#' Descrscription from '`gpt {operator} -h`':
#'
#' "Simulation of Compact Pol data from Quad Pol data"
#' @import xml2
#' @return snap_op_cp_simulation object
#' @export
op_cp_simulation <- function(
    operator_id,
    sourceProduct,
    compactMode = "Right Circular Hybrid Mode",
    outputFormat = "Covariance Matrix C2",
    noisePower = -25,
    simulateNoiseFloor = FALSE) {
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
  xml_add_child(node, "operator", "CP-Simulation")
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
    "compactMode",
    gpt_args$compactMode
  )
  xml_add_child(
    parameters,
    "outputFormat",
    gpt_args$outputFormat
  )
  xml_add_child(
    parameters,
    "noisePower",
    gpt_args$noisePower
  )
  xml_add_child(
    parameters,
    "simulateNoiseFloor",
    gpt_args$simulateNoiseFloor
  )
  operator_sources <- gpt_args[c("sourceProduct")]
  snap_op_cp_simulation <- S7::new_class(
    "snap_op_cp_simulation",
    parent = snap_operator
  )
  snap_op_cp_simulation(
    operator = "CP-Simulation",
    operator_id = operator_id,
    operator_sources = operator_sources,
    created_with = rlang::current_fn(),
    xml_graph = as.character(op_xml)
  )
}
