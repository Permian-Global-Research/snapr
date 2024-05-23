#' PhaseToHeight: snap operator function
#' @param operator_id character operator id
#' @param sourceProduct The source product(s) as input to the operator
#' @param nPoints Number of points for evaluation of flat earth phase at
#' different altitudes Value must be one of '100', '200', '300', '400', '500'.
#' Default value is '200'.
#' @param nHeights Number of height samples in range \[0,5000) Value must be one
#' of '2', '3', '4', '5'. Default value is '3'.
#' @param degree1D Degree of the 1D polynomial to fit reference phase through.
#' Value must be one of '1', '2', '3', '4', '5'. Default value is '2'.
#' @param degree2D Degree of the 2D polynomial to fit reference phase through.
#' Value must be one of '1', '2', '3', '4', '5', '6', '7', '8'. Default value is
#' '5'.
#' @param orbitDegree Degree of orbit (polynomial) interpolator Value must be
#' one of '2', '3', '4', '5'. Default value is '3'.
#' @details
#' Descrscription from '`gpt {operator} -h`':
#'
#' "Phase to Height conversion"
#' @import xml2
#' @return snap_op_phase_to_height object
#' @export
op_phase_to_height <- function(
    operator_id,
    sourceProduct,
    nPoints = 200,
    nHeights = 3,
    degree1D = 2,
    degree2D = 5,
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
  xml_add_child(node, "operator", "PhaseToHeight")
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
    "nPoints",
    gpt_args$nPoints
  )
  xml_add_child(
    parameters,
    "nHeights",
    gpt_args$nHeights
  )
  xml_add_child(
    parameters,
    "degree1D",
    gpt_args$degree1D
  )
  xml_add_child(
    parameters,
    "degree2D",
    gpt_args$degree2D
  )
  xml_add_child(
    parameters,
    "orbitDegree",
    gpt_args$orbitDegree
  )
  operator_sources <- gpt_args[c("sourceProduct")]
  snap_op_phase_to_height <- S7::new_class(
    "snap_op_phase_to_height",
    parent = snap_operator
  )
  snap_op_phase_to_height(
    operator = "PhaseToHeight",
    operator_id = operator_id,
    operator_sources = operator_sources,
    created_with = rlang::current_fn(),
    xml_graph = as.character(op_xml)
  )
}
