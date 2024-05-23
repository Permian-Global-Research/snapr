#' CoregistrationOp: snap operator function
#' @param operator_id character operator id
#' @param sourceProduct The source product(s) as input to the operator
#' @param sourceProduct.1 The source product which serves as slave. This is a
#' mandatory source.
#' @param masterSourceBand The master product band
#' @param slaveSourceBand The slave product band
#' @param levels The number of levels to process the images. Default value is
#' '6'.
#' @param rank Value used to compute the rank. Default value is '4'.
#' @param iterations The number of interations for each level and for each
#' radius. Default value is '2'.
#' @param radius The radius integer values splitted by comma. Default value is
#' '32, 28, 24, 20, 16, 12, 8'.
#' @details
#' Descrscription from '`gpt {operator} -h`':
#'
#' "Coregisters two rasters, not considering their location"
#' @import xml2
#' @return snap_op_coregistration_op object
#' @export
op_coregistration_op <- function(
    operator_id,
    sourceProduct,
    sourceProduct.1 = NULL,
    masterSourceBand = NULL,
    slaveSourceBand = NULL,
    levels = 6,
    rank = 4,
    iterations = 2,
    radius = "32, 28, 24, 20, 16, 12, 8") {
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
  xml_add_child(node, "operator", "CoregistrationOp")
  sources <- xml_add_child(node, "sources")
  op_src_id <- sub(".0", "", paste0(".", seq_along(sourceProduct) - 1))
  purrr::walk2(
    op_src_id,
    sourceProduct,
    function(.x, .y) {
      xml_add_child(sources, paste0("sourceProduct", .x), refid = .y)
    }
  )
  xml_add_child(
    sources,
    "sourceProduct.1",
    gpt_args$sourceProduct.1
  )
  parameters <- xml_add_child(node, "parameters")
  xml_add_child(
    parameters,
    "masterSourceBand",
    gpt_args$masterSourceBand
  )
  xml_add_child(
    parameters,
    "slaveSourceBand",
    gpt_args$slaveSourceBand
  )
  xml_add_child(
    parameters,
    "levels",
    gpt_args$levels
  )
  xml_add_child(
    parameters,
    "rank",
    gpt_args$rank
  )
  xml_add_child(
    parameters,
    "iterations",
    gpt_args$iterations
  )
  xml_add_child(
    parameters,
    "radius",
    gpt_args$radius
  )
  operator_sources <- gpt_args[c(
    "sourceProduct",
    "sourceProduct.1"
  )]
  snap_op_coregistration_op <- S7::new_class(
    "snap_op_coregistration_op",
    parent = snap_operator
  )
  snap_op_coregistration_op(
    operator = "CoregistrationOp",
    operator_id = operator_id,
    operator_sources = operator_sources,
    created_with = rlang::current_fn(),
    xml_graph = as.character(op_xml)
  )
}
