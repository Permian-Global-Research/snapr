#' PpeFiltering: snap operator function
#' @param operator_id character operator id
#' @param sourceProduct The source product(s) as input to the operator
#' @param cutOff Minimum threshold to differentiate with the neighboring
#' pixels. Default value is '0.7'.
#' @param numberOfMAD Multiplier of MAD (Median Absolute Deviation) used for
#' the threshold. Default value is '10'.
#' @param validExpression An expression to filter which pixel are considered.
#' Default value is 'not quality_flags_land'.
#' @details
#' Descrscription from '`gpt {operator} -h`':
#'
#' "Performs Prompt Particle Event (PPE) filtering on OLCI L1B"
#' @import xml2
#' @return snap_op_ppe_filtering object
#' @export
op_ppe_filtering <- function(
    operator_id,
    sourceProduct,
    cutOff = 0.7,
    numberOfMAD = 10,
    validExpression = "not quality_flags_land") {
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
  xml_add_child(node, "operator", "PpeFiltering")
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
    "cutOff",
    gpt_args$cutOff
  )
  xml_add_child(
    parameters,
    "numberOfMAD",
    gpt_args$numberOfMAD
  )
  xml_add_child(
    parameters,
    "validExpression",
    gpt_args$validExpression
  )
  operator_sources <- gpt_args[c("sourceProduct")]
  snap_op_ppe_filtering <- S7::new_class(
    "snap_op_ppe_filtering",
    parent = snap_operator
  )
  snap_op_ppe_filtering(
    operator = "PpeFiltering",
    operator_id = operator_id,
    operator_sources = operator_sources,
    created_with = rlang::current_fn(),
    xml_graph = as.character(op_xml)
  )
}
