#' Radar-Vegetation-Index: snap operator function
#' @param operator_id character operator id
#' @param sourceProduct The source product(s) as input to the operator
#' @param windowSize The sliding window size Valid interval is \[1, 100\].
#' Default value is '5'.
#' @details
#' Descrscription from '`gpt {operator} -h`':
#'
#' "Dual-pol Radar Vegetation Indices generation"
#' @import xml2
#' @return snap_op_radar_vegetation_index object
#' @export
op_radar_vegetation_index <- function(
    operator_id,
    sourceProduct,
    windowSize = 5) {
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
  xml_add_child(node, "operator", "Radar-Vegetation-Index")
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
    "windowSize",
    gpt_args$windowSize
  )
  operator_sources <- gpt_args[c("sourceProduct")]
  snap_op_radar_vegetation_index <- S7::new_class(
    "snap_op_radar_vegetation_index",
    parent = snap_operator
  )
  snap_op_radar_vegetation_index(
    operator = "Radar-Vegetation-Index",
    operator_id = operator_id,
    operator_sources = operator_sources,
    created_with = rlang::current_fn(),
    xml_graph = as.character(op_xml)
  )
}
