#' Convert-Datatype: snap operator function
#' @param operator_id character operator id
#' @param sourceProduct The source product(s) as input to the operator
#' @param sourceBands The list of source bands.
#' @param targetDataType Sets parameter 'targetDataType' to <string>. Value
#' must be one of 'int8', 'int16', 'int32', 'uint8', 'uint16', 'uint32',
#' 'float32', 'float64'. Default value is 'uint8'.
#' @param targetScalingStr Sets parameter 'targetScalingStr' to <string>. Value
#' must be one of 'Truncate', 'Linear (slope and intercept)', 'Linear (between 95%
#' clipped histogram)', 'Linear (peak clipped histogram)', 'Logarithmic'. Default
#' value is 'Linear (between 95% clipped histogram)'.
#' @param targetNoDataValue Sets parameter 'targetNoDataValue' to <double>.
#' Default value is '0'.
#' @details
#' Descrscription from '`gpt {operator} -h`':
#'
#' "Convert product data type"
#' @import xml2
#' @return snap_op_convert_datatype object
#' @export
op_convert_datatype <- function(
    operator_id,
    sourceProduct,
    sourceBands = NULL,
    targetDataType = "uint8",
    targetScalingStr = "Linear (between 95% clipped histogram)",
    targetNoDataValue = 0) {
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
  xml_add_child(node, "operator", "Convert-Datatype")
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
    "sourceBands",
    gpt_args$sourceBands
  )
  xml_add_child(
    parameters,
    "targetDataType",
    gpt_args$targetDataType
  )
  xml_add_child(
    parameters,
    "targetScalingStr",
    gpt_args$targetScalingStr
  )
  xml_add_child(
    parameters,
    "targetNoDataValue",
    gpt_args$targetNoDataValue
  )
  operator_sources <- gpt_args[c("sourceProduct")]
  snap_op_convert_datatype <- S7::new_class(
    "snap_op_convert_datatype",
    parent = snap_operator
  )
  snap_op_convert_datatype(
    operator = "Convert-Datatype",
    operator_id = operator_id,
    operator_sources = operator_sources,
    created_with = rlang::current_fn(),
    xml_graph = as.character(op_xml)
  )
}
