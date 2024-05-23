#' DeburstWSS: snap operator function
#' @param operator_id character operator id
#' @param sourceProduct The source product(s) as input to the operator
#' @param subSwath Sets parameter 'subSwath' to <string>. Value must be one of
#' 'SS1', 'SS2', 'SS3', 'SS4', 'SS5'. Default value is 'SS1'.
#' @param produceIntensitiesOnly Sets parameter 'produceIntensitiesOnly' to
#' <boolean>. Default value is 'false'.
#' @param average Sets parameter 'average' to <boolean>. Default value is
#' 'false'.
#' @details
#' Descrscription from '`gpt {operator} -h`':
#'
#' "Debursts an ASAR WSS product"
#' @import xml2
#' @return snap_op_deburst_wss object
#' @export
op_deburst_wss <- function(
    operator_id,
    sourceProduct,
    subSwath = "SS1",
    produceIntensitiesOnly = FALSE,
    average = FALSE) {
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
  xml_add_child(node, "operator", "DeburstWSS")
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
    "subSwath",
    gpt_args$subSwath
  )
  xml_add_child(
    parameters,
    "produceIntensitiesOnly",
    gpt_args$produceIntensitiesOnly
  )
  xml_add_child(
    parameters,
    "average",
    gpt_args$average
  )
  operator_sources <- gpt_args[c("sourceProduct")]
  snap_op_deburst_wss <- S7::new_class(
    "snap_op_deburst_wss",
    parent = snap_operator
  )
  snap_op_deburst_wss(
    operator = "DeburstWSS",
    operator_id = operator_id,
    operator_sources = operator_sources,
    created_with = rlang::current_fn(),
    xml_graph = as.character(op_xml)
  )
}
