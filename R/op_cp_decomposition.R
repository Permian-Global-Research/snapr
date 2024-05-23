#' CP-Decomposition: snap operator function
#' @param operator_id character operator id
#' @param sourceProduct The source product(s) as input to the operator
#' @param decomposition Sets parameter 'decomposition' to <string>. Value must
#' be one of 'M-Chi Decomposition', 'M-Delta Decomposition', 'H-Alpha
#' Decomposition', '2 Layer RVOG Model Based Decomposition', 'Model-free
#' 3-component decomposition'. Default value is 'M-Chi Decomposition'.
#' @param windowSizeXStr Sets parameter 'windowSizeXStr' to <string>. Value
#' must be one of '3', '5', '7', '9', '11', '13', '15', '17', '19'. Default value
#' is '5'.
#' @param windowSizeYStr Sets parameter 'windowSizeYStr' to <string>. Value
#' must be one of '3', '5', '7', '9', '11', '13', '15', '17', '19'. Default value
#' is '5'.
#' @param computeAlphaByT3 Compute alpha by coherency matrix T3 Default value
#' is 'true'.
#' @param outputRVOG Output RVOG parameters mv, ms, alphaS and phi Default
#' value is 'true'.
#' @details
#' Descrscription from '`gpt {operator} -h`':
#'
#' "Perform Compact Polarimetric decomposition of a given product"
#' @import xml2
#' @return snap_op_cp_decomposition object
#' @export
op_cp_decomposition <- function(
    operator_id,
    sourceProduct,
    decomposition = "M-Chi Decomposition",
    windowSizeXStr = 5,
    windowSizeYStr = 5,
    computeAlphaByT3 = TRUE,
    outputRVOG = TRUE) {
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
  xml_add_child(node, "operator", "CP-Decomposition")
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
    "decomposition",
    gpt_args$decomposition
  )
  xml_add_child(
    parameters,
    "windowSizeXStr",
    gpt_args$windowSizeXStr
  )
  xml_add_child(
    parameters,
    "windowSizeYStr",
    gpt_args$windowSizeYStr
  )
  xml_add_child(
    parameters,
    "computeAlphaByT3",
    gpt_args$computeAlphaByT3
  )
  xml_add_child(
    parameters,
    "outputRVOG",
    gpt_args$outputRVOG
  )
  operator_sources <- gpt_args[c("sourceProduct")]
  snap_op_cp_decomposition <- S7::new_class(
    "snap_op_cp_decomposition",
    parent = snap_operator
  )
  snap_op_cp_decomposition(
    operator = "CP-Decomposition",
    operator_id = operator_id,
    operator_sources = operator_sources,
    created_with = rlang::current_fn(),
    xml_graph = as.character(op_xml)
  )
}
