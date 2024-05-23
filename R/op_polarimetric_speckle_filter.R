#' Polarimetric-Speckle-Filter: snap operator function
#' @param operator_id character operator id
#' @param sourceProduct The source product(s) as input to the operator
#' @param filter Sets parameter 'filter' to <string>. Value must be one of 'Box
#' Car Filter', 'IDAN Filter', 'Refined Lee Filter', 'Improved Lee Sigma Filter'.
#' Default value is 'Refined Lee Filter'.
#' @param filterSize The filter size Valid interval is (1, 100\]. Default value
#' is '5'.
#' @param numLooksStr Sets parameter 'numLooksStr' to <string>. Value must be
#' one of '1', '2', '3', '4'. Default value is '1'.
#' @param windowSize Sets parameter 'windowSize' to <string>. Value must be one
#' of '5x5', '7x7', '9x9', '11x11', '13x13', '15x15', '17x17'. Default value is
#' '7x7'.
#' @param targetWindowSizeStr Sets parameter 'targetWindowSizeStr' to <string>.
#' Value must be one of '3x3', '5x5'. Default value is '3x3'.
#' @param anSize The Adaptive Neighbourhood size Valid interval is (1, 200\].
#' Default value is '50'.
#' @param sigmaStr Sets parameter 'sigmaStr' to <string>. Value must be one of
#' '0.5', '0.6', '0.7', '0.8', '0.9'. Default value is '0.9'.
#' @param searchWindowSizeStr The search window size Value must be one of '3',
#' '5', '7', '9', '11', '13', '15', '17', '19', '21', '23', '25'. Default value is
#' '15'.
#' @param patchSizeStr The patch size Value must be one of '3', '5', '7', '9',
#' '11'. Default value is '5'.
#' @param scaleSizeStr The scale size Value must be one of '0', '1', '2'.
#' Default value is '1'.
#' @details
#' Descrscription from '`gpt {operator} -h`':
#'
#' "Polarimetric Speckle Reduction"
#' @import xml2
#' @return snap_op_polarimetric_speckle_filter object
#' @export
op_polarimetric_speckle_filter <- function(
    operator_id,
    sourceProduct,
    filter = "Refined Lee Filter",
    filterSize = 5,
    numLooksStr = 1,
    windowSize = "7x7",
    targetWindowSizeStr = "3x3",
    anSize = 50,
    sigmaStr = 0.9,
    searchWindowSizeStr = 15,
    patchSizeStr = 5,
    scaleSizeStr = 1) {
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
  xml_add_child(node, "operator", "Polarimetric-Speckle-Filter")
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
    "filter",
    gpt_args$filter
  )
  xml_add_child(
    parameters,
    "filterSize",
    gpt_args$filterSize
  )
  xml_add_child(
    parameters,
    "numLooksStr",
    gpt_args$numLooksStr
  )
  xml_add_child(
    parameters,
    "windowSize",
    gpt_args$windowSize
  )
  xml_add_child(
    parameters,
    "targetWindowSizeStr",
    gpt_args$targetWindowSizeStr
  )
  xml_add_child(
    parameters,
    "anSize",
    gpt_args$anSize
  )
  xml_add_child(
    parameters,
    "sigmaStr",
    gpt_args$sigmaStr
  )
  xml_add_child(
    parameters,
    "searchWindowSizeStr",
    gpt_args$searchWindowSizeStr
  )
  xml_add_child(
    parameters,
    "patchSizeStr",
    gpt_args$patchSizeStr
  )
  xml_add_child(
    parameters,
    "scaleSizeStr",
    gpt_args$scaleSizeStr
  )
  operator_sources <- gpt_args[c("sourceProduct")]
  snap_op_polarimetric_speckle_filter <- S7::new_class(
    "snap_op_polarimetric_speckle_filter",
    parent = snap_operator
  )
  snap_op_polarimetric_speckle_filter(
    operator = "Polarimetric-Speckle-Filter",
    operator_id = operator_id,
    operator_sources = operator_sources,
    created_with = rlang::current_fn(),
    xml_graph = as.character(op_xml)
  )
}
