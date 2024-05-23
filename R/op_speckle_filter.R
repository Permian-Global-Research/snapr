#' Speckle-Filter: snap operator function
#' @param operator_id character operator id
#' @param sourceProduct The source product(s) as input to the operator
#' @param sourceBands The list of source bands.
#' @param filter Sets parameter 'filter' to <string>. Value must be one of
#' 'None', 'Boxcar', 'Median', 'Frost', 'Gamma Map', 'Lee', 'Refined Lee', 'Lee
#' Sigma', 'IDAN'. Default value is 'Lee Sigma'.
#' @param filterSizeX The kernel x dimension Valid interval is (1, 100\].
#' Default value is '3'.
#' @param filterSizeY The kernel y dimension Valid interval is (1, 100\].
#' Default value is '3'.
#' @param dampingFactor The damping factor (Frost filter only) Valid interval
#' is (0, 100\]. Default value is '2'.
#' @param estimateENL Sets parameter 'estimateENL' to <boolean>. Default value
#' is 'false'.
#' @param enl The number of looks Valid interval is (0, *). Default value is
#' '1.0'.
#' @param numLooksStr Sets parameter 'numLooksStr' to <string>. Value must be
#' one of '1', '2', '3', '4'. Default value is '1'.
#' @param windowSize Sets parameter 'windowSize' to <string>. Value must be one
#' of '5x5', '7x7', '9x9', '11x11', '13x13', '15x15', '17x17'. Default value is
#' '7x7'.
#' @param targetWindowSizeStr Sets parameter 'targetWindowSizeStr' to <string>.
#' Value must be one of '3x3', '5x5'. Default value is '3x3'.
#' @param sigmaStr Sets parameter 'sigmaStr' to <string>. Value must be one of
#' '0.5', '0.6', '0.7', '0.8', '0.9'. Default value is '0.9'.
#' @param anSize The Adaptive Neighbourhood size Valid interval is (1, 200\].
#' Default value is '50'.
#' @details
#' Descrscription from '`gpt {operator} -h`':
#'
#' "Speckle Reduction"
#' @import xml2
#' @return snap_op_speckle_filter object
#' @export
op_speckle_filter <- function(
    operator_id,
    sourceProduct,
    sourceBands = NULL,
    filter = "Lee Sigma",
    filterSizeX = 3,
    filterSizeY = 3,
    dampingFactor = 2,
    estimateENL = FALSE,
    enl = 1.0,
    numLooksStr = 1,
    windowSize = "7x7",
    targetWindowSizeStr = "3x3",
    sigmaStr = 0.9,
    anSize = 50) {
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
  xml_add_child(node, "operator", "Speckle-Filter")
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
    "filter",
    gpt_args$filter
  )
  xml_add_child(
    parameters,
    "filterSizeX",
    gpt_args$filterSizeX
  )
  xml_add_child(
    parameters,
    "filterSizeY",
    gpt_args$filterSizeY
  )
  xml_add_child(
    parameters,
    "dampingFactor",
    gpt_args$dampingFactor
  )
  xml_add_child(
    parameters,
    "estimateENL",
    gpt_args$estimateENL
  )
  xml_add_child(
    parameters,
    "enl",
    gpt_args$enl
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
    "sigmaStr",
    gpt_args$sigmaStr
  )
  xml_add_child(
    parameters,
    "anSize",
    gpt_args$anSize
  )
  operator_sources <- gpt_args[c("sourceProduct")]
  snap_op_speckle_filter <- S7::new_class(
    "snap_op_speckle_filter",
    parent = snap_operator
  )
  snap_op_speckle_filter(
    operator = "Speckle-Filter",
    operator_id = operator_id,
    operator_sources = operator_sources,
    created_with = rlang::current_fn(),
    xml_graph = as.character(op_xml)
  )
}
