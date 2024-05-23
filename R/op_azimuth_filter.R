#' AzimuthFilter: snap operator function
#' @param operator_id character operator id
#' @param sourceProduct The source product(s) as input to the operator
#' @param fftLength Length of filtering window Value must be one of '64',
#' '128', '256', '512', '1024', '2048'. Default value is '256'.
#' @param aziFilterOverlap Overlap between filtering windows in azimuth
#' direction \[lines\] Value must be one of '0', '8', '16', '32', '64', '128',
#' '256'. Default value is '0'.
#' @param alphaHamming Weight for Hamming filter (1 is rectangular window)
#' Value must be one of '0.5', '0.75', '0.8', '0.9', '1'. Default value is '0.75'.
#' @details
#' Descrscription from '`gpt {operator} -h`':
#'
#' "Azimuth Filter"
#' @import xml2
#' @return snap_op_azimuth_filter object
#' @export
op_azimuth_filter <- function(
    operator_id,
    sourceProduct,
    fftLength = 256,
    aziFilterOverlap = 0,
    alphaHamming = 0.75) {
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
  xml_add_child(node, "operator", "AzimuthFilter")
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
    "fftLength",
    gpt_args$fftLength
  )
  xml_add_child(
    parameters,
    "aziFilterOverlap",
    gpt_args$aziFilterOverlap
  )
  xml_add_child(
    parameters,
    "alphaHamming",
    gpt_args$alphaHamming
  )
  operator_sources <- gpt_args[c("sourceProduct")]
  snap_op_azimuth_filter <- S7::new_class(
    "snap_op_azimuth_filter",
    parent = snap_operator
  )
  snap_op_azimuth_filter(
    operator = "AzimuthFilter",
    operator_id = operator_id,
    operator_sources = operator_sources,
    created_with = rlang::current_fn(),
    xml_graph = as.character(op_xml)
  )
}
