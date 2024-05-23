#' BandPassFilter: snap operator function
#' @param operator_id character operator id
#' @param sourceProduct The source product(s) as input to the operator
#' @param subband Sets parameter 'subband' to <string>. Value must be one of
#' 'low', 'high'. Default value is 'low'.
#' @param alpha Hamming alpha Value must be one of '0.5', '0.75', '0.8', '0.9',
#' '1'. Default value is '1'.
#' @details
#' Descrscription from '`gpt {operator} -h`':
#'
#' "Creates a basebanded SLC based on a subband of 1/3 the original bandwidth"
#' @import xml2
#' @return snap_op_band_pass_filter object
#' @export
op_band_pass_filter <- function(
    operator_id,
    sourceProduct,
    subband = "low",
    alpha = 1) {
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
  xml_add_child(node, "operator", "BandPassFilter")
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
    "subband",
    gpt_args$subband
  )
  xml_add_child(
    parameters,
    "alpha",
    gpt_args$alpha
  )
  operator_sources <- gpt_args[c("sourceProduct")]
  snap_op_band_pass_filter <- S7::new_class(
    "snap_op_band_pass_filter",
    parent = snap_operator
  )
  snap_op_band_pass_filter(
    operator = "BandPassFilter",
    operator_id = operator_id,
    operator_sources = operator_sources,
    created_with = rlang::current_fn(),
    xml_graph = as.character(op_xml)
  )
}
