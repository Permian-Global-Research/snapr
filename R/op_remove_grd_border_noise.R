#' Remove-GRD-Border-Noise: snap operator function
#' @param operator_id character operator id
#' @param sourceProduct The source product(s) as input to the operator
#' @param selectedPolarisations The list of polarisations
#' @param borderLimit The border margin limit Default value is '500'.
#' @param trimThreshold The trim threshold Default value is '0.5'.
#' @details
#' Descrscription from '`gpt {operator} -h`':
#'
#' "Mask no-value pixels for GRD product"
#' @import xml2
#' @return snap_op_remove_grd_border_noise object
#' @export
op_remove_grd_border_noise <- function(
    operator_id,
    sourceProduct,
    selectedPolarisations = NULL,
    borderLimit = 500,
    trimThreshold = 0.5) {
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
  xml_add_child(node, "operator", "Remove-GRD-Border-Noise")
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
    "selectedPolarisations",
    gpt_args$selectedPolarisations
  )
  xml_add_child(
    parameters,
    "borderLimit",
    gpt_args$borderLimit
  )
  xml_add_child(
    parameters,
    "trimThreshold",
    gpt_args$trimThreshold
  )
  operator_sources <- gpt_args[c("sourceProduct")]
  snap_op_remove_grd_border_noise <- S7::new_class(
    "snap_op_remove_grd_border_noise",
    parent = snap_operator
  )
  snap_op_remove_grd_border_noise(
    operator = "Remove-GRD-Border-Noise",
    operator_id = operator_id,
    operator_sources = operator_sources,
    created_with = rlang::current_fn(),
    xml_graph = as.character(op_xml)
  )
}
