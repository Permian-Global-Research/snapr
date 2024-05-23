#' ThermalNoiseRemoval: snap operator function
#' @param operator_id character operator id
#' @param sourceProduct The source product(s) as input to the operator
#' @param selectedPolarisations The list of polarisations
#' @param removeThermalNoise Remove thermal noise Default value is 'true'.
#' @param outputNoise Output noise Default value is 'false'.
#' @param reIntroduceThermalNoise Re-introduce thermal noise Default value is
#' 'false'.
#' @details
#' Descrscription from '`gpt {operator} -h`':
#'
#' "Removes thermal noise from products"
#' @import xml2
#' @return snap_op_thermal_noise_removal object
#' @export
op_thermal_noise_removal <- function(
    operator_id,
    sourceProduct,
    selectedPolarisations = NULL,
    removeThermalNoise = TRUE,
    outputNoise = FALSE,
    reIntroduceThermalNoise = FALSE) {
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
  xml_add_child(node, "operator", "ThermalNoiseRemoval")
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
    "removeThermalNoise",
    gpt_args$removeThermalNoise
  )
  xml_add_child(
    parameters,
    "outputNoise",
    gpt_args$outputNoise
  )
  xml_add_child(
    parameters,
    "reIntroduceThermalNoise",
    gpt_args$reIntroduceThermalNoise
  )
  operator_sources <- gpt_args[c("sourceProduct")]
  snap_op_thermal_noise_removal <- S7::new_class(
    "snap_op_thermal_noise_removal",
    parent = snap_operator
  )
  snap_op_thermal_noise_removal(
    operator = "ThermalNoiseRemoval",
    operator_id = operator_id,
    operator_sources = operator_sources,
    created_with = rlang::current_fn(),
    xml_graph = as.character(op_xml)
  )
}
