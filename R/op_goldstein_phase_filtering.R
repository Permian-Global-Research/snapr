#' GoldsteinPhaseFiltering: snap operator function
#' @param operator_id character operator id
#' @param sourceProduct The source product(s) as input to the operator
#' @param alpha adaptive filter exponent Valid interval is (0, 1\]. Default
#' value is '1.0'.
#' @param FFTSizeString Sets parameter 'FFTSizeString' to <string>. Value must
#' be one of '32', '64', '128', '256'. Default value is '64'.
#' @param windowSizeString Sets parameter 'windowSizeString' to <string>. Value
#' must be one of '3', '5', '7'. Default value is '3'.
#' @param useCoherenceMask Use coherence mask Default value is 'false'.
#' @param coherenceThreshold The coherence threshold Valid interval is \[0, 1\].
#' Default value is '0.2'.
#' @details
#' Descrscription from '`gpt {operator} -h`':
#'
#' "Phase Filtering"
#' @import xml2
#' @return snap_op_goldstein_phase_filtering object
#' @export
op_goldstein_phase_filtering <- function(
    operator_id,
    sourceProduct,
    alpha = 1.0,
    FFTSizeString = 64,
    windowSizeString = 3,
    useCoherenceMask = FALSE,
    coherenceThreshold = 0.2) {
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
  xml_add_child(node, "operator", "GoldsteinPhaseFiltering")
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
    "alpha",
    gpt_args$alpha
  )
  xml_add_child(
    parameters,
    "FFTSizeString",
    gpt_args$FFTSizeString
  )
  xml_add_child(
    parameters,
    "windowSizeString",
    gpt_args$windowSizeString
  )
  xml_add_child(
    parameters,
    "useCoherenceMask",
    gpt_args$useCoherenceMask
  )
  xml_add_child(
    parameters,
    "coherenceThreshold",
    gpt_args$coherenceThreshold
  )
  operator_sources <- gpt_args[c("sourceProduct")]
  snap_op_goldstein_phase_filtering <- S7::new_class(
    "snap_op_goldstein_phase_filtering",
    parent = snap_operator
  )
  snap_op_goldstein_phase_filtering(
    operator = "GoldsteinPhaseFiltering",
    operator_id = operator_id,
    operator_sources = operator_sources,
    created_with = rlang::current_fn(),
    xml_graph = as.character(op_xml)
  )
}
