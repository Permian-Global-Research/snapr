#' IonosphericCorrection: snap operator function
#' @param operator_id character operator id
#' @param sourceProduct The source product(s) as input to the operator
#' @param sigma Standard deviation for Gaussian filter Default value is '81'.
#' @param coherenceThreshold Coherence threshold Valid interval is \[0, 1\].
#' Default value is '0.6'.
#' @param minCoherence Minimum coherence for output mask Valid interval is \[0,
#' 1\]. Default value is '0.2'.
#' @details
#' Descrscription from '`gpt {operator} -h`':
#'
#' "Estimation of Ionospheric Phase Screens Parameter Options:
#' -PcoherenceThreshold=<double> Coherence threshold Valid interval is \[0, 1\].
#' Default value is '0.6'. -PminCoherence=<double> Minimum coherence for output
#' mask Valid interval is \[0, 1\]. Default value is '0.2'. -Psigma=<int> Standard
#' deviation for Gaussian filter Default value is '81'."
#' @import xml2
#' @return snap_op_ionospheric_correction object
#' @export
op_ionospheric_correction <- function(
    operator_id,
    sourceProduct,
    sigma = 81,
    coherenceThreshold = 0.6,
    minCoherence = 0.2) {
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
  xml_add_child(node, "operator", "IonosphericCorrection")
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
    "sigma",
    gpt_args$sigma
  )
  xml_add_child(
    parameters,
    "coherenceThreshold",
    gpt_args$coherenceThreshold
  )
  xml_add_child(
    parameters,
    "minCoherence",
    gpt_args$minCoherence
  )
  operator_sources <- gpt_args[c("sourceProduct")]
  snap_op_ionospheric_correction <- S7::new_class(
    "snap_op_ionospheric_correction",
    parent = snap_operator
  )
  snap_op_ionospheric_correction(
    operator = "IonosphericCorrection",
    operator_id = operator_id,
    operator_sources = operator_sources,
    created_with = rlang::current_fn(),
    xml_graph = as.character(op_xml)
  )
}
