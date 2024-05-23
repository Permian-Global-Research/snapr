#' Polarimetric-Decomposition: snap operator function
#' @param operator_id character operator id
#' @param sourceProduct The source product(s) as input to the operator
#' @param decomposition Sets parameter 'decomposition' to <string>. Value must
#' be one of 'Sinclair Decomposition', 'Pauli Decomposition', 'Freeman-Durden
#' Decomposition', 'Generalized Freeman-Durden Decomposition', 'Yamaguchi
#' Decomposition', 'van Zyl Decomposition', 'H-A-Alpha Quad Pol Decomposition',
#' 'H-Alpha Dual Pol Decomposition', 'Cloude Decomposition', 'Touzi
#' Decomposition', 'Huynen Decomposition', 'Yang Decomposition', 'Krogager
#' Decomposition', 'Cameron Decomposition', 'Model-free 3-component
#' Decomposition', 'Model-free 4-component Decomposition'. Default value is
#' 'Sinclair Decomposition'.
#' @param windowSize The sliding window size Valid interval is \[1, 100\].
#' Default value is '5'.
#' @param outputHAAlpha Output entropy, anisotropy, alpha Default value is
#' 'false'.
#' @param outputBetaDeltaGammaLambda Output beta, delta, gamma, lambda Default
#' value is 'false'.
#' @param outputAlpha123 Output alpha 1, 2, 3 Default value is 'false'.
#' @param outputLambda123 Output lambda 1, 2, 3 Default value is 'false'.
#' @param outputTouziParamSet0 Output psi, tau, alpha, phi Default value is
#' 'false'.
#' @param outputTouziParamSet1 Output psi1, tau1, alpha1, phi1 Default value is
#' 'false'.
#' @param outputTouziParamSet2 Output psi2, tau2, alpha2, phi2 Default value is
#' 'false'.
#' @param outputTouziParamSet3 Output psi3, tau3, alpha3, phi3 Default value is
#' 'false'.
#' @param outputHuynenParamSet0 Output 2A0_b, B0_plus_B, B0_minus_B Default
#' value is 'true'.
#' @param outputHuynenParamSet1 Output A0, B0, B, C, D, E, F, G, H Default
#' value is 'false'.
#' @details
#' Descrscription from '`gpt {operator} -h`':
#'
#' "Perform Polarimetric decomposition of a given product"
#' @import xml2
#' @return snap_op_polarimetric_decomposition object
#' @export
op_polarimetric_decomposition <- function(
    operator_id,
    sourceProduct,
    decomposition = "Sinclair Decomposition",
    windowSize = 5,
    outputHAAlpha = FALSE,
    outputBetaDeltaGammaLambda = FALSE,
    outputAlpha123 = FALSE,
    outputLambda123 = FALSE,
    outputTouziParamSet0 = FALSE,
    outputTouziParamSet1 = FALSE,
    outputTouziParamSet2 = FALSE,
    outputTouziParamSet3 = FALSE,
    outputHuynenParamSet0 = TRUE,
    outputHuynenParamSet1 = FALSE) {
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
  xml_add_child(node, "operator", "Polarimetric-Decomposition")
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
    "windowSize",
    gpt_args$windowSize
  )
  xml_add_child(
    parameters,
    "outputHAAlpha",
    gpt_args$outputHAAlpha
  )
  xml_add_child(
    parameters,
    "outputBetaDeltaGammaLambda",
    gpt_args$outputBetaDeltaGammaLambda
  )
  xml_add_child(
    parameters,
    "outputAlpha123",
    gpt_args$outputAlpha123
  )
  xml_add_child(
    parameters,
    "outputLambda123",
    gpt_args$outputLambda123
  )
  xml_add_child(
    parameters,
    "outputTouziParamSet0",
    gpt_args$outputTouziParamSet0
  )
  xml_add_child(
    parameters,
    "outputTouziParamSet1",
    gpt_args$outputTouziParamSet1
  )
  xml_add_child(
    parameters,
    "outputTouziParamSet2",
    gpt_args$outputTouziParamSet2
  )
  xml_add_child(
    parameters,
    "outputTouziParamSet3",
    gpt_args$outputTouziParamSet3
  )
  xml_add_child(
    parameters,
    "outputHuynenParamSet0",
    gpt_args$outputHuynenParamSet0
  )
  xml_add_child(
    parameters,
    "outputHuynenParamSet1",
    gpt_args$outputHuynenParamSet1
  )
  operator_sources <- gpt_args[c("sourceProduct")]
  snap_op_polarimetric_decomposition <- S7::new_class(
    "snap_op_polarimetric_decomposition",
    parent = snap_operator
  )
  snap_op_polarimetric_decomposition(
    operator = "Polarimetric-Decomposition",
    operator_id = operator_id,
    operator_sources = operator_sources,
    created_with = rlang::current_fn(),
    xml_graph = as.character(op_xml)
  )
}
