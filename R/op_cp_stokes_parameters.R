#' CP-Stokes-Parameters: snap operator function
#' @param operator_id character operator id
#' @param sourceProduct The source product(s) as input to the operator
#' @param windowSizeXStr Sets parameter 'windowSizeXStr' to <string>. Value
#' must be one of '3', '5', '7', '9', '11', '13', '15', '17', '19'. Default value
#' is '5'.
#' @param windowSizeYStr Sets parameter 'windowSizeYStr' to <string>. Value
#' must be one of '3', '5', '7', '9', '11', '13', '15', '17', '19'. Default value
#' is '5'.
#' @param outputStokesVector Output Stokes vector Default value is 'false'.
#' @param outputDegreeOfPolarization Output degree of polarization Default
#' value is 'true'.
#' @param outputDegreeOfDepolarization Output degree of depolarization Default
#' value is 'true'.
#' @param outputDegreeOfCircularity Output degree of circularity Default value
#' is 'true'.
#' @param outputDegreeOfEllipticity Output degree of ellipticity Default value
#' is 'true'.
#' @param outputCPR Output circular polarization ratio Default value is 'true'.
#' @param outputLPR Output linear polarization ratio Default value is 'true'.
#' @param outputRelativePhase Output relative phase Default value is 'true'.
#' @param outputAlphas Output alphas Default value is 'true'.
#' @param outputConformity Output conformity coefficient Default value is
#' 'true'.
#' @param outputPhasePhi Output phase phi Default value is 'true'.
#' @details
#' Descrscription from '`gpt {operator} -h`':
#'
#' "Generates compact polarimetric Stokes child parameters"
#' @import xml2
#' @return snap_op_cp_stokes_parameters object
#' @export
op_cp_stokes_parameters <- function(
    operator_id,
    sourceProduct,
    windowSizeXStr = 5,
    windowSizeYStr = 5,
    outputStokesVector = FALSE,
    outputDegreeOfPolarization = TRUE,
    outputDegreeOfDepolarization = TRUE,
    outputDegreeOfCircularity = TRUE,
    outputDegreeOfEllipticity = TRUE,
    outputCPR = TRUE,
    outputLPR = TRUE,
    outputRelativePhase = TRUE,
    outputAlphas = TRUE,
    outputConformity = TRUE,
    outputPhasePhi = TRUE) {
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
  xml_add_child(node, "operator", "CP-Stokes-Parameters")
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
    "outputStokesVector",
    gpt_args$outputStokesVector
  )
  xml_add_child(
    parameters,
    "outputDegreeOfPolarization",
    gpt_args$outputDegreeOfPolarization
  )
  xml_add_child(
    parameters,
    "outputDegreeOfDepolarization",
    gpt_args$outputDegreeOfDepolarization
  )
  xml_add_child(
    parameters,
    "outputDegreeOfCircularity",
    gpt_args$outputDegreeOfCircularity
  )
  xml_add_child(
    parameters,
    "outputDegreeOfEllipticity",
    gpt_args$outputDegreeOfEllipticity
  )
  xml_add_child(
    parameters,
    "outputCPR",
    gpt_args$outputCPR
  )
  xml_add_child(
    parameters,
    "outputLPR",
    gpt_args$outputLPR
  )
  xml_add_child(
    parameters,
    "outputRelativePhase",
    gpt_args$outputRelativePhase
  )
  xml_add_child(
    parameters,
    "outputAlphas",
    gpt_args$outputAlphas
  )
  xml_add_child(
    parameters,
    "outputConformity",
    gpt_args$outputConformity
  )
  xml_add_child(
    parameters,
    "outputPhasePhi",
    gpt_args$outputPhasePhi
  )
  operator_sources <- gpt_args[c("sourceProduct")]
  snap_op_cp_stokes_parameters <- S7::new_class(
    "snap_op_cp_stokes_parameters",
    parent = snap_operator
  )
  snap_op_cp_stokes_parameters(
    operator = "CP-Stokes-Parameters",
    operator_id = operator_id,
    operator_sources = operator_sources,
    created_with = rlang::current_fn(),
    xml_graph = as.character(op_xml)
  )
}
