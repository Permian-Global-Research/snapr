#' Calibration: snap operator function
#' @param operator_id character operator id
#' @param sourceProduct The source product(s) as input to the operator
#' @param sourceBands The list of source bands.
#' @param auxFile The auxiliary file Value must be one of 'Latest Auxiliary
#' File', 'Product Auxiliary File', 'External Auxiliary File'. Default value is
#' 'Latest Auxiliary File'.
#' @param externalAuxFile The antenna elevation pattern gain auxiliary data
#' file.
#' @param outputImageInComplex Output image in complex Default value is
#' 'false'.
#' @param outputImageScaleInDb Output image scale Default value is 'false'.
#' @param createGammaBand Create gamma0 virtual band Default value is 'false'.
#' @param createBetaBand Create beta0 virtual band Default value is 'false'.
#' @param selectedPolarisations The list of polarisations
#' @param outputSigmaBand Output sigma0 band Default value is 'true'.
#' @param outputGammaBand Output gamma0 band Default value is 'false'.
#' @param outputBetaBand Output beta0 band Default value is 'false'.
#' @details
#' Descrscription from '`gpt {operator} -h`':
#'
#' "Calibration of products"
#' @import xml2
#' @return snap_op_calibration object
#' @export
op_calibration <- function(
    operator_id,
    sourceProduct,
    sourceBands = NULL,
    auxFile = "Latest Auxiliary File",
    externalAuxFile = NULL,
    outputImageInComplex = FALSE,
    outputImageScaleInDb = FALSE,
    createGammaBand = FALSE,
    createBetaBand = FALSE,
    selectedPolarisations = NULL,
    outputSigmaBand = TRUE,
    outputGammaBand = FALSE,
    outputBetaBand = FALSE) {
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
  xml_add_child(node, "operator", "Calibration")
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
    "auxFile",
    gpt_args$auxFile
  )
  xml_add_child(
    parameters,
    "externalAuxFile",
    gpt_args$externalAuxFile
  )
  xml_add_child(
    parameters,
    "outputImageInComplex",
    gpt_args$outputImageInComplex
  )
  xml_add_child(
    parameters,
    "outputImageScaleInDb",
    gpt_args$outputImageScaleInDb
  )
  xml_add_child(
    parameters,
    "createGammaBand",
    gpt_args$createGammaBand
  )
  xml_add_child(
    parameters,
    "createBetaBand",
    gpt_args$createBetaBand
  )
  xml_add_child(
    parameters,
    "selectedPolarisations",
    gpt_args$selectedPolarisations
  )
  xml_add_child(
    parameters,
    "outputSigmaBand",
    gpt_args$outputSigmaBand
  )
  xml_add_child(
    parameters,
    "outputGammaBand",
    gpt_args$outputGammaBand
  )
  xml_add_child(
    parameters,
    "outputBetaBand",
    gpt_args$outputBetaBand
  )
  operator_sources <- gpt_args[c("sourceProduct")]
  snap_op_calibration <- S7::new_class(
    "snap_op_calibration",
    parent = snap_operator
  )
  snap_op_calibration(
    operator = "Calibration",
    operator_id = operator_id,
    operator_sources = operator_sources,
    created_with = rlang::current_fn(),
    xml_graph = as.character(op_xml)
  )
}
