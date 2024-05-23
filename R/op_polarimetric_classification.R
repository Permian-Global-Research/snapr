#' Polarimetric-Classification: snap operator function
#' @param operator_id character operator id
#' @param sourceProduct The source product(s) as input to the operator
#' @param classification Sets parameter 'classification' to <string>. Value
#' must be one of 'Cloude-Pottier', 'Cloude-Pottier Dual Pol', 'H Alpha Wishart',
#' 'H Alpha Wishart Dual Pol', 'Freeman-Durden Wishart', 'General Wishart'.
#' Default value is 'H Alpha Wishart'.
#' @param windowSize The sliding window size Valid interval is (1, 100\].
#' Default value is '5'.
#' @param maxIterations The maximum number of iterations Valid interval is \[1,
#' 100\]. Default value is '3'.
#' @param numInitialClasses The initial number of classes Valid interval is \[9,
#' 1000\]. Default value is '90'.
#' @param numFinalClasses The desired number of classes Valid interval is \[9,
#' 100\]. Default value is '15'.
#' @param mixedCategoryThreshold The threshold for classifying pixels to mixed
#' category Valid interval is (0, *). Default value is '0.5'.
#' @param decomposition Sets parameter 'decomposition' to <string>. Value must
#' be one of 'Sinclair Decomposition', 'Pauli Decomposition', 'Freeman-Durden
#' Decomposition', 'Generalized Freeman-Durden Decomposition', 'Yamaguchi
#' Decomposition', 'van Zyl Decomposition', 'H-A-Alpha Quad Pol Decomposition',
#' 'Cloude Decomposition', 'Touzi Decomposition'. Default value is 'Sinclair
#' Decomposition'.
#' @details
#' Descrscription from '`gpt {operator} -h`':
#'
#' "Perform Polarimetric classification of a given product"
#' @import xml2
#' @return snap_op_polarimetric_classification object
#' @export
op_polarimetric_classification <- function(
    operator_id,
    sourceProduct,
    classification = "H Alpha Wishart",
    windowSize = 5,
    maxIterations = 3,
    numInitialClasses = 90,
    numFinalClasses = 15,
    mixedCategoryThreshold = 0.5,
    decomposition = "Sinclair Decomposition") {
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
  xml_add_child(node, "operator", "Polarimetric-Classification")
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
    "classification",
    gpt_args$classification
  )
  xml_add_child(
    parameters,
    "windowSize",
    gpt_args$windowSize
  )
  xml_add_child(
    parameters,
    "maxIterations",
    gpt_args$maxIterations
  )
  xml_add_child(
    parameters,
    "numInitialClasses",
    gpt_args$numInitialClasses
  )
  xml_add_child(
    parameters,
    "numFinalClasses",
    gpt_args$numFinalClasses
  )
  xml_add_child(
    parameters,
    "mixedCategoryThreshold",
    gpt_args$mixedCategoryThreshold
  )
  xml_add_child(
    parameters,
    "decomposition",
    gpt_args$decomposition
  )
  operator_sources <- gpt_args[c("sourceProduct")]
  snap_op_polarimetric_classification <- S7::new_class(
    "snap_op_polarimetric_classification",
    parent = snap_operator
  )
  snap_op_polarimetric_classification(
    operator = "Polarimetric-Classification",
    operator_id = operator_id,
    operator_sources = operator_sources,
    created_with = rlang::current_fn(),
    xml_graph = as.character(op_xml)
  )
}
