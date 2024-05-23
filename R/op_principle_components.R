#' Principle-Components: snap operator function
#' @param operator_id character operator id
#' @param sourceProduct The source product(s) as input to the operator
#' @param sourceBands The list of source bands.
#' @param selectEigenvaluesBy Sets parameter 'selectEigenvaluesBy' to <string>.
#' Value must be one of 'Eigenvalue Threshold', 'Number of Eigenvalues'. Default
#' value is 'Eigenvalue Threshold'.
#' @param eigenvalueThreshold The threshold for selecting eigenvalues Valid
#' interval is (0, 100\]. Default value is '100'.
#' @param numPCA The number of PCA images output Valid interval is (0, 100\].
#' Default value is '1'.
#' @param showEigenvalues Show the eigenvalues Default value is '1'.
#' @param subtractMeanImage Subtract mean image Default value is '1'.
#' @details
#' Descrscription from '`gpt {operator} -h`':
#'
#' "Principle Component Analysis"
#' @import xml2
#' @return snap_op_principle_components object
#' @export
op_principle_components <- function(
    operator_id,
    sourceProduct,
    sourceBands = NULL,
    selectEigenvaluesBy = "Eigenvalue Threshold",
    eigenvalueThreshold = 100,
    numPCA = 1,
    showEigenvalues = 1,
    subtractMeanImage = 1) {
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
  xml_add_child(node, "operator", "Principle-Components")
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
    "selectEigenvaluesBy",
    gpt_args$selectEigenvaluesBy
  )
  xml_add_child(
    parameters,
    "eigenvalueThreshold",
    gpt_args$eigenvalueThreshold
  )
  xml_add_child(
    parameters,
    "numPCA",
    gpt_args$numPCA
  )
  xml_add_child(
    parameters,
    "showEigenvalues",
    gpt_args$showEigenvalues
  )
  xml_add_child(
    parameters,
    "subtractMeanImage",
    gpt_args$subtractMeanImage
  )
  operator_sources <- gpt_args[c("sourceProduct")]
  snap_op_principle_components <- S7::new_class(
    "snap_op_principle_components",
    parent = snap_operator
  )
  snap_op_principle_components(
    operator = "Principle-Components",
    operator_id = operator_id,
    operator_sources = operator_sources,
    created_with = rlang::current_fn(),
    xml_graph = as.character(op_xml)
  )
}
