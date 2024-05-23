#' EMClusterAnalysis: snap operator function
#' @param operator_id character operator id
#' @param sourceProduct The source product(s) as input to the operator
#' @param clusterCount Number of clusters Valid interval is (0,100\]. Default
#' value is '14'.
#' @param iterationCount Number of iterations Valid interval is (0,10000\].
#' Default value is '30'.
#' @param randomSeed Seed for the random generator, used for initialising the
#' algorithm. Default value is '31415'.
#' @param roiMaskName The name of the ROI-Mask that should be used.
#' @param includeProbabilityBands Determines whether the posterior
#' probabilities are included as band data. Default value is 'false'.
#' @param sourceBandNames The names of the bands being used for the cluster
#' analysis.
#' @details
#' Descrscription from '`gpt {operator} -h`':
#'
#' "Performs an expectation-maximization (EM) cluster analysis."
#' @import xml2
#' @return snap_op_emcluster_analysis object
#' @export
op_emcluster_analysis <- function(
    operator_id,
    sourceProduct,
    clusterCount = 14,
    iterationCount = 30,
    randomSeed = 31415,
    roiMaskName = NULL,
    includeProbabilityBands = FALSE,
    sourceBandNames = NULL) {
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
  xml_add_child(node, "operator", "EMClusterAnalysis")
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
    "clusterCount",
    gpt_args$clusterCount
  )
  xml_add_child(
    parameters,
    "iterationCount",
    gpt_args$iterationCount
  )
  xml_add_child(
    parameters,
    "randomSeed",
    gpt_args$randomSeed
  )
  xml_add_child(
    parameters,
    "roiMaskName",
    gpt_args$roiMaskName
  )
  xml_add_child(
    parameters,
    "includeProbabilityBands",
    gpt_args$includeProbabilityBands
  )
  xml_add_child(
    parameters,
    "sourceBandNames",
    gpt_args$sourceBandNames
  )
  operator_sources <- gpt_args[c("sourceProduct")]
  snap_op_emcluster_analysis <- S7::new_class(
    "snap_op_emcluster_analysis",
    parent = snap_operator
  )
  snap_op_emcluster_analysis(
    operator = "EMClusterAnalysis",
    operator_id = operator_id,
    operator_sources = operator_sources,
    created_with = rlang::current_fn(),
    xml_graph = as.character(op_xml)
  )
}
