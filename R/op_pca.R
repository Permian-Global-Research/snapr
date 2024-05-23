#' PCA: snap operator function
#' @param operator_id character operator id
#' @param sourceProduct The source product(s) as input to the operator
#' @param componentCount The maximum number of principal components to compute.
#' Default value is '-1'.
#' @param roiMaskName The name of the ROI mask that should be used.
#' @param removeNonRoiPixels Removes all non-ROI pixels in the target product.
#' Default value is 'false'.
#' @param sourceBandNames The names of the bands being used for the cluster
#' analysis.
#' @details
#' Descrscription from '`gpt {operator} -h`':
#'
#' "Performs a Principal Component Analysis."
#' @import xml2
#' @return snap_op_pca object
#' @export
op_pca <- function(
    operator_id,
    sourceProduct,
    componentCount = -1,
    roiMaskName = NULL,
    removeNonRoiPixels = FALSE,
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
  xml_add_child(node, "operator", "PCA")
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
    "componentCount",
    gpt_args$componentCount
  )
  xml_add_child(
    parameters,
    "roiMaskName",
    gpt_args$roiMaskName
  )
  xml_add_child(
    parameters,
    "removeNonRoiPixels",
    gpt_args$removeNonRoiPixels
  )
  xml_add_child(
    parameters,
    "sourceBandNames",
    gpt_args$sourceBandNames
  )
  operator_sources <- gpt_args[c("sourceProduct")]
  snap_op_pca <- S7::new_class(
    "snap_op_pca",
    parent = snap_operator
  )
  snap_op_pca(
    operator = "PCA",
    operator_id = operator_id,
    operator_sources = operator_sources,
    created_with = rlang::current_fn(),
    xml_graph = as.character(op_xml)
  )
}
