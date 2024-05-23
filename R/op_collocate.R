#' Collocate: snap operator function
#' @param operator_id character operator id
#' @param sourceProduct The source product(s) as input to the operator
#' @param reference The source product which serves as reference. This is an
#' optional source.
#' @param secondary The source product which serves as secondary. This is an
#' optional source.
#' @param sourceProductPaths A comma-separated list of file paths specifying
#' the source products
#' @param referenceProductName The name of the reference product.
#' @param targetProductType The product type string for the target product
#' (informal) Default value is 'COLLOCATED'.
#' @param copySecondaryMetadata Copies also the metadata of the secondary
#' products to the target. Default value is 'false'.
#' @param renameReferenceComponents Whether or not components of the reference
#' product shall be renamed in the target product. Default value is 'true'.
#' @param renameSecondaryComponents Whether or not components of the secondary
#' product(s) shall be renamed in the target product. Default value is 'true'.
#' @param referenceComponentPattern The text pattern to be used when renaming
#' reference components. Default value is '$\{ORIGINAL_NAME\}_M'.
#' @param secondaryComponentPattern The text pattern to be used when renaming
#' secondary components. Default value is '$\{ORIGINAL_NAME\}_S$\{SLAVE_NUMBER_ID\}'.
#' @param resamplingType The method to be used when resampling the secondary
#' grid onto the reference grid. Default value is 'NEAREST_NEIGHBOUR'.
#' @details
#' Descrscription from '`gpt {operator} -h`':
#'
#' "Collocates two products based on their geo-codings."
#' @import xml2
#' @return snap_op_collocate object
#' @export
op_collocate <- function(
    operator_id,
    sourceProduct,
    reference = NULL,
    secondary = NULL,
    sourceProductPaths = NULL,
    referenceProductName = NULL,
    targetProductType = "COLLOCATED",
    copySecondaryMetadata = FALSE,
    renameReferenceComponents = TRUE,
    renameSecondaryComponents = TRUE,
    referenceComponentPattern = "${ORIGINAL_NAME}_M",
    secondaryComponentPattern = "${ORIGINAL_NAME}_S${SLAVE_NUMBER_ID}",
    resamplingType = "NEAREST_NEIGHBOUR") {
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
  xml_add_child(node, "operator", "Collocate")
  sources <- xml_add_child(node, "sources")
  op_src_id <- sub(".0", "", paste0(".", seq_along(sourceProduct) - 1))
  purrr::walk2(
    op_src_id,
    sourceProduct,
    function(.x, .y) {
      xml_add_child(sources, paste0("sourceProduct", .x), refid = .y)
    }
  )
  xml_add_child(
    sources,
    "reference",
    gpt_args$reference
  )
  xml_add_child(
    sources,
    "secondary",
    gpt_args$secondary
  )
  parameters <- xml_add_child(node, "parameters")
  xml_add_child(
    parameters,
    "sourceProductPaths",
    gpt_args$sourceProductPaths
  )
  xml_add_child(
    parameters,
    "referenceProductName",
    gpt_args$referenceProductName
  )
  xml_add_child(
    parameters,
    "targetProductType",
    gpt_args$targetProductType
  )
  xml_add_child(
    parameters,
    "copySecondaryMetadata",
    gpt_args$copySecondaryMetadata
  )
  xml_add_child(
    parameters,
    "renameReferenceComponents",
    gpt_args$renameReferenceComponents
  )
  xml_add_child(
    parameters,
    "renameSecondaryComponents",
    gpt_args$renameSecondaryComponents
  )
  xml_add_child(
    parameters,
    "referenceComponentPattern",
    gpt_args$referenceComponentPattern
  )
  xml_add_child(
    parameters,
    "secondaryComponentPattern",
    gpt_args$secondaryComponentPattern
  )
  xml_add_child(
    parameters,
    "resamplingType",
    gpt_args$resamplingType
  )
  operator_sources <- gpt_args[c(
    "sourceProduct",
    "reference",
    "secondary"
  )]
  snap_op_collocate <- S7::new_class(
    "snap_op_collocate",
    parent = snap_operator
  )
  snap_op_collocate(
    operator = "Collocate",
    operator_id = operator_id,
    operator_sources = operator_sources,
    created_with = rlang::current_fn(),
    xml_graph = as.character(op_xml)
  )
}
