#' Subset: snap operator function
#' @param operator_id character operator id
#' @param sourceProduct The source product(s) as input to the operator
#' @param region The subset region in pixel coordinates. Use the following
#' format: \{x\},\{y\},\{width\},\{height\} If not given, the entire scene is used. The
#' 'geoRegion' parameter has precedence over this parameter.
#' @param referenceBand The band used to indicate the pixel coordinates.
#' @param geoRegion The subset region in geographical coordinates using
#' WKT-format, e.g. POLYGON((\{lon1\} \{lat1\}, \{lon2\} \{lat2\}, ..., \{lon1\} \{lat1\}))
#' (make sure to quote the option due to spaces in \{geometry\}). If not given, the
#' entire scene is used.
#' @param subSamplingX The pixel sub-sampling step in X (horizontal image
#' direction) Default value is '1'.
#' @param subSamplingY The pixel sub-sampling step in Y (vertical image
#' direction) Default value is '1'.
#' @param fullSwath Forces the operator to extend the subset region to the full
#' swath. Default value is 'false'.
#' @param copyMetadata Whether to copy the metadata of the source product.
#' Default value is 'false'.
#' @param sourceBands The list of source bands.
#' @param tiePointGrids The list of tie-point grid names.
#' @details
#' Descrscription from '`gpt {operator} -h`':
#'
#' "Create a spatial and/or spectral subset of a data product."
#' @import xml2
#' @return snap_op_subset object
#' @export
op_subset <- function(
    operator_id,
    sourceProduct,
    region = NULL,
    referenceBand = NULL,
    geoRegion = NULL,
    subSamplingX = 1,
    subSamplingY = 1,
    fullSwath = FALSE,
    copyMetadata = FALSE,
    sourceBands = NULL,
    tiePointGrids = NULL) {
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
  xml_add_child(node, "operator", "Subset")
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
    "region",
    gpt_args$region
  )
  xml_add_child(
    parameters,
    "referenceBand",
    gpt_args$referenceBand
  )
  xml_add_child(
    parameters,
    "geoRegion",
    gpt_args$geoRegion
  )
  xml_add_child(
    parameters,
    "subSamplingX",
    gpt_args$subSamplingX
  )
  xml_add_child(
    parameters,
    "subSamplingY",
    gpt_args$subSamplingY
  )
  xml_add_child(
    parameters,
    "fullSwath",
    gpt_args$fullSwath
  )
  xml_add_child(
    parameters,
    "copyMetadata",
    gpt_args$copyMetadata
  )
  xml_add_child(
    parameters,
    "sourceBands",
    gpt_args$sourceBands
  )
  xml_add_child(
    parameters,
    "tiePointGrids",
    gpt_args$tiePointGrids
  )
  operator_sources <- gpt_args[c("sourceProduct")]
  snap_op_subset <- S7::new_class(
    "snap_op_subset",
    parent = snap_operator
  )
  snap_op_subset(
    operator = "Subset",
    operator_id = operator_id,
    operator_sources = operator_sources,
    created_with = rlang::current_fn(),
    xml_graph = as.character(op_xml)
  )
}
