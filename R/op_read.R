#' Read: snap operator function
#' @param operator_id character operator id
#'
#' @param file The file from which the data product is read. This is a
#' mandatory parameter. Value must not be empty.
#' @param formatName An (optional) format name. Value must not be empty.
#' @param pixelRegion The subset region in pixel coordinates. Use the following
#' format: \{x>,\{y>,\{width>,\{height> If not given, the entire scene is used. The
#' 'geoRegion' parameter has precedence over this parameter.
#' @param geometryRegion The subset region in geographical coordinates using
#' WKT-format, e.g. POLYGON((\{lon1\} \{lat1\}, \{lon2\} \{lat2\}, ..., \{lon1\} \{lat1\}))
#' (make sure to quote the option due to spaces in \{geometry\}). If not given, the
#' entire scene is used.
#' @param copyMetadata Whether to copy the metadata of the source product.
#' Default value is 'true'.
#' @param sourceBands The list of source bands.
#' @param sourceMasks The list of source masks.
#' @details
#' Descrscription from '`gpt {operator} -h`':
#'
#' "Reads a data product from a given file location. Parameter Options:
#' -PcopyMetadata=<boolean> Whether to copy the metadata of the source product.
#' Default value is 'true'. -Pfile=<file> The file from which the data product is
#' read. This is a mandatory parameter. Value must not be empty.
#' -PformatName=<string> An (optional) format name. Value must not be empty.
#' -PgeometryRegion=<geometry> The subset region in geographical coordinates using
#' WKT-format, e.g. POLYGON((\{lon1\} \{lat1\}, \{lon2\} \{lat2\}, ..., \{lon1\} \{lat1\}))
#' (make sure to quote the option due to spaces in \{geometry\}). If not given, the
#' entire scene is used. -PpixelRegion=<rectangle> The subset region in pixel
#' coordinates. Use the following format: \{x>,\{y>,\{width>,\{height> If not given,
#' the entire scene is used. The 'geoRegion' parameter has precedence over this
#' parameter. -PsourceBands=<string,string,string,...> The list of source bands.
#' -PsourceMasks=<string,string,string,...> The list of source masks.
#' -PuseAdvancedOptions=<boolean> Whether to use advanced options for reading of
#' the source product. Default value is 'false'."
#' @import xml2
#' @return snap_op_read object
#' @export
op_read <- function(
    operator_id,
    file = NULL,
    formatName = NULL,
    pixelRegion = NULL,
    geometryRegion = NULL,
    copyMetadata = TRUE,
    sourceBands = NULL,
    sourceMasks = NULL) {
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
  xml_add_child(node, "operator", "Read")
  sources <- xml_add_child(node, "sources")
  parameters <- xml_add_child(node, "parameters")
  xml_add_child(
    parameters,
    "file",
    gpt_args$file
  )
  xml_add_child(
    parameters,
    "formatName",
    gpt_args$formatName
  )
  xml_add_child(
    parameters,
    "pixelRegion",
    gpt_args$pixelRegion
  )
  xml_add_child(
    parameters,
    "geometryRegion",
    gpt_args$geometryRegion
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
    "sourceMasks",
    gpt_args$sourceMasks
  )
  operator_sources <- gpt_args[c("NA")]
  snap_op_read <- S7::new_class(
    "snap_op_read",
    parent = snap_read_operator
  )
  snap_op_read(
    operator = "Read",
    operator_id = operator_id,
    created_with = rlang::current_fn(),
    xml_graph = as.character(op_xml)
  )
}
