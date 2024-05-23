#' Update-Geo-Reference: snap operator function
#' @param operator_id character operator id
#' @param sourceProduct The source product(s) as input to the operator
#' @param sourceBands The list of source bands.
#' @param demName The digital elevation model. Value must be one of 'ACE',
#' 'ASTER 1sec GDEM', 'GETASSE30', 'SRTM 1Sec HGT', 'SRTM 3Sec'. Default value is
#' 'SRTM 3Sec'.
#' @param demResamplingMethod Sets parameter 'demResamplingMethod' to <string>.
#' Default value is 'BICUBIC_INTERPOLATION'.
#' @param externalDEMFile Sets parameter 'externalDEMFile' to <file>.
#' @param externalDEMNoDataValue Sets parameter 'externalDEMNoDataValue' to
#' <double>. Default value is '0'.
#' @param reGridMethod Sets parameter 'reGridMethod' to <boolean>. Default
#' value is 'false'.
#' @details
#' Descrscription from '`gpt {operator} -h`':
#'
#' "Update Geo Reference"
#' @import xml2
#' @return snap_op_update_geo_reference object
#' @export
op_update_geo_reference <- function(
    operator_id,
    sourceProduct,
    sourceBands = NULL,
    demName = "SRTM 3Sec",
    demResamplingMethod = "BICUBIC_INTERPOLATION",
    externalDEMFile = NULL,
    externalDEMNoDataValue = 0,
    reGridMethod = FALSE) {
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
  xml_add_child(node, "operator", "Update-Geo-Reference")
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
    "demName",
    gpt_args$demName
  )
  xml_add_child(
    parameters,
    "demResamplingMethod",
    gpt_args$demResamplingMethod
  )
  xml_add_child(
    parameters,
    "externalDEMFile",
    gpt_args$externalDEMFile
  )
  xml_add_child(
    parameters,
    "externalDEMNoDataValue",
    gpt_args$externalDEMNoDataValue
  )
  xml_add_child(
    parameters,
    "reGridMethod",
    gpt_args$reGridMethod
  )
  operator_sources <- gpt_args[c("sourceProduct")]
  snap_op_update_geo_reference <- S7::new_class(
    "snap_op_update_geo_reference",
    parent = snap_operator
  )
  snap_op_update_geo_reference(
    operator = "Update-Geo-Reference",
    operator_id = operator_id,
    operator_sources = operator_sources,
    created_with = rlang::current_fn(),
    xml_graph = as.character(op_xml)
  )
}
