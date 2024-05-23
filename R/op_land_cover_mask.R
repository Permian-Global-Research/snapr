#' Land-Cover-Mask: snap operator function
#' @param operator_id character operator id
#' @param sourceProduct The source product(s) as input to the operator
#' @param sourceBands The list of source bands.
#' @param landCoverBand Land cover band
#' @param validLandCoverClasses Land cover classes to include
#' @param validPixelExpression Valid pixel expression
#' @param includeOtherBands Add other bands unmasked Default value is 'false'.
#' @details
#' Descrscription from '`gpt {operator} -h`':
#'
#' "Perform a masking based on land cover classes"
#' @import xml2
#' @return snap_op_land_cover_mask object
#' @export
op_land_cover_mask <- function(
    operator_id,
    sourceProduct,
    sourceBands = NULL,
    landCoverBand = NULL,
    validLandCoverClasses = NULL,
    validPixelExpression = NULL,
    includeOtherBands = FALSE) {
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
  xml_add_child(node, "operator", "Land-Cover-Mask")
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
    "landCoverBand",
    gpt_args$landCoverBand
  )
  xml_add_child(
    parameters,
    "validLandCoverClasses",
    gpt_args$validLandCoverClasses
  )
  xml_add_child(
    parameters,
    "validPixelExpression",
    gpt_args$validPixelExpression
  )
  xml_add_child(
    parameters,
    "includeOtherBands",
    gpt_args$includeOtherBands
  )
  operator_sources <- gpt_args[c("sourceProduct")]
  snap_op_land_cover_mask <- S7::new_class(
    "snap_op_land_cover_mask",
    parent = snap_operator
  )
  snap_op_land_cover_mask(
    operator = "Land-Cover-Mask",
    operator_id = operator_id,
    operator_sources = operator_sources,
    created_with = rlang::current_fn(),
    xml_graph = as.character(op_xml)
  )
}
