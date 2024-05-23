#' Land-Sea-Mask: snap operator function
#' @param operator_id character operator id
#' @param sourceProduct The source product(s) as input to the operator
#' @param sourceBands The list of source bands.
#' @param landMask Sets parameter 'landMask' to <boolean>. Default value is
#' 'true'.
#' @param useSRTM Sets parameter 'useSRTM' to <boolean>. Default value is
#' 'true'.
#' @param geometry Sets parameter 'geometry' to <string>.
#' @param invertGeometry Sets parameter 'invertGeometry' to <boolean>. Default
#' value is 'false'.
#' @param shorelineExtension Sets parameter 'shorelineExtension' to <integer>.
#' Default value is '0'.
#' @details
#' Descrscription from '`gpt {operator} -h`':
#'
#' "Creates a bitmask defining land vs ocean."
#' @import xml2
#' @return snap_op_land_sea_mask object
#' @export
op_land_sea_mask <- function(
    operator_id,
    sourceProduct,
    sourceBands = NULL,
    landMask = TRUE,
    useSRTM = TRUE,
    geometry = NULL,
    invertGeometry = FALSE,
    shorelineExtension = 0) {
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
  xml_add_child(node, "operator", "Land-Sea-Mask")
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
    "landMask",
    gpt_args$landMask
  )
  xml_add_child(
    parameters,
    "useSRTM",
    gpt_args$useSRTM
  )
  xml_add_child(
    parameters,
    "geometry",
    gpt_args$geometry
  )
  xml_add_child(
    parameters,
    "invertGeometry",
    gpt_args$invertGeometry
  )
  xml_add_child(
    parameters,
    "shorelineExtension",
    gpt_args$shorelineExtension
  )
  operator_sources <- gpt_args[c("sourceProduct")]
  snap_op_land_sea_mask <- S7::new_class(
    "snap_op_land_sea_mask",
    parent = snap_operator
  )
  snap_op_land_sea_mask(
    operator = "Land-Sea-Mask",
    operator_id = operator_id,
    operator_sources = operator_sources,
    created_with = rlang::current_fn(),
    xml_graph = as.character(op_xml)
  )
}
