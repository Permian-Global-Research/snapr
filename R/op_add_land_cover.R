#' AddLandCover: snap operator function
#' @param operator_id character operator id
#' @param sourceProduct The source product(s) as input to the operator
#' @param landCoverNames The land cover model. Default value is 'AAFC Canada
#' Sand Pct'.
#' @param externalFiles The external landcover files.
#' @param resamplingMethod Sets parameter 'resamplingMethod' to <string>.
#' Default value is 'NEAREST_NEIGHBOUR'.
#' @details
#' Descrscription from '`gpt {operator} -h`':
#'
#' "Creates a land cover band"
#' @import xml2
#' @return snap_op_add_land_cover object
#' @export
op_add_land_cover <- function(
    operator_id,
    sourceProduct,
    landCoverNames = "AAFC Canada Sand Pct",
    externalFiles = NULL,
    resamplingMethod = "NEAREST_NEIGHBOUR") {
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
  xml_add_child(node, "operator", "AddLandCover")
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
    "landCoverNames",
    gpt_args$landCoverNames
  )
  xml_add_child(
    parameters,
    "externalFiles",
    gpt_args$externalFiles
  )
  xml_add_child(
    parameters,
    "resamplingMethod",
    gpt_args$resamplingMethod
  )
  operator_sources <- gpt_args[c("sourceProduct")]
  snap_op_add_land_cover <- S7::new_class(
    "snap_op_add_land_cover",
    parent = snap_operator
  )
  snap_op_add_land_cover(
    operator = "AddLandCover",
    operator_id = operator_id,
    operator_sources = operator_sources,
    created_with = rlang::current_fn(),
    xml_graph = as.character(op_xml)
  )
}
