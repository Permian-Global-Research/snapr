#' TileCache: snap operator function
#' @param operator_id character operator id
#' @param sourceProduct The source product(s) as input to the operator
#' @param cacheSize The cache size in MB. Set it to 0 to use default tile
#' cache. Default value is '1000'. Parameter unit is 'MB'.
#' @details
#' Descrscription from '`gpt {operator} -h`':
#'
#' "Experimental Operator which provides a dedicated cache for its source
#' product. A guide on how this operator is used is provided at
#' https://senbox.atlassian.net/wiki/x/VQCTLw."
#' @import xml2
#' @return snap_op_tile_cache object
#' @export
op_tile_cache <- function(
    operator_id,
    sourceProduct,
    cacheSize = 1000) {
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
  xml_add_child(node, "operator", "TileCache")
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
    "cacheSize",
    gpt_args$cacheSize
  )
  operator_sources <- gpt_args[c("sourceProduct")]
  snap_op_tile_cache <- S7::new_class(
    "snap_op_tile_cache",
    parent = snap_operator
  )
  snap_op_tile_cache(
    operator = "TileCache",
    operator_id = operator_id,
    operator_sources = operator_sources,
    created_with = rlang::current_fn(),
    xml_graph = as.character(op_xml)
  )
}
