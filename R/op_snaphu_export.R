#' SnaphuExport: snap operator function
#' @param operator_id character operator id
#' @param sourceProduct The source product(s) as input to the operator
#' @param targetFolder The output folder to which the data product is written.
#' @param statCostMode Size of coherence estimation window in Azimuth direction
#' Value must be one of 'TOPO', 'DEFO', 'SMOOTH', 'NOSTATCOSTS'. Default value is
#' 'DEFO'.
#' @param initMethod Algorithm used for initialization of the wrapped phase
#' values Value must be one of 'MST', 'MCF'. Default value is 'MST'.
#' @param numberOfTileRows Divide the image into tiles and process in parallel.
#' Set to 1 for single tiled. Default value is '10'.
#' @param numberOfTileCols Divide the image into tiles and process in parallel.
#' Set to 1 for single tiled. Default value is '10'.
#' @param numberOfProcessors Number of concurrent processing threads. Set to 1
#' for single threaded. Default value is '4'.
#' @param rowOverlap Overlap, in pixels, between neighboring tiles. Default
#' value is '200'.
#' @param colOverlap Overlap, in pixels, between neighboring tiles. Default
#' value is '200'.
#' @param tileCostThreshold Cost threshold to use for determining boundaries of
#' reliable regions (long, dimensionless; scaled according to other cost
#' constants). Larger cost threshold implies smaller regions---safer, but more
#' expensive computationally. Default value is '500'.
#' @details
#' Descrscription from '`gpt {operator} -h`':
#'
#' "Export data and prepare conf file for SNAPHU processing"
#' @import xml2
#' @return snap_op_snaphu_export object
#' @export
op_snaphu_export <- function(
    operator_id,
    sourceProduct,
    targetFolder = NULL,
    statCostMode = "DEFO",
    initMethod = "MST",
    numberOfTileRows = 10,
    numberOfTileCols = 10,
    numberOfProcessors = 4,
    rowOverlap = 200,
    colOverlap = 200,
    tileCostThreshold = 500) {
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
  xml_add_child(node, "operator", "SnaphuExport")
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
    "targetFolder",
    gpt_args$targetFolder
  )
  xml_add_child(
    parameters,
    "statCostMode",
    gpt_args$statCostMode
  )
  xml_add_child(
    parameters,
    "initMethod",
    gpt_args$initMethod
  )
  xml_add_child(
    parameters,
    "numberOfTileRows",
    gpt_args$numberOfTileRows
  )
  xml_add_child(
    parameters,
    "numberOfTileCols",
    gpt_args$numberOfTileCols
  )
  xml_add_child(
    parameters,
    "numberOfProcessors",
    gpt_args$numberOfProcessors
  )
  xml_add_child(
    parameters,
    "rowOverlap",
    gpt_args$rowOverlap
  )
  xml_add_child(
    parameters,
    "colOverlap",
    gpt_args$colOverlap
  )
  xml_add_child(
    parameters,
    "tileCostThreshold",
    gpt_args$tileCostThreshold
  )
  operator_sources <- gpt_args[c("sourceProduct")]
  snap_op_snaphu_export <- S7::new_class(
    "snap_op_snaphu_export",
    parent = snap_operator
  )
  snap_op_snaphu_export(
    operator = "SnaphuExport",
    operator_id = operator_id,
    operator_sources = operator_sources,
    created_with = rlang::current_fn(),
    xml_graph = as.character(op_xml)
  )
}
