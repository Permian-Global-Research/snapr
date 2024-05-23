#' TileWriter: snap operator function
#' @param operator_id character operator id
#' @param sourceProduct The source product(s) as input to the operator
#' @param file The output file to which the data product is written.
#' @param formatName The name of the output file format. Default value is
#' 'BEAM-DIMAP'.
#' @param divisionBy How to divide the tiles Value must be one of 'Tiles',
#' 'Pixels'. Default value is 'Tiles'.
#' @param numberOfTiles The number of output tiles Value must be one of '2',
#' '4', '9', '16', '36', '64', '100', '256'. Default value is '4'.
#' @param pixelSizeX Tile pixel width Default value is '200'.
#' @param pixelSizeY Tile pixel height Default value is '200'.
#' @param overlap Tile overlap Default value is '0'.
#' @details
#' Descrscription from '`gpt {operator} -h`':
#'
#' "Writes a data product to a tiles."
#' @import xml2
#' @return snap_op_tile_writer object
#' @export
op_tile_writer <- function(
    operator_id,
    sourceProduct,
    file = NULL,
    formatName = "BEAM-DIMAP",
    divisionBy = "Tiles",
    numberOfTiles = 4,
    pixelSizeX = 200,
    pixelSizeY = 200,
    overlap = 0) {
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
  xml_add_child(node, "operator", "TileWriter")
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
    "divisionBy",
    gpt_args$divisionBy
  )
  xml_add_child(
    parameters,
    "numberOfTiles",
    gpt_args$numberOfTiles
  )
  xml_add_child(
    parameters,
    "pixelSizeX",
    gpt_args$pixelSizeX
  )
  xml_add_child(
    parameters,
    "pixelSizeY",
    gpt_args$pixelSizeY
  )
  xml_add_child(
    parameters,
    "overlap",
    gpt_args$overlap
  )
  operator_sources <- gpt_args[c("sourceProduct")]
  snap_op_tile_writer <- S7::new_class(
    "snap_op_tile_writer",
    parent = snap_operator
  )
  snap_op_tile_writer(
    operator = "TileWriter",
    operator_id = operator_id,
    operator_sources = operator_sources,
    created_with = rlang::current_fn(),
    xml_graph = as.character(op_xml)
  )
}
