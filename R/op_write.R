#' Write: snap operator function
#' @param operator_id character operator id
#' @param sourceProduct The source product(s) as input to the operator
#' @param file The output file to which the data product is written.
#' @param formatName The name of the output file format. Default value is
#' 'BEAM-DIMAP'.
#' @param deleteOutputOnFailure If true, all output files are deleted after a
#' failed write operation. Default value is 'true'.
#' @param writeEntireTileRows If true, the write operation waits until an
#' entire tile row is computed. Default value is 'false'.
#' @param clearCacheAfterRowWrite If true, the internal tile cache is cleared
#' after a tile row has been written. Ignored if writeEntireTileRows=false.
#' Default value is 'false'.
#' @details
#' Descrscription from '`gpt {operator} -h`':
#'
#' "Writes a data product to a file."
#' @import xml2
#' @return snap_op_write object
#' @export
op_write <- function(
    operator_id,
    sourceProduct,
    file = NULL,
    formatName = "BEAM-DIMAP",
    deleteOutputOnFailure = TRUE,
    writeEntireTileRows = FALSE,
    clearCacheAfterRowWrite = FALSE) {
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
  xml_add_child(node, "operator", "Write")
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
    "deleteOutputOnFailure",
    gpt_args$deleteOutputOnFailure
  )
  xml_add_child(
    parameters,
    "writeEntireTileRows",
    gpt_args$writeEntireTileRows
  )
  xml_add_child(
    parameters,
    "clearCacheAfterRowWrite",
    gpt_args$clearCacheAfterRowWrite
  )
  operator_sources <- gpt_args[c("sourceProduct")]
  snap_op_write <- S7::new_class(
    "snap_op_write",
    parent = snap_operator
  )
  snap_op_write(
    operator = "Write",
    operator_id = operator_id,
    operator_sources = operator_sources,
    created_with = rlang::current_fn(),
    xml_graph = as.character(op_xml)
  )
}
