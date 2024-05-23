#' Ndi45Op: snap operator function
#' @param operator_id character operator id
#' @param sourceProduct The source product(s) as input to the operator
#' @param resampleType If selected band s differ in size, the resample method
#' used before computing the index Value must be one of 'None', 'Lowest
#' resolution', 'Highest resolution'. Default value is 'None'.
#' @param redB4Factor The value of the red source band (B4) is multiplied by
#' this value. Default value is '1.0F'.
#' @param redB5Factor The value of the red source band (B5) is multiplied by
#' this value. Default value is '1.0F'.
#' @param redSourceBand4 The red band (B4) for the NDI45 computation. If not
#' provided, the operator will try to find the best fitting band.
#' @param redSourceBand5 The red band (B5) for the NDI45 computation. If not
#' provided, the operator will try to find the best fitting band.
#' @param downsampling The method used for aggregation (downsampling to a
#' coarser resolution). Value must be one of 'First', 'Min', 'Max', 'Mean',
#' 'Median'. Default value is 'First'.
#' @param upsampling The method used for interpolation (upsampling to a finer
#' resolution). Value must be one of 'Nearest', 'Bilinear', 'Bicubic'. Default
#' value is 'Nearest'.
#' @details
#' Descrscription from '`gpt {operator} -h`':
#'
#' "Normalized Difference Index using bands 4 and 5"
#' @import xml2
#' @return snap_op_ndi45op object
#' @export
op_ndi45op <- function(
    operator_id,
    sourceProduct,
    resampleType = "None",
    redB4Factor = "1.0F",
    redB5Factor = "1.0F",
    redSourceBand4 = NULL,
    redSourceBand5 = NULL,
    downsampling = NULL,
    upsampling = NULL) {
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
  xml_add_child(node, "operator", "Ndi45Op")
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
    "resampleType",
    gpt_args$resampleType
  )
  xml_add_child(
    parameters,
    "redB4Factor",
    gpt_args$redB4Factor
  )
  xml_add_child(
    parameters,
    "redB5Factor",
    gpt_args$redB5Factor
  )
  xml_add_child(
    parameters,
    "redSourceBand4",
    gpt_args$redSourceBand4
  )
  xml_add_child(
    parameters,
    "redSourceBand5",
    gpt_args$redSourceBand5
  )
  xml_add_child(
    parameters,
    "downsampling",
    gpt_args$downsampling
  )
  xml_add_child(
    parameters,
    "upsampling",
    gpt_args$upsampling
  )
  operator_sources <- gpt_args[c("sourceProduct")]
  snap_op_ndi45op <- S7::new_class(
    "snap_op_ndi45op",
    parent = snap_operator
  )
  snap_op_ndi45op(
    operator = "Ndi45Op",
    operator_id = operator_id,
    operator_sources = operator_sources,
    created_with = rlang::current_fn(),
    xml_graph = as.character(op_xml)
  )
}
