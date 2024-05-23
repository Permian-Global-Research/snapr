#' PssraOp: snap operator function
#' @param operator_id character operator id
#' @param sourceProduct The source product(s) as input to the operator
#' @param resampleType If selected band s differ in size, the resample method
#' used before computing the index Value must be one of 'None', 'Lowest
#' resolution', 'Highest resolution'. Default value is 'None'.
#' @param redFactor The value of the red source band is multiplied by this
#' value. Default value is '1.0F'.
#' @param nirFactor The value of the NIR source band is multiplied by this
#' value. Default value is '1.0F'.
#' @param redSourceBand The red band for the PSSRa computation. If not
#' provided, the operator will try to find the best fitting band.
#' @param nirSourceBand The near-infrared band for the PSSRa computation. If
#' not provided, the operator will try to find the best fitting band.
#' @param downsampling The method used for aggregation (downsampling to a
#' coarser resolution). Value must be one of 'First', 'Min', 'Max', 'Mean',
#' 'Median'. Default value is 'First'.
#' @param upsampling The method used for interpolation (upsampling to a finer
#' resolution). Value must be one of 'Nearest', 'Bilinear', 'Bicubic'. Default
#' value is 'Nearest'.
#' @details
#' Descrscription from '`gpt {operator} -h`':
#'
#' "Pigment Specific Simple Ratio, chlorophyll index"
#' @import xml2
#' @return snap_op_pssra_op object
#' @export
op_pssra_op <- function(
    operator_id,
    sourceProduct,
    resampleType = "None",
    redFactor = "1.0F",
    nirFactor = "1.0F",
    redSourceBand = NULL,
    nirSourceBand = NULL,
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
  xml_add_child(node, "operator", "PssraOp")
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
    "redFactor",
    gpt_args$redFactor
  )
  xml_add_child(
    parameters,
    "nirFactor",
    gpt_args$nirFactor
  )
  xml_add_child(
    parameters,
    "redSourceBand",
    gpt_args$redSourceBand
  )
  xml_add_child(
    parameters,
    "nirSourceBand",
    gpt_args$nirSourceBand
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
  snap_op_pssra_op <- S7::new_class(
    "snap_op_pssra_op",
    parent = snap_operator
  )
  snap_op_pssra_op(
    operator = "PssraOp",
    operator_id = operator_id,
    operator_sources = operator_sources,
    created_with = rlang::current_fn(),
    xml_graph = as.character(op_xml)
  )
}
