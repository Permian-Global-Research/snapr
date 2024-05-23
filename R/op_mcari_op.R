#' McariOp: snap operator function
#' @param operator_id character operator id
#' @param sourceProduct The source product(s) as input to the operator
#' @param resampleType If selected band s differ in size, the resample method
#' used before computing the index Value must be one of 'None', 'Lowest
#' resolution', 'Highest resolution'. Default value is 'None'.
#' @param red1Factor The value of the red source band (B4) is multiplied by
#' this value. Default value is '1.0F'.
#' @param red2Factor The value of the red source band (B5) is multiplied by
#' this value. Default value is '1.0F'.
#' @param greenFactor The value of the green source band is multiplied by this
#' value. Default value is '1.0F'.
#' @param red1SourceBand The first red band for the MCARI computation. Choose
#' B4 for Sentinel-2. If not provided, the operator will try to find the best
#' fitting band.
#' @param red2SourceBand The second red band for the MCARI computation. Choose
#' B5 for Sentinel-2. If not provided, the operator will try to find the best
#' fitting band.
#' @param greenSourceBand The green band for the MCARI computation. If not
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
#' "Modified Chlorophyll Absorption Ratio Index, developed to be responsive to
#' chlorophyll variation"
#' @import xml2
#' @return snap_op_mcari_op object
#' @export
op_mcari_op <- function(
    operator_id,
    sourceProduct,
    resampleType = "None",
    red1Factor = "1.0F",
    red2Factor = "1.0F",
    greenFactor = "1.0F",
    red1SourceBand = NULL,
    red2SourceBand = NULL,
    greenSourceBand = NULL,
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
  xml_add_child(node, "operator", "McariOp")
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
    "red1Factor",
    gpt_args$red1Factor
  )
  xml_add_child(
    parameters,
    "red2Factor",
    gpt_args$red2Factor
  )
  xml_add_child(
    parameters,
    "greenFactor",
    gpt_args$greenFactor
  )
  xml_add_child(
    parameters,
    "red1SourceBand",
    gpt_args$red1SourceBand
  )
  xml_add_child(
    parameters,
    "red2SourceBand",
    gpt_args$red2SourceBand
  )
  xml_add_child(
    parameters,
    "greenSourceBand",
    gpt_args$greenSourceBand
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
  snap_op_mcari_op <- S7::new_class(
    "snap_op_mcari_op",
    parent = snap_operator
  )
  snap_op_mcari_op(
    operator = "McariOp",
    operator_id = operator_id,
    operator_sources = operator_sources,
    created_with = rlang::current_fn(),
    xml_graph = as.character(op_xml)
  )
}
