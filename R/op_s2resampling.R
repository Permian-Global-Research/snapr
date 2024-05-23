#' S2Resampling: snap operator function
#' @param operator_id character operator id
#' @param sourceProduct The source product(s) as input to the operator
#' @param resampleOnPyramidLevels This setting will increase performance when
#' viewing the image, but accurate resamplings are only retrieved when zooming in
#' on a pixel. Default value is 'true'.
#' @param downsampling The method used for aggregation (downsampling to a
#' coarser resolution). Value must be one of 'First', 'Min', 'Max', 'Mean',
#' 'Median'. Default value is 'Mean'.
#' @param flagDownsampling The method used for aggregation (downsampling to a
#' coarser resolution) of flags. Value must be one of 'First', 'FlagAnd',
#' 'FlagOr', 'FlagMedianAnd', 'FlagMedianOr'. Default value is 'First'.
#' @param resolution The output resolution. Value must be one of '10', '20',
#' '60'. Default value is '60'.
#' @param upsampling The method used for interpolation (upsampling to a finer
#' resolution). Value must be one of 'Nearest', 'Bilinear', 'Bicubic'. Default
#' value is 'Bilinear'.
#' @details
#' Descrscription from '`gpt {operator} -h`':
#'
#' "Specific S2 resample algorithm"
#' @import xml2
#' @return snap_op_s2resampling object
#' @export
op_s2resampling <- function(
    operator_id,
    sourceProduct,
    resampleOnPyramidLevels = TRUE,
    downsampling = NULL,
    flagDownsampling = NULL,
    resolution = NULL,
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
  xml_add_child(node, "operator", "S2Resampling")
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
    "resampleOnPyramidLevels",
    gpt_args$resampleOnPyramidLevels
  )
  xml_add_child(
    parameters,
    "downsampling",
    gpt_args$downsampling
  )
  xml_add_child(
    parameters,
    "flagDownsampling",
    gpt_args$flagDownsampling
  )
  xml_add_child(
    parameters,
    "resolution",
    gpt_args$resolution
  )
  xml_add_child(
    parameters,
    "upsampling",
    gpt_args$upsampling
  )
  operator_sources <- gpt_args[c("sourceProduct")]
  snap_op_s2resampling <- S7::new_class(
    "snap_op_s2resampling",
    parent = snap_operator
  )
  snap_op_s2resampling(
    operator = "S2Resampling",
    operator_id = operator_id,
    operator_sources = operator_sources,
    created_with = rlang::current_fn(),
    xml_graph = as.character(op_xml)
  )
}
