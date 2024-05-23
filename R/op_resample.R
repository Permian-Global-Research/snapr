#' Resample: snap operator function
#' @param operator_id character operator id
#' @param sourceProduct The source product(s) as input to the operator
#' @param targetWidth The width that all bands of the target product shall
#' have. If this is set, targetHeight must be set, too. Either this and
#' targetHeight or referenceBand or targetResolution must be set.
#' @param targetHeight The height that all bands of the target product shall
#' have. If this is set, targetWidth must be set, too. Either this and targetWidth
#' or referenceBand or targetResolution must be set.
#' @param targetResolution The resolution that all bands of the target product
#' shall have. The same value will be applied to scale image widths and heights.
#' Either this or referenceBand or targetwidth and targetHeight must be set.
#' @param resamplingPreset The resampling preset. This will over rules the
#' settings for upsampling, downsampling and flagDownsampling.
#' @param bandResamplings The band resamplings. This will over rules the
#' settings for resamplingPreset.
#' @param resampleOnPyramidLevels This setting will increase performance when
#' viewing the image, but accurate resamplings are only retrieved when zooming in
#' on a pixel. Default value is 'true'.
#' @param downsampling The method used for aggregation (downsampling to a
#' coarser resolution). Value must be one of 'First', 'Min', 'Max', 'Mean',
#' 'Median'. Default value is 'First'.
#' @param flagDownsampling The method used for aggregation (downsampling to a
#' coarser resolution) of flags. Value must be one of 'First', 'FlagAnd',
#' 'FlagOr', 'FlagMedianAnd', 'FlagMedianOr'. Default value is 'First'.
#' @param referenceBand The name of the reference band. All other bands will be
#' re-sampled to match its size and resolution. Either this or targetResolutionor
#' targetWidth and targetHeight must be set.
#' @param upsampling The method used for interpolation (upsampling to a finer
#' resolution). Value must be one of 'Nearest', 'Bilinear', 'Bicubic'. Default
#' value is 'Nearest'.
#' @details
#' Descrscription from '`gpt {operator} -h`':
#'
#' "Resampling of a multi-size source product to a single-size target product."
#' @import xml2
#' @return snap_op_resample object
#' @export
op_resample <- function(
    operator_id,
    sourceProduct,
    targetWidth = NULL,
    targetHeight = NULL,
    targetResolution = NULL,
    resamplingPreset = NULL,
    bandResamplings = NULL,
    resampleOnPyramidLevels = TRUE,
    downsampling = NULL,
    flagDownsampling = NULL,
    referenceBand = NULL,
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
  xml_add_child(node, "operator", "Resample")
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
    "targetWidth",
    gpt_args$targetWidth
  )
  xml_add_child(
    parameters,
    "targetHeight",
    gpt_args$targetHeight
  )
  xml_add_child(
    parameters,
    "targetResolution",
    gpt_args$targetResolution
  )
  xml_add_child(
    parameters,
    "resamplingPreset",
    gpt_args$resamplingPreset
  )
  xml_add_child(
    parameters,
    "bandResamplings",
    gpt_args$bandResamplings
  )
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
    "referenceBand",
    gpt_args$referenceBand
  )
  xml_add_child(
    parameters,
    "upsampling",
    gpt_args$upsampling
  )
  operator_sources <- gpt_args[c("sourceProduct")]
  snap_op_resample <- S7::new_class(
    "snap_op_resample",
    parent = snap_operator
  )
  snap_op_resample(
    operator = "Resample",
    operator_id = operator_id,
    operator_sources = operator_sources,
    created_with = rlang::current_fn(),
    xml_graph = as.character(op_xml)
  )
}
