#' Offset-Tracking: snap operator function
#' @param operator_id character operator id
#' @param sourceProduct The source product(s) as input to the operator
#' @param gridAzimuthSpacing The output grid azimuth spacing in pixels Valid
#' interval is (1, *). Default value is '40'.
#' @param gridRangeSpacing The output grid range spacing in pixels Valid
#' interval is (1, *). Default value is '40'.
#' @param registrationWindowWidth Sets parameter 'registrationWindowWidth' to
#' <string>. Value must be one of '32', '64', '128', '256', '512', '1024', '2048'.
#' Default value is '128'.
#' @param registrationWindowHeight Sets parameter 'registrationWindowHeight' to
#' <string>. Value must be one of '32', '64', '128', '256', '512', '1024', '2048'.
#' Default value is '128'.
#' @param xCorrThreshold The cross-correlation threshold Valid interval is (0,
#' *). Default value is '0.1'.
#' @param registrationOversampling Sets parameter 'registrationOversampling' to
#' <string>. Value must be one of '2', '4', '8', '16', '32', '64', '128', '256',
#' '512'. Default value is '16'.
#' @param averageBoxSize Sets parameter 'averageBoxSize' to <string>. Value
#' must be one of '3', '5', '9', '11'. Default value is '5'.
#' @param maxVelocity The threshold for eliminating invalid GCPs Valid interval
#' is (0, *). Default value is '5.0'.
#' @param radius Radius for Hole-Filling Valid interval is (0, *). Default
#' value is '4'.
#' @param resamplingType Methods for velocity interpolation. Value must be one
#' of 'NEAREST_NEIGHBOUR', 'BILINEAR_INTERPOLATION', 'BICUBIC_INTERPOLATION',
#' 'BISINC_5_POINT_INTERPOLATION', 'CUBIC_CONVOLUTION'. Default value is
#' 'BICUBIC_INTERPOLATION'.
#' @param spatialAverage Sets parameter 'spatialAverage' to <boolean>. Default
#' value is 'true'.
#' @param fillHoles Sets parameter 'fillHoles' to <boolean>. Default value is
#' 'true'.
#' @param roiVector Sets parameter 'roiVector' to <string>.
#' @details
#' Descrscription from '`gpt {operator} -h`':
#'
#' "Create velocity vectors from offset tracking"
#' @import xml2
#' @return snap_op_offset_tracking object
#' @export
op_offset_tracking <- function(
    operator_id,
    sourceProduct,
    gridAzimuthSpacing = 40,
    gridRangeSpacing = 40,
    registrationWindowWidth = 128,
    registrationWindowHeight = 128,
    xCorrThreshold = 0.1,
    registrationOversampling = 16,
    averageBoxSize = 5,
    maxVelocity = 5.0,
    radius = 4,
    resamplingType = "BICUBIC_INTERPOLATION",
    spatialAverage = TRUE,
    fillHoles = TRUE,
    roiVector = NULL) {
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
  xml_add_child(node, "operator", "Offset-Tracking")
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
    "gridAzimuthSpacing",
    gpt_args$gridAzimuthSpacing
  )
  xml_add_child(
    parameters,
    "gridRangeSpacing",
    gpt_args$gridRangeSpacing
  )
  xml_add_child(
    parameters,
    "registrationWindowWidth",
    gpt_args$registrationWindowWidth
  )
  xml_add_child(
    parameters,
    "registrationWindowHeight",
    gpt_args$registrationWindowHeight
  )
  xml_add_child(
    parameters,
    "xCorrThreshold",
    gpt_args$xCorrThreshold
  )
  xml_add_child(
    parameters,
    "registrationOversampling",
    gpt_args$registrationOversampling
  )
  xml_add_child(
    parameters,
    "averageBoxSize",
    gpt_args$averageBoxSize
  )
  xml_add_child(
    parameters,
    "maxVelocity",
    gpt_args$maxVelocity
  )
  xml_add_child(
    parameters,
    "radius",
    gpt_args$radius
  )
  xml_add_child(
    parameters,
    "resamplingType",
    gpt_args$resamplingType
  )
  xml_add_child(
    parameters,
    "spatialAverage",
    gpt_args$spatialAverage
  )
  xml_add_child(
    parameters,
    "fillHoles",
    gpt_args$fillHoles
  )
  xml_add_child(
    parameters,
    "roiVector",
    gpt_args$roiVector
  )
  operator_sources <- gpt_args[c("sourceProduct")]
  snap_op_offset_tracking <- S7::new_class(
    "snap_op_offset_tracking",
    parent = snap_operator
  )
  snap_op_offset_tracking(
    operator = "Offset-Tracking",
    operator_id = operator_id,
    operator_sources = operator_sources,
    created_with = rlang::current_fn(),
    xml_graph = as.character(op_xml)
  )
}
