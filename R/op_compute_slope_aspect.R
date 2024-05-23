#' Compute-Slope-Aspect: snap operator function
#' @param operator_id character operator id
#' @param sourceProduct The source product(s) as input to the operator
#' @param demName The digital elevation model. Default value is 'SRTM 1Sec
#' HGT'.
#' @param demResamplingMethod Sets parameter 'demResamplingMethod' to <string>.
#' Default value is 'BILINEAR_INTERPOLATION'.
#' @param externalDEMFile Sets parameter 'externalDEMFile' to <file>.
#' @param externalDEMNoDataValue Sets parameter 'externalDEMNoDataValue' to
#' <double>. Default value is '0'.
#' @param externalDEMApplyEGM Sets parameter 'externalDEMApplyEGM' to
#' <boolean>. Default value is 'false'.
#' @param demBandName Sets parameter 'demBandName' to <string>. Default value
#' is 'elevation'.
#' @details
#' Descrscription from '`gpt {operator} -h`':
#'
#' "Compute Slope and Aspect from DEM"
#' @import xml2
#' @return snap_op_compute_slope_aspect object
#' @export
op_compute_slope_aspect <- function(
    operator_id,
    sourceProduct,
    demName = "SRTM 1Sec HGT",
    demResamplingMethod = "BILINEAR_INTERPOLATION",
    externalDEMFile = NULL,
    externalDEMNoDataValue = 0,
    externalDEMApplyEGM = FALSE,
    demBandName = "elevation") {
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
  xml_add_child(node, "operator", "Compute-Slope-Aspect")
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
    "demName",
    gpt_args$demName
  )
  xml_add_child(
    parameters,
    "demResamplingMethod",
    gpt_args$demResamplingMethod
  )
  xml_add_child(
    parameters,
    "externalDEMFile",
    gpt_args$externalDEMFile
  )
  xml_add_child(
    parameters,
    "externalDEMNoDataValue",
    gpt_args$externalDEMNoDataValue
  )
  xml_add_child(
    parameters,
    "externalDEMApplyEGM",
    gpt_args$externalDEMApplyEGM
  )
  xml_add_child(
    parameters,
    "demBandName",
    gpt_args$demBandName
  )
  operator_sources <- gpt_args[c("sourceProduct")]
  snap_op_compute_slope_aspect <- S7::new_class(
    "snap_op_compute_slope_aspect",
    parent = snap_operator
  )
  snap_op_compute_slope_aspect(
    operator = "Compute-Slope-Aspect",
    operator_id = operator_id,
    operator_sources = operator_sources,
    created_with = rlang::current_fn(),
    xml_graph = as.character(op_xml)
  )
}
