#' Aatsr.SST: snap operator function
#' @param operator_id character operator id
#' @param sourceProduct The source product(s) as input to the operator
#' @param dual Enables/disables generation of the dual-view SST Default value
#' is 'true'.
#' @param dualCoefficientsFile Coefficient file for the dual-view SST Value
#' must be one of 'AVERAGE_POLAR_DUAL_VIEW', 'AVERAGE_TEMPERATE_DUAL_VIEW',
#' 'AVERAGE_TROPICAL_DUAL_VIEW', 'GRIDDED_POLAR_DUAL_VIEW',
#' 'GRIDDED_TEMPERATE_DUAL_VIEW', 'GRIDDED_TROPICAL_DUAL_VIEW',
#' 'GRIDDED_DUAL_VIEW_IPF'. Default value is 'AVERAGE_POLAR_DUAL_VIEW'.
#' @param dualMaskExpression ROI-mask used for the dual-view SST Default value
#' is '!cloud_flags_nadir.LAND and !cloud_flags_nadir.CLOUDY and
#' !cloud_flags_nadir.SUN_GLINT and !cloud_flags_fward.LAND and
#' !cloud_flags_fward.CLOUDY and !cloud_flags_fward.SUN_GLINT'.
#' @param nadir Enables/disables generation of the nadir-view SST Default value
#' is 'true'.
#' @param nadirCoefficientsFile Coefficient file for the nadir-view SST Value
#' must be one of 'AVERAGE_POLAR_SINGLE_VIEW', 'AVERAGE_TEMPERATE_SINGLE_VIEW',
#' 'AVERAGE_TROPICAL_SINGLE_VIEW', 'GRIDDED_POLAR_SINGLE_VIEW',
#' 'GRIDDED_TEMPERATE_SINGLE_VIEW', 'GRIDDED_TROPICAL_SINGLE_VIEW'. Default value
#' is 'AVERAGE_POLAR_SINGLE_VIEW'.
#' @param nadirMaskExpression ROI-mask used for the nadir-view SST Default
#' value is '!cloud_flags_nadir.LAND and !cloud_flags_nadir.CLOUDY and
#' !cloud_flags_nadir.SUN_GLINT'.
#' @param invalidSstValue Value used to fill invalid SST pixels Default value
#' is '-999.0f'.
#' @details
#' Descrscription from '`gpt {operator} -h`':
#'
#' "Computes sea surface temperature (SST) from (A)ATSR products."
#' @import xml2
#' @return snap_op_aatsr_sst object
#' @export
op_aatsr_sst <- function(
    operator_id,
    sourceProduct,
    dual = TRUE,
    dualCoefficientsFile = "AVERAGE_POLAR_DUAL_VIEW",
    dualMaskExpression = "!cloud_flags_nadir.LAND and !cloud_flags_nadir.CLOUDY and !cloud_flags_nadir.SUN_GLINT and !cloud_flags_fward.LAND and !cloud_flags_fward.CLOUDY and !cloud_flags_fward.SUN_GLINT",
    nadir = TRUE,
    nadirCoefficientsFile = "AVERAGE_POLAR_SINGLE_VIEW",
    nadirMaskExpression = "!cloud_flags_nadir.LAND and !cloud_flags_nadir.CLOUDY and !cloud_flags_nadir.SUN_GLINT",
    invalidSstValue = "-999.0f") {
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
  xml_add_child(node, "operator", "Aatsr.SST")
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
    "dual",
    gpt_args$dual
  )
  xml_add_child(
    parameters,
    "dualCoefficientsFile",
    gpt_args$dualCoefficientsFile
  )
  xml_add_child(
    parameters,
    "dualMaskExpression",
    gpt_args$dualMaskExpression
  )
  xml_add_child(
    parameters,
    "nadir",
    gpt_args$nadir
  )
  xml_add_child(
    parameters,
    "nadirCoefficientsFile",
    gpt_args$nadirCoefficientsFile
  )
  xml_add_child(
    parameters,
    "nadirMaskExpression",
    gpt_args$nadirMaskExpression
  )
  xml_add_child(
    parameters,
    "invalidSstValue",
    gpt_args$invalidSstValue
  )
  operator_sources <- gpt_args[c("sourceProduct")]
  snap_op_aatsr_sst <- S7::new_class(
    "snap_op_aatsr_sst",
    parent = snap_operator
  )
  snap_op_aatsr_sst(
    operator = "Aatsr.SST",
    operator_id = operator_id,
    operator_sources = operator_sources,
    created_with = rlang::current_fn(),
    xml_graph = as.character(op_xml)
  )
}
