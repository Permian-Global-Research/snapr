#' Arc.SST: snap operator function
#' @param operator_id character operator id
#' @param sourceProduct The source product(s) as input to the operator
#' @param tcwvExpression TCWV value to use in SST retrieval Default value is
#' '30.0'.
#' @param asdi Enables/disables generation of ATSR Saharan Dust Index Default
#' value is 'true'.
#' @param asdiCoefficientsFile Coefficient file for ASDI Value must be one of
#' 'ASDI_ATSR1', 'ASDI_ATSR2', 'ASDI_AATSR'. Default value is 'ASDI_AATSR'.
#' @param asdiMaskExpression ROI-mask used for the ASDI
#' @param dual Enables/disables generation of the dual-view SST Default value
#' is 'true'.
#' @param dualCoefficientsFile Coefficient file for the dual-view SST Value
#' must be one of 'ARC_D2_ATSR1', 'ARC_D2_ATSR2', 'ARC_D2_AATSR', 'ARC_D2_SLSTR',
#' 'ARC_D3_ATSR1', 'ARC_D3_ATSR2', 'ARC_D3_AATSR', 'ARC_D3_SLSTR'. Default value
#' is 'ARC_D2_AATSR'.
#' @param dualMaskExpression ROI-mask used for the dual-view SST
#' @param nadir Enables/disables generation of the nadir-view SST Default value
#' is 'true'.
#' @param nadirCoefficientsFile Coefficient file for the nadir-view SST Value
#' must be one of 'ARC_N2_ATSR1', 'ARC_N2_ATSR2', 'ARC_N2_AATSR', 'ARC_N2_SLSTR',
#' 'ARC_N3_ATSR1', 'ARC_N3_ATSR2', 'ARC_N3_AATSR', 'ARC_N3_SLSTR'. Default value
#' is 'ARC_N2_AATSR'.
#' @param nadirMaskExpression ROI-mask used for the nadir-view SST
#' @param invalidSstValue Value used to fill invalid SST pixels Default value
#' is '-999.0f'.
#' @details
#' Descrscription from '`gpt {operator} -h`':
#'
#' "Computes sea surface temperature (SST) from (A)ATSR and SLSTR products."
#' @import xml2
#' @return snap_op_arc_sst object
#' @export
op_arc_sst <- function(
    operator_id,
    sourceProduct,
    tcwvExpression = 30.0,
    asdi = TRUE,
    asdiCoefficientsFile = "ASDI_AATSR",
    asdiMaskExpression = NULL,
    dual = TRUE,
    dualCoefficientsFile = "ARC_D2_AATSR",
    dualMaskExpression = NULL,
    nadir = TRUE,
    nadirCoefficientsFile = "ARC_N2_AATSR",
    nadirMaskExpression = NULL,
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
  xml_add_child(node, "operator", "Arc.SST")
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
    "tcwvExpression",
    gpt_args$tcwvExpression
  )
  xml_add_child(
    parameters,
    "asdi",
    gpt_args$asdi
  )
  xml_add_child(
    parameters,
    "asdiCoefficientsFile",
    gpt_args$asdiCoefficientsFile
  )
  xml_add_child(
    parameters,
    "asdiMaskExpression",
    gpt_args$asdiMaskExpression
  )
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
  snap_op_arc_sst <- S7::new_class(
    "snap_op_arc_sst",
    parent = snap_operator
  )
  snap_op_arc_sst(
    operator = "Arc.SST",
    operator_id = operator_id,
    operator_sources = operator_sources,
    created_with = rlang::current_fn(),
    xml_graph = as.character(op_xml)
  )
}
