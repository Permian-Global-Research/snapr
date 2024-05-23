#' FUB.Water: snap operator function
#' @param operator_id character operator id
#' @param sourceProduct The source product(s) as input to the operator
#' @param computeCHL Whether chlorophyll-a concentration band shall be computed
#' Default value is 'true'.
#' @param computeYS Whether yellow substances band shall be computed Default
#' value is 'true'.
#' @param computeTSM Whether total suspended matter band shall be computed
#' Default value is 'true'.
#' @param computeAtmCorr Whether atmospheric correction bands shall be computed
#' Default value is 'true'.
#' @param checkWhetherSuspectIsValid Expert parameter. Performs a check whether
#' the 'l1_flags.SUSPECT' shall be considered in an expression.This parameter is
#' only considered when the expression contains the term 'and not
#' l1_flags.SUSPECT' Default value is 'true'.
#' @param expression Band maths expression which defines valid pixels. If the
#' expression is empty,all pixels will be considered. Default value is 'not
#' l1_flags.GLINT_RISK and not l1_flags.BRIGHT and not l1_flags.INVALID and not
#' l1_flags.SUSPECT'.
#' @details
#' Descrscription from '`gpt {operator} -h`':
#'
#' "MERIS FUB-CSIRO Coastal Water Processor to retrieve case II water
#' properties and atmospheric properties"
#' @import xml2
#' @return snap_op_fub_water object
#' @export
op_fub_water <- function(
    operator_id,
    sourceProduct,
    computeCHL = TRUE,
    computeYS = TRUE,
    computeTSM = TRUE,
    computeAtmCorr = TRUE,
    checkWhetherSuspectIsValid = TRUE,
    expression = "not l1_flags.GLINT_RISK and not l1_flags.BRIGHT and not l1_flags.INVALID and not l1_flags.SUSPECT") {
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
  xml_add_child(node, "operator", "FUB.Water")
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
    "computeCHL",
    gpt_args$computeCHL
  )
  xml_add_child(
    parameters,
    "computeYS",
    gpt_args$computeYS
  )
  xml_add_child(
    parameters,
    "computeTSM",
    gpt_args$computeTSM
  )
  xml_add_child(
    parameters,
    "computeAtmCorr",
    gpt_args$computeAtmCorr
  )
  xml_add_child(
    parameters,
    "checkWhetherSuspectIsValid",
    gpt_args$checkWhetherSuspectIsValid
  )
  xml_add_child(
    parameters,
    "expression",
    gpt_args$expression
  )
  operator_sources <- gpt_args[c("sourceProduct")]
  snap_op_fub_water <- S7::new_class(
    "snap_op_fub_water",
    parent = snap_operator
  )
  snap_op_fub_water(
    operator = "FUB.Water",
    operator_id = operator_id,
    operator_sources = operator_sources,
    created_with = rlang::current_fn(),
    xml_graph = as.character(op_xml)
  )
}
