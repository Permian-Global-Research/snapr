#' OWTClassification: snap operator function
#' @param operator_id character operator id
#' @param sourceProduct The source product(s) as input to the operator
#' @param owtType Sets parameter 'owtType' to <oWT_TYPE>. Default value is
#' 'COASTAL'.
#' @param reflectancesPrefix Sets parameter 'reflectancesPrefix' to <string>.
#' Default value is 'reflec'.
#' @param inputReflectanceIs Sets parameter 'inputReflectanceIs' to
#' <reflectanceEnum>. Default value is 'RADIANCE_REFLECTANCES'.
#' @param writeInputReflectances Sets parameter 'writeInputReflectances' to
#' <boolean>. Default value is 'false'.
#' @details
#' Descrscription from '`gpt {operator} -h`':
#'
#' "Performs an optical water type classification based on atmospherically
#' corrected reflectances."
#' @import xml2
#' @return snap_op_owtclassification object
#' @export
op_owtclassification <- function(
    operator_id,
    sourceProduct,
    owtType = "COASTAL",
    reflectancesPrefix = "reflec",
    inputReflectanceIs = "RADIANCE_REFLECTANCES",
    writeInputReflectances = FALSE) {
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
  xml_add_child(node, "operator", "OWTClassification")
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
    "owtType",
    gpt_args$owtType
  )
  xml_add_child(
    parameters,
    "reflectancesPrefix",
    gpt_args$reflectancesPrefix
  )
  xml_add_child(
    parameters,
    "inputReflectanceIs",
    gpt_args$inputReflectanceIs
  )
  xml_add_child(
    parameters,
    "writeInputReflectances",
    gpt_args$writeInputReflectances
  )
  operator_sources <- gpt_args[c("sourceProduct")]
  snap_op_owtclassification <- S7::new_class(
    "snap_op_owtclassification",
    parent = snap_operator
  )
  snap_op_owtclassification(
    operator = "OWTClassification",
    operator_id = operator_id,
    operator_sources = operator_sources,
    created_with = rlang::current_fn(),
    xml_graph = as.character(op_xml)
  )
}
