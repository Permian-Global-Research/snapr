#' FuClassification: snap operator function
#' @param operator_id character operator id
#' @param sourceProduct The source product(s) as input to the operator
#' @param copyAllSourceBands Weather or not to copy all the bands to the target
#' product from the source product. Default value is 'false'.
#' @param inputIsIrradianceReflectance If enabled, the source reflectances will
#' be converted to radiance reflectances by dividing it by PI before passing to
#' the algorithm. Default value is 'false'.
#' @param validExpression An expression to filter which pixel are considered.
#' @param reflectanceNamePattern The used reflectance band names must match the
#' given pattern. Useful, if there is more then one spectrum in the product.
#' @param instrument The instrument to compute FU for. Default value is
#' 'AUTO_DETECT'.
#' @param includeDominantLambda Whether or not the dominant wavelength shall be
#' derived from the hue angle Default value is 'false'.
#' @param includeIntermediateResults Whether or not the intermediate results
#' shall be written to the target output Default value is 'true'.
#' @details
#' Descrscription from '`gpt {operator} -h`':
#'
#' "Colour classification based on the discrete Forel-Ule scale"
#' @import xml2
#' @return snap_op_fu_classification object
#' @export
op_fu_classification <- function(
    operator_id,
    sourceProduct,
    copyAllSourceBands = FALSE,
    inputIsIrradianceReflectance = FALSE,
    validExpression = NULL,
    reflectanceNamePattern = NULL,
    instrument = "AUTO_DETECT",
    includeDominantLambda = FALSE,
    includeIntermediateResults = TRUE) {
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
  xml_add_child(node, "operator", "FuClassification")
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
    "copyAllSourceBands",
    gpt_args$copyAllSourceBands
  )
  xml_add_child(
    parameters,
    "inputIsIrradianceReflectance",
    gpt_args$inputIsIrradianceReflectance
  )
  xml_add_child(
    parameters,
    "validExpression",
    gpt_args$validExpression
  )
  xml_add_child(
    parameters,
    "reflectanceNamePattern",
    gpt_args$reflectanceNamePattern
  )
  xml_add_child(
    parameters,
    "instrument",
    gpt_args$instrument
  )
  xml_add_child(
    parameters,
    "includeDominantLambda",
    gpt_args$includeDominantLambda
  )
  xml_add_child(
    parameters,
    "includeIntermediateResults",
    gpt_args$includeIntermediateResults
  )
  operator_sources <- gpt_args[c("sourceProduct")]
  snap_op_fu_classification <- S7::new_class(
    "snap_op_fu_classification",
    parent = snap_operator
  )
  snap_op_fu_classification(
    operator = "FuClassification",
    operator_id = operator_id,
    operator_sources = operator_sources,
    created_with = rlang::current_fn(),
    xml_graph = as.character(op_xml)
  )
}
