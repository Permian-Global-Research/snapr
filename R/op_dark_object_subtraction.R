#' DarkObjectSubtraction: snap operator function
#' @param operator_id character operator id
#' @param sourceProduct The source product(s) as input to the operator
#' @param maskExpression Mask expression defining search area for dark object.
#' @param histogramMinimumPercentile Percentile of minimum in image data in
#' percent (the number means how many percent of the image data are lower than
#' detected minimum. Value must be one of '0', '1', '5'.
#' @param sourceBandNames Bands to be copied to the target. DOS will be applied
#' on spectral bands only.
#' @details
#' Descrscription from '`gpt {operator} -h`':
#'
#' "Performs dark object subtraction for spectral bands in source product."
#' @import xml2
#' @return snap_op_dark_object_subtraction object
#' @export
op_dark_object_subtraction <- function(
    operator_id,
    sourceProduct,
    maskExpression = NULL,
    histogramMinimumPercentile = NULL,
    sourceBandNames = NULL) {
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
  xml_add_child(node, "operator", "DarkObjectSubtraction")
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
    "maskExpression",
    gpt_args$maskExpression
  )
  xml_add_child(
    parameters,
    "histogramMinimumPercentile",
    gpt_args$histogramMinimumPercentile
  )
  xml_add_child(
    parameters,
    "sourceBandNames",
    gpt_args$sourceBandNames
  )
  operator_sources <- gpt_args[c("sourceProduct")]
  snap_op_dark_object_subtraction <- S7::new_class(
    "snap_op_dark_object_subtraction",
    parent = snap_operator
  )
  snap_op_dark_object_subtraction(
    operator = "DarkObjectSubtraction",
    operator_id = operator_id,
    operator_sources = operator_sources,
    created_with = rlang::current_fn(),
    xml_graph = as.character(op_xml)
  )
}
