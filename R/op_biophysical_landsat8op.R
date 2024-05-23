#' BiophysicalLandsat8Op: snap operator function
#' @param operator_id character operator id
#' @param sourceProduct The source product(s) as input to the operator
#' @param computeLAI Compute LAI (Leaf Area Index) Default value is 'true'.
#' @param computeFapar Compute FAPAR (Fraction of Absorbed Photosynthetically
#' Active Radiation) Default value is 'true'.
#' @param computeFcover Compute FVC (Fraction of Vegetation Cover) Default
#' value is 'true'.
#' @details
#' Descrscription from '`gpt {operator} -h`':
#'
#' "The 'Biophysical Processor' operator retrieves LAI from atmospherically
#' corrected Landsat8 products"
#' @import xml2
#' @return snap_op_biophysical_landsat8op object
#' @export
op_biophysical_landsat8op <- function(
    operator_id,
    sourceProduct,
    computeLAI = TRUE,
    computeFapar = TRUE,
    computeFcover = TRUE) {
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
  xml_add_child(node, "operator", "BiophysicalLandsat8Op")
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
    "computeLAI",
    gpt_args$computeLAI
  )
  xml_add_child(
    parameters,
    "computeFapar",
    gpt_args$computeFapar
  )
  xml_add_child(
    parameters,
    "computeFcover",
    gpt_args$computeFcover
  )
  operator_sources <- gpt_args[c("sourceProduct")]
  snap_op_biophysical_landsat8op <- S7::new_class(
    "snap_op_biophysical_landsat8op",
    parent = snap_operator
  )
  snap_op_biophysical_landsat8op(
    operator = "BiophysicalLandsat8Op",
    operator_id = operator_id,
    operator_sources = operator_sources,
    created_with = rlang::current_fn(),
    xml_graph = as.character(op_xml)
  )
}
