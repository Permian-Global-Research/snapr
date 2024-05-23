#' CreateStack: snap operator function
#' @param operator_id character operator id
#' @param sourceProduct The source product(s) as input to the operator
#' @param resamplingType The method to be used when resampling the slave grid
#' onto the master grid. Default value is 'NONE'.
#' @param extent The output image extents. Value must be one of 'Master',
#' 'Minimum', 'Maximum'. Default value is 'Master'.
#' @param initialOffsetMethod Method for computing initial offset between
#' master and slave Value must be one of 'Orbit', 'Product Geolocation'. Default
#' value is 'Orbit'.
#' @param masterBands The list of source bands.
#' @param sourceBands The list of source bands.
#' @details
#' Descrscription from '`gpt {operator} -h`':
#'
#' "Collocates two or more products based on their geo-codings. Parameter
#' Options: -Pextent=<string> The output image extents. Value must be one of
#' 'Master', 'Minimum', 'Maximum'. Default value is 'Master'.
#' -PinitialOffsetMethod=<string> Method for computing initial offset between
#' master and slave Value must be one of 'Orbit', 'Product Geolocation'. Default
#' value is 'Orbit'. -PmasterBands=<string,string,string,...> The list of source
#' bands. -PresamplingType=<string> The method to be used when resampling the
#' slave grid onto the master grid. Default value is 'NONE'.
#' -PsourceBands=<string,string,string,...> The list of source bands."
#' @import xml2
#' @return snap_op_create_stack object
#' @export
op_create_stack <- function(
    operator_id,
    sourceProduct,
    resamplingType = "NONE",
    extent = "Master",
    initialOffsetMethod = "Orbit",
    masterBands = NULL,
    sourceBands = NULL) {
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
  xml_add_child(node, "operator", "CreateStack")
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
    "resamplingType",
    gpt_args$resamplingType
  )
  xml_add_child(
    parameters,
    "extent",
    gpt_args$extent
  )
  xml_add_child(
    parameters,
    "initialOffsetMethod",
    gpt_args$initialOffsetMethod
  )
  xml_add_child(
    parameters,
    "masterBands",
    gpt_args$masterBands
  )
  xml_add_child(
    parameters,
    "sourceBands",
    gpt_args$sourceBands
  )
  operator_sources <- gpt_args[c("sourceProduct")]
  snap_op_create_stack <- S7::new_class(
    "snap_op_create_stack",
    parent = snap_operator
  )
  snap_op_create_stack(
    operator = "CreateStack",
    operator_id = operator_id,
    operator_sources = operator_sources,
    created_with = rlang::current_fn(),
    xml_graph = as.character(op_xml)
  )
}
