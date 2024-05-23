#' ReflectanceToRadianceOp: snap operator function
#' @param operator_id character operator id
#' @param sourceProduct The source product(s) as input to the operator
#' @param solarIrradiance The solar irradiance.
#' @param u U
#' @param incidenceAngle The incidence angle in degrees.
#' @param sourceBands The source bands for the computation.
#' @param copyMasks Copy masks from the source product Default value is
#' 'false'.
#' @details
#' Descrscription from '`gpt {operator} -h`':
#'
#' "The 'Reflectance To Radiance Processor' operator retrieves the radiance
#' from reflectance using Sentinel-2 products"
#' @import xml2
#' @return snap_op_reflectance_to_radiance_op object
#' @export
op_reflectance_to_radiance_op <- function(
    operator_id,
    sourceProduct,
    solarIrradiance = NULL,
    u = NULL,
    incidenceAngle = NULL,
    sourceBands = NULL,
    copyMasks = FALSE) {
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
  xml_add_child(node, "operator", "ReflectanceToRadianceOp")
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
    "solarIrradiance",
    gpt_args$solarIrradiance
  )
  xml_add_child(
    parameters,
    "u",
    gpt_args$u
  )
  xml_add_child(
    parameters,
    "incidenceAngle",
    gpt_args$incidenceAngle
  )
  xml_add_child(
    parameters,
    "sourceBands",
    gpt_args$sourceBands
  )
  xml_add_child(
    parameters,
    "copyMasks",
    gpt_args$copyMasks
  )
  operator_sources <- gpt_args[c("sourceProduct")]
  snap_op_reflectance_to_radiance_op <- S7::new_class(
    "snap_op_reflectance_to_radiance_op",
    parent = snap_operator
  )
  snap_op_reflectance_to_radiance_op(
    operator = "ReflectanceToRadianceOp",
    operator_id = operator_id,
    operator_sources = operator_sources,
    created_with = rlang::current_fn(),
    xml_graph = as.character(op_xml)
  )
}
