#' OlciSensorHarmonisation: snap operator function
#' @param operator_id character operator id
#' @param l1bProduct OLCI L1b or fully compatible product. This is a mandatory
#' source.
#' @param performSensorCrossCalibration If set to true, in addition to the
#' camera homogenisation, sensor cross-calibration (i.e. S3A->S3B or S3B->S3A) is
#' performed using linear regression Default value is 'false'.
#' @param copyInputBands If set to true, all bands of the input product (except
#' for the radiances) are copied to the target product. If set to false, only the
#' tie-point rasters are copied Default value is 'false'.
#' @details
#' Descrscription from '`gpt {operator} -h`':
#'
#' "Performs sensor harmonisation on OLCI L1b product. Implements algorithm
#' described in 'OLCI A/B Tandem Phase Analysis'"
#' @import xml2
#' @return snap_op_olci_sensor_harmonisation object
#' @export
op_olci_sensor_harmonisation <- function(
    operator_id,
    l1bProduct = NULL,
    performSensorCrossCalibration = FALSE,
    copyInputBands = FALSE) {
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
  xml_add_child(node, "operator", "OlciSensorHarmonisation")
  sources <- xml_add_child(node, "sources")
  xml_add_child(
    sources,
    "l1bProduct",
    gpt_args$l1bProduct
  )
  parameters <- xml_add_child(node, "parameters")
  xml_add_child(
    parameters,
    "performSensorCrossCalibration",
    gpt_args$performSensorCrossCalibration
  )
  xml_add_child(
    parameters,
    "copyInputBands",
    gpt_args$copyInputBands
  )
  operator_sources <- gpt_args[c("l1bProduct")]
  snap_op_olci_sensor_harmonisation <- S7::new_class(
    "snap_op_olci_sensor_harmonisation",
    parent = snap_operator
  )
  snap_op_olci_sensor_harmonisation(
    operator = "OlciSensorHarmonisation",
    operator_id = operator_id,
    operator_sources = operator_sources,
    created_with = rlang::current_fn(),
    xml_graph = as.character(op_xml)
  )
}
