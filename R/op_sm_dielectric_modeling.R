#' SM-Dielectric-Modeling: snap operator function
#' @param operator_id character operator id
#' @param sourceProduct The source product(s) as input to the operator
#' @param modelToUse Choice of dielectric models for SM inversion Value must be
#' one of 'Hallikainen', 'Mironov'. Default value is 'Hallikainen'.
#' @param minSM Minimum soil moisture value Valid interval is \[0.0, 1.0).
#' Default value is '0.0'. Parameter unit is 'm^3/m^3'.
#' @param maxSM Maximum soil moisture value Valid interval is (0.0, 1.0\].
#' Default value is '0.55'. Parameter unit is 'm^3/m^3'.
#' @param outputRDC Optional RDC in output Default value is 'true'.
#' @param outputLandCover Optional LandCover in output Default value is 'true'.
#' @param effectiveSoilTemperature Effective soil temperature Valid interval is
#' \[-50.0, 50.0\]. Default value is '18.0'. Parameter unit is 'Celsius'.
#' @details
#' Descrscription from '`gpt {operator} -h`':
#'
#' "Performs SM inversion using dielectric model"
#' @import xml2
#' @return snap_op_sm_dielectric_modeling object
#' @export
op_sm_dielectric_modeling <- function(
    operator_id,
    sourceProduct,
    modelToUse = "Hallikainen",
    minSM = 0.0,
    maxSM = 0.55,
    outputRDC = TRUE,
    outputLandCover = TRUE,
    effectiveSoilTemperature = 18.0) {
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
  xml_add_child(node, "operator", "SM-Dielectric-Modeling")
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
    "modelToUse",
    gpt_args$modelToUse
  )
  xml_add_child(
    parameters,
    "minSM",
    gpt_args$minSM
  )
  xml_add_child(
    parameters,
    "maxSM",
    gpt_args$maxSM
  )
  xml_add_child(
    parameters,
    "outputRDC",
    gpt_args$outputRDC
  )
  xml_add_child(
    parameters,
    "outputLandCover",
    gpt_args$outputLandCover
  )
  xml_add_child(
    parameters,
    "effectiveSoilTemperature",
    gpt_args$effectiveSoilTemperature
  )
  operator_sources <- gpt_args[c("sourceProduct")]
  snap_op_sm_dielectric_modeling <- S7::new_class(
    "snap_op_sm_dielectric_modeling",
    parent = snap_operator
  )
  snap_op_sm_dielectric_modeling(
    operator = "SM-Dielectric-Modeling",
    operator_id = operator_id,
    operator_sources = operator_sources,
    created_with = rlang::current_fn(),
    xml_graph = as.character(op_xml)
  )
}
