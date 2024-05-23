#' Meris.CorrectRadiometry: snap operator function
#' @param operator_id character operator id
#' @param sourceProduct The source product(s) as input to the operator
#' @param doCalibration Whether to perform the calibration. Default value is
#' 'true'.
#' @param sourceRacFile The radiometric correction auxiliary file for the
#' source product. The default
#' 'MER_RAC_AXVIEC20050708_135553_20021224_121445_20041213_220000'
#' @param targetRacFile The radiometric correction auxiliary file for the
#' target product. The default
#' 'MER_RAC_AXVACR20091016_154511_20021224_121445_20041213_220000'
#' @param doSmile Whether to perform Smile-effect correction. Default value is
#' 'true'.
#' @param doEqualization Perform removal of detector-to-detector systematic
#' radiometric differences in MERIS L1b data products. Default value is 'true'.
#' @param reproVersion The version of the reprocessing the product comes from.
#' Is only used if equalisation is enabled. Value must be one of 'AUTO_DETECT',
#' 'REPROCESSING_2', 'REPROCESSING_3'. Default value is 'AUTO_DETECT'.
#' @param doRadToRefl Whether to perform radiance-to-reflectance conversion.
#' When selecting ENVISAT as target format, the radiance to reflectance conversion
#' can not be performed. Default value is 'false'.
#' @details
#' Descrscription from '`gpt {operator} -h`':
#'
#' "Performs radiometric corrections on MERIS L1b data products."
#' @import xml2
#' @return snap_op_meris_correct_radiometry object
#' @export
op_meris_correct_radiometry <- function(
    operator_id,
    sourceProduct,
    doCalibration = TRUE,
    sourceRacFile = NULL,
    targetRacFile = NULL,
    doSmile = TRUE,
    doEqualization = TRUE,
    reproVersion = "AUTO_DETECT",
    doRadToRefl = FALSE) {
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
  xml_add_child(node, "operator", "Meris.CorrectRadiometry")
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
    "doCalibration",
    gpt_args$doCalibration
  )
  xml_add_child(
    parameters,
    "sourceRacFile",
    gpt_args$sourceRacFile
  )
  xml_add_child(
    parameters,
    "targetRacFile",
    gpt_args$targetRacFile
  )
  xml_add_child(
    parameters,
    "doSmile",
    gpt_args$doSmile
  )
  xml_add_child(
    parameters,
    "doEqualization",
    gpt_args$doEqualization
  )
  xml_add_child(
    parameters,
    "reproVersion",
    gpt_args$reproVersion
  )
  xml_add_child(
    parameters,
    "doRadToRefl",
    gpt_args$doRadToRefl
  )
  operator_sources <- gpt_args[c("sourceProduct")]
  snap_op_meris_correct_radiometry <- S7::new_class(
    "snap_op_meris_correct_radiometry",
    parent = snap_operator
  )
  snap_op_meris_correct_radiometry(
    operator = "Meris.CorrectRadiometry",
    operator_id = operator_id,
    operator_sources = operator_sources,
    created_with = rlang::current_fn(),
    xml_graph = as.character(op_xml)
  )
}
