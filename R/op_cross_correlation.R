#' Cross-Correlation: snap operator function
#' @param operator_id character operator id
#' @param sourceProduct The source product(s) as input to the operator
#' @param numGCPtoGenerate The number of GCPs to use in a grid Valid interval
#' is (10, *). Default value is '2000'.
#' @param coarseRegistrationWindowWidth Sets parameter
#' 'coarseRegistrationWindowWidth' to <string>. Value must be one of '32', '64',
#' '128', '256', '512', '1024', '2048'. Default value is '128'.
#' @param coarseRegistrationWindowHeight Sets parameter
#' 'coarseRegistrationWindowHeight' to <string>. Value must be one of '32', '64',
#' '128', '256', '512', '1024', '2048'. Default value is '128'.
#' @param rowInterpFactor Sets parameter 'rowInterpFactor' to <string>. Value
#' must be one of '2', '4', '8', '16'. Default value is '2'.
#' @param columnInterpFactor Sets parameter 'columnInterpFactor' to <string>.
#' Value must be one of '2', '4', '8', '16'. Default value is '2'.
#' @param maxIteration The maximum number of iterations Valid interval is (1,
#' 10\]. Default value is '10'.
#' @param gcpTolerance Tolerance in slave GCP validation check Valid interval
#' is (0, *). Default value is '0.5'.
#' @param applyFineRegistration Sets parameter 'applyFineRegistration' to
#' <boolean>. Default value is 'false'.
#' @param inSAROptimized Sets parameter 'inSAROptimized' to <boolean>. Default
#' value is 'false'.
#' @param fineRegistrationWindowWidth Sets parameter
#' 'fineRegistrationWindowWidth' to <string>. Value must be one of '8', '16',
#' '32', '64', '128', '256', '512'. Default value is '32'.
#' @param fineRegistrationWindowHeight Sets parameter
#' 'fineRegistrationWindowHeight' to <string>. Value must be one of '8', '16',
#' '32', '64', '128', '256', '512'. Default value is '32'.
#' @param fineRegistrationWindowAccAzimuth Sets parameter
#' 'fineRegistrationWindowAccAzimuth' to <string>. Value must be one of '2', '4',
#' '8', '16', '32', '64'. Default value is '16'.
#' @param fineRegistrationWindowAccRange Sets parameter
#' 'fineRegistrationWindowAccRange' to <string>. Value must be one of '2', '4',
#' '8', '16', '32', '64'. Default value is '16'.
#' @param fineRegistrationOversampling Sets parameter
#' 'fineRegistrationOversampling' to <string>. Value must be one of '2', '4', '8',
#' '16', '32', '64'. Default value is '16'.
#' @param coherenceWindowSize The coherence window size Valid interval is (1,
#' 16\]. Default value is '3'.
#' @param coherenceThreshold The coherence threshold Valid interval is (0, *).
#' Default value is '0.6'.
#' @param useSlidingWindow Use sliding window for coherence calculation Default
#' value is 'false'.
#' @param computeOffset Sets parameter 'computeOffset' to <boolean>. Default
#' value is 'false'.
#' @param onlyGCPsOnLand Sets parameter 'onlyGCPsOnLand' to <boolean>. Default
#' value is 'false'.
#' @details
#' Descrscription from '`gpt {operator} -h`':
#'
#' "Automatic Selection of Ground Control Points"
#' @import xml2
#' @return snap_op_cross_correlation object
#' @export
op_cross_correlation <- function(
    operator_id,
    sourceProduct,
    numGCPtoGenerate = 2000,
    coarseRegistrationWindowWidth = 128,
    coarseRegistrationWindowHeight = 128,
    rowInterpFactor = 2,
    columnInterpFactor = 2,
    maxIteration = 10,
    gcpTolerance = 0.5,
    applyFineRegistration = FALSE,
    inSAROptimized = FALSE,
    fineRegistrationWindowWidth = 32,
    fineRegistrationWindowHeight = 32,
    fineRegistrationWindowAccAzimuth = 16,
    fineRegistrationWindowAccRange = 16,
    fineRegistrationOversampling = 16,
    coherenceWindowSize = 3,
    coherenceThreshold = 0.6,
    useSlidingWindow = FALSE,
    computeOffset = FALSE,
    onlyGCPsOnLand = FALSE) {
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
  xml_add_child(node, "operator", "Cross-Correlation")
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
    "numGCPtoGenerate",
    gpt_args$numGCPtoGenerate
  )
  xml_add_child(
    parameters,
    "coarseRegistrationWindowWidth",
    gpt_args$coarseRegistrationWindowWidth
  )
  xml_add_child(
    parameters,
    "coarseRegistrationWindowHeight",
    gpt_args$coarseRegistrationWindowHeight
  )
  xml_add_child(
    parameters,
    "rowInterpFactor",
    gpt_args$rowInterpFactor
  )
  xml_add_child(
    parameters,
    "columnInterpFactor",
    gpt_args$columnInterpFactor
  )
  xml_add_child(
    parameters,
    "maxIteration",
    gpt_args$maxIteration
  )
  xml_add_child(
    parameters,
    "gcpTolerance",
    gpt_args$gcpTolerance
  )
  xml_add_child(
    parameters,
    "applyFineRegistration",
    gpt_args$applyFineRegistration
  )
  xml_add_child(
    parameters,
    "inSAROptimized",
    gpt_args$inSAROptimized
  )
  xml_add_child(
    parameters,
    "fineRegistrationWindowWidth",
    gpt_args$fineRegistrationWindowWidth
  )
  xml_add_child(
    parameters,
    "fineRegistrationWindowHeight",
    gpt_args$fineRegistrationWindowHeight
  )
  xml_add_child(
    parameters,
    "fineRegistrationWindowAccAzimuth",
    gpt_args$fineRegistrationWindowAccAzimuth
  )
  xml_add_child(
    parameters,
    "fineRegistrationWindowAccRange",
    gpt_args$fineRegistrationWindowAccRange
  )
  xml_add_child(
    parameters,
    "fineRegistrationOversampling",
    gpt_args$fineRegistrationOversampling
  )
  xml_add_child(
    parameters,
    "coherenceWindowSize",
    gpt_args$coherenceWindowSize
  )
  xml_add_child(
    parameters,
    "coherenceThreshold",
    gpt_args$coherenceThreshold
  )
  xml_add_child(
    parameters,
    "useSlidingWindow",
    gpt_args$useSlidingWindow
  )
  xml_add_child(
    parameters,
    "computeOffset",
    gpt_args$computeOffset
  )
  xml_add_child(
    parameters,
    "onlyGCPsOnLand",
    gpt_args$onlyGCPsOnLand
  )
  operator_sources <- gpt_args[c("sourceProduct")]
  snap_op_cross_correlation <- S7::new_class(
    "snap_op_cross_correlation",
    parent = snap_operator
  )
  snap_op_cross_correlation(
    operator = "Cross-Correlation",
    operator_id = operator_id,
    operator_sources = operator_sources,
    created_with = rlang::current_fn(),
    xml_graph = as.character(op_xml)
  )
}
