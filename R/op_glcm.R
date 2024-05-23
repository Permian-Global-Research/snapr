#' GLCM: snap operator function
#' @param operator_id character operator id
#' @param sourceProduct The source product(s) as input to the operator
#' @param sourceBands The list of source bands.
#' @param windowSizeStr Sets parameter 'windowSizeStr' to <string>. Value must
#' be one of '5x5', '7x7', '9x9', '11x11', '13x13', '15x15', '17x17', '21x21'.
#' Default value is '9x9'.
#' @param angleStr Sets parameter 'angleStr' to <string>. Value must be one of
#' '0', '45', '90', '135', 'ALL'. Default value is 'ALL'.
#' @param quantizerStr Sets parameter 'quantizerStr' to <string>. Value must be
#' one of 'Equal Distance Quantizer', 'Probabilistic Quantizer'. Default value is
#' 'Probabilistic Quantizer'.
#' @param quantizationLevelsStr Sets parameter 'quantizationLevelsStr' to
#' <string>. Value must be one of '8', '16', '32', '64', '128'. Default value is
#' '32'.
#' @param displacement Pixel displacement Valid interval is \[1, 8\]. Default
#' value is '4'.
#' @param noDataValue Target product no data value Default value is '-9999.0'.
#' @param outputContrast Output Contrast Default value is 'true'.
#' @param outputDissimilarity Output Dissimilarity Default value is 'true'.
#' @param outputHomogeneity Output Homogeneity Default value is 'true'.
#' @param outputASM Output Angular Second Moment Default value is 'true'.
#' @param outputEnergy Output Energy Default value is 'true'.
#' @param outputMAX Output Maximum Probability Default value is 'true'.
#' @param outputEntropy Output Entropy Default value is 'true'.
#' @param outputMean Output GLCM Mean Default value is 'true'.
#' @param outputVariance Output GLCM Variance Default value is 'true'.
#' @param outputCorrelation Output GLCM Correlation Default value is 'true'.
#' @details
#' Descrscription from '`gpt {operator} -h`':
#'
#' "Extract Texture Features"
#' @import xml2
#' @return snap_op_glcm object
#' @export
op_glcm <- function(
    operator_id,
    sourceProduct,
    sourceBands = NULL,
    windowSizeStr = "9x9",
    angleStr = "ALL",
    quantizerStr = "Probabilistic Quantizer",
    quantizationLevelsStr = 32,
    displacement = 4,
    noDataValue = -9999.0,
    outputContrast = TRUE,
    outputDissimilarity = TRUE,
    outputHomogeneity = TRUE,
    outputASM = TRUE,
    outputEnergy = TRUE,
    outputMAX = TRUE,
    outputEntropy = TRUE,
    outputMean = TRUE,
    outputVariance = TRUE,
    outputCorrelation = TRUE) {
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
  xml_add_child(node, "operator", "GLCM")
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
    "sourceBands",
    gpt_args$sourceBands
  )
  xml_add_child(
    parameters,
    "windowSizeStr",
    gpt_args$windowSizeStr
  )
  xml_add_child(
    parameters,
    "angleStr",
    gpt_args$angleStr
  )
  xml_add_child(
    parameters,
    "quantizerStr",
    gpt_args$quantizerStr
  )
  xml_add_child(
    parameters,
    "quantizationLevelsStr",
    gpt_args$quantizationLevelsStr
  )
  xml_add_child(
    parameters,
    "displacement",
    gpt_args$displacement
  )
  xml_add_child(
    parameters,
    "noDataValue",
    gpt_args$noDataValue
  )
  xml_add_child(
    parameters,
    "outputContrast",
    gpt_args$outputContrast
  )
  xml_add_child(
    parameters,
    "outputDissimilarity",
    gpt_args$outputDissimilarity
  )
  xml_add_child(
    parameters,
    "outputHomogeneity",
    gpt_args$outputHomogeneity
  )
  xml_add_child(
    parameters,
    "outputASM",
    gpt_args$outputASM
  )
  xml_add_child(
    parameters,
    "outputEnergy",
    gpt_args$outputEnergy
  )
  xml_add_child(
    parameters,
    "outputMAX",
    gpt_args$outputMAX
  )
  xml_add_child(
    parameters,
    "outputEntropy",
    gpt_args$outputEntropy
  )
  xml_add_child(
    parameters,
    "outputMean",
    gpt_args$outputMean
  )
  xml_add_child(
    parameters,
    "outputVariance",
    gpt_args$outputVariance
  )
  xml_add_child(
    parameters,
    "outputCorrelation",
    gpt_args$outputCorrelation
  )
  operator_sources <- gpt_args[c("sourceProduct")]
  snap_op_glcm <- S7::new_class(
    "snap_op_glcm",
    parent = snap_operator
  )
  snap_op_glcm(
    operator = "GLCM",
    operator_id = operator_id,
    operator_sources = operator_sources,
    created_with = rlang::current_fn(),
    xml_graph = as.character(op_xml)
  )
}
