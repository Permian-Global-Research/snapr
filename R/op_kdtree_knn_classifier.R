#' KDTree-KNN-Classifier: snap operator function
#' @param operator_id character operator id
#' @param sourceProduct The source product(s) as input to the operator
#' @param numNeighbours The number of neighbours Valid interval is (1,*\].
#' Default value is '5'.
#' @param numTrainSamples The number of training samples Valid interval is
#' (1,*\]. Default value is '5000'.
#' @param savedClassifierName The saved classifier name
#' @param doLoadClassifier Choose to save or load classifier Default value is
#' 'false'.
#' @param doClassValQuantization Quantization for raster traiing Default value
#' is 'true'.
#' @param minClassValue Quantization min class value for raster traiing Default
#' value is '0.0'.
#' @param classValStepSize Quantization step size for raster traiing Default
#' value is '5.0'.
#' @param classLevels Quantization class levels for raster traiing Default
#' value is '101'.
#' @param trainOnRaster Train on raster or vector data Default value is 'true'.
#' @param trainingBands Raster bands to train on
#' @param trainingVectors Vectors to train on
#' @param featureBands Names of bands to be used as features
#' @param labelSource 'VectorNodeName' or specific Attribute name
#' @param evaluateClassifier Evaluate classifier and features
#' @param evaluateFeaturePowerSet Evaluate the power set of features Default
#' value is 'false'.
#' @param minPowerSetSize Minimum size of the power set of features Default
#' value is '2'.
#' @param maxPowerSetSize Maximum size of the power set of features Default
#' value is '7'.
#' @details
#' Descrscription from '`gpt {operator} -h`':
#'
#' "KDTree KNN classifier Parameter Options: -PclassLevels=<int> Quantization
#' class levels for raster traiing Default value is '101'.
#' -PclassValStepSize=<double> Quantization step size for raster traiing Default
#' value is '5.0'. -PdoClassValQuantization=<boolean> Quantization for raster
#' traiing Default value is 'true'. -PdoLoadClassifier=<boolean> Choose to save or
#' load classifier Default value is 'false'. -PevaluateClassifier=<boolean>
#' Evaluate classifier and features -PevaluateFeaturePowerSet=<boolean> Evaluate
#' the power set of features Default value is 'false'.
#' -PfeatureBands=<string,string,string,...> Names of bands to be used as features
#' -PlabelSource=<string> 'VectorNodeName' or specific Attribute name
#' -PmaxPowerSetSize=<integer> Maximum size of the power set of features Default
#' value is '7'. -PminClassValue=<double> Quantization min class value for raster
#' traiing Default value is '0.0'. -PminPowerSetSize=<integer> Minimum size of the
#' power set of features Default value is '2'. -PnumNeighbours=<int> The number of
#' neighbours Valid interval is (1,*\]. Default value is '5'.
#' -PnumTrainSamples=<int> The number of training samples Valid interval is (1,*\].
#' Default value is '5000'. -PsavedClassifierName=<string> The saved classifier
#' name -PtrainingBands=<string,string,string,...> Raster bands to train on
#' -PtrainingVectors=<string,string,string,...> Vectors to train on
#' -PtrainOnRaster=<boolean> Train on raster or vector data Default value is
#' 'true'."
#' @import xml2
#' @return snap_op_kdtree_knn_classifier object
#' @export
op_kdtree_knn_classifier <- function(
    operator_id,
    sourceProduct,
    numNeighbours = 5,
    numTrainSamples = 5000,
    savedClassifierName = NULL,
    doLoadClassifier = FALSE,
    doClassValQuantization = TRUE,
    minClassValue = 0.0,
    classValStepSize = 5.0,
    classLevels = 101,
    trainOnRaster = TRUE,
    trainingBands = NULL,
    trainingVectors = NULL,
    featureBands = NULL,
    labelSource = NULL,
    evaluateClassifier = NULL,
    evaluateFeaturePowerSet = FALSE,
    minPowerSetSize = 2,
    maxPowerSetSize = 7) {
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
  xml_add_child(node, "operator", "KDTree-KNN-Classifier")
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
    "numNeighbours",
    gpt_args$numNeighbours
  )
  xml_add_child(
    parameters,
    "numTrainSamples",
    gpt_args$numTrainSamples
  )
  xml_add_child(
    parameters,
    "savedClassifierName",
    gpt_args$savedClassifierName
  )
  xml_add_child(
    parameters,
    "doLoadClassifier",
    gpt_args$doLoadClassifier
  )
  xml_add_child(
    parameters,
    "doClassValQuantization",
    gpt_args$doClassValQuantization
  )
  xml_add_child(
    parameters,
    "minClassValue",
    gpt_args$minClassValue
  )
  xml_add_child(
    parameters,
    "classValStepSize",
    gpt_args$classValStepSize
  )
  xml_add_child(
    parameters,
    "classLevels",
    gpt_args$classLevels
  )
  xml_add_child(
    parameters,
    "trainOnRaster",
    gpt_args$trainOnRaster
  )
  xml_add_child(
    parameters,
    "trainingBands",
    gpt_args$trainingBands
  )
  xml_add_child(
    parameters,
    "trainingVectors",
    gpt_args$trainingVectors
  )
  xml_add_child(
    parameters,
    "featureBands",
    gpt_args$featureBands
  )
  xml_add_child(
    parameters,
    "labelSource",
    gpt_args$labelSource
  )
  xml_add_child(
    parameters,
    "evaluateClassifier",
    gpt_args$evaluateClassifier
  )
  xml_add_child(
    parameters,
    "evaluateFeaturePowerSet",
    gpt_args$evaluateFeaturePowerSet
  )
  xml_add_child(
    parameters,
    "minPowerSetSize",
    gpt_args$minPowerSetSize
  )
  xml_add_child(
    parameters,
    "maxPowerSetSize",
    gpt_args$maxPowerSetSize
  )
  operator_sources <- gpt_args[c("sourceProduct")]
  snap_op_kdtree_knn_classifier <- S7::new_class(
    "snap_op_kdtree_knn_classifier",
    parent = snap_operator
  )
  snap_op_kdtree_knn_classifier(
    operator = "KDTree-KNN-Classifier",
    operator_id = operator_id,
    operator_sources = operator_sources,
    created_with = rlang::current_fn(),
    xml_graph = as.character(op_xml)
  )
}
