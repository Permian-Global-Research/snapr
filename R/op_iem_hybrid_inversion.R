#' IEM-Hybrid-Inversion: snap operator function
#' @param operator_id character operator id
#' @param sourceProduct The source product(s) as input to the operator
#' @param N  closest sigma match from LUT search Default value is '5'.
#' @param M Length (pixels) of side of square neighbourhood (M) Default value
#' is '5'.
#' @param doRemainingOutliersFilter Replace remaining outlier with neighbours's
#' average Default value is 'true'.
#' @param lutFile Sets parameter 'lutFile' to <file>.
#' @param outputRMS Optional rms in output Default value is 'false'.
#' @param outputCL Optional cl in output Default value is 'false'.
#' @param thresholdRDC RDC deviation threshold Default value is '0.5'.
#' @details
#' Descrscription from '`gpt {operator} -h`':
#'
#' "Performs IEM inversion using Hybrid approach"
#' @import xml2
#' @return snap_op_iem_hybrid_inversion object
#' @export
op_iem_hybrid_inversion <- function(
    operator_id,
    sourceProduct,
    N = 5,
    M = 5,
    doRemainingOutliersFilter = TRUE,
    lutFile = NULL,
    outputRMS = FALSE,
    outputCL = FALSE,
    thresholdRDC = 0.5) {
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
  xml_add_child(node, "operator", "IEM-Hybrid-Inversion")
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
    "N",
    gpt_args$N
  )
  xml_add_child(
    parameters,
    "M",
    gpt_args$M
  )
  xml_add_child(
    parameters,
    "doRemainingOutliersFilter",
    gpt_args$doRemainingOutliersFilter
  )
  xml_add_child(
    parameters,
    "lutFile",
    gpt_args$lutFile
  )
  xml_add_child(
    parameters,
    "outputRMS",
    gpt_args$outputRMS
  )
  xml_add_child(
    parameters,
    "outputCL",
    gpt_args$outputCL
  )
  xml_add_child(
    parameters,
    "thresholdRDC",
    gpt_args$thresholdRDC
  )
  operator_sources <- gpt_args[c("sourceProduct")]
  snap_op_iem_hybrid_inversion <- S7::new_class(
    "snap_op_iem_hybrid_inversion",
    parent = snap_operator
  )
  snap_op_iem_hybrid_inversion(
    operator = "IEM-Hybrid-Inversion",
    operator_id = operator_id,
    operator_sources = operator_sources,
    created_with = rlang::current_fn(),
    xml_graph = as.character(op_xml)
  )
}
