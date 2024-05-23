#' Azimuth-Shift-Estimation-ESD: snap operator function
#' @param operator_id character operator id
#' @param sourceProduct The source product(s) as input to the operator
#' @param cohThreshold The coherence threshold for outlier removal Valid
#' interval is (0, 1\]. Default value is '0.15'.
#' @param numBlocksPerOverlap The number of windows per overlap for ESD Valid
#' interval is \[1, 20\]. Default value is '10'.
#' @details
#' Descrscription from '`gpt {operator} -h`':
#'
#' "Estimate azimuth offset for the whole image"
#' @import xml2
#' @return snap_op_azimuth_shift_estimation_esd object
#' @export
op_azimuth_shift_estimation_esd <- function(
    operator_id,
    sourceProduct,
    cohThreshold = 0.15,
    numBlocksPerOverlap = 10) {
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
  xml_add_child(node, "operator", "Azimuth-Shift-Estimation-ESD")
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
    "cohThreshold",
    gpt_args$cohThreshold
  )
  xml_add_child(
    parameters,
    "numBlocksPerOverlap",
    gpt_args$numBlocksPerOverlap
  )
  operator_sources <- gpt_args[c("sourceProduct")]
  snap_op_azimuth_shift_estimation_esd <- S7::new_class(
    "snap_op_azimuth_shift_estimation_esd",
    parent = snap_operator
  )
  snap_op_azimuth_shift_estimation_esd(
    operator = "Azimuth-Shift-Estimation-ESD",
    operator_id = operator_id,
    operator_sources = operator_sources,
    created_with = rlang::current_fn(),
    xml_graph = as.character(op_xml)
  )
}
