#' AATSR.Ungrid: snap operator function
#' @param operator_id character operator id
#' @param sourceProduct The source product(s) as input to the operator
#' @param L1BCharacterisationFile L1B characterisation file is needed to
#' specify first forward pixel and first nadir pixel
#' @param cornerReferenceFlag Choose the pixel coordinate reference point for
#' use in the output file. Check for Corner (default), un-check for Centre.
#' Default value is 'true'.
#' @param topographicFlag Option to apply topographic corrections to tie points
#' Default value is 'false'.
#' @param topographyHomogenity Distance (image coordinates) pixel can be from
#' tie-point to have topo correction applied Default value is '0.05'.
#' @details
#' Descrscription from '`gpt {operator} -h`':
#'
#' "Ungrids (A)ATSR L1B products and extracts geolocation and pixel field of
#' view data."
#' @import xml2
#' @return snap_op_aatsr_ungrid object
#' @export
op_aatsr_ungrid <- function(
    operator_id,
    sourceProduct,
    L1BCharacterisationFile = NULL,
    cornerReferenceFlag = TRUE,
    topographicFlag = FALSE,
    topographyHomogenity = 0.05) {
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
  xml_add_child(node, "operator", "AATSR.Ungrid")
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
    "L1BCharacterisationFile",
    gpt_args$L1BCharacterisationFile
  )
  xml_add_child(
    parameters,
    "cornerReferenceFlag",
    gpt_args$cornerReferenceFlag
  )
  xml_add_child(
    parameters,
    "topographicFlag",
    gpt_args$topographicFlag
  )
  xml_add_child(
    parameters,
    "topographyHomogenity",
    gpt_args$topographyHomogenity
  )
  operator_sources <- gpt_args[c("sourceProduct")]
  snap_op_aatsr_ungrid <- S7::new_class(
    "snap_op_aatsr_ungrid",
    parent = snap_operator
  )
  snap_op_aatsr_ungrid(
    operator = "AATSR.Ungrid",
    operator_id = operator_id,
    operator_sources = operator_sources,
    created_with = rlang::current_fn(),
    xml_graph = as.character(op_xml)
  )
}
