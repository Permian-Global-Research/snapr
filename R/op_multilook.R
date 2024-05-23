#' Multilook: snap operator function
#' @param operator_id character operator id
#' @param sourceProduct The source product(s) as input to the operator
#' @param sourceBands The list of source bands.
#' @param nRgLooks The user defined number of range looks Valid interval is \[1,
#' *). Default value is '1'.
#' @param nAzLooks The user defined number of azimuth looks Valid interval is
#' \[1, *). Default value is '1'.
#' @param outputIntensity For complex product output intensity or i and q
#' Default value is 'false'.
#' @param grSquarePixel Use ground square pixel Default value is 'true'.
#' @details
#' Descrscription from '`gpt {operator} -h`':
#'
#' "Averages the power across a number of lines in both the azimuth and range
#' directions"
#' @import xml2
#' @return snap_op_multilook object
#' @export
op_multilook <- function(
    operator_id,
    sourceProduct,
    sourceBands = NULL,
    nRgLooks = 1,
    nAzLooks = 1,
    outputIntensity = FALSE,
    grSquarePixel = TRUE) {
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
  xml_add_child(node, "operator", "Multilook")
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
    "nRgLooks",
    gpt_args$nRgLooks
  )
  xml_add_child(
    parameters,
    "nAzLooks",
    gpt_args$nAzLooks
  )
  xml_add_child(
    parameters,
    "outputIntensity",
    gpt_args$outputIntensity
  )
  xml_add_child(
    parameters,
    "grSquarePixel",
    gpt_args$grSquarePixel
  )
  operator_sources <- gpt_args[c("sourceProduct")]
  snap_op_multilook <- S7::new_class(
    "snap_op_multilook",
    parent = snap_operator
  )
  snap_op_multilook(
    operator = "Multilook",
    operator_id = operator_id,
    operator_sources = operator_sources,
    created_with = rlang::current_fn(),
    xml_graph = as.character(op_xml)
  )
}
