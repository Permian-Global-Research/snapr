#' ALOS-Deskewing: snap operator function
#' @param operator_id character operator id
#' @param sourceProduct The source product(s) as input to the operator
#' @param sourceBands The list of source bands.
#' @param demName The digital elevation model. Default value is 'SRTM 3Sec'.
#' @details
#' Descrscription from '`gpt {operator} -h`':
#'
#' "Deskewing ALOS product"
#' @import xml2
#' @return snap_op_alos_deskewing object
#' @export
op_alos_deskewing <- function(
    operator_id,
    sourceProduct,
    sourceBands = NULL,
    demName = "SRTM 3Sec") {
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
  xml_add_child(node, "operator", "ALOS-Deskewing")
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
    "demName",
    gpt_args$demName
  )
  operator_sources <- gpt_args[c("sourceProduct")]
  snap_op_alos_deskewing <- S7::new_class(
    "snap_op_alos_deskewing",
    parent = snap_operator
  )
  snap_op_alos_deskewing(
    operator = "ALOS-Deskewing",
    operator_id = operator_id,
    operator_sources = operator_sources,
    created_with = rlang::current_fn(),
    xml_graph = as.character(op_xml)
  )
}
