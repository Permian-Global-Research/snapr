#' HorizontalVerticalMotion: snap operator function
#' @param operator_id character operator id
#' @param sourceProduct The source product(s) as input to the operator
#' @param refPixelX X position for reference pixel Default value is '0'.
#' @param refPixelY Y position for reference pixel Default value is '0'.
#' @details
#' Descrscription from '`gpt {operator} -h`':
#'
#' "Computation of Horizontal/Vertical Motion Components"
#' @import xml2
#' @return snap_op_horizontal_vertical_motion object
#' @export
op_horizontal_vertical_motion <- function(
    operator_id,
    sourceProduct,
    refPixelX = 0,
    refPixelY = 0) {
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
  xml_add_child(node, "operator", "HorizontalVerticalMotion")
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
    "refPixelX",
    gpt_args$refPixelX
  )
  xml_add_child(
    parameters,
    "refPixelY",
    gpt_args$refPixelY
  )
  operator_sources <- gpt_args[c("sourceProduct")]
  snap_op_horizontal_vertical_motion <- S7::new_class(
    "snap_op_horizontal_vertical_motion",
    parent = snap_operator
  )
  snap_op_horizontal_vertical_motion(
    operator = "HorizontalVerticalMotion",
    operator_id = operator_id,
    operator_sources = operator_sources,
    created_with = rlang::current_fn(),
    xml_graph = as.character(op_xml)
  )
}
