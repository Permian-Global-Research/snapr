#' create a snap_graph object
#' A snap graph object is used to compose snap gpt graphs
#' @param ... snap_operator objects
#' @name snap_graph
#' @import S7
#' @export
snap_graph <- new_class(
  "snap_graph",
  properties = list(
    xml_graph = class_character
  ),
  package = "snapr",
  validator = function(self) {

  },
  constructor = function(...) {
    dots <- rlang::list2(...)
    purrr::walk(dots, function(x) {
      if (!any(
        c(
          inherits(x, "snapr::snap_operator"),
          inherits(x, "snapr::snap_read_operator")
        )
      )) {
        cli::cli_abort(
          "All arguments must have the parent class 'snap_operator'"
        )
      }
    })

    sg <- xml2::xml_new_document()
    xml_add_child(sg, "graph")
    xml_add_child(sg, "version", "1.0")
    purrr::walk(dots, function(x) {
      op_graph <- xml2::read_xml(x@xml_graph)
      xml2::xml_find_first(op_graph, "//node") |>
        xml2::xml_add_child(sg, .value = _)
    })
    new_object(
      S7_object(),
      xml_graph = as.character(sg)
    )
  }
)

#' create xml_document object from snap_graph
#' @name as_xml
#' @param x snap_graph object
method(as_xml, snap_graph) <- function(x) {
  xml2::read_xml(x@xml_graph)
}

#' run snap graph
#' @name run_graph
#' @param x snap_graph object
#' @param gpt_path character path to gpt executable
#' @export
method(run_graph, snap_graph) <- function(x, gpt_path = "~/esa-snap/bin/gpt") {
  txml <- tempfile(fileext = ".xml")
  xml2::write_xml(as_xml(x), txml)
  system(paste(gpt_path, txml))
}
