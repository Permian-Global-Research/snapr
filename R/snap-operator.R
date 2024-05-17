#' An S7 class for containing snap operators
#' @param operator character name of the snap operator
#' @param created_with function name of the function that created the object
#' @param xml_graph character xml graph of the snap operator
#' @name snap_operator
#' @keywords internal
#' @import S7
#' @export
snap_operator <- new_class(
  "snap_operator",
  properties = list(
    operator = class_character,
    operator_id = class_character,
    operator_sources = class_character,
    created_with = class_function,
    xml_graph = class_character
  ),
  validator = function(self) {
    if (length(self@operator) != 1) {
      return("@operator must be a character of length 1")
    }
    if (length(self@operator_id) != 1) {
      return("@operator must be a character of length 1")
    }
    if (!check_xml_read(self@xml_graph)) {
      return("@xml_graph must contain valid XML content")
    }
  },
)

#' An S7 class for containing snap_read_operators
#' @param operator character name of the snap operator
#' @param operator_id character id of the snap operator
#' @param created_with function name of the function that created the object
#' @param xml_graph character xml graph of the snap operator
#' @name snap_operator
#' @keywords internal
#' @import S7
#' @export
snap_read_operator <- new_class(
  "snap_read_operator",
  properties = list(
    operator = class_character,
    operator_id = class_character,
    created_with = class_function,
    xml_graph = class_character
  ),
  validator = function(self) {
    if (length(self@operator) != 1) {
      return("@operator must be a character of length 1")
    }
    if (length(self@operator_id) != 1) {
      return("@operator must be a character of length 1")
    }
    if (!check_xml_read(self@xml_graph)) {
      return("@xml_graph must contain valid XML content")
    }
  },
)

#' create xml_document object from snap_operator
#' @name as_xml_document
#' @param x snap_operator object
method(as_xml_document, snap_operator) <- function(x) {
  xml2::read_xml(x@xml_graph)
}

#' create xml_document object from snap_read_operator
#' @name as_xml_document
#' @param x snap_read_operator object
#' @export
method(as_xml_document, snap_read_operator) <- function(x) {
  xml2::read_xml(x@xml_graph)
}


#' show xml graph method for snap_operator_help object
#' @name show_xml
#' @param x snap_operator object
#' @export
method(show_xml, snap_operator) <- function(x) {
  xml_printer(x)
  invisible()
}

#' show xml graph method for snap_read_operator object
#' @name show_xml
#' @param x snap_read_operator object
#' @export
method(show_xml, snap_read_operator) <- function(x) {
  xml_printer(x)
  invisible()
}
