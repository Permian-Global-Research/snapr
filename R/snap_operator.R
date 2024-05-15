#' An S7 class for containing snap operators
#' @param operator character name of the snap operator
#' @param created_by function name of the function that created the object
#' @param xml_graph character xml graph of the snap operator
#' @keywords internal
#' @noRd
snap_operator <- S7::new_class(
  "snap_operator",
  properties = list(
    operator = class_character,
    operator_id = class_character,
    operator_sources = class_character,
    created_by = class_function,
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
