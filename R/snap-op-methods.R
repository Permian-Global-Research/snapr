#' create xml_document object from snap_operator and snap_operator_help objects
#' @param x snap_operator or snap_operator_help object
#' @export
as_xml_document <- new_generic("as_xml_document", "x")


#' show xml graph method for snap_operator_helper object
#' @param x snap_operator or snap_operator_help object
#' @export
show_xml <- new_generic("show_xml", "x")
