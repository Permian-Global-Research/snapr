#' create xml_document object from snap_operator and snap_operator_help objects
#' @param x snap_graph, snap_operator or snap_operator_help object
#' @param ... not used
#' @export
as_xml <- new_generic("as_xml", "x")


#' show xml graph method for snap_operator_helper object
#' @param x snap_grah, snap_operator or snap_operator_help object
#' @param ... not used
#' @export
show_xml <- new_generic("show_xml", "x")


#' print method for snap_graph object
#' @param x snap_graph object
#' @param gpt_path character path to gpt executable
#' @param ... not used
#' @export
run_graph <- new_generic("run_graph", "x")


#' write snap graph to file
#' @param x snap_graph object
#' @param ... not used
#' @export
save_graph <- new_generic("save_graph", "x")
