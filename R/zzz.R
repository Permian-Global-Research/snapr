.onLoad <- function(...) {
  S7::methods_register()
  snapr_set_options()
  invisible()
}
