#' Get parameter defaults for a SNAP operator
#' function calls the esa_snappy python library to get the default parameters
#' for a SNAP operator
#' @param operator character name of the SNAP operator
#' @param py_env character path to the python environment
#' @return named list of parameter defaults
#' @keywords internal
#' @export
get_param_defaults <- function(
    operator,
    py_env = find_snappy_install()) {
  operator <- rlang::arg_match(operator, get_operators()$operator)
  check_reticulte()
  reticulate::use_python(py_env, required = TRUE)

  esa_snappy <- suppressMessages(reticulate::import("esa_snappy"))

  op_spi <- esa_snappy$GPF$
    getDefaultInstance()$
    getOperatorSpiRegistry()$
    getOperatorSpi(operator)
  op_desc <- op_spi$getOperatorDescriptor()$
    getParameterDescriptors()

  # Create an iterator over op_desc
  op_it <- reticulate::as_iterator(op_desc)

  # Iterate over op_desc
  op_vals <- reticulate::iterate(op_it, function(param) {
    # Get the parameter name
    name <- reticulate::py_to_r(param$getName())

    # Get the parameter default value
    default <- reticulate::py_to_r(param$getDefaultValue())

    # Get the parameter description
    description <- reticulate::py_to_r(param$getDescription())

    # Store the parameter attributes in the list
    list(param = name, default = default, description = description)
  })

  op_vals <- lapply(op_vals, function(x) {
    x$param <- fix_param_mismatches(x$param)
    x
  })

  op_val_df <- lapply(op_vals, \(x) {
    param_vals <- lapply(x, \(y) {
      if (is.null(y)) "NULL" else y
    }) |>
      unlist()
    tibble::as_tibble(t(param_vals))
  }) |>
    dplyr::bind_rows()

  gpt_params <- snap_operator_help(
    operator,
    check_operator = FALSE
  )@parameters

  if (nrow(op_val_df) == 0) {
    return(NULL)
  } else {
    df <- suppressWarnings(
      dplyr::right_join(op_val_df, gpt_params, by = "param") |>
        dplyr::mutate(
          description = dplyr::case_when(
            is.na(description.y) ~ description.x,
            TRUE ~ description.y
          ) |>
            stringr::str_squish(),
          default = dplyr::case_when(
            class == "boolean" & default == "off" ~ "FALSE",
            class == "boolean" ~ toupper(default),
            is.na(as.numeric(default)) & default != "NULL" ~
              paste0('"', default, '"'),
            TRUE ~ default
          )
        ) |>
        dplyr::select(param, default, class, description)
    )
    return(df)
  }
}

#' Fix parameter mismatches between the python and gpt params.
#' @param x character parameter name
#' @keywords internal
#' @noRd
fix_param_mismatches <- function(x) {
  switch(x,
    "sourceBandNames" = "sourceBands",
    x
  )
}

#' Configure the snappy python environment
#' congfigures the python library "esa_snappy"
configure_snappy_python <- function() {
  check_reticulte()
  snapbin <- getOption("snapr_bin")
  snappy_conf <- file.path(snapbin, "snappy-conf")
  snappy_env_exists <- reticulate::virtualenv_exists(envname = "snapr_snappy")

  if (isFALSE(snappy_env_exists)) {
    reticulate::virtualenv_create("snapr_snappy",
      packages = c("numpy", "pytest")
    )
  }

  venv_loc <- file.path(
    reticulate::virtualenv_root(),
    "snapr_snappy"
  )

  pyex <- file.path(
    venv_loc, "bin", "python"
  )


  reticulate::use_virtualenv("snapr_snappy", required = TRUE)

  if (!suppressMessages(reticulate::py_module_available("esa_snappy"))) {
    pkgdest <- file.path(venv_loc, "lib", "python*", "site-packages")
    system2(snappy_conf, args = c(pyex, pkgdest))
  }

  options(snappy_python = pyex)

  invisible()
}

destroy_snappy_env <- function(ask = interactive()) {
  check_reticulte()
  py_cleanup <- function() {
    reticulate::virtualenv_remove("snapr_snappy", confirm = FALSE)
    options(snappy_python = NULL)
  }


  if (isFALSE(ask)) {
    py_cleanup()
    return(invisible())
  }

  cli::cli_inform(
    c(
      "!" = "This will remove the 'snapr_snappy' virtual python environment.",
      "i" = "Are you sure you want to proceed?"
    )
  )

  choice <- menu(c(
    cli::col_green("Yes"),
    cli::col_red("No")
  ))

  switch(choice,
    py_cleanup(),
    invisible()
  )
}

find_snappy_install <- function() {
  if (length(getOption("snappy_python")) > 0) {
    return(getOption("snappy_python"))
  }

  check_reticulte()

  snappy_env_exists <- reticulate::virtualenv_exists(envname = "snapr_snappy")
  snappy_module_exists <- FALSE
  if (isTRUE(snappy_env_exists)) {
    reticulate::activate_virtualenv("snapr_snappy")
    snappy_module_exists <- suppressMessages(
      reticulate::py_module_available("esa_snappy")
    )
  }

  snappy_ex <- file.path(
    reticulate::virtualenv_root(),
    "snapr_snappy", "bin", "python"
  )

  if (isTRUE(snappy_module_exists)) {
    options(snappy_python = snappy_ex)
    return(snappy_ex)
  } else {
    cli::cli_abort(
      c(
        "x" = "The snappy python environment is not available.",
        "i" = "Please run `configure_snappy_python()` to create the environment."
      )
    )
  }
}

check_reticulte <- function() {
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    cli::cli_abort(
      paste(
        "The `reticulate` package is required to set use snappy from",
        "{{snapr}}.",
        "However, this is not essential and is required for some developmental",
        "functionality"
      )
    )
  }
}
