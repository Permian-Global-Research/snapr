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
    py_env = "~/virtualenvs/snap/bin/python") {
  operator <- rlang::arg_match(operator, get_operators()$operator)
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    cli::cli_abort(
      paste(
        "The `reticulate` package is required for the",
        "`get_param_defaults` function"
      )
    )
  }
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
      dplyr::left_join(op_val_df, gpt_params, by = "param") |>
        dplyr::mutate(
          description = dplyr::case_when(
            is.na(description.y) ~ description.x,
            TRUE ~ description.y
          ) |>
            stringr::str_squish(),
          default = dplyr::case_when(
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
