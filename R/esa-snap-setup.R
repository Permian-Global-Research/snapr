#' get the correct install file path for the snap installer
#' @return character path to the snap installer
#' @keywords internal
#' @noRd
get_os_installer <- function() {
  os <- Sys.info()[["sysname"]]
  version <- "10.0.0"
  ub <- paste0(
    "https://download.esa.int/step/snap/10_0/",
    "installers/esa-snap_sentinel_"
  )

  switch(os,
    Windows = glue::glue(ub, "windows-{version}.exe"),
    Darwin = glue::glue(ub, "macos-{version}.dmg"),
    Linux = glue::glue(ub, "linux-{version}.sh"),
    wrong_opsys_abort()
  )
}

#' download the snap installer
#' @param url character url to the installer
#' @param dest character path to the destination directory
#' @param chmod logical change the file permissions to 755
#' @return character path to the downloaded installer
#' @keywords internal
#' @noRd
download_installer <- function(
    url,
    dest,
    chmod = FALSE) {
  if (!dir.exists(dest)) {
    cli::cli_abort(
      c(
        "x" = "Destination directory does not exist.",
        "i" = "Please create the directory before downloading the installer."
      )
    )
  }

  dest_file <- file.path(dest, basename(url))

  dlf <- curl::multi_download(url, dest_file, resume = TRUE)

  if (dlf$status_code %in% c(200, 206, 416) && isTRUE(dlf$success)) {
    if (chmod) {
      Sys.chmod(dlf$destfile, "755")
    }
    return(dlf$destfile)
  } else {
    cli::cli_abort(
      c(
        "x" = "Download failed.",
        "i" = "Please check the url and try again."
      )
    )
  }
}

#' get the default snapr directory
#' This is the directory where snap will be installed.
#' @param child character child directory
#' @param create logical create the directory if it does not exist
#' @return character path to the snapr directory
#' @keywords internal
#' @noRd
snapr_dir <- function(child = NULL, create = TRUE) {
  base_path <- tools::R_user_dir(package = "snapr", which = "data")
  if (!is.null(child)) {
    p <- file.path(base_path, child)
  } else {
    p <- base_path
  }

  if (!dir.exists(p) && isTRUE(create)) {
    dir.create(p, recursive = TRUE)
  }

  return(p)
}

#' install snap
#' download and install ESA SNAP.
#' @param installer character path to the snap installer
#' @param install_dir character path to the installation directory
#' @details This function will download the installation file and install SNAP
#' in an appropraite location. If you already have SNAP installed and dont't
#' wish to install a second version (not sure if this is a real issue or not),
#' you can set the SNAPR_BIN environment variable to the path of the SNAP bin
#' directory.
#' @export
install_snap <- function(
    installer = download_installer(
      url = get_os_installer(),
      dest = snapr_dir("snap-installers"),
      chmod = TRUE
    ),
    install_dir = snapr_dir("esa-snap")) {
  opsys <- Sys.info()[["sysname"]]

  if (opsys %in% c("Windows", "Linux")) {
    system2(
      installer,
      args = c(
        "-q",
        "-dir",
        install_dir
      )
    )
    snapr_set_options()
  } else if (opsys == "Darwin") {
    cli::cli_abort(
      c(
        "!" = "Mac OS is currently not supported for automated snap
        installation using `install_snap`.",
        "i" = "Please download the installer from the snap website and install
        manually. Or, even better, please help us to automate the installation
        process for Mac OS by contributing to the snapr package!"
      )
    )
  } else {
    wrong_opsys_abort()
  }
}

#' abort if the operating system is not supported
#' @keywords internal
#' @noRd
wrong_opsys_abort <- function() {
  cli::cli_abort(
    c(
      "x" = "Operating system not supported.",
      "i" = "Snap isntallers are only available for Windows, MacOS, and Linux."
    )
  )
}

#' get the default snap bin directory
#' @return character path to the snap bin directory
#' @keywords internal
#' @noRd
default_snap_bin <- function() {
  file.path(snapr_dir("esa-snap", create = FALSE), "bin")
}

#' get the default snap gpt executable
#' @param bin_dir character path to the snap bin directory
#' @return character path to the snap gpt executable
#' @keywords internal
#' @noRd
default_snap_gpt <- function(
    bin_dir = default_snap_bin()) {
  opsys <- Sys.info()[["sysname"]]
  switch(opsys,
    Windows = file.path(
      bin_dir,
      "gpt.exe"
    ),
    Darwin = file.path(bin_dir, "gpt"),
    Linux = file.path(bin_dir, "gpt"),
    wrong_opsys_abort()
  )
}

#' check if snap is installed
#' @return logical TRUE if snap is installed
#' @keywords internal
#' @noRd
check_snap_install <- function() {
  if (any(
    !is.null(
      getOption("snapr_bin")
    ),
    file.exists(file.path(default_snap_bin(), "gpt"))
  )) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' set snapr options
#' @keywords internal
#' @noRd
snapr_set_options <- function() {
  if (Sys.getenv("SNAPR_BIN") == "") {
    if (isTRUE(check_snap_install())) {
      options(snapr_bin = default_snap_bin())
      options(snapr_gpt = default_snap_gpt())
    } else {
      no_snap_inform()
    }
  } else {
    options(snapr_bin = Sys.getenv("SNAPR_BIN"))
    options(snapr_gpt = default_snap_gpt(getOption("snapr_bin")))
  }
}

#' inform the user that snap is not installed
#' @keywords internal
#' @noRd
no_snap_inform <- function() {
  cli::cli_inform(
    c(
      "!" = "Snap gpt executable not found.",
      "i" = "Here are your options:",
      ">" = "1. Install snap with:",
      " " = cli::code_highlight("snapr::install_snap()"),
      ">" = "2. Manually install snap from
            {.url https://step.esa.int/main/download/snap-download/}",
      " " = "Set the SNAPR_BIN environment in your
            .Renviron file using:",
      " " = cli::code_highlight("usethis::edit_r_environ()")
    )
  )
}
