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


wrong_opsys_abort <- function() {
  cli::cli_abort(
    c(
      "x" = "Operating system not supported.",
      "i" = "Snap isntallers are only available for Windows, MacOS, and Linux."
    )
  )
}

default_snap_bin <- function() {
  file.path(snapr_dir("esa-snap", create = FALSE), "bin")
}

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

check_snap_install <- function() {
  if (length(getOption("snapr_bin") > 0 &&
    file.exists(
      file.path(default_snap_bin(), "gpt")
    ))) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

snapr_set_options <- function() {
  if (Sys.getenv("SNAPR_BIN") == "") {
    if (isTRUE(check_snap_install())) {
      options(snapr_bin = default_snap_bin())
      options(snapr_gpt = default_snap_gpt())
    } else {
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
  } else {
    options(snapr_bin = Sys.getenv("SNAPR_BIN"))
    options(snapr_gpt = default_snap_gpt(getOption("snapr_bin")))
  }
}
