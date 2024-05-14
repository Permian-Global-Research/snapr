#' Calibration: snap operator function
#' @param sourceBandNames The list of source bands.
#' @param auxFile The auxiliary file Value must be one of 'Latest Auxiliary
#' File', 'Product Auxiliary File', 'External Auxiliary File'. Default value is
#' 'Latest Auxiliary File'.
#' @param externalAuxFile The antenna elevation pattern gain auxiliary data
#' file.
#' @param outputImageInComplex Output image in complex Default value is
#' 'false'.
#' @param outputImageScaleInDb Output image scale Default value is 'false'.
#' @param createGammaBand Create gamma0 virtual band Default value is 'false'.
#' @param createBetaBand Create beta0 virtual band Default value is 'false'.
#' @param selectedPolarisations The list of polarisations
#' @param outputSigmaBand Output sigma0 band Default value is 'true'.
#' @param outputGammaBand Output gamma0 band Default value is 'false'.
#' @param outputBetaBand Output beta0 band Default value is 'false'.
#' @details
#' Descrscription from `gpt Calibration` -h`: Calibration of products Source
#' Options: -Ssource=<file> Sets source 'source' to <filepath>. This is a
#' mandatory source.
#' @export
calibration <- function(
    sourceBandNames = NULL,
    auxFile = "Latest Auxiliary File",
    externalAuxFile = NULL,
    outputImageInComplex = FALSE,
    outputImageScaleInDb = FALSE,
    createGammaBand = FALSE,
    createBetaBand = FALSE,
    selectedPolarisations = NULL,
    outputSigmaBand = TRUE,
    outputGammaBand = FALSE,
    outputBetaBand = FALSE) {
  cat("Running Calibration operator")
}
