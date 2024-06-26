% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/op_rayleigh_correction.R
\name{op_rayleigh_correction}
\alias{op_rayleigh_correction}
\title{RayleighCorrection: snap operator function}
\usage{
op_rayleigh_correction(
  operator_id,
  sourceProduct,
  computeTaur = FALSE,
  computeRBrr = TRUE,
  computeRtoaNg = FALSE,
  computeRtoa = FALSE,
  addAirMass = FALSE,
  s2MsiTargetResolution = 20,
  s2MsiSeaLevelPressure = 1013.25,
  s2MsiOzone = 300,
  sourceBandNames = NULL
)
}
\arguments{
\item{operator_id}{character operator id}

\item{sourceProduct}{The source product(s) as input to the operator}

\item{computeTaur}{Sets parameter 'computeTaur' to \if{html}{\out{<boolean>}}. Default value
is 'false'.}

\item{computeRBrr}{Sets parameter 'computeRBrr' to \if{html}{\out{<boolean>}}. Default value
is 'true'.}

\item{computeRtoaNg}{Sets parameter 'computeRtoaNg' to \if{html}{\out{<boolean>}}. Default
value is 'false'.}

\item{computeRtoa}{Sets parameter 'computeRtoa' to \if{html}{\out{<boolean>}}. Default value
is 'false'.}

\item{addAirMass}{Sets parameter 'addAirMass' to \if{html}{\out{<boolean>}}. Default value is
'false'.}

\item{s2MsiTargetResolution}{Sets parameter 's2MsiTargetResolution' to
\if{html}{\out{<int>}}. Value must be one of '10', '20', '60'. Default value is '20'.}

\item{s2MsiSeaLevelPressure}{Sets parameter 's2MsiSeaLevelPressure' to
\if{html}{\out{<double>}}. Default value is '1013.25'.}

\item{s2MsiOzone}{Sets parameter 's2MsiOzone' to \if{html}{\out{<double>}}. Default value is
'300.0'.}

\item{sourceBandNames}{The source bands for the computation.}
}
\value{
snap_op_rayleigh_correction object
}
\description{
RayleighCorrection: snap operator function
}
\details{
Descrscription from '\verb{gpt \{operator\} -h}':

"Performs radiometric corrections on OLCI, MERIS L1B and S2 MSI L1C data
products."
}
