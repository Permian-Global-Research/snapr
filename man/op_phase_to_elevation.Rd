% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/op_phase_to_elevation.R
\name{op_phase_to_elevation}
\alias{op_phase_to_elevation}
\title{PhaseToElevation: snap operator function}
\usage{
op_phase_to_elevation(
  operator_id,
  sourceProduct,
  demName = "SRTM 3Sec",
  demResamplingMethod = "BILINEAR_INTERPOLATION",
  externalDEMFile = NULL,
  externalDEMNoDataValue = 0
)
}
\arguments{
\item{operator_id}{character operator id}

\item{sourceProduct}{The source product(s) as input to the operator}

\item{demName}{The digital elevation model. Default value is 'SRTM 3Sec'.}

\item{demResamplingMethod}{Sets parameter 'demResamplingMethod' to \if{html}{\out{<string>}}.
Default value is 'BILINEAR_INTERPOLATION'.}

\item{externalDEMFile}{Sets parameter 'externalDEMFile' to \if{html}{\out{<file>}}.}

\item{externalDEMNoDataValue}{Sets parameter 'externalDEMNoDataValue' to
\if{html}{\out{<double>}}. Default value is '0'.}
}
\value{
snap_op_phase_to_elevation object
}
\description{
PhaseToElevation: snap operator function
}
\details{
Descrscription from '\verb{gpt \{operator\} -h}':

"DEM Generation"
}
