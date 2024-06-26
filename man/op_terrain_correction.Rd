% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/op_terrain_correction.R
\name{op_terrain_correction}
\alias{op_terrain_correction}
\title{Terrain-Correction: snap operator function}
\usage{
op_terrain_correction(
  operator_id,
  sourceProduct,
  sourceBands = NULL,
  demName = "SRTM 3Sec",
  externalDEMFile = NULL,
  externalDEMNoDataValue = 0,
  externalDEMApplyEGM = TRUE,
  demResamplingMethod = "BILINEAR_INTERPOLATION",
  imgResamplingMethod = "BILINEAR_INTERPOLATION",
  pixelSpacingInMeter = 0,
  pixelSpacingInDegree = 0,
  mapProjection = "WGS84(DD)",
  alignToStandardGrid = FALSE,
  standardGridOriginX = 0,
  standardGridOriginY = 0,
  nodataValueAtSea = TRUE,
  saveDEM = FALSE,
  saveLatLon = FALSE,
  saveIncidenceAngleFromEllipsoid = FALSE,
  saveLocalIncidenceAngle = FALSE,
  saveProjectedLocalIncidenceAngle = FALSE,
  saveSelectedSourceBand = TRUE,
  saveLayoverShadowMask = FALSE,
  outputComplex = FALSE,
  applyRadiometricNormalization = FALSE,
  saveSigmaNought = FALSE,
  saveGammaNought = FALSE,
  saveBetaNought = FALSE,
  incidenceAngleForSigma0 = "Use projected local incidence angle from DEM",
  incidenceAngleForGamma0 = "Use projected local incidence angle from DEM",
  auxFile = "Latest Auxiliary File",
  externalAuxFile = NULL
)
}
\arguments{
\item{operator_id}{character operator id}

\item{sourceProduct}{The source product(s) as input to the operator}

\item{sourceBands}{The list of source bands.}

\item{demName}{The digital elevation model. Default value is 'SRTM 3Sec'.}

\item{externalDEMFile}{Sets parameter 'externalDEMFile' to \if{html}{\out{<file>}}.}

\item{externalDEMNoDataValue}{Sets parameter 'externalDEMNoDataValue' to
\if{html}{\out{<double>}}. Default value is '0'.}

\item{externalDEMApplyEGM}{Sets parameter 'externalDEMApplyEGM' to
\if{html}{\out{<boolean>}}. Default value is 'true'.}

\item{demResamplingMethod}{Sets parameter 'demResamplingMethod' to \if{html}{\out{<string>}}.
Value must be one of 'NEAREST_NEIGHBOUR', 'BILINEAR_INTERPOLATION',
'CUBIC_CONVOLUTION', 'BISINC_5_POINT_INTERPOLATION',
'BISINC_11_POINT_INTERPOLATION', 'BISINC_21_POINT_INTERPOLATION',
'BICUBIC_INTERPOLATION', 'DELAUNAY_INTERPOLATION'. Default value is
'BILINEAR_INTERPOLATION'.}

\item{imgResamplingMethod}{Sets parameter 'imgResamplingMethod' to \if{html}{\out{<string>}}.
Value must be one of 'NEAREST_NEIGHBOUR', 'BILINEAR_INTERPOLATION',
'CUBIC_CONVOLUTION', 'BISINC_5_POINT_INTERPOLATION',
'BISINC_11_POINT_INTERPOLATION', 'BISINC_21_POINT_INTERPOLATION',
'BICUBIC_INTERPOLATION'. Default value is 'BILINEAR_INTERPOLATION'.}

\item{pixelSpacingInMeter}{The pixel spacing in meters Default value is '0'.}

\item{pixelSpacingInDegree}{The pixel spacing in degrees Default value is
'0'.}

\item{mapProjection}{The coordinate reference system in well known text
format Default value is 'WGS84(DD)'.}

\item{alignToStandardGrid}{Force the image grid to be aligned with a
specific point Default value is 'false'.}

\item{standardGridOriginX}{x-coordinate of the standard grid's origin point
Default value is '0'.}

\item{standardGridOriginY}{y-coordinate of the standard grid's origin point
Default value is '0'.}

\item{nodataValueAtSea}{Mask the sea with no data value (faster) Default
value is 'true'.}

\item{saveDEM}{Sets parameter 'saveDEM' to \if{html}{\out{<boolean>}}. Default value is
'false'.}

\item{saveLatLon}{Sets parameter 'saveLatLon' to \if{html}{\out{<boolean>}}. Default value is
'false'.}

\item{saveIncidenceAngleFromEllipsoid}{Sets parameter
'saveIncidenceAngleFromEllipsoid' to \if{html}{\out{<boolean>}}. Default value is 'false'.}

\item{saveLocalIncidenceAngle}{Sets parameter 'saveLocalIncidenceAngle' to
\if{html}{\out{<boolean>}}. Default value is 'false'.}

\item{saveProjectedLocalIncidenceAngle}{Sets parameter
'saveProjectedLocalIncidenceAngle' to \if{html}{\out{<boolean>}}. Default value is 'false'.}

\item{saveSelectedSourceBand}{Sets parameter 'saveSelectedSourceBand' to
\if{html}{\out{<boolean>}}. Default value is 'true'.}

\item{saveLayoverShadowMask}{Sets parameter 'saveLayoverShadowMask' to
\if{html}{\out{<boolean>}}. Default value is 'false'.}

\item{outputComplex}{Sets parameter 'outputComplex' to \if{html}{\out{<boolean>}}. Default
value is 'false'.}

\item{applyRadiometricNormalization}{Sets parameter
'applyRadiometricNormalization' to \if{html}{\out{<boolean>}}. Default value is 'false'.}

\item{saveSigmaNought}{Sets parameter 'saveSigmaNought' to \if{html}{\out{<boolean>}}.
Default value is 'false'.}

\item{saveGammaNought}{Sets parameter 'saveGammaNought' to \if{html}{\out{<boolean>}}.
Default value is 'false'.}

\item{saveBetaNought}{Sets parameter 'saveBetaNought' to \if{html}{\out{<boolean>}}. Default
value is 'false'.}

\item{incidenceAngleForSigma0}{Sets parameter 'incidenceAngleForSigma0' to
\if{html}{\out{<string>}}. Value must be one of 'Use incidence angle from Ellipsoid', 'Use local
incidence angle from DEM', 'Use projected local incidence angle from DEM'.
Default value is 'Use projected local incidence angle from DEM'.}

\item{incidenceAngleForGamma0}{Sets parameter 'incidenceAngleForGamma0' to
\if{html}{\out{<string>}}. Value must be one of 'Use incidence angle from Ellipsoid', 'Use local
incidence angle from DEM', 'Use projected local incidence angle from DEM'.
Default value is 'Use projected local incidence angle from DEM'.}

\item{auxFile}{The auxiliary file Value must be one of 'Latest Auxiliary
File', 'Product Auxiliary File', 'External Auxiliary File'. Default value is
'Latest Auxiliary File'.}

\item{externalAuxFile}{The antenne elevation pattern gain auxiliary data
file.}
}
\value{
snap_op_terrain_correction object
}
\description{
Terrain-Correction: snap operator function
}
\details{
Descrscription from '\verb{gpt \{operator\} -h}':

"RD method for orthorectification"
}
