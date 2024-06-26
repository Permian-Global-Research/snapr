% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/op_dem_assisted_coregistration.R
\name{op_dem_assisted_coregistration}
\alias{op_dem_assisted_coregistration}
\title{DEM-Assisted-Coregistration: snap operator function}
\usage{
op_dem_assisted_coregistration(
  operator_id,
  sourceProduct,
  demName = "SRTM 3Sec",
  demResamplingMethod = "BICUBIC_INTERPOLATION",
  externalDEMFile = NULL,
  externalDEMNoDataValue = 0,
  resamplingType = "BISINC_5_POINT_INTERPOLATION",
  tileExtensionPercent = 50,
  maskOutAreaWithoutElevation = TRUE,
  outputRangeAzimuthOffset = FALSE
)
}
\arguments{
\item{operator_id}{character operator id}

\item{sourceProduct}{The source product(s) as input to the operator}

\item{demName}{The digital elevation model. Default value is 'SRTM 3Sec'.}

\item{demResamplingMethod}{Sets parameter 'demResamplingMethod' to \if{html}{\out{<string>}}.
Default value is 'BICUBIC_INTERPOLATION'.}

\item{externalDEMFile}{Sets parameter 'externalDEMFile' to \if{html}{\out{<file>}}.}

\item{externalDEMNoDataValue}{Sets parameter 'externalDEMNoDataValue' to
\if{html}{\out{<double>}}. Default value is '0'.}

\item{resamplingType}{The method to be used when resampling the slave grid
onto the master grid. Default value is 'BISINC_5_POINT_INTERPOLATION'.}

\item{tileExtensionPercent}{Define tile extension percentage. Valid interval
is [0, *). Default value is '50'.}

\item{maskOutAreaWithoutElevation}{Sets parameter
'maskOutAreaWithoutElevation' to \if{html}{\out{<boolean>}}. Default value is 'true'.}

\item{outputRangeAzimuthOffset}{Sets parameter 'outputRangeAzimuthOffset' to
\if{html}{\out{<boolean>}}. Default value is 'false'.}
}
\value{
snap_op_dem_assisted_coregistration object
}
\description{
DEM-Assisted-Coregistration: snap operator function
}
\details{
Descrscription from '\verb{gpt \{operator\} -h}':

"Orbit and DEM based co-registration Parameter Options: -PdemName=\if{html}{\out{<string>}}
The digital elevation model. Default value is 'SRTM 3Sec'.
-PdemResamplingMethod=\if{html}{\out{<string>}} Sets parameter 'demResamplingMethod' to
\if{html}{\out{<string>}}. Default value is 'BICUBIC_INTERPOLATION'. -PexternalDEMFile=\if{html}{\out{<file>}}
Sets parameter 'externalDEMFile' to \if{html}{\out{<file>}}. -PexternalDEMNoDataValue=\if{html}{\out{<double>}}
Sets parameter 'externalDEMNoDataValue' to \if{html}{\out{<double>}}. Default value is '0'.
-PmaskOutAreaWithoutElevation=\if{html}{\out{<boolean>}} Sets parameter
'maskOutAreaWithoutElevation' to \if{html}{\out{<boolean>}}. Default value is 'true'.
-PoutputRangeAzimuthOffset=\if{html}{\out{<boolean>}} Sets parameter 'outputRangeAzimuthOffset'
to \if{html}{\out{<boolean>}}. Default value is 'false'. -PresamplingType=\if{html}{\out{<string>}} The method to
be used when resampling the slave grid onto the master grid. Default value is
'BISINC_5_POINT_INTERPOLATION'. -PtileExtensionPercent=\if{html}{\out{<int>}} Define tile
extension percentage. Valid interval is [0, *). Default value is '50'."
}
