% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/op_multilook.R
\name{op_multilook}
\alias{op_multilook}
\title{Multilook: snap operator function}
\usage{
op_multilook(
  operator_id,
  sourceProduct,
  sourceBands = NULL,
  nRgLooks = 1,
  nAzLooks = 1,
  outputIntensity = FALSE,
  grSquarePixel = TRUE
)
}
\arguments{
\item{operator_id}{character operator id}

\item{sourceProduct}{The source product(s) as input to the operator}

\item{sourceBands}{The list of source bands.}

\item{nRgLooks}{The user defined number of range looks Valid interval is [1,
*). Default value is '1'.}

\item{nAzLooks}{The user defined number of azimuth looks Valid interval is
[1, *). Default value is '1'.}

\item{outputIntensity}{For complex product output intensity or i and q
Default value is 'false'.}

\item{grSquarePixel}{Use ground square pixel Default value is 'true'.}
}
\value{
snap_op_multilook object
}
\description{
Multilook: snap operator function
}
\details{
Descrscription from '\verb{gpt \{operator\} -h}':

"Averages the power across a number of lines in both the azimuth and range
directions"
}
