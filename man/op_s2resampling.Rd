% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/op_s2resampling.R
\name{op_s2resampling}
\alias{op_s2resampling}
\title{S2Resampling: snap operator function}
\usage{
op_s2resampling(
  operator_id,
  sourceProduct,
  resampleOnPyramidLevels = TRUE,
  downsampling = NULL,
  flagDownsampling = NULL,
  resolution = NULL,
  upsampling = NULL
)
}
\arguments{
\item{operator_id}{character operator id}

\item{sourceProduct}{The source product(s) as input to the operator}

\item{resampleOnPyramidLevels}{This setting will increase performance when
viewing the image, but accurate resamplings are only retrieved when zooming in
on a pixel. Default value is 'true'.}

\item{downsampling}{The method used for aggregation (downsampling to a
coarser resolution). Value must be one of 'First', 'Min', 'Max', 'Mean',
'Median'. Default value is 'Mean'.}

\item{flagDownsampling}{The method used for aggregation (downsampling to a
coarser resolution) of flags. Value must be one of 'First', 'FlagAnd',
'FlagOr', 'FlagMedianAnd', 'FlagMedianOr'. Default value is 'First'.}

\item{resolution}{The output resolution. Value must be one of '10', '20',
'60'. Default value is '60'.}

\item{upsampling}{The method used for interpolation (upsampling to a finer
resolution). Value must be one of 'Nearest', 'Bilinear', 'Bicubic'. Default
value is 'Bilinear'.}
}
\value{
snap_op_s2resampling object
}
\description{
S2Resampling: snap operator function
}
\details{
Descrscription from '\verb{gpt \{operator\} -h}':

"Specific S2 resample algorithm"
}
