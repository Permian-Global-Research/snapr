% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/op_radar_vegetation_index.R
\name{op_radar_vegetation_index}
\alias{op_radar_vegetation_index}
\title{Radar-Vegetation-Index: snap operator function}
\usage{
op_radar_vegetation_index(operator_id, sourceProduct, windowSize = 5)
}
\arguments{
\item{operator_id}{character operator id}

\item{sourceProduct}{The source product(s) as input to the operator}

\item{windowSize}{The sliding window size Valid interval is [1, 100].
Default value is '5'.}
}
\value{
snap_op_radar_vegetation_index object
}
\description{
Radar-Vegetation-Index: snap operator function
}
\details{
Descrscription from '\verb{gpt \{operator\} -h}':

"Dual-pol Radar Vegetation Indices generation"
}
