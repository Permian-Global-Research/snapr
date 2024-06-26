% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/op_snaphu_export.R
\name{op_snaphu_export}
\alias{op_snaphu_export}
\title{SnaphuExport: snap operator function}
\usage{
op_snaphu_export(
  operator_id,
  sourceProduct,
  targetFolder = NULL,
  statCostMode = "DEFO",
  initMethod = "MST",
  numberOfTileRows = 10,
  numberOfTileCols = 10,
  numberOfProcessors = 4,
  rowOverlap = 200,
  colOverlap = 200,
  tileCostThreshold = 500
)
}
\arguments{
\item{operator_id}{character operator id}

\item{sourceProduct}{The source product(s) as input to the operator}

\item{targetFolder}{The output folder to which the data product is written.}

\item{statCostMode}{Size of coherence estimation window in Azimuth direction
Value must be one of 'TOPO', 'DEFO', 'SMOOTH', 'NOSTATCOSTS'. Default value is
'DEFO'.}

\item{initMethod}{Algorithm used for initialization of the wrapped phase
values Value must be one of 'MST', 'MCF'. Default value is 'MST'.}

\item{numberOfTileRows}{Divide the image into tiles and process in parallel.
Set to 1 for single tiled. Default value is '10'.}

\item{numberOfTileCols}{Divide the image into tiles and process in parallel.
Set to 1 for single tiled. Default value is '10'.}

\item{numberOfProcessors}{Number of concurrent processing threads. Set to 1
for single threaded. Default value is '4'.}

\item{rowOverlap}{Overlap, in pixels, between neighboring tiles. Default
value is '200'.}

\item{colOverlap}{Overlap, in pixels, between neighboring tiles. Default
value is '200'.}

\item{tileCostThreshold}{Cost threshold to use for determining boundaries of
reliable regions (long, dimensionless; scaled according to other cost
constants). Larger cost threshold implies smaller regions---safer, but more
expensive computationally. Default value is '500'.}
}
\value{
snap_op_snaphu_export object
}
\description{
SnaphuExport: snap operator function
}
\details{
Descrscription from '\verb{gpt \{operator\} -h}':

"Export data and prepare conf file for SNAPHU processing"
}
