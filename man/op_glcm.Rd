% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/op_glcm.R
\name{op_glcm}
\alias{op_glcm}
\title{GLCM: snap operator function}
\usage{
op_glcm(
  operator_id,
  sourceProduct,
  sourceBands = NULL,
  windowSizeStr = "9x9",
  angleStr = "ALL",
  quantizerStr = "Probabilistic Quantizer",
  quantizationLevelsStr = 32,
  displacement = 4,
  noDataValue = -9999,
  outputContrast = TRUE,
  outputDissimilarity = TRUE,
  outputHomogeneity = TRUE,
  outputASM = TRUE,
  outputEnergy = TRUE,
  outputMAX = TRUE,
  outputEntropy = TRUE,
  outputMean = TRUE,
  outputVariance = TRUE,
  outputCorrelation = TRUE
)
}
\arguments{
\item{operator_id}{character operator id}

\item{sourceProduct}{The source product(s) as input to the operator}

\item{sourceBands}{The list of source bands.}

\item{windowSizeStr}{Sets parameter 'windowSizeStr' to \if{html}{\out{<string>}}. Value must
be one of '5x5', '7x7', '9x9', '11x11', '13x13', '15x15', '17x17', '21x21'.
Default value is '9x9'.}

\item{angleStr}{Sets parameter 'angleStr' to \if{html}{\out{<string>}}. Value must be one of
'0', '45', '90', '135', 'ALL'. Default value is 'ALL'.}

\item{quantizerStr}{Sets parameter 'quantizerStr' to \if{html}{\out{<string>}}. Value must be
one of 'Equal Distance Quantizer', 'Probabilistic Quantizer'. Default value is
'Probabilistic Quantizer'.}

\item{quantizationLevelsStr}{Sets parameter 'quantizationLevelsStr' to
\if{html}{\out{<string>}}. Value must be one of '8', '16', '32', '64', '128'. Default value is
'32'.}

\item{displacement}{Pixel displacement Valid interval is [1, 8]. Default
value is '4'.}

\item{noDataValue}{Target product no data value Default value is '-9999.0'.}

\item{outputContrast}{Output Contrast Default value is 'true'.}

\item{outputDissimilarity}{Output Dissimilarity Default value is 'true'.}

\item{outputHomogeneity}{Output Homogeneity Default value is 'true'.}

\item{outputASM}{Output Angular Second Moment Default value is 'true'.}

\item{outputEnergy}{Output Energy Default value is 'true'.}

\item{outputMAX}{Output Maximum Probability Default value is 'true'.}

\item{outputEntropy}{Output Entropy Default value is 'true'.}

\item{outputMean}{Output GLCM Mean Default value is 'true'.}

\item{outputVariance}{Output GLCM Variance Default value is 'true'.}

\item{outputCorrelation}{Output GLCM Correlation Default value is 'true'.}
}
\value{
snap_op_glcm object
}
\description{
GLCM: snap operator function
}
\details{
Descrscription from '\verb{gpt \{operator\} -h}':

"Extract Texture Features"
}
