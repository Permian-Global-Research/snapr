#' TemporalPercentile: snap operator function
#' @param operator_id character operator id
#' @param sourceProduct The source product(s) as input to the operator
#' @param sourceProductPaths A comma-separated list of file paths specifying
#' the source products. Source products to be considered for percentile
#' computation. Each path may contain the wildcards '**' (matches recursively any
#' directory), '*' (matches any character sequence in path names) and '?' (matches
#' any single character). If, for example, all NetCDF files under /eodata/ shall
#' be considered, use '/eodata/**/*.nc'.
#' @param startDate The start date. If not given, it is taken from the 'oldest'
#' source product. Products that have a start date earlier than the start date
#' given by this parameter are not considered. Format for valid values is
#' 'yyyy-MM-dd HH:mm:ss'.
#' @param endDate The end date. If not given, it is taken from the 'newest'
#' source product. Products that have an end date later than the end date given by
#' this parameter are not considered. Format for valid values is 'yyyy-MM-dd
#' HH:mm:ss'.
#' @param keepIntermediateTimeSeriesProduct Determines whether the time series
#' product which is created during computation should be written to disk. Default
#' value is 'true'.
#' @param timeSeriesOutputDir The output directory for the intermediate time
#' series product. If not given, the time series product will be written to the
#' working directory.
#' @param crs A text specifying the target Coordinate Reference System, either
#' in WKT or as an authority code. For appropriate EPSG authority codes see
#' (www.epsg-registry.org). AUTO authority can be used with code 42001 (UTM), and
#' 42002 (Transverse Mercator) where the scene center is used as reference.
#' Examples: EPSG:4326, AUTO:42001 Default value is 'EPSG:4326'.
#' @param westBound The most-western longitude. All values west of this
#' longitude will not be considered. Valid interval is \[-180,180\]. Default value
#' is '-15.0'.
#' @param northBound The most-northern latitude. All values north of this
#' latitude will not be considered. Valid interval is \[-90,90\]. Default value is
#' '75.0'.
#' @param eastBound The most-eastern longitude. All values east of this
#' longitude will not be considered. Valid interval is \[-180,180\]. Default value
#' is '30.0'.
#' @param southBound The most-southern latitude. All values south of this
#' latitude will not be considered. Valid interval is \[-90,90\]. Default value is
#' '35.0'.
#' @param pixelSizeX Size of a pixel in X-direction in map units. Default value
#' is '0.05'.
#' @param pixelSizeY Size of a pixel in Y-direction in map units. Default value
#' is '0.05'.
#' @param sourceBandName The name of the band in the source products. Either
#' this or 'bandMathsExpression' must be provided.
#' @param bandMathsExpression A band maths expression serving as input band.
#' Either this or 'sourceBandName' must be provided.
#' @param percentileBandNamePrefix If given, this is the percentile band name
#' prefix. If empty, the resulting percentile band’s name prefix will be either
#' the 'sourceBandName' or created from the 'bandMathsExpression'.
#' @param validPixelExpression The valid pixel expression serving as criterion
#' for whether to consider pixels for computation. Default value is 'true'.
#' @param percentiles The percentiles. Default value is '90'.
#' @param gapFillingMethod The gap filling method for percentile calculation.
#' Value must be one of 'noGapFilling', 'gapFillingLinearInterpolation',
#' 'gapFillingSplineInterpolation', 'gapFillingQuadraticInterpolation'. Default
#' value is 'gapFillingLinearInterpolation'.
#' @param startValueFallback The fallback value for the start of a pixel time
#' series. It will be considered if there is no valid value at the pixel of the
#' oldest collocated mean band. This would be the case, if, e.g., there is a
#' cloudy day at the time period start. Default value is '0.0'.
#' @param endValueFallback The fallback value for the end of a pixel time
#' series. It will be considered ifthere is no valid value at the pixel of the
#' newest collocated mean band. This would be the case, if, e.g., there is a
#' cloudy day at the time period end. Default value is '0.0'.
#' @param resampling The method used for resampling of floating-point raster
#' data, if source products must be reprojected to the target CRS. Value must be
#' one of 'Nearest', 'Bilinear', 'Bicubic'. Default value is 'Nearest'.
#' @details
#' Descrscription from '`gpt {operator} -h`':
#'
#' "Computes percentiles over a given time period. Parameter Options:
#' -PbandMathsExpression=<string> A band maths expression serving as input band.
#' Either this or 'sourceBandName' must be provided. -Pcrs=<string> A text
#' specifying the target Coordinate Reference System, either in WKT or as an
#' authority code. For appropriate EPSG authority codes see
#' (www.epsg-registry.org). AUTO authority can be used with code 42001 (UTM), and
#' 42002 (Transverse Mercator) where the scene center is used as reference.
#' Examples: EPSG:4326, AUTO:42001 Default value is 'EPSG:4326'.
#' -PeastBound=<double> The most-eastern longitude. All values east of this
#' longitude will not be considered. Valid interval is \[-180,180\]. Default value
#' is '30.0'. -PendDate=<uTC> The end date. If not given, it is taken from the
#' 'newest' source product. Products that have an end date later than the end date
#' given by this parameter are not considered. Format for valid values is
#' 'yyyy-MM-dd HH:mm:ss'. -PendValueFallback=<double> The fallback value for the
#' end of a pixel time series. It will be considered ifthere is no valid value at
#' the pixel of the newest collocated mean band. This would be the case, if, e.g.,
#' there is a cloudy day at the time period end. Default value is '0.0'.
#' -PgapFillingMethod=<string> The gap filling method for percentile calculation.
#' Value must be one of 'noGapFilling', 'gapFillingLinearInterpolation',
#' 'gapFillingSplineInterpolation', 'gapFillingQuadraticInterpolation'. Default
#' value is 'gapFillingLinearInterpolation'.
#' -PkeepIntermediateTimeSeriesProduct=<boolean> Determines whether the time
#' series product which is created during computation should be written to disk.
#' Default value is 'true'. -PnorthBound=<double> The most-northern latitude. All
#' values north of this latitude will not be considered. Valid interval is
#' \[-90,90\]. Default value is '75.0'. -PpercentileBandNamePrefix=<string> If
#' given, this is the percentile band name prefix. If empty, the resulting
#' percentile band’s name prefix will be either the 'sourceBandName' or created
#' from the 'bandMathsExpression'. -Ppercentiles=<int,int,int,...> The
#' percentiles. Default value is '90'. -PpixelSizeX=<double> Size of a pixel in
#' X-direction in map units. Default value is '0.05'. -PpixelSizeY=<double> Size
#' of a pixel in Y-direction in map units. Default value is '0.05'.
#' -Presampling=<string> The method used for resampling of floating-point raster
#' data, if source products must be reprojected to the target CRS. Value must be
#' one of 'Nearest', 'Bilinear', 'Bicubic'. Default value is 'Nearest'.
#' -PsourceBandName=<string> The name of the band in the source products. Either
#' this or 'bandMathsExpression' must be provided.
#' -PsourceProductPaths=<string,string,string,...> A comma-separated list of file
#' paths specifying the source products. Source products to be considered for
#' percentile computation. Each path may contain the wildcards '**' (matches
#' recursively any directory), '*' (matches any character sequence in path names)
#' and '?' (matches any single character). If, for example, all NetCDF files under
#' /eodata/ shall be considered, use '/eodata/**/*.nc'. -PsouthBound=<double> The
#' most-southern latitude. All values south of this latitude will not be
#' considered. Valid interval is \[-90,90\]. Default value is '35.0'.
#' -PstartDate=<uTC> The start date. If not given, it is taken from the 'oldest'
#' source product. Products that have a start date earlier than the start date
#' given by this parameter are not considered. Format for valid values is
#' 'yyyy-MM-dd HH:mm:ss'. -PstartValueFallback=<double> The fallback value for the
#' start of a pixel time series. It will be considered if there is no valid value
#' at the pixel of the oldest collocated mean band. This would be the case, if,
#' e.g., there is a cloudy day at the time period start. Default value is '0.0'.
#' -PtimeSeriesOutputDir=<file> The output directory for the intermediate time
#' series product. If not given, the time series product will be written to the
#' working directory. -PvalidPixelExpression=<string> The valid pixel expression
#' serving as criterion for whether to consider pixels for computation. Default
#' value is 'true'. -PwestBound=<double> The most-western longitude. All values
#' west of this longitude will not be considered. Valid interval is \[-180,180\].
#' Default value is '-15.0'."
#' @import xml2
#' @return snap_op_temporal_percentile object
#' @export
op_temporal_percentile <- function(
    operator_id,
    sourceProduct,
    sourceProductPaths = NULL,
    startDate = NULL,
    endDate = NULL,
    keepIntermediateTimeSeriesProduct = TRUE,
    timeSeriesOutputDir = NULL,
    crs = "EPSG:4326",
    westBound = -15.0,
    northBound = 75.0,
    eastBound = 30.0,
    southBound = 35.0,
    pixelSizeX = 0.05,
    pixelSizeY = 0.05,
    sourceBandName = NULL,
    bandMathsExpression = NULL,
    percentileBandNamePrefix = NULL,
    validPixelExpression = "true",
    percentiles = 90,
    gapFillingMethod = "gapFillingLinearInterpolation",
    startValueFallback = 0.0,
    endValueFallback = 0.0,
    resampling = NULL) {
  args <- as.list(rlang::current_env())
  gpt_args <- purrr::map(args, function(x) {
    if (is.null(x)) {
      ""
    } else if (is.logical(x)) {
      tolower(x)
    } else {
      x
    }
  })
  op_xml <- xml2::xml_new_document()
  node <- xml_add_child(op_xml, "node", id = operator_id)
  xml_add_child(node, "operator", "TemporalPercentile")
  sources <- xml_add_child(node, "sources")
  op_src_id <- sub(".0", "", paste0(".", seq_along(sourceProduct) - 1))
  purrr::walk2(
    op_src_id,
    sourceProduct,
    function(.x, .y) {
      xml_add_child(sources, paste0("sourceProduct", .x), refid = .y)
    }
  )

  parameters <- xml_add_child(node, "parameters")
  xml_add_child(
    parameters,
    "sourceProductPaths",
    gpt_args$sourceProductPaths
  )
  xml_add_child(
    parameters,
    "startDate",
    gpt_args$startDate
  )
  xml_add_child(
    parameters,
    "endDate",
    gpt_args$endDate
  )
  xml_add_child(
    parameters,
    "keepIntermediateTimeSeriesProduct",
    gpt_args$keepIntermediateTimeSeriesProduct
  )
  xml_add_child(
    parameters,
    "timeSeriesOutputDir",
    gpt_args$timeSeriesOutputDir
  )
  xml_add_child(
    parameters,
    "crs",
    gpt_args$crs
  )
  xml_add_child(
    parameters,
    "westBound",
    gpt_args$westBound
  )
  xml_add_child(
    parameters,
    "northBound",
    gpt_args$northBound
  )
  xml_add_child(
    parameters,
    "eastBound",
    gpt_args$eastBound
  )
  xml_add_child(
    parameters,
    "southBound",
    gpt_args$southBound
  )
  xml_add_child(
    parameters,
    "pixelSizeX",
    gpt_args$pixelSizeX
  )
  xml_add_child(
    parameters,
    "pixelSizeY",
    gpt_args$pixelSizeY
  )
  xml_add_child(
    parameters,
    "sourceBandName",
    gpt_args$sourceBandName
  )
  xml_add_child(
    parameters,
    "bandMathsExpression",
    gpt_args$bandMathsExpression
  )
  xml_add_child(
    parameters,
    "percentileBandNamePrefix",
    gpt_args$percentileBandNamePrefix
  )
  xml_add_child(
    parameters,
    "validPixelExpression",
    gpt_args$validPixelExpression
  )
  xml_add_child(
    parameters,
    "percentiles",
    gpt_args$percentiles
  )
  xml_add_child(
    parameters,
    "gapFillingMethod",
    gpt_args$gapFillingMethod
  )
  xml_add_child(
    parameters,
    "startValueFallback",
    gpt_args$startValueFallback
  )
  xml_add_child(
    parameters,
    "endValueFallback",
    gpt_args$endValueFallback
  )
  xml_add_child(
    parameters,
    "resampling",
    gpt_args$resampling
  )
  operator_sources <- gpt_args[c("sourceProduct")]
  snap_op_temporal_percentile <- S7::new_class(
    "snap_op_temporal_percentile",
    parent = snap_operator
  )
  snap_op_temporal_percentile(
    operator = "TemporalPercentile",
    operator_id = operator_id,
    operator_sources = operator_sources,
    created_with = rlang::current_fn(),
    xml_graph = as.character(op_xml)
  )
}
