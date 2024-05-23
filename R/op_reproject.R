#' Reproject: snap operator function
#' @param operator_id character operator id
#' @param sourceProduct The source product(s) as input to the operator
#' @param collocateWith The source product will be collocated with this
#' product. This is an optional source.
#' @param wktFile A file which contains the target Coordinate Reference System
#' in WKT format.
#' @param crs A text specifying the target Coordinate Reference System, either
#' in WKT or as an authority code. For appropriate EPSG authority codes see
#' (www.epsg-registry.org). AUTO authority can be used with code 42001 (UTM), and
#' 42002 (Transverse Mercator) where the scene center is used as reference.
#' Examples: EPSG:4326, AUTO:42001
#' @param referencePixelX The X-position of the reference pixel.
#' @param referencePixelY The Y-position of the reference pixel.
#' @param easting The easting of the reference pixel.
#' @param northing The northing of the reference pixel.
#' @param orientation The orientation of the output product (in degree). Valid
#' interval is \[-360,360\]. Default value is '0'.
#' @param pixelSizeX The pixel size in X direction given in CRS units.
#' @param pixelSizeY The pixel size in Y direction given in CRS units.
#' @param width The width of the target product.
#' @param height The height of the target product.
#' @param tileSizeX The tile size in X direction.
#' @param tileSizeY The tile size in Y direction.
#' @param orthorectify Whether the source product should be orthorectified.
#' (Not applicable to all products) Default value is 'false'.
#' @param elevationModelName The name of the elevation model for the
#' orthorectification. If not given tie-point data is used.
#' @param noDataValue The value used to indicate no-data.
#' @param includeTiePointGrids Whether tie-point grids should be included in
#' the output product. Default value is 'true'.
#' @param addDeltaBands Whether to add delta longitude and latitude bands.
#' Default value is 'false'.
#' @param resampling The method used for resampling of floating-point raster
#' data. Value must be one of 'Nearest', 'Bilinear', 'Bicubic'. Default value is
#' 'Nearest'.
#' @details
#' Descrscription from '`gpt {operator} -h`':
#'
#' "Reprojection of a source product to a target Coordinate Reference System."
#' @import xml2
#' @return snap_op_reproject object
#' @export
op_reproject <- function(
    operator_id,
    sourceProduct,
    collocateWith = NULL,
    wktFile = NULL,
    crs = NULL,
    referencePixelX = NULL,
    referencePixelY = NULL,
    easting = NULL,
    northing = NULL,
    orientation = 0,
    pixelSizeX = NULL,
    pixelSizeY = NULL,
    width = NULL,
    height = NULL,
    tileSizeX = NULL,
    tileSizeY = NULL,
    orthorectify = FALSE,
    elevationModelName = NULL,
    noDataValue = NULL,
    includeTiePointGrids = TRUE,
    addDeltaBands = FALSE,
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
  xml_add_child(node, "operator", "Reproject")
  sources <- xml_add_child(node, "sources")
  op_src_id <- sub(".0", "", paste0(".", seq_along(sourceProduct) - 1))
  purrr::walk2(
    op_src_id,
    sourceProduct,
    function(.x, .y) {
      xml_add_child(sources, paste0("sourceProduct", .x), refid = .y)
    }
  )
  xml_add_child(
    sources,
    "collocateWith",
    gpt_args$collocateWith
  )
  parameters <- xml_add_child(node, "parameters")
  xml_add_child(
    parameters,
    "wktFile",
    gpt_args$wktFile
  )
  xml_add_child(
    parameters,
    "crs",
    gpt_args$crs
  )
  xml_add_child(
    parameters,
    "referencePixelX",
    gpt_args$referencePixelX
  )
  xml_add_child(
    parameters,
    "referencePixelY",
    gpt_args$referencePixelY
  )
  xml_add_child(
    parameters,
    "easting",
    gpt_args$easting
  )
  xml_add_child(
    parameters,
    "northing",
    gpt_args$northing
  )
  xml_add_child(
    parameters,
    "orientation",
    gpt_args$orientation
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
    "width",
    gpt_args$width
  )
  xml_add_child(
    parameters,
    "height",
    gpt_args$height
  )
  xml_add_child(
    parameters,
    "tileSizeX",
    gpt_args$tileSizeX
  )
  xml_add_child(
    parameters,
    "tileSizeY",
    gpt_args$tileSizeY
  )
  xml_add_child(
    parameters,
    "orthorectify",
    gpt_args$orthorectify
  )
  xml_add_child(
    parameters,
    "elevationModelName",
    gpt_args$elevationModelName
  )
  xml_add_child(
    parameters,
    "noDataValue",
    gpt_args$noDataValue
  )
  xml_add_child(
    parameters,
    "includeTiePointGrids",
    gpt_args$includeTiePointGrids
  )
  xml_add_child(
    parameters,
    "addDeltaBands",
    gpt_args$addDeltaBands
  )
  xml_add_child(
    parameters,
    "resampling",
    gpt_args$resampling
  )
  operator_sources <- gpt_args[c(
    "sourceProduct",
    "collocateWith"
  )]
  snap_op_reproject <- S7::new_class(
    "snap_op_reproject",
    parent = snap_operator
  )
  snap_op_reproject(
    operator = "Reproject",
    operator_id = operator_id,
    operator_sources = operator_sources,
    created_with = rlang::current_fn(),
    xml_graph = as.character(op_xml)
  )
}
