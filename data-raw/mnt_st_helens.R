## code to prepare `mnt_st_helens_s1.tif` dataset goes here

library(rsi)
library(sf)

if (!dir.exists("inst/s1")) {
  dir.create("inst/s1", recursive = TRUE)
}


msh_aoi <- st_point(c(-122.19, 46.2)) |>
  st_sfc() |>
  st_set_crs(4326) |>
  st_transform(32610) |>
  st_buffer(5000)


msh_path <- rsi::get_sentinel1_imagery(
  msh_aoi,
  start_date = "2022-05-01",
  end_date = "2022-06-01",
  output_filename = "inst/s1/mnt_st_helens_s1.tif",
  composite_function = "mean",
  gdalwarp_options = c(
    "-r", "bilinear", "-multi", "-overwrite", "-co",
    "COMPRESS=LZW", "-co", "PREDICTOR=2", "-co", "NUM_THREADS=ALL_CPUS"
  )
)

terra::rast("inst/s1/mnt_st_helens_s1.tif") |>
  terra::writeRaster("inst/s1/mt_st_helens_s1.tif", overwrite = TRUE)

# terra::plot(terra::rast(msh_path),
#   col = hcl.colors(100, "viridis"),
#   range = c(0, 250)
# )
