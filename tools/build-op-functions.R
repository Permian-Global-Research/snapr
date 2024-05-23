library(devtools)
load_all()
document()
operators <- get_operators()
# operators$operator

# we can return to these in time.
problematic_operators <- c(
  "DecisionTree", "BandsDifferenceOp", "Multi-size", "Binning", "BandMaths",
  "Unmix", "StatisticsOp", "SpectralAngleMapperOp", "RemoteExecutionOp",
  "PixEx", "Mosaic", "Merge"
)


# delete all op_*.R files
delete_op_r_files <- function() {
  # delete all op_*.R files
  op_files <- list.files("R", pattern = "op_.*\\.R", full.names = TRUE) |>
    setdiff(paste0("R/op_", problematic_operators, ".R"))

  file.remove(op_files)
}

# delete all test-op_*.R files
delete_op_test_files <- function() {
  # delete all test-op_*.R files
  test_files <- list.files(
    "tests/testthat",
    pattern = "test-op_.*\\.R", full.names = TRUE
  ) |>
    setdiff(paste0("tests/testthat/test-op_", problematic_operators, ".R"))
  file.remove(test_files)
}

# run delete functions
delete_op_r_files()
delete_op_test_files()


# --- build all other operators


all_ops <- operators |>
  dplyr::filter(!stringr::str_detect(
    operator,
    paste(problematic_operators, collapse = "|")
  ))


purrr::walk(all_ops$operator, ~ build_xml_engine(.x), .progress = TRUE)

document()
test()
# View(all_ops)

build_xml_engine("Read")
