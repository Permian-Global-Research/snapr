library(devtools)
load_all()
document()
operators <- get_operators()

delete_op_r_files <- function() {
  # delete all op_*.R files
  op_files <- list.files("R", pattern = "op_.*\\.R", full.names = TRUE)
  file.remove(op_files)
}

delete_op_test_files <- function() {
  # delete all test-op_*.R files
  test_files <- list.files("tests/testthat", pattern = "test-op_.*\\.R", full.names = TRUE)
  file.remove(test_files)
}

delete_op_r_files()
delete_op_test_files()


# -- create reader operators
readers <- dplyr::filter(operators, stringr::str_detect(operator, "Read"))
purrr::walk(readers$operator, ~ build_xml_engine(.x, null_src = TRUE))



# --- build all other operators

all_ops <- all_ops <- operators |>
  dplyr::filter(!stringr::str_detect(
    operator,
    "Read|DecisionTree|BandsDifferenceOp|Multi-size|Binning|BandMaths"
  ))


View(all_ops)

purrr::walk(all_ops$operator[20:40], ~ build_xml_engine(.x), .progress = TRUE)

document()
test()


build_xml_engine("Speckle-Filter")
system("/home/hugh/esa-snap/bin/gpt Speckle-Filter -h")
bpf <- snap_operator_help("c2rcc.landsat8")
get_operator_help("c2rcc.landsat8")
get_operator_help("Read")
get_operator_help("Speckle-Filter")
system("/home/hugh/esa-snap/bin/gpt c2rcc.landsat8 -h")
bpf
show_xml(bpf)

bpf <- snap_operator_help("Collocate")
system("/home/hugh/esa-snap/bin/gpt Collocate -h")
bpf
show_xml(bpf)
