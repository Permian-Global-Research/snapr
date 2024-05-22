library(devtools)
load_all()
document()
operators <- get_operators()
# operators$operator
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
purrr::walk(readers$operator, ~ build_xml_engine(.x))

document()
test()


# --- build all other operators

all_ops <- all_ops <- operators |>
  dplyr::filter(!stringr::str_detect(
    operator,
    "Read|DecisionTree|BandsDifferenceOp|Multi-size|Binning|BandMaths"
  ))


View(all_ops)

purrr::walk(all_ops$operator, ~ build_xml_engine(.x), .progress = TRUE)

document()
test()

snap_operator_help("Compactpol-Radar-Vegetation-Index")

build_xml_engine("Speckle-Filter")
snap_operator_help("Speckle-Filter")
snap_operator_help("ProductSet-Reader")
snap_operator_help("Read")
build_xml_engine("Read")
system("/home/hugh/esa-snap/bin/gpt Speckle-Filter -h")
bpf <- snap_operator_help("c2rcc.landsat8")
get_operator_help("c2rcc.landsat8")
get_operator_help("Read")
get_operator_help("Speckle-Filter")
system("/home/hugh/esa-snap/bin/gpt c2rcc.landsat8 -h")
bpf
show_xml(bpf)
load_all()
build_xml_engine("Collocate")
op_collocate("a", "b")
snap_operator_help("Collocate")

system("/home/hugh/esa-snap/bin/gpt Collocate -h")

show_xml(bpf)

op_collocate("lal", "aaa")

snap_operator_help("Coherence")

build_xml_engine("Aatsr.SST")
load_all()
snap_operator_help("Aatsr.SST")
cp <- snap_operator_help("CloudProb")
show_xml(cp)
build_xml_engine("CloudProb")

document()
test()

load_all()
fccop <- snap_operator_help("ForestCoverChangeOp")
show_xml(fccop)
build_xml_engine("ForestCoverChangeOp")

build_xml_engine("Read")
build_xml_engine("CloudProb")

build_xml_engine("ToolAdapterOp")
build_xml_engine("IntegerInterferogram")
snap_operator_help("IntegerInterferogram")
get_operator_help("Read")
