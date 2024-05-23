test_that("op_add_land_cover matches gpt xml", {
  r_op <- op_add_land_cover("testnode1", "testsrc1")
  gpt_op <- snap_operator_help("AddLandCover",
    check_operator = FALSE,
    node = TRUE
  )
  snapr_xml_nodes <- all_nodes(as_xml(r_op))
  gpt_example_nodes <- all_nodes(as_xml(gpt_op))
  testthat::expect_equal(snapr_xml_nodes, gpt_example_nodes)
})
