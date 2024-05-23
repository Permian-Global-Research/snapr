test_that("op_snaphu_export matches gpt xml", {
  r_op <- op_snaphu_export("testnode1", "testsrc1")
  gpt_op <- snap_operator_help("SnaphuExport",
    check_operator = FALSE,
    node = TRUE
  )
  snapr_xml_nodes <- all_nodes(as_xml(r_op))
  gpt_example_nodes <- all_nodes(as_xml(gpt_op))
  testthat::expect_equal(snapr_xml_nodes, gpt_example_nodes)
})
