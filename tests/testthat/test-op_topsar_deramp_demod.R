test_that("op_topsar_deramp_demod matches gpt xml", {
  r_op <- op_topsar_deramp_demod("testnode1", "testsrc1")
  gpt_op <- snap_operator_help("TOPSAR-DerampDemod",
    check_operator = FALSE,
    node = TRUE
  )
  snapr_xml_nodes <- all_nodes(as_xml(r_op))
  gpt_example_nodes <- all_nodes(as_xml(gpt_op))
  testthat::expect_equal(snapr_xml_nodes, gpt_example_nodes)
})
