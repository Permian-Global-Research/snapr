test_that("op_phase_to_displacement matches gpt xml", {
  r_op <- op_phase_to_displacement("testnode1", "testsrc1")
  gpt_op <- snap_operator_help("PhaseToDisplacement",
    check_operator = FALSE,
    node = TRUE
  )
  snapr_xml_nodes <- all_nodes(as_xml(r_op))
  gpt_example_nodes <- all_nodes(as_xml(gpt_op))
  testthat::expect_equal(snapr_xml_nodes, gpt_example_nodes)
})
