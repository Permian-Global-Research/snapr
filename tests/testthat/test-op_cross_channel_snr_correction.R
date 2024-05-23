test_that("op_cross_channel_snr_correction matches gpt xml", {
  r_op <- op_cross_channel_snr_correction("testnode1", "testsrc1")
  gpt_op <- snap_operator_help("Cross-Channel-SNR-Correction",
    check_operator = FALSE,
    node = TRUE
  )
  snapr_xml_nodes <- all_nodes(as_xml(r_op))
  gpt_example_nodes <- all_nodes(as_xml(gpt_op))
  testthat::expect_equal(snapr_xml_nodes, gpt_example_nodes)
})
