test_that("input/output work correctly", {

  data(pdb)

  io_dir <- tempdir()

  pdb_save(pdb, destination = io_dir)

  tab_nms <- names(pdb)

  fps <- paste0(io_dir, "/", tab_nms, ".txt")

  test_ind <- fps %in% list.files(io_dir, full.names = TRUE)

  expect_true(all(test_ind))

  new_pdb <- pdb_load(io_dir)

  expect_s3_class(new_pdb, "pdb")
  expect_equal(new_pdb$ParameterValues,
               pdb$ParameterValues,
               ignore_attr = "row.names")

  pdb_aaa310 <- pdb_make_proto_ipm(pdb, "aaa310")
  new_aaa310 <- pdb_make_proto_ipm(new_pdb, "aaa310")

  expect_equal(pdb_aaa310, new_aaa310, ignore_attr = "row.names")

  pdb_ipm <- pdb_make_ipm(pdb_aaa310)
  new_ipm <- pdb_make_ipm(new_aaa310)

  expect_equal(lambda(pdb_ipm), lambda(new_ipm))

  file.remove(fps)
})
