data(pdb)


test_that("pdb_make_ipm is doing what it should do" ,{

  proto_list <- suppressMessages(pdb_make_proto_ipm(pdb,
                                                    ipm_id = paste("aaaa",
                                                                   16:18,
                                                                   sep = "")))

  # Toy example for stochastic models mixed with deterministic ones.
  ipms <- pdb_make_ipm(proto_list,
                       addl_args = list(aaaa16 = list(iterations = 3)))


  targets <- list(
    aaaa17 = c(1.01, 1.00, 0.99, 0.98, 0.97, 0.99, 1.00, 1.00, 1.00),
    aaaa18 = c(1.00, 1.00, 0.99, 0.98, 0.97, 0.99, 1.00, 1.00, 1.00)
  )

  actuals <- lapply(lambda(ipms), function(x) unname(round(x, 2)))

  # Won't test for stochastic models hitting targets because... they're
  # stochastic
  expect_equal(targets, actuals[2:3])

  expect_s3_class(ipms$aaaa16, "simple_di_stoch_param_ipm")
  expect_s3_class(ipms$aaaa17, "general_di_det_ipm")
  expect_s3_class(ipms$aaaa18, "general_di_det_ipm")


  ev_ind <- which(pdb$Metadata$eviction_used)

  ipm <- pdb_make_proto_ipm(pdb, "aaaa34") %>%
    pdb_make_ipm()

  expect_s3_class(ipm$aaaa34, "simple_di_det_ipm")
  expect_equal(round(lambda(ipm)[[1]], 2), 0.89, ignore_attr = TRUE)

})

test_that("addl_args works correctly", {

  # General_di_det
  proto_list <- pdb_make_proto_ipm(pdb, ipm_id = "aaaa17")

  # Toy example for stochastic models mixed with deterministic ones.

  ipms <- pdb_make_ipm(proto_list,
                       addl_args = list(aaaa17 = list(iterations = 100)))

  expect_equal(ncol(ipms$aaaa17$pop_state$n_size_2004), 101L)
  expect_equal(nrow(ipms$aaaa17$pop_state$n_size_2004), 100L)


  # simple_di_det

  proto_list <- pdb_make_proto_ipm(pdb, "aaaa38")

  ipms <- pdb_make_ipm(proto_list,
                       addl_args = list(aaaa38 = list(iterations = 100)))

  expect_equal(ncol(ipms$aaaa38$pop_state$n_nleaves_2014_1), 101L)
  expect_equal(nrow(ipms$aaaa38$pop_state$n_nleaves_2014_1), 100L)

})

