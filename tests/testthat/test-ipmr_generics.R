library(rlang)
data(pdb)

test_that("make_iter_kernel works", {

  x <- pdb_make_proto_ipm(pdb, "aaaa55")
  ipm <- pdb_make_ipm(x)

  # Aldo's Opuntia model has the following form:
  # c(0, B2_yr, 0,
  #   0, 0, B1_yr,
  #   B2C_yr, B1C_yr, P_yr)

  iter_kern <- make_iter_kernel(ipm,
                                aaaa55 = c(0,      B2_yr,  0,
                                           0,      0,      B1_yr,
                                           B2C_yr, B1C_yr, P_yr))

  oh_4 <- ipm$aaaa55$sub_kernels[grepl("2004", names(ipm$aaaa55$sub_kernels))]
  oh_5 <- ipm$aaaa55$sub_kernels[grepl("2005", names(ipm$aaaa55$sub_kernels))]
  oh_6 <- ipm$aaaa55$sub_kernels[grepl("2006", names(ipm$aaaa55$sub_kernels))]
  oh_7 <- ipm$aaaa55$sub_kernels[grepl("2007", names(ipm$aaaa55$sub_kernels))]
  oh_9 <- ipm$aaaa55$sub_kernels[grepl("2009", names(ipm$aaaa55$sub_kernels))]
  tens <- ipm$aaaa55$sub_kernels[grepl("2010", names(ipm$aaaa55$sub_kernels))]
  elfs <- ipm$aaaa55$sub_kernels[grepl("2011", names(ipm$aaaa55$sub_kernels))]
  twel <- ipm$aaaa55$sub_kernels[grepl("2012", names(ipm$aaaa55$sub_kernels))]
  thir <- ipm$aaaa55$sub_kernels[grepl("2013", names(ipm$aaaa55$sub_kernels))]
  four <- ipm$aaaa55$sub_kernels[grepl("2014", names(ipm$aaaa55$sub_kernels))]

  oh_4_it <- rbind(
    cbind(0, oh_4$B2_2004, matrix(0, ncol = 200)),
    cbind(0, 0, oh_4$B1_2004),
    cbind(oh_4$B2C_2004, oh_4$B1C_2004, oh_4$P_2004)
  )

  oh_5_it <- rbind(
    cbind(0, oh_5$B2_2005, matrix(0, ncol = 200)),
    cbind(0, 0, oh_5$B1_2005),
    cbind(oh_5$B2C_2005, oh_5$B1C_2005, oh_5$P_2005)
  )

  oh_6_it <- rbind(
    cbind(0, oh_6$B2_2006, matrix(0, ncol = 200)),
    cbind(0, 0, oh_6$B1_2006),
    cbind(oh_6$B2C_2006, oh_6$B1C_2006, oh_6$P_2006)
  )

  oh_7_it <- rbind(
    cbind(0, oh_7$B2_2007, matrix(0, ncol = 200)),
    cbind(0, 0, oh_7$B1_2007),
    cbind(oh_7$B2C_2007, oh_7$B1C_2007, oh_7$P_2007)
  )

  oh_9_it <- rbind(
    cbind(0, oh_9$B2_2009, matrix(0, ncol = 200)),
    cbind(0, 0, oh_9$B1_2009),
    cbind(oh_9$B2C_2009, oh_9$B1C_2009, oh_9$P_2009)
  )

  tens_it <- rbind(
    cbind(0, tens$B2_2010, matrix(0, ncol = 200)),
    cbind(0, 0, tens$B1_2010),
    cbind(tens$B2C_2010, tens$B1C_2010, tens$P_2010)
  )

  elfs_it <- rbind(
    cbind(0, elfs$B2_2011, matrix(0, ncol = 200)),
    cbind(0, 0, elfs$B1_2011),
    cbind(elfs$B2C_2011, elfs$B1C_2011, elfs$P_2011)
  )

  twel_it <- rbind(
    cbind(0, twel$B2_2012, matrix(0, ncol = 200)),
    cbind(0, 0, twel$B1_2012),
    cbind(twel$B2C_2012, twel$B1C_2012, twel$P_2012)
  )

  thir_it <- rbind(
    cbind(0, thir$B2_2013, matrix(0, ncol = 200)),
    cbind(0, 0, thir$B1_2013),
    cbind(thir$B2C_2013, thir$B1C_2013, thir$P_2013)
  )

  four_it <- rbind(
    cbind(0, four$B2_2014, matrix(0, ncol = 200)),
    cbind(0, 0, four$B1_2014),
    cbind(four$B2C_2014, four$B1C_2014, four$P_2014)
  )

  expect_equal(iter_kern$aaaa55$mega_matrix_2004,
               oh_4_it, ignore_attr = TRUE)
  expect_equal(iter_kern$aaaa55$mega_matrix_2005,
               oh_5_it, ignore_attr = TRUE)
  expect_equal(iter_kern$aaaa55$mega_matrix_2006,
               oh_6_it, ignore_attr = TRUE)
  expect_equal(iter_kern$aaaa55$mega_matrix_2007,
               oh_7_it, ignore_attr = TRUE)
  expect_equal(iter_kern$aaaa55$mega_matrix_2009,
               oh_9_it, ignore_attr = TRUE)
  expect_equal(iter_kern$aaaa55$mega_matrix_2010,
               tens_it, ignore_attr = TRUE)
  expect_equal(iter_kern$aaaa55$mega_matrix_2011,
               elfs_it, ignore_attr = TRUE)
  expect_equal(iter_kern$aaaa55$mega_matrix_2012,
               twel_it, ignore_attr = TRUE)
  expect_equal(iter_kern$aaaa55$mega_matrix_2013,
               thir_it, ignore_attr = TRUE)
  expect_equal(iter_kern$aaaa55$mega_matrix_2014,
               four_it, ignore_attr = TRUE)

})

test_that("mean_kernel works", {

  stoch_mods <- pdb$HierarchTable$ipm_id[1]

  x <- pdb_make_proto_ipm(pdb, stoch_mods,
                          det_stoch = "stoch",
                          kern_param = "kern")

  arg_list <- list2(!!stoch_mods := list(iterations = 200))
  ipm <- pdb_make_ipm(x, addl_args = arg_list)

  test_it <- mean_kernel(ipm)

  Ps <- ipm$aaaa17$sub_kernels[grepl("P", names(ipm$aaaa17$sub_kernels))]
  Fs <- ipm$aaaa17$sub_kernels[grepl("F", names(ipm$aaaa17$sub_kernels))]

  actual_P <- Reduce("+",
                     Ps,
                     init = matrix(0, nrow = nrow(Ps[[1]]),
                                   ncol = ncol(Ps[[1]]))) / length(Ps)
  actual_F <- Reduce("+",
                     Fs,
                     init = matrix(0, nrow = nrow(Fs[[1]]),
                                   ncol = ncol(Fs[[1]]))) / length(Fs)

  actual_Y <- ipm$aaaa17$sub_kernels$Y

  expect_equal(test_it$aaaa17$mean_P_yr, actual_P, ignore_attr = TRUE)
  expect_equal(test_it$aaaa17$mean_F_yr, actual_F, ignore_attr = TRUE)
  expect_equal(test_it$aaaa17$mean_Y, actual_Y, ignore_attr = TRUE)

  # Next, stoch_param models

})


test_that("convergence diagnostics", {

  x <- pdb_make_proto_ipm(pdb, paste0("aaaa", 17:20))

  y <- pdb_make_ipm(x)

  par(mfrow = c(2,2))

  z <- conv_plot(y)

  expect_identical(z, y)
  expect_s3_class(z, "pdb_ipm")

  arg_list <- replicate(4, list(list(iterations = 150))) %>%
    setNames(paste0("aaaa",17:20))

  y <- pdb_make_ipm(x, addl_args = arg_list)

  expect_true(is_conv_to_asymptotic(y, tolerance = 1e-7))

})
