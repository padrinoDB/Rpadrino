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

  stoch_mods <- pdb$ParSetIndices$ipm_id[1]

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

  old_par <- par()

  par(mfrow = c(2,2))

  z <- conv_plot(y)

  expect_identical(z, y)
  expect_s3_class(z, "pdb_ipm")

  arg_list <- replicate(4, list(list(iterations = 150))) %>%
    setNames(paste0("aaaa",17:20))

  y <- pdb_make_ipm(x, addl_args = arg_list)

  expect_true(is_conv_to_asymptotic(y, tolerance = 1e-7))

  par(old_par)

})

test_that("print methods produce expected outputs", {


  ind <- c("aaaa15", "aaaa16", "aaaa17", "aaa310")

  x <- pdb_subset(pdb, ind)

  regex_1 <- "A 'pdb' object with 4 unique species, 3 publications, and 4 models"
  regex_2 <- "The following models have continuously varying environments:"

  expect_output(print(x), regex_1)
  expect_output(print(x), regex_2)

  proto_list <- pdb_make_proto_ipm(x)

  regex_1 <- "This list of 'proto_ipm's contains the following species"
  regex_2 <- gsub("_", " ", paste(x$Metadata$species_accepted, collapse = "\\n"))

  expect_output(print(proto_list), regex_1)
  expect_output(print(proto_list), regex_2)
})

test_that("getters and setters work as expected", {

  # Vital Rate exprs
  ind <- c("aaaa15", "aaaa16", "aaaa17", "aaa310")

  x <- pdb_subset(pdb, ind)
  y <- pdb_make_proto_ipm(x)

  pdb_exprs <- vital_rate_exprs(y)

  expect_s3_class(pdb_exprs$aaaa15, "ipmr_vital_rate_exprs")
  expect_s3_class(pdb_exprs$aaaa16, "ipmr_vital_rate_exprs")
  expect_s3_class(pdb_exprs$aaaa17, "ipmr_vital_rate_exprs")
  expect_s3_class(pdb_exprs$aaa310, "ipmr_vital_rate_exprs")

  test_exprs <- exprs(
    s = exp(s_i + s_s * size_1) / (1 + exp(s_i + s_s * size_1)),
    mu_g = g_int + g_slope * size_1,
    f_d_1 = stats::dunif(size_2, 0.15, 0.25),
    f_d_2 = stats::dnorm(size_2, mu_f_d_2, sd_f_d_2)
  )

  easterling_test <- pdb_exprs$aaa310[c("s", "mu_g", "f_d_1", "f_d_2")]

  expect_equal(easterling_test, test_exprs, ignore_attr = TRUE)

  z <- pdb_make_ipm(y["aaa310"])

  pdb_exprs <- vital_rate_exprs(z)

  easterling_test <- pdb_exprs$aaa310[c("s", "mu_g", "f_d_1", "f_d_2")]

  expect_s3_class(pdb_exprs$aaa310, "ipmr_vital_rate_exprs")
  expect_equal(easterling_test, test_exprs, ignore_attr = TRUE)

  vital_rate_exprs(y) <- pdb_new_fun_form(
    list(
      aaa310 = list(
        mu_g = g_int + g_slope_1 * size_1 + g_slope_3 * size_1 ^(1/3)
      )
    )
  )

  test_expr <- rlang::exprs(
    mu_g = g_int + g_slope_1 * size_1 + g_slope_3 * size_1 ^(1/3)
  )

  pdb_exprs <- vital_rate_exprs(y)

  expect_equal(pdb_exprs$aaa310$mu_g, test_expr[[1]], ignore_attr = TRUE)

  # kernel formulae

  pdb_exprs <- kernel_formulae(y)

  expect_s3_class(pdb_exprs$aaaa15, "ipmr_kernel_exprs")
  expect_s3_class(pdb_exprs$aaaa16, "ipmr_kernel_exprs")
  expect_s3_class(pdb_exprs$aaaa17, "ipmr_kernel_exprs")
  expect_s3_class(pdb_exprs$aaa310, "ipmr_kernel_exprs")

  test_exprs <- exprs(
    P_yr = s_yr * g_yr * d_size,
    Y = recr_size * yearling_s * d_size,
    F_yr = f_yr
  )

  geum_exprs <- pdb_exprs$aaaa17

  expect_equal(geum_exprs, test_exprs, ignore_attr = TRUE)

  test_exprs <- exprs(
    P = s * g,
    F = f_n * f_d
  )

  easterling_exprs <- pdb_exprs$aaa310

  expect_equal(easterling_exprs, test_exprs, ignore_attr = TRUE)

  kernel_formulae(y) <- pdb_new_fun_form(
    list(
      aaa310 = list(
        P = s * g * z,
        F = f_n * f_d * germ_prob
      ),
      aaaa17 = list(
        P_yr = s_yr * g_yr * z_yr * d_size
      )
    )
  )

  test_exprs <- exprs(
      P = s * g * z,
      F = f_n * f_d * germ_prob,
      P_yr = s_yr * g_yr * z_yr * d_size
    )

  pdb_exprs <- kernel_formulae(y)

  expect_equal(pdb_exprs$aaa310, test_exprs[1:2], ignore_attr = TRUE)
  expect_equal(pdb_exprs$aaaa17$P_yr, test_exprs[[3]], ignore_attr = TRUE)

  y <- pdb_make_proto_ipm(x)

  pdb_exprs <- kernel_formulae(z)

  test_exprs <- exprs(
    P = s * g,
    F = f_n * f_d
  )

  expect_equal(easterling_exprs, test_exprs, ignore_attr = TRUE)


  # Domains

  pdb_doms <- domains(y)

  expect_s3_class(pdb_doms$aaaa15, "ipmr_domains")
  expect_s3_class(pdb_doms$aaaa16, "ipmr_domains")
  expect_s3_class(pdb_doms$aaaa17, "ipmr_domains")
  expect_s3_class(pdb_doms$aaa310, "ipmr_domains")

  test_doms <- list(aaaa15 = list(leafarea = c(lower_bound  = 0.57,
                                               upper_bound  = 11.9,
                                               n_meshpoints = 50)),
                    aaa310 = list(size = c(lower_bound  = 0,
                                           upper_bound  = 5.83,
                                           n_meshpoints = 1000)))

  dom_list <- pdb_doms[c("aaaa15", "aaa310")]

  expect_equal(dom_list, test_doms, ignore_attr = TRUE)

  z <- pdb_make_ipm(y[c(1, 4)])

  dom_list <- domains(z)

  expect_equal(dom_list, test_doms, ignore_attr = TRUE)

  # Parameters

  pdb_pars <- parameters(y)

  expect_s3_class(pdb_pars$aaaa15, "ipmr_parameters")
  expect_s3_class(pdb_pars$aaaa16, "ipmr_parameters")
  expect_s3_class(pdb_pars$aaaa17, "ipmr_parameters")
  expect_s3_class(pdb_pars$aaa310, "ipmr_parameters")

  expect_equal(pdb_pars$aaa310$s_i, 1.34, ignore_attr = "flat_protect")

  parameters(y) <- list(aaa310 = list(s_i = 1.6),
                        aaaa16 = list(g_i = 12,
                                      g_s = 14))

  new_pars <- parameters(y)$aaa310

  expect_equal(new_pars$s_i, 1.6, ignore_attr = "flat_protect")

  new_pars <- parameters(y)$aaaa16

  expect_equal(new_pars$g_i, 12, ignore_attr = "flat_protect")
  expect_equal(new_pars$g_s, 14, ignore_attr = "flat_protect")

  # pop_state

  pdb_pops <- pop_state(y)

  expect_equal(pdb_pops$aaaa15$pop_state_leafarea,
               "Pre-defined population state.")
  expect_equal(pdb_pops$aaaa16$pop_state_leafarea,
               "Pre-defined population state.")
  expect_equal(pdb_pops$aaaa17$pop_state_size_yr,
               "Pre-defined population state.")
  expect_equal(pdb_pops$aaa310$pop_state_size,
               "Pre-defined population state.")

  z <- pdb_make_ipm(y)

  pdb_pops <- pop_state(z)

  expect_equal(dim(pdb_pops$aaaa15$n_leafarea), c(50, 51))
  expect_equal(dim(pdb_pops$aaaa16$n_leafarea), c(50, 51))
  expect_equal(dim(pdb_pops$aaaa17$n_size_2004), c(100, 51))
  expect_equal(dim(pdb_pops$aaa310$n_size), c(1000, 51))

  # Right/left_ev

  pdb_w <- suppressWarnings(right_ev(z))

  expect_s3_class(pdb_w$aaaa15, "ipmr_w")
  expect_s3_class(pdb_w$aaaa16, "ipmr_w")
  expect_s3_class(pdb_w$aaa310, "ipmr_w")

  expect_type(pdb_w$aaaa15$leafarea_w, "double")
  expect_type(pdb_w$aaaa16$leafarea_w, "double")
  expect_type(pdb_w$aaaa17, "double")
  expect_type(pdb_w$aaa310$size_w, "double")

  expect_equal(pdb_w$aaaa17, NA_real_)

  y  <- pdb_make_proto_ipm(pdb, ind[1:2])
  y1 <- pdb_make_proto_ipm(pdb, ind[3:4])

  z  <- pdb_make_ipm(y, addl_args = list(aaaa15 = list(return_sub_kernels = TRUE),
                                         aaaa16 = list(return_sub_kernels = TRUE)))
  z1 <- pdb_make_ipm(y1)

  pdb_v  <- suppressWarnings(left_ev(z, iterations = 50))
  pdb_v1 <- suppressWarnings(left_ev(z1, iterations = 200))

  expect_s3_class(pdb_v$aaaa15, "ipmr_v")
  expect_s3_class(pdb_v$aaaa16, "ipmr_v")
  expect_s3_class(pdb_v1$aaa310, "ipmr_v")

  expect_type(pdb_v$aaaa15$leafarea_v, "double")
  expect_type(pdb_v$aaaa16$leafarea_v, "double")
  expect_type(pdb_v1$aaaa17, "double")
  expect_type(pdb_v1$aaa310$size_v, "double")

  # lack of convergence should return NA
  expect_equal(pdb_v1$aaaa17, NA_real_)

})
