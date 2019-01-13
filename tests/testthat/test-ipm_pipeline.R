# test build_ipm


context('Build pipeline produces correct info')

# test_that('Proto works as expected', {
#
#
# })
#
#
# test_that('make_ipm works as expected', {
#
#
# })

test_that('make_k works as expected', {

  padrino <- RPadrino:::.read_all_sheets('../../../Padrino/metadata/Test_Padrino_methods.xlsx')


  oenethra_proto <- make_proto_ipm(padrino, ipm_id == 'xxxxx1')
  oenethra_kernels <- make_ipm(oenethra_proto)
  oenethra_k <- make_k(oenethra_kernels, oenethra_proto)

  target <- 1.059307
  actual <- max(Re(eigen(oenethra_k)$values))

  expect_equal(actual, target, tolerance = 1e-4)


  target_ovis <- 1.020261
  var_z_ovis <- 0.08001151
  mean_z_ovis <- 20.57083

  ovis_proto <- make_proto_ipm(padrino, ipm_id == 'xxxxx2')
  ovis_kernels <- make_ipm(ovis_proto)
  ovis_k <- make_k(ovis_kernels, ovis_proto)

  # Test lambda
  ovis_lam <- max(Re(eigen(ovis_k)$values))
  expect_equal(ovis_lam, target_ovis, tolerance = 1e-4)

  ovis_w <- Re(eigen(ovis_k)$vectors[ ,1])
  stab_ovis_w <- ovis_w/sum(ovis_w)

  dom_env <- RPadrino:::.make_domain_env(ovis_proto, NULL,NULL,NULL,NULL)
  mesh_p <- dom_env$log_size_vec
  mean_ovis_z_w <- sum(stab_ovis_w * mesh_p)

  var_ovis_est <- sum(stab_ovis_w * mesh_p^2) - mean_ovis_z_w^2

  expect_equal(var_ovis_est, var_z_ovis, tolerance = 1e-4)

  mean_z_est <- sum(stab_ovis_w * exp(mesh_p))
  expect_true(
    (mean_z_est - mean_z_ovis) / mean_z_ovis < 0.002 &
    (mean_z_est - mean_z_ovis) / mean_z_ovis > -0.002)


})
