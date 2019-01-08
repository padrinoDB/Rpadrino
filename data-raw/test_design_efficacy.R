# Test implementation of Padrino structure
# Table key: 1 - metadata, 2 - state_variables, 3 - discrete_states, 4 - discrete_transitions,
# 5 - continuous_domains, 6 - quadrature_routines, 7 - state_vectors, 8 - ipm_kernels,
# 9 - vital_rate_exprs, 10 - parameter_values, 11 - environment_variables


rm(list = ls())
# library(dplyr)
# library(rlang)
# library(stringr)
library(RPadrino)

# source('R/unexported_utils.R')

# Ailanthis altissima Crandall + knight 2017

padrino <- RPadrino:::.read_all_sheets('../Padrino/metadata/Test_Padrino_methods.xlsx')
padrino[[9]]$model_type <- gsub('Integrated', 'Substituted', padrino[[9]]$model_type)

# # All other progress
# padrino <- padrino_round <- RPadrino:::.read_all_sheets('data-raw/all_progress.xlsx')
#
# tests <- paste('aaaa', 17:20, sep = "")
# test_geum <- padrino_filter(padrino, ipm_id %in% tests)
# db <- padrino_filter(padrino, ipm_id %in% tests[1])

# test if rounding error matters

# padrino_round[[10]]$parameter_value <- round(padrino_round[[10]]$parameter_value, 3)
cont_unburnd <- make_proto_ipm(padrino, ipm_id == 'a2b1c1')

cr_unburnd <- make_proto_ipm(padrino, ipm_id == c('a2b3c1'))
#
# round_proto_ipm <- make_proto_ipm(padrino_round,
#                                       ipms = c('a2b3c1'))
# round_proto_ipm_2 <- make_proto_ipm(padrino_round,
#                                         ipms = c('a2b1c1'))
#
#
# # #
# new_domains <- RPadrino:::.extract_domains(proto_ipm)
#
# # create environment to house domains separate from kernel evaluation
# # environment
# domain_env <- RPadrino:::.generate_domain_env(new_domains)
#
# sub_kernel_list <- RPadrino:::.generate_sub_kernels(proto_ipm, domain_env)
#
# RPadrino:::.bind_vr_quos(sub_kernel_list)
#
# # discrete_extrema is the only one implemented right now
# if(any(proto_ipm$evict)) {
#   RPadrino:::.correct_eviction(sub_kernel_list)
# }
#
# kernel_list <- RPadrino:::.make_kernels(sub_kernel_list, proto_ipm)


test <- make_ipm(cr_unburnd)
test_2 <- make_ipm(cont_unburnd, return_all = FALSE)


# individual steps in make_k
#
# data_envs <- test$data_envs
# sub_kernels <- test$kernels
#
# domain_env <- RPadrino:::.make_domain_env(proto_ipm,
#                                           domains = NULL,
#                                           lower = NULL,
#                                           upper = NULL,
#                                           mesh_points = NULL)
#
# eval_envs <- RPadrino:::.generate_kernel_envs(proto_ipm,
#                                               data_envs,
#                                               sub_kernels,
#                                               domain_env)
#
# kernel_list <- RPadrino:::.prep_kernel_quos(proto_ipm, eval_envs, sub_kernels)
#
# .bind_kernel_quos(kernel_list)
#
# last_steps <- .extract_high_level_kernels(kern_list)
#
# .bind_to_k_env(last_steps, list(k_all = kernel_list$K_all))
#
# out <- .get_k_all(kernel_list$K_all$kern_env, 'K_all')

testK <- make_k(test, cr_unburnd)
test2K <- make_k(test_2, cont_unburnd)

max(Re(eigen(testK)$values))
max(Re(eigen(test2K)$values))

oenethra_proto <- make_proto_ipm(padrino, ipm_id == 'xxxxx1')
oenethra_kernels <- make_ipm(oenethra_proto)
oenethra_k <- make_k(oenethra_kernels, oenethra_proto)

target <- 1.059307 # Lambda from IPM_True in Chapter 2 of IPM book. The one above uses those params
actual <- max(Re(eigen(oenethra_k)$values))
actual - target


target_ovis <- 1.020261
var_z_ovis <- 0.08001151
mean_z_ovis <- 20.57083

ovis_proto <- make_proto_ipm(padrino, ipm_id == 'xxxxx2')
ovis_kernels <- make_ipm(ovis_proto)
ovis_k <- make_k(ovis_kernels, ovis_proto)

# Test lambda
ovis_lam <- max(Re(eigen(ovis_k)$values))
(ovis_lam - target_ovis)/target_ovis

ovis_w <- Re(eigen(ovis_k)$vectors[ ,1])
stab_ovis_w <- ovis_w/sum(ovis_w)

dom_env <- RPadrino:::.make_domain_env(ovis_proto, NULL,NULL,NULL,NULL)
mesh_p <- dom_env$log_size_vec
mean_ovis_z_w <- sum(stab_ovis_w * mesh_p)

var_ovis_est <- sum(stab_ovis_w * mesh_p^2) - mean_ovis_z_w^2

(var_ovis_est - var_z_ovis)/var_z_ovis

mean_z_est <- sum(stab_ovis_w * exp(mesh_p))
(mean_z_est - mean_z_ovis) / mean_z_ovis
