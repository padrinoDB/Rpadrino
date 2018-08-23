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

padrino <- RPadrino:::.read_all_sheets('../Padrino/metadata/Test_Padrino_methods.xlsx')

# test if rounding error matters

padrino[[10]]$parameter_value <- round(padrino[[10]]$parameter_value, 3)

# generate_proto_ipm() wraps everything below except the padrino_filter call
# db <- RPadrino:::padrino_filter(padrino, ipm_id == 'a2b3c1')
# #
# parsed_K <- RPadrino:::.parse_K_kernel(db)
# #
# parsed_kernels <- RPadrino:::.kernel_vr_LHS_RHS_mat(db, parsed_K)
# #
# protoIPMparams <- RPadrino:::.associate_vr_parameters(db, parsed_kernels)
# #
# proto_ipm <- dplyr::tibble(domain = db[[5]]$domain,
#                            upper = db[[5]]$upper,
#                            lower = db[[5]]$lower,
#                            state_variable = db[[5]]$state_variable,
#                            quad_rule = db[[6]]$integration_rule,
#                            mesh_p = db[[6]]$n_meshpoints,
#                            parameter_tree = list(protoIPMparams))
#
proto_ipm <- generate_proto_ipm(padrino, ipms = c('a2b3c1'))
proto_ipm_2 <- generate_proto_ipm(padrino, ipms = c('a2b1c1'))

# #
# new_domains <- RPadrino:::.extract_domains(proto_ipm)
#
# # create environment to house domains separate from kernel evaluation
# # environment
# domain_env <- RPadrino:::.generate_domain_env(new_domains)
#
# sub_kernel_list <- list()
#
# for(i in seq_len(nrow(proto_ipm))) {
#
#   kernel <- proto_ipm$kernel[i]
#   out <- list(RPadrino:::.build_kernel_env(proto_ipm,
#                                            kernel = kernel,
#                                            domain_env = domain_env))
#   names(out) <- kernel
#
#   sub_kernel_list <- purrr::splice(sub_kernel_list, out)
# }
#
# # Examine this function. seems to be doing some erroneous calculations....
# RPadrino:::.eval_vr_exprs(sub_kernel_list, domain_env)
# #
# kernel_list <- RPadrino:::.create_sub_kernels(proto_ipm,
#                                               sub_kernel_list[1:4],
#                                               domain_env)
# # #
#
test <- build_ipm(proto_ipm)
test_2 <- build_ipm(proto_ipm_2)

v <- test$K$sub_kernel_env$v_s
g <- test$K$sub_kernel_env$g_s
q_a <- test$K$sub_kernel_env$q_a

t_env <- .force_kernel_syms(test$F$sub_kernel_env)

K_1 <- test$P$sub_kernel_env$P +
  v*g*q_a*test$F$sub_kernel_env$F +
  test$C$sub_kernel_env$C

K_2 <- test_2$P$sub_kernel_env$P +
  v*g*q_a*test_2$F$sub_kernel_env$F +
  test_2$C$sub_kernel_env$C

source('tests/RPadrino_Test_Case.R')
source('tests/RPadrino_Test_Case_2.R')
#
# P_eig_padrino <- Re(eigen(test$P$sub_kernel_env$P)$values)[1]
# P_eig_Rae <- Re(eigen(P_CompN)$values)[1]
#
# F_eig_Padrino <- Re(eigen(test$F$sub_kernel_env$F * v*g*q_a)$values)[1]
# F_eig_Rae <- Re(eigen(F_CompN)$values)[1]
#
# C_eig_Padrino <- Re(eigen(test$C$sub_kernel_env$C)$values)[1]
# C_eig_Rae <- Re(eigen(C_CompN)$values)[1]
#
# (P_eig_Rae - P_eig_padrino)/P_eig_Rae
#
# (F_eig_Rae - F_eig_Padrino)/F_eig_Rae
#
# (C_eig_Rae - C_eig_Padrino)/C_eig_Rae

actual_1 <- Re(eigen(K_1)$values)[1]
actual_2 <- Re(eigen(K_2)$values[1])

(actual_1 - target_1)/target_1
(actual_2 - target_2)/target_2


# Apparently I have some bugs. Check for erroneous transpositions
# UPDATE: Fixed, now need to figure out how to suppress warnings in build_ipm
#
#
# test_g <- test$P$sub_kernel_env$forced_g
# test_g_2 <- test_2$P$sub_kernel_env$forced_g
#
# s_1 <- test$P$sub_kernel_env$s
# s_2 <- test_2$P$sub_kernel_env$s
#
# new_g <- discrete_extrema(test_g)
# new_g_2 <- discrete_extrema(test_g_2)
# new_P <- matrix(0, 50, 50)
# new_P_2 <- matrix(0, 50, 50)
#
# for(i in 1:50) new_P[ ,i] <- s_1[i] * new_g[ ,i]
# for(i in 1:50) new_P_2[ ,i] <- s_2[i] * new_g_2[ ,i]
#
# Re(eigen(new_P)$values)[1]
# Re(eigen(new_P_2)$values)[1]
#
# new_K_1 <- new_P +
#   v * g * q_a * test$F$sub_kernel_env$F +
#   test$C$sub_kernel_env$C
#
# new_actual_1 <- Re(eigen(new_K_1)$values)[1]
#
# new_K_2 <- new_P_2 +
#   v * g * q_a * test_2$F$sub_kernel_env$F +
#   test_2$C$sub_kernel_env$C
#
# new_actual_2 <- Re(eigen(new_K_2)$values)[1]
#
# (new_actual_1 - target_1)/target_1
#
# (new_actual_2 - target_2)/target_2
