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
# RPadrino:::.eval_vr_exprs(sub_kernel_list, domain_env)
#
# kernel_list <- RPadrino:::.create_sub_kernels(proto_ipm,
#                                               sub_kernel_list[1:4],
#                                               domain_env)
# # #

test <- build_ipm(proto_ipm)
test_2 <- build_ipm(proto_ipm_2)

v <- test$K$sub_kernel_env$v_s
g <- test$K$sub_kernel_env$g_s
q_a <- test$K$sub_kernel_env$q_a

K_1 <- test$P$sub_kernel_env$expr + v*g*q_a*test$F$sub_kernel_env$expr + test$C$sub_kernel_env$expr
K_2 <- test_2$P$sub_kernel_env$expr + v*g*q_a*test_2$F$sub_kernel_env$expr + test_2$C$sub_kernel_env$expr

source('tests/RPadrino_Test_Case.R')
source('tests/RPadrino_Test_Case_2.R')

actual_1 <- Re(eigen(K_1)$values[1])
actual_2 <- Re(eigen(K_2)$values[1])

(actual_1 - target_1)/target_1 * 100
(actual_2 - target_2)/target_2 * 100

