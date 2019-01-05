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

cr_unburnd <- proto_ipm <-  make_proto_ipm(padrino, ipm_id == c('a2b3c1'))
#
# round_proto_ipm <- make_proto_ipm(padrino_round,
#                                       ipms = c('a2b3c1'))
# round_proto_ipm_2 <- make_proto_ipm(padrino_round,
#                                         ipms = c('a2b1c1'))
#
#
# #
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
test_2 <- make_ipm(cont_unburnd)

testK <- make_K(test, proto_ipm)
test2K <- make_K(test, proto_ipm)


source('tests/RPadrino_Test_Case_Round.R')
source('tests/RPadrino_Test_Case_Round_2.R')
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
actual_2 <- Re(eigen(K_2)$values)[1]

# round_actual_1 <- Re(eigen(round_K_1)$values)[1]
# round_actual_2 <- Re(eigen(round_K_2)$values)[1]


(actual_1 - target_1)/target_1
(actual_2 - target_2)/target_2
(round_actual_1 - target_1)/target_1
(round_actual_2 - target_2)/target_2

# these values still aren't quite identical, but neither are the underlying
# parameters
(round_actual_1 - round_target_1)/round_target_1 -> round_diff_1
(round_actual_2 - round_target_2)/round_target_2


(round_target_1 - target_1)/target_1 # This makes me a little uneasy
(round_target_2 - target_2)/target_2 # This seems a bit more reasonable

# Apparently I have some bugs. Check for erroneous transpositions
# UPDATE: Fixed, now need to figure out how to suppress warnings in build_ipm
#
#
test_g <- test$P$sub_kernel_env$forced_g
test_g_2 <- test_2$P$sub_kernel_env$forced_g

s_1 <- test$P$sub_kernel_env$s
s_2 <- test_2$P$sub_kernel_env$s

new_g <- discrete_extrema(test_g)
new_g_2 <- discrete_extrema(test_g_2)
new_P <- matrix(0, 50, 50)
new_P_2 <- matrix(0, 50, 50)

for(i in 1:50) new_P[ ,i] <- s_1[i] * new_g[ ,i]
for(i in 1:50) new_P_2[ ,i] <- s_2[i] * new_g_2[ ,i]

Re(eigen(new_P)$values)[1]
Re(eigen(new_P_2)$values)[1]

new_K_1 <- new_P +
  v * g * q_a * test$F$sub_kernel_env$expr +
  test$C$sub_kernel_env$expr

new_actual_1 <- Re(eigen(new_K_1)$values)[1]

new_K_2 <- new_P_2 +
  v * g * q_a * test_2$F$sub_kernel_env$expr +
  test_2$C$sub_kernel_env$expr

new_actual_2 <- Re(eigen(new_K_2)$values)[1]

(new_actual_1 - target_1)/target_1

(new_actual_2 - target_2)/target_2
