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
db <- RPadrino:::padrino_filter(padrino, ipm_id == 'a2b3c1')
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

new_domains <- RPadrino:::.extract_domains(proto_ipm)

# create environment to house domains separate from kernel evaluation
# environment
domain_env <- RPadrino:::.generate_domain_env(new_domains)

# Build environments for each sub-kernel so that evaluation is safer
sub_kernel_list <- list()

for(i in seq_len(length(proto_ipm$parameters[[1]]$K$kernel_flags))){

  kernel <- proto_ipm$parameters[[1]]$K$kernel_flags[i]
  out <- list(RPadrino:::.build_kernel_env(proto_ipm,
                                           kernel = kernel,
                                           domain_env = domain_env))
  names(out) <- kernel

  sub_kernel_list <- purrr::splice(sub_kernel_list, out)
}

# make sure to add cell size multiplier to all functions tagged "Integrated"

RPadrino:::.eval_kernels(sub_kernel_list)

