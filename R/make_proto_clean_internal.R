# define a LHS_RHS matrix for math expressions in Padrino

.LHS_RHS_mat <- function(text, split = '[=]') {

  LHS_RHS <- .split_trim(text, split)

  # remove text bracketed text from RHS stuff
  LHS_RHS <- vapply(LHS_RHS,
                    FUN = function (x) gsub( "\\[.*?\\]", "", x),
                    FUN.VALUE = character(1))


  out <- matrix(LHS_RHS, ncol = 2, byrow = TRUE)

  return(out)

}

.split_trim <- function(text, splitter, perl = FALSE) {

  split_str <- strsplit(text, split = splitter, perl = perl)
  out <- lapply(split_str,
                FUN = function(x) trimws(x))


  return(unlist(out))
}

# Determine if a function is univariate, bivariate, or neither
.is_bivariate <- function(LHS_RHS_mat, domain_table) {
  state_vars <- paste(domain_table$state_variable, 1:2, sep = '_')

  out <- logical(dim(LHS_RHS_mat)[1])
  for(i in seq_len(dim(LHS_RHS_mat)[1])){
    LHS_test <- grepl(state_vars[1], LHS_RHS_mat[i, 1]) &
      grepl(state_vars[2], LHS_RHS_mat[i, 1])

    RHS_test <- grepl(state_vars[1], LHS_RHS_mat[i, 2]) &
      grepl(state_vars[2], LHS_RHS_mat[i, 2])

    if(RHS_test | LHS_test) {
      out[i] <- TRUE
    } else {
      out[i] <- FALSE
    }
  }

  return(out)

}

.vr_ids <- function(text) {
  gsub('\\s*\\([^\\)]+\\)', "", text)

}

.substituted_or_evaluated <- function(kernel, param_table) {
  param_table$model_type[grepl(kernel, param_table$kernel_id)]
}

.get_param_values <- function(text, param_table) {

  # Exact_vars may be misleading as I think there's still potential for
  # fuzzy matching, but this is the best I can come up with in base R.

  exact_vars <- paste('\\b',param_table$parameter_name, '\\b', sep = "")

  param_index <- vapply(exact_vars,
                        FUN = function(x) grepl(x, text),
                        FUN.VALUE = logical(1))

  params <- param_table$parameter_value[param_index]
  names(params) <- param_table$parameter_name[param_index]
  return(params)

}

.add_kernel_to_proto <- function(proto,
                                 ipm_id,
                                 kernel_id,
                                 kernel_tree,
                                 metadata,
                                 domains,
                                 quad_rules) {

  domain_table_ind <- .identify_kernels(kernel_id,
                                        domains$kernel_id)

  quad_rule_ind <- .identify_kernels(kernel_id,
                                     quad_rules$kernel_id)

  domain <- ifelse(length(domain_table_ind) == 0,
                   NA,
                   domains$domain[domain_table_ind])
  upper <- ifelse(length(domain_table_ind) == 0,
                  NA,
                  domains$upper[domain_table_ind])
  lower <- ifelse(length(domain_table_ind) == 0,
                  NA,
                  domains$lower[domain_table_ind])
  state_var <- ifelse(length(domain_table_ind) == 0,
                      NA,
                      domains$state_variable[domain_table_ind])
  quad_rule <- ifelse(length(quad_rule_ind) == 0,
                      NA,
                      quad_rules$integration_rule[quad_rule_ind])
  mesh_p <- ifelse(length(quad_rule_ind) == 0,
                   NA,
                   quad_rules$n_meshpoints[quad_rule_ind])

  temp <- tibble::tibble(
    ipm_id = ipm_id,
    kernel = kernel_id,
    domain = domain,
    lower = lower,
    upper = upper,
    state_variable = state_var,
    quad_rule = quad_rule,
    mesh_p = mesh_p,
    evict = as.logical(toupper(metadata$eviction_used)),
    evict_type = metadata$evict_type,
    parameters = list(kernel_tree)
  )

  out <- rbind(proto, temp)

  return(out)
}

# identifies which kernels are associated with a variable when they
# are separeted with a semicolon
.identify_kernels <- function(flag, kernels) {
  x <- unlist(strsplit(kernels,';'))
  x <- vapply(x, FUN = function(y) trimws(y), FUN.VALUE = character(1))
  if(!is.matrix(x)) {
    x <- matrix(c(x, rep(NA_character_, length(x))),
                ncol = length(x),
                nrow = 2,
                byrow = TRUE)
  }

  ind <- apply(x,
               MARGIN = 2,
               FUN =  function(y, flag) which(y == flag),
               flag = flag)
  ind <- unlist(ind)
  out <- unique(ind)
  return(out)

}



