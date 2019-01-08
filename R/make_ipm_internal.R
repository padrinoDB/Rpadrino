
.generate_sub_kernels <- function(proto_ipm, domain_env) {

  n_kernels <- dim(proto_ipm)[1]

  # store outputs, initialize index for it as well
  out <- list()
  out_ind <- 1

  for(i in seq_len(n_kernels)) {
    # K gets built later so this is skipped for now
    if(proto_ipm$parameters[[i]]$family %in% c('IPM_Discrete',
                                               'IPM_Full',
                                               'IPM_Cont')) next

    kern_env <- rlang::child_env(.parent = domain_env)
    out_quos <- list()
    kernel_param_tree <- proto_ipm$parameters[[i]]

    # first two slots in this list aren't vr functions
    n_funs <-  length(kernel_param_tree) - 2

    # still need an index for the total length
    ll <- length(kernel_param_tree)
    .bind_params(kernel_param_tree, kern_env, ll) # make_ipm_internal.R
    is_trunc <- grepl('truncated', proto_ipm$evict_type[i])
    out_quo_ind <- 1

    for(j in seq_len(n_funs)) {

      text_expr <- kernel_param_tree[[j + 2]]$vr_text

      # We don't want to accidentally bind a parameter that has an actual
      # value to it's own name (infinite recursion problem!!!), so skip if the
      # expression in question is just something like recr_sd = recr_sd

      if(text_expr == names(kernel_param_tree)[[j + 2]]) next

      is_biv <- kernel_param_tree[[j + 2]]$is_bivariate
      sub_eval <- kernel_param_tree[[j + 2]]$sub_eval

      text_expr <- .check_vr_text(text_expr,
                                  sub_eval,
                                  is_biv,
                                  is_trunc,
                                  proto_ipm$parameters[[i]]$family,
                                  proto_ipm$state_variable[i],
                                  domain_env$n_meshp,
                                  proto_ipm$kernel[i])

      # ready to quote vital rate expressions, assign environments, and
      # store everything to prepare for evaluation

      out_quo_j <- .prep_quo(text_expr, kern_env) # make_k_internal.R
      out_quos[[out_quo_ind]] <- out_quo_j
      names(out_quos)[out_quo_ind] <- names(kernel_param_tree)[j + 2]
      out_quo_ind <- out_quo_ind + 1

    }

    # Make kernel level expr trees
    for_out <- list(kernel_text = kernel_param_tree$kernel_text,
                    eval_env = kern_env,
                    vr_quos = out_quos)
    out[[out_ind]] <- for_out

    names(out)[out_ind] <- proto_ipm$kernel[i]
    out_ind <- out_ind + 1
  }

  return(out)
}

#' @importFrom rlang env_get
.get_kernels <- function(ipm_sys) {

  kerns <- list()
  kern_it <- 1

  for(i in seq_along(ipm_sys)){

    # Higher level kernels (e.g. K or seed banks that are sums of IPM_CD/IPM_DD)
    # need to be evaluated in their own special environments.
    if(ipm_sys[[i]]$family %in% c('IPM_CC', 'IPM_DD', 'IPM_DC', 'IPM_CD')){

      kerns[[kern_it]] <- rlang::env_get(ipm_sys[[i]]$eval_env,
                                         names(ipm_sys)[i],
                                         default = NA_real_)
      names(kerns)[kern_it] <- names(ipm_sys)[i]
      kern_it <- kern_it + 1
    }
  }
  return(kerns)

}

.bind_params <- function(kernel_param_tree, kern_env, ll) {
  params <- lapply(3:ll,
                   function(x){
                     kernel_param_tree[[x]]$param_values
                   })

  all_params <- unlist(params)

  # Remove duplicates, env_bind only works with unique values.
  bind_params <- all_params[!duplicated(names(all_params))]

  if(length(bind_params) > 0){
    rlang::env_bind(kern_env,
                    !!! bind_params)
  }
}

.check_vr_text <- function(text,
                           sub_eval,
                           is_biv,
                           is_trunc,
                           family,
                           state_variable,
                           n_meshp, kernel) {

  if(sub_eval == 'Evaluated') {
    out <- text

  } else {
    out <- switch(as.character(is_trunc),
                  "TRUE" = .sub_trunc_dens_fun(text, kernel, state_variable),
                  "FALSE" = .sub_dens_fun(text, kernel, state_variable),
                  stop('Supplied an unknown type of eviction correction.',
                       call. = FALSE))


    if(is_biv & grepl('P|P_', kernel)) {
      out <- .wrap_matrix_call(out, n_meshp) # make_ipm_internal.R
    }
  }

  if(family %in% c('IPM_DC', 'IPM_CD')) {
    out <- .convert_to_vector(out, state_variable) # make_ipm_internal.R
  }

  return(out)
}

#' @title Correct for eviction
#' @rdname eviction
#'
#' @description Correct for eviction in an IPM matrix using either
#' \code{discrete_extrema} or \code{truncated_distribution}. The corrections
#' differ in subtle ways. See details for more information
#'
#' @param mat The growth matrix in need of correction
#'
#' @return A matrix of the same dimension as \code{mat}, but with columns summing
#' to 1.
#'
#' @details
#'
#' \code{discrete_extrema} creates discrete stages at either end of the
#' domain of the state variable in question. It does this by adding all
#' transition probabilities not included in the matrix into the top row (for cells
#' corresponding to the lower half of the domain) or bottom row (for cells
#' corresponding to the upper half of the domain).
#'
#' \code{truncated_distribution} works differently. This generates a truncated
#' probability density function using the \code{\link{truncdist}} package and is
#' called when creating the expressions for the probability density function
#' describing growth.
#'
#' Note the differences in when these functions are called. This may affect construction
#' of custom IPMs from raw data or altering existing IPMs downloaded from the
#' \code{Padrino} database. Additionally, some testing suggest that this
#' doesnt \emph{completely} correct for eviction as remainders of
#' \code{1 - colSums(mat)} are occasionally between \code{1e-4} and \code{1e-6}
#' on the cells near the border of the function domain. See the \code{Eviction}
#' vignette (\code{vignette('Eviction-correction', pakage = "RPadrino")}) for
#' more details.
#'
#' @export

discrete_extrema <- function(mat) {

  mat_cols <- colSums(mat)

  for(i in seq_len(nrow(mat))) {
    if(i < (nrow(mat) / 2)){
      mat[1, i] <- mat[1, i] + (1 - mat_cols[i])
    } else {
      mat[nrow(mat), i] <- mat[nrow(mat), i] + (1 - mat_cols[i])
    }
  }

  return(mat)

}

.sub_dens_fun <- function(text, kernel, state_variable) {

  # remove parentheses and stuff in between it
  expr_fun <- gsub('\\s*\\([^\\)]+\\)', '', text)

  # look up density function in internal dictionary
  fun <- internal_data$pr_dens_dictionary[[expr_fun]]

  # Hardcoding usage of second state variable. This may be wrong (if an ipm is
  # computing the pr density of state_var_1), but I haven't seen that done before
  # so I'm going to assume this isn't going to pose any problems for now.
  state_var <- paste(state_variable, '_2', sep = "")

  # Rebuild valid R text representation of expression. Note that cell_size is
  # stored in domain_env and should **always** be included when any density
  # function is evaluated

  new_expr <- paste('cell_size * ',
                    fun,
                    '(',
                    state_var,
                    ',',
                    sep = "")

  out <- gsub(paste(expr_fun,
                    '\\(',
                    sep = ""),
              new_expr,
              text)
  return(out)
}

.make_domain_env <- function(proto_ipm, domains, lower, upper, mesh_points) {

  dom_list <- .extract_domains(proto_ipm,
                               domains = domains,
                               lower = domain_lower,
                               upper = domain_upper,
                               mesh_points = mesh_points)

  out <- .generate_domain_env(dom_list) # make_ipm_internal.R

  return(out)
}

.wrap_matrix_call <- function(text, n_meshp) {

  paste('matrix(',
        text, ', nrow = ',
        n_meshp,
        ', byrow = TRUE)',
        sep = "")
}

# Only works on P's currently.
.correct_eviction <- function(sub_kernel_list) {

  kernels <- sub_kernel_list[grepl('P|P_', names(sub_kernel_list))]

  # finds the name of the growth kernel by searching for calls to matrix
  # in the text expression
  for(i in seq_along(kernels)) {
    kernel <- kernels[[i]]
    nm_g <- .find_g_name(kernel) # make_ipm_internal.R

    g_mat <- kernel$eval_env[[nm_g]]

    assign(nm_g, discrete_extrema(g_mat), envir = kernel$eval_env)

  }

}

.find_g_name <- function(kernel) {
  vr_exprs <- names(kernel$vr_quos)

  ind <- logical(length(vr_exprs))
  for(i in seq_along(vr_exprs)) {
    ind[i] <- grepl('matrix', rlang::quo_text(kernel$vr_quos[[i]]))

  }

  names(kernel$vr_quos)[ind]
}

.bind_vr_quos <- function(sub_kernel_list) {

  for(i in seq_along(sub_kernel_list)) {

    rlang::env_bind_lazy(.env = sub_kernel_list[[i]]$eval_env,
                         !!! sub_kernel_list[[i]]$vr_quos,
                         .eval_env = sub_kernel_list[[i]]$eval_env)
  }

}

.make_kernels <- function(sub_kernel_list, proto_ipm) {


  for(i in seq_along(sub_kernel_list)) {
    kernel <- names(sub_kernel_list)[i]
    proto_ind <- which(proto_ipm$kernel == kernel)
    kern_fam <- proto_ipm$parameters[[proto_ind]]$family

    text_expr <- .prep_kernel_expr(sub_kernel_list[[i]], # make_ipm_internal.R
                                   kernel,
                                   kern_fam,
                                   proto_ipm$state_variable[i],
                                   proto_ipm$mesh_p[proto_ind])

    out_quo <- .prep_quo(text_expr, sub_kernel_list[[i]]$eval_env) # make_k_internal.R
    sub_kernel_list[[i]]$kernel_text <- NULL
    sub_kernel_list[[i]]$kernel_quo <- list(out_quo)
    names(sub_kernel_list[[i]]$kernel_quo) <- kernel
    sub_kernel_list[[i]]$family <- kern_fam

    if(kern_fam %in% c('IPM_CC', 'IPM_DD', 'IPM_DC', 'IPM_CD')){

      rlang::env_bind_lazy(sub_kernel_list[[i]]$eval_env,
                           !!! sub_kernel_list[[i]]$kernel_quo,
                           .eval_env = sub_kernel_list[[i]]$eval_env)
    }
  }

  return(sub_kernel_list)

}

.prep_kernel_expr <- function(kernel_list,
                              kernel_name,
                              family,
                              state_var,
                              n_meshp) {

  text <- kernel_list$kernel_text

  # start turning it into an evaluatable expression
  dropped_brackets <- gsub('\\s*\\[[^\\]]+\\]', '', text, perl = TRUE)

  LHS_RHS <- .LHS_RHS_mat(dropped_brackets, split = '[=]') # make_proto_clean_internal.R
  RHS <- LHS_RHS[ ,2]

  # if it's a growth kernel, we have to do some weird stuff to make sure
  # the s * g part is done correctly.
  if(grepl('P|P_', kernel_name)) {

    nm_g <- RPadrino:::.find_g_name(kernel_list)

    RHS <- gsub(nm_g,
                paste('as.vector(t(', nm_g, '))', sep = ''),
                RHS)

  }

  if(family == 'IPM_CC'){
    RHS <- .wrap_matrix_call(RHS, n_meshp)
  } else if(family %in% c('IPM_CD', 'IPM_DC')) {
    RHS <- .convert_to_vector(RHS, state_var)
  }

  return(RHS)

}

.convert_to_vector <- function(text, state_var) {
  sv_1 <- paste(state_var, '_1', sep = "")
  sv_2 <- paste(state_var, '_2', sep = "")
  sv_vec <- paste(state_var, '_vec', sep = "")

  text_1 <- gsub(sv_1, sv_vec, text)
  out <- gsub(sv_2, sv_vec, text_1)

  return(out)

}

.extract_domains <- function(proto_ipm, domains = NULL,
                             lower = NULL, upper = NULL,
                             mesh_points = NULL) {



  out <- list()

  if(is.null(domains)) {

    unique_domains <- unique(proto_ipm$domain)
    unique_domains <- unique_domains[!is.na(unique_domains)]

    # extract all unique domains from proto_ipm
    for(i in seq_along(unique_domains)) {

      # index for each domain. We only need to extract each one once, even
      # though it's repeated in the proto_ipm data structure

      j <- which(proto_ipm$domain == unique_domains[i])[1]

      nm <- proto_ipm$domain[j]
      sv <- proto_ipm$state_variable[j]

      out[[nm]] <- c(proto_ipm$lower[j],
                     proto_ipm$upper[j],
                     proto_ipm$mesh_p[j],
                     sv)


    } # end unique domain extraction

  } else { # ignore for now, see to making sure author specified things work first.


    # BROKEN DO NOT USE ------------------------------------------
    if(any(!domains %in% proto_ipm$domain)) {
      # I don't think users should/could be able to introduce new domains, only
      # manipulate existing ones.
      stop('User specified domains must have the same names as the\n',
           '"proto_ipm"-specified ones.',
           call. = FALSE)
    }
    # figure out which ones are user specified vs internal. create user specified
    # ones and then extract remaining ones
    usr_domains <- domains # user specified ones get a new, better name :)
    aut_domains <- proto_ipm$domain # author specified ones get a new, less cool name!

    all_domains <- unique(domains, proto_ipm$domain)

    for(i in seq_len(length(domains))) {


    } # end domain override loop
  } # end domain creation if-else

  return(out)
}


.prep_other_fun <- function(top_expr, state_var) {


}



.prep_LHS_exprs <- function(LHS_RHS_mat) {
  RHS <- LHS_RHS_mat[ , 1]

  out <- character(length(RHS))

  for(i in seq_len(length(RHS))) {
    out[i] <- gsub('\\s*\\([^\\)]+\\)', '', RHS[i])
  }

  return(out)

}

#' @inheritParams build_ipm
#' @rdname internal
#' @importFrom purrr map
#' @importFrom rlang child_env global_env env_bind is_list !!!
# takes list from .extract_domains
# generate numeric vectors for each domain, return them in the appropriate env

.generate_domain_env <- function(domain) {

  if(!rlang::is_list(domain)) domain <- list(domain)

  dom <- purrr::map(domain, .f = function(x) seq(as.numeric(x[1]),
                                                 as.numeric(x[2]),
                                                 length.out = as.integer(x[3]) + 1))

  mids <- purrr::map(dom, .f = function(x) {
    l <- length(x) - 1
    out_domain <- 0.5 * (x[1:l] + x[2:(l + 1)])
    return(out_domain)

  })

  out <- rlang::child_env(.parent = rlang::global_env())

  for(i in seq_len(length(mids))){
    temp_domain <- expand.grid(temp_1 = mids[[i]],
                               temp_2 = mids[[i]])

    name <- domain[[i]][4]
    names(temp_domain) <- paste(name, 1:2, sep = '_')


    rlang::env_bind(out,
                    !!! temp_domain)

    temp_cell_size <- list(cell_size = mids[[i]][2] - mids[[i]][1],
                           n_meshp = as.integer(domain[[i]][3]))

    rlang::env_bind(out,
                    !!! temp_cell_size)

    assign(paste(name, '_vec', sep = ""),
           value = unique(mids[[i]]),
           envir = out)

  }

  return(out)
}

