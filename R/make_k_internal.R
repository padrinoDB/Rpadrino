.extract_high_level_protos <- function(proto_ipm) {

  types <- vapply(proto_ipm$parameters,
                  function(x) x$family,
                  FUN.VALUE = character(1))

  proto_ind <- which(types %in% c('IPM_Discrete',
                                  'IPM_Full',
                                  'IPM_Cont'))

  out <- proto_ipm[proto_ind, ]
}



.generate_kernel_envs <- function(proto_ipm,
                                  sys_data,
                                  sub_kernels,
                                  domain_env) {

  high_level_proto <- .extract_high_level_protos(proto_ipm)

  out <- list()
  for(i in seq_along(high_level_proto$parameters)) {

    kernel_tree <- high_level_proto$parameters[[i]]

    kern_env <- rlang::child_env(.parent = domain_env)
    ll <- length(kernel_tree)

    # This binds constants to the correct environment. Next, we need to bind
    # the kernels from make_ipm
    .bind_params(kernel_tree, kern_env, ll)

    # This set of functions finds the kernels included in kernel_tree$kernel_text
    # and then binds them to the correct environment. Since we've already built
    # these, there is no need to re-parse any of the vital rate level info.
    .bind_sub_kernels(kernel_tree, sys_data, sub_kernels, kern_env)

    out[[i]] <- kern_env
    names(out)[i] <- high_level_proto$kernel[i]

  }
  return(out)

}


.bind_sub_kernels <- function(kernel_tree, sys_data, sub_kernels, kern_env) {

  kern_text <- kernel_tree$kernel_text
  # Manipulate the RHS to figure out which kernels are where

  kern_ind <- .which_kernels_in_text(kern_text, sub_kernels)

  kerns_to_bind <- sub_kernels[kern_ind]

  if(length(kerns_to_bind) > 0) {
    rlang::env_bind(kern_env,
                    !!! kerns_to_bind)
  }

}

.prep_kernel_quos <- function(proto_ipm, eval_envs, sub_kernels) {

  high_level_proto <- .extract_high_level_protos(proto_ipm)

  out <- list()

  for(i in seq_along(high_level_proto$parameters)) {
    kern_text <- high_level_proto$parameters[[i]]$kernel_text
    kern_fam <- high_level_proto$parameters[[i]]$family

    out_text <- switch(kern_fam,
                       'IPM_Full' = .prep_ipm_full_text(kern_text),
                       'IPM_Cont' = .prep_ipm_cont_text(kern_text,
                                                        proto_ipm,
                                                        sub_kernels),
                       'IPM_Discrete' = .prep_ipm_disc_text(kern_text,
                                                            proto_ipm,
                                                            sub_kernels))

    out_quo <- .prep_quo(out_text, eval_envs[[i]])

    temp <- list(kern_quo = out_quo,
                 kern_env = eval_envs[[i]])
    out[[i]] <- temp
    names(out)[i] <- names(eval_envs)[i]
  }

  return(out)
}

.bind_kernel_quos <- function(kern_list) {
  for(i in seq_along(kern_list)) {

    # skip k_all, that gets bound last
    if(grepl('K_all', names(kern_list)[i])) next
    quo_to_bind <- list(kern_list[[i]]$kern_quo)
    names(quo_to_bind) <- names(kern_list)[i]

    rlang::env_bind_lazy(kern_list[[i]]$kern_env,
                         !!! quo_to_bind,
                         .eval_env = kern_list[[i]]$kern_env)

  }
}

.extract_high_level_kernels <- function(kern_list) {
  out <- list()
  it <- 1
  for(i in seq_along(kern_list)){
    if(!grepl('K_all', names(kern_list)[i])) {

      out[[it]] <- rlang::env_get(kern_list[[i]]$kern_env,
                                  names(kern_list)[i],
                                  default = NA_real_)
      names(out)[it] <- names(kern_list)[i]
      it <- it + 1
    }
  }
  return(out)
}

.bind_to_k_env <- function(kern_list, k_list, name_k) {

  rlang::env_bind(k_list$kern_env,
                  !!! kern_list)

  quo_to_bind <- list(k_list$kern_quo)
  names(quo_to_bind) <- name_k

  rlang::env_bind_lazy(k_list$kern_env,
                       !!! quo_to_bind,
                       .eval_env = k_list$kern_env)

}

.get_k_all <- function(k_all_env, nm) {
  rlang::env_get(k_all_env, nm, default = NA_real_)
}


.prep_ipm_full_text <- function(kern_text) {

  RHS <- RPadrino:::.split_trim(kern_text, '[=]')[2]
  no_math <- gsub('[\\*|\\/|\\+|\\-|]', ',', RHS, perl = TRUE)
  out <- paste('rbind(', no_math, ')', sep = "")

  return(out)
}

.prep_ipm_disc_text <- function(kern_text, proto_ipm, sub_kernels) {

  # Currently not used, but will likely be need to get ordering of multiple
  # discrete stages right... maybe?
  fams <- .kernels_and_families(kern_text, proto_ipm, sub_kernels)

  # Discrete stages *shouldn't* require additional math, as they should
  # have already been evaluated during make_ipm.
  RHS <- RPadrino:::.split_trim(kern_text, '[=]')[2]
  no_math <- gsub('[\\*|\\/|\\+|\\-|]', ',', RHS, perl = TRUE)
  out <- paste('matrix(c(',
               no_math,
               '), nrow = 1, byrow = TRUE)', sep = "")

  return(out)
}

.prep_ipm_cont_text <- function(kern_text, proto_ipm, sub_kernels) {

  fams <- .kernels_and_families(kern_text, proto_ipm, sub_kernels)

  LHS_RHS <- RPadrino:::.LHS_RHS_mat(kern_text, '[=]')

  # If all the kernels here are continuous to continuous, then we can just
  # evaluate the math.
  # if not, we need to substitute in a cbind to make sure discrete stages are
  # added correctly. The data should be entered with kernels assembled left to
  # to right, so  math = DC + CC becomes cbind(DC, CC_expr)

  if(all(fams == 'IPM_CC')) {
    out <- gsub('\\s*\\[[^\\]]+\\]', '', LHS_RHS[ ,2], perl = TRUE)
  } else if('IPM_DC' %in% fams) {

    cc_ind <- paste(names(fams[fams == 'IPM_CC']), collapse = '|')

    # find the first IPM_CC instance and extract that sub-string, it will not be
    # modified except to remove square brackets, etc

    split_ind <- gregexpr(cc_ind, kern_text)[[1]][1]

    # rcb = right side of cbind, lcb = left side cbind
    rcb <- substr(kern_text, split_ind, nchar(kern_text))
    lcb <- .split_trim(substr(kern_text, 1, split_ind - 1), '[=]')[2]

    rcb <- gsub('\\s*\\[[^\\]]+\\]', '', rcb, perl = TRUE)
    lcb <- gsub('\\s*\\[[^\\]]+\\]', '', lcb, perl = TRUE) # remove brackets
    lcb <- gsub('[\\-|\\+|\\*|\\/]', '', lcb, perl = TRUE) # remove any extra math

    out <- paste('cbind(', lcb, ', ', rcb, ')', sep = "")
  }

  return(out)
}

.kernels_and_families <- function(kern_text, proto_ipm, sub_kernels) {
  kerns_in_expr <- .which_kernels_in_text(kern_text, sub_kernels)
  kern_ind <- names(sub_kernels)[kerns_in_expr]

  out <- vapply(proto_ipm$parameters[proto_ipm$kernel %in% kern_ind],
                 function(x) x$family,
                 FUN.VALUE = character(1))

  names(out) <- proto_ipm$kernel[proto_ipm$kernel %in% kern_ind]

  return(out)
}

.which_kernels_in_text <- function(kern_text, sub_kernels) {

  kernel_RHS <- RPadrino:::.LHS_RHS_mat(kern_text, '[=]')[ ,2]
  cleaner_RHS <- gsub('\\s*\\[[^\\]]+\\]', '', kernel_RHS, perl = TRUE)
  RHS_terms <- RPadrino:::.split_trim(cleaner_RHS, '[\\-|\\+|\\*|\\/]', perl = TRUE)

  which(names(sub_kernels) %in% RHS_terms)

}


#' @importFrom rlang parse_expr enquo quo_set_env
#' @noRd
# final step in quosure preparation, parses, wraps, and sets eval environment
.prep_quo <- function(text, env_2_set) {

  out_expr <- rlang::parse_expr(text)
  out_quo_i <- rlang::enquo(out_expr)
  out_quo <- rlang::quo_set_env(out_quo_i, env_2_set)

  return(out_quo)
}

