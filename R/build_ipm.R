#' @title Construct an IPM from the proto_IPM object
#'
#' @param proto_ipm An object of class \code{proto_ipm}. This can come from either
#' the \code{Padrino} database or from a user specified set of functions supplied
#' in \code{IPMr}.
#' @param domains An alternative domain or set of domains to evaluate each function
#' in the \code{proto_ipm} object. If \code{NULL} (default), then uses the information
#' in the \code{proto_ipm} object to construct the IPM
#' @param mesh_points An integer specifying number of meshpoints to use for integration.
#' @param domain_lower The smallest value on the specified domain(s). If using
#' more than 1 domain, then specify a numeric vector the same length as \code{domains}.
#' @param domain_upper The largest value on the specified domain(s). If using
#' more than 1 domain, then specify a numeric vector the same length as \code{domains}.
#' @param eviction The method to correct for eviction. \code{discrete_extremes}
#' puts all evicted individuals back into the cells on the matrix edge.
#' \code{truncated_distributions} creates truncated probability density functions
#' for each one called in the IPM creation.
#' @param truncated_bounds The upper and lower bounds for truncation. If \code{NULL}
#' (default), uses the upper and lower bounds for the domains associated with
#' the probability density function.
#' @param quad_routine The quadrature routin to use. Currently, \code{midpoint} is
#' the default. Alternatives are \code{trapezoid} and \code{Gauss-Legendre}. Others
#' may be added later.
#'
#' @return An object of class \code{ipm}.
#'
#' @importFrom purrr splice


build_ipm <- function(proto_ipm,
                      domains = NULL,
                      mesh_points = NULL,
                      domain_lower = NULL,
                      domain_upper = NULL,
                      eviction = c('discrete_extremes',
                                   'truncated_distributions'),
                      truncated_bounds = NULL,
                      quad_routine = c('midpoint',
                                       'trapezoid',
                                       'Gauss-Legendre')) {

  # Extract domain information from proto and/or user function call
  new_domains <- RPadrino:::.extract_domains(proto_ipm,
                                             domain,
                                             domain_lower,
                                             domain_upper,
                                             mesh_points)

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

  # Evaluate them!
  evaluated_vrs <- .eval_vr_exprs(sub_kernel_list)

  # Combine evaluated vr exprs to form the kernel mesh!
  evaluated_kernels <- .create_sub_kernels(proto_ipm, sub_kernel_list)

  # combine them!
  complete_kernels <- .combine_sub_kernels(evaluated_kernels)

  # Turing test completed?
  return(complete_kernels)

}

.create_sub_kernels <- function(proto_ipm, sub_kernel_list) {

  kernel_list <- list()
  for(i in seq_len(length(proto_ipm$parameters[[1]]$K$kernel_flags))){
    # hold off on evaluating the full kernels for continuous and discrete
    # portions. Otherwise, evaluate ALL the things!
    if(!proto_ipm$parameters[[1]]$K[[i]]$type %in% c('IPM_CC', 'IPM_DD',
                                                     'IPM_DC', 'IPM_CD')) next

    raw_text <- rlang::quo_text(proto_ipm$parameters[[1]]$K[[i]]$expr)

    kernel_expr <- .prep_kernel_exprs(raw_text)

    out_quo <- rlang::quo_set_expr(proto_ipm$parameters[[1]]$K[[i]]$expr,
                                   kernel_expr)

    quo_env <- sub_kernel_list[[i]]$eval_info

    out_quo <- rlang::quo_set_env(out_quo, quo_env)

    # either stash out_quo in sub_kernel_list or proto_ipm. Make up your mind
  }



}

.prep_kernel_exprs <- function(text) {

  out <- stringr::str_replace_all(text, '\\s*\\([^\\)]+\\)', '') %>%
    stringr::str_replace_all('[=]', '<-')

  out <- rlang::parse_expr(out)
  return(out)


}

.eval_vr_exprs <- function(sub_kernel_list) {
  out <- list()

  for(i in seq_len(length(sub_kernel_list))) {

    # K gets evaluated in combine_sub_kernels, so we wait for that one
    if(names(sub_kernel_list)[i] == 'K') next


    rlang::env_bind_exprs(.env = sub_kernel_list[[i]]$eval_info,
                          !!! sub_kernel_list[[i]]$vr_quos,
                          .eval_env = sub_kernel_list[[i]]$eval_info)
  }

}

.extract_domains <- function(proto_ipm, domains = NULL,
                             lower = NULL, upper = NULL,
                             mesh_points = NULL) {



  out <- list()

  if(is.null(domains)) {
    # extract domains from object
    for(i in seq_len(nrow(proto_ipm))) {
      nm <- proto_ipm$domain[i]
      sv <- proto_ipm$state_variable[i]

      out[[nm]] <- c(proto_ipm$lower[i],
                     proto_ipm$upper[i],
                     proto_ipm$mesh_p[i],
                     sv)


    }
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



#' @inheritParams build_ipm
#' @rdname internal
#'
# takes proto_ipm objects, moves parameter values into appropriate evaluation
# environment, and generates expressions to generate the kernel. The latter are
# stored in a list of quosures that can be bound to the environment later on

.build_kernel_env <- function(proto_ipm, kernel, domain_env) {
  kernel_LHS_RHS <- proto_ipm$parameters[[1]]$sub_kernels[[kernel]]
  kernel_tree <- proto_ipm$parameters[[1]]$parameters[[kernel]]

  LHS <- RPadrino:::.prep_LHS_exprs(kernel_LHS_RHS)
  RHS <- kernel_LHS_RHS[ , 2]
  all_exprs <- paste(LHS,
                     RHS,
                     sep = ' <- ')

  eval_env <- rlang::child_env(.parent = domain_env)

  out <- list(domain_info = domain_env)

  out_exprs <- list()

  names_iterator <- 1
  for(i in seq_len(length(kernel_tree))) {

    sub_tree <- kernel_tree[[i]]

    rlang::env_bind(eval_env, !!! as.list(sub_tree$values))

    top_expr <- all_exprs[i]

    # if the expression is just designating a constant, then skip storing
    # the expression as this will cause some infinite recursion at evaluation
    # time. We've already bound the value of the variable to the evaluation
    # environment so it will still be available

    if(LHS[i] == RHS[i]) next

    # if it's flagged w/ integrated, that means we need to transform it into
    # a pdf using our internal dictionary. If not, then the expression
    # **should** be ready for evaluation straight away

    if(sub_tree$int_eval == 'Integrated') {
      top_expr <- .prep_dens_fun(top_expr, proto_ipm$state_variable)
    }

    expr <- rlang::parse_expr(top_expr)
    expr <- rlang::enquo(expr)
    expr <- rlang::quo_set_env(expr, eval_env)

    out_exprs <- purrr::splice(out_exprs, expr)
    names(out_exprs)[names_iterator] <- LHS[i]
    names_iterator <- names_iterator + 1


  } # end parameter for loop

  out$eval_info <- eval_env
  out$vr_quos <- out_exprs

  return(out)
}

# turns ascii math pdf into text string of R expression
.prep_dens_fun <- function(top_expr, state_var) {
  # convert expr to text for manipulation, update state variable.
  # I think we're always doing the T+1, so I'm hardcoding this for now...
  # Probably is not actually correct though.
  state_var <- paste(state_var, '_2', sep = "")

  # get appropriate density function from internal dictionary
  fun <- stringr::str_extract(top_expr, names(internal_data$pr_dens_dictionary))
  fun_for_splitting <- fun[!is.na(fun)]
  fun_for_using <- internal_data$pr_dens_dictionary[[fun_for_splitting]]

  fun_for_splitting <- paste(fun_for_splitting, '\\(',sep = '')

  intermediate_top_level <- stringr::str_split(top_expr,
                                               fun_for_splitting,
                                               simplify = TRUE)

  intermediate_low_level <- paste(fun_for_using,
                                  '(',
                                  state_var,
                                  ', ',
                                  sep = '')

  out <- paste(intermediate_top_level[1],
               intermediate_low_level,
               intermediate_top_level[2],
               sep = "")

  return(out)

}

.prep_LHS_exprs <- function(LHS_RHS_mat) {
  RHS <- LHS_RHS_mat[ , 1]

  out <- character(length(RHS))

  for(i in seq_len(length(RHS))) {
    out[i] <- RHS[i] %>%
      stringr::str_replace_all('\\s*\\([^\\)]+\\)', '')
  }

  return(out)

}

#' @inheritParams build_ipm
#' @rdname internal
#' @importFrom purrr map
#' @importFrom rlang child_env global_env env_bind is_list := !!! !! quo_text
# takes list from .extract_domains
# generate numeric vectors for each domain, return them in the appropriate env

.generate_domain_env <- function(domain) {

  if(!rlang::is_list(domain)) domain <- list(domain)
  dom <- purrr::map(domain, .f = function(x) seq(as.numeric(x[1]),
                                                 as.numeric(x[2]),
                                                 length.out = as.integer(x[3])))
  # out <- list()


  out <- rlang::child_env(.parent = rlang::global_env())

  for(i in seq_len(length(dom))){
    temp_domain <- expand.grid(temp_1 = dom[[i]],
                               temp_2 = dom[[i]])

    name <- domain[[i]][4]
    names(temp_domain) <- paste(name, 1:2, sep = '_')

    name <- rlang::enquo(name)



    rlang::env_bind(out,
                    !!! temp_domain)

  }



  return(out)
}


.append_kernel_rules <- function(proto_ipm, sub_kernel_list, db) {

  model_cols <- c("kernel", "model_family")
  ipm_families <- db[[8]][ , model_cols]

  for(i in unique(ipm_families$kernel)) {
    family <- ipm_families$model_family[ipm_families$kernel == i]
    sub_kernel_list[[i]]$family <- family

    # Figure out what's happening with sub-kernels. Top level K won't need
    # evaluation at the moment
    if(family %in% c("P", "F", "C")) {
      sub_kernel_list$eval_fun <- 'outer'
    } else if(grepl('CD', family)) {
      sub_kernel_list$eval_fun <- 'row'
    } else if(grepl('DC', family)) {
      sub_kernel_list$eval_fun <- 'col'
    } else if(grepl('DD', family)) {
      sub_kernel_list$eval_fun <- 'top_left_corner'
    }
  }




}






