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
#' @export


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
                                             domains,
                                             domain_lower,
                                             domain_upper,
                                             mesh_points)

  # create environment to house domains separate from kernel evaluation
  # environment
  domain_env <- RPadrino:::.generate_domain_env(new_domains)

  # Build environments for each sub-kernel so that evaluation is safer
  sub_kernel_list <- list()

  for(i in seq_len(nrow(proto_ipm))) {

    kernel <- proto_ipm$kernel[i]
    out <- list(RPadrino:::.build_kernel_env(proto_ipm,
                                             kernel = kernel,
                                             domain_env = domain_env))
    names(out) <- kernel

    sub_kernel_list <- purrr::splice(sub_kernel_list, out)
  }

  # Evaluate them!
  RPadrino:::.eval_vr_exprs(sub_kernel_list, domain_env)

  # Combine evaluated vr exprs to form the kernel mesh!
  evaluated_kernels <- .create_sub_kernels(proto_ipm,
                                           sub_kernel_list,
                                           domain_env)

  # Turing test completed?
  return(evaluated_kernels)

}

.create_sub_kernels <- function(proto_ipm, sub_kernel_list, domain_env) {

  kernel_list <- list()

  for(i in seq_along(sub_kernel_list)) {

    # make sure indices match between proto_ipm and sub_kernel_list
    j <- which(proto_ipm$kernel == names(sub_kernel_list)[i])

    evict <- as.logical(toupper(proto_ipm$evict[j]))

    # generate quosures for each kernel exprssion
    raw_text <- rlang::quo_text(proto_ipm$K[[j]]$expr)
    kernel_expr <- RPadrino:::.prep_kernel_exprs(raw_text,
                                                 sub_kernel_list[[i]]$eval_info)

    out_quo <- rlang::quo_set_expr(proto_ipm$K[[j]]$expr,
                                   kernel_expr)

    quo_env <- sub_kernel_list[[i]]$eval_info
    out_quo <- rlang::quo_set_env(out_quo, quo_env)

    # Now, we generate and evaluate an expression to correct eviction for
    # discrete_extrema
    if(proto_ipm$kernel[j] == 'P' & evict) {

      evict_type <- proto_ipm$evict_type[j]

      # consider encapsulating with function
      # force g so that it can be evaluated
      force_g <- rlang::quo(force(g))
      force_g <- rlang::quo_set_env(force_g, sub_kernel_list[[i]]$eval_info)

      expr_2 <- paste('g <- RPadrino::',
                      evict_type,
                      '(forced_g)', sep = "")

      expr_2 <- rlang::parse_expr(expr_2)
      expr_2 <- rlang::enquo(expr_2)
      expr_2 <- rlang::quo_set_env(expr_2, sub_kernel_list[[i]]$eval_info)

      expr_list <- list(forced_g = force_g,
                        expr_2 = expr_2)

      rlang::env_bind_exprs(.env = sub_kernel_list[[i]]$eval_info,
                            !!! expr_list,
                            .eval_env = sub_kernel_list[[i]]$eval_info)
    }

    out_list <- list(sub_kernel_env = sub_kernel_list[[i]]$eval_info,
                     quos = list(expr = out_quo))

    kernel_list <- purrr::splice(kernel_list,
                                 list(out_list))

    names(kernel_list)[i] <- proto_ipm$kernel[j]

  # hold off on evaluating the complete K kernel (or other fully discrete expressions)
    if(proto_ipm$K[[j]]$type %in% c('IPM_CC', 'IPM_DD',
                                    'IPM_DC', 'IPM_CD')) {

      # rlang::eval_tidy(kernel_list[[i]]$quos)

      rlang::env_bind_exprs(.env = kernel_list[[i]]$sub_kernel_env,
                           !!! kernel_list[[i]]$quos,
                           .eval_env = kernel_list[[i]]$sub_kernel_env)

      force(kernel_list[[i]]$quos)
    }

  } # end kernel loop

  return(kernel_list)
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
#' \code{Padrino} database. Additionally, anecdotal reports suggest that this
#' doesnt \emph{completely} correct for eviction as remainders of
#' \code{1 - colSums(mat)} are occasionally between \code{1e-4} and \code{1e-6}
#' on the cells near the border of the function domain.
#'
#' @export

discrete_extrema <- function(mat) {
  for(i in seq_len(nrow(mat))) {
    if(i < (nrow(mat) / 2)){
      mat[1, i] <- mat[1, i] + (1 - colSums(mat)[i])
    } else {
      mat[nrow(mat), i] <- mat[nrow(mat), i] + (1 - colSums(mat)[i])
    }
  }

  return(mat)

}

.prep_kernel_exprs <- function(text, eval_env) {

  out <- stringr::str_replace_all(text, '\\s*\\[[^\\]]+\\]', '') %>%
    stringr::str_replace_all('[=]', '<-')

  out <- .orient_operations(out, eval_env)
  out <- rlang::parse_expr(out)
  return(out)

}

.orient_operations <- function(text, eval_env) {

  # Identify which things go into expression
  LHS <- stringr::str_split(text, ' <- ', simplify = TRUE)[1]
  RHS <- stringr::str_split(text, ' <- ', simplify = TRUE)[2]

  RHS_terms <- stringr::str_split(RHS,
                                  '[*+-/]',
                                  simplify = TRUE) %>%
    stringr::str_trim()

  # check if any of the terms in RHS are matrices
  form_ind <- eapply(eval_env, dim)
  eval_ind <- base::Filter(f = function(y) !is.null(y),
                       x = form_ind) %>% names()

  form_obj <- RHS_terms[RHS_terms %in% eval_ind]

  # If everything is a constant or vector, just return the text string
  if(length(form_obj) < 1) return(text)

  # otherwise, substitute in double transposition on RHS and return modified
  # string
  sub_text <- paste('t(', form_obj, ')', sep = "")
  RHS <- gsub(form_obj, sub_text, RHS)

  out <- paste(LHS, ' <- t(', RHS, ')', sep = "")

  return(out)
}

.eval_vr_exprs <- function(sub_kernel_list, domain_env) {
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

    unique_domains <- unique(proto_ipm$domain)
    unique_domains <- unique_domains[!is.na(unique_domains)]

    # extract all unique domains from proto_ipm
    for(i in seq_along(unique_domains)) {

      # index for each domain. We only need to extract each one once, even
      # though it's repeated in the proto_ipm data structure

      j <- which(proto_ipm$domain == unique_domains[i])[1]

      nm <- proto_ipm$domain[j]
      sv <- proto_ipm$state_variable[j]

      out[[nm]] <- c(proto_ipm$lower[i],
                     proto_ipm$upper[i],
                     proto_ipm$mesh_p[i],
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



#' @inheritParams build_ipm
#' @rdname internal
#'
# takes proto_ipm objects, moves parameter values into appropriate evaluation
# environment, and generates expressions to generate the kernel. The latter are
# stored in a list of quosures that can be bound to the environment later on

.build_kernel_env <- function(proto_ipm, kernel, domain_env) {

  j <- which(proto_ipm$kernel == kernel)

  kernel_LHS_RHS <- proto_ipm$parameters[[j]]$sub_kernels[[kernel]]
  kernel_tree <- proto_ipm$parameters[[j]]$parameters[[kernel]]

  LHS <- RPadrino:::.prep_LHS_exprs(kernel_LHS_RHS)
  RHS <- kernel_LHS_RHS[ , 2]
  all_exprs <- paste(LHS,
                     RHS,
                     sep = ' <- ')

  eval_env <- rlang::child_env(.parent = domain_env)

  out <- list()

  out_exprs <- list()

  names_iterator <- 1
  for(i in seq_along(kernel_tree)) {

    sub_tree <- kernel_tree[[i]]

    rlang::env_bind(eval_env, !!! as.list(sub_tree$values))

    top_expr <- all_exprs[i]

    # if the expression is just designating a constant, then skip storing
    # the expression as this will cause some infinite recursion at evaluation
    # time. We've already bound the value of the variable to the evaluation
    # environment (^^^env_bind(as.list(sub_tree$values))) so it will still be
    # available

    if(LHS[i] == RHS[i]) next

    # if it's flagged w/ integrated, that means we need to transform it into
    # a pdf using our internal dictionary. If not, then the expression
    # **should** be ready for evaluation straight away

    if(sub_tree$int_eval == 'Integrated') {
      top_expr <- RPadrino:::.prep_dens_fun(top_expr, proto_ipm$state_variable[j])
    }

    if(sub_tree$int_eval == 'Evaluated') {
      top_expr <- RPadrino:::.prep_other_fun(top_expr,
                                             proto_ipm$state_variable[j],
                                             proto_ipm$mesh_p[j])
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

.prep_other_fun <- function(top_expr, state_var, n_meshp) {
  # examine select correct state_variable from top_expr
  state_var <- paste(state_var, 1:2, sep = '_')
  state_var_1 <- state_var[1]
  state_var_2 <- state_var[2]

  LHS <- stringr::str_split(top_expr, ' <- ', simplify = TRUE)[1]
  RHS <- stringr::str_split(top_expr, ' <- ', simplify = TRUE)[2]

  if(grepl(state_var_1, top_expr)) {
    state_var <- state_var_1

    ind_seq <- "1:n_meshp"

   } else if(grepl(state_var_2, top_expr)) {
    state_var <- state_var_2

    ind_seq <- "c(1, seq((n_meshp + 1), n_meshp^2, by = n_meshp))"

  } else {
    # In this case, an expression that has no state variable defined for it
    # just return that and keep going (i.e. constant variance in growth
    # (sd_g <- growth_sd))
    return(top_expr)
  }

  new_RHS <- paste(RHS, '[c(', ind_seq, ')]', sep = "")

  top_expr <- paste(LHS,
                    ' <- ',
                    new_RHS,
                    sep = "")

  return(top_expr)

}

# turns ascii math pdf into text string of R expression
.prep_dens_fun <- function(top_expr, state_var) {
  # convert expr to text for manipulation, update state variable.
  # I think we're always doing the T+1, so I'm hardcoding this for now...
  # Probably is not actually correct though.
  state_var <- paste(state_var, '_2', sep = "")
  LHS <- stringr::str_split(top_expr, ' <- ', simplify = TRUE)[1]
  RHS <- stringr::str_split(top_expr, ' <- ', simplify = TRUE)[2]

  # get appropriate density function from internal dictionary
  fun <- stringr::str_extract(RHS, names(internal_data$pr_dens_dictionary))
  fun_for_splitting <- fun[!is.na(fun)]
  fun_for_using <- internal_data$pr_dens_dictionary[[fun_for_splitting]]

  fun_for_splitting <- paste(fun_for_splitting, '\\(',sep = '')

  fun_args <- stringr::str_split(top_expr,
                                 fun_for_splitting,
                                 simplify = TRUE)[2]

  fun_for_using <- paste(fun_for_using,
                         '(',
                         state_var,
                         ', ',
                         sep = '')

  out <- paste(LHS, ' <- ',
               't(matrix(c(',
               fun_for_using,
               fun_args,
               ' * cell_size), nrow = n_meshp, ncol = n_meshp))',
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
#' @importFrom rlang child_env global_env env_bind is_list !!!
# takes list from .extract_domains
# generate numeric vectors for each domain, return them in the appropriate env

.generate_domain_env <- function(domain) {

  if(!rlang::is_list(domain)) domain <- list(domain)

  dom <- purrr::map(domain, .f = function(x) seq(as.numeric(x[1]),
                                                 as.numeric(x[2]),
                                                 length.out = as.integer(x[3]) + 1))
  # out <- list()

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

  }



  return(out)
}









