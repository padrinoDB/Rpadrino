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


build_ipm <- function(proto_ipm, domains = NULL, mesh_points = NULL,
                      domain_lower = NULL, domain_upper = NULL,
                      eviction = c('discrete_extremes', 'truncated_distributions'),
                      truncated_bounds = NULL,
                      quad_routine = c('midpoint', 'trapezoid', 'Gauss-Legendre')) {

  new_domains <- .extract_domains(proto_ipm,
                                  domains,
                                  domain_lower,
                                  domain_upper,
                                  mesh_points)

  sub_kernel_envs <- .associate_vr_funs(...)

  unevaluated_kernels <- .associate_domains(...)

  evaluated_kernels <- .eval_kernels(...)

  return(evaluated_kernels)

}

.extract_domains <- function(proto_ipm, domains = NULL,
                             lower = NULL, upper = NULL,
                             mesh_points = NULL) {



  out <- vector('list', length = length(unique(proto_ipm$domain)))
  names(out) <- unique(proto_ipm$domain)
  if(is.null(domains)) {
    # extract domains from object
    for(i in seq_len(nrow(proto_ipm))) {
      out[[i]] <- c(proto_ipm$lower[i],
                    proto_ipm$upper[i],
                    proto_ipm$mesh_p[i])

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
# takes proto_ipm object and associates appropriate kernel expressions + parameters
# with environments for evaluation

.associate_vr_funs <- function(proto_ipm, kernel, domain) {
  kernel_LHS_RHS <- proto_ipm$parameters[[1]]$sub_kernels[[kernel]]
  kernel_tree <- proto_ipm$parameters[[1]]$parameters[[kernel]]

  domain_env <- .generate_domain(domain)
  eval_env <- rlang::child_env(.parent = domain_env)

  for(i in seq_len(length(kernel_tree))) {
    sub_tree <- kernel_tree[[i]]
    if(sub_tree$int_eval == 'Integrated') {
      # if integrated, need to replace math symbols w/ appropriate R function
      sub_tree$vr_exprs[[1]] <- .replace_w_dens_fun(sub_tree$vr_exprs[[1]])
    } else if(sub_tree$int_eval == 'Evaluated'){
      # add expression and values to evaluation environment


      rlang::env_bind(eval_env,
                      )


    } else {
      # if it's null, then that means we need to move down a level.
      top_expr <- sub_tree$vr_exprs[[1]]

      sub_tree <- sub_tree$sub_expr

    }

  }

}



#' @inheritParams build_ipm
#' @rdname internal
#' @importFrom purrr map
#' @importFrom rlang child_env global_env env_bind is_list
# takes list from .extract_domains
# generate numeric vectors for each domain, return them in the appropriate env

.generate_domain <- function(domain) {

  if(!rlang::is_list(domain)) domain <- list(domain)
  dom <- purrr::map(domain, .f = function(x) seq(x[1],
                                                 x[2],
                                                 length.out = x[3]))
  # out <- list()


  out <- rlang::child_env(.parent = rlang::global_env())

  temp_domain <- expand.grid(temp_1 = dom[[1]],
                             temp_2 = dom[[1]])

  names(temp_domain) <- paste(names(domain), 1:2, sep = '_')



  rlang::env_bind(out, domain = temp_domain)

  return(out)
}

