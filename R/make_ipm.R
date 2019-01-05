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
#' @param quad_routine The integration type to use. Currently, \code{midpoint} is
#' the default. Alternatives are \code{trapezoid} and \code{Gauss-Legendre}. Others
#' may be added later.
#' @param return_all Logical indicating whether to return the complete set of
#' expressions and environments or just the results of kernel evaluation (e.g.
#' the iteration matrices). Default is \code{TRUE}, but could cause large models
#' to get clunky. Set to \code{FALSE} if this is happening.
#'
#' @return An object of class \code{ipm_system}.
#'
#' @export


make_ipm <- function(proto_ipm,
                     domains = NULL,
                     mesh_points = NULL,
                     domain_lower = NULL,
                     domain_upper = NULL,
                     eviction = c('discrete_extremes',
                                  'truncated_distributions'),
                     truncated_bounds = NULL,
                     quad_routine = c('midpoint',
                                      'trapezoid',
                                      'Gauss-Legendre'),
                     return_all = TRUE) {

  dom_list <- .extract_domains(proto_ipm,
                               domains = domains,
                               lower = domain_lower,
                               upper = domain_upper,
                               mesh_points = mesh_points)

  domain_env <- .generate_domain_env(dom_list)

  sub_kernel_list <- RPadrino:::.generate_sub_kernels(proto_ipm, domain_env)

  RPadrino:::.bind_vr_quos(sub_kernel_list)

  # discrete_extrema is the only one implemented right now
  if(any(proto_ipm$evict)) {
    RPadrino:::.correct_eviction(sub_kernel_list)
  }

  sys <- .make_kernels(sub_kernel_list, proto_ipm)
  kerns <- list()
  for(i in seq_along(sys)){

    kerns[[i]] <- rlang::env_get(sys[[i]]$eval_env,
                                 names(sys)[i],
                                 default = NA_real_)
    names(kerns)[i] <- names(sys)[i]
  }

  if(return_all){
    out <- list(kernels = kerns,
                system = sys)
  } else {
    out <- kerns
  }

  class(out) <- c('ipm_system', class(out))
  return(out)

}
