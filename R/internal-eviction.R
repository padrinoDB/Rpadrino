#' @noRd
# Adds additional arguments to specified eviction correction functions.
# Right now, only works with truncated_distributions. Need to add discrete_extrema

.add_ev_args <- function(ev_fun, ev_target, dens_fun_exprs) {

  extra_args <- switch(
    ev_fun,
    "truncated_distributions" = .add_trunc_dist_args(dens_fun_exprs,
                                                     ev_target),
    "rescale_kernel" = .add_trunc_dist_args(dens_fun_exprs,
                                            ev_target),
    "discrete_extrema" = warning("discrete_extrema not",
                                 "implemented yet")
  )

  return(extra_args)
}

#' @noRd

.add_trunc_dist_args <- function(dens_fun_expr, ev_target) {

  dens_fun <- strsplit(dens_fun_expr, '=')[[1]][2] %>%
    trimws() %>%
    rlang::parse_expr() %>%
    rlang::call_name() %>%
    tolower()

  out <- list(dens_fun, ev_target)

  return(out)
}
