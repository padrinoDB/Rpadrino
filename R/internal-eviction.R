#' @noRd
# Adds additional arguments to specified eviction correction functions.

.add_ev_args <- function(ev_fun, ev_target, dens_fun_exprs, state) {

  extra_args <- switch(
    ev_fun,
    "truncated_distributions" = .add_trunc_dist_args(dens_fun_exprs,
                                                     ev_target),
    "rescale_kernel" = .add_trunc_dist_args(dens_fun_exprs,
                                            ev_target),
    "discrete_extrema" = .add_disc_extrema_call(ev_target, state)
  )

  return(extra_args)
}
#' @noRd

.add_trunc_dist_args <- function(dens_fun_expr, ev_target) {

  dens_fun <- strsplit(dens_fun_expr, ' = ')[[1]][2] %>%
    trimws() %>%
    rlang::parse_expr() %>%
    rlang::call_name() %>%
    tolower()

  out <- list(fun = dens_fun, target = ev_target)

  return(out)
}

#' @noRd

.add_disc_extrema_call <- function(ev_target, state) {

  list(target = ev_target, state = eval(unlist(state, use.names = FALSE)))

}
