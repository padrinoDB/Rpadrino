#' @title make_k
#'
#' @param ipm An object of class \code{ipm_system} or \code{ipm_kernels}.
#' @param proto_ipm an object of class \code{proto_ipm}. This should be the same
#' \code{proto_ipm} used to make the \code{ipm}.
#' @param ... additional arguments ADD MORE AS NEEDED
#'
#' @return For each distinct ipm, a list of length 2: \code{K} is the iteration
#' kernel, and \code{sub_kernels} are the components of it. If a model has multiple
#' Ks, then they will be named according to the hierarchical effects that create
#' them (e.g. for a model with a year effect, K_2000, K_2001, K_2002, etc). Each
#' part of the list will have the two slots described above.
#'
#'
#' @export


make_k <- function(ipm, ...) {

  UseMethod('make_k')
}

#' @rdname make_k
#' @export
make_k.ipm_kernels <- function(ipm, proto_ipm, ...) {

  new_ipm <- make_ipm(proto_ipm, ..., return_all = TRUE)

  .make_k_impl(new_ipm, proto_ipm, ...)

}

#' @rdname make_k
#' @export
make_k.ipm_system <- function(ipm, proto_ipm, ...) {

  .make_k_impl(ipm, proto_ipm, ...)

}

#' @rdname make_k
#'
#'
.make_k_impl <- function(ipm, proto_ipm,
                         domains = NULL,
                         domain_lower = NULL,
                         domain_upper = NULL,
                         mesh_points = NULL) {

  data_envs <- ipm$data_envs
  sub_kernels <- ipm$kernels

  domain_env <- .make_domain_env(proto_ipm,
                                 domains = domains,
                                 lower = domain_lower,
                                 upper = domain_upper,
                                 mesh_points = mesh_points)

  eval_envs <- .generate_kernel_envs(proto_ipm,
                                    data_envs,
                                    sub_kernels,
                                    domain_env)

  kernel_list <- .prep_kernel_quos(proto_ipm, eval_envs, sub_kernels)
  .bind_kernel_quos(kernel_list)

  subs_to_k <- .extract_high_level_kernels(kernel_list)
  # Name is currently hardcoded, but this needs to change for hierarchical models
  .bind_to_k_env(subs_to_k, kernel_list$K_all, "K_all")
  out <- .get_k_all(kernel_list$K_all$kern_env, "K_all")
  return(out)


}
