#' @noRd
#

.check_make_ipm_args_impl <- function(proto, args) {

  UseMethod(".check_make_ipm_args_impl")

}

#' @noRd
.check_make_ipm_args_impl.simple_di_det <- function(proto, args) {


  pos_args <- c("return_main_env",
                "return_all_envs",
                "usr_funs",
                "domain_list",
                "iterate",
                "iterations",
                "normalize_pop_size")

  cur_args <- names(args[[1]])

  if(any(! cur_args %in% pos_args)) {

    .stop_bad_make_ipm_args(cur_args, pos_args, "simple, deterministic IPMs")

  }

  return(TRUE)

}

#' @noRd
.check_make_ipm_args_impl.simple_dd_det <-
  .check_make_ipm_args_impl.simple_di_det

#' @noRd
.check_make_ipm_args_impl.simple_di_stoch_kern <- function(proto, args) {

  pos_args <- c("return_main_env",
                "return_all_envs",
                "usr_funs",
                "domain_list",
                "iterate",
                "iterations",
                "kernel_seq",
                "normalize_pop_size",
                "report_progress",
                "return_sub_kernels")

  cur_args <- names(args[[1]])

  if(any(! cur_args %in% pos_args)) {

    .stop_bad_make_ipm_args(cur_args,
                            pos_args,
                            "simple, stochastic kernel-resampled IPMs")

  }

  return(TRUE)

}

#' @noRd
.check_make_ipm_args_impl.simple_dd_stoch_kern <-
  .check_make_ipm_args_impl.simple_di_stoch_kern

#' @noRd
.check_make_ipm_args_impl.simple_di_stoch_param <- function(proto, args) {

  pos_args <- c("return_main_env",
                "return_all_envs",
                "usr_funs",
                "domain_list",
                "iterate",
                "iterations",
                "kernel_seq",
                "normalize_pop_size",
                "report_progress",
                "return_sub_kernels")

  cur_args <- names(args[[1]])

  if(any(! cur_args %in% pos_args)) {

    .stop_bad_make_ipm_args(cur_args,
                            pos_args,
                            "simple, stochastic parameter-resampled IPMs")

  }

  return(TRUE)

}

#' @noRd
.check_make_ipm_args_impl.simple_dd_stoch_param <-
  .check_make_ipm_args_impl.simple_di_stoch_param

#' @noRd
.check_make_ipm_args_impl.general_di_det <- function(proto, args) {

  pos_args <- c("return_main_env",
                "return_all_envs",
                "usr_funs",
                "domain_list",
                "iterate",
                "iterations",
                "normalize_pop_size",
                "return_sub_kernels")

  cur_args <- names(args[[1]])

  if(any(! cur_args %in% pos_args)) {

    .stop_bad_make_ipm_args(cur_args, pos_args, "general, deterministic IPMs")

  }

  return(TRUE)

}

#' @noRd
.check_make_ipm_args_impl.general_dd_det <-
  .check_make_ipm_args_impl.general_di_det

#' @noRd
.check_make_ipm_args_impl.general_di_stoch_kern <- function(proto, args) {

  pos_args <- c("return_main_env",
                "return_all_envs",
                "usr_funs",
                "domain_list",
                "iterate",
                "iterations",
                "kernel_seq",
                "normalize_pop_size",
                "report_progress",
                "return_sub_kernels")

  cur_args <- names(args[[1]])

  if(any(! cur_args %in% pos_args)) {

    .stop_bad_make_ipm_args(cur_args,
                            pos_args,
                            "general, stochastic kernel-resampled IPMs")

  }

  return(TRUE)

}

#' @noRd
.check_make_ipm_args_impl.general_dd_stoch_kern <-
  .check_make_ipm_args_impl.general_di_stoch_kern

#' @noRd
.check_make_ipm_args_impl.general_di_stoch_param <- function(proto, args) {

  pos_args <- c("return_main_env",
                "return_all_envs",
                "usr_funs",
                "domain_list",
                "iterate",
                "iterations",
                "kernel_seq",
                "normalize_pop_size",
                "report_progress",
                "return_sub_kernels")

  cur_args <- names(args[[1]])

  if(any(! cur_args %in% pos_args)) {

    .stop_bad_make_ipm_args(cur_args,
                            pos_args,
                            "general, stochastic parameter-resampled IPMs")

  }

  return(TRUE)

}

#' @noRd
.check_make_ipm_args_impl.general_dd_stoch_param <-
  .check_make_ipm_args_impl.general_di_stoch_param

.stop_bad_make_ipm_args <- function(cur_args, pos_args, cls) {

  bad_ind <- which(!cur_args %in% pos_args)

  bad_args <- cur_args[bad_ind]

  bad_args <- paste(bad_args, collapse = ", ")

  stop(bad_args, " are not arguments for ", cls, ".\n",
       "See ?make_ipm for possible options.",
       call. = FALSE)
}
