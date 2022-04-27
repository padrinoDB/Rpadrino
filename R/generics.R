# Generic functions

#' @rdname print_star
#' @title Print a \code{pdb} object.
#'
#' @param x A \code{pdb} object.
#' @param ... Only used by \code{pdb_new_fun_form}, otherwise ignored. See details
#' and examples for usage in \code{pdb_new_fun_form}.
#'
#' @return \code{x} invisibly.
#'
#' @export

print.pdb <- function(x, ...) {

  md <- x$Metadata

  if(nrow(md) == 0) {

    return("This 'pdb' object is empty!")

  }

  n_sp <- length(unique(md$species_accepted))
  n_pu <- length(unique(md$apa_citation))
  n_mo <- nrow(md)

  msg <- c("A 'pdb' object with ", n_sp, " unique species, ", n_pu, " publications,",
           " and ", n_mo, " models.\nPlease cite all publications used in an analysis!",
           " These can be accessed with\n'pdb_citations()'.\n\n")

  ev_tab <- x$EnvironmentalVariables

  if(nrow(ev_tab) > 0 ) {

    n_stoch <- length(unique(ev_tab$ipm_id))
    ids     <- unique(ev_tab$ipm_id)

    mods <- paste(ids, collapse = ", ")

    msg <- c(msg,
             "The following models have continuously varying environments: \n",
             mods,
             "\nThese can take longer to re-build - adjust your expectations accordingly!\n")

  }

  cat(msg, sep = "")
  invisible(x)
}

#' @rdname print_star
#' @export

print.pdb_proto_ipm_list <- function(x, ...) {

  spps <- vapply(x, function(x) attr(x, "species_accepted"), character(1L)) %>%
    gsub(pattern = "_", replacement = " ", x = .)

  cat("This list of 'proto_ipm's contains the following species: ", spps, sep = "\n")

  cat("\nYou can inspect each model by printing it individually.\n\n")

  invisible(x)

}

#' @rdname ipmr_generics
#'
#' @title Padrino methods for `ipmr` generic functions
#'
#' @description Provides wrappers around \code{ipmr} generic functions to extract
#' some quantities of interest from \code{pdb_proto_ipm_list}s and \code{pdb_ipm}s.
#'
#' @param object An object produced by \code{pdb_make_proto_ipm} or
#' \code{pdb_make_ipm}.
#' @param ipm A \code{pdb_ipm} object.
#' @param ... Usage depends on the function - see Details and Examples.
#' @param value The value to insert. See details and Examples.
#' @param full_mesh Logical. Return the complete set of meshpoints or only the
#' unique ones.
#' @param name_ps For \code{pdb_ipm} objects that contain \code{age_x_size} IPMs,
#' a named list. The names of the list should be the \code{ipm_id}s that are
#' \code{age_x_size} models, and the values in the list should be the the name
#' of the survival/growth kernels.
#' @param f_forms For \code{pdb_ipm} objects that contain \code{age_x_size} IPMs,
#' a named list. The names of the list should be the \code{ipm_id}s that are
#' \code{age_x_size} models, and the values in the list should be the the name
#' of the fecundity kernels. If multiple sub-kernels contribute to fecundity, we
#' can also supply a string specifying how they are combined (e.g.
#' \code{f_forms = "F + C"}).
#' @param log Log-transform lambdas for plotting?
#' @param show_stable Show horizontal line denoting stable population growth?
#' @param ipm A \code{pdb_ipm}.
#' @param iterations The number of times to iterate the model to reach
#' convergence. Default is 100.
#' @param tolerance Tolerance to evaluate convergence to asymptotic dynamics.
#' @param kernel Ignored, present for compatibility with \code{ipmr}.
#' @param vital_rate Ignored, present for compatibility with \code{ipmr}.
#'
#'
#' @details There are number of uses for \code{...} which depend on the function
#' used for them. These are described below.
#'
#' @return Most of these return named lists where names correspond to
#' \code{ipm_ids}. The exception is \code{pdb_new_fun_form}, which returns a list
#' of expressions. It is only intended for setting new expressions with
#' \code{vital_rate_exprs<-}.
#'
#'
#' @section \code{pdb_new_fun_form}:
#'
#' This must be used when setting new expressions for
#' vital rates and kernel formulae. The \code{...} argument should be a named list
#' of named lists. The top most layer should be \code{ipm_id}'s. The next layer
#' should be a list where the names are vital rates you wish to modify, and the
#' values are the expressions you want to insert. See examples.
#'
#' @section \code{make_iter_kernel}:
#'
#' The \code{...} here should be expressions representing the block kernel of
#' the IPMs in question. The names of each expression should be the ipm_id,
#' and the expressions should take the form of \code{c(<upper_left>,
#' <upper_right>, <lower_left>, <lower_right>)}
#' (i.e. a vector of symbols would create a matrix in row-major order).
#' See examples.
#'
#' @section \code{conv_plot}/\code{lambda}:
#'
#' The \code{...} are used pass additional arguments to \code{\link[ipmr]{lambda}}
#' and \code{\link[ipmr]{conv_plot}}.
#'
#' @examples
#'
#' data(pdb)
#' my_pdb <- pdb_make_proto_ipm(pdb, c("aaaa17", "aaa310"))
#'
#' # These values will be appended to the parameter list for each IPM, as they
#' # aren't currently present in them.
#'
#' parameters(my_pdb) <- list(
#'   aaa310 = list(
#'     g_slope_2 = 0.0001,
#'     establishment_prob = 0.02
#'   ),
#'   aaaa17 = list(
#'     g_var = 4.2,
#'     germ_prob = 0.3
#'   )
#' )
#'
#' # We can overwrite a parameter value with a new one as well. Old values aren't
#' # saved anywhere except in the pdb object, so be careful!
#'
#' parameters(my_pdb) <- list(
#'   aaa310 = list(
#'     s_s    = 0.93, # old value is 0.92
#'     gvar_i = 0.13 # old value is 0.127
#'   )
#' )
#'
#' vital_rate_exprs(my_pdb) <- pdb_new_fun_form(
#'     list(
#'       aaa310 = list(mu_g = g_int + g_slope * size_1 + g_slope_2 * size_1^2),
#'       aaaa17 = list(sigmax2 = sqrt(g_var * exp(cfv1 + cfv2 * size_1))
#'      )
#'    )
#'  )
#'
#'  kernel_formulae(my_pdb) <- pdb_new_fun_form(
#'    list(
#'      aaaa17 = list(Y = recr_size * yearling_s * germ_prob * d_size),
#'      aaa310 = list(F = f_n * f_d * establishment_prob)
#'    )
#'  )
#'
#'  my_ipms    <- pdb_make_ipm(my_pdb)
#'  iter_kerns <- make_iter_kernel(my_ipms, aaaa17 = c(0, F_yr, Y, P_yr))
#'
#' @importFrom ipmr vital_rate_exprs
#' @export


vital_rate_exprs.pdb_proto_ipm_list <- function(object) {

  lapply(object, ipmr::vital_rate_exprs)

}

#' @rdname ipmr_generics
#' @export

vital_rate_exprs.pdb_ipm <- function(object) {

  lapply(object, function(x) ipmr::vital_rate_exprs(x$proto_ipm))

}

#' @rdname ipmr_generics
#' @importFrom ipmr kernel_formulae
#' @export

kernel_formulae.pdb_proto_ipm_list <- function(object) {

  lapply(object, ipmr::kernel_formulae)

}

#' @rdname ipmr_generics
#' @export

kernel_formulae.pdb_ipm <- function(object) {

  lapply(object, function(x) ipmr::kernel_formulae(x$proto_ipm))

}

#' @rdname ipmr_generics
#' @export
#' @importFrom ipmr domains

domains.pdb_proto_ipm_list <- function(object) {

  lapply(object, ipmr::domains)

}

#' @rdname ipmr_generics
#' @export

domains.pdb_ipm <- function(object) {

  lapply(object, function(x) ipmr::domains(x$proto_ipm))

}


#' @rdname ipmr_generics
#' @importFrom ipmr parameters
#' @export

parameters.pdb_proto_ipm_list <- function(object) {

  lapply(object, ipmr::parameters)

}

#' @rdname ipmr_generics
#' @export

parameters.pdb_ipm <- function(object) {

  lapply(object, function(x) ipmr::parameters(x$proto_ipm))

}

#' @rdname ipmr_generics
#' @importFrom ipmr pop_state
#' @export

pop_state.pdb_proto_ipm_list <- function(object) {

  lapply(object, ipmr::pop_state)

}

#' @rdname ipmr_generics
#' @export

pop_state.pdb_ipm <- function(object) {

  lapply(object, ipmr::pop_state)

}

#' @rdname ipmr_generics
#' @importFrom ipmr vital_rate_funs
#' @export

vital_rate_funs.pdb_ipm <- function(ipm) {

  lapply(ipm, ipmr::vital_rate_funs)

}

#' @rdname ipmr_generics
#' @importFrom ipmr int_mesh
#' @export

int_mesh.pdb_ipm <- function(ipm, full_mesh = TRUE) {
  lapply(ipm,
         function(x, f_m) ipmr::int_mesh(x, full_mesh = f_m),
         f_m = full_mesh)
}

#' @rdname ipmr_generics
#' @importFrom ipmr lambda
#' @export

lambda.pdb_ipm <- function(ipm, ...) {

  addl_args <- list(...)


  out       <- lapply(ipm,
                      function(x, dots) {

                        all_args <- rlang::list2(ipm = x, !!! dots)

                        rlang::exec(ipmr::lambda, !!! all_args)

                      },
                      dots = addl_args)

  return(out)

}

#' @rdname ipmr_generics
#' @importFrom ipmr right_ev left_ev mean_kernel
#' @export

right_ev.pdb_ipm <- function(ipm, iterations = 100, tolerance = 1e-10, ...) {

  lapply(ipm,
         function(x, iterations, tolerance) {
           ipmr::right_ev(x, iterations = iterations, tolerance = tolerance)
         },
         iterations = iterations,
         tolerance = tolerance)

}

#' @rdname ipmr_generics
#' @export

left_ev.pdb_ipm <- function(ipm, iterations = 100, tolerance = 1e-10, ...) {

  lapply(ipm,
         function(x, iterations, tolerance, dots) {
           rlang::exec(
             ipmr::left_ev,
             ipm        = x,
             iterations = iterations,
             tolerance  = tolerance,
             !!! dots
           )
         },
         iterations = iterations,
         tolerance = tolerance,
         dots = list(...))

}

#' @rdname ipmr_generics
#' @param burn_in The proportion of iterations to discard as burn in when
#' assessing convergence.
#' @importFrom ipmr make_iter_kernel is_conv_to_asymptotic conv_plot
#' @export

is_conv_to_asymptotic.pdb_ipm <- function(ipm,
                                          tolerance = 1e-10,
                                          burn_in = 0.1) {

  test_ind <- vapply(ipm, function(x, tol, burn_in) {
    all(ipmr::is_conv_to_asymptotic(x, tolerance = tol, burn_in = burn_in))
  },
  logical(1L),
  tol     = tolerance,
  burn_in = burn_in)

  if(!all(test_ind)) {

    msg <- strwrap(
      paste0(
        "The following IPMs did not converge: \n",
        paste(
          names(
            test_ind[!test_ind]
          ),
          collapse = ", "
        )
      ),
      width = 80L,
      initial = "",
      prefix = "\n"
    )

    message(msg)

    return(FALSE)

  }

  return(TRUE)

}

#' @rdname ipmr_generics
#' @export

conv_plot.pdb_ipm <- function(ipm,
                              iterations = NULL,
                              log = FALSE,
                              show_stable = TRUE,
                              ...) {

  for(i in seq_along(ipm)) {

    ipmr::conv_plot(ipm[[i]],
                    iterations = iterations,
                    log = log,
                    show_stable = show_stable,
                    main = names(ipm)[i],
                    ...)
  }

  invisible(ipm)
}

#' @rdname ipmr_generics
#' @export

make_iter_kernel.pdb_ipm <- function(ipm,
                                     ...,
                                     name_ps = NULL,
                                     f_forms = NULL) {

  mega_mat <- rlang::enexprs(...)

  .check_make_iter_kernel_args(ipm, mega_mat, name_ps, f_forms)

  out <- vector("list", length(ipm))

  for(i in seq_along(ipm)) {

    if(grepl("simple", class(ipm[[i]])[1])) {

      out[[i]] <- ipmr::make_iter_kernel(ipm[[i]])

    } else {

      use_mat <- rlang::expr(!! mega_mat[[names(ipm)[i]]])

      if("age_x_size" %in% class(ipm[[i]])) {
        use_ps <- name_ps[[names(ipm)[i]]]
        use_fs <- f_forms[[names(ipm)[i]]]
      } else {
        use_ps <- NULL
        use_fs <- NULL
      }

      out[[i]] <- ipmr::make_iter_kernel(ipm[[i]],
                                         mega_mat = !! use_mat,
                                         name_ps  = use_ps,
                                         f_forms  = use_fs)
    }

  }

  names(out) <- names(ipm)
  return(out)

}

#' @rdname ipmr_generics
#' @export

mean_kernel.pdb_ipm <- function(ipm) {

  keep_ind <- vapply(ipm, function(x) grepl("stoch", class(x)[1]), logical(1L))

  use_ipms <- ipm[keep_ind]

  if(!any(keep_ind)) {

    simple_nms <- paste(names(!keep_ind), collapse = ", ")

    message("The following IPMs are determinstic and mean kernels",
            " will not be computed: \n",
            simple_nms)

  }

  out <- lapply(use_ipms, ipmr::mean_kernel)

  return(out)

}

#' @rdname ipmr_generics
#' @export

pdb_new_fun_form <- function(...) {

  dots <- enquos(...)
  out <- lapply(dots, function(x) .quo_to_bares(!! x)) %>%
    .flatten_to_depth(1L)

  lapply(out, call_args)

}

#' @rdname ipmr_generics
#' @export
#' @importFrom ipmr parameters<-

`parameters<-.pdb_proto_ipm_list` <- function(object, ..., value) {

  for(i in seq_along(value)) {

    use_obj <- object[[names(value)[i]]]

    parameters(use_obj) <- value[[i]]

    object[[names(value)[i]]] <- use_obj
  }

  return(object)

}


#' @rdname ipmr_generics
#' @importFrom ipmr vital_rate_exprs<-
#' @export

`vital_rate_exprs<-.pdb_proto_ipm_list` <- function(object,
                                                    kernel = NULL,
                                                    vital_rate = NULL,
                                                    value) {

  # Outermost layer should be list of ipm_ids
  for(i in seq_along(value)) {

    # This is now a list of bare expressions, with names matching kernel names.
    use_forms <- value[[i]]

    use_obj   <- object[[names(value)[i]]]

    # ipmr::vital_rate_exprs only works on one kernel at a time, so loop over
    # fun forms to insert them all (if users want to modify many at once).

    for(j in seq_along(use_forms)) {

      vr_nm <- names(use_forms)[j]

      # Finally, we have to find the kernels that the requested vital rates appear
      # in. Similar to above, a user may wish to modify a vital rate that appears
      # in multiple kernels. Thus, we need to modify all of those, necessitating
      # the third for loop (for k in ...)

      all_vrs <- lapply(use_obj$params, function(x) names(x$vr_text))

      nm_ind <- vapply(all_vrs, function(all_vrs, vr) {
        vr %in% all_vrs
      }, logical(1L),
      vr = vr_nm)

      nm <- use_obj$kernel_id[nm_ind]

      for(k in seq_along(nm)) {

        ipmr::vital_rate_exprs(use_obj, kernel = nm[k], vr_nm) <- ipmr::new_fun_form(
          !! use_forms[[j]]
        )
      }
    }

    # re-insert modified kernel

    object[[names(value)[i]]] <- use_obj
  }

  return(object)

}


#' @rdname ipmr_generics
#' @importFrom ipmr kernel_formulae<-
#' @export

`kernel_formulae<-.pdb_proto_ipm_list` <- function(object, kernel, value) {

  # Outermost layer should be list of ipm_ids
  for(i in seq_along(value)) {

    # This is now a list of bare expressions, with names matching kernel names.
    use_forms <- value[[i]]

    use_obj   <- object[[names(value)[i]]]

    # ipmr::kernel_formulae only works on one kernel at a time, so loop over
    # fun forms to insert them all (if users want to modify many at once).

    for(j in seq_along(use_forms)) {

      nm <- names(use_forms)[j]
      ipmr::kernel_formulae(use_obj, kernel = nm) <- ipmr::new_fun_form(
        !!use_forms[[j]]
      )

    }

    # re-insert modified kernel

    object[[names(value)[i]]] <- use_obj
  }

  return(object)
}

#' @rdname ipmr_generics
#' @param x A \code{pdb_ipm} object.
#' @param i The index to extract
#' @export

`[.pdb_ipm` <- function(x, i) {

  out <- NextMethod()

  class(out) <- c("pdb_ipm", "list")

  return(out)

}

#' @noRd

.check_make_iter_kernel_args <- function(ipm, mega_mat, name_ps, f_forms) {

  if(!all(names(mega_mat) %in% names(ipm)) && !rlang::is_empty(mega_mat)) {
    stop("Mis-matched names between '...' and 'ipm' object!")
  }

  if(!all(names(f_forms) %in% names(ipm)) && !is.null(f_forms)) {
    stop("Mis-matched names between 'f_forms' and 'ipm' object!")
  }

  if(!all(names(mega_mat) %in% names(ipm)) && !is.null(name_ps)) {
    stop("Mis-matched names between 'name_ps' and 'ipm' object!")
  }

  if(!all(names(name_ps) %in% names(f_forms)) ||
     !all(names(f_forms) %in% names(name_ps))) {
    stop("All names 'f_forms' must also be in 'name_ps' (and vice versa)")
  }

  invisible(TRUE)
}

#' @noRd
#' @importFrom rlang quo_squash
# Function captures many nested expressions and converts them into a nested list.
# top level is ipm_id with expression names nested within those, and the expressions
# themselves as the values.

.quo_to_bares <- function(exprr) {

  exprr <- rlang::enquo(exprr)
  exprr <- rlang::quo_squash(exprr)

  rlang::call_args(exprr)
}
