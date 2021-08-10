# Generic functions

#' @rdname print_star
#' @title Print a \code{pdb} object.
#'
#' @param x A \code{pdb} object.
#' @param ... ignored
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

  cat("\nYou can inspect each model by printing it individually.")

  invisible(x)

}

#' @rdname padrino_accessors
#'
#' @title Accessor functions for (semi) built Padrino objects
#'
#' @description Padrino versions of \code{\link[ipmr]{accessors}}.
#'
#' @param object An object produced by \code{pdb_make_proto_ipm} or
#' \code{pdb_make_ipm}.
#'
#' @importFrom ipmr vital_rate_exprs
#' @export


vital_rate_exprs.pdb_proto_ipm_list <- function(object) {

  lapply(object, ipmr::vital_rate_exprs)

}

#' @rdname padrino_accessors
#' @export

vital_rate_exprs.pdb_ipm <- function(object) {

  lapply(object, function(x) ipmr::vital_rate_exprs(x$proto_ipm))

}


#' @rdname padrino_accessors
#' @importFrom ipmr kernel_formulae
#' @export

kernel_formulae.pdb_proto_ipm_list <- function(object) {

  lapply(object, ipmr::kernel_formulae)

}

#' @rdname padrino_accessors
#' @export

kernel_formulae.pdb_ipm <- function(object) {

  lapply(object, function(x) ipmr::kernel_formulae(x$proto_ipm))

}



#' @rdname padrino_accessors
#' @export
#' @importFrom ipmr domains

domains.pdb_proto_ipm_list <- function(object) {

  lapply(object, ipmr::domains)

}

#' @rdname padrino_accessors
#' @export

domains.pdb_ipm <- function(object) {

  lapply(object, function(x) ipmr::domains(x$proto_ipm))

}


#' @rdname padrino_accessors
#' @importFrom ipmr parameters
#' @export

parameters.pdb_proto_ipm_list <- function(object) {

  lapply(object, ipmr::parameters)

}

#' @rdname padrino_accessors
#' @export

parameters.pdb_ipm <- function(object) {

  lapply(object, function(x) ipmr::parameters(x$proto_ipm))

}

#' @rdname padrino_accessors
#'
#' @param value The value to insert. Should be a named list where the names
#' correspond to parameter names and the entries are new parameter values.
#' @export
#' @importFrom ipmr parameters<-

`parameters<-.pdb_proto_ipm_list` <- function(object, value) {

  lapply(object, function(x, value) {
    parameters(x) <- value
    return(x)
  },
  value = value)

}

#' @rdname padrino_accessors
#' @export

`parameters<-.pdb_ipm` <- function(object, value) {

  lapply(object, function(x, value) {
    parameters(x) <- value
    return(x)
  },
  values = value)

}

#' @rdname padrino_accessors
#' @importFrom ipmr pop_state
#' @export

pop_state.pdb_proto_ipm_list <- function(object) {

  lapply(object, ipmr::pop_state)

}

#' @rdname padrino_accessors
#' @export

pop_state.pdb_ipm <- function(object) {

  lapply(object, ipmr::pop_state)

}

#' @rdname ipmr_generics
#' @title Padrino methods for \code{ipmr} generics
#'
#' @description Provides wrappers around \code{ipmr} generic functions to extract
#' some quantities of interest from \code{pdb_ipm}s.
#'
#' @param ipm A \code{pdb_ipm}.
#' @param ... further arguments passed to \code{\link[ipmr]{lambda}} and to
#' \code{\link[ipmr]{conv_plot}}. Unused for
#' \code{left_ev} and \code{right_ev} - only present for compatibility.
#' @param iterations The number of times to iterate the model to reach
#' convergence. Default is 100.
#' @param tolerance Tolerance to evaluate convergence to asymptotic dynamics.
#'
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
#' @importFrom ipmr make_iter_kernel is_conv_to_asymptotic conv_plot
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
         function(x, iterations, tolerance) {
           ipmr::left_ev(x, iterations = iterations, tolerance = tolerance)
         },
         iterations = iterations,
         tolerance = tolerance)

}

#' @rdname ipmr_generics
#' @export

is_conv_to_asymptotic.pdb_ipm <- function(ipm, tolerance = 1e-10) {

  test_ind <- vapply(ipm, function(x, tol) {
    all(ipmr::is_conv_to_asymptotic(x, tolerance = tol))
  },
  logical(1L),
  tol = tolerance)

  if(!all(test_ind)) {

    msg <- paste0(paste(names(test_ind[!test_ind]), collapse = ", "),
                 " did not converge!")

    message(msg)

    return(FALSE)

  }

  return(TRUE)

}

#' @rdname ipmr_generics
#' @param log Log-transform lambdas?
#' @param show_stable Show horizontal line denoting stable population growth?
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
#' @param ... Named expressions representing the block kernel of the
#' IPMs in question. The names of each expression should be the \code{ipm_id},
#' and the expressions should take the form of \code{c(<upper_left>, <upper_right>,
#' <lower_left>, <lower_right>)} (i.e. a vector of symbols would create a matrix
#' in \strong{row-major order}). See examples.
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
