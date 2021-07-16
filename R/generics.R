# Generic functions

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


vital_rate_exprs.pdb_proto_ipm <- function(object) {

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

kernel_formulae.pdb_proto_ipm <- function(object) {

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

domains.pdb_proto_ipm <- function(object) {

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

parameters.pdb_proto_ipm <- function(object) {

  lapply(object, ipmr::parameters)

}

#' @rdname padrino_accessors
#' @export

parameters.pdb_ipm <- function(object) {

  lapply(object, function(x) ipmr::parameters(x$proto_ipm))

}

#' @rdname padrino_accessors
#' @importFrom ipmr pop_state
#' @export

pop_state.pdb_proto_ipm <- function(object) {

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
#' @param ... further arguments passed to \code{\link[ipmr]{lambda}}. Unused for
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
#' @importFrom ipmr right_ev left_ev
#' @export

right_ev.pdb_ipm <- function(ipm, iterations = 100, tolerance = 100, ...) {

  lapply(ipm,
         function(x, iterations, tolerance) {
           ipmr::right_ev(x, iterations = iterations, tolerance = tolerance)
         },
         iterations = iterations,
         tolerance = tolerance)

}

#' @rdname ipmr_generics
#' @export

left_ev.pdb_ipm <- function(ipm, iterations = 100, tolerance = 100, ...) {

  lapply(ipm,
         function(x, iterations, tolerance) {
           ipmr::left_ev(x, iterations = iterations, tolerance = tolerance)
         },
         iterations = iterations,
         tolerance = tolerance)

}
