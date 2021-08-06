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
