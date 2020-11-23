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
#' @importFrom ipmr vital_rates
#' @export


vital_rates.pdb_proto_ipm <- function(object) {

  lapply(object, vital_rates)

}

#' @rdname padrino_accessors
#' @export

vital_rates.pdb_ipm <- function(object) {

  lapply(object, function(x) vital_rates(x$proto_ipm))

}


#' @rdname padrino_accessors
#' @importFrom ipmr kernel_formulae
#' @export

kernel_formulae.pdb_proto_ipm <- function(object) {

  lapply(object, kernel_formulae)

}

#' @rdname padrino_accessors
#' @export

kernel_formulae.pdb_ipm <- function(object) {

  lapply(object, function(x) kernel_formulae(x$proto_ipm))

}



#' @rdname padrino_accessors
#' @export
#' @importFrom ipmr domains

domains.pdb_proto_ipm <- function(object) {

  lapply(object, domains)

}

#' @rdname padrino_accessors
#' @export

domains.pdb_ipm <- function(object) {

  lapply(object, function(x) domains(x$proto_ipm))

}


#' @rdname padrino_accessors
#' @importFrom ipmr parameters
#' @export

parameters.pdb_proto_ipm <- function(object) {

  lapply(object, parameters)

}

#' @rdname padrino_accessors
#' @export

parameters.pdb_ipm <- function(object) {

  lapply(object, function(x) parameters(x$proto_ipm))

}

#' @rdname padrino_accessors
#' @importFrom ipmr pop_state
#' @export

pop_state.pdb_proto_ipm <- function(object) {

  lapply(object, pop_state)

}

#' @rdname padrino_accessors
#' @export

pop_state.pdb_ipm <- function(object) {

  lapply(object, pop_state)

}

