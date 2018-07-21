#' @title generate a \code{proto_ipm} object for a given IPM from the \code{Padrino}
#' database
#'
#' @param db an object downloaded from the \code{Padrino} database
#' @param ipms The IPM IDs to build \code{proto_ipm} objects for
#'
#' @return A \code{tibble} of \code{proto_ipm} objects. Each IPM will have its
#' own row in the tibble
#'
#' @importFrom dplyr bind_rows tibble
#' @importFrom rlang quo !!
#' @export


generate_proto_ipm <- function(db, ipms = NULL) {

  id <- rlang::quo(ipm_id)

  out <- dplyr::tibble(ipm_id = NA,
                       domain = NA,
                       upper = NA,
                       lower = NA,
                       state_variable = NA,
                       quad_rule = NA,
                       mesh_p = NA,
                       parameters = NA)

  if(!is.null(ipms)) {

    db <- padrino_filter(db, !! id %in% ipms)
  }

  for(i in seq_len(length(unique(db[[1]]$ipm_id)))) {
    use_id <- unique(db[[1]]$ipm_id)[i]

    use_db <- padrino_filter(db, !! id == use_id)
    # Generate parsed K-kernel
    parsed_K <- RPadrino:::.parse_K_kernel(use_db)

    # generate parsed sub-kernels
    parsed_kernels <- RPadrino:::.kernel_vr_LHS_RHS_mat(use_db, parsed_K)

    # associate parameters with appropriate functions
    proto_ipm_params <- RPadrino:::.associate_vr_parameters(use_db, parsed_kernels)

    # bundle all this stuff togther
    full_set_parameters <- list(K = parsed_K,
                                sub_kernels = parsed_kernels,
                                parameters = proto_ipm_params)

    temp <- dplyr::tibble(ipm_id = use_id,
                          domain = use_db[[5]]$domain,
                          upper = use_db[[5]]$upper,
                          lower = use_db[[5]]$lower,
                          state_variable = use_db[[5]]$state_variable,
                          quad_rule = use_db[[6]]$integration_rule,
                          mesh_p = use_db[[6]]$n_meshpoints,
                          parameters = list(full_set_parameters))

    # include result, on to the next ipm
    out <- dplyr::bind_rows(out, temp)
  }

  # remove dummy row of NAs, set class
  out <- out[-c(1), ]

  class(out) <- c('proto_ipm', class(out))

  return(out)

}
