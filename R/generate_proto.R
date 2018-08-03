#' @title generate a \code{proto_ipm} object for a given IPM from the \code{Padrino}
#' database
#'
#' @param db an object downloaded from the \code{Padrino} database
#' @param ipms The IPM IDs to build \code{proto_ipm} objects for
#'
#' @return A \code{tibble} of \code{proto_ipm} objects. Each IPM sub-kernel will
#'  have its own row in the tibble
#'
#' @importFrom dplyr bind_rows tibble
#' @importFrom rlang quo .data
#' @export


generate_proto_ipm <- function(db, ipms = NULL) {

  out <- dplyr::tibble(ipm_id = NA,
                       kernel = NA,
                       domain = NA,
                       upper = NA,
                       lower = NA,
                       state_variable = NA,
                       quad_rule = NA,
                       mesh_p = NA,
                       evict = NA,
                       evict_type = NA,
                       parameters = NA)

  if(!is.null(ipms)) {

    db <- padrino_filter(db, .data$ipm_id %in% ipms)
  }

  for(i in seq_len(length(unique(db[[1]]$ipm_id)))) {
    use_id <- unique(db[[1]]$ipm_id)[i]

    use_db <- padrino_filter(db, .data$ipm_id == use_id)
    # Generate parsed K-kernel
    parsed_K <- RPadrino:::.parse_K_kernel(use_db)

    domain_table <- use_db[[5]]
    quad_table <- use_db[[6]]
    metadata_table <- use_db[[1]]

    for(j in seq_along(parsed_K$kernel_flags)) {
      # generate parsed sub-kernel
      parsed_kernel <- RPadrino:::.kernel_vr_LHS_RHS_mat(use_db,
                                                         parsed_K[[j]],
                                                         names(parsed_K)[j])

      # associate parameters with appropriate functions
      proto_ipm_params <- RPadrino:::.associate_vr_parameters(use_db, parsed_kernel)

      # bundle all this stuff togther
      full_set_parameters <- list(sub_kernels = parsed_kernel,
                                  parameters = proto_ipm_params)

      domain_table_ind <- .identify_kernels(names(parsed_K)[j],
                                            domain_table$kernel_id)
      quad_table_ind <- .identify_kernels(names(parsed_K)[j],
                                          quad_table$kernel_id)

      domain <- ifelse(length(domain_table_ind) == 0,
                      NA,
                      domain_table$domain[domain_table_ind])
      upper <- ifelse(length(domain_table_ind) == 0,
                     NA,
                     domain_table$upper[domain_table_ind])

      lower <- ifelse(length(domain_table_ind) == 0,
                     NA,
                     domain_table$lower[domain_table_ind])

      state_variable <- ifelse(length(domain_table_ind) == 0,
                              NA,
                              domain_table$state_variable[domain_table_ind])
      quad_rule <- ifelse(length(quad_table_ind) == 0,
                         NA,
                         quad_table$integration_rule[quad_table_ind])
      mesh_p <- ifelse(length(quad_table_ind) == 0,
                      NA,
                      quad_table$n_meshpoints[quad_table_ind])

      temp <- dplyr::tibble(ipm_id = use_id,
                            kernel = names(parsed_K)[j],
                            domain = domain,
                            upper = upper,
                            lower = lower,
                            state_variable = state_variable,
                            quad_rule = quad_rule,
                            mesh_p = mesh_p,
                            evict = metadata_table$eviction_used,
                            evict_type = metadata_table$evict_type,
                            parameters = list(full_set_parameters),
                            K = list(parsed_K[[names(parsed_K)[j]]]))

      # include result, on to the next ipm
      out <- dplyr::bind_rows(out, temp)
    } # sub-kernel loop
  } # ipm_id loop

  # remove dummy row of NAs, set class
  out <- out[-c(1), ]

  class(out) <- c('proto_ipm', class(out))

  return(out)

}
