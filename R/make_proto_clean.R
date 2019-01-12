#' @title generate a \code{proto_ipm} object for a given IPM from the \code{Padrino}
#' database
#'
#' @param db an object downloaded from the \code{Padrino} database
#' @param condition logical conditions to subset the database. These are passed to
#' \code{padrino_filter}. Useful for only building a subset of downloaded IPMs.
#'
#' @return A \code{tibble} of \code{proto_ipm} objects. Each IPM sub-kernel will
#'  have its own row in the tibble
#'
#' @importFrom tibble tibble
#' @importFrom rlang enquos is_empty
#' @export

make_proto_ipm <- function(db, ...) {

  out <- tibble::tibble(ipm_id = NA,
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

  filter_logic <- rlang::enquos(...)
  if(!rlang::is_empty(filter_logic)) {
    db <- padrino_filter(db, !!! filter_logic)
  }

  n_models <- length(unique(db[[1]]$ipm_id))

  for(i in seq_len(n_models)) {
    current_id <- unique(db[[1]]$ipm_id)[i]

    current_model <- padrino_filter(db, .data$ipm_id == current_id)

    # if the model has hierarchical effects, we need to substitute in those
    # for the generic expressions that are stored in the database
    if(.has_ranefs(current_model)) {

      current_model <- .split_ranefs(current_model)
    }

    kernel_ids <- unique(current_model[[8]]$kernel_id)

    domain_table <- current_model[[5]]
    quad_table <- current_model[[6]]
    metadata <- current_model[[1]]
    kernel_table <- current_model[[8]]
    vr_table <- current_model[[9]]
    param_table <- current_model[[10]]

    for(j in seq_along(kernel_ids)) {
      kernel_param_out <- list()

      kernel <- kernel_ids[j]

      # insert the kernel text expression and the model_family (CC,CD, DC, DD)
      # into the kernel specific list

      kernel_param_out$kernel_text <- kernel_table$formula[kernel_table$kernel_id == kernel]
      kernel_param_out$family <- kernel_table$model_family[kernel_table$kernel_id == kernel]

      # Extract the vital rate expressions for each kernel. for kernels
      # that are comprised of other kernels, then just use the kernel
      # text
      exact_kernel <- paste('\\b', kernel, '\\b', sep = "")
      vr_textexprs <- vr_table$formula[grep(exact_kernel, vr_table$kernel_id)]
      if(length(vr_textexprs) > 0) {
        kernel_text_mat <- .LHS_RHS_mat(vr_textexprs, split = '[=]')
      } else {
        kernel_text_mat <- .LHS_RHS_mat(kernel_param_out$kernel_text,
                                        split = '[=]')
      }

      # is_bivariate is used in build_ipm
      is_bivs <- .is_bivariate(kernel_text_mat, domain_table)
      vr_ids <- .vr_ids(kernel_text_mat[ , 1]) # names for the list entries
      sub_eval <- .substituted_or_evaluated(kernel, vr_table)

      for(k in seq_len(dim(kernel_text_mat)[1])) {
        temp <- list(vr_text = character(),
                     is_bivariate = logical(),
                     sub_eval = character(),
                     param_values = numeric())

        # insert vital rate expressions and find the parameter values
        # associated with it.
        temp$vr_text <- kernel_text_mat[k, 2]
        temp$is_bivariate <- is_bivs[k]
        temp$sub_eval <- sub_eval[k]
        temp$param_values <- .get_param_values(kernel_text_mat[k , 2],
                                               param_table)
        kernel_param_out[[k + 2]] <- temp
        names(kernel_param_out)[k + 2] <- vr_ids[k]

      } # end vital rate loop

      # build the proto_ipm tibble
      out <- .add_kernel_to_proto(out,
                                  current_id,
                                  kernel,
                                  kernel_param_out,
                                  metadata,
                                  domain_table,
                                  quad_table)

    } # End kernels loop

  }

  # Remove the dummy row used to initialize the proto_ipm object
  class(out) <- c('proto_ipm', class(out))
  return(out[-c(1), ])
}
