#' @noRd
#

.check_make_ipm_args <- function(proto_ipm_list, addl_args) {

  if(rlang::is_empty(addl_args)) return(TRUE)

  model_ids    <- names(proto_ipm_list)
  mod_arg_nms <- names(addl_args)

  if(!all(mod_arg_nms %in% model_ids)) {
    stop("Some 'ipm_id's in 'addl_args' do not match the 'ipm_id's in",
         "'proto_ipm_list'.")
  }

  out <- logical(length(model_ids))

  for(i in seq_along(model_ids)) {

    check_mod <- proto_ipm_list[[i]]

    # No additional arguments supplied

    if(!model_ids[i] %in% mod_arg_nms) {

      out[i] <- TRUE

    } else {

      ind    <- which(names(addl_args) == model_ids[i])

      args   <- addl_args[ind]

      out[i] <- .check_make_ipm_args_impl(check_mod, args)

    }

  }

  invisible(out)

}



#' @noRd
#

.make_ipm_calls <- function(proto_ipm_list, addl_args) {

  model_ids   <- names(proto_ipm_list)
  mod_arg_nms <- names(addl_args)

  out <- list()

  for(i in seq_along(proto_ipm_list)) {

    use_id <- model_ids[i]

    temp <- rlang::call2(ipmr::make_ipm, proto_ipm = proto_ipm_list[[i]])

    if(!use_id %in% mod_arg_nms) {

      # If no additional arguments, then stick the call into the list and move
      # to the next one

      out[[i]] <- temp

    } else {

      arg_ind <- which(names(addl_args) == use_id)

      use_args <- .flatten_to_depth(addl_args[arg_ind], 1L)

      temp <- rlang::call_modify(temp,
                                 !!! use_args,
                                 .homonyms = "error")

      out[[i]] <- temp

    }

    names(out)[i] <- use_id

  }

  return(out)

}

