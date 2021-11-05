#' @noRd
#
.proto_check_stoch_possible <- function(db, det_stoch, kern_param) {

  # Check that there's either something in he_tab and/or env_vars tab



  return("OK")

}

#' @noRd
#
.proto_check_param_possible <- function(db, det_stoch, kern_param) {

  # check if parameter distributions exist

  return("OK")

}


#' @noRd
# Checks if requested models are possible, and tries to fail informatively
# if they aren't

.check_proto_args <- function(use_db, det_stoch, kern_param) {

  mods <- unique(use_db$Metadata$ipm_id)

  # error if has_time_lag, these are not implemented yet

  if(use_db[[1]]$has_time_lag) {

    stop("Time-lagged models are not yet implemented",
         call. = FALSE)

  }

  # Error if stoch and no hier_effs or environmental vars

  problems <- .proto_check_stoch_possible(use_db,
                                         det_stoch,
                                         kern_param)


  # Error if stoch_param requested and no stored distribution info

  temp_param_not_possible <- .proto_check_param_possible(use_db,
                                                         det_stoch,
                                                         kern_param)
  problems <- c(problems, temp_param_not_possible)

  # Internal errors. Hopefully these don't come up, but we will need to
  # generate informative ones to aid trouble shooting.

  if(any(problems != "OK")) {

    stop("The following models produced the following errors:\n",
         paste(
           paste(
             mods, ": ", problems, sep = ""
           ),
           collapse = "\n"
         ),
         call. = FALSE
    )

  }


  return(TRUE)

}
