#' @noRd
#
.proto_check_stoch_possible <- function(db, det_stoch, kern_param) {



}

#' @noRd
#
.proto_check_param_possible <- function(db, det_stoch, kern_param) {



}


#' @noRd
# Checks if requested models are possible, and tries to fail informatively
# if they aren't

.check_proto_args <- function(pdb, ipm_id, det_stoch, kern_param, .stop) {

  mods    <- character()
  reasons <- character()

  # increments mods and reasons index. Modified from within each individual
  # error function using <<-
  it <- 0L

  for(i in unique(pdb[[1]]$ipm_id)) {

    use_db <- lapply(pdb,
                     function(x, temp_id) {

                       x[x$ipm_id == temp_id, ]

                     },
                     temp_id == i)

    # Error if stoch and no hier_effs or environmental vars

    temp_stoch_not_possible <- .proto_check_stoch_possible(use_db,
                                                           det_stoch,
                                                           kern_param)
    mods[it]                <- temp_stoch_not_possible[1]
    reasons[it]             <- temp_stoch_not_possible[2]

    # Error if stoch_param requested and no stored distribution info

    temp_param_not_possible <- .proto_check_param_possible(use_db,
                                                           det_stoch,
                                                           kern_param)
    mods[it]                <- temp_param_not_possible[1]
    reasons[it]             <- temp_param_not_possible[2]

    # Internal errors. Hopefully these don't come up, but we will need to
    # generate informative ones to aid trouble shooting.


  }

  if(.stop && length(mods) > 0) {

    stop("The following models produced the following errors:\n",
         paste(
           paste(
             mods, ": ", reasons, sep = ""
           ),
           collapse = "\n"
         ),
         call. = FALSE
    )

  } else if(length(mods) > 0) {

    warning("The following models produced the following errors:\n",
            paste(
              paste(
                mods, ": ", reasons, sep = ""
              ),
              collapse = "\n"
            ),
            call. = FALSE
    )
  }


  return(TRUE)

}
