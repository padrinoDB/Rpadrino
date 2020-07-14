#' @title Generate proto_ipms from Padrino objects
#'
#' @description These functions generate \code{proto_ipm} objects from
#' Padrino Database tables.
#'
#' @param pdb A \code{pdb} object
#' @param ipm_id Optionally, one or more \code{ipm_id}'s to build. If empty,
#' all models contained in the \code{pdb} object will be processed into
#' \code{proto_ipm}'s.
#'
#' @return A list containing one or more \code{proto_ipms}. Names of the list
#' will correspond to \code{ipm_id}s.
#'
#' @details \code{proto_ipm} objects contain all of the information needed
#' to implement an IPM, but stop short of actually generating kernels.
#'

make_proto_ipm <- function(pdb, ipm_id = NULL) {

  if(!is.null(ipm_id)) {

    pdb <- lapply(pdb, function(x, ipm_id) {

      out <- x[x$ipm_id %in% ipm_id, ]

      return(out)

    },
    ipm_id = ipm_id)

    # restore pdb class, which lapply strips away

    class(pdb) <- c("pdb", "list")

  }

  out        <- list()

  unique_ids <- unique(pdb[[1]]$ipm_id)

  for(i in seq_along(unique_ids)) {

    out[[i]]      <- .make_proto(pdb, id = unique_ids[i])

    names(out)[i] <- unique_ids[i]
  }

  return(out)

}

