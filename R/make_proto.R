#' @title Generate proto_ipms from Padrino objects
#'
#' @description This function generates \code{proto_ipm} objects from
#' Padrino Database tables.
#'
#' @param pdb A \code{pdb} object.
#' @param ipm_id Optionally, one or more \code{ipm_id}'s to build. If empty,
#' all models contained in the \code{pdb} object will be processed into
#' \code{proto_ipm}'s.
#' @param det_stoch A vector containing either \code{"det"} or \code{"stoch"}.
#' This determines whether we want to construct a deterministic or stochastic
#' model. Default is \code{"det"}. See details
#' @param kern_param If \code{det_stoch = "stoch"}, then whether or not to construct
#' a kernel resampled model, or a parameter resampled model. See details.
#'
#'
#' @return A list containing one or more \code{proto_ipms}. Names of the list
#' will correspond to \code{ipm_id}s.
#'
#' @details \code{proto_ipm} objects contain all of the information needed
#' to implement an IPM, but stop short of actually generating kernels. These
#' are intermediate building blocks that can be modified before creating a full
#' IPM so that things like perturbation analysis are a bit more straightforward.
#'
#' When requesting many models, the \code{det_stoch} and \code{kern_param} parameters
#' can also be vectors. These are matched with \code{ipm_id} by position. If the
#' lengths of \code{det_stoch} and \code{kern_param} do not match the length
#' \code{ipm_id}, they will be recycled until they do.
#'
#' For stochastic models, there is sometimes the option of building either a kernel-resampled
#' or a parameter resampled model. A kernel resampled model uses some point estimate
#' for time and/or space varying parameters to generate kernels for each year/site/grouping factor.
#' Parameter resampled models sample parameters from distributions. Padrino stores this
#' information for some models when it is available in the literature, and tries
#' to fail informatively when these distributions aren't available in the database.
#'
#' @seealso
#' For more info on \code{kern_param} definitions:
#'
#' Metcalf \emph{et al.} (2015). Statistial modeling of annual variation
#' for inference on stochastic population dynamics using Integral Projection Models.
#' \emph{Methods in Ecology and Evolution}. DOI: 10.1111/2041-210X.12405
#'
#' @export

pdb_make_proto_ipm <- function(pdb,
                               ipm_id = NULL,
                               det_stoch = "det",
                               kern_param = "kern") {


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

  # Recycle det_stoch so it is the correct length.

  if(length(det_stoch) < length(unique_ids)) {

    det_stoch <- rep_len(det_stoch, length.out = length(unique_ids))

  }

  if(length(kern_param) < length(det_stoch) && any(det_stoch == "stoch")) {

    kern_param <- rep_len(kern_param, length.out = length(det_stoch))

    kern_param[det_stoch == "det"] <- NA_character_
  }

  for(i in seq_along(unique_ids)) {

    out[[i]]      <- .make_proto(pdb,
                                 id = unique_ids[i],
                                 det_stoch[i],
                                 kern_param[i])

    attr(out[[i]], "species_accepted") <- pdb$Metadata$species_accepted[i]

    names(out)[i] <- unique_ids[i]

    out[[i]]$id   <- unique_ids[i]

  }

  class(out) <- c("pdb_proto_ipm_list", "list")

  return(out)

}
