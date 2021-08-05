#' @rdname in_out
#' @title Download PADRINO \code{pdb} objects
#'
#' @description Download PADRINO from Github.
#'
#' @param save Write the PDB object to a folder of csv files?
#' @param destination Where to write the \code{pdb} object to.
#'
#' @details This does not currently support versioning because there is only
#' one version. \code{destination} should be a folder name. When \code{save = TRUE},
#' a set of 12 csv files will be saved in the \code{destination} folder.
#'
#' @importFrom utils read.csv write.csv
#' @export

pdb_download <- function(save = TRUE, destination = NULL) {

  if(save && is.null(destination)) {

    stop("'destination' must be specified when 'save = TRUE'!")

  }

  # Capture forgotten trailing slash
  if(save && !is.null(destination)) {

    if(substr(destination,
              start = nchar(destination),
              stop = nchar(destination)) != "/") {
      destination <- paste(destination, "/", sep = "")
    }

  }

  tab_nms <- c("Metadata", "StateVariables", "DiscreteStates", "ContinuousDomains",
               "IntegrationRules", "StateVectors", "IpmKernels", "VitalRateExpr",
               "ParameterValues", "EnvironmentalVariables", "HierarchTable",
               "UncertaintyTable")

  urls <- paste(
    "https://raw.githubusercontent.com/levisc8/Padrino/main/padrino-database/clean/",
    tab_nms,
    ".csv", sep = "")

  out <- list()

  for(i in seq_along(urls)) {

    out[[i]] <- utils::read.csv(file = url(urls[i]), stringsAsFactors = FALSE)


    names(out)[i] <- tab_nms[i]
  }

  if(save) {

    destination <- paste(destination, tab_nms, ".csv", sep = "")

    for(i in seq_along(out)) {

      utils::write.csv(out[[i]],
                       file = destination[i],
                       quote = FALSE,
                       na = "",
                       row.names = FALSE)

    }

  }

  class(out) <- c("pdb", "list")

  return(out)

}

#' @rdname in_out
#'
#' @param pdb A \code{pdb} object.
#' @export

pdb_save <- function(pdb, destination = NULL) {

  if(!is.null(destination)) {

    if(substr(destination,
              start = nchar(destination),
              stop = nchar(destination)) != "/") {

      destination <- paste(destination, "/", sep = "")

    }

  }

  tab_nms     <- names(pdb)

  destination <- paste(destination, tab_nms, ".csv", sep = "")


  for(i in seq_along(pdb)) {

    utils::write.csv(pdb[[i]],
                     file = destination[i],
                     quote = FALSE,
                     na = "",
                     row.names = FALSE)

  }

  invisible(pdb)
}


#' @rdname in_out
#' @param path The directory where the PADRINO tables are stored
#' @export

pdb_load <- function(path) {

  if(!is.null(path)) {

    if(substr(path,
              start = nchar(path),
              stop = nchar(path)) != "/") {

      path <- paste(path, "/", sep = "")

    }

  }

  tab_nms <- c("Metadata", "StateVariables", "DiscreteStates", "ContinuousDomains",
               "IntegrationRules", "StateVectors", "IpmKernels", "VitalRateExpr",
               "ParameterValues", "EnvironmentalVariables", "HierarchTable",
               "UncertaintyTable")

  path    <- paste(path, tab_nms, ".csv", sep = "")

  pdb <- list()

  for(i in seq_along(tab_nms)) {

    pdb[[i]] <- utils::read.csv(file = path[i],
                                stringsAsFactors = FALSE)

  }

  names(pdb) <- tab_nms

  class(pdb) <- c("pdb", "list")

  return(pdb)
}
