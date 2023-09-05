#' @rdname in_out
#' @title Download PADRINO \code{pdb} objects
#'
#' @description Download PADRINO from Github.
#'
#' @param save Write the PDB object to a folder of text files?
#' @param destination Where to write the \code{pdb} object to.
#'
#' @details This does not currently support versioning because there is only
#' one version. \code{destination} should be a folder name. When \code{save = TRUE},
#' a set of 12 text files will be saved in the \code{destination} folder. The files
#' are tab-delimited. If you are not connected to the internet, \code{pdb_download}
#' will load the internal \code{pdb} data object and return that instead.
#'
#' @return \code{pdb_download} and \code{pdb_load} return \code{pdb} objects.
#' \code{pdb_save} returns a \code{pdb} object invisibly.
#'
#' @importFrom utils read.table write.table data
#' @importFrom rlang caller_env
#' @importFrom curl has_internet
#' @export

pdb_download <- function(save = TRUE, destination = NULL) {

  if(!curl::has_internet()) {

    message("Must be connected to the internet to download PADRINO!\n",
            "Loading internal dataset instead.")

    utils::data("pdb", envir = rlang::caller_env())
    pdb <- get("pdb", envir = rlang::caller_env(),
               inherits = FALSE)
    return(pdb)

  }

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

  tab_nms <- c("Metadata", "StateVariables", "ContinuousDomains",
               "IntegrationRules", "StateVectors", "IpmKernels", "VitalRateExpr",
               "ParameterValues", "EnvironmentalVariables", "ParSetIndices")

  urls <- paste(
    "https://raw.githubusercontent.com/padrinoDB/Padrino/main/padrino-database/clean/",
    tab_nms,
    ".txt", sep = "")

  out <- list()

  for(i in seq_along(urls)) {

    out[[i]] <- utils::read.table(file = url(urls[i]),
                                  stringsAsFactors = FALSE,
                                  sep              = "\t",
                                  encoding         = "UTF-8",
                                  quote            = "\"",
                                  header           = TRUE)

    names(out)[i] <- tab_nms[i]
  }

  if(save) {

    destination <- paste(destination, tab_nms, ".txt", sep = "")

    for(i in seq_along(out)) {

      utils::write.table(out[[i]],
                         file = destination[i],
                         row.names    = FALSE,
                         sep          = "\t",
                         quote        = TRUE,
                         na           = "NA",
                         fileEncoding = "UTF-8")

    }

  }

  # Quote = TRUE messes up some of the ParSetIndex values
  pdb$ParSetIndices$range <- gsub("\\\\", "'", pdb$ParSetIndices$range)

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

  destination <- paste(destination, tab_nms, ".txt", sep = "")


  for(i in seq_along(pdb)) {

    utils::write.table(pdb[[i]],
                       file = destination[i],
                       row.names    = FALSE,
                       sep          = "\t",
                       quote        = TRUE,
                       na           = "NA",
                       fileEncoding = "UTF-8")

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

  tab_nms <- c("Metadata", "StateVariables", "ContinuousDomains",
               "IntegrationRules", "StateVectors", "IpmKernels", "VitalRateExpr",
               "ParameterValues", "EnvironmentalVariables", "ParSetIndices")

  path    <- paste(path, tab_nms, ".txt", sep = "")

  pdb <- list()

  for(i in seq_along(tab_nms)) {

    pdb[[i]] <- utils::read.table(file = path[i],
                                  stringsAsFactors = FALSE,
                                  sep              = "\t",
                                  encoding         = "UTF-8",
                                  quote            = "\"",
                                  header           = TRUE)
  }

  names(pdb) <- tab_nms

  pdb$ParSetIndices$range <- gsub("\\\\", "'", pdb$ParSetIndices$range)

  class(pdb) <- c("pdb", "list")

  return(pdb)
}
