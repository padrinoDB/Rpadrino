#' @rdname download
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

  destination <- paste(destination, tab_nms, ".csv", sep = "")

  out <- list()

  for(i in seq_along(urls)) {

    out[[i]] <- utils::read.csv(file = url(urls[i]), stringsAsFactors = FALSE)


    names(out)[i] <- tab_nms[i]
  }

  if(save) {

    for(i in seq_along(out)) {

      utils::write.csv(out[[i]],
                       file = destination[i],
                       quote = FALSE,
                       na = "",
                       row.names = FALSE)

    }

  }

  return(out)

}
