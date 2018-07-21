.read_all_sheets <- function(file) {

  sheets <- readxl::excel_sheets(file)

  out <- lapply(sheets,
               FUN = function(x) readxl::read_excel(file, sheet = x))

  names(out) <- sheets

  # padEnv <- rlang::new_environment(data = out)

  return(out)

}



.populate_domain_env <- function(db) {

  continuousDomains <- db[['ContinuousDomains']]
  meshPoints <- db[['QuadratureRoutines']]$n_meshpoints

  domains <- rlang::new_environment()

  for(i in seq_len(dim(continuousDomains)[1])) {
    DomainName <- continuousDomains$state_variable[i]

    domains[[DomainName]] <- seq(continuousDomains$lower,
                                 continuousDomains$upper,
                                 length.out = meshPoints)
  }

  return(domains)
}
