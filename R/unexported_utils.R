.read_all_sheets <- function(file) {

  sheets <- readxl::excel_sheets(file)

  out <- lapply(sheets,
               FUN = function(x) readxl::read_excel(file, sheet = x))

  names(out) <- sheets

  # padEnv <- rlang::new_environment(data = out)

  return(out)

}


