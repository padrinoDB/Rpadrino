#' @noRd

# creates named list of text expressions. names correspond to kernel or vital
# rate names, and entries are the right hand sides of the formulae

.rm_brackets <- function(form) {

  temp <- lapply(form, function(x) {
    temp <- gsub('\\s*\\[[^\\]]+\\]',
                 '',
                 x,
                 perl = TRUE)
  })


  out_nms <- vapply(temp, function(x) strsplit(x, '=')[[1]][1],
                    character(1L))
  out     <- lapply(temp, function(x) strsplit(x, '=')[[1]][2])

  names(out) <- trimws(out_nms)
  return(out)
}


#' @noRd

# Utility function to weed out parameter assignment operations. These are an
# unfortunate legacy in the database from when I wasn't sure how to work with
# rlang.

.is_the_same <- function(formula) {
  temp <- strsplit(formula, '=') %>%
    unlist() %>%
    trimws()

  isTRUE(identical(temp[1], temp[2]))
}

#' @noRd
# Reads all sheets from a PadrinoDB file

.read_all_sheets <- function(file) {

  sheets <- readxl::excel_sheets(file)

  out <- lapply(sheets,
                FUN = function(x) readxl::read_excel(file, sheet = x))

  names(out) <- sheets

  return(out)

}



