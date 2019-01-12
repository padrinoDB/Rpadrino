
# ranef handlers

.has_ranefs <- function(db) {
  if(dim(db[[11]])[1] > 0) {
    !all(is.null(db[[11]]$env_range))
  } else {
    FALSE
  }

}


# Simple extraction of effect suffixes
.get_ranef_names <- function(names) {
  name_list <- strsplit(names, '_')

  suffixes <- lapply(name_list, function(x) x[2])

  out <- unlist(suffixes)
  return(out)

}


# Extracts the different levels of each hierarchical variable and then
# expand.grids them

.make_ranef_levels <- function(ranef_table) {


  ranefs_list <- rlang::parse_exprs(unique(ranef_table$env_range))

  #
  ranef_levels <- lapply(ranefs_list, eval)
  ranef_names <- .get_ranef_names(unique(ranef_table$vr_expr_name))

  names(ranef_levels) <- ranef_names
  if(length(ranef_levels) > 1){
    out <- expand.grid(ranef_levels)
  } else {
    out <- ranef_levels[[1]]
  }

  return(out)

}


.expand_ranefs <- function(db_table, levels) {

  # column names that would require expansion
  cols <- c('value', 'kernel_id', 'formula')

  # If there aren't any columns that may require expansion, then just ignore it.
  if(!any(cols %in% names(db_table))) return(db_table)

  # Place holder, this will ultimate get rbind'ed
  new_tib <- db_table[0, ]

  for(i in seq_len(dim(db_table)[1])) {

    # Unadulterated garbage, but it works. Creates a row for every level/comibnation
    # of levels, next we substitute in everything
    temp <- .expand_tibble(db_table, levels, i)

    #

    for_bind <- .sub_levels(temp, levels, cols)

    new_tib <- rbind(new_tib, for_bind)
  }
  return(new_tib)
}

.sub_levels <- function(tib, levels, cols) {

  tib_dim <- dim(tib)[1]
  col_ind <- which(names(tib) %in% cols)
  it <- 1

  for(col in col_ind) { # column index loop
    for(j in seq_len(dim(levels)[2])) { # variable names loop
      nm <- names(levels)[j]

      for(k in seq_len(dim(levels)[1])) { # variable values loop

          tib[it, col] <- gsub(nm, levels[k, j], tib[it, col])


          if(it == dim(levels)[1]){
            it <- 1
          } else {
            it <- it + 1
          }

      } # end var names loop
    } # end tib loop
  } # end col index

  return(tib)
}


.expand_tibble <- function(tib, levels, i) {
  do.call("rbind",
          replicate(dim(levels)[1],
                    tib[i, ],
                    simplify = FALSE))
}
