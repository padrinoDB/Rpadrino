.split_ranefs <- function(current_model) {

  ranef_table <- current_model[[12]]

  levels <- .make_ranef_levels(ranef_table)


  out <- lapply(current_model,
                function(x) .expand_ranefs(x,levels = levels))

  return(out)

}

