#' @title Associates parameters in \code{ParameterValues} with correct names
#' in \code{VitalRateExpr}
#'
#' @param db A database object
#' @param parsed_kernels the output from \code{.kernel_vr_LHS_RHS_mat}
#'
#' @return A list of lists of quosures containing the expressions and parameters
#' needed to create a \code{kernel}.
#'
#' @importFrom stringr fixed str_detect
#' @importFrom rlang parse_expr

.associate_vr_parameters <- function(db, parsed_kernels) {

  param_table <- db[[10]]
  lowest_level_params <- param_table[ ,c('state_variable', 'parameter_name')]
  state_vars <- db[[2]]
  expr_table <- db[[9]]

  # to store complete output for each full sub-kernel
  out <- vector('list', length(parsed_kernels))

  # initialize with kernel ids and what not so I don't lose my mind writing this
  for(i in seq_len(length(out))) {
    out[[i]] <- list()
    names(out)[i] <- names(parsed_kernels)[i]
  }

  for(i in seq_len(length(parsed_kernels))) {
    kernel_exprs <- parsed_kernels[[i]]

    # to store parameters for each full sub_kernel
    params <- vector('list', nrow(kernel_exprs))

    # ibid out
    for(j in seq_len(length(params))) {
      params[[j]] <- list(vr_exprs = NULL,
                          values = NULL,
                          int_eval = NULL,
                          sub_expr = list(vr_expr = NULL,
                                          values = NULL,
                                          int_eval = NULL))
    }

    for(j in seq_len(nrow(kernel_exprs))) {
      # split vital rate expressions RHS into component parts and store RHS
      vr_expr <- kernel_exprs[j, 2]

      # create expressions from LHS and RHS
      params[[j]]$vr_exprs <- c(params[[j]]$vr_exprs,
                                rlang::parse_expr(vr_expr))

      vr_funs <- RPadrino:::.split_vr_expr(vr_expr)

      vr_funs <- base::Filter(function(x) {
        !grepl(state_vars$state_variable, x)
      }, vr_funs)

      # identify if given parameter is lowest level,
      # or is defined by other parameters

      # NOTE: this should be recursive. update when working

      # Get parameters for lowest levels
      for(x in seq_len(length(vr_funs))) {

        if(.is_lowest_level(vr_funs[x], lowest_level_params$parameter_name)) {

          parameter_value <- param_table$parameter_value[param_table$parameter_name == vr_funs[x]]
          params[[j]]$values <- c(params[[j]]$values, parameter_value)
          params[[j]]$int_eval <- expr_table$model_type[stringr::str_detect(expr_table$formula,
                                                                            stringr::fixed(vr_expr))]

        } else {
          # new vital rate expression
          new_expr <- kernel_exprs[kernel_exprs[,1] %in% vr_funs[x], 2]


          params[[j]]$sub_expr$vr_expr <- c(params[[j]]$sub_expr$vr_expr,
                                            rlang::parse_expr(new_expr))

          # find if also contains sub-expressions
          new_funs <- .split_vr_expr(new_expr)

          new_funs <- base::Filter(function(x) {
            !grepl(state_vars$state_variable, x)
          }, new_funs)

          # if not, store it and name it
          if(.is_lowest_level(new_funs, lowest_level_params$parameter_name)) {
            parameter_value <- param_table$parameter_value[param_table$parameter_name == new_funs]
            params[[j]]$sub_expr$values <- c(params[[j]]$sub_expr$values, parameter_value)
            params[[j]]$sub_expr$int_eval <- expr_table$model_type[stringr::str_detect(expr_table$formula,
                                                                                       stringr::fixed(vr_expr))]

            names(params[[j]]$sub_expr$values)[x] <- new_funs
          }


        }

      } # End checking and extraction

      if(!is.null(params[[j]]$values)) names(params[[j]]$values) <- vr_funs
    } # end kernel_expressions loop
    out[[i]] <- params

  } # end parsed_kernels loop


  # create list of functional forms form RHS and associated parameter values


  return(out)
}

.is_lowest_level <- function(text, params) {
  if(any(grepl(text, params))) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' @title splits a vital rate expression and returns its components
#'
#' @param text a string with the vital rate expression
#'
#' @return A character vector of parameters

.split_vr_expr <- function(text) {


  # if it's a function, split it up
  test_1 <- stringr::str_split(text, '[*+]', simplify = TRUE)

  if(dim(test_1)[2] < 2 |
     grepl('[(]', text) |
     grepl('_[:digits:]', text)) {

    # if it's a definition of a distribution, split it into component parts
    test_1 <- stringr::str_extract_all(text,
                                       '[:lower:]+[_][:lower:]+',
                                       simplify = TRUE)
  }

  test_1 <- stringr::str_trim(test_1)

  return(test_1)

}


#' @title kernel vital rate expressions
#'
#' @description Create LHS-RHS matrix of vital rate expressions for all sub-kernels
#' of a model
#'
#' @param db a database object or user created pre-\code{protoIPM}-ish object
#' @param parsed_K The output from \code{.parse_K}
#'
#' @importFrom stringr str_split str_trim
#'
#' @rdname internal

.kernel_vr_LHS_RHS_mat <- function(db, parsed_K) {

  expr_table <- db[[9]]

  out <- vector("list", length(parsed_K$kernel_flags))
  # find vr expressions associated with a given kernel
  for(i in seq_len(length(parsed_K$kernel_flags))) {

    flag <- parsed_K$kernel_flags[i]

    # kernel lhs rhs matrix
    kernel_funs <- expr_table$formula[grepl(flag, expr_table$kernel_id)]

    kernel_LHS_RHS <- stringr::str_split(kernel_funs,
                                         "=",
                                         simplify = TRUE)
    kernel_LHS_RHS <- apply(kernel_LHS_RHS,
                            MARGIN = 2,
                            FUN = function(x) stringr::str_trim(x))

    out[[i]] <- kernel_LHS_RHS
    names(out)[i] <- flag
  }

  return(out)

}

#' @title parse a K kernel to a quosure
#'
#' @param db a database object or user created pre-\code{protoIPM}-ish object
#'
#' @return A list with 2 components: kernel components in the form of quosures and
#' a character vector indicating which sub-kernels are included in the full model
#'
#' @importFrom stringr str_extract_all str_replace_all
#' @importFrom magrittr %>%
#' @importFrom rlang enquo parse_expr
#'


.parse_K_kernel <- function(db) {

  kernel_table <- db[[8]]

  # get K kernel and inspect it for sub-kernels
  K_kernel <- kernel_table$formula[kernel_table$kernel == 'K']

  sub_kernel_names <- stringr::str_extract_all(K_kernel, "[:upper:][(]",
                                                simplify = TRUE) %>%
    stringr::str_replace_all("\\(", "") %>%
    unique()

  # pull out sub-kernels, turn into expressions, and attach the chr vector
  # with kernel flags
  sub_kernels <- kernel_table$formula[kernel_table$kernel %in% sub_kernel_names]

  out <- vector("list", length(sub_kernel_names))

  for(i in seq_len(length(sub_kernels))) {
     kernel_expr <- rlang::parse_expr(sub_kernels[i])
     out[[i]] <- rlang::enquo(kernel_expr)

     name <- stringr::str_extract_all(sub_kernels[i], "[:upper:][(]",
                             simplify = TRUE) %>%
       stringr::str_replace_all("[(]", "") %>%
       unique()

     names(out)[i] <- name
  }

  out$kernel_flags <- sub_kernel_names

  # exit
  return(out)
}
