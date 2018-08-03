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
#' @importFrom purrr set_names splice
#' @importFrom magrittr %>%

.associate_vr_parameters <- function(db, parsed_kernels) {

  param_table <- db[[10]]
  lowest_level_params <- param_table[ ,c('state_variable', 'parameter_name')]
  state_vars <- db[[2]]
  expr_table <- db[[9]]

  # to store complete output for each full sub-kernel
  out <- list()

  for(i in seq_len(length(parsed_kernels))) {
    kernel_exprs <- parsed_kernels[[i]]

    # to store parameters for each full sub_kernel
    params <- list()

    for(j in seq_len(nrow(kernel_exprs))) {

      temp_params <- list(vr_exprs = NULL,
                          values = NULL,
                          int_eval = NULL)

      # split vital rate expressions RHS into component parts and store RHS
      vr_expr <- kernel_exprs[j, 2]

      # create expressions from LHS and RHS
      temp_params$vr_exprs <- rlang::parse_expr(vr_expr)

      vr_funs <- RPadrino:::.split_vr_expr(vr_expr)

      vr_funs <- base::Filter(function(x) {
        !grepl(state_vars$state_variable, x)
      }, vr_funs)

      # identify if given parameter is lowest level,
      # or is defined by other parameters

      # NOTE: this should be recursive. update when working

      # Get parameters for lowest levels
      for(x in seq_len(length(vr_funs))) {

          parameter_value <-
            param_table$parameter_value[param_table$parameter_name == vr_funs[x]]

          temp_params$values <- c(temp_params$values, parameter_value)

          # if it contains a density function or math:
          if(grepl('\\(|\\*|\\+', vr_expr)) {

            temp_params$int_eval <- expr_table$model_type[stringr::str_detect(
              expr_table$formula,
              stringr::fixed(vr_expr)
            )]

          } else {
            # if it doesn't contain those:

            for_matching <- paste(vr_expr, '$', sep = '')
            temp_params$int_eval <- expr_table$model_type[stringr::str_detect(
              expr_table$formula,
              stringr::regex(for_matching)
            )]
          }

      } # End checking and extraction

      params <- purrr::splice(params, list(temp_params))

      # if there weren't any values associated, don't worry, we'll get those
      # from another expression

      if(!is.null(params[[j]]$values) & length(params[[j]]$values) != 0) {

        names(params[[j]]$values) <- vr_funs

      }

    } # end kernel_expressions loop
    out <- purrr::splice(out, list(params))
    names(out)[i] <- names(parsed_kernels)[i]

  } # end parsed_kernels loop

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

.kernel_vr_LHS_RHS_mat <- function(db, parsed_K, name) {

  expr_table <- db[[9]]

  out <- list()
  # find vr expressions associated with a given kernel
  # kernel lhs rhs matrix
  kernel_funs <- expr_table$formula[.identify_kernels(name,
                                                      expr_table$kernel_id)]

  kernel_LHS_RHS <- stringr::str_split(kernel_funs,
                                       "=",
                                       simplify = TRUE)

  kernel_LHS_RHS <- apply(kernel_LHS_RHS,
                          MARGIN = 2,
                          FUN = function(x) stringr::str_trim(x))

  out <- list(kernel_LHS_RHS)
  names(out) <- name


  return(out)

}

#' @importFrom stringr str_trim str_split
#' @importFrom magrittr %>%
#
# identifies which expressions map to which kernels and returns a numeric
# index for subsetting

.identify_kernels <- function(flag, kernels) {
  x <- stringr::str_split(kernels,';', simplify = TRUE)
  x <- apply(x, MARGIN = 2, FUN = function(y) stringr::str_trim(y))
  if(!is.matrix(x)) {
    x <- matrix(c(x, rep(NA_character_, length(x))),
                ncol = length(x),
                nrow = 2,
                byrow = TRUE)
  }

  ind <- apply(x,
               MARGIN = 2,
               FUN =  function(y, flag) which(y == flag),
               flag = flag) %>%
    unlist() %>%
    unique()
  return(ind)

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
  K_kernel <- kernel_table$formula[kernel_table$kernel_id == 'K']

  sub_kernel_names <- kernel_table$kernel_id

  # pull out sub-kernels, turn into expressions, and attach the chr vector
  # with kernel flags
  sub_kernels <- c(kernel_table$formula[!kernel_table$kernel_id %in% 'K'],
                   K_kernel)

  out <- vector("list", length(sub_kernel_names))

  for(i in seq_len(length(sub_kernels))) {
     index <- which(kernel_table$formula == sub_kernels[i])

     kernel_expr <- rlang::parse_expr(sub_kernels[i])
     out[[i]] <- list(expr = rlang::enquo(kernel_expr),
                      type = kernel_table$model_family[index])

     name <- kernel_table$kernel_id[index]

     names(out)[i] <- name
  }

  out$kernel_flags <- sub_kernel_names

  # exit
  return(out)
}
