#' @title expression parser
#' @description parses expressions based on conventions specified in this package
#' @param expr_alist an alist that contains expressions
#' @name expr_parser
#' @export
expr_parser <- function(expr_alist) {

  expr_types <- expr_alist %>% purrr::map(purrr::safely(function(x) eval(x) %>% class())) %>%
    purrr::map_chr(function(x) if (purrr::is_null(x$result)) return("NULL") else return(x$result))

  formula_exprs <- expr_alist[which(expr_types == 'formula')]
  equation_exprs <- expr_alist[which(expr_types == 'NULL')]
  scalar_exprs <- expr_alist[which(!expr_types %in% c('formula', 'NULL') )]

  return(
    list(
      formula_exprs = formula_exprs
      , equation_exprs = equation_exprs
      , scalar_exprs = scalar_exprs %>% purrr::map(eval)
    )
  )
}

#' @title evalute formula expressions
#' @description evalute formula expressions
#' @param df dataframe with info
#' @param data_list all the information to be parsed to eval_tidy
#' @param formula_exprs formula expressions
#' @name eval_formula_exprs
#' @export
eval_formula_exprs <- function(df, data_list, formula_exprs) {
  for (i in 1:length(formula_exprs)) {
    formula_tmp <- formula_exprs %>% .[[i]] %>% as.list()
    dep <- formula_tmp[[2]]
    RHS <- formula_tmp[[3]] %>% as.list()
    RHS <- ifelse(RHS %>% purrr::map(is.character), paste0("'", RHS, "'"), RHS)
    RHS_name <- formula_tmp[[3]] %>% as.list() %>% names()
    if (is.null(RHS_name)) RHS_name <- rep("", length(RHS))

    func <- RHS[[1]]
    args <- RHS[-1]
    #args <- ifelse(RHS_name[-1] == "", paste0(args, ' = ', args), paste0(RHS_name[-1], ' = ', args))
    simulation_func <- paste0(func, "(n = dplyr::n(), ",
                              paste( args , collapse = ", "), ")")
    expr_to_eval <- paste0('df %>% dplyr::mutate(', dep, " = ", simulation_func, ")")
    df <- rlang::eval_tidy(rlang::parse_expr(expr_to_eval), data = data_list)
    data_list$df <- df
  }
  return(df)
}


#' @title evalute parsed expressions
#' @description evaluate parsed expressions
#' @param df data frame
#' @param expr_alist an alist that contains expressions
#' @param params_alist an alist that contains parameter distributions
#' @name expr_evaluation
#' @export
expr_evaluation <- function(df, expr_alist, params_alist = NULL) {
  parsed_exprs <- expr_parser(expr_alist)
  data_list <- append(list(df = df), parsed_exprs$scalar_exprs)

  if (length(params_alist) > 0) {
    parsed_params_exprs <- expr_parser(params_alist)
    data_list <- append( data_list,  parsed_params_exprs$scalar_exprs)
    df <- eval_formula_exprs(df, data_list, formula_exprs = parsed_params_exprs$formula_exprs)
    data_list$df <- df
  }

  if (length(parsed_exprs$equation_exprs) > 0) {
    equation_exprs <- parsed_exprs$equation_exprs %>% rev()
    ###evaluate equations first
    for (i in 1:length(equation_exprs)) {
      expr_var <- equation_exprs[[i]]
      var <- names(equation_exprs)[i]
      expr_to_eval <- 'df %>% dplyr::mutate(!!expr_var)'

      df <- rlang::eval_tidy(rlang::parse_expr(expr_to_eval), data = data_list)
      df <- df[!colnames(df) %in% var]
      colnames(df)[length(colnames(df))] <- var
      data_list$df <- df
    }
  }

  ###evaluate simulation
  for (i in 1:length(parsed_exprs$formula_exprs)) {
    formula_tmp <- parsed_exprs$formula_exprs %>% .[[i]] %>% as.list()
    dep <- formula_tmp[[2]]
    RHS <- formula_tmp[[3]] %>% as.list()
    RHS <- ifelse(RHS %>% purrr::map(is.character), paste0("'", RHS, "'"), RHS)
    RHS_name <- formula_tmp[[3]] %>% as.list() %>% names()
    if (is.null(RHS_name)) RHS_name <- rep("", length(RHS))

    func <- RHS[[1]]
    args <- RHS[-1]
    args <- ifelse(RHS_name[-1] == "", paste0(args, ' = ', args), paste0(RHS_name[-1], ' = ', args))
    simulation_func <- paste0(func, "(n = dplyr::n(), ",
                              paste( args , collapse = ", "), ")")
    expr_to_eval <- paste0('df %>% dplyr::mutate(', dep, " = ", simulation_func, ")")
    df <- rlang::eval_tidy(rlang::parse_expr(expr_to_eval), data = data_list)
    data_list$df <- df
  }
  return(df)
}
