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
}
