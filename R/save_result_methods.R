#' @title save simulation results
#' @param con a DBI connection
#' @param tab name of the table
#' @param df name of the dataframe
#' @export
#' @name  save_sim_result
save_sim_result <- function(con, tab, df) {
    tb_exist <- DBI::dbExistsTable(con, tab)
    DBI::dbWriteTable(con, tab, df, overwrite = !tb_exist, append = tb_exist)
}
