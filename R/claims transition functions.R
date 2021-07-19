#' @title claims transition
#' @name  claims_trans
#' @param df data frame after evaluating on severity transit components
#' @export
claims_trans <- function(df) {
   df %>%
    mutate(
      status = case_when(
        status == 'open' & closing ~ 'close',
        status == 'close' & reopen ~ 'open',
        TRUE ~ status
      ),
      no_change = case_when(
        status == 'close' ~ TRUE,
        TRUE ~ no_change
      ),
      ini_indemn_reserve = case_when(
        no_change ~ ini_indemn_reserve,
        #TRUE ~ ini_indemn_reserve * indemn_reserve_change
        TRUE ~ ini_indemn_reserve + total_indemn * (indemn_reserve_change - 1)
      ),
      ini_expense_reserve = case_when(
        no_change ~ ini_expense_reserve,
        #TRUE ~ ini_expense_reserve * expense_reserve_change
        TRUE ~ ini_expense_reserve + total_expense * (expense_reserve_change - 1)
      ),
      ini_indemn_paid = ini_indemn_paid + ini_indemn_reserve * percent_indemn_reserve_paid,
      ini_expense_paid = ini_expense_paid + ini_expense_reserve * percent_expense_reserve_paid,
      ini_indemn_reserve = ini_indemn_reserve * (1 - percent_indemn_reserve_paid),
      ini_expense_reserve = ini_expense_reserve * (1 - percent_expense_reserve_paid),
      total_indemn = ini_indemn_paid  + ini_indemn_reserve,
      total_expense = ini_expense_paid + ini_expense_reserve,
      total_inc = total_indemn + total_expense
    )
}
