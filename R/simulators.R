#' @title  base simulator
#' @param N_Policies number of policies
#' @param policy_exprs  alist of policy attribute formula, not NULL
#' @param policy_parameters  alist of parameters of policy formula
#' @param frequency_exprs alist of frequency attribute formula, the first formula should describe claims count, not NULL
#' @param frequnecy_parameters alist of parameters of frequency formula
#' @param severity_exprs  alist of sevevrity attribute formula, the first formula should describe loss, not NULL
#' @param severity_paramters alist of parameters of severity formula
#' @description This is the basic simulation model that doesn't not specifically define the transition of claims, such as open/close, payment, and also case reserve development
#' @export
base_simulator <- function(N_Policies, policy_exprs, policy_parameters, frequency_exprs,
                           frequnecy_parameters, severity_exprs, severity_paramters) {

  ###create number of policies
  policy_df <- tibble::tibble(Policy_Number = seq(1, N_Policies, 1))
  policy_df <- policy_df %>%
    ###evaluate policy attributes
    expr_evaluation(expr_alist = policy_exprs,
                    params_alist = policy_parameters)

  claimcount_var <- frequency_alist[[1]] %>% as.list() %>% .[[2]]
  policy_df_w_claims <- policy_df %>%
    ###evaluate number of claims
    expr_evaluation(expr_alist = frequency_alist,
                    params_alist = frequency_params_components_alist) %>%
    dplyr::select_at(dplyr::vars(Policy_Number,  !!claimcount_var))

  policy_df_w_claims <- policy_df_w_claims %>%
    dplyr::filter(!!claimcount_var > 0)

  loss_var <- severity_alist[[1]] %>% as.list() %>% .[[2]]
  claims_df <- policy_df_w_claims %>% dplyr::pull(!!claimcount_var ) %>%
    purrr::map(~seq(1, ., 1)) %>%
    dplyr::mutate(policy_df_w_claims, ClaimNo = .) %>% tidyr::unnest(cols = c(ClaimNo)) %>%
    dplyr::mutate(ClaimNo = paste(Policy_Number, ClaimNo, sep = '_')) %>%
    dplyr::left_join(policy_df)  %>%
    expr_evaluation(expr_alist = severity_alist,
                    params_alist = severity_params_alist ) %>%
    dplyr::filter(!!loss_var > ded) %>%
    dplyr::mutate(
      lim_exceed = ifelse(!!loss_var > limit, 1, 0),
      !!loss_var := pmin(!!loss_var, limit)
    )

  frq_data_net <- policy_df %>%
    dplyr::left_join(
      claims_df %>% dplyr::group_by(Policy_Number) %>%
        dplyr::summarise(claimcount = dplyr::n())
    ) %>%
    dplyr::mutate(claimcount = dplyr::coalesce(claimcount, 0),
                  freq2 = 1)

  claims_df1 <- claims_df %>%
    dplyr::select(
      dplyr::one_of(colnames(frq_data_net)), !!loss_var, limit, lim_exceed
    ) %>%
    dplyr::mutate(freq2 = 0)

  full_data <- dplyr::bind_rows(
    frq_data_net, claims_df1
  ) %>%
    dplyr::mutate(
      !!loss_var := ifelse(freq2 == 1, ded + 0.1, !!loss_var),
    ) %>%
    dplyr::mutate_at(
      dplyr::vars(claimcount, lim_exceed), ~dplyr::coalesce(., 0)
    )
  return(full_data)
}
