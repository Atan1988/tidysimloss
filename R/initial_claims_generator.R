#' @title generate initial claims states
#' @name ini_claims_gen
#' @description generates initial claims
#' @param policy_df Polidy Data frame
#' @param policy_required_field_map maps the required fields, Policy Number, Policy Effective Date, Policy Expriation Date
#' @param severity_init_components_alist initial State components:
#' @param severity_params_components_alist parameter components:
#' @export
crt_tidysimloss  <- function(policy_df, policy_required_field_map,
                             frequency_alist, severity_init_components_alist,
                             severity_transit_components_alist, severity_params_components_alist) {
  claims_key <- c('PolicyNo', 'NumberOfClaims')

  policy_df_w_claims <- policy_df %>% dplyr::select_at(dplyr::vars(policy_required_field_map[claims_key]))
  colnames(policy_df_w_claims) <- claims_key

  policy_df_w_claims <- policy_df_w_claims %>%
    dplyr::filter(NumberOfClaims > 0)

  claims_df <- policy_df_w_claims %>% dplyr::pull(NumberOfClaims) %>%
    purrr::map(~seq(1, ., 1)) %>%
    dplyr::mutate(policy_df_w_claims, ClaimNo = .) %>% tidyr::unnest() %>%
    dplyr::mutate(ClaimNo = paste(PolicyNo, ClaimNo, sep = '_')) %>%
    dplyr::select(PolicyNo, ClaimNo) %>%
    dplyr::left_join(Policy_df, by = policy_required_field_map[1])


  claims_df %>%
    expr_evaluation(df = ., expr_alist = severity_init_components_alist)

}
