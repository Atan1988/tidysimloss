#' @title create a tidysimloss object
#' @name crt_tidysimloss
#' @description this function creates a tidysimloss object, based on a policy listing, as well as a claims transaction list
#' @param policy_df Polidy Data frame
#' @param policy_required_field_map maps the required fields, Policy Number, Policy Effective Date, Policy Expriation Date
#' @param frequency_alist Frequency Component:
#' @param severity_init_components_alist initial State components:
#' @param severity_transit_components_alist transition state components:
#' @param severity_params_components_alist parameter components:
#' @export
crt_tidysimloss  <- function(policy_df, policy_required_field_map,
                             frequency_alist, severity_init_components_alist,
                             severity_transit_components_alist, severity_params_components_alist) {

  policy_df <- policy_df %>%
    expr_evaluation(df = ., expr_alist = frequency_alist)


  claims_df <- crt_tidysimloss(policy_df, policy_required_field_map,
                              frequency_alist, severity_init_components_alist,
                              severity_transit_components_alist, severity_params_components_alist)

  return(
    list(policy_df = policy_df,
         claims_df = claims_df)
  )
}
