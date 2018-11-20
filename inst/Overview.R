# The claims simulation model has 2 major components
#
# 1. Frequency Component:
# {
# total_claims ~ ,
# occurrence_lag ~ ,
# report_lag ~
# }
#
# 2. Severity Component:
# severity simulation has 3 components:
#initial State components: variables defined in ... using = assignment
#{
#ini_indemn_paid ~ ,
#ini_expense_paid ~ ,
#ini_indemn_reserve ~ ,
#ini_expense_reserve ~ ,
#...
#}
#
#transition state components: variables defined in ... using = assignment
#{
#closing ~ ,
#reopen ~ ,
#no_change ~ ,
#indemn_reserve_change ~ ,
#expense_reserve_change ~ ,
#percent_indemn_reserve_paid ~ ,
#percent_expense_reserve_paid ~ ,
#...
#}
#parameter components:  parameters distribution should be defined using ~ or using tidy dataframes
#  should the various data frames be automatically considerred independent?
#{
#...
#}
