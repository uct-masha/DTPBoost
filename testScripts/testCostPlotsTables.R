library(tidyverse)
source(here::here('models/R/scripts/calculateCosts.R'))
costs <- readRDS(here::here('cachedAppObjects/r_costs.rds'))
costsComparisons <- readRDS(here::here('cachedAppObjects/r_costsComparisons.rds'))
currency = 'USD'
DIS <- c('All','DTP','Diphtheria','Tetanus','Pertussis')
isFinancial = TRUE
scenarios = levels(costs$resultSummary$scenario)
# scenarios = c("Baseline", "Infant")

DIS = "DTP"
isDiscounted = T
isFinancial = T
makeCostsPlot(costs, scenarios, DIS=DIS, currency=currency, isFinancial = F, isDiscounted = F)
makeCostsPlot2(costs, scenarios=scenarios, startYear=2023, currency = "USD", isFinancial=T)
x=c('avert_clin', 'avert_death')[[1]]
makeCostsPlot3(costs, x=x, scenarios=scenarios, DIS="DTP", currency=currency,isFinancial=F, isDiscounted=F)

myplot <- makeCostsPlot4(
  costsComparisons = costsComparisons,
  costs = costs,
  scenario = scenarios[[2]],
  DIS = "DTP",
  currency = currency,
  isFinancial = isFinancial, 
  isDiscounted = isDiscounted
) %>% ggplotly(tooltip = c("text")) %>% 
  layout(
    legend = list(orientation = 'h')
  )


tbMapping = tibble::tribble(
  ~variableSimple,          ~pretty,
  "costTot",                "Total cost",
  "costVacc_wIntro",        "Cost of vaccination",
  "tot_coi",                "Cost of illness",
  "cost_ib",  "Cost of infant booster",
  "cost_icb", "Cost of child booster",
  "cost_iab", "Cost of adolescent booster",
  "cost_im",  "Cost of maternal"#,
  # "clin_Inc",               "Clinical cases",
  # "deaths",                 "Deaths",
  # "icer_clin", "Cost per clinical case averted",
  # "icer_death",         "Cost per death averted"
)
tbMapping = tibble::tribble(
  ~variableSimple,          ~pretty,
  "costTot",                "Total cost",
  "costVacc_wIntro",        "Cost of vaccination",
  "tot_coi",                "Cost of illness",
  # "cost_ib",  "Cost of infant booster",
  # "cost_icb", "Cost of child booster",
  # "cost_iab", "Cost of adolescent booster",
  # "cost_im",  "Cost of maternal",
  "clin_Inc",               "Clinical cases (Total)",
  "deaths",                 "Deaths (Total)",
  "icer_clin", "Cost per clinical case averted",
  "icer_death",         "Cost per death averted"
)
makeCostsTable(costs=costs,
               scenarios=scenarios,
               currency=currency,
               tbMapping=tbMapping,
               isDiscounted=T,
               isFinancial=F)
