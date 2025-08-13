pacman::p_load(tidyverse)
source(here::here('www/R/utils/utils.R'))
LOG <- makeLogger()
source(here::here("models/R/models/commonModelCode.R"))
source(here::here("models/R/scripts/calculateCosts.R"))
# To calculate costsBaseline reactive:
costInputsBaseline = readRDS(here::here('cachedAppObjects/r_costInputs0.rds'))  # getShinyInputCosts(input)
simulBaseline = readRDS(here::here('cachedAppObjects/r_simulBaseline.rds'))  # simul_baseline()

analysisRange = c(2023,2038)
discountRate = 0.035
costsBaseline <- calculateCostsScenario(moAnnual=simulBaseline, costInputs=costInputsBaseline,
                                        analysisRange = analysisRange, discountRate = discountRate)

# To calculate costsScenarios reactive:
costInputsScenario = readRDS(here::here('cachedAppObjects/r_costInputs.rds'))  # getShinyInputCosts(input)
fname <- last(fs::dir_ls(here::here('cachedAppObjects'), glob='*r_simulScenario*.rds'))
simulScenario = readRDS(fname)  # simul$results_epi

costsScenario <- calculateCostsScenario(moAnnual=simulScenario, costInputs=costInputsScenario,
                                        analysisRange = analysisRange, discountRate = discountRate)

calculateCostsComparison(costsBaseline = costsBaseline, costsScenario = costsScenario)
