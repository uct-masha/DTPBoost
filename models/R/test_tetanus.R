source(here::here('models/R/scripts/utils.R'))
LOG = makeLogger()
source('models/R/scripts/utils.R')
LOG=makeLogger()
source(here::here('models/R/models/commonModelCode.R'))
source(here::here('models/R/models/tetanusModel.R'))

#### Run the model ####
setLogLevel(LEVEL$TRACE)
#setLogLevel(LEVEL$INFO) #to shush the logging 
parameters = c(getModelParameters(), readRDS('cachedAppObjects/shinyInputs_baseline.rds'))
initialConditions = makeInitialConditionsCode.T(mtMod_T,
                                                parameters,
                                                coverage_table = readRDS('coverage_table.rds'),
                                                ISO3 = 'UGA')

t1=Sys.time()
mo <- run_model.T(parameters, initialConditions, timesteps)
t2=Sys.time()
modelRuntime <- t2 - t1
LOG("Model took a total of {round(as.numeric(modelRuntime, unit='secs'),digits=2)} seconds", LEVEL$INFO)
mo

#### Plots ####
pltPostProc_yr.T(mo[[1]], var='clin_Inc_T')
