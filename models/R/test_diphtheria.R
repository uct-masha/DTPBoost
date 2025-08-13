source(here::here('models/R/scripts/utils.R'))
LOG = makeLogger()
source(here::here('models/R/models/commonModelCode.R'))
source(here::here('models/R/models/diphtheriaModel.R'))

#### Run the model ####
setLogLevel(LEVEL$TRACE)
#setLogLevel(LEVEL$INFO) #to shush the logging 
parameters = c(getModelParameters(), readRDS('cachedAppObjects/shinyInputs_baseline.rds'))
initialConditions = makeInitialConditionsCode.D(mtMod_D,
                                                parameters,
                                                coverage_table = readRDS('coverage_table.rds'),
                                                ISO3 = 'UGA')

t1=Sys.time()
mo <- run_model.D(parameters, initialConditions, timesteps)
t2=Sys.time()
modelRuntime <- t2 - t1
LOG("Model took a total of {round(as.numeric(modelRuntime, unit='secs'),digits=2)} seconds", LEVEL$INFO)
mo

#saveRDS(mo[[1]], file = "mo_D_baseline.rds")

# sum(is.na(mo[[2]]$value))
# sum(mo[[2]]$value<0)
# min(mo$value[which(mo$value<0)])
# 
# ggplot(mo %>% filter(value < 0, variable=="vac_prev")) +
#   aes(x = time, y = value, colour = age_group) +
#   geom_point(shape = "circle", size = 1) +
#   scale_color_hue(direction = 1) +
#   theme_minimal()


#### Plots ####
pltPostProc_yr.D(mo[[1]], var='clin_Inc_D')


