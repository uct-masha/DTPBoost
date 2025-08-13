source(here::here('models/R/scripts/utils.R'))
LOG = makeLogger()
source(here::here('models/R/models/commonModelCode.R'))
source(here::here('models/R/models/pertussisModel.R'))

#### Run the model ####
setLogLevel(LEVEL$TRACE)
#setLogLevel(LEVEL$INFO) #to shush the logging 
parameters = c(getModelParameters(), readRDS('cachedAppObjects/shinyInputs_baseline.rds'))
initialConditions = makeInitialConditionsCode.P(mtMod_P,
                                                parameters,
                                                coverage_table = readRDS('coverage_table.rds'),
                                                ISO3 = 'UGA')

t1=Sys.time()
mo <- run_model.P(parameters, initialConditions, timesteps)
t2=Sys.time()
modelRuntime <- t2 - t1
LOG("Model took a total of {round(as.numeric(modelRuntime, unit='secs'),digits=2)} seconds", LEVEL$INFO)
mo

# View protected
mo %>%
  last() %>%
  ungroup %>% 
  filter(age_group!='All',
         disease=='Pertussis',
         variable=='protected_prop_P') %>%
  mutate(age_group=as_factor(age_group))%>%
  ggplot(aes(x=year,y=value,color=age_group,group=age_group)) +
  geom_line()

# sum(is.na(mo$value))
# sum(mo$value<0)
# min(mo$value[which(mo$value<0)])
# 
# ggplot(mo %>% filter(value < 0, variable=="popa")) +
#   aes(x = time, y = value, colour = age_group) +
#   geom_point(shape = "circle", size = 1) +
#   scale_color_hue(direction = 1) +
#   theme_minimal()

# mo_b<-mo[[1]] 
# mo_b %>% filter(variable=="protected_prop_P", age_group=="All")
# 
# mo_m<-mo[[1]] 
# mo_m %>% filter(variable=="protected_prop_P", age_group=="All") %>% ungroup() %>% select(value)
# 
# View(cbind(mo_b %>% filter(variable=="clin_Inc_P", age_group=="All"),mo_m %>% filter(variable=="clin_Inc_P", age_group=="All") %>% ungroup() %>% select(value)))

#### Plots ####
# pltPostProc.P(mo[[1]], var='clin_Inc_P')
pltPostProc_yr.P(mo[[1]], var='clin_Inc_P')

