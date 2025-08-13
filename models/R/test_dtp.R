source(here::here('models/R/scripts/getModelParameters.R'))
source(here::here('models/R/models/commonModelCode.R'))
source(here::here('models/R/models/diphtheriaModel.R'))
source(here::here('models/R/models/tetanusModel.R'))
source(here::here('models/R/models/pertussisModel.R'))
source(here::here('models/R/models/dtpModel.R'))

library(future)
library(promises)
plan(multisession)
parameters = c(getModelParameters(), readRDS('shinyInputs.rds'))
coverage_table <- readRDS('coverage_table.rds')
iso3 = 'ZAF'
fname = 'moSmallParallelListNew.rds'
mo <- run_model(parameters=parameters, timesteps=timesteps, coverage_table=coverage_table, iso3=iso3, inParallel = T) %...>%
  saveRDS(fname)
# Now wait until this file appears on disk
cat('waiting')
while(!file.exists(fname)) {
  cat('\r.')
  Sys.sleep(1)
}
mo <- readRDS(fname)
tbMap <- tribble(~ModelChar, ~Model,
                 'p.D','Diphtheria',
                 'p.T','Tetanus',
                 'p.P','Pertussis')
tbTiming <- map(mo, ~tail(.x,-1)) %>%
  as_tibble() %>%
  unnest_longer(p.D) %>%
  unnest_longer(p.T) %>%
  unnest_longer(p.P) %>%
  mutate(Event=c('Sourcing scripts', 'Calling run_model()', 'Complete')) %>%
  pivot_longer(!Event, names_to='ModelChar', values_to='Time') %>%
  arrange(ModelChar,Time) %>% 
  mutate(MinTime=min(Time)) %>%
  left_join(tbMap, by = "ModelChar") %>% 
  group_by(ModelChar) %>% 
  mutate(Model=Model %>% as_factor,
         Time=as_datetime(Time, tz='Africa/Harare'),
         DeltaTime=Time-first(Time),
         DeltaTimeNext=lead(DeltaTime),
         TimeSinceStart=as_datetime(0, tz='Africa/Harare')+(min(Time)-MinTime)+DeltaTime,
         TimeSinceStartNext=TimeSinceStart+DeltaTimeNext-DeltaTime,
         Event=as_factor(Event)) %>% 
  ungroup() %>% 
  arrange(Model)

parallel_times_plt <- tbTiming %>%
  ggplot() +
  geom_point(aes(x=TimeSinceStart, y=Model, color=Event), size=3, shape=16, na.rm = T) +
  geom_segment(aes(x=TimeSinceStart, xend=TimeSinceStartNext,
                   y=Model, yend=Model, color=Event),
               size=2,
               arrow = arrow(length = unit(10, 'points'), ends = 'last', type = 'closed'), na.rm = T) +
  scale_color_manual(values = list(
    `Sourcing scripts`="#8DB6CD",
    `Calling run_model()`="#9BCD9B"
  )) +
  # scale_fill_viridis_d(option = "viridis", direction = -1) +
  scale_x_datetime(labels = scales::label_time('%M:%S'),
                   breaks = scales::breaks_width("15 secs")) +
  labs(title = "Runtimes for parallel model run",
       subtitle = paste0("Started at ",min(tbTiming$Time))) +
  ggthemes::theme_par() +
  theme(legend.position = "bottom")

parallel_times_plt

mo2 <- mo %>% lapply(first) %>% do.call(bind_rows,.) %>% ungroup %>% mutate(time=lubridate::ymd(paste0(year,'-1-1')))
mo2

mo2 %>%
  filter(str_starts(variable,'clin_Inc_'),age_group=='All') %>%
  mutate(variable='clin_Inc', age_group=as_factor(age_group)) %>% 
  ggplot() +
  aes(x = time, y = value, color = disease) +
  geom_line() +
  scale_color_hue(direction = 1) +
  theme_minimal() +
  facet_wrap(vars(disease), scales = 'free_y')

#### Plots ####
pltModelOutput(model_outputs = mo2 %>% mutate(scenario='Testing'), age_group="All")

#### Test Costing ####
parameters_baseline <- c(getModelParameters(), readRDS('../../DTPShiny/shinyInputParameters_Baseline.rds'))
moBaseline <- readRDS('../../DTPShiny/mo_baseline.rds')#run_model(parameters=parameters_baseline, timesteps=timesteps)

parameters_scenario <- c(getModelParameters(), readRDS('../../DTPShiny/shinyInputParameters_infant_maternal.rds'))
moScenario <- readRDS('../../DTPShiny/mo_maternal_infant.rds')#run_model(parameters=parameters_scenario, timesteps=timesteps)

source('models/R/scripts/calculateCosts.R')

costInputs <- parameters_scenario
costInputs['del_o_ps'] = 100-costInputs[['del_hf_ps']]
costInputs['del_o_b'] = 55
costInputs['del_o_ab'] = 44
costInputs['del_o_cb'] = 44
costInputs['del_o_m'] = 66
costInputs['del_o_icb'] = 1
costInputs['del_o_iab'] = 2

costInputs['unit_cpd_1'] = 56
costInputs['unit_cpd_ps'] = costInputs['unit_cpd_1']
costInputs['unit_cpd_b'] = 51
costInputs['unit_cpd_ab'] = 52
costInputs['unit_cpd_cb'] = 53
costInputs['unit_cpd_m'] = 54
costInputs['unit_cpd_i1'] = 56
costInputs['unit_cpd_ips'] = costInputs['unit_cpd_i1']
costInputs['unit_cpd_ib'] = 51
costInputs['unit_cpd_iab'] = 52
costInputs['unit_cpd_icb'] = 53
costInputs['unit_cpd_im'] = 54
ns=names(parameters_scenario)
nb=names(parameters_baseline)
full_join(
  tibble(name=ns,inS=T),
  tibble(name=nb,inB=T)
) %>% 
  mutate(across(where(is.logical),~ifelse(is.na(.x),F,.x))) %>%
  filter(!(inS&inB)) %>% view()

fix <- function(mo) {
  mo %>%
    mutate(variable=ifelse(variable=='v1prev_pred',
                           paste0(variable, '_', substr(disease,1,1)),
                           variable)) %>% 
    group_by(year, age_group, variable, disease) %>% 
    summarise(value=max(value), .groups = 'drop_last')
}

moBaseline <- fix(moBaseline)
moScenario <- fix(moScenario)

# TODO: Get the checkbox input from shiny app and pass into below function together with the intro_cost from shiny app.
# moScenario will have run with (maybe) many interventions and we want to know about:
# intro_cost_count = length(unique(c(infantsy*infant_boost, childsy*child_boost, adolescentsy*adolescent_boost,maternalsy*maternal_boost))) # for Uganda, not generally
costs <- calculateCosts(moBaseline, moScenario, costInputs = costInputs)

makeCostsTable(costs)
makeCostsPlot(costs)
