#Calibration script 
# to be integrated into parallel implementation of model 
calibrateModel <- function(parameters,
                           tbData,
                           timesteps=timesteps,
                           coverage_table = NULL,
                           iso3 = NULL,
                           dataType='rep_Clin',  # clin_Inc for gbd only
                           fitpar=c(ptrans_D = 0.01,
                                    ptrans_T = 0.01,
                                    ptrans_P = 0.1),
                           maxit=2) {
  if (!dataType %in% c('rep_Clin', 'clin_Inc')) {
    warning("dataType must be either 'rep_Clin' or 'clin_Inc'.")
  }
  dataType <- paste0(dataType, '_') # otherwise we'll include, eg, clin_Inc100k_D
  # TODO: check not null coverage_table and iso3
  LOG("Calibration function found that dataType={dataType}...")
  
  least_squares <- function(fitpar, tbData, timesteps, coverage_table, iso3, dataType) {
    LOG("Least squares running with dataType={dataType}...")
    parameters['ptrans_D'] <- fitpar['ptrans_D']
    parameters['ptrans_T'] <- fitpar['ptrans_T']
    parameters['ptrans_P'] <- fitpar['ptrans_P']
    
    # Run joint DTP function with parameters above that will overwrite get_shiny_parameters()
    mo <- run_model(
      parameters, 
      #timesteps, 
      timesteps=timesteps[timesteps <max(tbData$year+1)],
      coverage_table = coverage_table,
      iso3 = iso3,
      DEBUG = FALSE,  # no caching
      inParallel = FALSE # do not return a promise object
    )
    
    mo %>% 
      filter(variable %>% str_starts(dataType), age_group=="All") %>% 
      left_join(tbData%>%rename(dataValue=value), by=c('year', 'disease')) %>%
      group_by(disease) %>% 
      summarise(error = sum(value-dataValue, na.rm = T)^2, .groups = 'drop')  %>% 
      pull(error) %>%
      sum()
  }
  LOG("Fitting with with dataType={dataType}...")
  fit <- optim(fitpar,
               least_squares,
               tbData=tbData,
               timesteps=timesteps,
               coverage_table=coverage_table,
               iso3=iso3,
               dataType=dataType,
               control = list(maxit=maxit))  # maxit must change after debug
  # we could fit them all together minimise the joint SSE by standardising the errors...
  # or fit them one at at time
  fit
}

# TO RUN:
