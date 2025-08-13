# Model developed by MASHA at UCT
# https:://masha.uct.ac.za
# Disease: Diphtheria, Tetanus and Pertussis
run_model <- function(parameters, timesteps, coverage_table, iso3,
                      DEBUG=F, cacheAs=NULL, inParallel=F,
                      caliStart = 2010, modelStart = 2020) {
  if (!DEBUG) cacheAs <- NULL
  if (!is.null(cacheAs) && file.exists(cacheAs)) {
    LOG("FOUND A CACHED VERSION AT {cacheAs}! WOOHOO!", LEVEL$DEBUG)
    if (inParallel) return(future_promise({readRDS(cacheAs)}))
    return(readRDS(cacheAs))
  }
  LOG("Running the full DTP model. Be patient :)", LEVEL$INFO)
  if (inParallel) {
    if (!length(c('package:promises','package:future') %>% setdiff(search()))==0) {
      warning('Parallel processing of the model requires the promises and future libraries - loading those now')
      library(promises)
      library(future)
    }
    if (!'call' %in% names(attributes(future::plan()))) {
      warning("You should set up your own parallelisation plan - using default: `plan(multisession)`")
      plan(multisession)
    }
    currentPlan <- deparse(attributes(future::plan())$call)
    if (currentPlan!='plan(multisession)') {
      warning('You chose `',currentPlan,'`, whereas `plan(multisession)` is recommended. Continuing anyway.')
    }
    
    timestepsWarmup = timesteps[timesteps<=caliStart]
    timestepsCali = timesteps[timesteps%between%c(caliStart, modelStart)]
    timestepsModel = timesteps[timesteps>=modelStart]
    
    promisedResults <- promise_all(
      p.D=future_promise({
        t1=Sys.time()
        source(here::here('models/R/models/commonModelCode.R'))
        source(here::here('models/R/models/diphtheriaModel.R'))
        LOG("D.Model run start mem_used: {pryr::mem_used()}")
        initialConditionsWarmup <- makeInitialConditionsCode.D(mtMod_D, parameters, coverage_table, ISO3 = iso3)
        t2=Sys.time()
        # Run the model to get the conditions at the warmup endpoint
        moRawWarmup = run_model.D(parameters, initialConditionsWarmup, timestepsWarmup,
                                  returnRawModel = T, returnPostprocessedModel = F)
        
        # Run the model with warmup endpoint as initial conditions
        initialConditionsCalibration = unname(moRawWarmup[nrow(moRawWarmup),2:ncol(moRawWarmup)])
        moCali = run_model.D(parameters, initialConditionsCalibration, timestepsCali,
                             returnRawModel = T, returnPostprocessedModel = T)
        moRawCali = moCali$moRaw
        moPostprocessingCali = moCali$moPostprocessing %>% 
          last() %>% 
          ungroup() %>% 
          filter(year < modelStart)
        
        # ** The garbage collector now has a chance to collect tranode created inside run_model
        
        # Run the model with calibration endpoint as initial conditions
        initialConditionsModel = unname(moRawCali[nrow(moRawCali),2:ncol(moRawCali)])
        moPostprocessingModel = run_model.D(parameters, initialConditionsModel, timestepsModel,
                                            returnRawModel = F, returnPostprocessedModel = T) %>% 
          last() %>% 
          ungroup()
        
        # Construct a tibble from calibration and model run tibbles
        # This is what we have always returned but now it's from 2010
        LOG("bind_rows(moPostprocessingCali, moPostprocessingModel)")
        mo = bind_rows(moPostprocessingCali, moPostprocessingModel)
        
        t3=Sys.time()
        LOG("Diphtheria (parallel) finished, time: {t3-t2}")
        LOG("D.Model run end mem_used: {pryr::mem_used()}")
        return(list(mo=mo,t1=t1,t2=t2,t3=t3))
      }),
      p.T=future_promise({
        t1=Sys.time()
        source(here::here('models/R/models/commonModelCode.R'))
        source(here::here('models/R/models/tetanusModel.R'))
        LOG("T.Model run start mem_used: {pryr::mem_used()}")
        initialConditionsWarmup <- makeInitialConditionsCode.T(mtMod_T, parameters, coverage_table, ISO3 = iso3)
        t2=Sys.time()
        # Run the model to get the conditions at the warmup endpoint
        moRawWarmup = run_model.T(parameters, initialConditionsWarmup, timestepsWarmup,
                                  returnRawModel = T, returnPostprocessedModel = F)
        
        # Run the model with warmup endpoint as initial conditions
        initialConditionsCalibration = unname(moRawWarmup[nrow(moRawWarmup),2:ncol(moRawWarmup)])
        moCali = run_model.T(parameters, initialConditionsCalibration, timestepsCali,
                             returnRawModel = T, returnPostprocessedModel = T)
        moRawCali = moCali$moRaw
        moPostprocessingCali = moCali$moPostprocessing %>% 
          last() %>% 
          ungroup() %>% 
          filter(year < modelStart)
        
        # Run the model with calibration endpoint as initial conditions
        initialConditionsModel = unname(moRawCali[nrow(moRawCali),2:ncol(moRawCali)])
        moPostprocessingModel = run_model.T(parameters, initialConditionsModel, timestepsModel,
                                            returnRawModel = F, returnPostprocessedModel = T) %>% 
          last() %>% 
          ungroup()
        
        # Construct a tibble from calibration and model run tibbles
        # This is what we have always returned but now it's from 2010
        LOG("bind_rows(moPostprocessingCali, moPostprocessingModel)")
        mo = bind_rows(moPostprocessingCali, moPostprocessingModel)
        
        t3=Sys.time()
        LOG("Tetanus (parallel) finished, time: {t3-t2}")
        LOG("T.Model run end mem_used: {pryr::mem_used()}")
        return(list(mo=mo,t1=t1,t2=t2,t3=t3))
      }),
      p.P=future_promise({
        t1=Sys.time()
        source(here::here('models/R/models/commonModelCode.R'))
        source(here::here('models/R/models/pertussisModel.R'))
        LOG("P.Model run start mem_used: {pryr::mem_used()}")
        initialConditionsWarmup <- makeInitialConditionsCode.P(mtMod_P, parameters, coverage_table, ISO3 = iso3)
        
        t2=Sys.time()
        # Run the model to get the conditions at the warmup endpoint
        moRawWarmup = run_model.P(parameters, initialConditionsWarmup, timestepsWarmup,
                               returnRawModel = T, returnPostprocessedModel = F)
        
        # Run the model with warmup endpoint as initial conditions
        initialConditionsCalibration = unname(moRawWarmup[nrow(moRawWarmup),2:ncol(moRawWarmup)])
        moCali = run_model.P(parameters, initialConditionsCalibration, timestepsCali,
                             returnRawModel = T, returnPostprocessedModel = T)
        moRawCali = moCali$moRaw
        moPostprocessingCali = moCali$moPostprocessing %>% 
          last() %>% 
          ungroup() %>%
          filter(year < modelStart)
        
        # Run the model with calibration endpoint as initial conditions
        initialConditionsModel = unname(moRawCali[nrow(moRawCali),2:ncol(moRawCali)])
        moPostprocessingModel = run_model.P(parameters, initialConditionsModel, timestepsModel,
                                            returnRawModel = F, returnPostprocessedModel = T) %>% 
          last() %>% 
          ungroup()
        
        # Construct a tibble from calibration and model run tibbles
        LOG("bind_rows(moPostprocessingCali, moPostprocessingModel)")
        mo = bind_rows(moPostprocessingCali, moPostprocessingModel)
        
        t3=Sys.time()
        LOG("Pertussis (parallel) finished, time: {t3-t2}")
        LOG("P.Model run end mem_used: {pryr::mem_used()}")
        return(list(mo=mo,t1=t1,t2=t2,t3=t3))
      })
    ) %...>% {PostPostProc(.,parameters)}
    LOG("DPT.Model run end mem_used: {pryr::mem_used()}")
    if (!is.null(cacheAs)) {
      return(promisedResults %...>% {
        object <- .
        saveRDS(object = object, file=cacheAs)
        object
      })
    }
    return(promisedResults)
  } else {
    listResults <- lapply(c('D','T','P'),
                          function(modelLetter) {
                            t1=Sys.time()
                            initCondFunc <- eval(parse(text=paste0('makeInitialConditionsCode.',modelLetter)))
                            runFunc <- eval(parse(text=paste0('run_model.',modelLetter)))
                            mtMod <- eval(parse(text=paste0('mtMod_',modelLetter)))
                            initialConditions <- initCondFunc(mtMod, parameters, coverage_table, ISO3 = iso3)
                            t2=Sys.time()
                            mo <- runFunc(parameters, initialConditions, timesteps) %>%
                              last() %>%
                              ungroup() %>% 
                              filter(year < endyear) 
                            t3=Sys.time()
                            LOG("Model {modelLetter} (sequential) finished, time: {t3-t2}")
                            return(list(mo=mo,t1=t1,t2=t2,t3=t3))
                          })
    LOG("DONE WITH FULL MODEL! (ran sequentially)", LEVEL$INFO)
    if (!is.null(cacheAs)) {
      saveRDS(listResults, cacheAs)
    }
    LOG("PostPostProc")
    return(PostPostProc(listResults,parameters))
  }
  LOG("*mem_used: {pryr::mem_used()}")
  LOG("Model runs and PostPostProc are complete.")
}

PostPostProc <- function(listResult, parameters) {
  LOG("PostPostProc...")
  LOG("mem_used: {pryr::mem_used()}")
  saveRDS(listResult, 'eo_listResult.rds')
  saveRDS(parameters, 'eo_parameters.rds')
  # listResult=readRDS( 'eo_listResult.rds')
  # parameters=readRDS( 'eo_parameters.rds')
  # TODO: Add timing information
  DISoutyr <- listResult %>% map(first) %>% do.call(bind_rows, .) %>% ungroup()
  
  ppData <- DISoutyr %>%
    filter(age_group=='All', str_starts(variable, 'doses_')) %>%
    select(year,variable,value) %>%
    pivot_wider(names_from = variable, values_from=value) %>% 
    as.list()
  
  DISoutDoses <- with(as.list(c(parameters, ppData)), {
    wdy <- names(whenDose_1)%in%year
    
    doses_1 <- unname(whenDose_1[wdy])*pmax(doses_1_D, doses_1_T, doses_1_P)
    doses_2 <- unname(whenDose_2[wdy])*pmax(doses_2_D, doses_2_T, doses_2_P)
    doses_3 <- unname(whenDose_3[wdy])*pmax(doses_3_D, doses_3_T, doses_3_P)
    
    doses_m_T=(doses_mm1_T+2*doses_mm2_T)
    doses_m <- unname(whenDose_mb[wdy])*doses_m_T
    doses_im <- unname(whenDose_imb[wdy])*doses_m_T
    
    doses_b  <- unname(whenDose_b[wdy])*pmax(doses_b_D, doses_b_T, doses_b_P)
    doses_ib <- unname(whenDose_ib[wdy])*pmax(doses_b_D, doses_b_T, doses_b_P)
    
    doses_cb <- unname(whenDose_cb[wdy])*pmax(doses_cb_D, doses_cb_T, doses_cb_P)
    doses_icb<- unname(whenDose_icb[wdy])*pmax(doses_cb_D, doses_cb_T, doses_cb_P)
    
    doses_ab <- unname(whenDose_ab[wdy])*pmax(doses_ab_D, doses_ab_T, doses_ab_P)
    doses_iab<- unname(whenDose_iab[wdy])*pmax(doses_ab_D, doses_ab_T, doses_ab_P)
    
    # year age_group variable    disease  value unit
    tibble(year = year,
           doses_1 = doses_1,
           doses_2 = doses_2,
           doses_3 = doses_3,
           doses_m = doses_m,
           doses_im = doses_im,
           doses_b  = doses_b  ,
           doses_ib = doses_ib ,
           doses_cb = doses_cb ,
           doses_icb = doses_icb,
           doses_ab = doses_ab ,
           doses_iab = doses_iab) %>% 
      pivot_longer(!year, names_to = 'variable') %>% 
      mutate(age_group="All", disease="All", unit="Annual") %>% 
      select(year,age_group,variable,disease,value,unit)
  })
  LOG("Returning from PostPostProc - mem_used: {pryr::mem_used()}")
  bind_rows(DISoutyr, DISoutDoses)  # TODO: time=year?
}

pltPostProc  <- function(mo, varname='inc_pred', age_group=NULL) {
  tbData <- mo %>%
    moToFives %>% 
    dplyr::mutate(Date=lubridate::date_decimal(time)) %>% 
    dplyr::filter(variable==varname)
  if (is.null(age_group)) {
    plt <- ggplot(tbData) +
      aes(x=Date, y=value, group=disease,colour=disease) +
      geom_line(size=1.5) +
      theme_minimal() +
      facet_wrap(vars(age_group)) +
      labs(title=as.character(glue::glue("{varname} plot per age group")),
           y=varname, color="Disease")
  } else {
    plt <- ggplot(tbData %>% filter(age_group==age_group)) +
      aes(x=Date, y=value, group=disease,colour=disease) +
      geom_line(size=1.5) +
      theme_minimal() +
      labs(title=as.character(glue::glue("{varname} plot for ages = {age_group}")),
           y=varname, color="Disease")
  }
  plt
}
pltModelOutput <- function(model_outputs,  # model_outputs() in shiny rbinds the annual model output tibbles after adding a scenario column
                           age_group = 'All',
                           scenarios = c("Baseline", "InfantBoost+ MaternalImmune"),
                           sim_window = c(2020,2030),
                           disease = "All",
                           count_type = "Total",
                           output_variable = "Clinical cases",
                           scale_option = "free_y") {
  
  # We need to split out the variable names to simpler names
  mo_simplified <- model_outputs %>%
    # This will be standard unless we change postprocs:
    mutate(variable_simple=variable%>%str_replace('_[DTP]+$','')%>%str_replace('100k$',''),
           variable_count_type=ifelse(str_detect(variable,'100k'),'Rate','Total'),
           variable_disease=str_extract(variable,'[DTP]+$'))
  
  # mo_simplified %>%
  #   distinct(variable,variable_simple,variable_count_type,variable_disease,disease) %>% 
  #   arrange(variable_simple)
  
  # mo_simplified
  
  # We need to map output_variable through parameter sheet outputs
  tbOutputs <- readxl::read_excel('models/parameters/parametersDTP.xlsx',sheet='outputs') %>% 
    mutate(variable=Name) %>%
    select(variable, Output, everything())
  # tbOutputs
  
  # Some work to filter the inputs
  diseases <- c("Diphtheria","Tetanus","Pertussis")
  
  if (disease!="All") diseases = disease
  LOG('scenario = {scenarios}')
  if(output_variable=="Population protected") {
    count_type = "Total"
    warning("Population protected not plottable as a Rate")
  }
  mo_simplified %>%
    left_join(tbOutputs, by = "variable") %>% 
    filter(disease %in% diseases,
           scenario %in% scenarios,
           year %>% between(sim_window[1], sim_window[2]),
           variable_count_type == count_type,
           Output == output_variable,
           age_group == !!age_group) %>% 
    mutate(scenario = factor(scenario, levels = scenarios))-> mo_simplified_result
  saveRDS(mo_simplified_result, 'mo_simplified_result.RDS')
  
  n <- mo_simplified_result %>% pull(scenario) %>% unique() %>% length() # figure out how many scenarios there are and choose a name for the color palette
  palette_name <- case_when(
    n == 1 ~ "tol1dtp",
    n == 2 ~ "tol2dtp",
    n == 3 ~ "tol3dtp",
    n == 4 ~ "tol4dtp",
    n == 5 ~ "tol5dtp",
    n == 6 ~ "tol6dtp",
    n == 7 ~ "tol7dtp",
    n == 8 ~ "tol8dtp",
    n == 9 ~ "tol9dtp",
    n == 10 ~ "tol10dtp",
    TRUE ~ "tol21rainbow"
  )
  
  if (output_variable == "Population protected") {
    p <- ggplot(mo_simplified_result) +
      aes(
        x = factor(year), fill = scenario, y = value,
        text = sprintf(
          "Value: %s<br>Year: %s<br>Scenario: %s", 
          scales::percent(value, accuracy = 1L), 
          year,
          scenario
        )
      ) +
      geom_col(position = "dodge") +
      scale_fill_hue(direction = 1) +
      scale_y_continuous(labels = scales::percent) +
      scale_fill_dtp_d(name = palette_name) +
      labs(
        # x = "",
        # y = glue::glue("{count_type}"),
        title = glue::glue("{output_variable} plot for age={age_group}"),
        caption = "Note that these outputs are results of models which are still in development",
        fill = "Scenario"
      ) +
      theme_minimal() +
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal"
      ) +
      facet_wrap(vars(disease), scales = scale_option, ncol = 1L)
  } else {
    
    # When Diphtheria cases are under 0.5 we should just display a single 0 
    # on the epi output graphs.
    mo_simplified_result <- mo_simplified_result %>% 
      mutate(value = if_else(value < 0.5, 0, value))
    
    p <- ggplot(mo_simplified_result) +
      aes(
        x = factor(year), fill = scenario, y = value,
        text = sprintf(
          "Value: %s<br>Year: %s<br>Scenario: %s", 
          format(round(value), big.mark = ",", scientific = FALSE), 
          year,
          scenario
        )
      ) +
      geom_col(position = "dodge") +
      scale_fill_hue(direction = 1) +
      scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
      scale_fill_dtp_d(name = palette_name) +
      labs(
        # x = "",
        # y = glue::glue("{count_type}"),
        title = glue::glue("{output_variable} plot for age={age_group}"),
        caption = "Note that these outputs are results of models which are still in development",
        fill = "Scenario"
      ) +
      theme_minimal() +
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal"
      ) +
      facet_wrap(vars(disease), scales = scale_option, ncol = 1L)
  }
  ggplotly(p, tooltip = c("text")) %>% 
    layout(
      legend=list(orientation='h'),
      margin = list(l = 80, r = 40)
    ) %>%
    plotly::config(
      toImageButtonOptions = list(filename = glue::glue("epi-output-{output_variable}-plot"))
    )
}

moToFives <- function(mo, addTotal=TRUE) {
  tbAgeMapping <- readRDS("models/inputs/tbModelAges.rds")
  fiveYearBands <- paste(seq(0,70,5),
                         seq(4,74,5),
                         sep='-') %>%
    c('75+') %>%
    as_factor()
  tbResult <- mo %>%
    left_join(tbAgeMapping, by=c(age_group="agecat")) %>%
    group_by(time,variable,disease,fiveYearCat) %>% 
    summarise(value=sum(value), .groups='drop') %>% 
    mutate(age_group=fiveYearBands[fiveYearCat])
  if (addTotal==TRUE) {
    tbResultTotals <- tbResult %>% 
      group_by(time,variable,disease) %>%
      summarise(age_group="All", value=sum(value), .groups='drop')
    tbResult <- tbResult %>%
      bind_rows(tbResultTotals) %>% 
      mutate(age_group=age_group %>% as_factor()) %>% 
      arrange(time,variable,disease,age_group)
  }
  tbResult %>%
    ungroup() %>%
    select(time,variable,disease,age_group,value)
}
