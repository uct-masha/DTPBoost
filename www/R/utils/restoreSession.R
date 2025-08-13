restoreSession <- function(fnameSession, cost_data, simul_baseline, simul_packages, calibration_fit, session=getDefaultReactiveDomain()) {
  # This function restores a session.
  # To restore interventions it builds up the structures required for 
  # add_intervention_package and calls it on each.
  
  # remove the modal after a successful file upload
  removeModal()
  withProgress(message = "Uploading session.", {
    
    if (fnameSession == 'AFENET_Custom_Session.xlsx') {
      session_files <- tibble::tibble(Name = fnameSession)
    } else {
      session_files <- unzip(fnameSession, list = TRUE)
    }

    session_sheets <- map(session_files$Name, \(excel_file) {
      if (excel_file == "model_workbook.xlsx") {
        unzip(fnameSession, files = excel_file, exdir = tempdir())
        file_path <- file.path(tempdir(), excel_file)
        available_sheets <- excel_sheets(file_path)
        
        sheets <- available_sheets %>% 
          set_names() %>% 
          map(\(sheet) {
            read_excel(file_path, sheet = sheet)
          })
      } else if(fnameSession == 'AFENET_Custom_Session.xlsx') {
        available_sheets <- excel_sheets(fnameSession)
        sheets <- available_sheets %>% 
          set_names() %>% 
          map(\(sheet) {
            read_excel(excel_file, sheet = sheet)
          })
      } 
      else {
        unzip(fnameSession, files = excel_file, exdir = tempdir())
        file_path <- file.path(tempdir(), excel_file)
        available_sheets <- excel_sheets(file_path)
        
        sheets <- available_sheets %>% 
          set_names() %>% 
          map(\(sheet) {
            read_excel(file_path, sheet = sheet)
          })
        
        scenario_sheets <- list(epi_output = bind_rows(sheets)) 
        
      }
    }) %>% unlist(recursive = FALSE)
    
    available_sheets <- names(session_sheets)
    
    user_app_destination <- NULL # where to send the user after file upload
    ## parameters sheet ####
    if("parameters" %in% available_sheets) {
      
      parameters <- session_sheets$parameters
      coverage_table <- session_sheets$coverage_table
      
      # update coverage table
      updateReactable(
        outputId = "vaccineCoverageTable",
        data = coverage_table,
        session = session
      )
      
      user_app_destination <- "model2" # calibration
    }
    
    ## clinical_burden sheet ####
    if("clinical_burden" %in% available_sheets) {
      
      clinical_burden_table <- session_sheets$clinical_burden
      baseline_output <- session_sheets$baseline
      
      # update clinical burden table
      updateReactable(
        outputId = "custom_burden",
        data = clinical_burden_table,
        session = session
      )
      
      simul_baseline(baseline_output)
      
      # populate the calibration plot
      moNeeded <- simul_baseline() %>%
        filter(age_group=="All", variable %>% str_starts('rep_Clin_')) %>% 
        select(Year=year, modelValue=value, Disease=disease)
      
      calibration_data_source <- parameters %>% 
        filter(Parameter == "data_source_calibration") %>% 
        pull(Value1)
      
      selectedCountry <- parameters %>% 
        filter(Parameter == "selectedCountry") %>% 
        pull(Value1)
      
      caliTimeRange <- reactiveVal(c(2010, 2022))
      
      if (calibration_data_source == "GBD") {
        
        # load gbd data
        tbGBDIncidence <- read_rds(here("data/tbGBDIncidenceAllAges.rds"))
        
        plot_data <- tbGBDIncidence %>%
          filter(year%between%caliTimeRange(), iso3 == selectedCountry) %>%
          rename(Year=year, value=val, Disease=disease) %>% 
          left_join(moNeeded, by = c('Year', 'Disease')) %>% 
          mutate(across(where(is.numeric), round)) %>% 
          mutate(
            Disease = factor(Disease, levels = c("Diphtheria", "Tetanus", "Pertussis" ))
          )
        
        p <- gbd_calibration_fit(plot_data)
        
        calibration_fit(p)
        
      } else {
        plot_data <- getClinicalBurden(selectedCountry) %>%
          mutate(type="Data") %>% 
          select(Year, value, Disease) %>% 
          left_join(moNeeded, by = c('Year', 'Disease')) %>%
          filter(Year%between%caliTimeRange()) %>% 
          mutate(Disease = as_factor(Disease)) %>% 
          mutate(across(where(is.numeric), round))
        
        p <- who_calibration_fit(plot_data = plot_data)
        
        calibration_fit(p)
      }
      
      user_app_destination <- "model3" # intervention
    }
    
    ## strategies sheet ####
    if("strategies" %in% available_sheets) {
      booster_strategies <- session_sheets$strategies
      epi_upload <- session_sheets$epi_output
      econ_summary <- session_sheets$econ_output_summary
      econ_annual <- session_sheets$econ_output_annual
      
      # combine the econ datasets
      costs_upload <- list(
        resultSummary = econ_summary,
        resultAnnual = econ_annual
      )
      
      # update baseline
      baseline_output <- epi_upload %>% 
        filter(scenario == "Baseline") %>% 
        select(-scenario)
      
      simul_baseline(baseline_output)
      
      # get names of  uploaded strategies
      strategies <- booster_strategies %>% pull(strategy_name) %>% unique()
      
      for (strategy in strategies) {
        
        lsInterventions <- list()
        
        # switch ids for the selected strategy
        switch_ids <- booster_strategies %>%
          filter(strategy_name == strategy) %>%
          pull(SwitchId) %>%
          unique()
        
        for (switch_id in switch_ids) {
          
          booster_values <- list()
          
          selected_strategy <- booster_strategies %>%
            filter(Intervention!='All',
                   strategy_name == strategy,
                   SwitchId == switch_id)
          
          for (excel_id in selected_strategy$ParameterId) {
            if (excel_id %in% selected_strategy$ParameterId[selected_strategy$Type == 'dual_slider_input']) {
              booster_values[[excel_id]] <- c(selected_strategy$Value1[selected_strategy$ParameterId == excel_id],
                                              selected_strategy$Value2[selected_strategy$ParameterId == excel_id]) %>% as.numeric()
            } else if (excel_id %in% selected_strategy$ParameterId[selected_strategy$Type %in% c('numeric_input','slider_input')]) {
              booster_values[[excel_id]] <- selected_strategy$Value1[selected_strategy$ParameterId == excel_id] %>% as.numeric()
            } else {
              booster_values[[excel_id]] <- selected_strategy$Value1[selected_strategy$ParameterId == excel_id]
            }
          }
          
          lsInterventions[[switch_id]] <- booster_values
          
        }
        
        results_epi <- epi_upload %>% 
          filter(scenario == strategy) %>% 
          select(-scenario)
        
        ecoVals =  booster_strategies %>%
          filter(Intervention=="All",
                 strategy_name == strategy) %>%
          mutate(ParameterId=str_replace(ParameterId,'strat_','')) %>%
          pull(Value1, name = ParameterId) %>%
          as.list
        
        # TODO: More general way to do this because datatype is in the Type column
        ecoVals$intro_cost = ecoVals$intro_cost %>% as.numeric()
        ecoVals$intro_cost_pfin = ecoVals$intro_cost_pfin %>% as.numeric()
        ecoVals$intro_once = ecoVals$intro_once %>% as.logical()
        
        currency <- parameters %>% 
          filter(Parameter == "selectedCurrency") %>% 
          pull(Value1)
        
        # update currency with the saved currency
        updateRadioGroupButtons(
          session = session,
          inputId = "selectedCurrency",
          choices = unique(c("USD",currency)), # if currency = USD just show one USD
          selected = currency
        )
        
        add_intervention_package(
          package_name = strategy, 
          mo = results_epi, 
          lsInterventions = lsInterventions, 
          ecoVals = ecoVals, 
          simul_packages = simul_packages
        )
      }
      
      user_app_destination <- "model4" # explore results
    }
    
    # Update all sliders with one value
    if(!is_empty(parameters$Parameter[parameters$Type == 'slider_input'])) {
      for (input_excel in parameters$Parameter[parameters$Type == 'slider_input']){
        updateSliderInput(session = session, inputId = input_excel, value = parameters$Value1[parameters$Parameter == input_excel] %>% as.numeric())
      }
    }
    
    # Update all sliders with two values
    if(!is_empty(parameters$Parameter[parameters$Type == 'slider_input_double'])) {
      for (input_excel in parameters$Parameter[parameters$Type == 'slider_input_double']){
        updateSliderInput(session = session, inputId = input_excel,
                          value = c(parameters$Value1[parameters$Parameter == input_excel], parameters$Value2[parameters$Parameter == input_excel]) %>% as.numeric())
      }
    }
    
    # Update all select input values
    if(!is_empty(parameters$Parameter[parameters$Type == 'select_input'])) {
      for (input_excel in parameters$Parameter[parameters$Type == 'select_input']){
        updateNumericInput(session = session, inputId = input_excel, value = parameters$Value1[parameters$Parameter == input_excel])
      }
    }
    
    # Update all numeric input values
    if(!is_empty(parameters$Parameter[parameters$Type == 'numeric_input'])) {
      for (input_excel in parameters$Parameter[parameters$Type == 'numeric_input']){
        updateNumericInput(session = session, inputId = input_excel, value = parameters$Value1[parameters$Parameter == input_excel] %>% as.numeric())
      }
    }
    
    # Update all text area inputs
    if(!is_empty(parameters$Parameter[parameters$Type == 'text_area_input'])) {
      for (input_excel in parameters$Parameter[parameters$Type == 'text_area_input']){
        updateTextAreaInput(session = session, inputId = input_excel, value = parameters$Value1[parameters$Parameter == input_excel])
      }
    }
    
    # Update all radio button inputs
    if(!is_empty(parameters$Parameter[parameters$Type == 'radio_button'])) {
      for (input_excel in parameters$Parameter[parameters$Type == 'radio_button']){
        updateRadioGroupButtons(session = getDefaultReactiveDomain(), inputId = input_excel, selected = parameters$Value1[parameters$Parameter == input_excel])
      }
    }
    #update all cost data
    cost_ids <- cost_data() %>% pull(inputId)
    cost_parameters <- parameters %>%
      filter(Parameter %in% cost_ids) %>%
      select(inputId = Parameter, value = Value1)
    if (length(cost_ids) == nrow(cost_parameters)) cost_data(cost_parameters)

    p <- function(inputId) {
      parameters$Value1[parameters$Parameter==inputId]
    }

    tbCostParmsCurrency <- p('selectedCurrency')
    updateCosts(tbCostParms = cost_data,
                tbCostParmsCurrency = tbCostParmsCurrency,
                selectedCurrency = tbCostParmsCurrency,
                vv_ps = p('vv_ps'),
                vv_b =  p('vv_b'),
                vv_cb = p('vv_cb'),
                vv_ab = p('vv_ab'),
                vv_m =  p('vv_m'),
                session = session
    )
    
    updateTabsetPanel(session, "menu", selected = user_app_destination)
    
  })
}