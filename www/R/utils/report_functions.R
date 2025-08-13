report_pltModelOutput <- function(model_outputs,  # model_outputs() in shiny rbinds the annual model output tibbles after adding a scenario column
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
  tbOutputs <- params$tbOutputs
  # tbOutputs
  
  # Some work to filter the inputs
  diseases <- c("Diphtheria","Tetanus","Pertussis")
  
  if (disease!="All") diseases = disease
  mo_simplified %>%
    left_join(params$tbOutputs, by = "variable") %>% 
    filter(disease %in% diseases,
           scenario %in% scenarios,
           year %>% between(sim_window[1], sim_window[2]),
           variable_count_type == count_type,
           Output == output_variable,
           age_group == !!age_group) %>% 
    mutate(scenario = factor(scenario, levels = scenarios))-> mo_simplified_result
  
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
      aes(x = year, fill = scenario, y = value) +
      geom_col(position = "dodge") +
      scale_fill_hue(direction = 1) +
      scale_y_continuous(labels = scales::percent) +
      scale_fill_dtp_d(name = palette_name) +
      labs(
        # x = "",
        # y = glue::glue("{count_type}"),
        title = glue::glue("{output_variable} across all ages"),
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
    p <- ggplot(mo_simplified_result) +
      aes(x = year, fill = scenario, y = value) +
      geom_col(position = "dodge") +
      scale_fill_hue(direction = 1) +
      scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
      scale_fill_dtp_d(name = palette_name) +
      labs(
        # x = "",
        # y = glue::glue("{count_type}"),
        title = glue::glue("{output_variable} across all ages"),
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
  
  p
}


report_makeEpiTable <- function(epiEconResultSummary, DIS=NULL, scenarios=NULL,
                                resSimWindow,
                                tbMapping=tibble::tribble(
                                   ~variableSimple, ~pretty,
                                   "clin_Inc",      "Clinical cases (Total)",
                                   "deaths",        "Deaths (Total)",
                                   "avert_clin",    "Averted clinical cases**",
                                   "avert_death",   "Averted deaths**"
                                 )) {
  if (is.null(DIS)) DIS <- c('DTP','Diphtheria','Tetanus','Pertussis')
  if (is.null(scenarios)) scenarios <- unique(epiEconResultSummary$scenario)
  
  getDiseaseName <- function(variable) {
    v=variable %>% str_split('_') %>% first %>% last
    case_when(
      v=='D'~'Diphtheria',
      v=='T'~'Tetanus',
      v=='P'~'Pertussis',
      v=='DTP'~'DTP',
      TRUE~'All')
  }
  
  # getVariableSimple <- function(variable) {
  #   regex <- '_[DTP]{1,3}$'
  #   result <- ifelse(str_detect(variable,regex),str_replace(variable,regex,''),variable)
  #   result
  # }
  
  getVariableSimple <- function(variable) {
    re = '(_[DTP]{1,3})?(_fin)?$'
    str_replace(variable, pattern = re,'')
  }
  
  mm <- tibble(diseaseDst=c('DTP','Diphtheria','Tetanus','Pertussis')) %>% mutate(disease='All')
  
  tblEpi <- epiEconResultSummary %>% 
    filter(discounting=="Undiscounted") %>%
    select(!discounting) %>%
    rowwise() %>% 
    mutate(disease=getDiseaseName(variable),
           variableSimple=getVariableSimple(variable)) %>% 
    left_join(tbMapping, by = "variableSimple") %>% 
    select(type,disease,scenario,pretty,value) %>% 
    filter(!is.na(pretty)) %>% 
    distinct() %>%
    ungroup() %>% 
    left_join(mm, by = "disease") %>%
    filter(scenario %in% scenarios,
           disease %in% DIS) %>% 
    mutate(disease=if_else(is.na(diseaseDst),disease,diseaseDst),
           type=if_else(type=='Comparison','Scenario',type),
           pretty=factor(pretty,levels=unique(tbMapping$pretty)),
           disease=factor(disease,levels=mm$diseaseDst)) %>% 
    select(!diseaseDst) %>% 
    arrange(pretty) %>% 
    pivot_wider(id_cols = c('type','disease','scenario'), names_from='pretty', values_from='value') %>% 
    arrange(disease,type) %>% 
    rename(Scenario=scenario, Disease=disease) %>%
    select(!type)
  
  myColDef <- colDef(na = "-",
                     format = colFormat(separators = TRUE, digits = 0))
  
  columns = rep(list(myColDef), nrow(tbMapping)) %>% as.list() %>% setNames(object = ., nm = tbMapping$pretty)
  
  if (length(DIS)==1) {
    tblEpi <- tblEpi %>% select(!Disease)
  }
  
  gt(data = tblEpi) %>% 
    sub_missing(
      columns = everything(),
      rows = everything(),
      missing_text = "-"
    ) %>% 
    fmt_number(
      columns = -c(Scenario),
      sep_mark = ",",
      decimals = 0
    ) %>% 
    tab_source_note(
      source_note = glue("* Outputs are shown as the total for the full model timeframe ({resSimWindow[1]} - {resSimWindow[2]})")
    ) %>%
    tab_source_note(
      source_note = "** Compared to Baseline"
    )
}

report_makeCostsPlot <- function(costsComparisons, costs, scenario, 
                                 DIS="DTP", currency="USD", 
                                 isFinancial=F, isDiscounted=T) {
  
  net_costTot_DTP = if_else(isFinancial, 'net_costTot_DTP_fin', 'net_costTot_DTP')
  net_costVacc = if_else(isFinancial, 'net_costVacc_fin', 'net_costVacc')
  net_cost_intro = if_else(isFinancial, 'net_cost_intro_fin', 'net_cost_intro')
  
  disc_val = if_else(isDiscounted, 'Discounted', 'Undiscounted')
  y = costsComparisons[[scenario]][[disc_val]][c(
    net_costVacc,
    net_cost_intro,
    'net_tot_coi_DTP',
    net_costTot_DTP
  )]
  names(y) <- c(
    'net_costVacc',
    'net_cost_intro',
    'net_tot_coi_DTP',
    'net_costTot_DTP'
  )
  years = costsComparisons[[scenario]]$year
  
  plot_data <- y %>% as_tibble %>% 
    mutate(years=years) %>% 
    pivot_longer(c(net_costVacc, net_cost_intro, net_tot_coi_DTP)) %>%
    mutate(
      name = case_match(name, 
                        'net_costVacc' ~ "Cost of vaccination (routine delivery)",
                        'net_cost_intro' ~ "Cost of vaccine introduction (fixed)",
                        "net_tot_coi_DTP" ~ "Treatment costs averted",
                        .default = name
      )
    ) %>% 
    mutate(
      value = if_else(
        abs(value) < 10 ^ 6,
        signif(value, digits = 3),
        round(value / 1000) * 1000
      )
    ) %>%
    mutate(
      net_costTot_DTP = if_else(
        abs(net_costTot_DTP)  < 10 ^ 6,
        signif(net_costTot_DTP , digits = 3),
        round(net_costTot_DTP / 1000) * 1000
      )
    )
  
  # custom fill colours
  cols <- c(
    "Cost of vaccination (routine delivery)" = "#4477AA",
    "Cost of vaccine introduction (fixed)" = "#CC6677",
    "Treatment costs averted" = "#44AA99"
  )
  
  ggplot(plot_data) +
    aes(
      x = years, y = value, fill = factor(name), group = 1,
      text = sprintf(
        glue::glue("{name}: %s<br>Year: %s<br>"),
        format(value, big.mark = ",", scientific = FALSE), 
        years
      )
    ) +
    geom_col() +
    geom_line(
      aes(
        y=net_costTot_DTP,
        text = sprintf(
          "Net total cost: %s<br>Year: %s<br>",
          format(net_costTot_DTP, big.mark = ",", scientific = FALSE), 
          years
        ),
        color = "Net total cost",
        fill = NULL
      ), 
      linewidth=1.05
    ) +
    scale_fill_manual(values = cols) +
    scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
    scale_color_manual(name = "", values = c("Net total cost" = "black")) +
    labs(
      title = glue::glue(
        "Annual costs (vaccination and treatment) for {scenario}\ncompared to baseline ({currency})"
      ),
      x = NULL,
      y = NULL,
      fill = NULL
    ) +
    theme_minimal() +
    theme(legend.position = "bottom") +
    guides(fill = guide_legend(nrow = 2, byrow = TRUE))
}

report_makeCostsPlot2 <- function(costs, scenarios=NULL, startYear=2023, currency="USD", isFinancial=F, isDiscounted=F) {
  tbData <- costs$resultAnnual
  if (!is.null(scenarios)) {
    tbData <- tbData %>% filter(scenario %in% scenarios)
    n <- tbData %>% pull(scenario) %>% unique() %>% length() # figure out how many scenarios there are and choose a name for the color palette
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
  }
  costVacc_wIntro <- 'costVacc_wIntro'
  if (isFinancial) {
    costVacc_wIntro <- paste0(costVacc_wIntro, '_fin')
  }
  
  analysisRange = range(costs$resultAnnual$year)
  yearText = glue::glue("{analysisRange[[1]]} - {analysisRange[[2]]}")
  
  tbData %>%
    filter(variable==costVacc_wIntro,
           discounting==if(isDiscounted){"Discounted"}else{"Undiscounted"},
           year>=startYear) %>%
    ggplot() +
    aes(x = year, y = value, color = scenario) +
    geom_line(size=.5) +
    geom_point(size=1.5) +
    scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
    scale_colour_dtp_d(palette_name) +
    expand_limits(y = 0) +
    theme_minimal() +
    # theme(legend.position = "bottom") +
    labs(title=glue::glue("Total annual cost of vaccination, {yearText} ({currency})"), color="Scenario") +
    xlab('') +
    ylab('')
}

report_makeCostsTable <- function(costs, scenarios, DIS=NULL, 
                                  currency="USD",econ_type = NULL,
                                  isFinancial=F, isDiscounted=F,
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
                                  )) {
  # tbMapping is a tibble with `variableSimple` and `pretty`, eg tot_coi and "Cost of illness"
  # Commenting out so we don't filter by DIS
  if (is.null(DIS)) {
    DIS <- c('All','DTP','Diphtheria','Tetanus','Pertussis')
  }
  
  # Total cost	Cost of vaccination	Cost of illness	Clinical cases	Deaths	Cost per clinical case averted	Cost per death averted
  
  getDiseaseName <- function(variable) {
    re = '_[DTP]{1,3}(_fin)?$'
    dis = str_match(string = variable, pattern = '_([DTP]{1,3})(_fin)?') %>% `[`(, 2)
    case_match(dis,
               'D'~'Diphtheria',
               'T'~'Tetanus',
               'P'~'Pertussis',
               'DTP'~'DTP',
               .default = 'All')
  }
  
  getVariableSimple <- function(variable) {
    re = '(_[DTP]{1,3})?(_fin)?$'
    str_replace(variable, pattern = re,'')
  }
  
  mm <- tibble(diseaseDst=c('DTP','Diphtheria','Tetanus','Pertussis')) %>% mutate(disease='All')
  tbMappingWithFin <- tbMapping %>% mutate(variableSimple = paste0(variableSimple,'_fin'))
  tbMappingBoth <- bind_rows(tbMapping, tbMappingWithFin)
  
  resultSummary = costs$resultSummary %>%
    filter(discounting==if(isDiscounted){"Discounted"}else{"Undiscounted"}) %>%
    select(!discounting)
  
  finVarNames <- resultSummary %>% filter(str_ends(variable, '_fin')) %>% pull(variable) %>% unique()
  finCostsVarNamesToExclude <- if(isFinancial) {
    finVarNames %>% str_replace('_fin$','')
  } else {
    finVarNames
  }
  
  tblCostsBasic <- resultSummary %>% 
    rowwise() %>% 
    mutate(disease=getDiseaseName(variable),
           variableSimple=getVariableSimple(variable)) %>% 
    left_join(tbMappingBoth, by = "variableSimple") %>% 
    ungroup() %>%
    # Either throw out economic costs or financial costs:
    filter(! (variable %in% finCostsVarNamesToExclude))
  
  tblCosts <- tblCostsBasic %>%
    select(type,disease,scenario,pretty,value) %>% 
    filter(!is.na(pretty)) %>% 
    distinct() %>%
    ungroup() %>% 
    left_join(mm, by = "disease", relationship = "many-to-many") %>%
    filter(scenario %in% scenarios,
           disease %in% DIS) %>% 
    mutate(disease=if_else(is.na(diseaseDst),disease,diseaseDst),
           type=if_else(type=='Comparison','Scenario',type),
           pretty=factor(pretty,levels=unique(tbMapping$pretty)),
           disease=factor(disease,levels=mm$diseaseDst)) %>% 
    select(!diseaseDst) %>% 
    arrange(pretty) %>% 
    pivot_wider(id_cols = c('type','disease','scenario'), names_from='pretty', values_from='value') %>% 
    arrange(disease,type) %>% 
    rename(Scenario=scenario, Disease=disease) %>%
    select(!type) %>%
    filter(Disease==last(DIS)) %>%
    select(!Disease)
  
  if ('Cost of illness' %in% colnames(tblCosts))
    tblCosts$`Cost of illness` <- if_else(tblCosts$`Cost of illness`!=0, tblCosts$`Cost of illness`, NA)
  
  myColDef <- colDef(na = "-",
                     format = colFormat(separators = TRUE, digits = 0))
  
  columns = rep(list(myColDef), nrow(tbMapping)) %>% setNames(.,tbMapping$pretty)
  
  # replace all zeros with na
  tblCosts <- tblCosts %>% 
    mutate(across(where(is.numeric), ~na_if(., 0)))
  
  bi_columns <- c(
    "Total cost",
    "Cost of vaccination",
    "Cost of illness",
    "Cost of infant booster",
    "Cost of child booster",
    "Cost of adolescent booster",
    "Cost of maternal"
  )
  
  ce_columns <- c(
    "Total cost",
    "Cost of vaccination",
    "Cost of illness",
    # "Clinical cases (Total)",
    # "Deaths (Total)",
    # "Cost per clinical case averted"
    "Cost per death averted"
  )
  
  if (econ_type == "bi") {
    mil_columns <- bi_columns
  } else {
    mil_columns <- ce_columns
  }
  
  
  if (currency == "USD") {
    tbl <-  tblCosts %>%
      flextable() %>% 
      font(part = "all", fontname = "Roboto") %>%
      hline_top(
        part="all",
        border = officer::fp_border(color = "black", style = "solid", width = 0.9)
      ) %>%
      hline_bottom(
        border = officer::fp_border(color = "black", style = "solid", width = 0.9)
      ) %>% 
      set_table_properties(align = "left") %>% 
      align(align = "center", part = "header") 
  } else {
    tbl <-  tblCosts %>%
      mutate(across(mil_columns, ~ .x/1000000)) %>% 
      flextable() %>% 
      font(part = "all", fontname = "Roboto") %>%
      hline_top(
        part="all",
        border = officer::fp_border(color = "black", style = "solid", width = 0.9)
      ) %>%
      hline_bottom(
        border = officer::fp_border(color = "black", style = "solid", width = 0.9)
      ) %>% 
      set_table_properties(align = "left") %>% 
      align(align = "center", part = "header") %>% 
      colformat_num(j =mil_columns, suffix = "Mil")
  }
  
  tbl 
}


# point plot of net_costTot vs avert_clin/avert_death across all years (summary not annual) depending on selectedInput
report_makeCostsPlot3 <- function(costs, x='avert_clin', scenarios=NULL, DIS=NULL, currency="USD",
                           isFinancial=F, isDiscounted=F) {
  tbData <- costs$resultSummary %>%
    filter(discounting==if(isDiscounted){"Discounted"}else{"Undiscounted"}) %>%
    select(!discounting)
  
  if (is.null(DIS)) DIS <- c('DTP','Diphtheria','Tetanus','Pertussis')
  if (is.null(scenarios)) scenarios <- unique(tbData$scenario)
  
  getDiseaseName <- function(variable) {
    re = '_[DTP]{1,3}(_fin)?$'
    dis = str_match(string = variable, pattern = '_([DTP]{1,3})(_fin)?') %>% `[`(, 2)
    case_match(dis,
               'D'~'Diphtheria',
               'T'~'Tetanus',
               'P'~'Pertussis',
               'DTP'~'DTP',
               .default = 'All')
  }
  
  getVariableSimple <- function(variable) {
    result <- ifelse(str_detect(variable,'_[DTP]{1,3}(_fin)?$'),str_replace(variable,'_[DTP]{1,3}',''),variable)
    result
  }
  
  tbData = tbData %>%
    filter(str_detect(variable,'^avert_|^net_costTot')) %>%
    rowwise() %>%
    mutate(disease=getDiseaseName(variable),
           variableSimple=getVariableSimple(variable)) %>%
    ungroup() %>%
    filter(disease %in% DIS, scenario %in% scenarios) %>%
    select(!variable) %>%
    pivot_wider(names_from = variableSimple)
  
  prettyName <- if(x=='avert_clin'){'Averted Cases'}else{'Averted Deaths'}
  
  palette_name <- tbData %>%
    distinct(scenario) %>%
    nrow() %>% # figure out how many scenarios there are and choose a name for the color palette
    case_match(
      1 ~ "tol1dtp",
      2 ~ "tol2dtp",
      3 ~ "tol3dtp",
      4 ~ "tol4dtp",
      5 ~ "tol5dtp",
      6 ~ "tol6dtp",
      7 ~ "tol7dtp",
      8 ~ "tol8dtp",
      9 ~ "tol9dtp",
      10 ~ "tol10dtp",
      .default = "tol21rainbow"
    )
  
  net_costTot = "net_costTot"
  if(isFinancial){
    net_costTot = paste0(net_costTot,'_fin')
  }
  
  analysisRange = range(costs$resultAnnual$year)
  yearText = glue::glue("{analysisRange[[1]]} - {analysisRange[[2]]}")
  
  tbData %>%
    ggplot() +
    aes_string(x = x, y = net_costTot) +
    aes(colour = scenario) +
    geom_point(shape = "circle", size = 4L) +
    scale_color_hue(direction = 1) +
    theme_minimal() +
    expand_limits(x = 0, y = 0) +
    scale_x_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
    scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
    scale_colour_dtp_d(palette_name) +
    labs(title=glue::glue("Incremental Cost Effectiveness, {yearText} ({currency})"), color="Scenario") +
    xlab(prettyName) +
    ylab(glue::glue('Net total cost ({yearText})'))
}

## Add net total cost plot for all scenarios 
report_makeCostsPlot4 = function(costsComparisons,
                          DIS="DTP", currency="USD", 
                          isFinancial=F, isDiscounted=T) {
  
  get_plot_data <- function(costsComparisons,  
                            DIS="DTP", currency="USD", 
                            isFinancial=F, isDiscounted=T) {
    
    net_costTot_DTP = if_else(isFinancial, 'net_costTot_DTP_fin', 'net_costTot_DTP')
    disc_val = if_else(isDiscounted, 'Discounted', 'Undiscounted')
    
    all_scenarios_data <- list()
    
    for (scenario in seq_along(names(costsComparisons))) {
      
      net_costTot_DTP = if_else(isFinancial, 'net_costTot_DTP_fin', 'net_costTot_DTP')
      net_costVacc = if_else(isFinancial, 'net_costVacc_fin', 'net_costVacc')
      net_cost_intro = if_else(isFinancial, 'net_cost_intro_fin', 'net_cost_intro')
      
      disc_val = if_else(isDiscounted, 'Discounted', 'Undiscounted')
      y = costsComparisons[[scenario]][[disc_val]][c(
        net_costVacc,
        net_cost_intro,
        'net_tot_coi_DTP',
        net_costTot_DTP
      )]
      names(y) <- c(
        'net_costVacc',
        'net_cost_intro',
        'net_tot_coi_DTP',
        'net_costTot_DTP'
      )
      years = costsComparisons[[scenario]]$year
      
      plot_data <- y %>% as_tibble %>% 
        mutate(years=years) %>% 
        pivot_longer(c(net_costVacc, net_cost_intro, net_tot_coi_DTP)) %>%
        mutate(
          name = case_match(name, 
                            "net_costVacc" ~ "Cost of vaccination (routine delivery)",
                            "net_cost_intro" ~ "Cost of vaccine introduction (fixed)",
                            "net_tot_coi_DTP" ~ "Treatment costs averted",
                            .default = name
          )
        ) %>% 
        mutate(
          value = if_else(
            abs(value) < 10 ^ 6,
            signif(value, digits = 3),
            round(value / 1000) * 1000
          )
        ) %>%
        mutate(
          net_costTot_DTP = if_else(
            abs(net_costTot_DTP)  < 10 ^ 6,
            signif(net_costTot_DTP , digits = 3),
            round(net_costTot_DTP / 1000) * 1000
          )
        ) %>% 
        mutate("scenario" = names(costsComparisons)[scenario])
      
      all_scenarios_data[[scenario]] <- plot_data
      
    }
    
    
    plot_data <- bind_rows(all_scenarios_data)
  }
  
  combined_plot_data <- get_plot_data(costsComparisons, 
                                      DIS="DTP", currency="USD", 
                                      isFinancial=isFinancial, 
                                      isDiscounted=isDiscounted)
  
  palette_name <- combined_plot_data %>%
    distinct(scenario) %>%
    nrow() %>% # figure out how many scenarios there are and choose a name for the color palette
    case_match(
      1 ~ "tol1dtp",
      2 ~ "tol2dtp",
      3 ~ "tol3dtp",
      4 ~ "tol4dtp",
      5 ~ "tol5dtp",
      6 ~ "tol6dtp",
      7 ~ "tol7dtp",
      8 ~ "tol8dtp",
      9 ~ "tol9dtp",
      10 ~ "tol10dtp",
      .default = "tol21rainbow"
    )
  
  p <- ggplot(combined_plot_data) +
    aes(
      x = years,
      y=net_costTot_DTP,
      color = scenario
    ) +
    geom_line(linewidth=1.05) +
    scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
    scale_colour_dtp_d(palette_name) +
    labs(
      title = glue::glue(
        "Net total cost (vaccination and treatment) compared to baseline ({currency})"
      ),
      x = NULL,
      y = NULL,
      color = "Scenario"
    ) +
    theme_minimal() + 
    theme(legend.position = "bottom")
}