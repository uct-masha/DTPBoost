# helper data sets
# LOG <- makeLogger(default_level = LEVEL$TRACE)
here <- here::here

app_read_excel <- function(path, sheet = NULL, range = NULL, col_names = TRUE, 
                           col_types = NULL, na = "", trim_ws = TRUE, skip = 0, n_max = Inf, 
                           guess_max = min(1000, n_max), progress = readxl_progress(), 
                           .name_repair = "unique") {
  LOG("app_read_excel called with path={path}")
  read_excel(path=path, sheet = sheet, range = range, col_names = col_names, 
             col_types = col_types, na = na, trim_ws = trim_ws, skip = skip, n_max = n_max, 
             guess_max = guess_max, progress = progress, 
             .name_repair = .name_repair)
}

exchange_rates <- readr::read_csv(here('data/tbExchangeRates.csv'),
                                  col_types = readr::cols(
                                    year = readr::col_integer(),
                                    ISO3_Code = col_character(),
                                    Country = col_character(),
                                    average_rate = col_double(),
                                    Code = col_character()
                                    )
                                  )

toUSD <- readRDS(here("models/inputs/tbUNLoc.rds")) %>% 
  left_join(exchange_rates, by='ISO3_Code') %>% 
  select(Code, value=average_rate) %>% 
  drop_na() %>%
  pull(value,name = Code)

country_iso <- dplyr::as_tibble(readRDS(here("models/inputs/tbUNLoc.rds")))

# standard styling of the (reactable) tables
reactable_format <- function(df) {
	 reactable(
      df,
      pagination = FALSE, height = 250,
      defaultColDef = colDef(
        format = colFormat(separators = TRUE),
        headerStyle = list(background = "#f7f7f8")
      ),
      columns = list(
        Year = colDef(
          format = colFormat(separators = FALSE),
          style = list(position = "sticky", left = 0, background = "#f7f7f8", zIndex = 1 ,borderRight = "1px solid #eee"),
          headerStyle = list(position = "sticky", left = 0, background = "#f7f7f8", zIndex = 1, borderRight = "1px solid #eee")
        )
      )
    )
}

# input styling
sliderInputReg <- function (label,...) {
  fluidRow(
    column(12, div(class = 'reg', label), sliderInput(label = NULL, ticks = FALSE,...))
  )
}
numericInputReg <- function (label,...) {
  fluidRow(
    column(12, div(class = 'reg', label), numericInput(label = NULL,...))
  )
}
selectionInputReg <- function (label,...) {
  fluidRow(
    column(12, div(class = 'reg', label), selectInput(label = NULL,...))
  )
}

# function to get the coverage data
get_coverage <- function(country) {
  LOG("Getting the coverage for {country}")
  coverage_tbl <- getVaccineSchedule(country, fillBlanks = T)
  return(coverage_tbl)
}

# function to get clinical burden data
get_clinical_burden <- function(data_source, country) {
  LOG("Getting the burden ({data_source}) for {country}")
  if (data_source == "WHO") {
    burden_data <- getClinicalBurden(country) %>% 
      pivot_wider(Year, names_from = Disease, values_from = value) %>% 
      arrange(desc(Year))
  } else {
    burden_data <- read_rds(here("data/tbGBDIncidenceAllAges.rds")) %>% 
      filter(iso3 == country) %>% 
      pivot_wider(year, names_from = disease, values_from = val) %>% 
      rename(Year = year)
  }
  
  return(burden_data)
}

# function to get created strategies
get_strategies <- function(simulations) {
  LOG("get_strategies() called")
  LOG("get_strategies() called with {length(simulations)} simulations")
  all_strategies <- list()
  # get the default template
  fname <- here("data/DTPApp-BoosterStrategies.xlsx")
  booster_strategies_mapping <- app_read_excel(fname)
  
  for (i in seq_along(simulations)) {
    simulation <- simulations[[i]]
    LOG("- {simulation$name}")
    # update template with strategy name
    booster_strategies <- booster_strategies_mapping %>%
      mutate(strategy_name = simulation$name)
    
    # get ids of selected switches
    switch_ids <- names(simulation$interventions)
    
    # update template with actual values for each intervention
    for (switch_id in switch_ids) {
      LOG("--  switch_id = {switch_id}")
      excel_ids <- booster_strategies %>%
        filter(SwitchId == switch_id) %>%
        pull(ParameterId)
      for (excel_id in excel_ids) {
        boostMatchIds <- (booster_strategies$ParameterId == excel_id)
        if (sum(boostMatchIds)==0) warning(paste0("Warning unable to attach simulation parameter '",excel_id,"' to excel sheet: value not in excel sheet."))
        if (booster_strategies$Type[boostMatchIds] == "dual_slider_input") {
          booster_strategies$Value1[boostMatchIds] = simulation$interventions[[switch_id]][[excel_id]][1]
          booster_strategies$Value2[boostMatchIds] = simulation$interventions[[switch_id]][[excel_id]][2]
        } else {
          if (excel_id %in% names(simulation$interventions[[switch_id]])) {
            booster_strategies$Value1[boostMatchIds] = simulation$interventions[[switch_id]][[excel_id]]
          }
        }
      }
    }
    
    booster_strategies <- booster_strategies %>% 
      mutate(across(everything(), as.character)) %>% 
      drop_na(Value1)
    
    ecoVals = simulation$ecoVals
    intro_cost_tbl <- tibble(
      Intervention = "All",
      SwitchId = booster_strategies %>% pull(SwitchId) %>% first(),
      Label = c(
        "Cost of introduction (total)",
        "Proportion of fixed introduction costs paid directly by government",
        "Introduction cost will only be incurred once"
      ),
      Type = c("select_input", "slider_input", "radio_button"),
      ParameterId = paste0('strat_', names(ecoVals)),
      Value1 = unname(as.character(ecoVals)),
      Value2 = NA_character_,
      strategy_name = booster_strategies %>% pull(strategy_name) %>% first()
    ) %>% 
      mutate(across(everything(), as.character))
    
    booster_strategies <- bind_rows(booster_strategies, intro_cost_tbl)
    
    booster_strategies[is.na(booster_strategies)] <- ""
    
    all_strategies[[i]] <- booster_strategies
  }
  LOG("Stitching the outputs")
  strat_dfs <- bind_rows(all_strategies)
  return(strat_dfs)
}

#### Costs
# function to combine the country iso code data (in the models folder) with the
# exchange rates data
get_exchange_rates <- function() {
  LOG("getting exchange rates")
  exchange_rates_iso <- exchange_rates %>% 
    left_join(country_iso, by = 'ISO3_Code')
  
  return(exchange_rates_iso)
}

convert_currency <- function(amount, src_currency, dst_currency) {
  LOG("convert_currency({amount}, {src_currency}, {dst_currency})")
  if (src_currency==dst_currency) return(amount)
  amount*toUSD[[dst_currency]]/toUSD[[src_currency]]
}
# convert from USD to local currency
convert_fromUSD <- function(usd_amount, currency) {
  LOG("convert_fromUSD({usd_amount}, {currency})")
  exchange_rate <- get_exchange_rates() %>% 
    filter(Code == req(currency)) %>% 
    pull(average_rate) %>% 
    first() %>% 
    as.numeric()
  result <- as.numeric(usd_amount) * exchange_rate
  if (!is.numeric(result) || length(result)==0)
    warning(glue::glue("issue converting {usd_amount} to {currency}: value is {result} with class {class(result)}"))
  
  return(result)
}

# create default output table for epi results
output_tbl <- function() {
  
  tbl <- tibble(
    Scenario = "Baseline",
    `Clinical cases (Total)` = NA,
    `Deaths (Total)` = NA,
    `Clinical cases averted**` = NA,
    `Deaths averted**` = NA
  )
  
  return(tbl)
}

# create default output table for economic output results
eo_output_tbl <- function() {
  
  tbl <- tibble(
    Scenario = "Baseline",
    `Total Cost` = NA,
    `Cost of vaccination` = NA,
    `Cost of Illness` = NA,
    `Clinical cases` = NA,
    Deaths = NA,
    `Cost per clinical case averted` = NA,
    `Cost per death averted` = NA
  )
  
  return(tbl)
}

add_intervention_package <- function(package_name, mo, lsInterventions, ecoVals, simul_packages) {
  LOG("add_intervention_package({package_name}, mo, lsInterventions, ecoVals, simul_packages)")
  simul_packages_this <- list(
    name = package_name,
    results_epi = mo,
    interventions=lsInterventions,
    ecoVals=ecoVals
  )
  
  simul_packages$simul[[simul_packages$nb + 1]] <- simul_packages_this
  simul_packages$nb <- simul_packages$nb + 1
  simul_packages$names <- c(simul_packages$names, package_name)
  LOG("add_intervention_package mem_used: {pryr::mem_used()}")
}

getOrCreate <- function(fname, toCreate, ...) {
  if (!file.exists(fname)) {
    result <- toCreate(...)
    saveRDS(result, fname)
  }
  readRDS(fname)
}

alwaysCreate <- function(fname, toCreate, ...) {
  toCreate(...)
}

# When not debugging you can always do this
getOrCreate <- alwaysCreate

makeBucketList <- function(packagesAsHTMLRankList,
                           inputIdShowList="rank_list_show",
                           inputIdHideList="rank_list_hide") {
  LOG("makeBucketList() called with show='{inputIdShowList}', hide='{inputIdHideList}' and packagesAsHTMLRankList.")
  # LOG("[{paste0(packagesAsHTMLRankList, collapse=', ')}]")
  
  bucket_list(
    #header = "Baseline & Strategies (Display/Hide)",
    header = NULL,
    group_name = "bucket_list_group",
    orientation = "horizontal",
    add_rank_list(
      text = "Drag here to display",
      labels = c("Baseline", packagesAsHTMLRankList),
      input_id = inputIdShowList
    ),
    add_rank_list(
      text = "Drag here to hide",
      labels = NULL,
      input_id = inputIdHideList
    )
  )
}


js <- 'var fmtMSS = function(s){return(s-(s%=60))/60+(9<s?":":":0")+s};
  var expectedTimeSeconds = 6*60;
  var useProgressBar = true;
  var move = function() {
    console.log("MOV");
    var elem = document.getElementById("myBar");
    window.startTime = new Date();
    window.id = setInterval(function() {
      console.log("FRAME");
      let elem = document.getElementById("myBar");
      if (!elem)clearInterval(window.id);
      let endTime = new Date();
      let deltaTimeSeconds = Math.floor((endTime-window.startTime)/1000)
      let width = Math.max(1, Math.min(100, Math.floor((deltaTimeSeconds/expectedTimeSeconds)*100)))
      if (useProgressBar) {
        elem.style.width = width + "%";
      } else {
        elem.style.width = "100%";
      }
      elem.innerHTML = fmtMSS(deltaTimeSeconds); // Keep a timer inside here
    }, 555);
}'

css <- "
  .nav li a.disabled,.sidebar-menu a.disabled {
  opacity: 0.4 !important;
  cursor: not-allowed !important;
}"

# validation functions

validateCustomCoverage <- function(tbCov) {
  # tbCov: resulting coverage tibble from read_excel()
  # returns: either TRUE if validation passed or a list of problematic columns if it failed
  isProblematicColumn <- logical(ncol(tbCov))
  for (i in seq(1,ncol(tbCov))) {
    colname = colnames(tbCov)[[i]]
    col <- tbCov[[colname]]
    if (is.character(col) && suppressWarnings(sum(is.na(as.numeric(col)))!=sum(is.na(col)))) {
      isProblematicColumn[[i]] = TRUE
    } else {
      num.col = as.numeric(col)
      if (colname == "Year") {
        mostRecentDataYear = year(Sys.Date())-1
        if (max(num.col) < mostRecentDataYear) {
          LOG("The issue here is the most recent year {mostRecentDataYear} was not filled for the custom coverage to be uploaded")
        }
        inBounds = num.col %between% c(1950, mostRecentDataYear)
      } else {
        inBounds = num.col %between% c(0,1)
      }
      isNa = is.na(inBounds)
      isProblematicColumn[[i]] = !all(inBounds | isNa)
    }
  }
  problematicColumns = colnames(tbCov)[isProblematicColumn]
  validationFailed = length(problematicColumns) > 0
  if (validationFailed) return(problematicColumns)
  return(TRUE)
}

# Calibration fit plots implemented in ggplot for the report

cols <- c(
  "Diphtheria" = "#4477AA",
  "Tetanus" = "#44AA99",
  "Pertussis" = "#CC6677"
)

who_calibration_fit <- function(plot_data) {
  ggplot(
    data = plot_data,
    mapping = aes(
      x = Year, y = value, colour = Disease, group = 1,
      text = sprintf(
        "Value: %s<br>Year: %s<br>", 
        format(value, big.mark = ",", scientific = FALSE), 
        Year
      )
    )
  ) +
    geom_point(size = 3, color='Black') +
    geom_line(aes(
      y=modelValue, 
      text = sprintf(
        "Model Value:%s<br>Year: %s<br>",  
        format(modelValue, big.mark = ",", scientific = FALSE), 
        Year
      )
    ),
    size = 0.5
    ) +
    expand_limits(y = 0) +
    scale_color_hue(direction = 1) +
    scale_colour_manual(values = cols) +
    labs(
      # x = NULL,
      # y = "Reported Incidence",
      shape=''
    ) +
    theme_minimal() +
    theme(axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          legend.position="none") +
    scale_x_continuous(breaks=scales::pretty_breaks())+
    scale_y_continuous(labels = scales::comma) +
    facet_wrap(vars(Disease), scales='free_y', nrow = 1)
}

gbd_calibration_fit <- function(plot_data) {
  ggplot(plot_data) +
    aes(
      x = Year, y = value, colour = Disease,  group = 1,
      text = sprintf(
        "Value: %s<br>Year: %s<br>", 
        format(value, big.mark = ",", scientific = FALSE), 
        Year
      )
    ) +
    geom_errorbar(aes(ymin=lower, ymax=upper), width=.2, color='#7E7E7E') +
    geom_point(color='Black') +
    geom_line(aes(y=modelValue, text = sprintf(
      "Model Value:%s<br>Year: %s<br>",  
      format(modelValue, big.mark = ",", scientific = FALSE), 
      Year
    )),size = 0.5) +
    expand_limits(y = 0) +
    scale_colour_manual(values = cols) +
    scale_y_continuous(labels = scales::comma) +
    scale_x_continuous(breaks = scales::pretty_breaks()) +
    theme_minimal() +
    theme(axis.title.y = element_blank(),
          axis.title.x = element_blank()
    ) +
    # labs(x = NULL, y = "Estimated Incidence") +
    facet_wrap(vars(Disease), scales = "free_y", nrow = 1)
}

# function to plot the coverage over time
coverage_plot <- function(xx, yy, y_start, y_end, startyear) { 
  ggplot(data = data.frame(x = xx, y = yy)) +
    geom_step(aes(x = x, y = y), color = "#222222") +
    geom_point(aes(x = x, y = y), color = "#237D80", size = 2) +
    geom_vline(xintercept = y_start, linetype = "dashed", color = '#237D80') +
    geom_vline(xintercept = y_end, linetype = "dashed", color = '#237D80') +
    labs(x = "Year", y = "Coverage (%)") +
    scale_x_continuous(
      limits = c(2015, endyear), # endyear(2040) is a global variable
      breaks = c(2015, startyear, y_start, y_end, startyear+tyears) # tyears is a global variable
    ) +
    scale_y_continuous(limits=c(0, 1), labels=scales::label_percent(accuracy = 1)) +
    theme_minimal()
}