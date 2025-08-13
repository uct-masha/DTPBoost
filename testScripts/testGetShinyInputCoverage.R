source(here::here("www/R/utils/getShinyInputs.R"))
source(here::here("www/R/packages.R"))
source(here::here('models/R/models/commonModelCode.R'))

testGetShinyInputCoverage = function(baseline='default', intervention='maternal') {
  dir_shinyInputs = here::here(paste0("getShinyInputsObjects_", baseline, '_', intervention))
  
  stopifnot(fs::dir_exists(dir_shinyInputs))
  
  input <- readRDS(fs::path_join(c(dir_shinyInputs,'input.rds')))
  startyear <- 2000
  tyears <- 40
  
  coverage_table = getVaccineSchedule('UGA', fillBlanks = T)
  
  # Ensure intervention is set to run
  input[[glue("boost_{intervention}_switch")]] = (intervention!='none')
  
  LOG(glue("input$boost_{intervention}_switch = {(intervention!='none')}"))
  
  # Set maternal target coverage
  input$vtc_im = 97
  input$vtc_im2 = 94
  getShinyInputCoverage(input, coverage_table=coverage_table, baseline=(intervention=='none'), startyear=startyear, tyears=tyears)
}

parms = testGetShinyInputCoverage(baseline='default', intervention='maternal')
parms$covm[50,]
