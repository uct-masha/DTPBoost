source(here::here("www/R/utils/getShinyInputs.R"))
source(here::here("www/R/packages.R"))

tauMapping <- tibble::tribble(
  # Map tau column to vaccine input (primary series, infant/child/adolescent booster, maternal)
  ~colName,   ~inputBaseline,  ~inputAge,
  "tau_1_T", "vv_ps",   'va_p1',
  "tau_2_T", "vv_ps",   'va_p2',
  "tau_3_T", "vv_ps",   'va_p3',
  "tau_4_T",    "vv_b",   'va_b',
  "tau_5_T",   "vv_cb",   'va_cb',
  "tau_6_T",   "vv_ab",   'va_ab',
  "tau_mi_T",   "vv_m",  'vmba',
  "tau_mm_T",   "vv_m",  'vmba', 
  "tau_vmm_T",   "vv_m", 'vmba', # this is a unpregnanting state, history of maternal vaccination
  "tau_1_D", "vv_ps",   'va_p1',
  "tau_2_D", "vv_ps",   'va_p2',
  "tau_3_D", "vv_ps",   'va_p3',
  "tau_b_D",    "vv_b",   'va_b',
  "tau_cb_D",   "vv_cb",  'va_cb',
  "tau_ab_D",   "vv_ab",  'va_ab',
  "tau_mm_D",   "vv_m",  'vmba',
  "tau_mi_D",   "vv_m",  'vmba', 
  "tau_mm_D",   "vv_m",  'vmba',
  "tau_vmm_D",   "vv_m", 'vmba',
  "tau_1_P", "vv_ps",   'va_p1',
  "tau_2_P", "vv_ps",   'va_p2',
  "tau_3_P", "vv_ps",   'va_p3',
  "tau_b_P",    "vv_b",   'va_b',
  "tau_cb_P",   "vv_cb",  'va_cb',
  "tau_ab_P",   "vv_ab",  'va_ab',
  "tau_mi_P",   "vv_m",  'vmba',
  "tau_mm_P",   "vv_m",  'vmba', 
  "tau_vmm_P",   "vv_m", 'vmba',
)

findColumnsWithDifferentValues = function(tau_output) {
  for (colName in names(tau_output)) {
    colValue = tau_output[[colName]]
    if (length(unique(range(colValue)))!=1) {
      print(colName)
    }
  }
}

checkZeros = function(tau_output, baseline, intervention) {
  for (colName in names(tau_output)) {
    colValue = tau_output[[colName]]
    numZeros = sum(colValue==0)
    if (numZeros>0) {
      warning(glue::glue("{baseline}_{intervention}: Found {numZeros} zeros in {colName}"))
    }
  }
}

testTaus <- function(baseline='default', intervention='maternal') {
  dir_shinyInputs = here::here(paste0("getShinyInputsObjects_", baseline, '_', intervention))
  stopifnot(fs::dir_exists(dir_shinyInputs))
  
  input <- readRDS(fs::path_join(c(dir_shinyInputs,'input.rds')))
  coverage_table <- readRDS(fs::path_join(c(dir_shinyInputs,'coverage_table.rds')))
  startyear <- readRDS(fs::path_join(c(dir_shinyInputs,'startyear.rds')))
  tyears <- readRDS(fs::path_join(c(dir_shinyInputs,'tyears.rds')))
  
  result1 = getShinyInputVaccineTaus(input, baseline=intervention=='none', startyear, tyears)
  result2 = result1 %>% matrixToNamedList()
  checkZeros(result2, baseline, intervention)
  findColumnsWithDifferentValues(result2)
  result2
}

default_none = testTaus(baseline = 'default', intervention = 'none')
default_maternal = testTaus(baseline = 'default', intervention = 'maternal')
infant_none = testTaus(baseline = 'infant', intervention = 'none')
default_infant = testTaus(baseline = 'default', intervention = 'infant')
maternal_none = testTaus(baseline = 'maternal', intervention = 'none')
maternal_maternal = testTaus(baseline = 'maternal', intervention = 'maternal')

# Run more checks ####
# Check 2 column in https://docs.google.com/spreadsheets/d/1zD3pBliv3TVxTujKJg67rptcBuCIYDdqG-6yOM5jxGg/edit#gid=0
# Expect default+none to have maternal Diphtheria values 0.2 everywhere
stopifnot(
  all.equal(unname(default_none$tau_mm_D),
            rep_along(default_none$tau_mm_D, 0.2))
  )
# Expect maternal+maternal to give tau_b_P = 0.2 everywhere
stopifnot(
  all.equal(unname(maternal_maternal$tau_b_P),
            rep_along(maternal_maternal$tau_b_P,0.2))
)


# Expect infant+none to give tau_b_P = 1/6 everywhere
stopifnot(
  all.equal(unname(infant_none$tau_b_P),
            rep_along(infant_none$tau_b_P,1/6))
  )
# Expect defaults+infant to give tau_b_P = c(0.2,...,1.6,...)
stopifnot(
  all.equal(unique(default_infant$tau_b_P), c(0.2, 1/6))
  )

infant_none$tau_b_P
infant_none$tau_mm_D


maternal_maternal$tau_mi_P
maternal_maternal$tau_mm_P
maternal_maternal$tau_vmm_P
