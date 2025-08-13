# Notes: 
#---The model structure is written in R.
#---Simulations are run in R with a C compiler.

#### Install and load packages ####
library(tidyverse)
library(deSolve)
library(ggplot2)
library(glue)
library(readxl)
library(data.table)
library(lubridate)
library(Rcpp)
library(here)
library(crayon)

options(tidyverse.quiet = TRUE)
source(here::here('models/R/scripts/getModelParameters.R'))
source(here::here("models/R/scripts/utils.R"))
source(here::here('models/R/scripts/mt_utils.R'))
# LOG <- makeLogger('trace')


#### Rcpp transitions to equations -------------------------------------------
LOG('Compiling CPP')
cppFunction('
  void EQ(NumericVector eq, NumericVector transit, 
          IntegerVector transitionsiu1, IntegerVector transitionsiu2, 
          IntegerVector transitionsiv1, IntegerVector transitionsiv2) {
    int i, iu1, iu2, iv1, iv2;
    // Zero out eq
    eq.fill(0.0);
    // Fill equations with new deltas
    //Rcpp::Rcout << transit.length() << std::endl;
    for(i=0; i<transit.length(); i++) {
      iu1 = transitionsiu1[i]-1;
      iv1 = transitionsiv1[i];
      iu2 = transitionsiu2[i]-1;
      iv2 = transitionsiv2[i];
      
      // Include transition differential in the relevant eq components
      
      eq[iu1] += transit[i]*iv1;
      eq[iu2] += transit[i]*iv2;
      
      //Rcpp::Rcout << "eq["<<iu1<<"] += "<<transit[i]<<"*"<<iv1<<";" << std::endl;
      //Rcpp::Rcout << "eq["<<iu2<<"] += "<<transit[i]<<"*"<<iv2<<";" << std::endl;
    }
  }
')
#### Model start-up definitions ####
N <- 56   # number of patches (age bands)
#### Import Data ####

startyear <- 2000 # starting year of simulation 2015-01-01
endyear <- 2040
tyears <- endyear - startyear # total years of simulation
dtout <- 1/365 # output timestep
timesteps <- startyear+seq(0, tyears, dtout) # time vector

LOG('Prepping data')
dfAge <- readRDS("models/inputs/tbModelAges.rds") %>% mutate(age_group=as_factor(agecat))
getPopData <- function(iso3, year=2010) {
  dfAge <- readRDS("models/inputs/tbModelAges.rds") %>% mutate(age_group=as_factor(agecat))
  tbUNRaw = readRDS("models/inputs/tbUNData.rds") %>%
    filter(ISO3_Code==iso3)
  # Find the nearest year for which we have population data
  getPopTotColName = function(tbUNRaw, year) {
    valid_pop_years = str_extract(colnames(tbUNRaw),'popTot\\d{4}') %>% Filter(function(x)!is.na(x),.) %>% parse_number()
    popTotYearIndex = which.min(abs(valid_pop_years-year))
    popTotYear = valid_pop_years[popTotYearIndex]
    paste0('popTot',popTotYear)
  }
  # Create a new column
  LOG('overwriting tbUNRaw$popTot = tbUNRaw[[{getPopTotColName(tbUNRaw, year=year)}]]')
  tbUNRaw$popTot = tbUNRaw[[getPopTotColName(tbUNRaw, year=year)]]
  tbUNRaw %>% 
    mutate(fiveYearCat=as.integer(pop_age)) %>%
    left_join(dfAge, by="fiveYearCat") %>%
    summarise(popTot=popTot*prop,
              mortTot=mortTot*prop,
              mortRate=mortRate,
              birthRate=birthRate,
              femProp=femProp,
              fertProp=fertProp,
              fiveYearCat=fiveYearCat,
              .by=age_group) %>%
    mutate(age_group=factor(age_group, levels = levels(dfAge$age_group)))
}

getLocMap <- function() {
  as_tibble(readRDS("models/inputs/tbUNLoc.rds"))
}

getVaccineSchedule <- function(iso3, fillBlanks=F) {
  result = as_tibble(readRDS("models/inputs/tbCoverage.rds")) %>% 
    filter(iso3=={{iso3}})
  
  if (fillBlanks) {
    result = result %>%
      # Fill forward then backward in time
      arrange(Year) %>% 
      fill(DTPCV1:TTCV2, .direction = 'downup') %>% 
      # Replace columns which were entirely NA with all 0's
      mutate(
        across(where(is.numeric), ~replace_na(.x, 0))
      ) %>%
      # DTP2 data never exists so interpolate between 1 and 3
      mutate(DTPCV2=(DTPCV1+DTPCV3)/2) %>%
      # TTCV2 data is larger than 100% in some cases
      mutate(TTCV2=pmin(1,TTCV2))
  }
  result %>%
    select(Year,
           DTPCV1, DTPCV2, DTPCV3,
           InfantBooster,
           ChildBooster,
           AdolescentBooster,
           ANC, TTCV2) %>%
    arrange(desc(Year))
}

getClinicalBurden <- function(iso3, source="WHO") {
  if (source=="WHO") {
    result <- readRDS("models/inputs/tbDiseases.rds") %>%
      as_tibble() %>%
      filter(ISO3_Code==iso3)
  } else if (source=="GBD") {
    readRDS("tbGBDIncidenceAllAges.rds") %>%
      as_tibble() %>%
      filter(.$iso3==iso3) %>% 
      rename(
        ISO3_Code=iso3,
        Year=year,
        value=val,
        Disease=disease
      )
  } else {
    # force,on.exit,stop(stringParamErrorMessage, call.=F),match.arg,message,readline
    stop(glue('Invalid source {source} passed to getClinicalBurden'))
  }
}

getClinicalBurden <- function(iso3) {
  as_tibble(readRDS("models/inputs/tbDiseases.rds")) %>% 
    filter(ISO3_Code==iso3)
}
