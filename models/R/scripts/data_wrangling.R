#Data Wrangling
#Purpose: To read in and prepare datasets for input into model

#### Functions ####
#function to read in all sheets in a workbook
read_excel_allsheets <- function(filename, tibble = FALSE) {
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}

getMasterAgeFile <- function() {
  read_excel(here::here('models/inputs/Age_master.xlsx'))
}

getPopdata <- function(iso3="ZAF") {
  tbUnPop <- getUNPop() %>% 
    filter(ISO3_Code==iso3) %>%
    mutate(pop_age=as.numeric(pop_age))
  if (nrow(tbUnPop)==0) warning("Invalid iso3 code in getPopdata() call. Find ones in getUNPop().")
  
  getMasterAgeFile() %>%
    left_join(tbUnPop, by="pop_age") %>% 
    mutate(population=value*prop) %>%
    select(age, agecat, population)
}

# Download data which does not already exist locally
source(here::here('models/R/scripts/ensure_data_exists.R'))
# getUNPop()
source(here::here('models/R/scripts/process_population.R'))
# Ensure tbContacts.rds exists as contact_all and make prem.rebinned
source(here::here("models/R/scripts/process_contacts.R"))
