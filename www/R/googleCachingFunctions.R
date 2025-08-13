makeSummaryTibble <- function(input, coverage_data, timesteps=c(2010,2020), oneRow=F) {
  # Uses input$selectedCountry for population and ptrans values
  # Assumes you have access to getShinyInputVaccineEfficacyRates()
  # timesteps is c(min,max), the last element is not included in this range.
  tbCov <- coverage_data %>%
    arrange(Year) %>%
    filter(Year %between% range(timesteps)) %>%
    mutate(across(.fns = ~ifelse(is.na(.x),0,.x)))
  
  tbEff <- getShinyInputVaccineEfficacyRates(input, baseline = T, startyear = first(timesteps), tyears = diff(range(timesteps))) %>%
    as.data.frame() %>%
    rownames_to_column('Year') %>%
    pivot_longer(!Year, names_sep = '_', names_to=c('eff','dose','disease')) %>%
    mutate(disease=c(D='Diphtheria', T='Tetanus', P='Pertussis')[disease]) %>%
    mutate(dose=paste0(eff,'_',dose) %>%
             str_replace('_4','_b') %>%
             str_replace('_5','_cb') %>%
             str_replace('_6','_ab')) %>%
    select(!eff) %>%
    pivot_wider(names_from=dose, values_from=value) %>%
    mutate(Year=as.integer(Year))
  
  tbCovEff = left_join(tbEff, tbCov, by='Year') %>%
    transmute(Year=Year, disease=disease,
              vps_weighted = eff_1*DTPCV1 + eff_2*DTPCV2 + eff_3*DTPCV3,
              vb_weighted = eff_b*InfantBooster + eff_cb*ChildBooster + eff_ab*AdolescentBooster,
              eff_m = if_else(is.na(eff_m), (eff_m1+eff_m2)/2, eff_m),
              vm_weighted = eff_m*(ANC)) %>%
    group_by(disease) %>%
    summarise(across(.fns=\(x)mean(x, na.rm=T)), .groups = 'drop') %>%
    select(!Year)
  
  # browser()  # vm_weighted is way to big!
  
  # popDat = readRDS(here('models/inputs/tbPop.rds')) %>% filter(ISO3_Code==first(input$selectedCountry)) %>%
  #   pull(value,name=pop_age)
  
  # tbData = read_rds(here("data/tbGBDIncidenceAllAges.rds")) %>% 
  #   filter(year %between% range(timesteps),
  #          iso3 == input$selectedCountry) %>%
  #   mutate(across(where(is.double),~.x*1e5/sum(popDat))) %>%  # per 100k pop
  #   group_by(disease) %>%
  #   summarise(dataLow = mean(lower),
  #             dataVal = mean(val),
  #             dataUpp = mean(upper))
  # 
  # result = inner_join(tbData, tbCovEff, by = join_by(disease))
  
  result = tbCovEff %>%
    mutate(ptrans=case_match(disease,
                             'Diphtheria' ~ input$ptrans_D,
                             'Tetanus' ~ input$ptrans_T,
                             'Pertussis' ~ input$ptrans_P,
                             .default = NA
    ))
  
  # Here we spread everything to a single row if needed
  if(oneRow==TRUE) {
    result <- result %>% pivot_wider(names_from = disease, values_from = !disease, names_sep = '.')
  }
  
  # Here we attach disease-invariant parameters
  # result <- result  %>%
  #   mutate(
  #     propYoung = popDat[['0-4']]/sum(popDat),
  #     logPop = log10(sum(popDat))
  #   )

  result = result %>%
    mutate(iso3=input$selectedCountry) %>%
    select(iso3,disease,vps_weighted,vb_weighted,vm_weighted,ptrans)
  
  return(result)
}

addToSheetsIfUnique <- function(tbSummaryForSheets, ss=NULL) {
  if (is.null(ss)) {
    warning('ss is not allowed to be NULL')
    return(FALSE)
  }
  # read content of sheet to determine if current data is unique - i.e. not already captured
  currentSheet <- read_sheet(ss, range = "Calibration")
  
  # currentSheet %>%   bind_rows(tbSummaryForSheets)
  # probOfMakingDistinct=0.1
  # if (runif(1) <= probOfMakingDistinct) {
  #   
  # }
  tbRowsToAdd <- anti_join(tbSummaryForSheets, currentSheet)
  if (nrow(tbRowsToAdd) == 0) {
    warning('The summary version of these parameters were already recorded.')
    return(FALSE)
  }
  # Check if making distinct will be a good idea
  currentNrow = nrow(currentSheet)
  distinctNrow = nrow(distinct(currentSheet))
  if (distinctNrow < 0.1*currentNrow) {
    LOG("Replacing the google sheet!")
    sheet_write(data = distinct(currentSheet),
                ss = ss)
  } else {
    # Otherwise append
    LOG("Rows to add to google sheets: {nrow(tbRowsToAdd)}.")
    sheet_append(ss = ss,
                 data = as.data.frame(tbRowsToAdd)) 
  }
  return(TRUE)
}
