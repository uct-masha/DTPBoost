getModelParameters <- function() {
  parametersRaw <- suppressMessages(read_excel_allsheets("models/parameters/parametersDTP.xlsx"))
  # Diphtheria
  tbParmsD <- parametersRaw$param_D %>% select(Name, modelValue) %>% drop_na() %>% mutate(modelValue=ifelse(str_detect(modelValue,"TBD"),1,
                                                                                                            suppressWarnings(as.numeric(modelValue))))
  if (any(is.na(tbParmsD$modelValue))) warning(paste0("Some parameter values were NA: ",tbParmsD %>% filter(is.na(modelValue)) %>% pull(Name) %>% paste0(collapse=', ')))
  parametersD <- tbParmsD %>% drop_na %>% transmute(Name=Name,Value=modelValue,Disease="Diphtheria")
  
  # Tetanus
  tbParmsT <- parametersRaw$param_T %>% select(Name, modelValue) %>% drop_na() %>% mutate(modelValue=ifelse(str_detect(modelValue,"TBD"),1,
                                                                                                            suppressWarnings(as.numeric(modelValue))))
  if (any(is.na(tbParmsT$modelValue))) warning(paste0("Some parameter values were NA: ",tbParmsT %>% filter(is.na(modelValue)) %>% pull(Name) %>% paste0(collapse=', ')))
  parametersT <- tbParmsT %>% drop_na %>% transmute(Name=Name,Value=modelValue,Disease="Tetanus")
  
  # Pertussis
  tbParmsP <- parametersRaw$param_P %>% select(Name, modelValue) %>% drop_na() %>% mutate(modelValue=ifelse(str_detect(modelValue,"TBD"),1,
                                                                                                            suppressWarnings(as.numeric(modelValue))))
  if (any(is.na(tbParmsP$modelValue))) warning(paste0("Some parameter values were NA: ",tbParmsP %>% filter(is.na(modelValue)) %>% pull(Name) %>% paste0(collapse=', ')))
  parametersP <- tbParmsP %>% drop_na %>% transmute(Name=Name,Value=modelValue,Disease="Pertussis")
  
  # Put them together into scalar and age-specific params
  com_param <- bind_rows(
    parametersD,
    parametersT,
    parametersP)
  
  ageParametersGeneral <- parametersRaw$AgeRates %>% as_tibble() %>% rowid_to_column('age') %>% select(age, agecat=`age group`, agerate)
  ageParametersD <- parametersRaw$age_ref  # age_ref means NO age params but can easily be replaced
  ageParametersT <- parametersRaw$age_ref
  ageParametersP <- parametersRaw$param_age_P %>% select(age,agecat,ends_with('_P'))
  age_param <- ageParametersGeneral %>% 
    left_join(ageParametersD, by=c('age','agecat'))%>% 
    left_join(ageParametersT, by=c('age','agecat'))%>% 
    left_join(ageParametersP, by=c('age','agecat'))
  
  # Put them in a single data structure
  parameters <- com_param %>%
    pull(Value,name=Name) %>% 
    c(age_param %>% select(!c(age, agecat)) %>% as.list())
  
  # Return the result
  parameters
}
