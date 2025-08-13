ageMapping <- getPopData(iso3='ZAF') %>%
  transmute(age_group, ageID=as.integer(age_group)) %>%
  mutate(age_parts = age_group %>% str_match('(\\d+)-(\\d+)([^s]+)'),
         age1 = paste(age_parts[,2],age_parts[,4],sep=' '),
         age2 = paste(age_parts[,3],age_parts[,4],sep=' ')) %>% 
  mutate(age_parts=age_group %>% str_replace('\\+','yrs') %>% str_match('(\\d+)([^s-]+)'),
         age1 = ifelse(str_detect(age1,'NA'),paste(age_parts[,2],age_parts[,3],sep=' '),age1),
         age2 = ifelse(str_detect(age2,'NA'),NA,age2)) %>% 
  pivot_longer(c(age1,age2)) %>%
  select(ageID, age_group, value) %>%
  drop_na() %>% 
  mutate(value=ifelse(value=='6 mth', '26 wk', value))

ageId <- function(inputText) {
  ageMapping %>% filter(value==inputText) %>% pull(ageID) %>% first
}


makePulse <- function(input,
                      boost_switch_lo, boost_switch_hi,
                      i_lo, i_hi) {
  inp = function(v_part, i_part) {
    input[[paste0(v_part, '_', i_part)]]
  }
  if (is.null(inp('vps', i_lo)) || as.integer(inp('vps', i_lo))==0) {
    return(NULL) # If we don't want to apply pulsing, return NULL to indicate so.
  }
  
  if((input[[boost_switch_lo]]==TRUE & input[[boost_switch_hi]]==TRUE) & (inp('vsy', i_lo)==inp('vsy', i_hi))) {
    LOG("makePulse has been asked for by the user")
    sandwich_lo = ageId(inp('va', i_lo))  # age index of lower intervention
    sandwich_hi = ageId(inp('va', i_hi))  # age index of higher intervention
    if (sandwich_lo <= sandwich_hi) {  # this should always happen
      LOG("There are age groups between the lower and upper intervention to fill with values")
      pulse_age_groups = seq(sandwich_lo, sandwich_hi, 1) %>% setdiff(c(sandwich_lo, sandwich_hi))
      if (length(pulse_age_groups) > 0) {
        LOG("There are {length(pulse_age_groups)} of them in fact")
        cov_pulse_relative = inp('vps', i_lo)
        return(list(pulse_age_groups = pulse_age_groups,
                    cov_pulse_relative = cov_pulse_relative))
      }
    }
  }
  return(NULL)
}

getShinyInputCoverage <- function(input,coverage_table,baseline,startyear,tyears) {
  result <- list()
  
  
  #### Construct the tibble without ages first ####
  #Behind
  tbCovBehind <- coverage_table %>% 
    filter(Year>=startyear) %>%
    mutate(Year=as.integer(Year)) %>% 
    arrange(Year)
  colnames(tbCovBehind) <- c('t_floor', 'cov1', 'cov2', 'cov3', 'covb', 'covcb', 'covab', 'covm', 'covm2')
  #Ahead
  aheadYears <- seq(max(coverage_table$Year)+1, startyear+tyears+2)
  vecCovAhead <- tail(tbCovBehind,3) %>%
    summarise(across(.fns=mean, na.rm=T), .groups = 'drop_last') %>%
    select(!t_floor) %>% 
    as.list()
  tbCovAhead <- cbind(tibble(t_floor=aheadYears), do.call(tibble,vecCovAhead))
  #Both
  tbCovBaseline <- bind_rows(tbCovBehind,tbCovAhead) %>%
    mutate(across(.fns=~replace_na(.x,0)))
  # if (baseline) return(tbCovBaseline)
  #Interventions
  clamp <- function(x, minval=0, maxval=1){pmax(pmin(x, maxval), minval)}
  mkLinearInterp <- function(cond,
                             currentTimes,  
                             baselineCov, interventionCov,
                             interventionStartTime, interventionScaleTime,
                             mids=T) {
    if(!cond) return(baselineCov)
    if (interventionScaleTime==0) return(ifelse(currentTimes<interventionStartTime,baselineCov,interventionCov))
    P = clamp((currentTimes-interventionStartTime+0.5*mids)/interventionScaleTime)
    cov <- P*interventionCov + (1-P)*baselineCov
    cov
  }
  
  LOG("Baseline={baseline}, switches: ib:{input$boost_infant_switch}, icb:{input$boost_child_switch}, iab:{input$boost_adolescent_switch}, im:{input$boost_maternal_switch}")
  
  tbCovIntervention <- tbCovBaseline %>% 
    mutate(
      covb = mkLinearInterp(!baseline&&input$boost_infant_switch,
                            currentTimes=t_floor, 
                            # `!baseline&&input$boost_infant_switch` is the condition `cond` argument, ensuring we put the
                            # baseline value in here if the condition is not met, making it revert to
                            # baseline coverage.
                            # otherwise, for time >= t_floor set the baseline for interpolation
                            # to the starting coverage the user asked for. This lets the interpolator
                            # go between this value and the interventionCov value over the period.
                            baselineCov=ifelse(!baseline&input$boost_infant_switch&t_floor>=input$vsy_ib,input$vic_ib/100,covb),
                            interventionCov=input$vtc_ib/100,
                            interventionStartTime=input$vsy_ib, interventionScaleTime=input$vytrt_ib),
      covcb = mkLinearInterp(!baseline&&input$boost_child_switch,
                             currentTimes=t_floor, 
                             baselineCov=ifelse(!baseline&input$boost_child_switch&t_floor>=input$vsy_icb,input$vic_icb/100,covcb),
                             interventionCov=input$vtc_icb/100,
                             interventionStartTime=input$vsy_icb, interventionScaleTime=input$vytrt_icb),
      covab = mkLinearInterp(!baseline&&input$boost_adolescent_switch,
                             currentTimes=t_floor, 
                             baselineCov=ifelse(!baseline&input$boost_adolescent_switch&t_floor>=input$vsy_iab,input$vic_iab/100,covab),
                             interventionCov=input$vtc_iab/100,
                             interventionStartTime=input$vsy_iab, interventionScaleTime=input$vytrt_iab),
      covm = mkLinearInterp(!baseline&&input$boost_maternal_switch,
                            currentTimes=t_floor, 
                            baselineCov=covm, interventionCov=input$vtc_im/100,
                            interventionStartTime=input$vsy_im, interventionScaleTime=input$vytrt_im),
      covm2 = mkLinearInterp(!baseline&&input$boost_maternal_switch,
                             currentTimes=t_floor, 
                             baselineCov=covm2, interventionCov=input$vtc_im2/100,
                             interventionStartTime=input$vsy_im, interventionScaleTime=input$vytrt_im)
    )
  #### Modify the matrix ####
  modelAges <- levels(ageMapping$age_group)
  ROWNAMES <- seq_along(modelAges)
  YearNames <- tbCovIntervention$t_floor %>% as.character()
  Years <- YearNames %>% as.integer()
  
  
  # Here we make the matrix which we will ultimately return.
  # Note we did not take into account maternal
  mat <- matrix(0, nrow=length(ROWNAMES), ncol=nrow(tbCovIntervention))
  colnames(mat) <- YearNames
  
  cov1yr <- matrix(0,nrow=length(ROWNAMES),ncol=nrow(tbCovIntervention))
  cov2yr <- matrix(0,nrow=length(ROWNAMES),ncol=nrow(tbCovIntervention))
  cov3yr <- matrix(0,nrow=length(ROWNAMES),ncol=nrow(tbCovIntervention))
  covbyr <- matrix(0,nrow=length(ROWNAMES),ncol=nrow(tbCovIntervention))
  covcbyr <- matrix(0,nrow=length(ROWNAMES),ncol=nrow(tbCovIntervention))
  covabyr <- matrix(0,nrow=length(ROWNAMES),ncol=nrow(tbCovIntervention))
  colnames(cov1yr) <- YearNames
  colnames(cov2yr) <- YearNames
  colnames(cov3yr) <- YearNames
  colnames(covbyr) <- YearNames
  colnames(covcbyr) <- YearNames
  colnames(covabyr) <- YearNames
  
  # Helper function for logging
  asC = function(x){
    paste0('c(',
           paste0(x,collapse=','),
           ')')
  }
  
  # Primary Series
  if (!('None' %in% c(input$va_p1, input$vv_ps))) {
    cov1yr[ageId(input$va_p1),] <- tbCovIntervention$cov1
    mat[ageId(input$va_p1),] <- tbCovIntervention$cov1
  }
  if (!('None' %in% c(input$va_p2, input$vv_ps))) {
    cov2yr[ageId(input$va_p2),] <- tbCovIntervention$cov2
    mat[ageId(input$va_p2),] <- tbCovIntervention$cov2
  }
  if (!('None' %in% c(input$va_p3, input$vv_ps))) {
    cov3yr[ageId(input$va_p3),] <- tbCovIntervention$cov3
    mat[ageId(input$va_p3),] <- tbCovIntervention$cov3
  }
  # Interventions
  # Infant Booster
  if (!('None' %in% c(input$va_b, input$vv_b))) {
    baselineYears <- seq(min(Years), min(input$vsy_ib-1, max(Years))) %>% as.character()
    baselineYears <- as.integer(baselineYears)-startyear+1
    baselineAgeGroups <- ageId(input$va_b)
    baselineCoverage <- tbCovIntervention$covb[baselineYears]
    covbyr[baselineAgeGroups, baselineYears] <- baselineCoverage
    mat[baselineAgeGroups, baselineYears] <- baselineCoverage
  }
  if (is.null(input$vsy_ib) || input$vsy_ib>max(Years)) {
    interventionYears<-c()
  } else {
    # The "intervention" years could contain baseline projected values (projected forward to intervention time)
    interventionYears <- seq(input$vsy_ib, max(Years)) %>% intersect(Years)
    interventionYears <- interventionYears-startyear+1
    interventionAgeGroups <- ageId(input$va_ib)
    interventionCoverage <- tbCovIntervention$covb[interventionYears]
    LOG("Applying covbyr[{asC(interventionAgeGroups)}), {asC(interventionYears)}] <- {asC(interventionCoverage)}")
    covbyr[interventionAgeGroups, interventionYears] <- interventionCoverage
    mat[interventionAgeGroups, interventionYears] <- interventionCoverage
    # Pulse
    # browser()
    pulse = makePulse(input,
                      boost_switch_lo = 'boost_infant_switch',
                      boost_switch_hi = 'boost_child_switch',
                      i_lo = 'ib', i_hi = 'icb')
    if (!is.null(pulse)) {
      LOG("Applying a pulse covbyr&mat")
      cov_pulse = pulse$cov_pulse_relative * first(interventionCoverage)
      pulse_age_groups = pulse$pulse_age_groups
      LOG("covbyr[c({paste0(pulse_age_groups,collapse=',')}), {first(interventionYears)}] <- {cov_pulse} = {pulse$cov_pulse_relative}*{first(interventionCoverage)}")
      covbyr[pulse_age_groups, first(interventionYears)] <- cov_pulse
      mat[pulse_age_groups , first(interventionYears)] <- cov_pulse
    }
  }
  
  # Child Booster
  if (!('None' %in% c(input$va_cb, input$vv_cb))) {
    baselineYears <- seq(min(Years), min(input$vsy_icb-1, max(Years))) %>% as.character()
    baselineYears <- as.integer(baselineYears)-startyear+1
    baselineAgeGroups <- ageId(input$va_cb)
    baselineCoverage <- tbCovIntervention$covcb[baselineYears]
    
    covcbyr[baselineAgeGroups, baselineYears] <- baselineCoverage
    mat[baselineAgeGroups, baselineYears] <- tbCovIntervention$covcb[baselineYears]
  }
  if (is.null(input$vsy_icb) || input$vsy_icb>max(Years)) {
    interventionYears<-c()
  } else {
    interventionYears <- seq(input$vsy_icb, max(Years)) %>% intersect(Years)
    interventionYears <- interventionYears-startyear+1
    interventionAgeGroups <- ageId(input$va_icb)
    interventionCoverage <- tbCovIntervention$covcb[interventionYears]
    LOG("Applying covcbyr[{asC(interventionAgeGroups)}), {asC(interventionYears)}] <- {asC(interventionCoverage)}")
    covcbyr[interventionAgeGroups, interventionYears] <- interventionCoverage
    mat[interventionAgeGroups, interventionYears] <- interventionCoverage
    # Pulse
    # browser()
    pulse <- makePulse(input,
                       boost_switch_lo = 'boost_child_switch',
                       boost_switch_hi = 'boost_adolescent_switch',
                       i_lo = 'icb', i_hi = 'iab')
    if (!is.null(pulse)) {
      LOG("Applying a pulse covcbyr&mat")
      cov_pulse = pulse$cov_pulse_relative * first(interventionCoverage)
      pulse_age_groups = pulse$pulse_age_groups
      LOG("covcbyr[c({paste0(pulse_age_groups,collapse=',')}), {first(interventionYears)}] <- {cov_pulse} = {pulse$cov_pulse_relative}*{first(interventionCoverage)}")
      covcbyr[pulse_age_groups, first(interventionYears)] <- cov_pulse
      mat[pulse_age_groups , first(interventionYears)] <- cov_pulse
    }
  }
  
  # Adolescent
  if (!('None' %in% c(input$va_ab, input$vv_ab))) {
    baselineYears <- seq(min(Years), min(input$vsy_iab-1, max(Years))) %>% as.character()
    baselineYears <- as.integer(baselineYears)-startyear+1
    baselineAgeGroups <- ageId(input$va_ab)
    baselineCoverage <- tbCovIntervention$covab[baselineYears]
    LOG("Applying covabyr[{asC(baselineAgeGroups)}), {asC(baselineYears)}] <- {asC(baselineCoverage)}")
    covabyr[baselineAgeGroups, baselineYears] <- baselineCoverage
    mat[baselineAgeGroups, baselineYears] <- tbCovIntervention$covab[baselineYears]
  }
  if (is.null(input$vsy_iab) || input$vsy_iab>max(Years) || !(input$boost_adolescent_switch)) {
    interventionYears<-c()
  } else {
    interventionYears <- seq(input$vsy_iab, max(Years)) %>% intersect(Years)
    interventionYears <- interventionYears-startyear+1
    interventionAgeGroups <- ageId(input$va_iab)
    interventionCoverage <- tbCovIntervention$covab[interventionYears]
    covabyr[interventionAgeGroups, interventionYears] <- interventionCoverage
    mat[interventionAgeGroups, interventionYears] <- interventionCoverage
  }
  
  result$cov1yr <- cov1yr
  result$cov2yr <- cov2yr
  result$cov3yr <- cov3yr

  result$covbyr <- covbyr
  result$covcbyr <- covcbyr
  result$covabyr <- covabyr
  
  # btw mat = sum(cov1yr:covabyr)
  
  result$cov <- mat
  
  mat <- matrix(0, nrow=length(ROWNAMES), ncol=nrow(tbCovIntervention))
  mat2 <- matrix(0, nrow=length(ROWNAMES), ncol=nrow(tbCovIntervention))
  colnames(mat) <- YearNames
  colnames(mat2) <- YearNames
  # MATERNAL (to be removed)
  
  maternalAgeIDs <- ageMapping %>%
    filter(between(ageID,
                   ageID[value=='15 yr'],
                   ageID[value=='49 yr'])) %>%
    pull(ageID) %>%
    unique()

  baselineYears <- seq(min(Years), min(input$vsy_im-1, max(Years))) %>% as.character()
  baselineYears <- as.integer(baselineYears)-startyear+1
  baselineAgeGroups <- maternalAgeIDs
  # vmba %in% c("None", "During pregnancy")
  # maternal_biag %in% c("Women of reproductive age (WRA)","Pregnant Woman","WRA & pregnant women")
  if (input$vv_m!='None') {
    for (bag in baselineAgeGroups) {
      mat[bag, ] <- tbCovIntervention$covm
      mat2[bag, ] <- tbCovIntervention$covm2
    }
  }
  if (is.null(input$vsy_im) || input$vsy_im>max(Years)) {
    interventionYears<-c()
  } else {
    interventionYears <- seq(input$vsy_im, max(Years)) %>% intersect(Years)
    interventionYears <- interventionYears-startyear+1
    interventionAgeGroups <- maternalAgeIDs
    if (!('None' %in% c(input$vv_im, input$maternal_biag))) {
      for (iag in interventionAgeGroups) {
        mat[iag, interventionYears] <- tbCovIntervention$covm[interventionYears]
        mat2[iag, interventionYears] <- tbCovIntervention$covm2[interventionYears]
      }
    }
  }
  
  result$covm <- mat
  result$covm2 <- mat2
  result$maternalAgeIDs <- maternalAgeIDs
  result
}
makeDosageSpecificRates <- function(input,
                                    tbDoseTypeMapping,
                                    baseline,
                                    startyear, tyears,
                                    sheetname='tb_eff',
                                    isEffnTau=F,
                                    extraYears=2) {
  movMapping <- tbDoseTypeMapping %>% pull(inputBaseline, name=colName)
  ageMapping <- tbDoseTypeMapping %>% pull(inputAge, name=colName)
  
  tb <- read_excel(here('models/parameters/parametersDTP.xlsx'), sheet=sheetname) %>%
    select(!Description)
  
  mat <- column_to_rownames(tb, 'Formulation') %>% as.matrix()
  
  result <- matrix(0,nrow=tyears+extraYears+1,ncol=ncol(mat))
  colnames(result)<-colnames(mat)
  rownames(result)<-startyear:(startyear+tyears+extraYears)
  
  for(colName in colnames(result)) {
    doseBaseline <- movMapping[[colName]]  # eg vv_ps
    ageBaseline <- ageMapping[[colName]]  # eg va_p1
    # If baseline use dosebaseline else use intervention value
    if (ageBaseline=="vmba") { ageCheck <- F }
    else {                     ageCheck <- input[[ageBaseline]]=="None" }
    shouldJustBeDefault <- input[[doseBaseline]]=="None" || ageCheck
    # The default tau value should be the same as DTaP, as of 02/04/2023
    # otherwise it's 0 for other functions making use of makeDosageSpecificRates()
    defaultValue = if(str_starts(colName,'tau_')){mat["DTaP", colName]}else{0}
    colVal <- ifelse(shouldJustBeDefault,
                     defaultValue,
                     mat[input[[doseBaseline]], colName])
    result[,colName] <- colVal
  }
  
  addIntervention <- function(result, int_switch, int_startyear, int_input_src, int_input_dst, isEffnTau) {
    if (int_switch==TRUE) {
      yearsActive <- rownames(result)[as.integer(rownames(result)) >= int_startyear]
      interventionColumns <- names(movMapping)[movMapping==int_input_src]
      for(yearActive in yearsActive) {
        # If isEffnTau and baseline is nonzero then don't ramp down to zero (ie don't just lose immunity)
        if (isEffnTau){
          result[yearActive, interventionColumns] <- ifelse(mat[input[[int_input_dst]], interventionColumns]==0,result[yearActive, interventionColumns],mat[input[[int_input_dst]], interventionColumns])
        } else {
          result[yearActive, interventionColumns] <- mat[input[[int_input_dst]], interventionColumns]
        }
      }
    }
    result
  }
  
  if (baseline==FALSE) {
    result <- result %>% 
      addIntervention(int_switch=input$boost_infant_switch,
                      int_startyear=input$vsy_ib,
                      int_input_src='vv_b',
                      int_input_dst='vv_ib', isEffnTau=isEffnTau) %>% 
      addIntervention(int_switch=input$boost_child_switch,
                      int_startyear=input$vsy_icb,
                      int_input_src='vv_cb',
                      int_input_dst='vv_icb', isEffnTau=isEffnTau) %>% 
      addIntervention(int_switch=input$boost_adolescent_switch,
                      int_startyear=input$vsy_iab,
                      int_input_src='vv_ab',
                      int_input_dst='vv_iab', isEffnTau=isEffnTau) %>% 
      addIntervention(int_switch=input$boost_maternal_switch,
                      int_startyear=input$vsy_im,
                      int_input_src='vv_m',
                      int_input_dst='vv_im', isEffnTau=isEffnTau)
  }
  result
}
matrixToNamedList <- function(mat) {
  sapply(colnames(mat), function(nam){mat[,nam]}, simplify = F)
}
getShinyInputVaccineEfficacyRates <- function(input, baseline, startyear,tyears) {
  effMapping <- tibble::tribble(
    # Map efficacy column to vaccine input (primary series, infant/child/adolescent booster, maternal)
    ~colName,   ~inputBaseline,  ~inputAge,
    "eff_1_T",        "vv_ps",   'va_p1',
    "eff_2_T",        "vv_ps",   'va_p2',
    "eff_3_T",        "vv_ps",   'va_p3',
    "eff_4_T",           "vv_b",   'va_b',
    "eff_5_T",          "vv_cb",   'va_cb',
    "eff_6_T",          "vv_ab",   'va_ab',
    "eff_m1_T",         "vv_m",   'vmba', 
    "eff_m2_T",         "vv_m",   'vmba',
    "eff_1_D",        "vv_ps",   'va_p1',
    "eff_2_D",        "vv_ps",   'va_p2',
    "eff_3_D",        "vv_ps",   'va_p3',
    "eff_b_D",           "vv_b",   'va_b',
    "eff_cb_D",         "vv_cb",   'va_cb',
    "eff_ab_D",         "vv_ab",   'va_ab',
    "eff_m_D",          "vv_m",   'vmba',
    "eff_1_P",        "vv_ps",   'va_p1',
    "eff_2_P",        "vv_ps",   'va_p2',
    "eff_3_P",        "vv_ps",   'va_p3',
    "eff_b_P",           "vv_b",   'va_b',
    "eff_cb_P",         "vv_cb",   'va_cb',
    "eff_ab_P",         "vv_ab",   'va_ab',
    "eff_m_P",          "vv_m",   'vmba',
    "eff_w_P",        "vv_ps",   'va_p1'
  )
  
  makeDosageSpecificRates(input=input,
                          tbDoseTypeMapping=effMapping,
                          startyear=startyear,tyears=tyears,
                          baseline=baseline,
                          sheetname = 'tb_eff',
                          isEffnTau=F)
}
getShinyInputVaccineTaus <- function(input,baseline,startyear,tyears) {
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
  
  makeDosageSpecificRates(input=input,
                          tbDoseTypeMapping=tauMapping,
                          baseline=baseline,
                          startyear=startyear,tyears=tyears,
                          sheetname = 'tb_tau',
                          isEffnTau=T)
}
getShinyInputVaccineMovementVectors <- function(input,baseline,startyear,tyears) {
  print("VMR HI")
  movMappingBase <- tibble::tribble(
    ~colName,  ~inputBaseline,  ~inputAge,
    "mov_1_T",       "vv_ps",   'va_p1',
    "mov_2_T",       "vv_ps",   'va_p2',
    "mov_3_T",       "vv_ps",   'va_p3',
    "mov_4_T",       "vv_b",    'va_b',
    "mov_5_T",       "vv_cb",   'va_cb',
    "mov_6_T",       "vv_ab",   'va_ab',
    "mov_m1_T",      "vv_m",    'vmba',
    "mov_m2_T",      "vv_m",    'vmba',
    "mov_1_D",       "vv_ps",   'va_p1',
    "mov_2_D",       "vv_ps",   'va_p2',
    "mov_3_D",       "vv_ps",   'va_p3',
    "mov_b_D",       "vv_b",    'va_b',
    "mov_cb_D",      "vv_cb",   'va_cb',
    "mov_ab_D",      "vv_ab",   'va_ab',
    "mov_m_D",       "vv_m",    'vmba',
    "mov_1_P",       "vv_ps",   'va_p1',
    "mov_2_P",       "vv_ps",   'va_p2',
    "mov_3_P",       "vv_ps",   'va_p3',
    "mov_b_P",       "vv_b",    'va_b',
    "mov_cb_P",      "vv_cb",   'va_cb',
    "mov_ab_P",      "vv_ab",   'va_ab',
    "mov_m_P",       "vv_m",    'vmba'
  )
  
  mvMat <- makeDosageSpecificRates(input=input,
                                   tbDoseTypeMapping=movMappingBase,
                                   startyear=startyear,tyears=tyears,
                                   baseline=baseline,
                                   isEffnTau=F,
                                   sheetname = 'tb_mov')
  mv <- matrixToNamedList(mvMat)
  # Now expand to age groups
  AGES <- getPopData(iso3='ZAF')$age_group
  YEARS <- seq(startyear, startyear+tyears+2)
  nAges <- length(AGES)
  nYears <- length(YEARS)
  matD <- matrix(0, nrow=nAges, ncol=nYears)
  matT <- matrix(0, nrow=nAges, ncol=nYears)
  matP <- matrix(0, nrow=nAges, ncol=nYears)
  ageMapping <- getPopData(iso3='ZAF') %>%
    transmute(age_group, ageID=as.integer(age_group)) %>%
    mutate(age_parts = age_group %>% str_match('(\\d+)-(\\d+)([^s]+)'),
           age1 = paste(age_parts[,2],age_parts[,4],sep=' '),
           age2 = paste(age_parts[,3],age_parts[,4],sep=' ')) %>% 
    mutate(age_parts=age_group %>% str_replace('\\+','yrs') %>% str_match('(\\d+)([^s-]+)'),
           age1 = ifelse(str_detect(age1,'NA'),paste(age_parts[,2],age_parts[,3],sep=' '),age1),
           age2 = ifelse(str_detect(age2,'NA'),NA,age2)) %>% 
    pivot_longer(c(age1,age2)) %>%
    select(ageID, age_group, value) %>%
    drop_na() %>% 
    mutate(value=ifelse(value=='6 mth', '26 wk', value))
  ageId <- function(inputText) {
    ageMapping %>% 
      filter(value==inputText) %>% 
      pull(ageID) %>% 
      first()
  }
  # Primary Series
  if (!('None' %in% c(input$va_p1, input$vv_ps))) {
    matD[ageId(input$va_p1),] <- mv$mov_1_D
    matT[ageId(input$va_p1),] <- mv$mov_1_T
    matP[ageId(input$va_p1),] <- mv$mov_1_P
  }
  
  if (!('None' %in% c(input$va_p2, input$vv_ps))) {
    matD[ageId(input$va_p2),] <- mv$mov_2_D
    matT[ageId(input$va_p2),] <- mv$mov_2_T
    matP[ageId(input$va_p2),] <- mv$mov_2_P
  }
  
  if (!('None' %in% c(input$va_p3, input$vv_ps))) {
    matD[ageId(input$va_p3),] <- mv$mov_3_D
    matT[ageId(input$va_p3),] <- mv$mov_3_T
    matP[ageId(input$va_p3),] <- mv$mov_3_P
  }
  # Baseline Boosters
  if (!('None' %in% c(input$va_b, input$vv_b))) {
    matD[ageId(input$va_b),] <- mv$mov_b_D
    matT[ageId(input$va_b),] <- mv$mov_4_T
    matP[ageId(input$va_b),] <- mv$mov_b_P
  }
  
  if (!('None' %in% c(input$va_cb, input$vv_cb))) {
    matD[ageId(input$va_cb),] <- mv$mov_cb_D
    matT[ageId(input$va_cb),] <- mv$mov_5_T
    matP[ageId(input$va_cb),] <- mv$mov_cb_P
  }
  
  if (!('None' %in% c(input$va_ab, input$vv_ab))) {
    matD[ageId(input$va_ab),] <- mv$mov_ab_D
    matT[ageId(input$va_ab),] <- mv$mov_6_T
    matP[ageId(input$va_ab),] <- mv$mov_ab_P
  }
  # Intervention Boosters
  # Infant
  switchInfant <- !is.null(input$boost_infant_switch) && input$boost_infant_switch
  switchChild <- !is.null(input$boost_child_switch) && input$boost_child_switch
  switchAdolescent <- !is.null(input$boost_adolescent_switch) && input$boost_adolescent_switch
  switchMaternal <- !is.null(input$boost_maternal_switch) && input$boost_maternal_switch
  intermediateValues <- function(lo, hi){
    seq(lo, hi, 1) %>%
      setdiff(c(lo, hi))
  }
  if (switchInfant) {
    if(input$vsy_ib %in% YEARS) {
      interventionYears <- seq(input$vsy_ib, last(YEARS))
      baselineYears <- setdiff(YEARS, interventionYears)
      iAgeIntervention <- ageId(input$va_ib)
      iAgeBaseline <- ageId(input$va_b)
      iYearIntervention <- interventionYears-startyear+1
      iYearBaseline <- baselineYears-startyear+1
      # populate baseline values
      matD[iAgeBaseline,iYearBaseline] <- mv$mov_b_D[iYearBaseline]
      matT[iAgeBaseline,iYearBaseline] <- mv$mov_4_T[iYearBaseline]
      matP[iAgeBaseline,iYearBaseline] <- mv$mov_b_P[iYearBaseline]
      # populate intervention values
      matD[iAgeIntervention,iYearIntervention] <- mv$mov_b_D[iYearIntervention]
      matT[iAgeIntervention,iYearIntervention] <- mv$mov_4_T[iYearIntervention]
      matP[iAgeIntervention,iYearIntervention] <- mv$mov_b_P[iYearIntervention]
    }
  }
  # Child
  if (switchChild) {
    if(input$vsy_icb %in% YEARS) {
      interventionYears <- seq(input$vsy_icb, last(YEARS))
      baselineYears <- setdiff(YEARS, interventionYears)
      iAgeIntervention <- ageId(input$va_icb)
      iAgeBaseline <- ageId(input$va_cb)
      iYearIntervention <- interventionYears-startyear+1
      iYearBaseline <- baselineYears-startyear+1
      # populate baseline values
      matD[iAgeBaseline,iYearBaseline] <- mv$mov_cb_D[iYearBaseline]
      matT[iAgeBaseline,iYearBaseline] <- mv$mov_5_T[iYearBaseline]
      matP[iAgeBaseline,iYearBaseline] <- mv$mov_cb_P[iYearBaseline]
      # populate intervention values
      matD[iAgeIntervention,iYearIntervention] <- mv$mov_cb_D[iYearIntervention]
      matT[iAgeIntervention,iYearIntervention] <- mv$mov_5_T[iYearIntervention]
      matP[iAgeIntervention,iYearIntervention] <- mv$mov_cb_P[iYearIntervention]
      # Pulsing
      if (switchInfant & (input$vsy_ib==input$vsy_icb)) {
        iAgeIntervention = intermediateValues(ageId(input$va_ib), ageId(input$va_icb))
        iYearIntervention = first(iYearIntervention)
        matD[iAgeIntervention,iYearIntervention] <- mv$mov_cb_D[iYearIntervention]
        matT[iAgeIntervention,iYearIntervention] <- mv$mov_5_T[iYearIntervention]
        matP[iAgeIntervention,iYearIntervention] <- mv$mov_cb_P[iYearIntervention]
      }
    }
  }
  # Adolescent
  if (switchAdolescent) {
    if(input$vsy_iab %in% YEARS) {
      interventionYears <- seq(input$vsy_iab, last(YEARS))
      baselineYears <- setdiff(YEARS, interventionYears)
      iAgeIntervention <- ageId(input$va_iab)
      iAgeBaseline <- ageId(input$va_ab)
      iYearIntervention <- interventionYears-startyear+1
      iYearBaseline <- baselineYears-startyear+1
      # populate baseline values
      matD[iAgeBaseline,iYearBaseline] <- mv$mov_ab_D[iYearBaseline]
      matT[iAgeBaseline,iYearBaseline] <- mv$mov_6_T[iYearBaseline]
      matP[iAgeBaseline,iYearBaseline] <- mv$mov_ab_P[iYearBaseline]
      # populate intervention values
      matD[iAgeIntervention,iYearIntervention] <- mv$mov_ab_D[iYearIntervention]
      matT[iAgeIntervention,iYearIntervention] <- mv$mov_6_T[iYearIntervention]
      matP[iAgeIntervention,iYearIntervention] <- mv$mov_ab_P[iYearIntervention]
      # Pulsing
      if (switchChild & (input$vsy_icb==input$vsy_iab)) {
        iAgeIntervention = intermediateValues(ageId(input$va_icb), ageId(input$va_iab))
        iYearIntervention = first(iYearIntervention)
        matD[iAgeIntervention,iYearIntervention] <- mv$mov_ab_D[iYearIntervention]
        matT[iAgeIntervention,iYearIntervention] <- mv$mov_6_T[iYearIntervention]
        matP[iAgeIntervention,iYearIntervention] <- mv$mov_ab_P[iYearIntervention]
      }
    }
  }
  # Finish it up
  colnames(matD) = as.character(YEARS)
  colnames(matT) = as.character(YEARS)
  colnames(matP) = as.character(YEARS)
  mov_D <- matD
  mov_T <- matT
  mov_P <- matP

    # Maternal
  matD <- matrix(0, nrow=nAges, ncol=nYears)
  matT <- matrix(0, nrow=nAges, ncol=nYears)
  matP <- matrix(0, nrow=nAges, ncol=nYears)

  maternalAgeIDs <- ageMapping %>%
    filter(between(ageID,
                   ageID[value=='15 yr'],
                   ageID[value=='49 yr'])) %>%
    pull(ageID) %>%
    unique()
  
  
  if (!all.equal(mv$mov_m1_D,mv$mov_m2_D)) warning("Assumption broke: mov_m1_D != mov_m2_D")
  if (baseline || !switchMaternal) interventionYears <- numeric(0)
  else interventionYears <- seq(input$vsy_im, last(YEARS))
  baselineYears <- setdiff(YEARS, interventionYears)
  iYearIntervention <- interventionYears-startyear+1
  iYearBaseline <- baselineYears-startyear+1
  for (iAge in maternalAgeIDs) {
    matD[iAge, iYearBaseline] <- mv$mov_m_D[iYearBaseline]
    matT[iAge, iYearBaseline] <- mv$mov_m1_T[iYearBaseline]
    matP[iAge, iYearBaseline] <- mv$mov_m_P[iYearBaseline]
    if (switchMaternal && input$vv_im!='None') {
      matD[iAge, iYearIntervention] <- mv$mov_m_D[iYearIntervention]
      matT[iAge, iYearIntervention] <- mv$mov_m1_T[iYearIntervention]
      matP[iAge, iYearIntervention] <- mv$mov_m_P[iYearIntervention]
    }
    
  }
  # Finish it up
  colnames(matD) = as.character(YEARS)
  colnames(matT) = as.character(YEARS)
  colnames(matP) = as.character(YEARS)
  movm_D <- matD
  movm_T <- matT
  movm_P <- matP
  return(list(
    mov_D=mov_D,
    mov_T=mov_T,
    mov_P=mov_P,
    movm_D=movm_D,
    movm_T=movm_T,
    movm_P=movm_P))
}

getShinyInputVaccineAges <- function(input,startyear) {
  ageMapping <- getPopData(iso3='ZAF') %>%
    transmute(age_group, ageID=as.integer(age_group)) %>%
    mutate(age_parts = age_group %>% str_match('(\\d+)-(\\d+)([^s]+)'),
           age1 = paste(age_parts[,2],age_parts[,4],sep=' '),
           age2 = paste(age_parts[,3],age_parts[,4],sep=' ')) %>% 
    mutate(age_parts=age_group %>% str_replace('\\+','yrs') %>% str_match('(\\d+)([^s-]+)'),
           age1 = ifelse(str_detect(age1,'NA'),paste(age_parts[,2],age_parts[,3],sep=' '),age1),
           age2 = ifelse(str_detect(age2,'NA'),NA,age2)) %>% 
    pivot_longer(c(age1,age2)) %>%
    select(ageID, age_group, value) %>%
    drop_na() %>% 
    mutate(value=ifelse(value=='6 mth', '26 wk', value))
  
  inputNames <- paste0('va_p',1:3)#strsplit('va_p1,va_p2,va_p3',',')[[1]]
  tibble(name=inputNames) %>%
    rowwise() %>%
    mutate(value=input[[name]]) %>% 
    left_join(ageMapping, by='value') %>% 
    select(name, ageID) %>% 
    mutate(value=1) %>% 
    pivot_wider(names_from=name, values_from=value) %>% 
    filter(!is.na(ageID)) %>% 
    complete(ageID=1:56) %>%
    mutate(across(.fns = function(x){ifelse(is.na(x),0,x)})) %>% 
    #infant:12:23mth
    #child:4:7yr
    #adol:9:15yr
    mutate(va_b = ifelse(ageID %in% 18:29,1,0),
           va_cb = ifelse(ageID %in% 32:35,1,0),
           va_ab = ifelse(ageID %in% 37:43,1,0))
}

getShinyInputPopData <- function(input) {
  getPopData(input$selectedCountry) %>%
    #age_group  popTot mortTot mortRate birthRate femProp fertProp fiveYearCat
    select(popTot,
           deathprop=mortRate,
           births=birthRate,
           femProp,
           fertProp)
}

getShinyInputHealthSystemData <- function(input) {
  sapply(
    c("p_mt_P",
      "p_st_P",
      "p_mt_D",
      "p_st_D",
      "p_mt_T",
      "p_st_T", 
      "p_rep_clin_P",  # TODO: Check this, as it was repClin before
      "p_rep_death_P",
      "p_rep_clin_D",
      "p_rep_death_D", 
      "p_rep_clin_T",
      "p_rep_death_T"),
    function(x) {
      input[[x]]/100
    }
  )
}

getShinyInputCosts <- function(input) {
  acronyms_base <- c('ps','b','cb','ab','m')
  acronyms_interv <- c('ib','icb','iab','im')
  acronyms_allboost <- c(acronyms_base, acronyms_interv)
  acronyms_diseases <- c('D','T','P')
  dels <- expand.grid(A='del',B=c('_hf_','_o_'),C=acronyms_allboost) %>%
    arrange(B) %>%
    mutate(Z=paste0(A,B,C)) %>%
    pull(Z)
  del_cpds <- expand.grid(A='del_cpd',B=c('_hf_','_o_'),C=acronyms_allboost) %>%
    arrange(B) %>%
    mutate(Z=paste0(A,B,C)) %>%
    pull(Z)
  units <- paste0('unit_cpd_',acronyms_allboost)
  outps <- paste0("outp_cost_", acronyms_diseases)
  inps <- paste0("inp_cost_", acronyms_diseases)
  unit_pfins <- paste0("unit_pfin_", acronyms_allboost)
  del_pfins <- paste0("del_pfin_", acronyms_allboost)
  # strat_intro_cost
  others <- c("strat_intro_cost", "strat_intro_applyonce")
  others100 <- c("strat_intro_cost_pfin")
  costsInputIds <- c(units, outps, inps, others)
  costsInput100Ids <- c(dels, del_cpds, del_pfins, unit_pfins, others100)
  result <- sapply(
    costsInputIds,
    function(x) {
      as.numeric(input[[x]])
    }
  )
  result100 <- sapply(
    costsInput100Ids,
    function(x) {
      # hospital facility and outreach percentages should add up to 1
      # in the app we let the user choose the hospital percentage
      # here we calculate the outreach percentage
      if (str_detect(x, '_o_')) {
        num <- 100-as.numeric(input[[str_replace(x,'_o_','_hf_')]])
      } else {
        num <- as.numeric(input[[x]])
      }
      num/100
    }
  )
  numericResult <- c(result, result100)
  
  append(list(selectedCurrency=input$selectedCurrency), numericResult)
  # To include the interventions we'd need to look at, eg, infant_booster.R costs
  # eg input$infant_bicpd
}

getShinyInputContacts <- function(input) {
  lsContactsAll <- readRDS(glue::glue("models/inputs/lsContactsAll.rds"))
  
  contact <- lsContactsAll[[input$selectedCountry]]
  contact
}

getShinyInputANC <- function(input) {
  c(covANC=read.csv('anc.csv') %>% filter(iso3==input$selectedCountry) %>% pull(covANC))
}

getShinyInputCalibrationParameters <- function(input) {
  c(ptrans_D = input$ptrans_D/100,
    ptrans_T = input$ptrans_T/100,
    ptrans_P = input$ptrans_P/100)
}

getShinyInputWhenDose <- function(input, baseline, startyear, tyears) {
  # Helper variables
  YEARNAMES <- as.character(startyear+seq(0,tyears+2))
  lastYear <- last(YEARNAMES)
  ones = rep_along(YEARNAMES, 1) %>% setNames(YEARNAMES)
  zeros =rep_along(YEARNAMES, 0) %>% setNames(YEARNAMES)
  # Helper functions
  notNone <- function(...){all(as.logical(map(list(...),~!is.null(.x) && .x!="None")))}
  isTrue <- function(...){ all(as.logical(map(list(...),~!is.null(.x) && .x==TRUE)))}
  # Defaults
  whenDose_1 = zeros
  whenDose_2 = zeros
  whenDose_3 = zeros
  whenDose_b = zeros
  whenDose_cb = zeros
  whenDose_ab = zeros
  whenDose_mb = zeros
  whenDose_ib = zeros
  whenDose_icb = zeros
  whenDose_iab = zeros
  whenDose_imb = zeros
  has_1 <- notNone(input$va_p1, input$vv_ps)
  has_2 <- notNone(input$va_p2, input$vv_ps)
  has_3 <- notNone(input$va_p3, input$vv_ps)
  has_b <- notNone(input$va_b, input$vv_b)
  has_cb <-notNone(input$va_cb, input$vv_cb)
  has_ab <-notNone(input$va_ab, input$vv_ab)
  has_mb <-notNone(input$vv_m)
  has_ib <- isTrue(!baseline, input$boost_infant_switch)
  has_icb <-isTrue(!baseline, input$boost_child_switch)
  has_iab <-isTrue(!baseline, input$boost_adolescent_switch)
  has_imb <-isTrue(!baseline, input$boost_maternal_switch)
  if (has_1) whenDose_1 <- ones
  if (has_2) whenDose_2 <- ones
  if (has_3) whenDose_3 <- ones
  
  yrs <- function(startYr,endYr) {
    which(as.integer(YEARNAMES) %>% between(as.integer(startYr),as.integer(endYr)))
  }
  if(has_ib ) whenDose_ib[yrs(input$vsy_ib,lastYear)] <- 1
  if(has_icb) whenDose_icb[yrs(input$vsy_icb,lastYear)] <- 1
  if(has_iab) whenDose_iab[yrs(input$vsy_iab,lastYear)] <- 1
  if(has_imb) whenDose_imb[yrs(input$vsy_im,lastYear)] <- 1
  
  if (has_b ){whenDose_b  <- 1 - whenDose_ib}
  if (has_cb){whenDose_cb <- 1 - whenDose_icb}
  if (has_ab){whenDose_ab <- 1 - whenDose_iab}
  if (has_mb){whenDose_mb <- 1 - whenDose_imb}
  list(
    whenDose_1  =whenDose_1,
    whenDose_2  =whenDose_2,
    whenDose_3  =whenDose_3,
    whenDose_b  =whenDose_b,
    whenDose_cb =whenDose_cb,
    whenDose_ab =whenDose_ab,
    whenDose_mb =whenDose_mb,
    whenDose_ib =whenDose_ib,
    whenDose_icb=whenDose_icb,
    whenDose_iab=whenDose_iab,
    whenDose_imb=whenDose_imb
  )
}

TS <- function() {
  strftime(Sys.time(),'%Y%m%d_%H%M%S')
}

getShinyInputsDebugPath = function(input) {
  # Calculate baseline part
  inputNamesBaselineBoosterAges = c(infant='va_b',
                                    child='va_cb',
                                    adolescent='va_ab',
                                    maternal='vv_m')
  baselinePart = Filter(x = inputNamesBaselineBoosterAges, function(.x) {
    input[[.x]] != 'None'
  }) %>% names() %>% paste0(collapse='')
  if (baselinePart=="") baselinePart='default'
  
  # Calculate interventions part
  inputNamesInterventionBooster = c(infant='boost_infant_switch',
                                    child='boost_child_switch',
                                    adolescent='boost_adolescent_switch',
                                    maternal='boost_maternal_switch')
  interventionPart = Filter(x=inputNamesInterventionBooster, function(.x) {
    input[[.x]]
  }) %>% names() %>% paste0(collapse='')
  if (interventionPart=="") interventionPart='none'
  
  dir_shinyInputs = paste0("getShinyInputsObjects_", baselinePart, '_', interventionPart)
  dir_shinyInputs
}

getShinyInputs <- function(input, coverage_table, baseline=FALSE, startyear, tyears, saveForDebugging=F) {
  LOG("Calculating Shiny inputs now...")
  TIME <- TS()
  LOG("Making object with baseline={baseline} at time {TIME}")
  
  if (saveForDebugging) {
    # Save object logic for debugging:
    dir_shinyInputs = getShinyInputsDebugPath(input)
    
    if(!fs::dir_exists(dir_shinyInputs)) {fs::dir_create(dir_shinyInputs)}
    saveRDS(reactiveValuesToList(input), fs::path_join(c(dir_shinyInputs,'input.rds')))
    saveRDS(coverage_table, fs::path_join(c(dir_shinyInputs,'coverage_table.rds')))
    saveRDS(startyear, fs::path_join(c(dir_shinyInputs,'startyear.rds')))
    saveRDS(tyears, fs::path_join(c(dir_shinyInputs,'tyears.rds')))
  }
  
  shiny_parms <- c(getShinyInputVaccineEfficacyRates(input, baseline, startyear, tyears) %>% matrixToNamedList(),
                   getShinyInputVaccineTaus(input, baseline, startyear, tyears) %>% matrixToNamedList(),
                   getShinyInputVaccineMovementVectors(input, baseline, startyear, tyears),
                   getShinyInputPopData(input),
                   getShinyInputHealthSystemData(input),
                   getShinyInputCosts(input),
                   getShinyInputANC(input),
                   getShinyInputWhenDose(input, baseline, startyear, tyears), 
                   getShinyInputCalibrationParameters(input),
                   getShinyInputCoverage(input, coverage_table=coverage_table, baseline=baseline, startyear=startyear, tyears=tyears),
                   list(
                     contact = getShinyInputContacts(input),
                     vaxAges = getShinyInputVaccineAges(input, startyear)
                   )
  )
  LOG("Shiny inputs calculation complete. Have a nice day!")
  shiny_parms
  # add shiny_parms to base_parameters
}
