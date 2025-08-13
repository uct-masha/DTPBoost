# Model developed by MASHA at UCT
# https:://masha.uct.ac.za
# Disease: Diphtheria
modelLetter <- 'D'
modelFilename <- glue::glue('cache/big{modelLetter}.xlsx')

#### Ensure bigX exists #### 
# mt_updateBigX(modelLetter=modelLetter,
#               overwrite = T)
# To update shinyInputs: mt_updateShinyInputs
# To update parameter sheet: mt_updateParameters(overwrite=T)

#### Read mtMod_D in ####
mtMod_D <- mt_getTibblesInList(modelFilename=modelFilename)

lsModel_D <- mtMod_D$lsModel

tbCompartments_D <- mtMod_D$tbCompartments
tbTransitions_D <- mtMod_D$tbTransitions
alivepop_D <- mtMod_D$alivepop

varind_D <- mtMod_D$varind
traind_D <- mtMod_D$traind
#Define state names for solver 
symbols_D <- mtMod_D$tbCompartments %>% pull(id, name=State)
getStates.D <- function(x) {
  popc <- .colSums(x[varind_D[alivepop_D,]], m=length(alivepop_D), n=N)
  states <- lapply(symbols_D, function(id){x[varind_D[id,]]})
  states$D.pop <- c(popc)
  return(states)
}

#### Define transitions_D #### 
# ************************************************************************************* #

# transitions_D will be a matrix with 4 columns:
# columns 1,2 describe the source of transitions
# columns 3,4 describe the destination
# columns 1,3 describe the compartment
# columns 2,4 describe if this transition is coming, leaving or both
transitionCreationExpressions_D <- mt_makeTransitionCreationCode(mtMod_D, 
                                                               modelLetter=modelLetter) %>% 
  parse(text=.)
# TO PRINT THE CODE USED TO CONSTRUCT TRANSITIONS `transitions_D`:
# as.character(transitionCreationExpressions_D)%>%paste0(collapse='\n')%>%cat
transitions_D <- matrix(0, nrow=N*nrow(tbTransitions_D), ncol=4)
for (n in 1:N) {
  nxt <- min(n+1, N)  # nxt used in aging, for the last age group nxt should just be n
  # Evaluate each of the transition creation expressions in the current environment
  curEnv <- environment()
  transitionCreationExpressions_D %>% 
    eval(envir = curEnv)
}
# ************************************************************************************* # #YES
#### Base functions ####

# ************************************************************************************* #
# Function to calculate transition rates, given variables and parameters
# ************************************************************************************* #
# transitions function
disrates.D <- function(x, parameters, t) {
  with(as.list(c(parameters, getStates.D(x))), {
    
    #time filters 
    t_internal<-t#+startyear
    tic<-as.character(floor(t_internal) )
    #if(!(tic %in% colnames(covm))) browser()
    #  if(sum(D.V_1<0)>0) browser()
    # if(t_internal>2025.87) browser()
    #browser()
    
    #parameters
    infectious_D <- (theta_Ia_D*D.I_a + D.I_pr + D.I_p + D.I_pt + D.I_m +D.I_mt)/D.pop
    lambda_D = 1*ptrans_D*as.vector(contact%*%infectious_D)
    
    mmcov =  femProp*fertProp*covm[,tic]
    mrate = -log(1-mmcov)/1
    
    totbirths = c(births[1]*sum(D.pop), rep(0,N-1))
  #  mprotect = femProp[1]*sum((D.V_1+D.V_2+D.V_3+D.V_b+D.V_cb+D.V_ab+D.V_mm+D.M)[44:51])/(sum(D.pop[44:51]))
    mprotect =sum((femProp[1]*fertProp*(D.V_1+D.V_2+D.V_3+D.V_b+D.V_cb+D.V_ab)+D.V_mm1+D.V_mm2+D.M1+D.M2)[44:51])/(sum(femProp[1]*fertProp*D.pop[44:51]))
    mprotect = min(mprotect, 1)
    
    #Debugging
    #Vaccine start conditions set to 0
    #cov[,tic]<-rep(0, N)
    # mov_D[,tic]<-1
    #mrate=0
    #totbirths <- deathprop <- rep(0,N)
    
    #lambda_D<-rep(0, N)
    #tau_vmm_D[tic]<-0
    #tau_mi_D[tic]<-tau_1_D[tic]<-tau_2_D[tic]<-tau_3_D[tic]<-tau_b_D[tic]<-tau_cb_D[tic]<-tau_ab_D[tic]<-tau_vmm_D[tic]<-0
    #agerate<-rep(0, N)
    
    
    #### TO MAKE RATES CODE: #
    #mt_makeTransitionRatesCode(mtMod_D) %>% cat
    #### Check symbols defined
    
    # view(mt_findSymbols(mtMod_D, ls()))
    # T1 <- Sys.time()
    # RATES (PASTED) ####
    
    cov1<-cov1yr[,tic]
    cov2<-cov2yr[,tic]
    cov3<-cov3yr[,tic]
    covb<-covbyr[,tic]
    covcb<-covcbyr[,tic]
    covab<-covabyr[,tic]
    
    covboost<-covb+covcb+covab
    
    delta_D<-365.25/3.5 #NICD https://www.nicd.ac.za/wp-content/uploads/2017/03/NICD-guidelines_diphtheria_v3_28-May-2018.pdf
    omega_D<-365.25/3 # upper bound of parameter
    phi_D <- 365.25/5 #range is 2 (0-33) for Rohingya response. should be higher in non-emergency setting

    
 # if (tic>2025) browser()
    
    tranrate <- array(c(
      (1-mprotect)*totbirths,  # births not Protected -> S
      mprotect*totbirths,  # births Protected -> V_mi_P
      deathprop * D.S,  # natural death
      deathprop * D.E,  # natural death
      deathprop * D.Epv,  # natural death
      deathprop * D.Ev,  # natural death
      deathprop * D.I_a,  # natural death
      deathprop * D.I_pr,  # natural death
      deathprop * D.I_p,  # natural death
      deathprop * D.I_pt,  # natural death
      deathprop * D.I_m,  # natural death
      deathprop * D.I_mt,  # natural death
      deathprop * D.W,  # natural death
      deathprop * D.R,  # natural death
      deathprop * D.V_mi,  # natural death
      deathprop * D.V_1,  # natural death
      deathprop * D.V_2,  # natural death
      deathprop * D.V_3,  # natural death
      deathprop * D.V_b,  # natural death
      deathprop * D.V_cb,  # natural death
      deathprop * D.V_ab,  # natural death
      deathprop * D.V_mm1,  # natural death
      deathprop * D.M1,  # natural death
      deathprop * D.V_mm2,  # natural death
      deathprop * D.M2,  # natural death
      lambda_D * D.S,  # incidence S->E
      lambda_D * D.V_1,  # incidence V_1->Epv
      lambda_D * D.V_2,  # incidence V_2->Epv
      lambda_D * D.V_3,  # incidence V_3->Ev
      lambda_D * D.V_b,  # incidence V_b->Ev
      lambda_D * D.V_cb,  # incidence V_cb->Ev
      lambda_D * D.V_ab,  # incidence V_ab->Ev
      lambda_D * D.V_mm1,  # incidence V_mm1->Epv
      lambda_D * D.M1,  # incidence M1->Epv
      lambda_D * D.V_mm2,  # incidence V_mm2->Ev
      lambda_D * D.M2,  # incidence M2->Ev
      lambda_D * D.R,  # incidence R->Ev
      lambda_D * D.W,  # incidence W->Epv
      p_a_D*delta_D*D.E,  # incubation E->Ia
      (1-p_mt_D)*(1-p_s_D)*(1-p_a_D)*delta_D*D.E,  # incubation E->Ipr
      p_s_D*(1-p_a_D)*delta_D*D.E,  # incubation E->Ip
      p_mt_D*(1-p_s_D)*(1-p_a_D)*delta_D*D.E,  # incubation E->Ipt
      p_a_pv_D*delta_D*D.Epv,  # incubation Epv->Ia
      (1-p_mt_D)*(1-p_s_pv_D)*(1-p_a_pv_D)*delta_D*D.Epv,  # incubation Epv->Ipr
      p_s_pv_D*(1-p_a_pv_D)*delta_D*D.Epv,  # incubation Epv->Ip
      p_mt_D*(1-p_s_pv_D)*(1-p_a_pv_D)*delta_D*D.Epv,  # incubation Epv->Ipt
      p_a_v_D*delta_D*D.Ev,  # incubation Ev->Ia
      (1-p_mt_D)*(1-p_s_v_D)*(1-p_a_v_D)*delta_D*D.Ev,  # incubation Ev->Ipr
      p_s_v_D*(1-p_a_v_D)*delta_D*D.Ev,  # incubation Ev->Ip
      p_mt_D*(1-p_s_v_D)*(1-p_a_v_D)*delta_D*D.Ev,  # incubation Ev->Ipt
      (1-p_st_D)*omega_D*D.I_p,  # severe Ip->Im
      p_st_D*omega_D*D.I_p,  # severe Ip->Imt
      gamma_nt_D * D.I_a,  # recovery Ia -> R
      gamma_nt_D * D.I_pr,  # recovery I_pr -> R
      gamma_t_D * D.I_pt,  # recovery I_pt -> R
      (1-p_nt_d_D)*gamma_m_D*D.I_m,  # recovery I_m -> R
      (1-p_t_d_D)*gamma_t_D*D.I_mt,  # recovery I_mt -> R
      p_nt_d_D*gamma_m_D*D.I_m,  # diphtheria death Im -> D
      p_t_d_D*gamma_t_D*D.I_mt,  # diphtheria death Imt -> D
      tau_mi_D[tic] * D.V_mi,  # wane V_mi -> S
      tau_1_D[tic] * D.V_1,  # wane V_1 -> W
      tau_2_D[tic] * D.V_2,  # wane V_2 -> W
      tau_3_D[tic] * D.V_3,  # wane V_3 -> W
      tau_b_D[tic] * D.V_b,  # wane V_b -> W
      tau_cb_D[tic] * D.V_cb,  # wane V_cb -> W
      tau_ab_D[tic]* D.V_ab,  # wane V_ab -> W
      tau_mm_D[tic] * D.M1,  # wane M1 -> W
      tau_mm_D[tic] * D.M2,  # wane M2 -> W
      tau_D * D.R,  # wane R->W
      agerate*(1-(cov1+covboost)*mov_D[,tic])*(1-deathprop) * D.S,  # ageing, not vaccinated
      agerate * (1-deathprop) * D.E,  # ageing
      agerate * (1-deathprop) * D.Epv,  # ageing
      agerate * (1-deathprop) * D.Ev,  # ageing
      agerate * (1-deathprop) * D.I_a,  # ageing
      agerate * (1-deathprop) * D.I_pr,  # ageing
      agerate * (1-deathprop) * D.I_p,  # ageing
      agerate * (1-deathprop) * D.I_pt,  # ageing
      agerate * (1-deathprop) * D.I_m,  # ageing
      agerate * (1-deathprop) * D.I_mt,  # ageing
      agerate*(1-(cov1+covboost)*mov_D[,tic])*(1-deathprop) * D.W,  # ageing, not vaccinated
      agerate*(1-(cov1+covboost)*mov_D[,tic])*(1-deathprop) * D.R,  # ageing, not vaccinated
      agerate*(1-cov1*mov_D[,tic])*(1-deathprop) * D.V_mi,  # ageing, not vaccinated
      agerate*(1-(cov2+covboost)*mov_D[,tic])*(1-deathprop) * D.V_1,  # ageing, not vaccinated
      agerate*(1-(cov3+covboost)*mov_D[,tic])*(1-deathprop) * D.V_2,  # ageing, not vaccinated
      agerate*(1-covboost*mov_D[,tic])*(1-deathprop) * D.V_3,  # ageing, not vaccinated
      agerate*(1-(covcb+covab)*mov_D[,tic])*(1-deathprop) * D.V_b,  # ageing, not vaccinated
      agerate*(1-covab*mov_D[,tic])*(1-deathprop) * D.V_cb,  # ageing, not vaccinated
      agerate*(1-deathprop) * D.V_ab,  # ageing, not vaccinated
      agerate * (1-deathprop) * D.V_mm1,  # ageing
      agerate * (1-deathprop) * D.M1,  # ageing
      agerate * (1-deathprop) * D.V_mm2,  # ageing
      agerate * (1-deathprop) * D.M2,  # ageing
      agerate*cov1*mov_D[,tic]*(1-deathprop) * D.S,  # ageing, vaccinated S -> V_1
      agerate*covb*mov_D[,tic]*(1-deathprop) * D.S,  # ageing, vaccinated S -> V_b
      agerate*covcb*mov_D[,tic]*(1-deathprop) * D.S,  # ageing, vaccinated S -> V_cb
      agerate*covab*mov_D[,tic]*(1-deathprop) * D.S,  # ageing, vaccinated S -> V_ab
      agerate*cov1*mov_D[,tic]*(1-deathprop) * D.V_mi,  # ageing, vaccinated V_mi -> V_1
      agerate*cov1*mov_D[,tic]*(1-deathprop) * D.W,  # ageing, vaccinated W -> V_1
      agerate*covb*mov_D[,tic]*(1-deathprop) * D.W,  # ageing, vaccinated W -> V_b
      agerate*covcb*mov_D[,tic]*(1-deathprop) * D.W,  # ageing, vaccinated W -> V_cb
      agerate*covab*mov_D[,tic]*(1-deathprop) * D.W,  # ageing, vaccinated W -> V_ab
      agerate*cov1*mov_D[,tic]*(1-deathprop) * D.R,  # ageing, vaccinated R -> V_1
      agerate*covb*mov_D[,tic]*(1-deathprop) * D.R,  # ageing, vaccinated R -> V_b
      agerate*covcb*mov_D[,tic]*(1-deathprop) * D.R,  # ageing, vaccinated R -> V_cb
      agerate*covab*mov_D[,tic]*(1-deathprop) * D.R,  # ageing, vaccinated R -> V_ab
      agerate*cov2*mov_D[,tic]*(1-deathprop) * D.V_1,  # ageing, vaccinated V_1 -> V_2
      agerate*covb*mov_D[,tic]*(1-deathprop) * D.V_1,  # ageing, vaccinated V_1 -> V_b
      agerate*covcb*mov_D[,tic]*(1-deathprop) * D.V_1,  # ageing, vaccinated V_1 -> V_cb
      agerate*covab*mov_D[,tic]*(1-deathprop) * D.V_1,  # ageing, vaccinated V_1 -> V_ab
      agerate*cov3*mov_D[,tic]*(1-deathprop) * D.V_2,  # ageing, vaccinated V_2 -> V_3
      agerate*covb*mov_D[,tic]*(1-deathprop) * D.V_2,  # ageing, vaccinated V_2 -> V_b
      agerate*covcb*mov_D[,tic]*(1-deathprop) * D.V_2,  # ageing, vaccinated V_2 -> V_cb
      agerate*covab*mov_D[,tic]*(1-deathprop) * D.V_2,  # ageing, vaccinated V_2 -> V_ab
      agerate*covb*mov_D[,tic]*(1-deathprop) * D.V_3,  # ageing, vaccinated V_3 -> V_b
      agerate*covcb*mov_D[,tic]*(1-deathprop) * D.V_3,  # ageing, vaccinated V_3 -> V_cb
      agerate*covab*mov_D[,tic]*(1-deathprop) * D.V_3,  # ageing, vaccinated V_3 -> V_ab
      agerate*covcb*mov_D[,tic]*(1-deathprop) * D.V_b,  # ageing, vaccinated V_b -> V_cb
      agerate*covab*mov_D[,tic]*(1-deathprop) * D.V_b,  # ageing, vaccinated V_b -> V_ab
      agerate*covab*mov_D[,tic]*(1-deathprop) * D.V_cb,  # ageing, vaccinated V_cb -> V_ab
      mrate*movm_D[,tic] * D.S,  # Maternal vaccinated S-> V_mm1
      mrate*movm_D[,tic] * D.W,  # Maternal vaccinated W-> V_mm2
      mrate*movm_D[,tic] * D.R,  # Maternal vaccinated R-> V_mm2
      mrate*movm_D[,tic] * D.V_1,  # Maternal vaccinated V1-> V_mm2
      mrate*movm_D[,tic] * D.V_2,  # Maternal vaccinated V2-> V_mm2
      mrate*movm_D[,tic] * D.V_3,  # Maternal vaccinated V3-> V_mm2
      mrate*movm_D[,tic] * D.V_b,  # Maternal vaccinated Vb-> V_mm2
      mrate*movm_D[,tic] * D.V_cb,  # Maternal vaccinated Vcb-> V_mm2
      mrate*movm_D[,tic] * D.V_ab,  # Maternal vaccinated Vab-> V_mm2
      mrate*movm_D[,tic] * D.M1,  # Maternal vaccinated M1-> V_mm2
      mrate*movm_D[,tic] * D.M2,  # Maternal vaccinated M2-> V_mm2
      tau_vmm_D[tic] * D.V_mm1,  # Ending pregnancy for MM protected V_mm1 -> M1
      tau_vmm_D[tic] * D.V_mm2  # Ending pregnancy for MM protected V_mm2 -> M2
    ), dim=c(N, 132))
    tranrate <- c(t(tranrate))
    # T2 <- Sys.time()
    return(tranrate)
  })
}

# POST PROCESSING function
# Assumes dfAge/N/traind_D/varind_D exists
postproc.D  <- function(parameters, out, tran) {
  with(as.list(c(parameters)), {
    # ************************************************************************************* #
    # for outputting the  time series for each patch
    # ************************************************************************************* #
    ### Case outputs ####
    # REMEMBER to modify BOTH commented sections 1 and 2 below:
    # 1 - Add your postproc variable names
    postprocVars <- vars(popa,
                         prev_D,
                         all_Inc_D,
                         clin_Inc_D,
                         clin_Inc100k_D,
                         treat_Inc_D,
                         treat_Inc100k_D,
                         outp_Inc_D,
                         outp_Inc100k_D,
                         inp_Inc_D,
                         inp_Inc100k_D,
                         deaths_D,
                         deaths100k_D,
                         rep_Clin_D,
                         rep_Clin100k_D,
                         rep_deaths_D,
                         rep_deaths100k_D,
                         doses_1_D,
                         doses_2_D,
                         doses_3_D,
                         doses_b_D,
                         doses_cb_D,
                         doses_ab_D,
                         doses_m_D,
                         protected_D)
    
    postprocVarNames <- postprocVars %>% sapply(rlang::as_name)
    postprocVarList <- lapply(postprocVarNames, function(varName){
      matrix(0, nrow = length(out[, 1]), ncol = N)
    })
    names(postprocVarList) <- postprocVarNames
    #browser()
    allincTransitions <- tbTransitions_D %>% filter(To%in%c('D.I_pr[n]', 'D.I_p[n]', 'D.I_pt[n]', 'D.I_a[n]')) %>% pull(id)
    clinincTransitions <- tbTransitions_D %>% filter(To%in%c('D.I_pr[n]', 'D.I_p[n]', 'D.I_pt[n]')) %>% pull(id)
    treatincTransitions <- tbTransitions_D %>% filter(To%in%c('D.I_mt[n]', 'D.I_pt[n]')) %>% pull(id)
    outincTransitions <- tbTransitions_D %>% filter(To==c('D.I_pt[n]')) %>% pull(id)
    inincTransitions <- tbTransitions_D %>% filter(To==c('D.I_mt[n]')) %>% pull(id)
    infCompartments <- rownames(varind_D)[rownames(varind_D) %>% str_starts('D.I')]
    deathTransitions <- tbTransitions_D %>% filter(To=='D.D[n]') %>% pull(id)
    
    doses_1Transitions <- tbTransitions_D %>% filter(To=='D.V_1[nxt]', TransitionName %>% str_starts('ageV')) %>% pull(id)
    doses_2Transitions <- tbTransitions_D %>% filter(To=='D.V_2[nxt]', TransitionName %>% str_starts('ageV')) %>% pull(id)
    doses_3Transitions <- tbTransitions_D %>% filter(To=='D.V_3[nxt]', TransitionName %>% str_starts('ageV')) %>% pull(id)
    doses_bTransitions <- tbTransitions_D %>% filter(To=='D.V_b[nxt]', TransitionName %>% str_starts('ageV')) %>% pull(id)
    doses_cbTransitions <- tbTransitions_D %>% filter(To=='D.V_cb[nxt]', TransitionName %>% str_starts('ageV')) %>% pull(id)
    doses_abTransitions <- tbTransitions_D %>% filter(To=='D.V_ab[nxt]', TransitionName %>% str_starts('ageV')) %>% pull(id)
  #  doses_mTransitions <- tbTransitions_D %>% filter(To=='D.V_mm[n]', TransitionName %>% str_starts('M')) %>% pull(id)
    doses_mTransitions <- tbTransitions_D %>% filter(str_starts(TransitionComment,'Maternal vaccinated')) %>% pull(id)
    
    protectCompartments <- c(rownames(varind_D)[rownames(varind_D) %>% str_starts('D.V')], 'D.M2', 'D.R')
    
    #browser()
    for (n in 1:N) {
      # 2 - Fill variables with values for each N
      popa <- rowSums(out[,c(varind_D[c(alivepop_D),n])+1])  # Alive population
      postprocVarList$popa[, n]  <- popa
      postprocVarList$prev_D[, n] <- rowSums(out[, c(varind_D[infCompartments, n])+1]) # Prevalence

      postprocVarList$all_Inc_D[, n]  <- rowSums(tran[, unname(traind_D[allincTransitions, n])] / 365)     # All Incidence
      postprocVarList$clin_Inc_D[, n]  <- rowSums(tran[, unname(traind_D[clinincTransitions, n])] / 365)     # Clinical Incidence
      postprocVarList$clin_Inc100k_D[, n]  <- (rowSums(tran[, unname(traind_D[clinincTransitions, n])])/popa*100000)/365     # Clinical Incidence per 100k
      postprocVarList$treat_Inc_D[, n]  <- rowSums(tran[, unname(traind_D[treatincTransitions, n])] / 365)     # Treated Incidence
      postprocVarList$treat_Inc100k_D[, n]  <- (rowSums(tran[, unname(traind_D[treatincTransitions, n])])/popa*100000)/365     # Treated Incidence per 100k
      postprocVarList$outp_Inc_D[, n]  <- rowSums(tran[, unname(traind_D[outincTransitions, n])] / 365)     # Outpatients Incidence
      postprocVarList$outp_Inc100k_D[, n]  <- (rowSums(tran[, unname(traind_D[outincTransitions, n])])/popa*100000)/365     # Outpatients Incidence per 100k
      postprocVarList$inp_Inc_D[, n]  <- (tran[, unname(traind_D[inincTransitions, n])] / 365)     # Inpatients Incidence
      postprocVarList$inp_Inc100k_D[, n]  <- ((tran[, unname(traind_D[inincTransitions, n])])/popa*100000)/365     # Inpatients  per 100k
      postprocVarList$rep_Clin_D[, n]  <- p_rep_clin_D*rowSums(tran[, unname(traind_D[clinincTransitions, n])] / 365)     # Reported Clinical Incidence
      postprocVarList$rep_Clin100k_D[, n]  <- p_rep_clin_D*(rowSums(tran[, unname(traind_D[clinincTransitions, n])])/popa*100000)/365     # Reported Clinical Incidence per 100k
      
      postprocVarList$deaths_D[, n]  <- rowSums(tran[, unname(traind_D[deathTransitions, n])] / 365)     # Disease-related deaths incidence
      postprocVarList$deaths100k_D[, n]  <- (rowSums(tran[, unname(traind_D[deathTransitions, n])])/popa*100000)/365     # Disease-related deaths Incidence per 100k
      postprocVarList$rep_deaths_D[, n]  <- p_rep_death_D*rowSums(tran[, unname(traind_D[deathTransitions, n])] / 365)     # Reported Disease-related deaths incidence
      postprocVarList$rep_deaths100k_D[, n]  <- p_rep_death_D*(rowSums(tran[, unname(traind_D[deathTransitions, n])])/popa*100000)/365     # Reported Disease-related deaths Incidence per 100k
      
      postprocVarList$doses_1_D[, n]  <- rowSums(tran[, traind_D[doses_1Transitions, n]] / 365)     # Doses
      postprocVarList$doses_2_D[, n]  <- (tran[, traind_D[doses_2Transitions, n]] / 365)     # Doses
      postprocVarList$doses_3_D[, n]  <- (tran[, traind_D[doses_3Transitions, n]] / 365)     # Doses
      postprocVarList$doses_b_D[, n]  <- rowSums(tran[, traind_D[doses_bTransitions, n]] / 365)     # Doses
      postprocVarList$doses_cb_D[, n]  <- rowSums(tran[, traind_D[doses_cbTransitions, n]] / 365)     # Doses
      postprocVarList$doses_ab_D[, n]  <- rowSums(tran[, traind_D[doses_abTransitions, n]] / 365)     # Doses
      postprocVarList$doses_m_D[, n]  <- rowSums(tran[, traind_D[doses_mTransitions, n]] / 365)     # Doses
      
      postprocVarList$protected_D[, n] <- rowSums(out[, c(varind_D[protectCompartments, n])+1]) # Prevalence
    }
    # Ignore from here down
    timesteps <- out[, 1, drop = T]
    formatAsTibble <- function(varName) {
      df <- postprocVarList[[varName]]
      colnames(df) <- 1:N
      as_tibble(df) %>%
        mutate(time = timesteps) %>%
        pivot_longer(!time, names_to = 'age', values_to = varName) %>%
        mutate(age=as.integer(age), age_group = as_factor(dfAge$age_group[age])) %>% 
        select(!age)
    }
    df <- as_tibble(expand.grid(age_group = as_factor(dfAge$age_group[1:N]),
                                time = timesteps)) %>%
      select(time, age_group)
    
    for (i in seq_along(postprocVarList)) {
      varName <- postprocVarNames[[i]]
      df <- df %>%
        left_join(formatAsTibble(varName), by=c('time','age_group'))
    }
    # Return nicely formatted tibble
    df %>%
      pivot_longer(!c(time,age_group), names_to='variable')
  })
}


# EQUATION COMPILER
epiModel.D <- function(t, state, parameters) {
    # rates of change
  transit <- disrates.D(state, parameters, t)
  EQ(dZ, transit, transitions_D[,1], transitions_D[,3], transitions_D[,2], transitions_D[,4])

  list(c(dZ))
}
  
makeInitialConditionsCode.D <- function(mtMod_D, parameters, coverage_table, ISO3) {
  chr = as.character
  sy = startyear
  cov1 <- coverage_table %>% pull(DTPCV1, name=Year)
  cov2 <- coverage_table %>% pull(DTPCV2, name=Year)
  cov3 <- coverage_table %>% pull(DTPCV3, name=Year)
  eff1 <- first(parameters$eff_1_D)
  eff2 <- first(parameters$eff_2_D)
  eff3 <- first(parameters$eff_3_D)
  
  gbdPrev <- readRDS('models/data/tbGbdPrev.rds') %>%
    filter(iso3==ISO3, disease=="Diphtheria") %>%
    pull(value, name=age_group)
  
  LOG('makeInitialConditionsCode.D(sy={sy}, ISO3={ISO3})')
  pop <- getPopData(ISO3, year=sy) %>% pull(popTot, name=age_group)
  initcond <- as_tibble(expand_grid(compartment=tbCompartments_D$State, age_group=names(pop))) %>%
    mutate(value=0) %>%
    pivot_wider(names_from = 'compartment', values_from=value) %>%
    mutate(age_yrs=case_when(age_group%in%age_group[1:17] ~ 0,
                             age_group%in%age_group[18:29] ~ 1,
                             age_group==age_group[30] ~ 2,
                             age_group==age_group[31] ~ 3,
                             age_group==age_group[32] ~ 4,
                             age_group==age_group[33] ~ 5,
                             age_group%in%age_group[34:56] ~ 6),
           ageId=as.integer(as_factor(age_group))
           ) %>%
    select(age_yrs,everything()) %>%
    group_by(ageId, age_group, age_yrs) %>%
    group_modify(function(.x,.y) {
      with(.y, {
        .x$D.V_3 <- case_when(age_yrs==0 ~ 0,
                              age_yrs==6 ~ 0,
                              TRUE ~ cov3[[chr(sy-age_yrs)]]
        )
        .x$D.V_2 <- case_when(age_yrs==0 ~ 0,
                              age_yrs==6 ~ 0,
                              TRUE ~ (cov2[[chr(sy-age_yrs)]]-cov3[[chr(sy-age_yrs)]])
        )
        .x$D.V_1 <- case_when( age_yrs==0 ~ 0,
                               age_yrs==6 ~ mean(cov1[chr(sy-5:15)]-cov2[chr(sy-5:15)]),
                               TRUE ~ cov1[[chr(sy-age_yrs)]]-cov2[[chr(sy-age_yrs)]]
        )
        .x$D.I_pr <- 0.1*gbdPrev[[as.character(age_group)]]
        # .x$D.V_mm1 <- 0.5*with(parameters, femProp[ageId]*fertProp[ageId]*covANC)
        # .x$D.M1 <- 0.5*with(parameters, femProp[ageId]*fertProp[ageId]*covANC)
        # .x$D.V_mm2 <- 0.5*with(parameters, femProp[ageId]*fertProp[ageId]*covANC)
        # .x$D.M2 <- 0.5*with(parameters, femProp[ageId]*fertProp[ageId]*covANC)
        
        if (sum(.x)>1) {
          warning("btw, sum(.x)>1")
          .x <- .x %>% mutate(across(.fns=function(col){col/sum(.x)}))
        }
        .x$D.S <- pmax(0,1-sum(.x))
        .x
      })
    }) %>% ungroup() %>% select(age_group, starts_with('D.'))
  #browser()
  initcond %>%
    mutate(pop=unname(pop), across(where(is.numeric), .fns = ~.x*pop)) %>%
    pivot_longer(!c(age_group,pop)) %>%
    pull(value) %>% unname()
}

# RUN FUNCTION  ####
run_model.D <- function(parameters, initialConditions, timesteps,
                        returnRawModel = F, returnPostprocessedModel = T) {
  # ************************************************************************************* 
  if (!returnRawModel && !returnPostprocessedModel) {
    LOG("Not sure why you're running a model without asking for anything back...")
    return(NULL)
  }
  # If returnRawModel and returnPostprocessedModel are both true then return a
  # list with names moRaw and moPostprocessing:
  #   `moRaw` is the raw output
  #   `moPostprocessing` is a list with the tibble containing postprocessed outputs as the only element
  # The reason it's a list is because initially we were gonna have many things in there but
  # We later decided to just have one thing. The bulk of the code assumes this structure now.
  #Solve ODE
  dZ <<- as.numeric(rep(0.0, N*nrow(tbCompartments_D)))
  LOG("Solving the ODE for Diphtheria with ptrans={parameters$ptrans_D}", LEVEL$TRACE)
  outoderun <- ode(y=initialConditions, times=timesteps, func=epiModel.D, method = "euler", 
                   parms=parameters)
  LOG("Diphtheria solver diagnostics: {diagnostics(outoderun)}", LEVEL$TRACE)
  
  # Return just the raw model output:
  if(returnRawModel && !returnPostprocessedModel) {
    LOG("Returning *just* the raw model output object...")
    return(outoderun)
  }
  
  LOG("Calculating transition values for Diphtheria now", LEVEL$TRACE)
  # Compute transitions at each time step
  tranoderun <- matrix(0,
                       nrow=length(timesteps),
                       ncol=N*nrow(tbTransitions_D))
  for (ti in seq_along(timesteps)) {
    compartmentValues <- outoderun[ti,2:(N*nrow(tbCompartments_D)+1)]
    tranoderun[ti,]<-t(disrates.D(x=compartmentValues,
                                  parameters=parameters,
                                  t=timesteps[[ti]]))
  }
  # Could be useful to have a tibble here?
  # tbTranRates <- lapply(seq_along(timesteps), function(ti) {
  #     compartmentValues <- outoderun[ti,1+seq(N*nrow(tbCompartments_D))]
  #     transitionRates <- disrates.D(x=compartmentValues,
  #                                   parameters=parameters,
  #                                   t=0)
  #     matTransitionRates <- matrix(transitionRates, nrow=nrow(tbTransitions_D), ncol=N, byrow = F)
  #     matTransitionRates %>%
  #       as_tibble() %>%
  #       rowid_to_column('transitionIndex') %>%
  #       pivot_longer(!transitionIndex, names_to = 'age_group') %>% 
  # mutate(time=timesteps[[ti]])
  #     }) %>%
  #   bind_rows()
  #Compute outputs
  LOG("Postprocessing Diphtheria", LEVEL$TRACE)
  ppout <- postproc.D(parameters, outoderun, tranoderun)
  
  DISoutage <- ppout %>% 
    mutate(disease="Diphtheria",
           unit = "Daily") #Daily outputs by age group

  # DISoutage2 <- DISoutage %>% #Prevalence proportion calcs
  #   postProcSummarize.D()
  
  # DISoutall<-DISoutage %>%  #Daily outputs for all ages
  #   postProcSummarizeAges.D()
  
  # DISout<-bind_rows(DISoutage, DISoutage2, DISoutall) #Daily output
  
  # This object takes a second or two but is useful in saving time in further calculations
  DISoutageVT <- ppout %>%
    filter(str_ends(variable,'100k_D',negate=T)) %>%
    mutate(year=floor(time),
           varType=case_when(
             variable=="prev_D" ~ 'prev',
             variable=="protected_D" ~ 'prev',
             variable=='popa' ~ 'pop',
             TRUE~'general') %>%
             factor(levels=c('general','prev','pop'))) %>%
    arrange(time,age_group,varType)

  # This function %btwn% takes two inputs:
  #  * x: A factor or character vector
  #  * ivl: A vector with two elements defining a range
  # It returns a logical vector indicating whether each element of x falls between the range specified by ivl.
  `%btwn%` <- function(x,ivl) {
    lo = ivl[1]
    hi = ivl[2]
    ilo = which(levels(x)==lo)
    ihi = which(levels(x)==hi)
    result <- ilo <= as.numeric(x) & as.numeric(x) <= ihi
    result
  }
  
  DISoutu1yr <- postProcSummarizeAgesYear.D(DISoutageVT %>% filter(age_group %btwn% c("0-5wks", "11mths"))) %>% 
    mutate(age_group='<1yo')  
  
  DISoutu5yr <- postProcSummarizeAgesYear.D(DISoutageVT %>% filter(age_group %btwn% c("0-5wks", "4yrs"))) %>% 
    mutate(age_group='<5yo')
  
  DISout5_9yr <- postProcSummarizeAgesYear.D(DISoutageVT %>% filter(age_group %btwn% c("5yrs", "9yrs"))) %>% 
    mutate(age_group='5-9yo')
  
  DISout10_14yr <- postProcSummarizeAgesYear.D(DISoutageVT %>% filter(age_group %btwn% c("10yrs", "14yrs"))) %>% 
    mutate(age_group='10-14yo')
  
  DISout10_12yr <- postProcSummarizeAgesYear.D(DISoutageVT %>% filter(age_group %btwn% c("10yrs", "12yrs"))) %>% 
    mutate(age_group='10-12yo')
  
  DISoutu15yr <- postProcSummarizeAgesYear.D(DISoutageVT %>% filter(age_group %btwn% c("0-5wks", "14yrs"))) %>% 
    mutate(age_group='<15yo')
 
  DISout15_49yr <- postProcSummarizeAgesYear.D(DISoutageVT %>% filter(age_group %btwn% c("15yrs", "45-49yrs"))) %>% 
    mutate(age_group='15-49yo')
  
  DISout50_75yr <- postProcSummarizeAgesYear.D(DISoutageVT %>% filter(age_group %btwn% c("50-54yrs", "75+"))) %>% 
    mutate(age_group='50-75+yo')
    
  DISoutallyr <- postProcSummarizeAgesYear.D(DISoutageVT)  # Annual outputs for all ages
  
  DISoutageyr <- postProcSummarizeYear.D(DISoutageVT)  # Annual outputs by age group
  
  DISoutyr <- bind_rows(DISoutageyr, DISoutallyr, DISoutu1yr, DISoutu5yr, DISout5_9yr, DISout10_14yr, DISoutu15yr, DISout15_49yr, DISout50_75yr) %>% 
    
    mutate(disease="Diphtheria",
           unit = "Annual") %>% 
    select(year, age_group, variable, disease, value, unit)
  
  LOG("Completed Diphtheria, returning tibble with unit=Annual", LEVEL$TRACE)
  
  moPostprocessing = list(DISoutyr)
  if (returnRawModel && returnPostprocessedModel) {
    LOG("Returning moRaw and moPostprocessing for Diphtheria")
    return(list(moRaw=outoderun,
                moPostprocessing=moPostprocessing))
  } else {
    LOG("Returning *just* moPostprocessing for Diphtheria")
    return(moPostprocessing)
  }
}

postProcSummarize.D <- function(DISoutage) {
  
  popa<- DISoutage %>% 
    filter(variable =="popa") %>% 
    rename(popa = value)
  
  DISoutage %>%
    filter(variable%in%c('prev_D', 'protected_D')) %>% 
    ungroup() %>% 
    left_join((popa %>% select(time,age_group, popa)), by=c("time", "age_group")) %>% 
    mutate(value = value/popa) %>% 
    select(time, variable,age_group, value, disease, unit) %>%
    transmute(time=time,
              age_group = age_group,
              variable=str_replace(variable,'_D','_prop_D'),
              value = value,
              disease=disease, 
              unit=unit) 
}

postProcSummarizeAges.D <- function(DISoutage) {
  
  pop<- DISoutage %>% 
    filter(variable =="popa") %>% 
    group_by(time, variable, disease) %>% 
    summarise(pop = sum(value), .groups = 'drop_last') %>% 
    mutate(variable = "pop")
  
  sum1<- DISoutage %>%
    filter(str_ends(variable, '100k_D', negate = T) ) %>% 
    group_by(time, variable, disease) %>%
    summarise(value = sum(value), .groups = 'drop_last') 
  
  
  sum2<- sum1 %>% 
    left_join((pop %>% select(time,pop)), by=c("time")) %>% 
    mutate(per100k = value/pop*100000) %>% 
    transmute(time=time,
              variable=str_replace(variable.x,'_D','100k_D'),
              disease=disease,
              value=per100k) %>% 
    filter(!variable%in%c('popa'))
  
  
  sum3<- sum1 %>% 
    filter(variable%in%c('prev_D', 'protected_D')) %>%
    left_join((pop %>% select(time,pop)), by=c("time")) %>% 
    mutate(value = value/pop) %>% 
    transmute(time=time,
              variable=str_replace(variable.x,'_D','_prop_D'),
              value = value,disease=disease) %>% 
    filter(!variable%in%c('popa')) %>% 
    select(time, variable, value, disease)
  
  
  bind_rows(sum1, sum2, sum3, (pop %>% rename(value = pop))) %>% mutate(age_group = "All",
                                                                        unit = "Daily")
}

postProcSummarizeAgesYear.D <- function(DISoutageVT) {
  popyr<- DISoutageVT %>% 
    filter(varType == "pop") %>% 
    group_by(time, variable) %>% 
    summarise(pop = sum(value),.groups='keep') %>% 
    group_by(year=floor(time)) %>% 
    summarise(pop = mean(pop), .groups='drop')
  
  # general
  sum1yr<- DISoutageVT %>%
    filter(varType=='general') %>% 
    group_by(year, variable) %>% 
    summarise(value = sum(value), .groups='drop')
  
  # per100k
  sum2yr<- sum1yr %>% 
    left_join(popyr, by="year") %>%
    mutate(per100k = value/pop*100000) %>% 
    transmute(year=year,
              variable=str_replace(variable,'_D$','100k_D'),
              value=per100k)
  
  # prevalence
  sum3yr<- DISoutageVT %>%
    filter(varType=='prev') %>% 
    group_by(time, variable) %>%
    summarise(year=year,value = sum(value),.groups='keep') %>%
    group_by(year, variable) %>% 
    summarise(value = mean(value),.groups='drop') %>% 
    left_join((popyr %>% select(year, pop)), by=c("year")) %>% 
    mutate(value = value/pop) %>%
    select(year, variable, value) %>%
    transmute(year=year,
              variable=str_replace(variable, '_D$', '_prop_D'),
              value = value)
  
  sum4yr = popyr %>%
    transmute(year, variable='pop_D', value=pop, disease="Diphtheria")
  
  bind_rows(sum1yr, sum2yr, sum3yr, sum4yr) %>% mutate(age_group = "All",
                                                       unit = "Annual")
}

postProcSummarizeYear.D <- function(DISoutageVT) {
  popyr <- DISoutageVT %>% 
    filter(variable =="popa") %>% 
    group_by(year=floor(time), age_group, variable) %>% 
    summarise(pop = mean(value), .groups='drop') %>%
    select(year,age_group,pop)
  
  sum1yr <- DISoutageVT %>%
    filter(varType=='general') %>% 
    group_by(year, age_group, variable) %>%
    summarise(value = sum(value), .groups='drop')
  
  sum2yr <- sum1yr %>% 
    left_join(popyr, by=c("year", "age_group")) %>%
    transmute(year=year,
              age_group = age_group,
              variable=str_replace(variable,'_D$','100k_D'),
              value=value*1e5/pop)
  
  sum3yr<- DISoutageVT %>%
    filter(varType=='prev') %>% 
    group_by(year, age_group, variable) %>%
    summarise(value = mean(value), .groups='drop') %>%
    left_join(popyr, by=c("year", 'age_group')) %>% 
    mutate(value = value/pop,
           variable = str_replace(variable,'_D$','_prop_D')) %>% 
    select(year, age_group, variable, value)
  
  sum4yr = popyr %>%
    transmute(year=year,
              age_group=age_group,
              variable='pop_D',
              value=pop,
              disease="Diphtheria")
  
  bind_rows(sum1yr, sum2yr, sum3yr, sum4yr) %>% mutate(unit = "Annual")
}

# Plotting Functions ####
# Postprocessing
pltPostProc.D  <- function(mo, var='inc_pred') {
  mo %>%
    mutate(Date=lubridate::date_decimal(time)) %>% 
    filter(variable==var) %>% 
    ggplot() +
    aes(x = Date, y=value) +
    geom_line(size = 1.5) +
    theme_minimal() +
    facet_wrap(vars(as_factor(age_group)),scales = 'free_y') +
    labs(title=as.character(glue::glue("Diphtheria {var} plot per age group")))
}

pltPostProc_yr.D  <- function(mo, var='inc_pred') {
  mo %>%
    mutate(Date=lubridate::date_decimal(year)) %>% 
    filter(variable==var) %>% 
    ggplot() +
    aes(x = Date, y=value) +
    geom_col(size = 1.5) +
    theme_minimal() +
    facet_wrap(vars(as_factor(age_group)),scales = 'free_y') +
    labs(title=as.character(glue::glue("Diphtheria {var} plot per age group")))
}
