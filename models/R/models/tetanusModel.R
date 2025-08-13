# Model developed by MASHA at UCT
# https:://masha.uct.ac.za
# Disease: Tetanus
modelLetter <- 'T'
modelFilename <- glue::glue('cache/big{modelLetter}.xlsx')

#### Ensure bigX exists #### 
# mt_updateBigX(modelLetter=modelLetter,
#               overwrite = T)
# To update shinyInputs: mt_updateShinyInputs
# To update parameter sheet: mt_updateParameters(overwrite=T)

#### Read mtMod_T in ####
mtMod_T <- mt_getTibblesInList(modelFilename=modelFilename)

lsModel_T <- mtMod_T$lsModel

tbCompartments_T <- mtMod_T$tbCompartments
tbTransitions_T <- mtMod_T$tbTransitions
alivepop_T <- mtMod_T$alivepop

varind_T <- mtMod_T$varind
traind_T <- mtMod_T$traind
#Define state names for solver 
symbols_T <- mtMod_T$tbCompartments %>% pull(id, name=State)
getStates.T <- function(x) {
  popc <- .colSums(x[varind_T[alivepop_T,]], m=length(alivepop_T), n=N)
  states <- lapply(symbols_T, function(id){x[varind_T[id,]]})
  states$T.pop <- c(popc)
  return(states)
}

#### Define transitions_T #### 
# ************************************************************************************* #

# transitions_T will be a matrix with 4 columns:
# columns 1,2 describe the source of transitions
# columns 3,4 describe the destination
# columns 1,3 describe the compartment
# columns 2,4 describe if this transition is coming, leaving or both
transitionCreationExpressions_T <- mt_makeTransitionCreationCode(mtMod_T,
                                                                 modelLetter=modelLetter) %>% 
  parse(text=.)
# TO PRINT THE CODE USED TO CONSTRUCT TRANSITIONS `transitions_T`:
# as.character(transitionCreationExpressions_T)%>%paste0(collapse='\n')%>%cat
transitions_T <- matrix(0, nrow=N*nrow(tbTransitions_T), ncol=4)
for (n in 1:N) {
  nxt <- min(n+1, N)  # nxt used in aging, for the last age group nxt should just be n
  # Evaluate each of the transition creation expressions in the current environment
  curEnv <- environment()
  transitionCreationExpressions_T %>% 
    eval(envir = curEnv)
}
# ************************************************************************************* # #YES
#### Base functions ####

# ************************************************************************************* #
# Function to calculate transition rates, given variables and parameters
# ************************************************************************************* #
# transitions function
disrates.T <- function(x, parameters, t) {
  with(as.list(c(parameters, getStates.T(x))), {
    
    #time filters 
    t_internal<-t
    tic<-as.character(floor(t_internal) )
    #if(!(tic %in% colnames(covm))) browser()
    #  if(!(sum(is.na(T.pop))>0)) browser()
    
    #parameters
    lambda_T = (10^-4)*ptrans_T #
    
    mu_T = c((mu_ntrt_nT*(1-p_st_T) + mu_trt_nT*p_st_T), 
             rep((mu_ntrt_T*(1-p_st_T) + mu_trt_T*p_st_T), N-4),
             rep((mu_ntrt_nT*(1-p_st_T) + mu_trt_nT*p_st_T),3))
    
    mmcov2 =  femProp*fertProp*covm2[,tic]
    mmcov1 =  femProp*fertProp*(covm[,tic]-covm2[,tic])
    mrate2 =  -log(1-mmcov2)/1 #; mrate2<-na.action(mrate2, 0)
    mrate1 = -log(1-mmcov1)/1
    
    totbirths = c(births[1]*sum(T.pop), rep(0,N-1))
    mprotect =sum((femProp[1]*fertProp*(T.V_1_p+T.V_2_p+T.V_3_p+T.V_4_p+T.V_5_p+T.V_6_p)+T.V_mm1_p+T.V_mm2_p+T.V_mm3_p+T.M1_p+T.M2_p+T.M3_p)[44:51])/(sum(femProp[1]*fertProp*T.pop[44:51]))
    mprotect = min(mprotect, 1)
   
    # coverage to count scheduled vaccination
    cov1<-cov1yr[,tic]
    cov2<-cov2yr[,tic]
    cov3<-cov3yr[,tic]
    covb<-covbyr[,tic]
    covcb<-covcbyr[,tic]
    covab<-covabyr[,tic]
    
    # dont delete this coverage to count vaccination at all ages
    covt1<-cov[,tic]
    covt2<-cov[,tic] - cov1
    covt3<-covt2 - cov2
    covt4<-covt3 - cov3
    covt5<-covt4 - covb
    covt6<-covab
    
    trans_nT = trans_nT*(1-0.9999)#(1-0.74) # reduced by skilled attendance at birth.
    
    #if (tic > 2013) browser()
    #### TO MAKE RATES CODE: #
     # mt_makeTransitionRatesCode(mtMod_T) %>% cat
    #### Check symbols defined
   #  browser()
    # view(mt_findSymbols(mtMod_T, ls()))
    # T1 <- Sys.time()
    # RATES (PASTED) ####
    tranrate <- array(c(
      (1-trans_nT)*(1-mprotect)*totbirths,  # births not Protected -> S
      mprotect*totbirths,  # births Protected -> V_mi_P
      trans_nT*(1-mprotect)*totbirths,  # births no Protected, poor hygiene -> I_s
      deathprop * T.S,  # natural death
      deathprop * T.I_s,  # natural death
      deathprop * T.I_vm1,  # natural death
      deathprop * T.I_m1,  # natural death
      deathprop * T.I_vm2,  # natural death
      deathprop * T.I_m2,  # natural death
      deathprop * T.I_vm3,  # natural death
      deathprop * T.I_m3,  # natural death
      deathprop * T.I_v1,  # natural death
      deathprop * T.I_v2,  # natural death
      deathprop * T.I_v3,  # natural death
      deathprop * T.I_v4,  # natural death
      deathprop * T.I_v5,  # natural death
      deathprop * T.I_v6,  # natural death
      deathprop * T.V_mi_p,  # natural death
      deathprop * T.V_1_p,  # natural death
      deathprop * T.V_1_np,  # natural death
      deathprop * T.V_2_p,  # natural death
      deathprop * T.V_2_np,  # natural death
      deathprop * T.V_3_p,  # natural death
      deathprop * T.V_3_np,  # natural death
      deathprop * T.V_4_p,  # natural death
      deathprop * T.V_4_np,  # natural death
      deathprop * T.V_5_p,  # natural death
      deathprop * T.V_5_np,  # natural death
      deathprop * T.V_6_p,  # natural death
      deathprop * T.V_6_np,  # natural death
      deathprop * T.V_mm1_p,  # natural death
      deathprop * T.V_mm1_np,  # natural death
      deathprop * T.M1_p,  # natural death
      deathprop * T.M1_np,  # natural death
      deathprop * T.V_mm2_p,  # natural death
      deathprop * T.V_mm2_np,  # natural death
      deathprop * T.M2_p,  # natural death
      deathprop * T.M2_np,  # natural death
      deathprop * T.V_mm3_p,  # natural death
      deathprop * T.V_mm3_np,  # natural death
      deathprop * T.M3_p,  # natural death
      deathprop * T.M3_np,  # natural death
      lambda_T * T.S,  # incidence S->Is
      lambda_T * T.V_1_np,  # incidence V_1_np->I_v1
      lambda_T * T.V_2_np,  # incidence V_2_np->I_v2
      lambda_T * T.V_3_np,  # incidence V_3_np->I_v3
      lambda_T * T.V_4_np,  # incidence V_4_np->I_v4
      lambda_T * T.V_5_np,  # incidence V_5_np->I_v5
      lambda_T * T.V_6_np,  # incidence V_6_np->I_v6
      lambda_T * T.V_mm1_np,  # incidence V_mm1_np->I_vm1
      lambda_T * T.M1_np,  # incidence M1_np->I_m1
      lambda_T * T.V_mm2_np,  # incidence V_mm2_np->I_vm2
      lambda_T * T.M2_np,  # incidence M2_np->I_m2
      lambda_T * T.V_mm3_np,  # incidence V_mm3_np->I_vm3
      lambda_T * T.M3_np,  # incidence M3_np->I_m3
      (1-mu_T )*gamma_T * T.I_s,  # recovery Is -> S
      (1-mu_T )*gamma_T * T.I_v1,  # recovery I_v1 -> V_1_np
      (1-mu_T )*gamma_T * T.I_v2,  # recovery I_v2 -> V_2_np
      (1-mu_T )*gamma_T * T.I_v3,  # recovery I_v3 -> V_3_np
      (1-mu_T )*gamma_T * T.I_v4,  # recovery I_v4 -> V_4_np
      (1-mu_T )*gamma_T * T.I_v5,  # recovery I_v5 -> V_5_np
      (1-mu_T )*gamma_T * T.I_v6,  # recovery I_v6 -> V_6_np
      (1-mu_T )*gamma_T * T.I_vm1,  # recovery I_vm1 -> V_mm1_np
      (1-mu_T )*gamma_T * T.I_m1,  # recovery I_m1 -> M1_np
      (1-mu_T )*gamma_T * T.I_vm2,  # recovery I_vm2 -> V_mm2_np
      (1-mu_T )*gamma_T * T.I_m2,  # recovery I_m2 -> M2_np
      (1-mu_T )*gamma_T * T.I_vm3,  # recovery I_vm3 -> V_mm3_np
      (1-mu_T )*gamma_T * T.I_m3,  # recovery I_m3 -> M3_np
      mu_T *gamma_T *  T.I_s,  # tetanus death Is -> D
      mu_T * gamma_T * T.I_v1,  # tetanus death I_v1 -> D
      mu_T * gamma_T * T.I_v2,  # tetanus death I_v2 -> D
      mu_T * gamma_T * T.I_v3,  # tetanus death I_v3 -> D
      mu_T *gamma_T *  T.I_v4,  # tetanus death I_v4 -> D
      mu_T *gamma_T *  T.I_v5,  # tetanus death I_v5 -> D
      mu_T *gamma_T *  T.I_v6,  # tetanus death I_v6 -> D
      mu_T *gamma_T *  T.I_vm1,  # tetanus death I_vmm1 -> D
      mu_T *gamma_T *  T.I_m1,  # tetanus death I_m1 -> D
      mu_T *gamma_T *  T.I_vm2,  # tetanus death I_vmm2 -> D
      mu_T *gamma_T *  T.I_m2,  # tetanus death I_m2 -> D
      mu_T *gamma_T *  T.I_vm3,  # tetanus death I_vmm3 -> D
      mu_T *gamma_T *  T.I_m3,  # tetanus death I_m3 -> D
      tau_mi_T[tic] * T.V_mi_p,  # V_mi_p -> S
      tau_1_T[tic]  * T.V_1_p,  # V_1_p -> V_1_np
      tau_2_T[tic]  * T.V_2_p,  # V_2_p -> V_2_np
      tau_3_T[tic]  * T.V_3_p,  # V_3_p -> V_3_np
      tau_4_T[tic]  * T.V_4_p,  # V_4_p -> V_4_np
      tau_5_T[tic]  * T.V_5_p,  # V_5_p -> V_5_np
      tau_6_T[tic]  * T.V_6_p,  # V_6_p -> V_6_np
      tau_mm_T[tic] * T.M1_p,  # M1_p -> M1_np
      tau_4_T[tic] * T.M2_p,  # M2_p -> M2_np
      tau_6_T[tic] * T.M3_p,  # M3_p -> M3_np
      agerate*(1-covt1*mov_T[,tic])*(1-deathprop) * T.S,  # ageing, not vaccinated
      agerate*(1-deathprop) * T.I_s,  # ageing
      agerate * (1-deathprop) * T.I_vm1,  # ageing
      agerate * (1-deathprop) * T.I_m1,  # ageing
      agerate * (1-deathprop) * T.I_vm2,  # ageing
      agerate * (1-deathprop) * T.I_m2,  # ageing
      agerate * (1-deathprop) * T.I_vm3,  # ageing
      agerate * (1-deathprop) * T.I_m3,  # ageing
      agerate * (1-deathprop) * T.I_v1,  # ageing
      agerate * (1-deathprop) * T.I_v2,  # ageing
      agerate * (1-deathprop) * T.I_v3,  # ageing
      agerate * (1-deathprop) * T.I_v4,  # ageing
      agerate * (1-deathprop) * T.I_v5,  # ageing
      agerate * (1-deathprop) * T.I_v6,  # ageing
      agerate*(1-covt1*mov_T[,tic])*(1-deathprop) * T.V_mi_p,  # ageing, not vaccinated
      agerate*(1-covt2*mov_T[,tic])*(1-deathprop) * T.V_1_p,  # ageing, not vaccinated
      agerate*(1-covt2*mov_T[,tic])*(1-deathprop) * T.V_1_np,  # ageing, not vaccinated
      agerate*(1-covt3*mov_T[,tic])*(1-deathprop) * T.V_2_p,  # ageing, not vaccinated
      agerate*(1-covt3*mov_T[,tic])*(1-deathprop) * T.V_2_np,  # ageing, not vaccinated
      agerate*(1-covt4*mov_T[,tic])*(1-deathprop) * T.V_3_p,  # ageing, not vaccinated
      agerate*(1-covt4*mov_T[,tic])*(1-deathprop) * T.V_3_np,  # ageing, not vaccinated
      agerate*(1-covt5*mov_T[,tic])*(1-deathprop) * T.V_4_p,  # ageing, not vaccinated
      agerate*(1-covt5*mov_T[,tic])*(1-deathprop) * T.V_4_np,  # ageing, not vaccinated
      agerate*(1-covt6*mov_T[,tic])*(1-deathprop) * T.V_5_p,  # ageing, not vaccinated
      agerate*(1-covt6*mov_T[,tic])*(1-deathprop) * T.V_5_np,  # ageing, not vaccinated
      agerate*(1-deathprop) * T.V_6_p,  # ageing, not vaccinated
      agerate*(1-deathprop) * T.V_6_np,  # ageing, not vaccinated
      agerate * (1-deathprop) * T.V_mm1_p,  # ageing
      agerate * (1-deathprop) * T.V_mm1_np,  # ageing
      agerate * (1-deathprop) * T.M1_p,  # ageing
      agerate * (1-deathprop) * T.M1_np,  # ageing
      agerate * (1-deathprop) * T.V_mm2_p,  # ageing
      agerate * (1-deathprop) * T.V_mm2_np,  # ageing
      agerate * (1-deathprop) * T.M2_p,  # ageing
      agerate * (1-deathprop) * T.M2_np,  # ageing
      agerate * (1-deathprop) * T.V_mm3_p,  # ageing
      agerate * (1-deathprop) * T.V_mm3_np,  # ageing
      agerate * (1-deathprop) * T.M3_p,  # ageing
      agerate * (1-deathprop) * T.M3_np,  # ageing
      agerate*covt1*mov_T[,tic]*(1-deathprop) * T.S,  # ageing, vaccinated S -> V_1_np
      agerate*covt1*mov_T[,tic]*(1-deathprop) * T.V_mi_p,  # ageing, vaccinated V_mi_p -> V_1_p
      agerate*covt2*mov_T[,tic]*(1-deathprop) * T.V_1_p,  # ageing, vaccinated V_1_p -> V_2_p
      agerate*covt2*mov_T[,tic]*eff_2_T[tic]*(1-deathprop) * T.V_1_np,  # ageing, vaccinated V_1_np -> V_2_p
      agerate*covt2*mov_T[,tic]*(1-eff_2_T[tic])*(1-deathprop) * T.V_1_np,  # ageing, vaccinated V_1_np -> V_2_np
      agerate*covt3*mov_T[,tic]*(1-deathprop) * T.V_2_p,  # ageing, vaccinated V_2_p -> V_3_p
      agerate*covt3*mov_T[,tic]*eff_3_T[tic]*(1-deathprop) * T.V_2_np,  # ageing, vaccinated V_2_np -> V_3_p
      agerate*covt3*mov_T[,tic]*(1-eff_3_T[tic])*(1-deathprop) * T.V_2_np,  # ageing, vaccinated V_2_np -> V_3_np
      agerate*covt4*mov_T[,tic]*(1-deathprop) * T.V_3_p,  # ageing, vaccinated V_3_p -> V_4_p
      agerate*covt4*mov_T[,tic]*eff_4_T[tic]*(1-deathprop) * T.V_3_np,  # ageing, vaccinated V_3_np -> V_4_p
      agerate*covt4*mov_T[,tic]*(1-eff_4_T[tic])*(1-deathprop) * T.V_3_np,  # ageing, vaccinated V_3_np -> V_4_np
      agerate*covt5*mov_T[,tic]*(1-deathprop) * T.V_4_p,  # ageing, vaccinated V_4_p -> V_5_p
      agerate*covt5*mov_T[,tic]*eff_5_T[tic]*(1-deathprop) * T.V_4_np,  # ageing, vaccinated V_4_np -> V_5_p
      agerate*covt5*mov_T[,tic]*(1-eff_5_T[tic])*(1-deathprop) * T.V_4_np,  # ageing, vaccinated V_4_np -> V_5_np
      agerate*covt6*mov_T[,tic]*(1-deathprop) * T.V_5_p,  # ageing, vaccinated V_5_p -> V_6_p
      agerate*covt6*mov_T[,tic]*eff_6_T[tic]*(1-deathprop) * T.V_5_np,  # ageing, vaccinated V_5_np -> V_6_p
      agerate*covt6*mov_T[,tic]*(1-eff_6_T[tic])*(1-deathprop) * T.V_5_np,  # ageing, vaccinated V_5_np -> V_6_np
      mrate1*movm_T[,tic] * T.V_1_p,  # Maternal vaccinated 1 dose V_1_p -> V_mm1_p
      mrate1*movm_T[,tic] * T.V_2_p,  # Maternal vaccinated 1 dose V_2_p -> V_mm1_p
      mrate1*movm_T[,tic] * T.V_3_p,  # Maternal vaccinated 1 dose V_3_p -> V_mm2_p
      mrate1*movm_T[,tic] * T.V_4_p,  # Maternal vaccinated 1 dose V_4_p -> V_mm3_p
      mrate1*movm_T[,tic] * T.V_5_p,  # Maternal vaccinated 1 dose V_5_p -> V_mm3_p
      mrate1*movm_T[,tic] * T.V_6_p,  # Maternal vaccinated 1 dose V_6_p -> V_mm3_p
      mrate2*movm_T[,tic] * T.V_1_p,  # Maternal vaccinated 2 dose V_1_p -> V_mm1_p
      mrate2*movm_T[,tic] * T.V_2_p,  # Maternal vaccinated 2 dose V_2_p -> V_mm2_p
      mrate2*movm_T[,tic] * T.V_3_p,  # Maternal vaccinated 2 dose V_3_p -> V_mm3_p
      mrate2*movm_T[,tic] * T.V_4_p,  # Maternal vaccinated 2 dose V_4_p -> V_mm3_p
      mrate2*movm_T[,tic] * T.V_5_p,  # Maternal vaccinated 2 dose V_5_p -> V_mm3_p
      mrate2*movm_T[,tic] * T.V_6_p,  # Maternal vaccinated 2 dose V_6_p -> V_mm3_p
      mrate1*movm_T[,tic] * T.M1_p,  # Maternal vaccinated 1 dose M1_p -> V_mm2_p
      mrate2*movm_T[,tic] * T.M1_p,  # Maternal vaccinated 2 dose M1_p -> V_mm2_p
      mrate1*movm_T[,tic] * T.M2_p,  # Maternal vaccinated 1 dose M2_p -> V_mm3_p
      mrate2*movm_T[,tic] * T.M2_p,  # Maternal vaccinated 2 dose M2_p -> V_mm3_p
      mrate1*movm_T[,tic] * T.M3_p,  # Maternal vaccinated 1 dose M3_p -> V_mm3_p
      mrate2*movm_T[,tic] * T.M3_p,  # Maternal vaccinated 2 dose M3_p -> V_mm3_p
      mrate1*movm_T[,tic] * T.S,  # Maternal vaccinated 1 dose S -> V_mm1_np
      mrate1*movm_T[,tic] * T.V_1_np,  # Maternal vaccinated 1 dose V_1_np -> V_mm1_np
      mrate1*movm_T[,tic] * T.V_2_np,  # Maternal vaccinated 1 dose V_2_np -> V_mm1_np
      mrate1*movm_T[,tic] * T.V_3_np,  # Maternal vaccinated 1 dose V_3_np -> V_mm1_np
      mrate1*movm_T[,tic] * T.V_4_np,  # Maternal vaccinated 1 dose V_4_np -> V_mm1_np
      mrate1*movm_T[,tic] * T.V_5_np,  # Maternal vaccinated 1 dose V_5_np -> V_mm1_np
      mrate1*movm_T[,tic] * T.V_6_np,  # Maternal vaccinated 1 dose V_6_np -> V_mm1_np
      mrate1*movm_T[,tic] * T.M1_np,  # Maternal vaccinated 1 dose M1_np -> V_mm1_np
      mrate1*movm_T[,tic] * T.M2_np,  # Maternal vaccinated 1 dose M2_np -> V_mm1_np
      mrate1*movm_T[,tic] * T.M3_np,  # Maternal vaccinated 1 dose M3_np -> V_mm1_np
      mrate2*movm_T[,tic] * eff_2_T[tic] * T.S,  # Maternal vaccinated 2 dose S -> V_mm1_p
      mrate2*movm_T[,tic] * eff_m2_T[tic] * T.V_1_np,  # Maternal vaccinated 2 dose V_1_np -> V_mm1_p
      mrate2*movm_T[,tic] * eff_m2_T[tic] * T.V_2_np,  # Maternal vaccinated 2 dose V_2_np -> V_mm1_p
      mrate2*movm_T[,tic] * eff_m2_T[tic] * T.V_3_np,  # Maternal vaccinated 2 dose V_3_np -> V_mm1_p
      mrate2*movm_T[,tic] * eff_m2_T[tic] * T.V_4_np,  # Maternal vaccinated 2 dose V_4_np -> V_mm1_p
      mrate2*movm_T[,tic] * eff_m2_T[tic] * T.V_5_np,  # Maternal vaccinated 2 dose V_5_np -> V_mm1_p
      mrate2*movm_T[,tic] * eff_m2_T[tic] * T.V_6_np,  # Maternal vaccinated 2 dose V_6_np -> V_mm1_p
      mrate2*movm_T[,tic] * eff_m2_T[tic] * T.M1_np,  # Maternal vaccinated 2 dose M1_np -> V_mm1_p
      mrate2*movm_T[,tic] * eff_m2_T[tic] * T.M2_np,  # Maternal vaccinated 2 dose M2_np -> V_mm1_p
      mrate2*movm_T[,tic] * eff_m2_T[tic] * T.M3_np,  # Maternal vaccinated 2 dose M3_np -> V_mm1_p
      mrate2*movm_T[,tic] * (1-eff_2_T[tic]) * T.S,  # Maternal vaccinated 2 dose S -> V_mm1_np
      mrate2*movm_T[,tic] * (1-eff_m2_T[tic]) * T.V_1_np,  # Maternal vaccinated 2 dose V_1_np -> V_mm1_np
      mrate2*movm_T[,tic] * (1-eff_m2_T[tic]) * T.V_2_np,  # Maternal vaccinated 2 dose V_2_np -> V_mm1_np
      mrate2*movm_T[,tic] * (1-eff_m2_T[tic]) * T.V_3_np,  # Maternal vaccinated 2 dose V_3_np -> V_mm1_np
      mrate2*movm_T[,tic] * (1-eff_m2_T[tic]) * T.V_4_np,  # Maternal vaccinated 2 dose V_4_np -> V_mm1_np
      mrate2*movm_T[,tic] * (1-eff_m2_T[tic]) * T.V_5_np,  # Maternal vaccinated 2 dose V_5_np -> V_mm1_np
      mrate2*movm_T[,tic] * (1-eff_m2_T[tic]) * T.V_6_np,  # Maternal vaccinated 2 dose V_6_np -> V_mm1_np
      mrate2*movm_T[,tic] * (1-eff_m2_T[tic]) * T.M1_np,  # Maternal vaccinated 2 dose M1_np -> V_mm1_np
      mrate2*movm_T[,tic] * (1-eff_m2_T[tic]) * T.M2_np,  # Maternal vaccinated 2 dose M2_np -> V_mm1_np
      mrate2*movm_T[,tic] * (1-eff_m2_T[tic]) * T.M3_np,  # Maternal vaccinated 2 dose M3_np -> V_mm1_np
      tau_vmm_T[tic] * T.V_mm1_p,  # Ending pregnancy for MM protected V_mm1_p -> M1_p
      tau_vmm_T[tic] * T.V_mm1_np,  # Ending pregnancy for MM protected V_mm1_np -> M1_np
      tau_vmm_T[tic] * T.V_mm2_p,  # Ending pregnancy for MM protected V_mm2_p -> M2_p
      tau_vmm_T[tic] * T.V_mm2_np,  # Ending pregnancy for MM protected V_mm2_np -> M2_np
      tau_vmm_T[tic] * T.V_mm3_p,  # Ending pregnancy for MM protected V_mm3_p -> M3_p
      tau_vmm_T[tic] * T.V_mm3_np,  # Ending pregnancy for MM protected V_mm3_np -> M3_np
      agerate*cov1*mov_T[,tic]*(1-deathprop) * (T.S+T.V_mi_p),  # count: vacc dose 1
      agerate*cov2*mov_T[,tic]*(1-deathprop) * (T.S+T.V_mi_p+T.V_1_p+T.V_1_np),  # count: vacc dose 2
      agerate*cov3*mov_T[,tic]*(1-deathprop) * (T.S+T.V_mi_p+T.V_1_p+T.V_1_np++T.V_2_p+T.V_2_np),  # count: vacc dose 3
      agerate*covb*mov_T[,tic]*(1-deathprop) * (T.S+T.V_mi_p+T.V_1_p+T.V_1_np+T.V_2_p+T.V_2_np+T.V_3_p+T.V_3_np),  # count: vacc dose b
      agerate*covcb*mov_T[,tic]*(1-deathprop) * (T.S+T.V_mi_p+T.V_1_p+T.V_1_np+T.V_2_p+T.V_2_np+T.V_3_p+T.V_3_np+T.V_4_p+T.V_4_np),  # count: vacc dose cb
      agerate*covab*mov_T[,tic]*(1-deathprop) * (T.S+T.V_mi_p+T.V_1_p+T.V_1_np+T.V_2_p+T.V_2_np+T.V_3_p+T.V_3_np+T.V_4_p+T.V_4_np+T.V_5_p+T.V_5_np)  # count: vacc dose ab
    ), dim=c(N, 207))
    tranrate <- c(t(tranrate))
    # T2 <- Sys.time()
    return(tranrate)
  })
}

# POST PROCESSING function
# Assumes dfAge/N/traind_T/varind_T exists
postproc.T  <- function(parameters, out, tran) {
  with(as.list(c(parameters)), {
    # ************************************************************************************* #
    # for outputting the  time series for each patch
    # ************************************************************************************* #
    ### Case outputs ####
    # REMEMBER to modify BOTH commented sections 1 and 2 below:
    # 1 - Add your postproc variable names
    
    postprocVars <- vars(popa,
                         prev_T,
                         deathprev_T,
                         all_Inc_T,
                         clin_Inc_T,
                         clin_Inc100k_T,
                         treat_Inc_T,
                         treat_Inc100k_T,
                         outp_Inc_T,
                         outp_Inc100k_T,
                         inp_Inc_T,
                         inp_Inc100k_T,
                         deaths_T,
                         deaths100k_T,
                         rep_Clin_T,
                         rep_Clin100k_T,
                         rep_deaths_T,
                         rep_deaths100k_T,
                         nnt_Birth_T,
                         mat_Inc_T,
                         mat_Inc100k_T,
                         mat_deaths_T,
                         mat_deaths100k_T,
                         doses_1_T,
                         doses_2_T,
                         doses_3_T,
                         doses_b_T,
                         doses_cb_T,
                         doses_ab_T,
                         doses_mm1_T,
                         doses_mm2_T,
                         doses_m_T,
                         protected_T)
    
    
    postprocVarNames <- postprocVars %>% sapply(rlang::as_name)
    postprocVarList <- lapply(postprocVarNames, function(varName){
      matrix(0, nrow = length(out[, 1]), ncol = N)
    })
    names(postprocVarList) <- postprocVarNames
    
#    incTransitions <- rownames(traind_T)[rownames(traind_T) %>% str_starts('inc')]
    incTransitions <- tbTransitions_T %>% filter(TransitionName %>% str_starts('inc')) %>% pull(id)
    infCompartments <- rownames(varind_T)[rownames(varind_T) %>% str_starts('T.I')]
    deathCompartments <- rownames(varind_T)[rownames(varind_T) %>% str_starts('T.D')]
    deathTransitions <- tbTransitions_T %>% filter(TransitionName %>% str_starts('ddeath')) %>% pull(id)
    nntbirthTransitions <- tbTransitions_T %>% filter(To=='T.I_s[n]', From=='NullD[n]') %>% pull(id)
    matincTransitions <- tbTransitions_T %>% filter(TransitionName %>% str_starts('incVmm')) %>% pull(id)
    matdeathTransitions <- tbTransitions_T %>% filter(str_detect(TransitionName,'ddeathVmm')) %>% pull(id)
    
    vac1Transitions <- tbTransitions_T %>% filter(TransitionName=='cnt_V_1') %>% pull(id)
    vac2Transitions <- tbTransitions_T %>% filter(TransitionName=='cnt_V_2') %>% pull(id)
    vac3Transitions <- tbTransitions_T %>% filter(TransitionName=='cnt_V_3') %>% pull(id)
    vacbTransitions <- tbTransitions_T %>% filter(TransitionName=='cnt_V_b') %>% pull(id)
    vaccbTransitions <- tbTransitions_T %>% filter(TransitionName=='cnt_V_cb') %>% pull(id)
    vacabTransitions <- tbTransitions_T %>% filter(TransitionName=='cnt_V_ab') %>% pull(id)
    mat1Transitions <- tbTransitions_T %>% filter(str_starts(TransitionComment,'Maternal vaccinated 1')) %>% pull(id)
    mat2Transitions <- tbTransitions_T %>% filter(str_starts(TransitionComment,'Maternal vaccinated 2')) %>% pull(id)
    
    protectCompartments <- (rownames(varind_T)[rownames(varind_T) %>% str_ends('_p')])
    
    
    for (n in 1:N) {
      #browser()
      # 2 - Fill variables with values for each N
      popa <- rowSums(out[,c(varind_T[c(alivepop_T),n])+1])  # Alive population
      postprocVarList$popa[, n]  <- popa
      postprocVarList$prev_T[, n] <- rowSums(out[, c(varind_T[infCompartments, n])+1]) # Prevalence
     # postprocVarList$deathprev_T[, n] <- rowSums(out[, c(varind_T[deathCompartments, n])]+1) # Prevalence

      postprocVarList$all_Inc_T[, n]  <- rowSums(tran[, unname(traind_T[incTransitions, n])] / 365)     # All Incidence (for tetanus this is the same as clin_Inc)
      postprocVarList$clin_Inc_T[, n]  <- rowSums(tran[, unname(traind_T[incTransitions, n])] / 365)     # Clinical Incidence
      postprocVarList$clin_Inc100k_T[, n]  <- (rowSums(tran[, unname(traind_T[incTransitions, n])])/popa*100000)/365     # Clinical Incidence per 100k
      postprocVarList$treat_Inc_T[, n]  <- (p_m_T*p_mt_T + p_s_T*p_st_T)*rowSums(tran[, unname(traind_T[incTransitions, n])] / 365)     # Treated Incidence
      postprocVarList$treat_Inc100k_T[, n]  <- (p_m_T*p_mt_T + p_s_T*p_st_T)*(rowSums(tran[, unname(traind_T[incTransitions, n])])/popa*100000)/365     # Treated Incidence per 100k
      postprocVarList$outp_Inc_T[, n]  <- p_m_T*p_mt_T*rowSums(tran[, unname(traind_T[incTransitions, n])] / 365)     # Outpatients Incidence
      postprocVarList$outp_Inc100k_T[, n]  <- p_m_T*p_mt_T*(rowSums(tran[, unname(traind_T[incTransitions, n])])/popa*100000)/365     # Outpatients Incidence per 100k
      postprocVarList$inp_Inc_T[, n]  <- p_s_T*p_st_T*rowSums(tran[, unname(traind_T[incTransitions, n])] / 365)     # Inpatients Incidence
      postprocVarList$inp_Inc100k_T[, n]  <- p_s_T*p_st_T*(rowSums(tran[, unname(traind_T[incTransitions, n])])/popa*100000)/365     # Inpatients  per 100k
      postprocVarList$rep_Clin_T[, n]  <- p_rep_clin_T*rowSums(tran[, unname(traind_T[incTransitions, n])] / 365)     # Reported Clinical Incidence
      postprocVarList$rep_Clin100k_T[, n]  <- p_rep_clin_T*(rowSums(tran[, unname(traind_T[incTransitions, n])])/popa*100000)/365     # Reported Clinical Incidence per 100k
      
      postprocVarList$deaths_T[, n]  <- rowSums(tran[, unname(traind_T[deathTransitions, n])]) / 365     # Disease-related deaths incidence
      postprocVarList$deaths100k_T[, n]  <- (rowSums(tran[, unname(traind_T[deathTransitions, n])])/popa*100000)/365     # Disease-related deaths Incidence per 100k
      postprocVarList$rep_deaths_T[, n]  <- p_rep_death_T*rowSums(tran[, unname(traind_T[deathTransitions, n])] / 365)     # Reported Disease-related deaths incidence
      postprocVarList$rep_deaths100k_T[, n]  <- p_rep_death_T*(rowSums(tran[, unname(traind_T[deathTransitions, n])])/popa*100000)/365     # Reported Disease-related deaths Incidence per 100k
      
      postprocVarList$nnt_Birth_T[, n]  <- (tran[, unname(traind_T[nntbirthTransitions, n])]/ 365)     # Neonatal tetanus at birth
      postprocVarList$mat_Inc_T[, n]  <- rowSums(tran[, unname(traind_T[matincTransitions, n])]/ 365)     # Maternal tetanus incidence 
      postprocVarList$mat_Inc100k_T[, n]  <- rowSums(tran[, unname(traind_T[matincTransitions, n])]/popa*100000)/ 365     # Maternal tetanus incidence per 100k 
      postprocVarList$mat_deaths_T[, n]  <- rowSums(tran[, unname(traind_T[matdeathTransitions, n])]/ 365)     # Maternal tetanus deaths
      postprocVarList$mat_deaths100k_T[, n]  <- rowSums(tran[, unname(traind_T[matdeathTransitions, n])]/popa*100000)/ 365     # Maternal tetanus deaths per 100k 
      
      postprocVarList$doses_1_T[, n]  <- (tran[, unname(traind_T[vac1Transitions, n])] / 365)     # No. of 1st doses
      postprocVarList$doses_2_T[, n]  <- (tran[, unname(traind_T[vac2Transitions, n])] / 365)     # No. of 2nd doses
      postprocVarList$doses_3_T[, n]  <- (tran[, unname(traind_T[vac3Transitions, n])] / 365)     # No. of 3rd doses
      postprocVarList$doses_b_T[, n]  <- (tran[, unname(traind_T[vacbTransitions, n])] / 365)     # No. of infant booster doses
      postprocVarList$doses_cb_T[, n]  <- (tran[, unname(traind_T[vaccbTransitions, n])] / 365)     # No. of child booster doses
      postprocVarList$doses_ab_T[, n]  <- (tran[, unname(traind_T[vacabTransitions, n])] / 365)     # No. of adolescent booster doses
      postprocVarList$doses_mm1_T[, n]  <- rowSums(tran[, unname(traind_T[mat1Transitions, n])] / 365)     # No. of people with 1 maternal doses 
      postprocVarList$doses_mm2_T[, n]  <- rowSums(tran[, unname(traind_T[mat2Transitions, n])] / 365)     # No. of people with 2 maternal doses 
      postprocVarList$doses_m_T[, n] <- postprocVarList$doses_mm1_T[, n]+2*postprocVarList$doses_mm2_T[, n] # No. of maternal doses 
      
      postprocVarList$protected_T[, n] <- rowSums(out[, c(varind_T[protectCompartments, n])+1]) # Prevalence
      
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
epiModel.T <- function(t, state, parameters) {
  # rates of change
  transit <- disrates.T(state, parameters, t)
  EQ(dZ, transit, transitions_T[,1], transitions_T[,3], transitions_T[,2], transitions_T[,4])

  list(c(dZ))
}
  
makeInitialConditionsCode.T <- function(mtMod_T, parameters, coverage_table, ISO3 = 'ZAF') {

  chr = as.character
  sy = startyear
  cov1 <- coverage_table %>% pull(DTPCV1, name=Year)
  cov2 <- coverage_table %>% pull(DTPCV2, name=Year)
  cov3 <- coverage_table %>% pull(DTPCV3, name=Year)
  eff1 <- first(parameters$eff_1_T)
  eff2 <- first(parameters$eff_2_T)
  eff3 <- first(parameters$eff_3_T)
  
  gbdPrev <- readRDS('models/data/tbGbdPrev.rds') %>%
    filter(iso3==ISO3, disease=="Tetanus") %>% 
    pull(value, name=age_group)
  
  LOG('makeInitialConditionsCode.T(sy={sy}, ISO3={ISO3})')
  pop <- getPopData(ISO3, year=sy) %>% pull(popTot, name=age_group)
  
  initcond <- as_tibble(expand_grid(compartment=tbCompartments_T$State, age_group=names(pop))) %>% 
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
        .x$T.V_3_p <- case_when(age_yrs==0 ~ 0,
                                age_yrs==6 ~ 0,
                                TRUE ~ eff3*cov3[[chr(sy-age_yrs)]])
        .x$T.V_3_np <- case_when(age_yrs==0 ~ 0,
                                 age_yrs==6 ~ mean(cov3[chr(sy-5:15)]),
                                 TRUE ~ (1-eff3)*cov3[[chr(sy-age_yrs)]])
        .x$T.V_2_p <- case_when(age_yrs==0 ~ 0,
                                age_yrs==6 ~ 0,
                                TRUE ~ eff2*(cov2[[chr(sy-age_yrs)]]-cov3[[chr(sy-age_yrs)]]))
        .x$T.V_2_np <- case_when(age_yrs==0 ~ 0,
                                 age_yrs==6 ~ mean(cov2[chr(sy-5:15)]-cov3[chr(sy-5:15)]),
                                 TRUE ~ (1-eff2)*(cov2[[chr(sy-age_yrs)]]-cov3[[chr(sy-age_yrs)]]))
        .x$T.V_1_np <- case_when(
          age_yrs==0 ~ 0,
          age_yrs==6 ~ mean(cov1[chr(sy-5:15)]-cov2[chr(sy-5:15)]),
          TRUE ~ cov1[[chr(sy-age_yrs)]]-cov2[[chr(sy-age_yrs)]])
        .x$T.I_s <- 2*gbdPrev[[as.character(age_group)]]
       # browser()
        # .x$T.V_mm1_p <- 0.33*with(parameters, femProp[ageId]*fertProp[ageId]*covANC)
        # .x$T.M1_p <- 0.33*with(parameters, femProp[ageId]*fertProp[ageId]*covANC)
        # .x$T.V_mm2_p <- 0.33*with(parameters, femProp[ageId]*fertProp[ageId]*covANC)
        # .x$T.M2_p <- 0.33*with(parameters, femProp[ageId]*fertProp[ageId]*covANC)
        # .x$T.V_mm3_p <- 0.33*with(parameters, femProp[ageId]*fertProp[ageId]*covANC)
        # .x$T.M3_p <- 0.33*with(parameters, femProp[ageId]*fertProp[ageId]*covANC)
        if (sum(.x)>1) {
          warning("btw, sum(.x)>1")
          .x <- .x %>% mutate(across(.fns=function(col){col/sum(.x)}))
        }
        .x$T.S <- pmax(0,1-sum(.x))
        .x
      })
    }) %>% ungroup() %>% select(age_group, starts_with('T.'))
 # browser()
  initcond %>%
    mutate(pop=unname(pop), across(where(is.numeric),.fns = ~.x*pop)) %>%
    pivot_longer(!c(age_group,pop)) %>% 
    pull(value) %>% unname()
}

# RUN FUNCTION  ####
run_model.T <- function(parameters, initialConditions, timesteps,
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
  dZ <<- as.numeric(rep(0.0, N*nrow(tbCompartments_T)))
  LOG("Solving the ODE for Tetanus with ptrans={parameters$ptrans_T}", LEVEL$TRACE)
  outoderun <- ode(y=initialConditions, times=timesteps, func=epiModel.T, 
                   parms=parameters)
  LOG("Tetanus solver diagnostics: {diagnostics(outoderun)}", LEVEL$TRACE)
  
  # Return just the raw model output:
  if(returnRawModel && !returnPostprocessedModel) {
    LOG("Returning *just* the raw model output object...")
    return(outoderun)
  }
  
  LOG("Calculating transition values for Tetanus now", LEVEL$TRACE)
  # Compute transitions at each time step
  tranoderun <- matrix(0,
                       nrow=length(timesteps),
                       ncol=N*nrow(tbTransitions_T))
  for (ti in seq_along(timesteps)) {
    compartmentValues <- outoderun[ti,2:(N*nrow(tbCompartments_T)+1)]
    tranoderun[ti,]<-t(disrates.T(x=compartmentValues,
                                  parameters=parameters,
                                  t=timesteps[[ti]]))
  }
  # Could be useful to have a tibble here?
  # tbTranRates <- lapply(seq_along(timesteps), function(ti) {
  #     compartmentValues <- outoderun[ti,1+seq(N*nrow(tbCompartments_T))]
  #     transitionRates <- disrates.T(x=compartmentValues,
  #                                   parameters=parameters,
  #                                   t=0)
  #     matTransitionRates <- matrix(transitionRates, nrow=nrow(tbTransitions_T), ncol=N, byrow = F)
  #     matTransitionRates %>%
  #       as_tibble() %>%
  #       rowid_to_column('transitionIndex') %>%
  #       pivot_longer(!transitionIndex, names_to = 'age_group') %>% 
  # mutate(time=timesteps[[ti]])
  #     }) %>%
  #   bind_rows()
  #Compute outputs
  LOG("Postprocessing Tetanus", LEVEL$TRACE)
  ppout <- postproc.T(parameters, outoderun, tranoderun)
  
  DISoutage <- ppout %>%
    mutate(disease="Tetanus",
           unit = "Daily") #Daily outputs by age group

  # DISoutage2 <- DISoutage %>% #Prevalence proportion calcs
  #   postProcSummarize.T()
  
  # DISoutall<-DISoutage %>%  #Daily outputs for all ages
  #   postProcSummarizeAges.T()
  
  # DISout<-bind_rows(DISoutage, DISoutage2, DISoutall) #Daily output
  
  # TODO: Add DISoutAgeVT here (see Diphtheria model)
  
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
  
  DISoutu1yr <- postProcSummarizeAgesYear.T(DISoutage %>% filter(age_group %btwn% c("0-5wks", "11mths"))) %>% 
    mutate(age_group='<1yo')  
  
  DISoutu5yr <- postProcSummarizeAgesYear.T(DISoutage %>% filter(age_group %btwn% c("0-5wks", "4yrs"))) %>% 
    mutate(age_group='<5yo')
  
  DISout5_9yr <- postProcSummarizeAgesYear.T(DISoutage %>% filter(age_group %btwn% c("5yrs", "9yrs"))) %>% 
    mutate(age_group='5-9yo')
  
  DISout10_14yr <- postProcSummarizeAgesYear.T(DISoutage %>% filter(age_group %btwn% c("10yrs", "14yrs"))) %>% 
    mutate(age_group='10-14yo')
  
  DISout10_12yr <- postProcSummarizeAgesYear.T(DISoutage %>% filter(age_group %btwn% c("10yrs", "12yrs"))) %>% 
    mutate(age_group='10-12yo')
  
  DISoutu15yr <- postProcSummarizeAgesYear.T(DISoutage %>% filter(age_group %btwn% c("0-5wks", "14yrs"))) %>% 
    mutate(age_group='<15yo')
 # print(unique(DISoutage$age_group))
  
  DISout15_49yr <- postProcSummarizeAgesYear.T(DISoutage %>% filter(age_group %btwn% c("15yrs", "45-49yrs"))) %>% 
    mutate(age_group='15-49yo')
  
  DISout50_75yr <- postProcSummarizeAgesYear.T(DISoutage %>% filter(age_group %btwn% c("50-54yrs", "75+"))) %>% 
    mutate(age_group='50-75+yo')
  
  DISoutallyr<-postProcSummarizeAgesYear.T(DISoutage)  # Annual outputs for all ages
  
  DISoutageyr<-postProcSummarizeYear.T(DISoutage) #Annual outputs by age group

  DISoutyr <- bind_rows(DISoutageyr, DISoutallyr, DISoutu1yr, DISoutu5yr, DISout5_9yr, DISout10_14yr, DISoutu15yr, DISout15_49yr, DISout50_75yr) %>% 
    
    mutate(disease="Tetanus",
           unit = "Annual") %>% 
    select(year, age_group, variable, disease, value, unit)
  
  LOG("Completed Tetanus, returning tibble with unit=Annual", LEVEL$TRACE)
  
  moPostprocessing = list(DISoutyr)
  if (returnRawModel && returnPostprocessedModel) {
    LOG("Returning moRaw and moPostprocessing for Tetanus")
    return(list(moRaw=outoderun,
                moPostprocessing=moPostprocessing))
  } else {
    LOG("Returning *just* moPostprocessing for Tetanus")
    return(moPostprocessing)
  }
}

postProcSummarize.T <- function(DISoutage) {
  
  popa<- DISoutage %>% 
    filter(variable =="popa") %>% 
    rename(popa = value)
  
  DISoutage %>%
    filter(variable%in%c('prev_T', 'protected_T')) %>% 
    ungroup() %>% 
    left_join((popa %>% select(time,age_group, popa)), by=c("time", "age_group")) %>% 
    mutate(value = value/popa) %>% 
    select(time, variable,age_group, value, disease, unit) %>%
    transmute(time=time,
              age_group = age_group,
              variable=str_replace(variable,'_T','_prop_T'),
              value = value,
              disease=disease, 
              unit=unit) 
}

postProcSummarizeAges.T <- function(DISoutage) {
  
  pop<- DISoutage %>% 
    filter(variable =="popa") %>% 
    group_by(time, variable, disease) %>% 
    summarise(pop = sum(value), .groups = 'drop_last') %>% 
    mutate(variable = "pop")
  
  sum1<- DISoutage %>%
    filter(str_ends(variable, '100k_T', negate = T) ) %>% 
    group_by(time, variable, disease) %>%
    summarise(value = sum(value), .groups = 'drop_last') 
  
  
  sum2<- sum1 %>% 
    left_join((pop %>% select(time,pop)), by=c("time")) %>% 
    mutate(per100k = value/pop*100000) %>% 
    transmute(time=time,
              variable=str_replace(variable.x,'_T','100k_T'),
              disease=disease,
              value=per100k) %>% 
    filter(!variable%in%c('popa'))
  
  
  sum3<- sum1 %>% 
    filter(variable%in%c('prev_T', 'protected_T')) %>%
    left_join((pop %>% select(time,pop)), by=c("time")) %>% 
    mutate(value = value/pop) %>% 
    transmute(time=time,
              variable=str_replace(variable.x,'_T','_prop_T'),
              value = value,disease=disease) %>% 
    filter(!variable%in%c('popa')) %>% 
    select(time, variable, value, disease)
  
  
  bind_rows(sum1, sum2, sum3, (pop %>% rename(value = pop))) %>% mutate(age_group = "All",
                                                                        unit = "Daily")
}

postProcSummarizeAgesYear.T <- function(DISoutage) {
  
  popyr<- DISoutage %>% 
    filter(variable =="popa") %>% 
    group_by(time, variable, disease) %>% 
    summarise(pop = sum(value), .groups = 'drop') %>% 
    mutate(variable = "pop") %>%  
    group_by(year=floor(time)) %>% 
    summarise(pop = mean(pop)) #average annual value
  
  sum1yr<- DISoutage %>%
    filter(str_ends(variable, '100k_T', negate = T) ) %>% 
    group_by(time, variable, disease) %>%
    summarise(value = sum(value), .groups = 'drop') %>%
    group_by(year=floor(time), variable, disease) %>% 
    summarise(value = sum(value), .groups = 'drop_last') %>% 
    filter(!variable%in%c('popa', 'prev_T', 'protect_T')) 
  
  sum2yr<- sum1yr %>% 
    left_join(popyr, by=c("year")) %>%
    mutate(per100k = value/pop*100000) %>% 
    transmute(year=year,
              variable=str_replace(variable,'_T','100k_T'),
              disease=disease,
              value=per100k)
  
  sum3yr<- DISoutage %>%
    filter(variable %in% c('prev_T', 'protected_T') ) %>% 
    group_by(time, variable, disease) %>%
    summarise(value = sum(value), .groups = 'drop') %>%
    group_by(year=floor(time), variable, disease) %>% 
    summarise(value = mean(value), .groups = 'drop') %>%
    left_join((popyr %>% select(year, pop)), by=c("year")) %>% 
    mutate(value = value/pop) %>%
    select(year, variable, value, disease) %>%
    transmute(year=year,
              variable=str_replace(variable,'_T','_prop_T'),
              value = value,
              disease=disease)
  
  sum4yr = popyr %>%
    transmute(year, variable='pop_T', value=pop, disease='Tetanus')
  
  bind_rows(sum1yr, sum2yr, sum3yr, sum4yr) %>% mutate(age_group = "All",
                                               unit = "Annual")
}

postProcSummarizeYear.T <- function(DISoutage) {
  
  popyr<- DISoutage %>% 
    filter(variable =="popa") %>% 
    group_by(year=floor(time), age_group, variable, disease) %>% 
    summarise(pop = mean(value), .groups = 'drop_last') #average annual value
  
  sum1yr<- DISoutage %>%
    filter(str_ends(variable, '100k_T', negate = T) ) %>% 
    group_by(year=floor(time),age_group, variable, disease) %>%
    summarise(value = sum(value), .groups = 'drop_last') %>% 
    filter(!variable%in%c('popa', 'prev_T', 'protected_T'))
  
  sum2yr<- sum1yr %>% 
    ungroup() %>% 
    left_join(popyr, by=c("year", "age_group", "disease")) %>%
    mutate(per100k = value/pop*100000) %>%
    transmute(year=year,
              age_group = age_group,
              variable=str_replace(variable.x,'_T','100k_T'),
              disease=disease,
              value=per100k)
  
  sum3yr<- DISoutage %>%
    filter(variable %in% c('prev_T', 'protected_T') ) %>% 
    group_by(year=floor(time),age_group, variable, disease) %>%
    summarise(value = mean(value), .groups = 'drop') %>%
    left_join((popyr %>% select(year, age_group,  pop)), by=c("year", 'age_group')) %>% 
    mutate(value = value/pop) %>% 
    select(year, age_group, variable.x, value, disease) %>% 
    transmute(year=year,
              age_group = age_group,
              variable=str_replace(variable.x,'_T','_prop_T'),
              value = value,
              disease=disease) 
  
  sum4yr = popyr %>%
    transmute(year=year,
              age_group=age_group,
              variable='pop_T',
              value=pop,
              disease='Tetanus')
  
  bind_rows(sum1yr, sum2yr, sum3yr, sum4yr) %>% mutate(unit = "Annual")
}

# Plotting Functions ####
# Postprocessing
pltPostProc.T  <- function(mo, var='inc_pred') {
  mo %>%
    mutate(Date=lubridate::date_decimal(time)) %>% 
    filter(variable==var) %>% 
    ggplot() +
    aes(x = Date, y=value) +
    geom_line(size = 1.5) +
    theme_minimal() +
    facet_wrap(vars(as_factor(age_group)),scales = 'free_y') +
    labs(title=as.character(glue::glue("Tetanus {var} plot per age group")))
}

pltPostProc_yr.T  <- function(mo, var='inc_pred') {
  # mo %>%
  #   mutate(Date=lubridate::date_decimal(year)) %>% 
  #   filter(variable==var) %>% ggplot(.) +
  #   aes(x = Date, y = value, colour = age_group) +
  #   geom_line(size = 0.5) +
  #   scale_color_hue(direction = 1) +
  #   theme_minimal() +
  #   theme(legend.position = "none") +
  #   facet_wrap(vars(age_group), scales = "free_y") +
  #   labs(title=as.character(glue::glue("Tetanus {var} plot per age group")))
  mo %>%
    mutate(Date=lubridate::date_decimal(year)) %>% 
    filter(variable==var) %>% 
    ggplot() +
    aes(x = Date, y=value) +
    geom_col(size = 1.5) +
    theme_minimal() +
    facet_wrap(vars(as_factor(age_group)),scales = 'free_y') +
    labs(title=as.character(glue::glue("Tetanus {var} plot per age group")))
}
