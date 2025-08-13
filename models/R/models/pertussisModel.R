# Model developed by MASHA at UCT
# https:://masha.uct.ac.za
# Disease: Pertussis
modelLetter <- 'P'
modelFilename <- glue::glue('cache/big{modelLetter}.xlsx')

#### Ensure bigX exists #### 
# mt_updateBigX(modelLetter=modelLetter,
#               overwrite = T)
# To update shinyInputs: mt_updateShinyInputs
# To update parameter sheet: mt_updateParameters(overwrite=T)

#### Read mtMod_P in ####
mtMod_P <- mt_getTibblesInList(modelFilename=modelFilename)

lsModel_P <- mtMod_P$lsModel

tbCompartments_P <- mtMod_P$tbCompartments
tbTransitions_P <- mtMod_P$tbTransitions
alivepop_P <- mtMod_P$alivepop

varind_P <- mtMod_P$varind
traind_P <- mtMod_P$traind
#Define state names for solver 
symbols_P <- mtMod_P$tbCompartments %>% pull(id, name=State)
getStates.P <- function(x) {
  popc <- .colSums(x[varind_P[alivepop_P,]], m=length(alivepop_P), n=N)
  states <- lapply(symbols_P, function(id){x[varind_P[id,]]})
  states$P.pop <- c(popc)
  return(states)
}

#### Define transitions_P #### 
# ************************************************************************************* #

# transitions_P will be a matrix with 4 columns:
# columns 1,2 describe the source of transitions
# columns 3,4 describe the destination
# columns 1,3 describe the compartment
# columns 2,4 describe if this transition is coming, leaving or both
transitionCreationExpressions_P <- mt_makeTransitionCreationCode(mtMod_P,
                                                                 modelLetter=modelLetter) %>% 
  parse(text=.)
# TO PRINT THE CODE USED TO CONSTRUCT TRANSITIONS `transitions_P`:
# as.character(transitionCreationExpressions_P)%>%paste0(collapse='\n')%>%cat
transitions_P <- matrix(0, nrow=N*nrow(tbTransitions_P), ncol=4)
for (n in 1:N) {
  nxt <- min(n+1, N)  # nxt used in aging, for the last age group nxt should just be n
  # Evaluate each of the transition creation expressions in the current environment
  curEnv <- environment()
  transitionCreationExpressions_P %>% 
    eval(envir = curEnv)
}
# ************************************************************************************* # #YES
#### Base functions ####

# ************************************************************************************* #
# Function to calculate transition rates, given variables and parameters
# ************************************************************************************* #
# transitions function
disrates.P <- function(x, parameters, t) {
  with(as.list(c(parameters, getStates.P(x))), {
    
    #time filters 
    t_internal<-t#+startyear
    tic<-as.character(floor(t_internal) )
    #if(!(tic %in% colnames(covm))) browser()
    #  if(!(sum(is.na(P.pop))>0)) browser()
    
    #  if(sum(P.V_2_p<0)>0) browser()
   #  if(tic > 2026) browser()
    #parameters 
    stoc=0.2
    a<-0.5
    rand.noise <- 1# + runif(N,-stoc, stoc)
    amp<-1 #runif(1,0.8*a,1.2*a)
    seas<-1#amp*(1+cos(2*pi*t))

    # TESTING THETA VALUES FOR ASYMPOMATIC
    theta_Ia_P <- 0.25
    gamma_Ityp_P <- 12
    gamma_Iatyp_s_P <- 12
    gamma_Iatyp_ns_P <- 12
    gamma_Ia_P <- 12
    
    infectious_P <- (theta_Ityp_P*P.I_typ + theta_Iatyp_s_P*P.I_atyp_s + theta_Iatyp_ns_P*P.I_atyp_ns + theta_Ia_P*P.I_a)/P.pop
    lambda_P = 100*rand.noise*seas*ptrans_P*as.vector(contact%*%infectious_P)
    
    mmcov =  femProp*fertProp*covm[,tic]
    mrate = -log(1-mmcov)/1
    
    totbirths = c(births[1]*sum(P.pop), rep(0,N-1))
    mprotect = femProp[1]*sum((P.V_1_p+P.V_2_p+P.V_3_p+P.V_b_p+P.V_cb_p+P.V_ab_p+P.V_mm1_p+P.M1_p+P.V_mm2_p+P.M2_p+P.V_mm3_p+P.M3_p+P.R+eff_w_P*P.W)[44:51])/(sum(P.pop[44:51]))
    mprotect =sum((femProp[1]*fertProp*(P.V_1_p+P.V_2_p+P.V_3_p+P.V_b_p+P.V_cb_p+P.V_ab_p+P.R+eff_w_P*P.W)+P.V_mm1_p+P.M1_p+P.V_mm2_p+P.M2_p+P.V_mm3_p+P.M3_p)[44:51])/(sum(femProp[1]*fertProp*P.pop[44:51]))
    mprotect = min(mprotect, 1)
    #if (t_internal >2015.25) browser()
    #Debugging
    #Vaccine start conditions set to 0
    # cov[,tic]<-rep(0, N)
    # mov_P[,tic]<-1
    #mrate=0
    #totbirths <- deathprop <- rep(0,N)
    
    #lambda_P<-rep(0, N)
    #tau_vmm_D[tic]<-0
    #tau_mi_D[tic]<-tau_1_D[tic]<-tau_2_D[tic]<-tau_3_D[tic]<-tau_b_D[tic]<-tau_cb_D[tic]<-tau_ab_D[tic]<-tau_vmm_D[tic]<-0
    #agerate<-rep(0, N)
    
    
    #### TO MAKE RATES CODE: #
    # mt_makeTransitionRatesCode(mtMod_P) %>% cat
    #### Check symbols defined
    # browser()
    # view(mt_findSymbols(mtMod_P, ls()))
    # T1 <- Sys.time()
    # RATES (PASTED) ####
    #Regenerate to paste over with pertussis tranrate

    cov1<-cov1yr[,tic]
    cov2<-cov2yr[,tic]
    cov3<-cov3yr[,tic]
    covb<-covbyr[,tic]
    covcb<-covcbyr[,tic]
    covab<-covabyr[,tic]
    
    covboost<-covb+covcb+covab
    
    tranrate <- array(c(
      (1-mprotect)*totbirths,  # births not Protected -> S
      mprotect*totbirths,  # births Protected by maternal vaccination -> V_mi_p
      deathprop * P.S,  # natural death
      deathprop * P.I_typ,  # natural death
      deathprop * P.I_atyp_s,  # natural death
      deathprop * P.I_atyp_ns,  # natural death
      deathprop * P.I_a,  # natural death
      deathprop * P.W,  # natural death
      deathprop * P.R,  # natural death
      deathprop * P.V_mi_p,  # natural death
      deathprop * P.V_1_p,  # natural death
      deathprop * P.V_1_np,  # natural death
      deathprop * P.V_2_p,  # natural death
      deathprop * P.V_2_np,  # natural death
      deathprop * P.V_3_p,  # natural death
      deathprop * P.V_3_np,  # natural death
      deathprop * P.V_b_p,  # natural death
      deathprop * P.V_b_np,  # natural death
      deathprop * P.V_cb_p,  # natural death
      deathprop * P.V_cb_np,  # natural death
      deathprop * P.V_ab_p,  # natural death
      deathprop * P.V_ab_np,  # natural death
      deathprop * P.V_mm1_p,  # natural death
      deathprop * P.V_mm1_np,  # natural death
      deathprop * P.M1_p,  # natural death
      deathprop * P.M1_np,  # natural death
      deathprop * P.V_mm2_p,  # natural death
      deathprop * P.M2_p,  # natural death
      deathprop * P.V_mm3_p,  # natural death
      deathprop * P.M3_p,  # natural death
      lambda_P *poi_Ityp_P* P.S,  # incidence S->I_typ
      lambda_P *poi_Iatyps_P* P.S,  # incidence S->I_atyp_s
      lambda_P *poi_Iatypns_P* P.S,  # incidence S->I_atyp_ns
      lambda_P *poi_Ia_P* P.S,  # incidence S->I_a
      lambda_P *poi_Ityp_v_P* P.V_1_np,  # incidence V_1_np->I_typ
      lambda_P *poi_Iatyps_v_P* P.V_1_np,  # incidence V_1_np->I_atyp_s
      lambda_P *poi_Iatypns_v_P* P.V_1_np,  # incidence V_1_np->I_atyp_ns
      lambda_P *poi_Ia_v_P* P.V_1_np,  # incidence V_1_np->I_a
      lambda_P *poi_Ityp_v_P* P.V_2_np,  # incidence V_2_np->I_typ
      lambda_P *poi_Iatyps_v_P* P.V_2_np,  # incidence V_2_np->I_atyp_s
      lambda_P *poi_Iatypns_v_P* P.V_2_np,  # incidence V_2_np->I_atyp_ns
      lambda_P *poi_Ia_v_P* P.V_2_np,  # incidence V_2_np->I_a
      lambda_P *poi_Ityp_v_P* P.V_3_np,  # incidence V_3_np->I_typ
      lambda_P *poi_Iatyps_v_P* P.V_3_np,  # incidence V_3_np->I_atyp_s
      lambda_P *poi_Iatypns_v_P* P.V_3_np,  # incidence V_3_np->I_atyp_ns
      lambda_P *poi_Ia_v_P* P.V_3_np,  # incidence V_3_np->I_a
      lambda_P *poi_Ityp_v_P* P.V_b_np,  # incidence V_b_np->I_typ
      lambda_P *poi_Iatyps_v_P* P.V_b_np,  # incidence V_b_np->I_atyp_s
      lambda_P *poi_Iatypns_v_P* P.V_b_np,  # incidence V_b_np->I_atyp_ns
      lambda_P *poi_Ia_v_P* P.V_b_np,  # incidence V_b_np->I_a
      lambda_P *poi_Ityp_v_P* P.V_cb_np,  # incidence V_cb_np->I_typ
      lambda_P *poi_Iatyps_v_P* P.V_cb_np,  # incidence V_cb_np->I_atyp_s
      lambda_P *poi_Iatypns_v_P* P.V_cb_np,  # incidence V_cb_np->I_atyp_ns
      lambda_P *poi_Ia_v_P* P.V_cb_np,  # incidence V_cb_np->I_a
      lambda_P *poi_Ityp_v_P* P.V_ab_np,  # incidence V_ab_np->I_typ
      lambda_P *poi_Iatyps_v_P* P.V_ab_np,  # incidence V_ab_np->I_atyp_s
      lambda_P *poi_Iatypns_v_P* P.V_ab_np,  # incidence V_ab_np->I_atyp_ns
      lambda_P *poi_Ia_v_P* P.V_ab_np,  # incidence V_ab_np->I_a
      lambda_P *poi_Ityp_v_P* P.V_mm1_np,  # incidence V_mm1_np->I_typ
      lambda_P *poi_Iatyps_v_P* P.V_mm1_np,  # incidence V_mm1_np->I_atyp_s
      lambda_P *poi_Iatypns_v_P* P.V_mm1_np,  # incidence V_mm1_np->I_atyp_ns
      lambda_P *poi_Ia_v_P* P.V_mm1_np,  # incidence V_mm1_np->I_a
      lambda_P *poi_Ityp_v_P* P.M1_np,  # incidence M1_np->I_typ
      lambda_P *poi_Iatyps_v_P* P.M1_np,  # incidence M1_np->I_atyp_s
      lambda_P *poi_Iatypns_v_P* P.M1_np,  # incidence M1_np->I_atyp_ns
      lambda_P *poi_Ia_v_P* P.M1_np,  # incidence M1_np->I_a
      (1-eff_w_P) * (1 - kappa_P) * lambda_P *poi_Ityp_v_P* P.W,  # incidence W->I_typ
      (1-eff_w_P) * (1 - kappa_P) * lambda_P *poi_Iatyps_v_P* P.W,  # incidence W->I_atyp_s
      (1-eff_w_P) * (1 - kappa_P) * lambda_P *poi_Iatypns_v_P* P.W,  # incidence W->I_atyp_ns
      (1-eff_w_P) * (1 - kappa_P) * lambda_P *poi_Ia_v_P* P.W,  # incidence W->I_a
      (1-eff_w_P ) * kappa_P *  lambda_P * P.W,  # boosting W->R
      gamma_Ityp_P * P.I_typ,  # recovery I_typ -> R
      gamma_Iatyp_s_P * P.I_atyp_s,  # recovery I_atyp_s -> R
      gamma_Iatyp_ns_P * P.I_atyp_ns,  # recovery I_atyp_ns -> R
      gamma_Ia_P * P.I_a,  # recovery I_a -> R
      mu_P * P.I_typ,  # pertussis death I_typ -> D
      mu_P * P.I_atyp_s,  # pertussis death I_atyp_s -> D
      mu_P * P.I_atyp_ns,  # pertussis death I_atyp_ns -> D
      mu_P * P.I_a,  # pertussis death I_a -> D
      tau_mi_P[tic] * P.V_mi_p,  # waning V_mi_p -> S
      tau_1_P[tic] * P.V_1_p,  # waning V_1_p -> W
      tau_2_P[tic] * P.V_2_p,  # waning V_2_p -> W
      tau_3_P[tic] * P.V_3_p,  # waning V_3_p -> W
      tau_b_P[tic] * P.V_b_p,  # waning V_b_p -> W
      tau_cb_P[tic] * P.V_cb_p,  # waning V_cb_p -> W
      tau_ab_P[tic] * P.V_ab_p,  # waning V_ab_p -> W
      tau_vmm_P[tic] * P.V_mm1_p,  # waning pregnancy V_mm1_p -> M1_p, history of maternal vaccination
      tau_vmm_P[tic] * P.V_mm1_np,  # waning pregnancy V_mm1_np -> M1_np, history of maternal vaccination
      tau_vmm_P[tic] * P.V_mm2_p,  # waning pregnancy V_mm2_p -> M2_p, history of maternal vaccination
      tau_vmm_P[tic] * P.V_mm3_p,  # waning pregnancy V_mm3_p -> M3_p, history of maternal vaccination
      tau_mm_P[tic] * P.M1_p,  # waning M1_p -> W
      tau_ab_P[tic] * P.M2_p,  # waning M2_p -> W
      tau_P * P.M3_p,  # waning M3_p -> W
      tau_P * P.R,  # waning R -> W
      agerate*(1-(cov1+covboost)*mov_P[,tic])*(1-deathprop) * P.S,  # ageing, not vaccinated
      agerate * (1-deathprop) * P.I_typ,  # ageing
      agerate * (1-deathprop) * P.I_atyp_s,  # ageing
      agerate * (1-deathprop) * P.I_atyp_ns,  # ageing
      agerate * (1-deathprop) * P.I_a,  # ageing
      agerate*(1-cov1*mov_P[,tic])*(1-deathprop) * P.V_mi_p,  # ageing, not vaccinated
      agerate*(1-(cov2+covboost)*mov_P[,tic])*(1-deathprop) * P.V_1_p,  # ageing, not vaccinated
      agerate*(1-(cov2+covboost)*mov_P[,tic])*(1-deathprop) * P.V_1_np,  # ageing, not vaccinated
      agerate*(1-(cov3+covboost)*mov_P[,tic])*(1-deathprop) * P.V_2_p,  # ageing, not vaccinated
      agerate*(1-(cov3+covboost)*mov_P[,tic])*(1-deathprop) * P.V_2_np,  # ageing, not vaccinated
      agerate*(1-covboost*mov_P[,tic])*(1-deathprop) * P.V_3_p,  # ageing, not vaccinated
      agerate*(1-covboost*mov_P[,tic])*(1-deathprop) * P.V_3_np,  # ageing, not vaccinated
      agerate*(1-(covcb+covab)*mov_P[,tic])*(1-deathprop) * P.V_b_p,  # ageing, not vaccinated
      agerate*(1-(covcb+covab)*mov_P[,tic])*(1-deathprop) * P.V_b_np,  # ageing, not vaccinated
      agerate*(1-covab*mov_P[,tic])*(1-deathprop) * P.V_cb_p,  # ageing, not vaccinated
      agerate*(1-covab*mov_P[,tic])*(1-deathprop) * P.V_cb_np,  # ageing, not vaccinated
      agerate*(1-deathprop) * P.V_ab_p,  # ageing, not vaccinated
      agerate*(1-deathprop) * P.V_ab_np,  # ageing, not vaccinated
      agerate*(1-covboost*mov_P[,tic])*(1-deathprop) * P.W,  # ageing, not vaccinated
      agerate*(1-(cov1+cov2+cov3+covboost)*mov_P[,tic])*(1-deathprop) * P.R,  # ageing, not vaccinated
      agerate * (1-deathprop) * P.V_mm1_p,  # ageing, not vaccinated
      agerate * (1-deathprop) * P.V_mm1_np,  # ageing, not vaccinated
      agerate * (1-deathprop) * P.M1_p,  # ageing, not vaccinated
      agerate * (1-deathprop) * P.M1_np,  # ageing, not vaccinated
      agerate * (1-deathprop) * P.V_mm2_p,  # ageing, not vaccinated
      agerate * (1-deathprop) * P.M2_p,  # ageing, not vaccinated
      agerate * (1-deathprop) * P.V_mm3_p,  # ageing, not vaccinated
      agerate * (1-deathprop) * P.M3_p,  # ageing, not vaccinated
      agerate*cov1*mov_P[,tic]*(eff_1_P[tic])*(1-deathprop) * P.S,  # ageing, vaccinated S -> V_1_p
      agerate*cov1*mov_P[,tic]*(1-eff_1_P[tic])*(1-deathprop) * P.S,  # ageing, vaccinated S -> V_1_np
      agerate*cov1*mov_P[,tic]*(1-deathprop) * P.V_mi_p,  # ageing, vaccinated V_mi_p -> V_1_p
      agerate*cov2*mov_P[,tic]*(1-deathprop) * P.V_1_p,  # ageing, vaccinated V_1_p -> V_2_p
      agerate*cov2*mov_P[,tic]*eff_2_P[tic]*(1-deathprop) * P.V_1_np,  # ageing, vaccinated V_1_np -> V_2_p
      agerate*cov2*mov_P[,tic]*(1-eff_2_P[tic])*(1-deathprop) * P.V_1_np,  # ageing, vaccinated V_1_np -> V_2_np
      agerate*cov3*mov_P[,tic]*(1-deathprop) * P.V_2_p,  # ageing, vaccinated V_2_p -> V_3_p
      agerate*cov3*mov_P[,tic]*eff_3_P[tic]*(1-deathprop) * P.V_2_np,  # ageing, vaccinated V_2_np -> V_3_p
      agerate*cov3*mov_P[,tic]*(1-eff_3_P[tic])*(1-deathprop) * P.V_2_np,  # ageing, vaccinated V_2_np -> V_3_np
      agerate*covb*mov_P[,tic]*(1-deathprop) * P.V_3_p,  # ageing, vaccinated V_3_p -> V_b_p
      agerate*covb*mov_P[,tic]*eff_b_P[tic]*(1-deathprop) * P.V_3_np,  # ageing, vaccinated V_3_np -> V_b_p
      agerate*covb*mov_P[,tic]*(1-eff_b_P[tic])*(1-deathprop) * P.V_3_np,  # ageing, vaccinated V_3_np -> V_b_np
      agerate*covcb*mov_P[,tic]*(1-deathprop) * P.V_b_p,  # ageing, vaccinated V_b_p -> V_cb_p
      agerate*covcb*mov_P[,tic]*eff_cb_P[tic]*(1-deathprop) * P.V_b_np,  # ageing, vaccinated V_b_np -> V_cb_p
      agerate*covcb*mov_P[,tic]*(1-eff_cb_P[tic])*(1-deathprop) * P.V_b_np,  # ageing, vaccinated V_b_np -> V_cb_np
      agerate*covab*mov_P[,tic]*(1-deathprop) * P.V_cb_p,  # ageing, vaccinated V_cb_p -> V_ab_p
      agerate*covab*mov_P[,tic]*eff_ab_P[tic]*(1-deathprop) * P.V_cb_np,  # ageing, vaccinated V_cb_np -> V_ab_p
      agerate*covab*mov_P[,tic]*(1-eff_ab_P[tic])*(1-deathprop) * P.V_cb_np,  # ageing, vaccinated V_cb_np -> V_ab_np
      agerate*covb*mov_P[,tic]*(eff_b_P[tic])*(1-deathprop) * P.S,  # ageing, vaccinated S -> V_b_p
      agerate*covb*mov_P[,tic]*(1-eff_b_P[tic])*(1-deathprop) * P.S,  # ageing, vaccinated S -> V_b_np
      agerate*covcb*mov_P[,tic]*(eff_cb_P[tic])*(1-deathprop) * P.S,  # ageing, vaccinated S -> V_cb_p
      agerate*covcb*mov_P[,tic]*(1-eff_cb_P[tic])*(1-deathprop) * P.S,  # ageing, vaccinated S -> V_cb_np
      agerate*covab*mov_P[,tic]*(eff_ab_P[tic])*(1-deathprop) * P.S,  # ageing, vaccinated S -> V_ab_p
      agerate*covab*mov_P[,tic]*(1-eff_ab_P[tic])*(1-deathprop) * P.S,  # ageing, vaccinated S -> V_ab_np
      agerate*covb*mov_P[,tic]*(1-deathprop) * P.W,  # ageing, vaccinated W -> V_b_p
      agerate*covcb*mov_P[,tic]*(1-deathprop) * P.W,  # ageing, vaccinated W -> V_cb_p
      agerate*covab*mov_P[,tic]*(1-deathprop) * P.W,  # ageing, vaccinated W -> V_ab_p
      agerate*cov1*mov_P[,tic]*(1-deathprop) * P.R,  # ageing, vaccinated R -> R(1)
      agerate*cov2*mov_P[,tic]*(1-deathprop) * P.R,  # ageing, vaccinated R -> R(2)
      agerate*cov3*mov_P[,tic]*(1-deathprop) * P.R,  # ageing, vaccinated R -> R(3)
      agerate*covb*mov_P[,tic]*(1-deathprop) * P.R,  # ageing, vaccinated R -> R(b)
      agerate*covcb*mov_P[,tic]*(1-deathprop) * P.R,  # ageing, vaccinated R -> R(cb)
      agerate*covab*mov_P[,tic]*(1-deathprop) * P.R,  # ageing, vaccinated R -> R(ab)
      agerate*covb*mov_P[,tic]*(1-deathprop) * P.V_1_p,  # ageing, vaccinated V_1_p -> V_b_p
      agerate*covb*mov_P[,tic]*eff_b_P[tic]*(1-deathprop) * P.V_1_np,  # ageing, vaccinated V_1_np -> V_b_p
      agerate*covb*mov_P[,tic]*(1-eff_b_P[tic])*(1-deathprop) * P.V_1_np,  # ageing, vaccinated V_1_np -> V_b_np
      agerate*covcb*mov_P[,tic]*(1-deathprop) * P.V_1_p,  # ageing, vaccinated V_1_p -> V_cb_p
      agerate*covcb*mov_P[,tic]*eff_cb_P[tic]*(1-deathprop) * P.V_1_np,  # ageing, vaccinated V_1_np -> V_cb_p
      agerate*covcb*mov_P[,tic]*(1-eff_cb_P[tic])*(1-deathprop) * P.V_1_np,  # ageing, vaccinated V_1_np -> V_cb_np
      agerate*covab*mov_P[,tic]*(1-deathprop) * P.V_1_p,  # ageing, vaccinated V_1_p -> V_ab_p
      agerate*covab*mov_P[,tic]*eff_ab_P[tic]*(1-deathprop) * P.V_1_np,  # ageing, vaccinated V_1_np -> V_ab_p
      agerate*covab*mov_P[,tic]*(1-eff_ab_P[tic])*(1-deathprop) * P.V_1_np,  # ageing, vaccinated V_1_np -> V_ab_np
      agerate*covb*mov_P[,tic]*(1-deathprop) * P.V_2_p,  # ageing, vaccinated V_2_p -> V_b_p
      agerate*covb*mov_P[,tic]*eff_b_P[tic]*(1-deathprop) * P.V_2_np,  # ageing, vaccinated V_2_np -> V_b_p
      agerate*covb*mov_P[,tic]*(1-eff_b_P[tic])*(1-deathprop) * P.V_2_np,  # ageing, vaccinated V_2_np -> V_b_np
      agerate*covcb*mov_P[,tic]*(1-deathprop) * P.V_2_p,  # ageing, vaccinated V_2_p -> V_cb_p
      agerate*covcb*mov_P[,tic]*eff_cb_P[tic]*(1-deathprop) * P.V_2_np,  # ageing, vaccinated V_2_np -> V_cb_p
      agerate*covcb*mov_P[,tic]*(1-eff_cb_P[tic])*(1-deathprop) * P.V_2_np,  # ageing, vaccinated V_2_np -> V_cb_np
      agerate*covab*mov_P[,tic]*(1-deathprop) * P.V_2_p,  # ageing, vaccinated V_2_p -> V_ab_p
      agerate*covab*mov_P[,tic]*eff_ab_P[tic]*(1-deathprop) * P.V_2_np,  # ageing, vaccinated V_2_np -> V_ab_p
      agerate*covab*mov_P[,tic]*(1-eff_ab_P[tic])*(1-deathprop) * P.V_2_np,  # ageing, vaccinated V_2_np -> V_ab_np
      agerate*covcb*mov_P[,tic]*(1-deathprop) * P.V_3_p,  # ageing, vaccinated V_3_p -> V_cb_p
      agerate*covcb*mov_P[,tic]*eff_cb_P[tic]*(1-deathprop) * P.V_3_np,  # ageing, vaccinated V_3_np -> V_cb_p
      agerate*covcb*mov_P[,tic]*(1-eff_cb_P[tic])*(1-deathprop) * P.V_3_np,  # ageing, vaccinated V_3_np -> V_cb_np
      agerate*covab*mov_P[,tic]*(1-deathprop) * P.V_3_p,  # ageing, vaccinated V_3_p -> V_ab_p
      agerate*covab*mov_P[,tic]*eff_ab_P[tic]*(1-deathprop) * P.V_3_np,  # ageing, vaccinated V_3_np -> V_ab_p
      agerate*covab*mov_P[,tic]*(1-eff_ab_P[tic])*(1-deathprop) * P.V_3_np,  # ageing, vaccinated V_3_np -> V_ab_np
      agerate*covab*mov_P[,tic]*(1-deathprop) * P.V_b_p,  # ageing, vaccinated V_b_p -> V_ab_p
      agerate*covab*mov_P[,tic]*eff_ab_P[tic]*(1-deathprop) * P.V_b_np,  # ageing, vaccinated V_b_np -> V_ab_p
      agerate*covab*mov_P[,tic]*(1-eff_ab_P[tic])*(1-deathprop) * P.V_b_np,  # ageing, vaccinated V_b_np -> V_ab_np
      mrate*movm_P[,tic] * P.V_cb_p,  # Maternal vaccinated V_cb_p -> V_mm2_p
      mrate*movm_P[,tic] * eff_m_P[tic] * P.V_cb_np,  # Maternal vaccinated V_cb_np -> V_mm1_p
      mrate*movm_P[,tic] * (1-eff_m_P[tic]) * P.V_cb_np,  # Maternal vaccinated V_cb_np -> V_mm1_np
      mrate*movm_P[,tic] * P.V_ab_p,  # Maternal vaccinated V_ab_p -> V_mm2_p
      mrate*movm_P[,tic] * eff_m_P[tic] * P.V_ab_np,  # Maternal vaccinated V_ab_np -> V_mm1_p
      mrate*movm_P[,tic] * (1-eff_m_P[tic]) * P.V_ab_np,  # Maternal vaccinated V_ab_np -> V_mm1_np
      mrate*movm_P[,tic] * eff_m_P[tic] * P.S,  # Maternal vaccinated S -> V_mm1_p
      mrate*movm_P[,tic] * (1-eff_m_P[tic]) * P.S,  # Maternal vaccinated S -> V_mm1_np
      mrate*movm_P[,tic] * eff_w_P * P.W,  # Maternal vaccinated W -> V_mm2_p
      mrate*movm_P[,tic] * (1-eff_w_P) * P.W,  # Maternal vaccinated W -> V_mm1_p
      mrate*movm_P[,tic] * P.R  # Maternal vaccinated R -> V_mm3_p
    ), dim=c(N, 193))
    tranrate <- c(t(tranrate))
    # T2 <- Sys.time()
    return(tranrate)
  })
}

# POST PROCESSING function
# Assumes dfAge/N/traind_P/varind_P exists
postproc.P  <- function(parameters, out, tran) {
  with(as.list(c(parameters)), {
    # ************************************************************************************* #
    # for outputting the  time series for each patch
    # ************************************************************************************* #
    ### Case outputs ####
    # REMEMBER to modify BOTH commented sections 1 and 2 below:
    # 1 - Add your postproc variable names
    
    postprocVars <- vars(popa,
                         prev_P,
                         all_Inc_P,
                         clin_Inc_P,
                         clin_Inc100k_P,
                         treat_Inc_P,
                         treat_Inc100k_P,
                         outp_Inc_P,
                         outp_Inc100k_P,
                         inp_Inc_P,
                         inp_Inc100k_P,
                         deaths_P,
                         deaths100k_P,
                         rep_Clin_P,
                         rep_Clin100k_P,
                         rep_deaths_P,
                         rep_deaths100k_P,
                         doses_1_P,
                         doses_2_P,
                         doses_3_P,
                         doses_b_P,
                         doses_cb_P,
                         doses_ab_P,
                         doses_m_P,
                         protected_P)
    
    
    postprocVarNames <- postprocVars %>% sapply(rlang::as_name)
    postprocVarList <- lapply(postprocVarNames, function(varName){
      matrix(0, nrow = length(out[, 1]), ncol = N)
    })
    names(postprocVarList) <- postprocVarNames
    
    # incTransitions_Ityp <- rownames(traind_P)[rownames(traind_P) %>% str_detect('^inc.*Ityp$')]
    # incTransitions_Iatyps <- rownames(traind_P)[rownames(traind_P) %>% str_detect('^inc.*Iatyps$')]
    # incTransitions_Iatypns <- rownames(traind_P)[rownames(traind_P) %>% str_detect('^inc.*Iatypns$')]
    # incTransitions_Ia <- rownames(traind_P)[rownames(traind_P) %>% str_detect('^inc.*Ia$')]
    #browser()
    allincTransitions <- tbTransitions_P %>% filter(To%in%c('P.I_typ[n]', 'P.I_atyp_s[n]', 'P.I_atyp_ns[n]', 'P.I_a[n]')) %>% pull(id)
    
    clinincTransitions <- tbTransitions_P %>% filter(To%in%c('P.I_typ[n]', 'P.I_atyp_s[n]', 'P.I_atyp_ns[n]')) %>% pull(id)
    sevincTransitions <- tbTransitions_P %>% filter(To%in%c('P.I_typ[n]', 'P.I_atyp_s[n]')) %>% pull(id)
    mildincTransitions <- tbTransitions_P %>% filter(To=='P.I_atyp_ns[n]') %>% pull(id)
    infCompartments <- rownames(varind_P)[rownames(varind_P) %>% str_starts('P.I')]
    deathTransitions <- tbTransitions_P %>% filter(To=='P.D[n]') %>% pull(id)
    
    doses_1Transitions <- tbTransitions_P %>% filter(To%in%c('P.V_1_p[nxt]', 'P.V_1_np[nxt]') | TransitionName == 'ageV_R_R(1)', TransitionName %>% str_starts('ageV')) %>% pull(id)
    doses_2Transitions <- tbTransitions_P %>% filter(To%in%c('P.V_2_p[nxt]', 'P.V_2_np[nxt]') | TransitionName == 'ageV_R_R(2)', TransitionName %>% str_starts('ageV')) %>% pull(id)
    doses_3Transitions <- tbTransitions_P %>% filter(To%in%c('P.V_3_p[nxt]', 'P.V_3_np[nxt]') | TransitionName == 'ageV_R_R(3)', TransitionName %>% str_starts('ageV')) %>% pull(id)
    doses_bTransitions <- tbTransitions_P %>% filter(To%in%c('P.V_b_p[nxt]', 'P.V_b_np[nxt]') | TransitionName == 'ageV_R_R(b)', TransitionName %>% str_starts('ageV')) %>% pull(id)
    doses_cbTransitions <- tbTransitions_P %>% filter(To%in%c('P.V_cb_p[nxt]', 'P.V_cb_np[nxt]') | TransitionName == 'ageV_R_R(cb)', TransitionName %>% str_starts('ageV')) %>% pull(id)
    doses_abTransitions <- tbTransitions_P %>% filter(To%in%c('P.V_ab_p[nxt]', 'P.V_ab_np[nxt]') | TransitionName == 'ageV_R_R(ab)', TransitionName %>% str_starts('ageV')) %>% pull(id)
    doses_mTransitions <- tbTransitions_P %>% filter(str_starts(TransitionComment,'Maternal vaccinated')) %>% pull(id)

    protectCompartments <- c(rownames(varind_P)[rownames(varind_P) %>% str_ends('_p')], 'P.R')
    
    
    for (n in 1:N) {
      
      # 2 - Fill variables with values for each N
      popa <- rowSums(out[,c(varind_P[c(alivepop_P),n])+1])  # Alive population
      postprocVarList$popa[, n]  <- popa
      postprocVarList$prev_P[, n] <- rowSums(out[, c(varind_P[infCompartments, n])+1]) # Prevalence

      postprocVarList$all_Inc_P[, n]  <- rowSums(tran[, unname(traind_P[allincTransitions, n])] / 365)     # All Incidence
      postprocVarList$clin_Inc_P[, n]  <- rowSums(tran[, unname(traind_P[clinincTransitions, n])] / 365)     # Clinical Incidence
      postprocVarList$clin_Inc100k_P[, n]  <- (rowSums(tran[, unname(traind_P[clinincTransitions, n])])/popa*100000)/365     # Clinical Incidence per 100k
      postprocVarList$outp_Inc_P[, n]  <- p_mt_P*rowSums(tran[, unname(traind_P[mildincTransitions, n])] / 365)     # Outpatients Incidence
      postprocVarList$outp_Inc100k_P[, n]  <- p_mt_P*(rowSums(tran[, unname(traind_P[mildincTransitions, n])])/popa*100000)/365     # Outpatients Incidence per 100k
      postprocVarList$inp_Inc_P[, n]  <- p_st_P*rowSums(tran[, unname(traind_P[sevincTransitions, n])] / 365)     # Inpatients Incidence
      postprocVarList$inp_Inc100k_P[, n]  <- p_st_P*(rowSums(tran[, unname(traind_P[sevincTransitions, n])])/popa*100000)/365     # Inpatients  per 100k
      
      postprocVarList$treat_Inc_P[, n]      <-  postprocVarList$outp_Inc_P[, n] + postprocVarList$inp_Inc_P[, n]     # Treated Incidence
      postprocVarList$treat_Inc100k_P[, n]  <-  postprocVarList$outp_Inc100k_P[, n] + postprocVarList$inp_Inc100k_P[, n]   # Treated Incidence per 100k
      
      postprocVarList$rep_Clin_P[, n]  <- p_rep_clin_P*rowSums(tran[, unname(traind_P[clinincTransitions, n])] / 365)     # Reported Clinical Incidence
      postprocVarList$rep_Clin100k_P[, n]  <- p_rep_clin_P*(rowSums(tran[, unname(traind_P[clinincTransitions, n])])/popa*100000)/365     # Reported Clinical Incidence per 100k
      
      postprocVarList$deaths_P[, n]  <- rowSums(tran[, unname(traind_P[deathTransitions, n])] / 365)     # Disease-related deaths incidence
      postprocVarList$deaths100k_P[, n]  <- (rowSums(tran[, unname(traind_P[deathTransitions, n])])/popa*100000)/365     # Disease-related deaths Incidence per 100k
      postprocVarList$rep_deaths_P[, n]  <- p_rep_death_P*rowSums(tran[, unname(traind_P[deathTransitions, n])] / 365)     # Reported Disease-related deaths incidence
      postprocVarList$rep_deaths100k_P[, n]  <- p_rep_death_P*(rowSums(tran[, unname(traind_P[deathTransitions, n])])/popa*100000)/365     # Reported Disease-related deaths Incidence per 100k
      
      postprocVarList$doses_1_P[, n]   <- rowSums(tran[, traind_P[doses_1Transitions, n]] / 365)     # No. of primary series 1 doses
      postprocVarList$doses_2_P[, n]   <- rowSums(tran[, traind_P[doses_2Transitions, n]] / 365)     # No. of primary series 2 doses
      postprocVarList$doses_3_P[, n]   <- rowSums(tran[, traind_P[doses_3Transitions, n]] / 365)     # No. of primary series 3 doses
      postprocVarList$doses_b_P[, n]   <- rowSums(tran[, traind_P[doses_bTransitions, n]] / 365)     # No. of infant booster doses
      postprocVarList$doses_cb_P[, n]  <- rowSums(tran[, traind_P[doses_cbTransitions, n]] / 365)     # No. of childhood booster doses
      postprocVarList$doses_ab_P[, n]  <- rowSums(tran[, traind_P[doses_abTransitions, n]] / 365)     # No. of adolescent booster doses
      postprocVarList$doses_m_P[, n]   <- rowSums(tran[, traind_P[doses_mTransitions, n]] / 365)      # No. of maternal doses 
      
      postprocVarList$protected_P[, n] <- rowSums(out[, c(varind_P[protectCompartments, n])+1]) # Prevalence of protection
      
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
epiModel.P <- function(t, state, parameters) {
  # rates of change
  transit <- disrates.P(state, parameters, t)
  EQ(dZ, transit, transitions_P[,1], transitions_P[,3], transitions_P[,2], transitions_P[,4])

  list(c(dZ))
}
  
makeInitialConditionsCode.P <- function(mtMod_P, parameters, coverage_table, ISO3 = 'ZAF') {

  chr = as.character
  sy = startyear
  cov1 <- coverage_table %>% pull(DTPCV1, name=Year)
  cov2 <- coverage_table %>% pull(DTPCV2, name=Year)
  cov3 <- coverage_table %>% pull(DTPCV3, name=Year)
  eff1 <- first(parameters$eff_1_P)
  eff2 <- first(parameters$eff_2_P)
  eff3 <- first(parameters$eff_3_P)
  
  gbdPrev <- readRDS('models/data/tbGbdPrev.rds') %>%
    filter(iso3==ISO3, disease=="Whooping cough") %>%
    pull(value, name=age_group)
  
  LOG('makeInitialConditionsCode.P(sy={sy}, ISO3={ISO3})')
  pop <- getPopData(ISO3, year=sy) %>% pull(popTot, name=age_group)
  initcond <- as_tibble(expand_grid(compartment=tbCompartments_P$State, age_group=names(pop))) %>%
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
        .x$P.V_3_p <- case_when(age_yrs==0 ~ 0,
                                age_yrs==6 ~ 0,
                                TRUE ~ eff3*cov3[[chr(sy-age_yrs)]])
        .x$P.V_3_np <- case_when(age_yrs==0 ~ 0,
                                 age_yrs==6 ~ mean(cov3[chr(sy-5:15)]),
                                 TRUE ~ (1-eff3)*cov3[[chr(sy-age_yrs)]])
        .x$P.V_2_p <- case_when(age_yrs==0 ~ 0,
                                age_yrs==6 ~ 0,
                                TRUE ~ eff2*(cov2[[chr(sy-age_yrs)]]-cov3[[chr(sy-age_yrs)]]))
        .x$P.V_2_np <- case_when(age_yrs==0 ~ 0,
                                 age_yrs==6 ~ mean(cov2[chr(sy-5:15)]-cov3[chr(sy-5:15)]),
                                 TRUE ~ (1-eff2)*(cov2[[chr(sy-age_yrs)]]-cov3[[chr(sy-age_yrs)]]))
        .x$P.V_1_p <- case_when( age_yrs==0 ~ 0,
                                 age_yrs==6 ~ mean(cov1[chr(sy-5:15)]-cov2[chr(sy-5:15)]),
                                 TRUE ~ eff1*(cov1[[chr(sy-age_yrs)]]-cov2[[chr(sy-age_yrs)]]))
        .x$P.V_1_np <- case_when( age_yrs==0 ~ 0,
                                  age_yrs==6 ~ mean(cov1[chr(sy-5:15)]-cov2[chr(sy-5:15)]),
                                  TRUE ~ (1-eff1)*(cov1[[chr(sy-age_yrs)]]-cov2[[chr(sy-age_yrs)]]))
        .x$P.I_typ <- 1.5*gbdPrev[[as.character(age_group)]]
        .x$P.I_a <- 2*gbdPrev[[as.character(age_group)]]
        
        # .x$P.V_mm1_p <- 0.33*with(parameters, femProp[ageId]*fertProp[ageId]*covANC)
        # .x$P.M1_p <- 0.33*with(parameters, femProp[ageId]*fertProp[ageId]*covANC)
        # .x$P.V_mm2_p <- 0.33*with(parameters, femProp[ageId]*fertProp[ageId]*covANC)
        # .x$P.M2_p <- 0.33*with(parameters, femProp[ageId]*fertProp[ageId]*covANC)
        # .x$P.V_mm3_p <- 0.33*with(parameters, femProp[ageId]*fertProp[ageId]*covANC)
        # .x$P.M3_p <- 0.33*with(parameters, femProp[ageId]*fertProp[ageId]*covANC)
        if (sum(.x)>1) {
          warning("btw, sum(.x)>1")
          .x <- .x %>% mutate(across(.fns=function(col){col/sum(.x)}))
        }
        .x$P.S <- 0.85*pmax(0,1-sum(.x))
        .x$P.W <- 0.05*pmax(0,1-sum(.x))        
        .x$P.R <- 0.10*pmax(0,1-sum(.x))        
        .x
      })
    }) %>% ungroup() %>% select(age_group, starts_with('P.'))
  #browser()
  initcond %>%
    mutate(pop=unname(pop), across(where(is.numeric),.fns = ~.x*pop), pop=pop^0.5) %>%
    pivot_longer(!c(age_group,pop)) %>%
    pull(value) %>% unname()
}

# RUN FUNCTION  ####

run_model.P <- function(parameters, initialConditions, timesteps,
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
  dZ <<- as.numeric(rep(0.0, N*nrow(tbCompartments_P)))
  LOG("Solving the ODE for Pertussis with ptrans={parameters$ptrans_P}", LEVEL$TRACE)
  outoderun <- ode(y=initialConditions, times=timesteps, func=epiModel.P, #method = "vode", 
                   parms=parameters)
  LOG("Pertussis solver diagnostics: {diagnostics(outoderun)}", LEVEL$TRACE)
  
  # Return just the raw model output:
  if(returnRawModel && !returnPostprocessedModel) {
    LOG("Returning *just* the raw model output object...")
    return(outoderun)
  }
  
  LOG("Calculating transition values for Pertussis now", LEVEL$TRACE)
  # Compute transitions at each time step
  tranoderun <- matrix(0,
                       nrow=length(timesteps),
                       ncol=N*nrow(tbTransitions_P))
  for (ti in seq_along(timesteps)) {
    compartmentValues <- outoderun[ti,2:(N*nrow(tbCompartments_P)+1)]
    tranoderun[ti,]<-t(disrates.P(x=compartmentValues,
                                  parameters=parameters,
                                  t=timesteps[[ti]]))
  }
  # Could be useful to have a tibble here?
  # tbTranRates <- lapply(seq_along(timesteps), function(ti) {
  #     compartmentValues <- outoderun[ti,1+seq(N*nrow(tbCompartments_P))]
  #     transitionRates <- disrates.P(x=compartmentValues,
  #                                   parameters=parameters,
  #                                   t=0)
  #     matTransitionRates <- matrix(transitionRates, nrow=nrow(tbTransitions_P), ncol=N, byrow = F)
  #     matTransitionRates %>%
  #       as_tibble() %>%
  #       rowid_to_column('transitionIndex') %>%
  #       pivot_longer(!transitionIndex, names_to = 'age_group') %>% 
  # mutate(time=timesteps[[ti]])
  #     }) %>%
  #   bind_rows()
  #Compute outputs
  LOG("Postprocessing Pertussis", LEVEL$TRACE)
  ppout <- postproc.P(parameters, outoderun, tranoderun) #daily model outpupt in defined terms
  
  DISoutage <- ppout %>% 
    mutate(disease="Pertussis",
           unit = "Daily") #Daily outputs by age group

  # DISoutage2 <- DISoutage %>% #Prevalence proportion calcs
  #  postProcSummarize.P()
  
  # DISoutall<-DISoutage %>%  #Daily outputs for all ages
  #   postProcSummarizeAges.P()
  
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
  
  DISoutu1yr <- postProcSummarizeAgesYear.P(DISoutage %>% filter(age_group %btwn% c("0-5wks", "11mths"))) %>% 
    mutate(age_group='<1yo')  
  
  DISoutu5yr <- postProcSummarizeAgesYear.P(DISoutage %>% filter(age_group %btwn% c("0-5wks", "4yrs"))) %>% 
    mutate(age_group='<5yo')
  
  DISout5_9yr <- postProcSummarizeAgesYear.P(DISoutage %>% filter(age_group %btwn% c("5yrs", "9yrs"))) %>% 
    mutate(age_group='5-9yo')
  
  DISout10_14yr <- postProcSummarizeAgesYear.P(DISoutage %>% filter(age_group %btwn% c("10yrs", "14yrs"))) %>% 
    mutate(age_group='10-14yo')
  
  DISout10_12yr <- postProcSummarizeAgesYear.P(DISoutage %>% filter(age_group %btwn% c("10yrs", "12yrs"))) %>% 
    mutate(age_group='10-12yo')
  
  DISoutu15yr <- postProcSummarizeAgesYear.P(DISoutage %>% filter(age_group %btwn% c("0-5wks", "14yrs"))) %>% 
    mutate(age_group='<15yo')
  
  DISout15_49yr <- postProcSummarizeAgesYear.P(DISoutage %>% filter(age_group %btwn% c("15yrs", "45-49yrs"))) %>% 
    mutate(age_group='15-49yo')
  
  DISout50_75yr <- postProcSummarizeAgesYear.P(DISoutage %>% filter(age_group %btwn% c("50-54yrs", "75+"))) %>% 
    mutate(age_group='50-75+yo')
  
  DISoutallyr<-postProcSummarizeAgesYear.P(DISoutage)  #Annual outputs for all ages
  
  DISoutageyr<-postProcSummarizeYear.P(DISoutage) #Annual outputs by age group
  
  DISoutyr <- bind_rows(DISoutageyr, DISoutallyr, DISoutu1yr, DISoutu5yr, DISout5_9yr, DISout10_14yr, DISoutu15yr, DISout15_49yr, DISout50_75yr) %>% 
    
    mutate(disease="Pertussis",
           unit = "Annual") %>% 
    select(year, age_group, variable, disease, value, unit)
  
  LOG("Completed Pertussis, returning tibble with unit=Annual", LEVEL$TRACE)
  
  moPostprocessing = list(DISoutyr)
  if (returnRawModel && returnPostprocessedModel) {
    LOG("Returning moRaw and moPostprocessing for Pertussis")
    return(list(moRaw=outoderun,
                moPostprocessing=moPostprocessing))
  } else {
    LOG("Returning *just* moPostprocessing for Pertussis")
    return(moPostprocessing)
  }
}


postProcSummarize.P <- function(DISoutage) {
  
  popa<- DISoutage %>% 
    filter(variable =="popa") %>% 
    rename(popa = value)
  
  DISoutage %>%
    filter(variable%in%c('prev_P', 'protected_P')) %>% 
    ungroup() %>% 
    left_join((popa %>% select(time,age_group, popa)), by=c("time", "age_group")) %>% 
    mutate(value = value/popa) %>% 
    select(time, variable,age_group, value, disease, unit) %>%
    transmute(time=time,
              age_group = age_group,
              variable=str_replace(variable,'_P','_prop_P'),
              value = value,
              disease=disease, 
              unit=unit) 
}

postProcSummarizeAges.P <- function(DISoutage) {
  
  pop<- DISoutage %>% 
    filter(variable =="popa") %>% 
    group_by(time, variable, disease) %>% 
    summarise(pop = sum(value), .groups = 'drop_last') %>% 
    mutate(variable = "pop")
  
  sum1<- DISoutage %>%
    filter(str_ends(variable, '100k_P', negate = T) ) %>% 
    group_by(time, variable, disease) %>%
    summarise(value = sum(value), .groups = 'drop_last') 
  
  
  sum2<- sum1 %>% 
    left_join((pop %>% select(time,pop)), by=c("time")) %>% 
    mutate(per100k = value/pop*100000) %>% 
    transmute(time=time,
              variable=str_replace(variable.x,'_P','100k_P'),
              disease=disease,
              value=per100k) %>% 
    filter(!variable%in%c('popa'))
  
  
  sum3<- sum1 %>% 
    filter(variable%in%c('prev_P', 'protected_P')) %>%
    left_join((pop %>% select(time,pop)), by=c("time")) %>% 
    mutate(value = value/pop) %>% 
    transmute(time=time,
              variable=str_replace(variable.x,'_P','_prop_P'),
              value = value,disease=disease) %>% 
    filter(!variable%in%c('popa')) %>% 
    select(time, variable, value, disease)
  
  
  bind_rows(sum1, sum2, sum3, (pop %>% rename(value = pop))) %>% mutate(age_group = "All",
                                                                        unit = "Daily")
}

postProcSummarizeAgesYear.P <- function(DISoutage) {
  
  popyr<- DISoutage %>% 
    filter(variable =="popa") %>% 
    group_by(time, variable, disease) %>% 
    summarise(pop = sum(value), .groups = 'drop') %>% 
    mutate(variable = "pop") %>%  
    group_by(year=floor(time)) %>% 
    summarise(pop = mean(pop), .groups = 'drop_last') #average annual value
  
  sum1yr<- DISoutage %>%
    filter(str_ends(variable, '100k_P', negate = T) ) %>% 
    group_by(time, variable, disease) %>%
    summarise(value = sum(value), .groups = 'drop') %>%
    group_by(year=floor(time), variable, disease) %>% 
    summarise(value = sum(value), .groups = 'drop_last') %>% 
    filter(!variable%in%c('popa', 'prev_P', 'protect_P')) 
  
  sum2yr<- sum1yr %>% 
    left_join(popyr, by=c("year")) %>%
    mutate(per100k = value/pop*100000) %>% 
    transmute(year=year,
              variable=str_replace(variable,'_P','100k_P'),
              disease=disease,
              value=per100k)
  
  sum3yr<- DISoutage %>%
    filter(variable %in% c('prev_P', 'protected_P') ) %>% 
    group_by(time, variable, disease) %>%
    summarise(value = sum(value), .groups = 'drop_last') %>%
    group_by(year=floor(time), variable, disease) %>% 
    summarise(value = mean(value), .groups = 'drop') %>% 
    ungroup() %>% 
    left_join((popyr %>% select(year, pop)), by=c("year")) %>% 
    mutate(value = value/pop) %>%
    select(year, variable, value, disease) %>%
    transmute(year=year,
              variable=str_replace(variable,'_P','_prop_P'),
              value = value,
              disease=disease)
  
  sum4yr = popyr %>%
    transmute(year, variable='pop_P', value=pop, disease='Pertussis')
  
  bind_rows(sum1yr, sum2yr, sum3yr, sum4yr) %>% mutate(age_group = "All",
                                                       unit = "Annual")
}

postProcSummarizeYear.P <- function(DISoutage) {
  
  popyr<- DISoutage %>% 
    filter(variable =="popa") %>% 
    group_by(year=floor(time), age_group, variable, disease) %>% 
    summarise(pop = mean(value), .groups = 'drop_last') #average annual value
  
  sum1yr<- DISoutage %>%
    filter(str_ends(variable, '100k_P', negate = T) ) %>% 
    group_by(year=floor(time),age_group, variable, disease) %>%
    summarise(value = sum(value), .groups = 'drop_last') %>% 
    filter(!variable%in%c('popa', 'prev_P', 'protected_P'))
  
  sum2yr<- sum1yr %>% 
    ungroup() %>% 
    left_join(popyr, by=c("year", "age_group", "disease")) %>%
    mutate(per100k = value/pop*100000) %>%
    transmute(year=year,
              age_group = age_group,
              variable=str_replace(variable.x,'_P','100k_P'),
              disease=disease,
              value=per100k)
  
  sum3yr<- DISoutage %>%
    filter(variable %in% c('prev_P', 'protected_P') ) %>% 
    group_by(year=floor(time),age_group, variable, disease) %>%
    summarise(value = mean(value), .groups = 'drop') %>%
    left_join((popyr %>% select(year, age_group,  pop)), by=c("year", 'age_group')) %>% 
    mutate(value = value/pop) %>% 
    select(year, age_group, variable.x, value, disease) %>% 
    transmute(year=year,
              age_group = age_group,
              variable=str_replace(variable.x,'_P','_prop_P'),
              value = value,
              disease=disease) 
  
  sum4yr = popyr %>%
    transmute(year=year,
              age_group=age_group,
              variable='pop_P',
              value=pop,
              disease='Pertussis')
  
  bind_rows(sum1yr, sum2yr, sum3yr, sum4yr) %>% mutate(unit = "Annual")
}


# Plotting Functions ####
# Postprocessing
pltPostProc.P  <- function(mo, var='inc_pred') {
  mo %>%
    mutate(Date=lubridate::date_decimal(time)) %>% 
    filter(variable==var) %>% 
    ggplot() +
    aes(x = Date, y=value) +
    geom_line(size = 1.5) +
    theme_minimal() +
    facet_wrap(vars(as_factor(age_group)),scales = 'free_y') +
    labs(title=as.character(glue::glue("Pertussis {var} plot per age group")))
}

pltPostProc_yr.P  <- function(mo, var='inc_pred') {
  mo %>%
    mutate(Date=lubridate::date_decimal(year)) %>% 
    filter(variable==var) %>% 
    ggplot() +
    aes(x = Date, y=value) +
    geom_col(size = 1.5) +
    theme_minimal() +
    facet_wrap(vars(as_factor(age_group)),scales = 'free_y') +
    labs(title=as.character(glue::glue("Pertussis {var} plot per age group")))
}
