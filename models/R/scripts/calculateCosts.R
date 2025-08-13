# Once the model has been run, we want to calculate the eco stuff using the model outputs (baseline and each scenario) as well as costs_params and various shinyInputs

calculateCostsScenario <- function(moAnnual, costInputs, analysisRange, discountRate) {
  if (discountRate<0 || discountRate > 0.1) { # unlikely user wants to discount over 10%, probably they put in a 
    warning("Discounting should be a positive number between 0 and 1")
  }
  
  ppData <- moAnnual %>%
    filter(year %between% analysisRange,
           age_group=='All') %>%
    select(year,variable,value) %>%
    pivot_wider(names_from = variable,values_from=value) %>%
    as.list()
  # Calculate intro costs:
  tbDosesAll <- moAnnual %>%
    filter(year %between% analysisRange) %>%
    filter(str_starts(variable, 'doses_i')) %>%
    filter(value!=0)

  cost_intro <- rep_along(x = 0, along=ppData$year)
  if (nrow(tbDosesAll)>0) {
    introCostYears <- tbDosesAll %>%
      group_by(variable) %>% 
      summarise(year=min(year), .groups = 'drop') %>% 
      pull(year) %>%
      unique()
    # TODO: Consider if this is/isn't within analysisRange?
    if (as.logical(costInputs$strat_intro_applyonce)) {
      introCostYears <- min(introCostYears)
    }
    cost_intro[ppData$year %in% introCostYears] <- costInputs$strat_intro_cost
  }
  
  cost_intro_fin = cost_intro * costInputs$strat_intro_cost_pfin
  
  # Calculate other costs
  with(c(as.list(costInputs), ppData), {
    
    #### Delivery cost per dose administered ####
    del_cpd_1	<-  del_hf_ps * del_cpd_hf_ps + del_o_ps * del_cpd_o_ps	  # Delivery cost per dose of primary series 1 vaccination (existing)
    del_cpd_2	<-  del_hf_ps * del_cpd_hf_ps + del_o_ps * del_cpd_o_ps	  # Delivery cost per dose of primary series 2 vaccination (existing)
    del_cpd_3	<-  del_hf_ps * del_cpd_hf_ps + del_o_ps * del_cpd_o_ps	  # Delivery cost per dose of primary series 3 vaccination (existing)
    del_cpd_b	<-  del_hf_b  * del_cpd_hf_b +  del_o_b * del_cpd_o_b	    # Delivery cost per dose of infant booster vaccination (existing)
    del_cpd_cb <-	del_hf_cb * del_cpd_hf_cb + del_o_cb * del_cpd_o_cb   # Delivery cost per dose of childhood booster vaccination (existing)
    del_cpd_ab <-	del_hf_ab * del_cpd_hf_ab + del_o_ab * del_cpd_o_ab	  # Delivery cost per dose of adolescent booster vaccination (existing)
    del_cpd_m	<-  del_hf_m  * del_cpd_hf_m +  del_o_m * del_cpd_o_m	    # Delivery cost per dose of maternal vaccination (existing)
    del_cpd_ib <-	del_hf_ib * del_cpd_hf_ib + del_o_ib * del_cpd_o_ib	  # Delivery cost per dose of infant booster vaccination (intervention)
    del_cpd_icb	<-del_hf_icb* del_cpd_hf_icb +del_o_icb * del_cpd_o_icb	# Delivery cost per dose of childhood booster vaccination (intervention)
    del_cpd_iab	<-del_hf_iab* del_cpd_hf_iab +del_o_iab * del_cpd_o_iab	# Delivery cost per dose of adolescent booster vaccination (intervention)
    del_cpd_im <-	del_hf_im * del_cpd_hf_im + del_o_im * del_cpd_o_im   # Delivery cost per dose of maternal vaccination (intervention)
    
    #### Delivery financial cost per dose administered ####
    del_cpd_1_fin	  <-  del_pfin_ps  * del_cpd_1		 # Delivery cost per dose of primary series 1 vaccination (existing)
    del_cpd_2_fin	  <-  del_pfin_ps  * del_cpd_2		 # Delivery cost per dose of primary series 2 vaccination (existing)
    del_cpd_3_fin	  <-  del_pfin_ps  * del_cpd_3		 # Delivery cost per dose of primary series 3 vaccination (existing)
    del_cpd_b_fin	  <-  del_pfin_b   * del_cpd_b		 # Delivery cost per dose of infant booster vaccination (existing)
    del_cpd_cb_fin  <-	del_pfin_cb  * del_cpd_cb	 # Delivery cost per dose of childhood booster vaccination (existing)
    del_cpd_ab_fin  <-	del_pfin_ab  * del_cpd_ab	 # Delivery cost per dose of adolescent booster vaccination (existing)
    del_cpd_m_fin	  <-  del_pfin_m   * del_cpd_m		 # Delivery cost per dose of maternal vaccination (existing)
    del_cpd_ib_fin  <-	del_pfin_ib  * del_cpd_ib	 # Delivery cost per dose of infant booster vaccination (intervention)
    del_cpd_icb_fin	<-  del_pfin_icb * del_cpd_icb  # Delivery cost per dose of childhood booster vaccination (intervention)
    del_cpd_iab_fin	<-  del_pfin_iab * del_cpd_iab	 # Delivery cost per dose of adolescent booster vaccination (intervention)
    del_cpd_im_fin  <-	del_pfin_im  * del_cpd_im   # Delivery cost per dose of maternal vaccination (intervention)
    
    #### Cost per dose administered ####
    cpd_1	<- unit_cpd_ps + del_cpd_1	        # Existing Schedule: Cost per dose administered of primary series 1 = unit cost + delivery cost (no intro costs)
    cpd_2	<- unit_cpd_ps + del_cpd_2	        # Existing Schedule: Cost per dose administered of primary series 2 = unit cost + delivery cost (no intro costs)
    cpd_3	<- unit_cpd_ps + del_cpd_3	        # Existing Schedule: Cost per dose administered of primary series 3 = unit cost + delivery cost (no intro costs)
    cpd_b	<- unit_cpd_b + del_cpd_b	          # Existing Schedule: Cost per dose administered of infant booster = unit cost + delivery cost (no intro costs)
    cpd_cb	<- unit_cpd_cb + del_cpd_cb	      # Existing Schedule: Cost per dose administered of childhood booster = unit cost + delivery cost (no intro costs)
    cpd_ab	<- unit_cpd_ab + del_cpd_ab	      # Existing Schedule: Cost per dose administered of adolescent booster = unit cost + delivery cost (no intro costs)
    cpd_m	<- unit_cpd_m + del_cpd_m	          # Existing Schedule: Cost per dose administered of maternal vaccination = unit cost + delivery cost (no intro costs)
    cpd_ib	<- unit_cpd_ib + del_cpd_ib	      # Intervention: Cost per dose administered of infant booster = unit cost + delivery cost (no intro costs)
    cpd_icb	<- unit_cpd_icb + del_cpd_icb	    # Intervention: Cost per dose administered of childhood booster = unit cost + delivery cost (no intro costs)
    cpd_iab	<- unit_cpd_iab + del_cpd_iab	    # Intervention: Cost per dose administered of adolescent booster = unit cost + delivery cost (no intro costs)
    cpd_im	<- unit_cpd_im + del_cpd_im	      # Intervention: Cost per dose administered of maternal vaccination = unit cost + delivery cost (no intro costs)
    
    #### Financial cost per dose administered ####
    cpd_1_fin	  <- unit_pfin_ps * unit_cpd_ps + del_cpd_1_fin	        # Existing Schedule: Cost per dose administered of primary series 1 = unit cost + delivery cost (no intro costs) (financial costs)
    cpd_2_fin	  <- unit_pfin_ps * unit_cpd_ps + del_cpd_2_fin	        # Existing Schedule: Cost per dose administered of primary series 2 = unit cost + delivery cost (no intro costs) (financial costs)
    cpd_3_fin	  <- unit_pfin_ps * unit_cpd_ps + del_cpd_3_fin	        # Existing Schedule: Cost per dose administered of primary series 3 = unit cost + delivery cost (no intro costs) (financial costs)
    cpd_b_fin	  <- unit_pfin_b * unit_cpd_b + del_cpd_b_fin	          # Existing Schedule: Cost per dose administered of infant booster = unit cost + delivery cost (no intro costs) (financial costs)
    cpd_cb_fin	<- unit_pfin_cb * unit_cpd_cb + del_cpd_cb_fin	      # Existing Schedule: Cost per dose administered of childhood booster = unit cost + delivery cost (no intro costs) (financial costs)
    cpd_ab_fin	<- unit_pfin_ab * unit_cpd_ab + del_cpd_ab_fin	      # Existing Schedule: Cost per dose administered of adolescent booster = unit cost + delivery cost (no intro costs) (financial costs)
    cpd_m_fin	  <- unit_pfin_m * unit_cpd_m + del_cpd_m_fin	          # Existing Schedule: Cost per dose administered of maternal vaccination = unit cost + delivery cost (no intro costs) (financial costs)
    cpd_ib_fin	<- unit_pfin_ib * unit_cpd_ib + del_cpd_ib_fin	      # Intervention: Cost per dose administered of infant booster = unit cost + delivery cost (no intro costs) (financial costs)
    cpd_icb_fin	<- unit_pfin_icb * unit_cpd_icb + del_cpd_icb_fin	    # Intervention: Cost per dose administered of childhood booster = unit cost + delivery cost (no intro costs) (financial costs)
    cpd_iab_fin	<- unit_pfin_iab * unit_cpd_iab + del_cpd_iab_fin	    # Intervention: Cost per dose administered of adolescent booster = unit cost + delivery cost (no intro costs) (financial costs)
    cpd_im_fin	<- unit_pfin_im * unit_cpd_im + del_cpd_im_fin	      # Intervention: Cost per dose administered of maternal vaccination = unit cost + delivery cost (no intro costs) (financial costs)
    
    
    #### Annual cost of vaccination by dose type ####
    cost_1	<- doses_1 * cpd_1	         # Existing schedule: Annual cost of primary series 1 
    cost_2	<- doses_2 * cpd_2	         # Existing schedule: Annual cost of primary series 2
    cost_3	<- doses_3 * cpd_3	         # Existing schedule: Annual cost of primary series 3
    cost_b	<- doses_b * cpd_b	         # Existing schedule: Annual cost of infant booster
    cost_cb	<- doses_cb * cpd_cb	       # Existing schedule: Annual cost of childhood booster
    cost_ab	<- doses_ab * cpd_ab	       # Existing schedule: Annual cost of adolescent booster
    cost_m	<- doses_m * cpd_m	         # Existing schedule: Annual cost of maternal vaccination
    cost_ib	<- doses_ib * cpd_ib	       # Intervention: Annual cost of infant booster
    cost_icb	<- doses_icb * cpd_icb     # Intervention: Annual cost of childhood booster
    cost_iab	<- doses_iab * cpd_iab	   # Intervention: Annual cost of adolescent booster
    cost_im	<- doses_im * cpd_im	       # Intervention: Annual cost of maternal vaccination
    
    #### Annual financial cost of vaccination by dose type ####
    cost_1_fin	  <- doses_1 * cpd_1_fin	      # Existing schedule: Annual cost of primary series 1   (Financial Costs)
    cost_2_fin	  <- doses_2 * cpd_2_fin	      # Existing schedule: Annual cost of primary series 2 (Financial Costs)
    cost_3_fin	  <- doses_3 * cpd_3_fin	      # Existing schedule: Annual cost of primary series 3 (Financial Costs)
    cost_b_fin	  <- doses_b * cpd_b_fin	      # Existing schedule: Annual cost of infant booster (Financial Costs)
    cost_cb_fin	  <- doses_cb * cpd_cb_fin	    # Existing schedule: Annual cost of childhood booster (Financial Costs)
    cost_ab_fin	  <- doses_ab * cpd_ab_fin	    # Existing schedule: Annual cost of adolescent booster  (Financial Costs)
    cost_m_fin	  <- doses_m * cpd_m_fin	      # Existing schedule: Annual cost of maternal vaccination (Financial Costs)
    cost_ib_fin	  <- doses_ib * cpd_ib_fin	    # Intervention: Annual cost of infant booster (Financial Costs)
    cost_icb_fin	<- doses_icb * cpd_icb_fin    # Intervention: Annual cost of childhood booster  (Financial Costs)
    cost_iab_fin	<- doses_iab * cpd_iab_fin	  # Intervention: Annual cost of adolescent booster  (Financial Costs)
    cost_im_fin	  <- doses_im * cpd_im_fin	    # Intervention: Annual cost of maternal vaccination (Financial Costs)
    
    zeros <- rep_along(0, along=cost_intro)
    if(0==length(cost_1))  {cost_1	 <- zeros}
    if(0==length(cost_2))  {cost_2	 <- zeros}
    if(0==length(cost_3))  {cost_3	 <- zeros}
    if(0==length(cost_b))  {cost_b	 <- zeros}
    if(0==length(cost_cb)) {cost_cb	 <- zeros}
    if(0==length(cost_ab)) {cost_ab	 <- zeros}
    if(0==length(cost_m))  {cost_m	 <- zeros}
    if(0==length(cost_ib)) {cost_ib	 <- zeros}
    if(0==length(cost_icb)){cost_icb <- zeros}
    if(0==length(cost_iab)){cost_iab <- zeros}
    if(0==length(cost_im)) {cost_im	 <- zeros}
    if(0==length(cost_1_fin))  {cost_1_fin	 <- zeros}
    if(0==length(cost_2_fin))  {cost_2_fin	 <- zeros}
    if(0==length(cost_3_fin))  {cost_3_fin	 <- zeros}
    if(0==length(cost_b_fin))  {cost_b_fin	 <- zeros}
    if(0==length(cost_cb_fin)) {cost_cb_fin	 <- zeros}
    if(0==length(cost_ab_fin)) {cost_ab_fin	 <- zeros}
    if(0==length(cost_m_fin))  {cost_m_fin	 <- zeros}
    if(0==length(cost_ib_fin)) {cost_ib_fin	 <- zeros}
    if(0==length(cost_icb_fin)){cost_icb_fin <- zeros}
    if(0==length(cost_iab_fin)){cost_iab_fin <- zeros}
    if(0==length(cost_im_fin)) {cost_im_fin	 <- zeros}
    
    #### Total cost of vaccination ####
    costVacc 	        <- cost_1 + cost_2 + cost_3 + cost_b + cost_cb + cost_ab + cost_m + cost_ib + cost_icb + cost_iab + cost_im  #	Annual cost of vaccination without Intro costs (by scenario)
    costVacc_wIntro 	<- costVacc + cost_intro #	Annual cost of vaccination Including Intro costs (by scenario)
    
    costVacc_fin 	        <- cost_1_fin + cost_2_fin + cost_3_fin + cost_b_fin + cost_cb_fin + cost_ab_fin + cost_m_fin + cost_ib_fin + cost_icb_fin + cost_iab_fin + cost_im  #	Annual cost of vaccination without Intro costs (by scenario)
    costVacc_wIntro_fin 	<- costVacc_fin + cost_intro_fin #  Annual cost of vaccination Including Intro costs (by scenario)
    
    
    # ************************************************************************************* #
    #### COST OF ILLNESS ####
    # ************************************************************************************* #
    tot_outp_coi_P	<- outp_Inc_P * mean(outp_cost_P)	             # 	Total cost of illness OP: Total annual cost of outpatient cases (pertussis)
    tot_outp_coi_T	<- outp_Inc_T * mean(outp_cost_T)	             # 	Total cost of illness OP: Total annual cost of outpatient cases (tetanus)
    tot_outp_coi_D	<- outp_Inc_D * mean(outp_cost_D)	             # 	Total cost of illness OP: Total annual cost of outpatient cases (diphtheria)
    tot_inp_coi_P	<- inp_Inc_P * mean(inp_cost_P)	               # 	Total cost of illness IP: Total annual cost of inpatient cases (pertussis)
    tot_inp_coi_T	<- inp_Inc_T * mean(inp_cost_T)	               # 	Total cost of illness IP: Total annual cost of inpatient cases (tetanus)
    tot_inp_coi_D	<- inp_Inc_D * mean(inp_cost_D)	               # 	Total cost of illness IP: Total annual cost of inpatient cases (diphtheria)
    tot_coi_P	<- tot_outp_coi_P + tot_inp_coi_P	         # 	Total cost of illness P: Total annual cost of pertussis cases (OP + IP)
    tot_coi_T	<- tot_outp_coi_T + tot_inp_coi_T	         # 	Total cost of illness T: Total annual cost of tetanus cases (OP + IP)
    tot_coi_D	<- tot_outp_coi_D + tot_inp_coi_D	         # 	Total cost of illness D: Total annual cost of diphtheria cases (OP + IP)
    tot_coi_DTP	<- tot_coi_P + tot_coi_T + tot_coi_D	   # 	Total cost of illness DTP: Total annual cost of illness for pertussis, tetanus and diphtheria combined
    
    
    # ************************************************************************************* #
    #### TOTAL COSTS (VACCINATION + ILLNESS) ####
    # ************************************************************************************* #
    # Costs
    costTot_DTP	<-	costVacc_wIntro + tot_coi_DTP	     # 	DTP: Total cost of vaccination and illness (treatment)
    costTot_P	<-	costVacc_wIntro + tot_coi_P	         # 	Pertussis: Total cost of vaccination (all) and cost illness (pertussis)
    costTot_T	<-	costVacc_wIntro + tot_coi_T	         # 	Tetanus: Total cost of vaccination (all) and cost illness (tetanus)
    costTot_D	<-	costVacc_wIntro + tot_coi_D	         # 	Diphtheria: Total cost of vaccination (all) and cost illness (diphtheria)
    # Financial costs
    costTot_DTP_fin	<-	costVacc_wIntro_fin + tot_coi_DTP	     # 	DTP: Total financial cost of vaccination and illness (treatment)
    costTot_P_fin	  <-	costVacc_wIntro_fin + tot_coi_P	       # 	Pertussis: Total financial cost of vaccination (all) and cost illness (pertussis)
    costTot_T_fin	  <-	costVacc_wIntro_fin + tot_coi_T	       # 	Tetanus: Total financial cost of vaccination (all) and cost illness (tetanus)
    costTot_D_fin	  <-	costVacc_wIntro_fin + tot_coi_D	       # 	Diphtheria: Total financial cost of vaccination (all) and cost illness (diphtheria)
    
    
    # ************************************************************************************* #
    #### ECONOMIC OUTPUTS ####
    # ************************************************************************************* #
    
    #### Annual cost of vaccination by dose type ####
    clin_Inc_DTP	<-	clin_Inc_P + clin_Inc_T + clin_Inc_D	   # 	Total clinical cases DTP, annual value																							
    deaths_DTP	<-	deaths_P + deaths_T + deaths_D	           # 	Total deaths DTP, annual value
    
    Undiscounted = list(deaths_DTP=deaths_DTP,
         deaths_D=deaths_D,
         deaths_T=deaths_T,
         deaths_P=deaths_P,
         clin_Inc_DTP=clin_Inc_DTP,
         clin_Inc_D=clin_Inc_D,
         clin_Inc_T=clin_Inc_T,
         clin_Inc_P=clin_Inc_P,
         costTot_DTP=costTot_DTP,
         costTot_D=costTot_D,
         costTot_T=costTot_T,
         costTot_P=costTot_P,
         costTot_DTP_fin=costTot_DTP_fin,	
         costTot_D_fin=costTot_D_fin,  
         costTot_T_fin=costTot_T_fin,  
         costTot_P_fin=costTot_P_fin,  
         tot_coi_DTP=tot_coi_DTP,
         tot_coi_D=tot_coi_D,
         tot_coi_T=tot_coi_T,
         tot_coi_P=tot_coi_P,
         costVacc_wIntro=costVacc_wIntro,
         costVacc=costVacc,
         cost_intro=cost_intro,
         costVacc_wIntro_fin=costVacc_wIntro_fin,
         costVacc_fin=costVacc_fin,
         cost_intro_fin=cost_intro_fin,
         cost_1=cost_1,
         cost_2=cost_2,
         cost_3=cost_3,
         cost_b=cost_b,
         cost_cb=cost_cb,
         cost_ab=cost_ab,
         cost_m=cost_m,
         cost_ib=cost_ib,
         cost_icb=cost_icb,
         cost_iab=cost_iab,
         cost_im=cost_im,
         cost_1_fin=cost_1_fin,
         cost_2_fin=cost_2_fin,
         cost_3_fin=cost_3_fin,
         cost_b_fin=cost_b_fin,
         cost_cb_fin=cost_cb_fin,
         cost_ab_fin=cost_ab_fin,
         cost_m_fin=cost_m_fin,
         cost_ib_fin=cost_ib_fin,
         cost_icb_fin=cost_icb_fin,
         cost_iab_fin=cost_iab_fin,
         cost_im_fin=cost_im_fin
    )
    
    Discounted = Undiscounted %>% 
      # apply discounting
      map(~(1+discountRate)^(first(analysisRange)-year)*.x)
    
    result = list(Undiscounted = Undiscounted,
                  Discounted = Discounted,
                  year = year)
    result
  })
}

calculateCostsComparison <- function(costsBaseline, costsScenario) {
  # Only process values where they exist in both cost inputs:
  years = base::intersect(costsBaseline$year, costsScenario$year)
  yearsBaseline = (costsBaseline$year %in% years)
  yearsScenario = (costsScenario$year %in% years)
  result = lapply(list(Discounted="Discounted", Undiscounted="Undiscounted"), function(disc){
    # Define delta for years common to both cost inputs:
    delta <- function(varname) {
      varname <- as.character(ensyms(varname)[[1]])
      result <- costsScenario[[disc]][[varname]][yearsScenario] - costsBaseline[[disc]][[varname]][yearsBaseline]
      # cat(crayon::magenta(paste0(varname,': ',paste0(result,sep='',collapse=', '),'\n')))
      # cat(' ')
      # cat(glue("{crayon::blue(costsScenario[[varname]][yearsScenario])} - {crayon::red(costsBaseline[[varname]][yearsBaseline])}","\n\n"))
      # cat(crayon::magenta(paste0(rep('-', 20),collapse='')))
      result
    }
    # Create lists for the various elements, in specific order so intermediate results can inform further results:
    nets <- list(
      #### Annual net costs ####
      net_cost_intro = delta(cost_intro),	     # 	Net cost of vaccination introduction
      net_costVacc = delta(costVacc),	         # 	Net cost of vaccination (excl introduction)
      net_costVacc_wIntro = delta(costVacc_wIntro), # 	Net cost of vaccination (Inl introduction)
      
      net_tot_coi_DTP = delta(tot_coi_DTP),	   # 	Net cost of illness (DTP): Difference between scenario and baseline, annual value
      net_tot_coi_P = delta(tot_coi_P),	     # 	Net cost of illness (pertussis): Difference between scenario and baseline, annual value
      net_tot_coi_T = delta(tot_coi_T),	     # 	Net cost of illness (tetanus): Difference between scenario and baseline, annual value
      net_tot_coi_D = delta(tot_coi_D),	     # 	Net cost of illness (diphtheria),: Difference between scenario and baseline, annual value
      
      net_costTot_DTP = delta(costTot_DTP),	       # 	Total net cost (DTP),: Difference between scenario and baseline, annual value
      net_costTot_P = delta(costTot_P),	     # 	Total net cost (pertussis),: Difference between scenario and baseline, annual value
      net_costTot_T = delta(costTot_T),	     # 	Total net cost (tetanus),: Difference between scenario and baseline, annual value
      net_costTot_D = delta(costTot_D),	     # 	Total net cost (diphtheria),: Difference between scenario and baseline, annual value
      # Financial
      net_cost_intro_fin = delta(cost_intro_fin),	     # 	Net cost of vaccination introduction
      net_costVacc_fin = delta(costVacc_fin),	         # 	Net cost of vaccination (excl introduction)
      net_costVacc_wIntro_fin = delta(costVacc_wIntro_fin), # 	Net cost of vaccination (Inl introduction)
      
      net_costTot_DTP_fin = delta(costTot_DTP_fin),	 # 	Total net cost (DTP),: Difference between scenario and baseline, annual value
      net_costTot_P_fin = delta(costTot_P_fin),	     # 	Total net cost (pertussis),: Difference between scenario and baseline, annual value
      net_costTot_T_fin = delta(costTot_T_fin),	     # 	Total net cost (tetanus),: Difference between scenario and baseline, annual value
      net_costTot_D_fin = delta(costTot_D_fin),	     # 	Total net cost (diphtheria),: Difference between scenario and baseline, annual value
      
      #### Total net costs (over full model time horizon), ####

      #### Impact (clinical cases and deaths averted), ####
      avert_clin_DTP = (-1)*delta(clin_Inc_DTP),	   # 		Clinical cases averted (DTP),: Annual number of clinical cases averted compared to baseline
      avert_death_DTP = (-1)*delta(deaths_DTP),	       # 		Deaths averted (DTP),: Annual number of deaths averted compared to baseline
      #avert_daly_DTP = (-1)*delta(daly_DTP),	           # 		DALYs averted (DTP),: Annual number of DALYs averted compared to baseline
      avert_clin_P = (-1)*delta(clin_Inc_P),	         # 		Clinical cases averted (pertussis),: Annual number of clinical cases averted compared to baseline
      avert_death_P = (-1)*delta(deaths_P),	             # 		Deaths averted (pertussis),: Annual number of deaths averted compared to baseline
      #avert_daly_P = (-1)*delta(daly_P),	                 # 		DALYs averted (pertussis),: Annual number of DALYs averted compared to baseline
      avert_clin_T = (-1)*delta(clin_Inc_T),	         # 		Clinical cases averted (tetanus),: Annual number of clinical cases averted compared to baseline
      avert_death_T = (-1)*delta(deaths_T),	             # 		Deaths averted (tetanus),: Annual number of deaths averted compared to baseline
      #avert_daly_T = (-1)*delta(daly_T),	                 # 		DALYs averted (tetanus),: Annual number of DALYs averted compared to baseline
      avert_clin_D = (-1)*delta(clin_Inc_D),	         # 		Clinical cases averted (diphtheria),: Annual number of clinical cases averted compared to baseline
      avert_death_D = (-1)*delta(deaths_D)	             # 		Deaths averted (diphtheria),: Annual number of deaths averted compared to baseline
      #avert_daly_D = (-1)*delta(daly_D)	                 # 		DALYs averted (diphtheria): Annual number of DALYs averted compared to baseline
    )
    icers <- with(nets, {list(
      #### ICERs for cases and deaths averted ####
      icer_clin_DTP	= sum(net_costTot_DTP)/sum(avert_clin_DTP),	     #	All: Cost per clinical case averted
      icer_death_DTP = sum(net_costTot_DTP)/sum(avert_death_DTP),	 #	All: Cost per death averted
      #icer_daly_DTP = sum(net_costTot_DTP)/sum(avert_daly_DTP),	     #	All: Cost per DALY averted
      icer_clin_P = sum(net_costTot_P)/sum(avert_clin_P),	       #	Pertussis: Cost per clinical case averted
      icer_death_P = sum(net_costTot_P)/sum(avert_death_P),	   #	Pertussis: Cost per death averted
      #icer_daly_P = sum(net_costTot_P)/sum(avert_daly_P),	       #	Pertussis: Cost per DALY averted
      icer_clin_T = sum(net_costTot_T)/sum(avert_clin_T),	       #	Tetanus: Cost per clinical case averted
      icer_death_T = sum(net_costTot_T)/sum(avert_death_T),	   #	Tetanus: Cost per death averted
      #icer_daly_T = sum(net_costTot_T)/sum(avert_daly_T),	       #	Tetanus: Cost per DALY averted
      icer_clin_D = sum(net_costTot_D)/sum(avert_clin_D),	       #	Diphtheria: Cost per clinical case averted
      icer_death_D = sum(net_costTot_D)/sum(avert_death_D)	   #	Diphtheria: Cost per death averted
      # icer_daly_D = sum(net_costTot_D)/sum(avert_daly_D)	       #	Diphtheria: Cost per DALY averted
    )})
    icers_fin <- with(nets, {list(
      #### ICERs for cases and deaths averted ####
      icer_clin_DTP_fin	= sum(net_costTot_DTP_fin)/sum(avert_clin_DTP),	 #	All: Cost per clinical case averted
      icer_death_DTP_fin = sum(net_costTot_DTP_fin)/sum(avert_death_DTP),#	All: Cost per death averted
      #icer_daly_DTP_fin = sum(net_costTot_DTP_fin)/sum(avert_daly_DTP), #	All: Cost per DALY averted
      icer_clin_P_fin = sum(net_costTot_P_fin)/sum(avert_clin_P),	       #	Pertussis: Cost per clinical case averted
      icer_death_P_fin = sum(net_costTot_P_fin)/sum(avert_death_P),	     #	Pertussis: Cost per death averted
      #icer_daly_P_fin = sum(net_costTot_P_fin)/sum(avert_daly_P),	     #	Pertussis: Cost per DALY averted
      icer_clin_T_fin = sum(net_costTot_T_fin)/sum(avert_clin_T),	       #	Tetanus: Cost per clinical case averted
      icer_death_T_fin = sum(net_costTot_T_fin)/sum(avert_death_T),	     #	Tetanus: Cost per death averted
      #icer_daly_T_fin = sum(net_costTot_T_fin)/sum(avert_daly_T),	     #	Tetanus: Cost per DALY averted
      icer_clin_D_fin = sum(net_costTot_D_fin)/sum(avert_clin_D),	       #	Diphtheria: Cost per clinical case averted
      icer_death_D_fin = sum(net_costTot_D_fin)/sum(avert_death_D)	     #	Diphtheria: Cost per death averted
      # icer_daly_D_fin = sum(net_costTot_D_fin)/sum(avert_daly_D)	     #	Diphtheria: Cost per DALY averted
    )})
    c(nets, icers, icers_fin)
  })
  # Note: as per `?c`, passing lists returns a list - the result will be a flat list.
  c(list(year=costsScenario$year), result)
}

makeCostsTable <- function(costs, scenarios, DIS=NULL, currency="USD",
                           isFinancial=F, isDiscounted=F,
                           tbMapping = tibble::tribble(
                             ~variableSimple,          ~pretty,
                             "costTot",                "Total cost",
                             "costVacc_wIntro",        "Cost of vaccination",
                             "tot_coi",                "Cost of illness",
                             "cost_ib",  "Cost of infant booster",
                             "cost_icb", "Cost of child booster",
                             "cost_iab", "Cost of adolescent booster",
                             "cost_im",  "Cost of maternal"#,
                             # "clin_Inc",               "Clinical cases",
                             # "deaths",                 "Deaths",
                             # "icer_clin", "Cost per clinical case averted",
                             # "icer_death",         "Cost per death averted"
                           )) {
  # tbMapping is a tibble with `variableSimple` and `pretty`, eg tot_coi and "Cost of illness"
  if (is.null(DIS)) {
    DIS <- c('All','DTP','Diphtheria','Tetanus','Pertussis')
  }
    
  # Total cost	Cost of vaccination	Cost of illness	Clinical cases	Deaths	Cost per clinical case averted	Cost per death averted
  
  getDiseaseName <- function(variable) {
    re = '_[DTP]{1,3}(_fin)?$'
    dis = str_match(string = variable, pattern = '_([DTP]{1,3})(_fin)?') %>% `[`(, 2)
    case_match(dis,
               'D'~'Diphtheria',
               'T'~'Tetanus',
               'P'~'Pertussis',
               'DTP'~'DTP',
               .default = 'All')
  }
  
  getVariableSimple <- function(variable) {
    re = '(_[DTP]{1,3})?(_fin)?$'
    str_replace(variable, pattern = re,'')
  }
  
  mm <- tibble(diseaseDst=c('DTP','Diphtheria','Tetanus','Pertussis')) %>% mutate(disease='All')
  tbMappingWithFin <- tbMapping %>% mutate(variableSimple = paste0(variableSimple,'_fin'))
  tbMappingBoth <- bind_rows(tbMapping, tbMappingWithFin)
  
  resultSummary = costs$resultSummary %>%
    filter(discounting==if(isDiscounted){"Discounted"}else{"Undiscounted"}) %>%
    select(!discounting)
  
  finVarNames <- resultSummary %>% filter(str_ends(variable, '_fin')) %>% pull(variable) %>% unique()
  finCostsVarNamesToExclude <- if(isFinancial) {
    finVarNames %>% str_replace('_fin$','')
  } else {
    finVarNames
  }

  tblCostsBasic <- resultSummary %>% 
    rowwise() %>% 
    mutate(disease=getDiseaseName(variable),
           variableSimple=getVariableSimple(variable)) %>% 
    left_join(tbMappingBoth, by = "variableSimple") %>% 
    ungroup() %>%
    # Either throw out economic costs or financial costs:
    filter(! (variable %in% finCostsVarNamesToExclude))
  
  tblCosts <- tblCostsBasic %>%
    select(type,disease,scenario,pretty,value) %>% 
    filter(!is.na(pretty)) %>% 
    distinct() %>%
    ungroup() %>% 
    left_join(mm, by = "disease", relationship = "many-to-many") %>%
    filter(scenario %in% scenarios,
           disease %in% DIS) %>% 
    mutate(disease=if_else(is.na(diseaseDst),disease,diseaseDst),
           type=if_else(type=='Comparison','Scenario',type),
           pretty=factor(pretty,levels=unique(tbMapping$pretty)),
           disease=factor(disease,levels=mm$diseaseDst)) %>% 
    select(!diseaseDst) %>% 
    arrange(pretty) %>% 
    pivot_wider(id_cols = c('type','disease','scenario'), names_from='pretty', values_from='value') %>% 
    arrange(disease,type) %>% 
    rename(Scenario=scenario, Disease=disease) %>%
    select(!type) %>%
    filter(Disease==last(DIS)) %>%
    select(!Disease) %>% 
    mutate(Scenario = fct_relevel(Scenario, scenarios)) %>% 
    arrange(Scenario)
  
  if ('Cost of illness' %in% colnames(tblCosts))
    tblCosts$`Cost of illness` <- if_else(tblCosts$`Cost of illness`!=0, tblCosts$`Cost of illness`, NA)
  
  myColDef <- colDef(na = "-",
                     format = colFormat(separators = TRUE, digits = 0))
  
  colnamesToDefine = intersect(tbMapping$pretty, colnames(tblCosts))
  columns = rep(list(myColDef), length(colnamesToDefine)) %>% setNames(.,colnamesToDefine)
  reactable(data = tblCosts, columns = columns, bordered = TRUE)
}

makeCostsPlot = function(costsComparisons, costs, scenario, 
                          DIS="DTP", currency="USD", 
                          isFinancial=F, isDiscounted=T) {
  
  net_costTot_DTP = if_else(isFinancial, 'net_costTot_DTP_fin', 'net_costTot_DTP')
  net_costVacc = if_else(isFinancial, 'net_costVacc_fin', 'net_costVacc')
  net_cost_intro = if_else(isFinancial, 'net_cost_intro_fin', 'net_cost_intro')
  
  disc_val = if_else(isDiscounted, 'Discounted', 'Undiscounted')
  y = costsComparisons[[scenario]][[disc_val]][c(
    net_costVacc,
    net_cost_intro,
    'net_tot_coi_DTP',
    net_costTot_DTP
  )]
  names(y) <- c(
    'net_costVacc',
    'net_cost_intro',
    'net_tot_coi_DTP',
    'net_costTot_DTP'
  )
  years = costsComparisons[[scenario]]$year
  
  plot_data <- y %>% as_tibble %>% 
    mutate(years=years) %>% 
    pivot_longer(c(net_costVacc, net_cost_intro, net_tot_coi_DTP)) %>%
    mutate(
      name = case_match(name, 
                        'net_costVacc' ~ "Cost of vaccination (routine delivery)",
                        'net_cost_intro' ~ "Cost of vaccine introduction (fixed)",
                        "net_tot_coi_DTP" ~ "Treatment costs averted",
                        .default = name
      )
    ) %>% 
    mutate(
      value = if_else(
        abs(value) < 10 ^ 6,
        signif(value, digits = 3),
        round(value / 1000) * 1000
      )
    ) %>%
    mutate(
      net_costTot_DTP = if_else(
        abs(net_costTot_DTP)  < 10 ^ 6,
        signif(net_costTot_DTP , digits = 3),
        round(net_costTot_DTP / 1000) * 1000
      )
    )
  
  # custom fill colours
  cols <- c(
    "Cost of vaccination (routine delivery)" = "#4477AA",
    "Cost of vaccine introduction (fixed)" = "#CC6677",
    "Treatment costs averted" = "#44AA99"
  )

  ggplot(plot_data) +
    aes(
      x = factor(years), y = value, fill = factor(name), group = 1,
      text = sprintf(
        glue::glue("{name}: %s<br>Year: %s<br>"),
        format(value, big.mark = ",", scientific = FALSE), 
        years
      )
    ) +
    geom_col() +
    geom_line(
      aes(
        y=net_costTot_DTP,
        text = sprintf(
          "Net total cost: %s<br>Year: %s<br>",
          format(net_costTot_DTP, big.mark = ",", scientific = FALSE), 
          years
        ),
        color = "Net total cost",
        fill = NULL
      ), 
      linewidth=1.05
    ) +
    scale_fill_manual(values = cols) +
    scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
    scale_color_manual(name = "", values = c("Net total cost" = "black")) +
    labs(
      title = glue::glue(
        "Annual costs (vaccination and treatment) compared to baseline ({currency})"
      ),
      x = NULL,
      y = NULL,
      fill = NULL
    ) +
    theme_minimal()
}


makeCostsPlot2 <- function(costs, scenarios=NULL, startYear=2023, currency="USD", isFinancial=F, isDiscounted=F) {
  tbData <- costs$resultAnnual
  if (!is.null(scenarios)) {
    tbData <- tbData %>% filter(scenario %in% scenarios)
    n <- tbData %>% pull(scenario) %>% unique() %>% length() # figure out how many scenarios there are and choose a name for the color palette
    palette_name <- case_when(
      n == 1 ~ "tol1dtp",
      n == 2 ~ "tol2dtp",
      n == 3 ~ "tol3dtp",
      n == 4 ~ "tol4dtp",
      n == 5 ~ "tol5dtp",
      n == 6 ~ "tol6dtp",
      n == 7 ~ "tol7dtp",
      n == 8 ~ "tol8dtp",
      n == 9 ~ "tol9dtp",
      n == 10 ~ "tol10dtp",
      TRUE ~ "tol21rainbow"
    )
  }
  costVacc_wIntro <- 'costVacc_wIntro'
  if (isFinancial) {
    costVacc_wIntro <- paste0(costVacc_wIntro, '_fin')
  }
  
  analysisRange = range(costs$resultAnnual$year)
  yearText = glue::glue("{analysisRange[[1]]} - {analysisRange[[2]]}")
 
  p <- tbData %>%
    filter(variable==costVacc_wIntro,
           discounting==if(isDiscounted){"Discounted"}else{"Undiscounted"},
           year>=startYear) %>%
    ggplot() +
    aes(x = year, y = value, color = scenario) +
    geom_line(aes(text = NULL),size=.5) +
    geom_point(aes(text = sprintf(
      "Value: %s<br>Year: %s<br>Scenario: %s<br>",
      format(value, big.mark = ",", scientific = FALSE),
      year,
      scenario
    )), size=1.5) +
    scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
    scale_colour_dtp_d(palette_name) +
    scale_x_continuous(breaks = analysisRange[1]:analysisRange[2]) +
    expand_limits(y = 0) +
    theme_minimal() +
    # theme(legend.position = "bottom") +
    labs(title=glue::glue("Total annual cost of vaccination, {yearText} ({currency})"), color="Scenario") +
    xlab('') +
    ylab('') 
  
  ggplotly(p, tooltip = c("text")) %>% 
    layout(
      legend = list(orientation = 'h')
    ) %>% 
    plotly::config(
      toImageButtonOptions = list(filename = "total-annual-cost-of-vaccination-plot")
    )
}

makeEpiTable <- function(epiEconResultSummary, DIS=NULL, scenarios=NULL,
                         tbMapping=tibble::tribble(
                           ~variableSimple, ~pretty,
                           "clin_Inc",      "Clinical cases (Total)",
                           "deaths",        "Deaths (Total)",
                           "avert_clin",    "Averted clinical cases**",
                           "avert_death",   "Averted deaths**"
                         )) {
  if (is.null(DIS)) DIS <- c('DTP','Diphtheria','Tetanus','Pertussis')
  if (is.null(scenarios)) scenarios <- unique(epiEconResultSummary$scenario)

  getDiseaseName <- function(variable) {
    re = '_[DTP]{1,3}(_fin)?$'
    dis = str_match(string = variable, pattern = '_([DTP]{1,3})(_fin)?') %>% `[`(, 2)
    case_match(dis,
               'D'~'Diphtheria',
               'T'~'Tetanus',
               'P'~'Pertussis',
               'DTP'~'DTP',
               .default = 'All')
  }
  
  getVariableSimple <- function(variable) {
    re = '(_[DTP]{1,3})?(_fin)?$'
    str_replace(variable, pattern = re,'')
  }
  
  mm <- tibble(diseaseDst=c('DTP','Diphtheria','Tetanus','Pertussis')) %>% mutate(disease='All')

  tblEpi <- epiEconResultSummary %>% 
    filter(discounting=="Undiscounted") %>%
    select(!discounting) %>%
    rowwise() %>% 
    mutate(disease=getDiseaseName(variable),
           variableSimple=getVariableSimple(variable)) %>% 
    left_join(tbMapping, by = "variableSimple") %>% 
    select(type,disease,scenario,pretty,value) %>% 
    filter(!is.na(pretty)) %>% 
    distinct() %>%
    ungroup() %>% 
    left_join(mm, by = "disease") %>%
    filter(scenario %in% scenarios,
           disease %in% DIS) %>% 
    mutate(disease=if_else(is.na(diseaseDst),disease,diseaseDst),
           type=if_else(type=='Comparison','Scenario',type),
           pretty=factor(pretty,levels=unique(tbMapping$pretty)),
           disease=factor(disease,levels=mm$diseaseDst)) %>% 
    select(!diseaseDst) %>% 
    arrange(pretty) %>% 
    pivot_wider(id_cols = c('type','disease','scenario'), names_from='pretty', values_from='value') %>% 
    arrange(disease,type) %>% 
    rename(Scenario=scenario, Disease=disease) %>%
    select(!type) %>% 
    mutate(Scenario = fct_relevel(Scenario, scenarios)) %>% 
    arrange(Scenario)
  
  myColDef <- colDef(na = "-",
                     format = colFormat(separators = TRUE, digits = 0))
  
  columns = rep(list(myColDef), nrow(tbMapping)) %>% as.list() %>% setNames(object = ., nm = tbMapping$pretty)

  if (length(DIS)==1) {
    tblEpi <- tblEpi %>% select(!Disease)
  }
  reactable(data = tblEpi, columns = columns, bordered = TRUE)
}

# point plot of net_costTot vs avert_clin/avert_death across all years (summary not annual) depending on selectedInput
makeCostsPlot3 <- function(costs, x='avert_clin', scenarios=NULL, DIS=NULL, currency="USD",
                           isFinancial=F, isDiscounted=F) {
  LOG("Getting tbData")
  tbData <- costs$resultSummary %>%
    filter(discounting==if(isDiscounted){"Discounted"}else{"Undiscounted"}) %>%
    select(!discounting)
  
  if (is.null(DIS)) DIS <- c('DTP','Diphtheria','Tetanus','Pertussis')
  if (is.null(scenarios)) scenarios <- unique(tbData$scenario)
  
  getDiseaseName <- function(variable) {
    re = '_[DTP]{1,3}(_fin)?$'
    dis = str_match(string = variable, pattern = '_([DTP]{1,3})(_fin)?') %>% `[`(, 2)
    case_match(dis,
               'D'~'Diphtheria',
               'T'~'Tetanus',
               'P'~'Pertussis',
               'DTP'~'DTP',
               .default = 'All')
  }
  
  getVariableSimple <- function(variable) {
    result <- ifelse(str_detect(variable,'_[DTP]{1,3}(_fin)?$'),str_replace(variable,'_[DTP]{1,3}',''),variable)
    result
  }
  
  tbData = tbData %>%
    filter(str_detect(variable,'^avert_|^net_costTot')) %>%
    rowwise() %>%
    mutate(disease=getDiseaseName(variable),
           variableSimple=getVariableSimple(variable)) %>%
    ungroup() %>%
    filter(disease %in% DIS, scenario %in% scenarios) %>%
    select(!variable) %>%
    pivot_wider(names_from = variableSimple)
  
  prettyName <- if(x=='avert_clin'){'Averted Cases'}else{'Averted Deaths'}
  
  palette_name <- tbData %>%
    distinct(scenario) %>%
    nrow() %>% # figure out how many scenarios there are and choose a name for the color palette
    case_match(
      1 ~ "tol1dtp",
      2 ~ "tol2dtp",
      3 ~ "tol3dtp",
      4 ~ "tol4dtp",
      5 ~ "tol5dtp",
      6 ~ "tol6dtp",
      7 ~ "tol7dtp",
      8 ~ "tol8dtp",
      9 ~ "tol9dtp",
      10 ~ "tol10dtp",
      .default = "tol21rainbow"
  )
  LOG("Creating plot with data ({nrow(tbData)}*{ncol(tbData)})")
  net_costTot = "net_costTot"
  if(isFinancial){
    net_costTot = paste0(net_costTot,'_fin')
  }
  
  analysisRange = range(costs$resultAnnual$year)
  yearText = glue::glue("{analysisRange[[1]]} - {analysisRange[[2]]}")
  
  tbData %>%
    ggplot() +
      aes(
        x = .data[[x]], y = .data[[net_costTot]],
        colour = scenario,
        text = sprintf(
          "Averted cases: %s<br>Net total cost: %s<br>Scenario: %s<br>",
          format(round(.data[[x]]), big.mark = ",", scientific = FALSE), 
          format(.data[[net_costTot]], big.mark = ",", scientific = FALSE),
          scenario
        )
      ) +
      geom_point(shape = "circle", size = 4L) +
      scale_color_hue(direction = 1) +
      theme_minimal() +
      expand_limits(x = 0, y = 0) +
      scale_x_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
      scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
      scale_colour_dtp_d(palette_name) +
      labs(title=glue::glue("Incremental Cost Effectiveness, {yearText} ({currency})"), color="Scenario") +
      xlab(prettyName) +
      ylab(glue::glue('Net total cost ({yearText})')) 
}


## Add net total cost plot for all scenarios on budget impact page
makeCostsPlot4 = function(costsComparisons,
                         DIS="DTP", currency="USD", 
                         isFinancial=F, isDiscounted=T) {
  
  get_plot_data <- function(costsComparisons,  
                            DIS="DTP", currency="USD", 
                            isFinancial=F, isDiscounted=T) {
    
    net_costTot_DTP = if_else(isFinancial, 'net_costTot_DTP_fin', 'net_costTot_DTP')
    disc_val = if_else(isDiscounted, 'Discounted', 'Undiscounted')
    
    all_scenarios_data <- list()
    
    for (scenario in seq_along(names(costsComparisons))) {
      
      net_costTot_DTP = if_else(isFinancial, 'net_costTot_DTP_fin', 'net_costTot_DTP')
      net_costVacc = if_else(isFinancial, 'net_costVacc_fin', 'net_costVacc')
      net_cost_intro = if_else(isFinancial, 'net_cost_intro_fin', 'net_cost_intro')
      
      disc_val = if_else(isDiscounted, 'Discounted', 'Undiscounted')
      y = costsComparisons[[scenario]][[disc_val]][c(
        net_costVacc,
        net_cost_intro,
        'net_tot_coi_DTP',
        net_costTot_DTP
      )]
      names(y) <- c(
        'net_costVacc',
        'net_cost_intro',
        'net_tot_coi_DTP',
        'net_costTot_DTP'
      )
      years = costsComparisons[[scenario]]$year
      
      plot_data <- y %>% as_tibble %>% 
        mutate(years=years) %>% 
        pivot_longer(c(net_costVacc, net_cost_intro, net_tot_coi_DTP)) %>%
        mutate(
          name = case_match(name, 
                            "net_costVacc" ~ "Cost of vaccination (routine delivery)",
                            "net_cost_intro" ~ "Cost of vaccine introduction (fixed)",
                            "net_tot_coi_DTP" ~ "Treatment costs averted",
                            .default = name
          )
        ) %>% 
        mutate(
          value = if_else(
            abs(value) < 10 ^ 6,
            signif(value, digits = 3),
            round(value / 1000) * 1000
          )
        ) %>%
        mutate(
          net_costTot_DTP = if_else(
            abs(net_costTot_DTP)  < 10 ^ 6,
            signif(net_costTot_DTP , digits = 3),
            round(net_costTot_DTP / 1000) * 1000
          )
        ) %>% 
        mutate("scenario" = names(costsComparisons)[scenario])
      
      all_scenarios_data[[scenario]] <- plot_data
      
    }
    
    
    plot_data <- bind_rows(all_scenarios_data)
  }
  
  combined_plot_data <- get_plot_data(costsComparisons, 
                                      DIS="DTP", currency="USD", 
                                      isFinancial=isFinancial, 
                                      isDiscounted=isDiscounted)
  
  palette_name <- combined_plot_data %>%
    distinct(scenario) %>%
    nrow() %>% # figure out how many scenarios there are and choose a name for the color palette
    case_match(
      1 ~ "tol1dtp",
      2 ~ "tol2dtp",
      3 ~ "tol3dtp",
      4 ~ "tol4dtp",
      5 ~ "tol5dtp",
      6 ~ "tol6dtp",
      7 ~ "tol7dtp",
      8 ~ "tol8dtp",
      9 ~ "tol9dtp",
      10 ~ "tol10dtp",
      .default = "tol21rainbow"
    )
  
  p <- ggplot(combined_plot_data) +
    aes(
      x = factor(years),
      y=net_costTot_DTP,
      color = scenario,
      group = 1
    ) +
    geom_line(
      aes(
        text = sprintf(
          "Net total cost: %s<br>Year: %s<br>",
          format(net_costTot_DTP, big.mark = ",", scientific = FALSE),
          years
        )
      ),
      linewidth=1.05
    ) +
    scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
    scale_colour_dtp_d(palette_name) +
    labs(
      title = glue::glue(
        "Net total cost (vaccination and treatment) compared to baseline ({currency})"
      ),
      x = NULL,
      y = NULL,
      color = "Scenario"
    ) +
    theme_minimal()  
  
  ggplotly(p, tooltip = c("text")) %>% 
    layout(
      legend = list(orientation = 'h')
    ) %>% 
    plotly::config(
      toImageButtonOptions = list(filename = "net-total-costs-compared-to-baseline-all-scenarios-plot")
    )
}