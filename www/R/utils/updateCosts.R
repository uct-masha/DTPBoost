updateCosts <- function(tbCostParms,
                        tbCostParmsCurrency,
                        selectedCurrency,
                        vv_ps,
                        vv_b,
                        vv_cb,
                        vv_ab,
                        vv_m,
                        session=getDefaultReactiveDomain()) {
  # tbCostParms is a tibble with inputID and value where the values currency is given in tbCostParmsCurrency
  costFor <- function(.inputId) {
    amount <- tbCostParms() %>% filter(inputId == .inputId) %>% pull(value)
    convert_currency(amount = amount,
                     src_currency = tbCostParmsCurrency,
                     dst_currency = selectedCurrency)
  }
  # Cost of vaccination (delivery + vaccine) ----
  ## Primary series (health facility) ----
  updateNumericInput(
    session = session,
    inputId = "del_cpd_hf_ps",
    value = costFor("del_cpd_hf_ps"),
    label = paste0("Health facility (", selectedCurrency, ")")
  )
  ## Primary series (Outreach) ----
  updateNumericInput(
    session = session,
    inputId = "del_cpd_o_ps",
    value = costFor("del_cpd_o_ps"),
    label = paste0("Outreach (", selectedCurrency, ")")
  )
  
  ## Cost of routine delivery - Early Childhood Booster (health facility) ----
  updateNumericInput(
    session = session,
    inputId = "del_cpd_hf_b",
    value = costFor("del_cpd_hf_b"),
    label = paste0("Health facility (", selectedCurrency, ")")
  )
  ## Cost of routine delivery - Early Childhood Booster (outreach) ----
  updateNumericInput(
    session = session,
    inputId = "del_cpd_o_b",
    value = costFor("del_cpd_o_b"),
    label = paste0("Outreach (", selectedCurrency, ")")
  )
  
  ## Cost of routine delivery - Child Booster (health facility) ----
  updateNumericInput(
    session = session,
    inputId = "del_cpd_hf_cb",
    value = costFor("del_cpd_hf_cb"),
    label = paste0("Health facility (", selectedCurrency, ")")
  )
  ## Cost of routine delivery - Child Booster (outreach) ----
  updateNumericInput(
    session = session,
    inputId = "del_cpd_o_cb",
    value = costFor("del_cpd_o_cb"),
    label = paste0("Outreach (", selectedCurrency, ")")
  )
  
  ## Cost of routine delivery - Adolescent Booster (health facility) ----
  updateNumericInput(
    session = session,
    inputId = "del_cpd_hf_ab",
    value = costFor("del_cpd_hf_ab"),
    label = paste0("Health facility (", selectedCurrency, ")")
  )
  ## Cost of routine delivery - Adolescent Booster (outreach) ----
  updateNumericInput(
    session = session,
    inputId = "del_cpd_o_ab",
    value = costFor("del_cpd_o_ab"),
    label = paste0("Outreach (", selectedCurrency, ")")
  )
  
  ## Cost of routine delivery - Women of child-bearing age (health facility) ----
  updateNumericInput(
    session = session,
    inputId = "del_cpd_hf_m",
    value = costFor("del_cpd_hf_m"),
    label = paste0("Health facility (", selectedCurrency, ")")
  )
  ## Cost of routine delivery - Women of child-bearing age (outreach) ----
  updateNumericInput(
    session = session,
    inputId = "del_cpd_o_m",
    value = costFor("del_cpd_o_m"),
    label = paste0("Outreach (", selectedCurrency, ")")
  )
  # Cost of vaccines (per dose) ----
  ## Cost of vaccine per dose primary series ----
  updateNumericInput(
    session = session,
    inputId = "unit_cpd_ps",
    value = costFor("unit_cpd_ps"),
    label = paste0(vv_ps, " (", selectedCurrency, ")")
  )
  
  ## Cost of vaccine per dose early childhood booster ----
  updateNumericInput(
    session = session,
    inputId = "unit_cpd_b",
    value = costFor("unit_cpd_b"),
    label = paste0(vv_b, " (", selectedCurrency, ")")
  )
  ## Cost of vaccine per dose child booster ----
  updateNumericInput(
    session = session,
    inputId = "unit_cpd_cb",
    value = costFor("unit_cpd_cb"),
    label = paste0(vv_cb, " (", selectedCurrency, ")")
  )
  ## Cost of vaccine per dose adolescent booster ----
  updateNumericInput(
    session = session,
    inputId = "unit_cpd_ab",
    value = costFor("unit_cpd_ab"),
    label = paste0(vv_ab, " (", selectedCurrency, ")")
  )
  ## Cost of vaccine per dose maternal vaccination ----
  updateNumericInput(
    session = session,
    inputId = "unit_cpd_m",
    value = costFor("unit_cpd_m"),
    label = paste0(vv_m, " (", selectedCurrency, ")")
  )
  # Cost of illness ----
  ## Cost per outpatient case Diphtheria ----
  updateNumericInput(
    session = session,
    inputId = "outp_cost_D",
    value = costFor("outp_cost_D"),
    label = paste0("Cost per outpatient case", " (", selectedCurrency, ")")
  )
  ## Cost per inpatient case Diphtheria ----
  updateNumericInput(
    session = session,
    inputId = "inp_cost_D",
    value = costFor("inp_cost_D"),
    label = paste0("Cost per inpatient case", " (", selectedCurrency, ")")
  )
  ## Cost per outpatient case Pertussis ----
  updateNumericInput(
    session = session,
    inputId = "outp_cost_P",
    value = costFor("outp_cost_P"),
    label = paste0("Cost per outpatient case", " (", selectedCurrency, ")")
  )
  ## Cost per inpatient case Pertussis ----
  updateNumericInput(
    session = session,
    inputId = "inp_cost_P",
    value = costFor("inp_cost_P"),
    label = paste0("Cost per inpatient case", " (", selectedCurrency, ")")
  )
  ## Cost per outpatient case Tetanus ----
  updateNumericInput(
    session = session,
    inputId = "outp_cost_T",
    value = costFor("outp_cost_T"),
    label = paste0("Cost per outpatient case", " (", selectedCurrency, ")")
  )
  ## Cost per inpatient case Tetanus ----
  updateNumericInput(
    session = session,
    inputId = "inp_cost_T",
    value = costFor("inp_cost_T"),
    label = paste0("Cost per inpatient case", " (", selectedCurrency, ")")
  )
  # Cost of introduction ----
  ## Average total once-off cost of introducing new vaccine ----
  updateNumericInput(
    session = session,
    inputId = "strat_intro_cost",
    value = costFor("strat_intro_cost"),
    label = paste0("Fixed cost", " (", selectedCurrency, ")")
  )
  # DTP Booster doses (Interventions) ----
  ## Early childhood unit cost of vaccine ----
  updateNumericInput(
    inputId = "unit_cpd_ib",
    value = costFor("unit_cpd_ib"),
    label = paste0("Unit cost of vaccine (average per dose, excl delivery) (", selectedCurrency, ")")
  )
  ## Early childhood delivery health facility ----
  updateNumericInput(
    inputId = "del_cpd_hf_ib",
    value = costFor("del_cpd_hf_ib"),
    label = paste0("Health facility (", selectedCurrency, ")")
  )
  ## Early childhood delivery outreach ----
  updateNumericInput(
    inputId = "del_cpd_o_ib",
    value = costFor("del_cpd_o_ib"),
    label = paste0("Outreach (", selectedCurrency, ")")
  )
  ## Child booster unit cost of vaccine ----
  updateNumericInput(
    inputId = "unit_cpd_icb",
    value = costFor("unit_cpd_icb"),
    label = paste0("Unit cost of vaccine (average per dose, excl delivery) (", selectedCurrency, ")")
  )
  ## Child booster delivery health facility ----
  updateNumericInput(
    inputId = "del_cpd_hf_icb",
    value = costFor("del_cpd_hf_icb"),
    label = paste0("Health facility (", selectedCurrency, ")")
  )
  ## Child booster delivery outreach ----
  updateNumericInput(
    inputId = "del_cpd_o_icb",
    value = costFor("del_cpd_o_icb"),
    label = paste0("Outreach  (", selectedCurrency, ")")
  )
  
  ## Adolescent booster unit cost of vaccine ----
  updateNumericInput(
    inputId = "unit_cpd_iab",
    value = costFor("unit_cpd_iab"),
    label = paste0("Unit cost of vaccine (average per dose, excl delivery) (", selectedCurrency, ")")
  )
  
  ## Adolescent booster delivery health facility ----
  updateNumericInput(
    inputId = "del_cpd_hf_iab",
    value = costFor("del_cpd_hf_iab"),
    label = paste0("Health facility (", selectedCurrency, ")")
  )
  
  ## Adolescent booster delivery outreach ----
  updateNumericInput(
    inputId = "del_cpd_o_iab",
    value = costFor("del_cpd_o_iab"),
    label = paste0("Outreach (", selectedCurrency, ")")
  )
  
  ## Maternal vaccination unit cost of vaccine ----
  updateNumericInput(
    inputId = "unit_cpd_im",
    value = costFor("unit_cpd_im"),
    label = paste0("Unit cost of vaccine (average per dose, excl delivery) (", selectedCurrency, ")")
  )
  
  ## Maternal vaccination delivery health facility ----
  updateNumericInput(
    inputId = "del_cpd_hf_im",
    value = costFor("del_cpd_hf_im"),
    label = paste0("Health facility (", selectedCurrency, ")")
  )
  
  ## Maternal vaccination delivery outreach ----
  updateNumericInput(
    inputId = "del_cpd_o_im",
    value = costFor("del_cpd_o_im"),
    label = paste0("Outreach (", selectedCurrency, ")")
  )
}
