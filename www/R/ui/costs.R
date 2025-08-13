# ui functions for the costs page (1.4 Costs)

costsDelRowUI = function(col1="Primary Series", acronym='ps', pfin = 40) {
  fluidRow(
    column(
      width = 3,
      h4(col1),
    ),
    column(
      width = 2,
      autonumericInput(
        inputId = paste0('del_cpd_hf_',acronym),
        label = "Health facility (USD)",
        value = 1.701,
        decimalCharacter = ".",
        digitGroupSeparator = ",",
        decimalPlaces = 3,
        align = "left",
        minimumValue = 0
      ),
    ),
    column(
      width = 2,
      autonumericInput(
        inputId = paste0('del_cpd_o_',acronym),
        label =  "Outreach (USD)",
        value = 1.425,
        decimalCharacter = ".",
        digitGroupSeparator = ",",
        decimalPlaces = 3,
        align = "left",
        minimumValue = 0
      )
    ),   
    column(
      width = 4,
      sliderInputReg(
        post = "%",
        inputId = paste0('del_pfin_',acronym),
        label =  "Delivery costs paid directly by government (%)",
        value = pfin,
        min = 0,
        max = 100,
        step = 1
        #ticks=F
      )
    )
  )
}

costsVaxRowUI = function(col1, acronym, pfin = 40) {
  fluidRow(
    column(
      width = 3,
      h4(col1),
    ),
    column(
      width = 4,
      autonumericInput(
        inputId = paste0('unit_cpd_',acronym),
        label = "Primary series",
        value = 0.2247,
        decimalCharacter = ".",
        digitGroupSeparator = ",",
        decimalPlaces = 3,
        align = "left",
        minimumValue = 0
      )
    ),
    column(
      width = 4,
      sliderInputReg(
        post = "%",
        inputId = paste0('unit_pfin_',acronym),
        label =  "Cost of vaccines paid directly by government (%)",
        value = pfin,
        min = 0,
        max = 100,
        step = 1
        #ticks = F
      )
    )
  )
}