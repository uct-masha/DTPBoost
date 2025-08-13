div(
  class = "interventions",
  fluidRow(
    column(
      width = 8,
      materialSwitch(
        inputId = "boost_adolescent_switch", label = "Adolescent booster", value = FALSE,
        status = "info", right = TRUE, inline = FALSE, width = NULL
      )
    ),
    column(
      width = 4,
      conditionalPanel(
        condition = "input.boost_adolescent_switch",
        dropdownButton(
          inputId = "boost_adolescent_settings", label = "Settings", circle = FALSE, status = "primary", size = "sm", icon = icon("gear"), tooltip = FALSE,
          width = "1000px", margin = "15px", right = TRUE, up = FALSE,
          # includeMarkdown('www/markdown/adolescent_booster.md'),
          fluidRow(
            column(
              width = 4,
              h4("Schedule"),
              selectizeInput(
                inputId = "va_iab",
                label = "Age group",
                choices = c(paste(seq(9, 15), "yr")),
                selected = "10 yr"
              ),
              selectizeInput(
                inputId = "vv_iab",
                label = "Vaccine",
                choices = c("Td", "TdaP", "TdaP-IPV", "Td-IPV", "TT"),
                selected = "Td"
              ),
              sliderInputReg(
                inputId = "vsy_iab",
                label = "Year of introduction",
                value = 2025,
                min = 2024,
                max = 2034,
                step = 1, sep = ""
              )
            ),
            column(
              width = 4,
              h4("Coverage"),
              sliderInputReg(
                inputId = "vic_iab",
                label = "Coverage in year of introduction",
                value = 30, min = 0, max = 100,
                step = 1, sep = NULL, post = "%"
              ),
              sliderInputReg(
                inputId = "vtc_iab",
                label = "Target operational coverage",
                value = 75, min = 0, max = 100,
                step = 1, sep = NULL, post = "%"
              ),
              sliderInputReg(
                inputId = "vytrt_iab",
                label = "Years to reach target coverage",
                value = 3, min = 0, max = 7,
                step = 1, post = " years"
              )
            ),
            column(
              width = 4,
              plotOutput("coverage_widget_ab_settings", height = "300px") |> 
              withSpinner(color = "#237D80")
            )
          ),
          br(),
          fluidRow(
            column(
              width = 4,
              h4("Delivery"),
              sliderInputReg(
                inputId = "del_hf_iab",
                label = "Health facility (%)",
                value = 10,
                min = 0,
                max = 100,
                post = "%"
              ),
              sliderInputReg(
                inputId = "del_o_iab",
                label = "Outreach site (%)",
                value = 90,
                min = 0,
                max = 100,
                post = "%"
              )
            ),
            column(
              width = 4,
              h4("Cost of routine delivery (per dose)"),
              autonumericInput(
                inputId = "del_cpd_hf_iab",
                label = "Health facility (USD)",
                value = 0.701,
                decimalCharacter = ".",
                digitGroupSeparator = ",",
                decimalPlaces = 3,
                align = "left",
                minimumValue = 0
              ),
              autonumericInput(
                inputId = "del_cpd_o_iab",
                label = "Outreach (USD)",
                value = 0.425,
                decimalCharacter = ".",
                digitGroupSeparator = ",",
                decimalPlaces = 3,
                align = "left",
                minimumValue = 0
              ),
              sliderInputReg(
                inputId = "del_pfin_iab",
                label = "Proportion of routine delivery costs paid directly by government",
                value = 22,
                min = 0,
                max = 100,
                post = "%"
              )
            ),
            column(
              width = 4,
              h4("Cost of vaccines (per dose)"), # Each vaccine can have a different cost...
              autonumericInput(
                inputId = "unit_cpd_iab",
                label = "Unit cost of vaccine (average per dose, excl delivery) (USD)",
                value = 0.1284,
                decimalCharacter = ".",
                digitGroupSeparator = ",",
                decimalPlaces = 3,
                align = "left",
                minimumValue = 0
              ),
              sliderInputReg(
                inputId = "unit_pfin_iab",
                label = "Proportion of unit costs paid directly by government",
                value = 100,
                min = 0,
                max = 100,
                post = "%"
              )
              # sliderInputReg(inputId="adolescent_bicppv", label = "Average cost per person vaccinated:", value = c(10, 150), min=0, max=1000, step=5, post = " US$"),
            )
          )
        )
      )
    )
  ),
  tags$small("") # "Just some extra information"
)
