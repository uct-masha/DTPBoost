div(
  class = "interventions",
  fluidRow(
    column(
      width = 4,
      materialSwitch(
        inputId = "boost_child_switch", label = "Child booster", value = FALSE,
        status = "info", right = TRUE, inline = FALSE, width = NULL
      )
    ),
    column(
      width = 4,
      conditionalPanel(
        condition = "(input.boost_child_switch & input.boost_adolescent_switch) & (input.vsy_icb == input.vsy_iab)",
        checkboxInput(
          inputId = "vps_icb",
          label = span(tags$b("Expanded cohort"), actionButton("pulseRelativeCoverageInfoChild", label = "", icon = icon("info-circle"), class = "btn-info")),
          value = FALSE
        )
      )
    ),
    column(
      width = 4,
      conditionalPanel(
        condition = "input.boost_child_switch",
        dropdownButton(
          inputId = "boost_child_settings", label = "Settings", circle = FALSE, status = "primary", size = "sm", icon = icon("gear"), tooltip = FALSE,
          width = "1000px", margin = "15px", right = TRUE, up = FALSE,
          # includeMarkdown('www/markdown/child_booster.md'),
          fluidRow(
            column(
              width = 4,
              h4("Schedule"),
              selectizeInput(
                inputId = "va_icb",
                label = "Age group",
                choices = c(paste(seq(4, 7), "yr")),
                selected = "6 yr"
              ),
              selectizeInput(
                inputId = "vv_icb",
                label = "Vaccine",
                choices = c("DT", "DTaP-IPV", "DTwP", "Td", "TdaP", "TdaP-IPV", "Td-IPV", "TT"),
                selected = "Td"
              ),
              sliderInputReg(
                inputId = "vsy_icb",
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
                inputId = "vic_icb",
                label = "Coverage in year of introduction",
                value = 40, min = 0, max = 100,
                step = 1, sep = NULL, post = "%"
              ),
              sliderInputReg(
                inputId = "vtc_icb",
                label = "Target operational coverage",
                value = 80, min = 0, max = 100,
                step = 1, sep = NULL, post = "%"
              ),
              sliderInputReg(
                inputId = "vytrt_icb",
                label = "Years to reach target coverage",
                value = 4, min = 0, max = 7, step = 1,
                post = " years"
              )
            ),
            column(
              width = 4,
              plotOutput("coverage_widget_cb_settings", height = "300px") |> 
              withSpinner(color = "#237D80")
            )
          ),
          br(),
          fluidRow(
            column(
              width = 4,
              h4("Delivery"),
              sliderInputReg(
                inputId = "del_hf_icb",
                label = "Health facility (%)",
                value = 10,
                min = 0,
                max = 100,
                post = "%"
              ),
              sliderInputReg(
                inputId = "del_o_icb",
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
                inputId = "del_cpd_hf_icb",
                label = "Health facility (USD)",
                value = 0.701,
                decimalCharacter = ".",
                digitGroupSeparator = ",",
                decimalPlaces = 3,
                align = "left",
                minimumValue = 0
              ),
              autonumericInput(
                inputId = "del_cpd_o_icb",
                label = "Outreach (USD)",
                value = 0.425,
                decimalCharacter = ".",
                digitGroupSeparator = ",",
                decimalPlaces = 3,
                align = "left",
                minimumValue = 0
              ),
              sliderInputReg(
                inputId = "del_pfin_icb",
                label = "Proportion of routine delivery costs paid directly by government",
                value = 22,
                min = 0,
                max = 100,
                post = "%"
              )
            ),
            column(
              width = 4,
              h4("Cost of vaccines (per dose)"),
              autonumericInput(
                inputId = "unit_cpd_icb",
                label = "Unit cost of vaccine (average per dose, excl delivery) (USD)",
                value = 0.1284,
                decimalCharacter = ".",
                digitGroupSeparator = ",",
                decimalPlaces = 3,
                align = "left",
                minimumValue = 0
              ),
              sliderInputReg(
                inputId = "unit_pfin_icb",
                label = "Proportion of unit costs paid directly by government",
                value = 100,
                min = 0,
                max = 100,
                post = "%"
              )
              # sliderInputReg(inputId="child_bicppv", label = "Average cost per person vaccinated:", value = c(10, 150), min=0, max=1000, step=5, post = " US$"),
            )
          )
        )
      )
    )
  ),
  tags$small("") # "Just some extra information"
)
