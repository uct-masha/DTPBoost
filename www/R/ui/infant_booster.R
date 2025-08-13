div(
  class = "interventions",
  fluidRow(
    column(
      width = 4,
      materialSwitch(
        inputId = "boost_infant_switch", label = "Early childhood booster", value = FALSE,
        status = "info", right = TRUE, inline = FALSE, width = NULL
      )
    ),
    column(
      width = 4,
      conditionalPanel(
        condition = "(input.boost_infant_switch & input.boost_child_switch) & (input.vsy_ib == input.vsy_icb)",
        checkboxInput(
          inputId = "vps_ib",
          label = span(tags$b("Expanded cohort"), actionButton("pulseRelativeCoverageInfoInfant", label = "", icon = icon("info-circle"), class = "btn-info")),
          value = FALSE
        )
      )
    ),
    column(
      width = 4,
      conditionalPanel(
        condition = "input.boost_infant_switch",
        dropdownButton(
          inputId = "boost_infant_settings", label = "Settings", circle = FALSE, status = "primary", size = "sm", icon = icon("gear"), tooltip = FALSE,
          width = "1000px", margin = "15px", right = TRUE, up = FALSE,
          # includeMarkdown('www/markdown/infant_booster.md'),
          fluidRow(
            column(
              width = 4,
              h4("Schedule"),
              selectizeInput(
                inputId = "va_ib",
                label = "Age group",
                choices = c(paste(seq(12, 23), "mth")),
                selected = "18 mth"
              ),
              selectizeInput(
                inputId = "vv_ib",
                label = "Vaccine",
                choices = c(
                  "DTaP", "DTaP-HepB-IPV", "DTaP-Hib",
                  "DTaP-Hib-HepB", "DTaP-Hib-HepB-IPV", "DTaP-Hib-IPV",
                  "DTaP-IPV", "DTwP", "DTwP-HepB", "DTwP-Hib", "DTwP-Hib-HepB",
                  "DTwP-Hib-HepB-IPV"
                ),
                selected = "DTwP-Hib-HepB"
              ),
              sliderInputReg(
                inputId = "vsy_ib",
                label = "Year of introduction",
                value = 2025,
                min = 2024,
                max = 2034,
                step = 1,
                sep = ""
              )
            ),
            column(
              width = 4,
              h4("Coverage"),
              sliderInputReg(
                inputId = "vic_ib",
                label = "Coverage in year of introduction",
                value = 40, min = 0, max = 100, step = 1,
                sep = NULL, post = "%"
              ),
              sliderInputReg(
                inputId = "vtc_ib",
                label = "Target operational coverage",
                value = 80, min = 0, max = 100, step = 1,
                sep = NULL, post = "%"
              ),
              sliderInputReg(
                inputId = "vytrt_ib",
                label = "Years to reach target coverage",
                value = 4, min = 0, max = 7, step = 1,
                post = " years"
              )
            ),
            column(
              width = 4,
              h4(" "),
              plotOutput("coverage_widget_ib_settings", height = "300px") |> 
                withSpinner(color = "#237D80")
            )
          ),
          br(),
          fluidRow(
            column(
              width = 4,
              h4("Delivery"),
              sliderInputReg(
                inputId = "del_hf_ib",
                label = "Health facility (%)",
                value = 69,
                min = 0,
                max = 100,
                post = "%"
              ),
              sliderInputReg(
                inputId = "del_o_ib",
                label = "Outreach site (%)",
                value = 31,
                min = 0,
                max = 100,
                post = "%"
              )
            ),
            column(
              width = 4,
              h4("Cost of routine delivery (per dose)"),
              autonumericInput(
                inputId = "del_cpd_hf_ib",
                label = "Health facility (USD)",
                value = 1.701,
                decimalCharacter = ".",
                digitGroupSeparator = ",",
                decimalPlaces = 3,
                align = "left",
                minimumValue = 0
              ),
              autonumericInput(
                inputId = "del_cpd_o_ib",
                label = "Outreach (USD)",
                value = 0.425,
                decimalCharacter = ".",
                digitGroupSeparator = ",",
                decimalPlaces = 3,
                align = "left",
                minimumValue = 0
              ),
              sliderInputReg(
                inputId = "del_pfin_ib",
                label = "Proportion of routine delivery costs paid directly by government",
                min = 0,
                max = 100,
                value = 22,
                post = "%"
              )
            ),
            column(
              width = 4,
              h4("Cost of vaccines (Per dose)"),
              autonumericInput(
                inputId = "unit_cpd_ib",
                label = "Unit cost of vaccine (average per dose, excl delivery) (USD)",
                value = 0.2247,
                decimalCharacter = ".",
                digitGroupSeparator = ",",
                decimalPlaces = 3,
                align = "left",
                minimumValue = 0
              ),
              sliderInputReg(
                inputId = "unit_pfin_ib",
                label = "Proportion of unit costs paid directly by government",
                min = 0,
                max = 100,
                value = 25,
                post = "%"
              )
            )
          )
        )
      )
    )
  ),
  tags$small("") # "Just some extra information"
)
