div(
  class = "interventions",
  fluidRow(
    column(
      width = 8,
      materialSwitch(
        inputId = "boost_maternal_switch", label = "Maternal vaccination", value = FALSE,
        status = "info", right = TRUE, inline = FALSE, width = NULL
      )
    ),
    column(
      width = 4,
      conditionalPanel(
        condition = "input.boost_maternal_switch",
        dropdownButton(
          inputId = "boost_maternal_settings", label = "Settings", circle = FALSE, status = "primary", size = "sm", icon = icon("gear"), tooltip = FALSE,
          width = "1000px", margin = "15px", right = TRUE, up = FALSE,
          # includeMarkdown('www/markdown/maternal_booster.md'),
          fluidRow(
            column(
              2,
              h4("Schedule"),
              # selectizeInput(inputId="maternal_biag", label = "Age Group:", choices=c("Women of reproductive age (WRA)","Pregnant Woman","WRA & pregnant women")),
              selectizeInput(
                inputId = "vv_im",
                label = "Vaccine",
                choices = c("Td", "TdaP", "TT"),
                selected = "Td"
              ),
              sliderInputReg(
                inputId = "vsy_im",
                label = "Year of introduction",
                value = 2025,
                min = 2024,
                max = 2030,
                step = 1,
                sep = ""
              )
            ),
            column(
              2,
              h4("Coverage"),
              sliderInputReg(
                inputId = "vtc_im",
                label = "Target operational coverage (1+ doses)",
                value = 95, min = 0, max = 100,
                step = 1, sep = NULL, post = "%"
              ),
              sliderInputReg(
                inputId = "vtc_im2",
                label = "Target operational coverage (2+ doses)",
                value = 55, min = 0, max = 100,
                step = 1, sep = NULL, post = "%"
              ),
              sliderInputReg(
                inputId = "vytrt_im",
                label = "Years to reach target coverage",
                value = 2, min = 0, max = 7,
                step = 1, post = " years"
              )
            ),
            column(
              2,
              h4("Delivery"),
              sliderInputReg(
                inputId = "del_hf_im",
                label = "Health facility (%)",
                value = 69,
                min = 0,
                max = 100,
                post = "%"
              ),
              sliderInputReg(
                inputId = "del_o_im",
                label = "Outreach site (%)",
                value = 31,
                min = 0,
                max = 100,
                post = "%"
              )
            ),
            column(
              width = 3,
              h4("Cost of routine delivery (per dose)"),
              autonumericInput(
                inputId = "del_cpd_hf_im",
                label = "Health facility (USD)",
                value = 0.701,
                decimalCharacter = ".",
                digitGroupSeparator = ",",
                decimalPlaces = 3,
                align = "left",
                minimumValue = 0
              ),
              autonumericInput(
                inputId = "del_cpd_o_im",
                label = "Outreach (USD)",
                value = 0.425,
                decimalCharacter = ".",
                digitGroupSeparator = ",",
                decimalPlaces = 3,
                align = "left",
                minimumValue = 0
              ),
              sliderInputReg(
                inputId = "del_pfin_im",
                label = "Proportion of routine delivery costs paid directly by government",
                value = 22,
                min = 0,
                max = 100,
                post = "%"
              )
            ),
            column(
              3,
              h4("Cost of vaccines (per dose)"),
              autonumericInput(
                inputId = "unit_cpd_im",
                label = "Unit cost of vaccine (average per dose, excl delivery) (USD)",
                value = 0.1284,
                decimalCharacter = ".",
                digitGroupSeparator = ",",
                decimalPlaces = 3,
                align = "left",
                minimumValue = 0
              ),
              sliderInputReg(
                inputId = "unit_pfin_im",
                label = "Proportion of unit costs paid directly by government",
                value = 100,
                min = 0,
                max = 100,
                post = "%"
              )
              # sliderInputReg(inputId="adolescent_bicppv", label = "Average cost per person vaccinated:", value = c(10, 150), min=0, max=1000, step=5, post = " US$"),
              # sliderInputReg(inputId="maternal_bicppv", label = "Average Cost per person vaccinated:", value = c(10, 150), min=0, max=1000, step=5, post = " US$")
            )
          )
        )
      )
    )
  ),
  tags$small("") # "Just some extra information"
)
