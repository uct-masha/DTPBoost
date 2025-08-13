div(class = 'interventions',
    fluidRow(
      column(width = 4,
             materialSwitch(inputId = "boost_maternal_switch", label = 'Maternal Booster', value = FALSE,
                            status = "info", right = TRUE, inline = FALSE, width = NULL)
      ),
      column(width = 8,
             conditionalPanel(condition = "input.boost_maternal_switch",
                              dropdownButton(label = "Settings", circle = FALSE, status = "primary", size = "sm", icon = icon("gear"), tooltip = FALSE,
                                             width = "700px", margin = "15px", right = FALSE, up = TRUE,
                                             includeMarkdown('www/markdown/maternal_booster.md'),
                                             fluidRow(
                                               column(4,
                                                      h4("Target"),
                                                      selectionInputReg(inputId="biag", label = "Age Group:", choices=ageGroups, selected=first(ageGroups)),
                                                      selectionInputReg(inputId="biv", label = "Vaccine:", choices=vaccines, selected=first(vaccines))
                                               ),
                                               column(4,
                                                      h4("Timeline"),
                                                      sliderInputReg(inputId="bisy", label = "Year to Start:", value = 2025, min=2019, max=2030,step=1, sep=NULL),
                                                      sliderInputReg(inputId="bitd", label = "Operational coverage:", value = 0.8, min=0, max=1, step=.05, sep=NULL, post = " %"),
                                                      sliderInputReg(inputId="biytrt", label = "Years to reach coverage:", value = 1, min=0, max=100,step=1, post = " years")
                                               ),
                                               column(4,
                                                      h4("Cost"),
                                                      sliderInputReg(inputId="bicpd", label = "Average Cost per dose:", value = c(10, 15), min=0, max=1000, step=5, post = " US$"),
                                                      sliderInputReg(inputId="bicppv", label = "Average Cost per person vaccinated:", value = c(10, 15), min=0, max=1000, step=5, post = " US$")
                                                      
                                               )
                                             )
                              )
             )
      )
    ),
    tags$small("Just some extra information")
)