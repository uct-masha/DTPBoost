div(class = 'interventions',
    fluidRow(
      column(width = 8,
             materialSwitch(inputId = "boost_mass_switch", label = 'Mass Vaccination', value = FALSE,
                            status = "info", right = TRUE, inline = FALSE, width = NULL)
      ),
      column(width = 4,
             conditionalPanel(condition = "input.boost_mass_switch",
                              dropdownButton(label = "Settings", circle = FALSE, status = "primary", size = "sm", icon = icon("gear"), tooltip = FALSE,
                                             width = "700px", margin = "15px", right = TRUE, up = FALSE,
                                             includeMarkdown('www/markdown/mass_vaccination.md'),
                                             fluidRow(
                                               column(4,
                                                      h4("Target"),
                                                      selectizeInput(inputId="mass_mvv", label = "Vaccine(s):", choices=boosters, selected=first(boosters), multiple=T)
                                               ),
                                               column(4,
                                                      h4("Timeline"),
                                                      sliderInputReg(inputId="mass_mvsy", label = "Year to Start:", value = 2025, min=2022, max=2030,step=1, sep=""),
                                                      sliderInputReg(inputId="mass_mvtd", label = "Operational coverage:", value = 80, min=0, max=100, step=1, sep=NULL, post = "%"),
                                                      sliderInputReg(inputId="mass_mvytrt", label = "Duration:", value = 1, min=0, max=100,step=1, post = " years")
                                               ),
                                               column(4,
                                                      h4("Cost"),
                                                      sliderInputReg(inputId="mass_mvcpd", label = "Average Cost per dose:", value = c(10, 150), min=0, max=1000, step=5, post = " US$"),
                                                      #sliderInputReg(inputId="mass_mvcppv", label = "Average Cost per person vaccinated:", value = c(10, 150), min=0, max=1000, step=5, post = " US$")

                                               )
                                             )
                              )
             )
      )
    ),
    tags$small("") # "Just some extra information"
)
