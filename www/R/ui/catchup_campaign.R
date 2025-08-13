div(class = 'interventions',
    fluidRow(
      column(width = 8,
             materialSwitch(inputId = "boost_catchup_switch", label = 'Catchup Campaign', value = FALSE,
                            status = "info", right = TRUE, inline = FALSE, width = NULL)
      ),
      column(width = 4,
             conditionalPanel(condition = "input.boost_catchup_switch",
                              dropdownButton(label = "Settings", circle = FALSE, status = "primary", size = "sm", icon = icon("gear"), tooltip = FALSE,
                                             width = "815px", margin = "15px", right = TRUE,
                                             includeMarkdown('www/markdown/catchup_campaign.md'),
                                             fluidRow(
                                               column(4,
                                                      fluidRow(
                                                        column(6,
                                                               "Vaccine Targets"),
                                                        column(6,
                                                               "Vaccine Type")
                                                      ),
                                                      fluidRow(
                                                        column(6,
                                                               checkboxGroupInput(inputId="ccvta", label = "", choices=boosters[[1]], selected=boosters[[1]])),
                                                        column(6,
                                                               selectizeInput(inputId="ccvty1", label = "", choices=vaccines[[1]], selected=vaccines[[1]]))
                                                      ),
                                                      fluidRow(
                                                        column(6,
                                                               checkboxGroupInput(inputId="ccvta", label = "", choices=boosters[[2]], selected=boosters[[2]])),
                                                        column(6,
                                                               selectizeInput(inputId="ccvty2", label = "", choices=vaccines[[2]], selected=vaccines[[2]]))
                                                      ),
                                                      fluidRow(
                                                        column(6,
                                                               checkboxGroupInput(inputId="ccvta", label = "", choices=boosters[[3]], selected=boosters[[3]])),
                                                        column(6,
                                                               selectizeInput(inputId="ccvty3", label = "", choices=vaccines[[3]], selected=vaccines[[3]]))
                                                      ),
                                                      fluidRow(
                                                        column(6,
                                                               checkboxGroupInput(inputId="ccvta", label = "", choices=boosters[[4]], selected=boosters[[4]])),
                                                        column(6,
                                                               selectizeInput(inputId="ccvty4", label = "", choices=vaccines[[4]], selected=vaccines[[4]]))
                                                      )
                                               ),
                                               column(4,
                                                      h4("Timeline"),
                                                      sliderInputReg(inputId="catchup_ccsy", label = "Year to Start:", value = 2025, min=2022, max=2030,step=1, sep=""),
                                                      sliderInputReg(inputId="catchup_cctd", label = "Operational coverage:", value = 0.8, min=0, max=1, step=.05, sep=NULL, post = "%"),
                                                      sliderInputReg(inputId="catchup_ccytrt", label = "Duration:", value = 1, min=0, max=100,step=1, post = " years")
                                               ),
                                               column(4,
                                                      h4("Cost"),
                                                      sliderInputReg(inputId="catchup_cccpd", label = "Average Cost per dose:", value = c(10, 15), min=0, max=1000, step=5, post = " US$"),
                                                      #sliderInputReg(inputId="catchup_cccppv", label = "Average Cost per person vaccinated:", value = c(10, 15), min=0, max=1000, step=5, post = " US$"),
                                                      selectizeInput(inputId="catchup_ccbidp", label = "Delivery Platform:", choices=c("School"), selected="School")
                                               )
                                             )
                              )
             )
      )
    ),
    tags$small("") # "Just some extra information"
)
