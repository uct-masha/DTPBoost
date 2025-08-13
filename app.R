DEBUG=F
shouldCacheCalibrationTibble = !DEBUG && F
shouldSaveAppObjects = DEBUG  # Change to TRUE to always save objects
shouldPredictPtrans = T  # ptrans calculated from model
shouldZipFiles = F  # Only download the xlsx of the results until issue with zip is fixed on shinyapps.io
shouldIncludeDebugWidgets = F  # Include widgets for debugging
options(tidyverse.quiet = TRUE) # Hush tidyverse loads 
options(datatable.quiet = TRUE) # Hush data.table loads 
options(future.globals.onReference = "warn") # Warn on global references not copied
options(future.rng.onMisuse = "ignore") # Suppress warning about parallel random numbers
source(here::here("www/R/packages.R"))
source(here::here("models/R/scripts/utils.R"))  # logging
LOG <- makeLogger(default_level = LEVEL$TRACE)

saveDebugRDS = function(object, file="", ...) {
  if(shouldSaveAppObjects) {
    cachedAppObjectsDir = 'cachedAppObjects'
    if (!fs::dir_exists(cachedAppObjectsDir)) fs::dir_create(cachedAppObjectsDir)
    fileNew = fs::path_join(parts=c(cachedAppObjectsDir, fs::path_file(file)))
    if (file != fileNew) {
        LOG("Fixing app object file name so {file} becomes {fileNew}")
        file <- fileNew
    }
    LOG("Saving app object {file}")
    saveRDS(object, file=file, ...)
  }
}

plan(sequential)

# model scripts ----
source(here::here('models/R/models/commonModelCode.R'))
source(here::here('models/R/models/diphtheriaModel.R'))
source(here::here('models/R/models/tetanusModel.R'))
source(here::here('models/R/models/pertussisModel.R'))
source(here::here('models/R/models/dtpModel.R'))
source(here::here('models/R/scripts/calibration.R'))
source(here::here("models/R/scripts/calculateCosts.R"))

# app scripts ----
source(here::here("www/R/utils/utils.R"))
source(here::here("www/R/utils/updateCosts.R"))
source(here::here("www/R/utils/restoreSession.R"))
source(here::here("www/R/utils/getShinyInputs.R"))
source(here::here("www/R/utils/dtp_colours.R"))
source(here::here("www/R/ui/costs.R"))
source(here::here("www/R/googleCachingFunctions.R"))

# Service Account JSON file
fnameJson = here::here('custom-service-acct-file.json')
if(shouldCacheCalibrationTibble) {
  Sys.setenv(GOOGLE_APPLICATION_CREDENTIALS=fnameJson)
  drive_auth(path = Sys.getenv('GOOGLE_APPLICATION_CREDENTIALS'))
}

# ggplot theme

setLogLevel(LEVEL$TRACE)
#ggthemr('fresh')

setLockedState <- function(isLocked) {
  if(is.null(isLocked)) {
    LOG("App locked state was null")
    return(NULL)
  } else if (isLocked) {
    LOG("Locking the app")
    shinyjs::disable(id='calibrate_manual')
    shinyjs::disable(id='go_interventions')
    shinyjs::runjs('move()')
  } else {
    LOG("Unlocking the app")
    shinyjs::enable(id='calibrate_manual')
    shinyjs::enable(id='go_interventions')
  }
}

# load gbd data
tbGBDIncidence <- read_rds(here("data/tbGBDIncidenceAllAges.rds"))

aggregate_age_categories <- c("<1yo", "<5yo", "5-9yo", "10-14yo", "<15yo", "15-49yo", "50-75+yo") 

age_groups <- levels(getPopData("UGA")$age_group)
ageGroups <- list(
  `Aggregate age groups` = aggregate_age_categories,
  `Age groups` = age_groups
)

choices_va_p1 = c(paste(seq(6,26),"wk"), "None")
choices_va_p2 = c(paste(seq(10,26),"wk"), "None")
choices_va_p3 = c(paste(seq(14,26),"wk"), "None")

choices_va_b = c("None", paste(seq(12,23), "mth"))
choices_va_cb = c("None", paste(seq(4,7),"yr"))
choices_va_ab = c("None", paste(seq(9,15),"yr"))
choices_vmba = c("None","During pregnancy")

choices_vv_ps <- c("None", "DTaP","DTaP-HepB-IPV","DTaP-Hib","DTaP-Hib-HepB",
                   "DTaP-Hib-HepB-IPV","DTaP-Hib-IPV","DTaP-IPV","DTwP",
                   "DTwP-HepB","DTwP-Hib","DTwP-Hib-HepB","DTwP-Hib-HepB-IPV")
choices_vv_b <- choices_vv_ps
choices_vv_cb = c("None", "DT", "DTaP-IPV", "DTwP", "Td", "TdaP", "TdaP-IPV", "Td-IPV", "TT")
choices_vv_ab = c("None", "Td", "TdaP", "TdaP-IPV", "Td-IPV", "TT")
choices_vv_m = c("None", "Td", "TdaP", "TT")

boosters <- str_split("Primary Series,Infant Booster,Child Booster,Adolescent Booster",',') %>% first

# dashboard header ----
header <- dashboardHeader(
  title = "DTP Boost", titleWidth = 265,
  # https://stackoverflow.com/questions/47569992/home-button-in-header-in-r-shiny-dashboard
  # tags$li(class = "dropdown", 
  #         style = "padding: 20px 10px 0px 0px;",
  #         tags$p("IMPORTANT: You can only save a session after completing both Step 2 and Step 3   ")),
  tags$li(class = "dropdown", downloadButton("save_session", "Save current session"),
          actionButton("parameter_upload", "Upload previous session", icon = icon('fas fa-upload')))
)

# dashboard sidebar ----
sidebar <- dashboardSidebar(
  #id = "sidebar",
  width = 265,
  sidebarMenu(
    id = "menu",
    menuItem("Welcome", tabName = 'welcome', icon = icon("fas fa-home")),
    menuItem("Step 1. Set-up Country Profile", tabName = 'model1', icon = icon("1")),
    menuItem("Step 2. Calibrate Model", tabName = 'model2', icon = icon("2")),
    menuItem("Step 3. Design Booster Strategy", tabName = 'model3', icon = icon("3")),
    menuItem("Step 4. Explore Results", tabName = 'model4', icon = icon("4"))
  )
)

# dashboard body ----
body <- dashboardBody(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    # https://stackoverflow.com/questions/64018032/using-font-awesome-5-7-icons-in-shiny-apps
    tags$style("@import url(https://use.fontawesome.com/releases/v6.1.2/css/all.css);"),
    tags$script(HTML(js)),
    useShinyjs(),
    shinyjs::inlineCSS(css)
  ),
  tags$head(
    tags$link(rel='icon', 
              href=base64enc::dataURI(file='www/favicon.ico', mime='image/x-icon'))
  ),
  chooseSliderSkin(skin = c("Flat"), color = "#237D80"), # styling for sliderInputs
  tabItems(
    ## Welcome ----
    tabItem(
      tabName = "welcome",
      fluidRow(
        column(
          width = 12,
          tabBox(
            width = NULL, title = NULL,
            ### About ----
            tabPanel(
              "About",
              fluidRow(
                column(
                  width = 8,
                  h4("DTP Booster Strategy Tool"),
                  br(),
                  p("Welcome to the DTP Booster Strategy Tool. DTP Boost is a tool that allows users to design vaccination strategies to explore the health impact and cost-effectiveness of introducing diphtheria, tetanus and pertussis booster doses in a selected country.", style = "font-size:15px !important"),
                  br(),
                  p("The tool uses a combination of epidemiological and economic models to generate projections. Multiple vaccination strategies can be designed, allowing a detailed and interactive exploration of the relative costs and benefits of each. The projections are tailored to the selected country, accounting for differences in existing vaccination strategy, current burden of disease, and health systems characteristics. Click on the tabs above to learn more about the tool and models used." ,style = "font-size:15px !important"),
                  br(),
                  p(strong("To begin, please click the 'Get Started' button or follow the steps outlined in the menu on the left."),  style = "font-size:15px !important"),
                  p(strong("Alternatively, click the 'Load AFENET Session' button to load the session used in the Uganda first-use in May 2023."),  style = "font-size:15px !important"),
                  br(),
                  p("This work was supported by funding from the U.S. Centers for Disease Control and Prevention under a Cooperative Agreement with the African Field Epidemiology Network (AFENET). By continuing to use this tool, you accept that the tool is as is and that the partners involved in development are not responsible for the results, interpretation, or decisions made based on the tool. They do not represent the official position of the U.S. Centers for Disease Control and Prevention, AFENET, or MASHA. ",  style = "font-size:15px !important"),
                  br(),
                  p("Citation for use of the tool: DTP Boost Strategy Tool, version 1.0 (18 July 2023).",
                    style = "font-size:15px !important"),
                  # p(),
                  br()
                ),
                column(
                  width = 4,
                  HTML(
                    '
                    <div>
                      <a href="http://www.masha.uct.ac.za" target="_blank">
                        <img src="DTP_Shiny_logos.png">
                      </a>
                    </div>
                    '
                  )
                )
              ),
              fluidRow(
                column(
                  width = 4,
                  actionButton("start", "Get started",  class = "btn-block")
                ),
                column(
                  width = 4,
                  input_task_button("restoreSpecialSession", "Load AFENET Session",  class = "btn-block")
                )
              )
            ),
            ### The Project ----
            tabPanel(
              "The Project",
              fluidRow(
                column(
                  12,
                  h4("DTP Boost Project Overview"),
                  br()
                )
              ),
              p("Given WHO recommendations for booster doses of existing diphtheria- tetanus- pertussis- containing vaccines (DTPCV) and resource limitations faced by low- and middle-income countries (LMIC), there is a need for information on country-specific health and economic benefits to inform the introduction of DTPCV booster doses. There is currently no available model to inform country-level decision making about DTPCV booster dose introduction."),
              br(),
              p("The objective of this project was to develop a single integrated mathematical (disease and economic evaluation) modelling tool to estimate the health impact on diphtheria, tetanus, and pertussis (DTP) and the cost-effectiveness of introducing three WHO-recommended DTPCV booster doses in an individual low- or middle-income country."),
              br(),
              p("The resulting DTP Boost tool was developed by MASHA with input from CDC, AFENET, and a Technical Expert Group (see “The Team”).  An initial version was piloted in Uganda in collaboration with the Uganda National EPI (UNEPI) and Ministry of Health."),
              br()
              
            ),
            ### The model ----
            tabPanel(
              "The Model",
              #br(),
              # fluidRow(column(12, img(src = "clinical_burden.png", width = "100%"))),
              fluidRow(
                column(
                  12,
                  h4("DTP Boost Model"),
                  br()
                )
              ),
              fluidRow(
                column(
                  width = 5,
                  p("As diphtheria, pertussis and tetanus have fundamentally different disease processes, three semi-independent models have been developed for the same population. The three models are be linked together at each time step to incorporate interactions and dependencies between the three diseases. The parameters that link the three models are age-dependent timing and coverage of vaccination."),
                  br(),
                  h4("Key features"),
                  p(),
                  p("DTP Boost is a suite of linked deterministic age-structured compartmental models to simulate the transmission of and booster vaccine impact on diphtheria, tetanus and pertussis prevalence. The figure presents an overview of key model features, linkage of the three disease components and integration of the epidemiological and economic modules."),
                  br(),
                  h4("Calibration"),
                  p("The model-projected incidence may be fitted to observed incidence data for all three diseases (WHO Global Health Observatory dataset, or user-defined data) or estimated incidence data (Global Burden of Disease dataset). Calibration may be performed statistically through a least squares data fitting routine, or manually through manipulated transmission parameters such that the modelled incidence mimics the dataset."),
                  br(),
                  h4("Economic Analysis"),
                  p("The suite of models incorporate an economic module where outputs from each of the disease models, such as number of clinical cases, number of deaths, number of cases per treatment avenue and number of doses of vaccine administered, are computed into benefits (cases averted, deaths averted) and costs (cost of vaccination, cost of illness) for further economic analysis. A budget impact analysis and cost-effectiveness analysis is presented. Analyses can be run using local currency or United States dollars (USD). The economic analyses are undertaken from the provider perspective (assumed to be equivalent to the government). Further details of the costing and economic methodology is available on the Costs page under Step 1."),
                  br(),
                  h4("Results"),
                  p("Projected impact of vaccine booster strategies are collated for the three diseases to assess the result change in incidence, deaths and population protected by age group."),
                  br(),
                  h4("Data sources"),
                  p("Datasets inclusive of all countries and territories are used in the tool to inform demographic characteristics, contact matrices, vaccine coverage and disease incidence."),
                  tags$ul(
                    tags$li(
                      style = "text-align: justify;list-style-type: disc", "United Nations World Population Prospects 2019"),
                    tags$li(
                      style = "text-align: justify;list-style-type: disc", "WHO Global Health Observatory"),
                    tags$li(
                      style = "text-align: justify;list-style-type: disc", "IHME Global Burden of Diseases, Injuries, and Risk Factors Study (GBD)"),
                    tags$li(
                      style = "text-align: justify;list-style-type: disc", "Prem, K., Cook, A. R., & Jit, M. (2017). Projecting social contact matrices in 152 countries using contact surveys and demographic data. PLOS Computational Biology, 13(9), e1005697. https://doi.org/10.1371/journal.pcbi.1005697")
                  )
                ),
                column(
                  width = 7,
                  img(src = "dtpmodel.png", width = "100%")
                  
                )
              )
            ),
            ### The Team ----
            tabPanel(
              "The Team",
              fluidRow(
                column(
                  12,
                  h4("DTP Boost App Development Team"),
                  br()
                )
              ),
              h4(tags$a(href = "http://www.masha.uct.ac.za", target="_blank", "Modelling and Simulation Hub, Africa (MASHA)")),
              p("The MASHA team was responsible for the development of the epidemiological and economic models, and the development of the interactive web-based tool. The MASHA team includes Prof Sheetal Silal, Ms Rachel Hounsell, Mr Jared Norman, Prof Rudzani Muloiwa and Mr Retselisitosoe Monyake."),
              br(),
              h4(tags$a(href = "https://www.cdc.gov/globalhealth/immunization", target="_blank", "U.S. Centers for Disease Control and Prevention")),
              p("The U.S. Centers for Disease Control and Prevention’s",tags$a(href = "https://www.cdc.gov/globalhealth/immunization", target = "_blank", "Global Immunization Division")," (CDC/GID) conceptualized the project, convened the Technical Expert Group, provided technical assistance to the development of the models and tool, and funded development of the models and web-based tool.  CDC/GID team members included Dr. Kirsten Ward, Dr. Nishant Kishore, Dr. Sarah Pallas, Dr. Rania Tohme, and Dr. Aaron Wallace.  The models and tool reflect the views of the individual team members and do not represent the official position of the U.S. Centers for Disease Control and Prevention."),
              br(),
              h4(tags$a(href = "http://afenet.net", target="_blank", "African Field Epidemiology Network (AFENET)")),
              p("The African Field Epidemiological Network (AFENET) provided technical assistance and financial support to the development of the epidemiological and economic models, and development of the web-based tool. The AFENET team included Mr. Joseph Magoola, under the supervision of Dr. Nicholas Ayebazibwe. "),
              br(),
              h4("Technical Expert Group"),
              p("The role of the Technical Expert Group was to advise on the model development, epidemiology and immunology of disease, application features and dissemination."),
              tags$ul(
                tags$li(
                  style = "text-align: justify;list-style-type: disc", "Dr Ampeire Immaculate, Ugandan National Immunization Program, Ministry of Health, Government of Uganda."),
                tags$li(
                  style = "text-align: justify;list-style-type: disc", "Dr Anna Acosta, Meningitis and Vaccine Preventable Diseases Branch at U.S. Centers for Disease Control and Prevention (CDC)"),
                tags$li(
                  style = "text-align: justify;list-style-type: disc", "Dr Annet Kisakye, Expanded Program on Immunization, World Health Organization, Uganda "),
                tags$li( 
                  style = "text-align: justify;list-style-type: disc", "Prof Paula Mendes Luz, Instituto Nacional de Infectologia Evandro Chagas (INI)"),
                tags$li(
                  style = "text-align: justify;list-style-type: disc", "Dr Todi Mengistu, Measurement, Evaluation and Learning (MEL) team at Gavi, the Vaccine Alliance"),
                tags$li(
                  style = "text-align: justify;list-style-type: disc", "Dr Helen Quinn, National Centre for Immunisation Research and Surveillance (NCIRS);  University of Sydney Children’s Hospital Westmead Clinical School."),
                tags$li(
                  style = "text-align: justify;list-style-type: disc", "So Yoon Sim, Value of Vaccines, Modeling & Economics (VoV) team, Immunization Analysis & Insights (IAI) unit, Department of Immunization, Vaccines and Biologicals (IVB), World Health Organization"),
                tags$li(
                  style = "text-align: justify;list-style-type: disc", "Dr Rania Tohme, Hepatitis B and Tetanus Team in the Global Immunization Division at the U.S. Centers for Disease Control and Prevention (CDC)")
              )
            ),
            ### Contact ----
            tabPanel(
              "Contact",
              h4("Contact"),
              br(),
              p("For more information about the project or use of the DTP Boost tool, please contact Joseph Magoola (jmagoola@afenet.net).")
            )
          )
        )
      )
    ),
    ## Step 1. Set-up Country Profile ----
    tabItem(
      tabName = "model1",
      fluidRow(
        tabBox(
          id = "country_profile",
          width = 12, title = NULL,
          ### 1.1 Country ----
          tabPanel(
            title = "1.1 Country",
            value = 1,
            # br(),
            # fluidRow(column(12, img(src = "country_and_data.png", width = "100%"))),
            fluidRow(
              column(
                width = 4,
                fluidRow(
                  box(
                    width = 12, title = "Country selection",
                    selectizeInput(
                      "selectedCountry",
                      p("Select country:",actionButton("demographicsInfo", label = "", icon = icon("info-circle"), class = "btn-info")),
                      choices = split(getLocMap() %>% pull(ISO3_Code), getLocMap() %>% pull(Location)),
                      selected = c("UGA")
                    )
                  ),
                  box(
                    width = 12,
                    title = "Currency selection",
                    radioGroupButtons(
                      inputId = "selectedCurrency",
                      label = p("Select currency:",actionButton("currencyInfo", label = "", icon = icon("info-circle"), class = "btn-info")),
                      choices = c("USD"),
                      selected = "USD"
                    )
                    #uiOutput("currency_ui")
                  )
                ),
                actionButton(
                  inputId = "verify_setup",
                  label = "Verify selections",
                  width = "100%"
                ),
                fluidRow(
                  column(
                    width = 12,
                    br(),
                    p(id = "verify_text", icon("info-circle"), 
                      "Once verified, the country and currency selections cannot be changed. 
                      To change the selection, please restart the application.
                      You can restart the application by refreshing this web page.")
                  )
                )
              ),
              column(
                width = 8,
                fluidRow(
                  box(
                    width = 12, title = "Instructions for Step 1",
                    h4("Setting up a country profile"),
                    br(),
                    p("",tags$b("Process:")," Proceed through each tab to create a baseline profile that is representative of your selected country's current vaccine schedule; clinical burden of diphtheria, tetanus and pertussis; health system features; and costs."),
                    p("Each tab is pre-populated with default data and settings for the selected country, which you can replace."),
                    #p("You can only proceed to ",tags$i("Step 2. Design Booster Strategy")," if all sections have been completed (verified or replaced)."),
                    br(),
                    p("",tags$b("Default data:")," Each tab is pre-populated with default data and settings for the selected country. Click ", icon('info-circle'), " for more information on the default data source."),
                    p("You may proceed with the defaults or replace the defaults with your own data and/or settings."),
                    br(),
                    p("",tags$b("Replace defaults:"),"If you would like to replace the default data or settings, click the ",tags$b("Replace")," button under each dataset or adjust the parameters using the sliders."),
                    br(),
                    p("",tags$b("Save your country profile:"),"Once you have completed setting up the country profile, you may save a copy for future use. This will allow you to skip Step 1 when revisiting the tool.  "),
                    br(),
                    p("You may also proceed to the next steps and save your entire session (including your modelled booster strategies) at any stage using the ",tags$b("SAVE CURRENT SESSION")," button at the top right of the screen."),
                    br()
                  )
                )
              )
            )
          ),
          ### 1.2 Current Vaccine Schedule ----
          tabPanel(
            title = "1.2 Current Vaccine Schedule",
            value = 2,
            fluidRow(
              column(
                width = 2, 
                offset = 10, 
                actionButton("next_cvs", "Next", class = "btn-block")
              ),
              column(
                12,
                h3("Instructions"),
                p("Please use the options below to indicate the selected country's currently implemented vaccination schedule (target age, vaccine, delivery platform and coverage for primary series and any boosters currently in use) for diphtheria, tetanus and pertussis. Where there is not a perfect match to select, please choose the closest option."),
                p("The defaults used are sourced from the World Health Organization's Immunization Data, which show reported coverage and the vaccines in use for each country."),
                p("You may proceed with the default selections or adjust the existing schedule using the dropdown menus and upload your own coverage data using the ",tags$b("Edit Coverage Data")," button at the bottom.")
                
              )
            ),
            fluidRow(
              column(12, h3("Currently implemented vaccination schedules"), br()),
              box(
                title = "Primary series", width = 12,
                p("Use the options below to select the target age, vaccine type, and delivery location of the existing primary series."),
                fluidRow(
                  column(
                    width = 5,
                    selectizeInput(
                      inputId="va_p1",
                      label = "Dose 1 target age",
                      choices = choices_va_p1
                    ),
                    selectizeInput(
                      inputId="va_p2",
                      label = "Dose 2 target age",
                      choices = choices_va_p2
                    ),
                    selectizeInput(
                      inputId="va_p3",
                      label = "Dose 3 target age",
                      choices = choices_va_p3
                    )
                  ),
                  column(
                    width = 4,
                    offset = 1,
                    selectizeInput(
                      inputId="vv_ps",
                      label = "Primary series vaccine",
                      choices = choices_vv_ps,
                      selected = "DTwP-Hib-HepB"
                    ),
                    sliderInputReg(
                      inputId = "del_hf_ps",
                      label = "Delivery: Health facility (%)",
                      value = 69,
                      min = 0,
                      max = 100,
                      post = "%"
                    ),
                    verbatimTextOutput(outputId='del_o_ps')
                  )
                )
              ),
              box(
                title = "Booster vaccination", width = 12,
                p("Use the options below to select the target age, vaccine type, and delivery location of any existing booster doses."),
                fluidRow(
                  column(3,
                         selectizeInput(
                           inputId="va_b",
                           label = "Early childhood booster target age",
                           choices = choices_va_b,
                           multiple = FALSE
                         )
                  ),
                  column(3,
                         selectizeInput(
                           inputId="vv_b",
                           label = "Early childhood booster vaccine",
                           choices = choices_vv_b,
                           selected = "DTwP-Hib-HepB",
                           multiple = FALSE
                         )),
                  column(3,sliderInputReg(
                    inputId = "del_v_hf_b",
                    label = "Delivery: Health facility (%)",
                    value = 69,
                    min = 0,
                    max = 100,
                    post = "%"
                  )),
                  column(3, verbatimTextOutput(outputId='del_o_b'))
                ),
                fluidRow(
                  column(3,selectizeInput(
                    inputId="va_cb",
                    label = "Child booster target age",
                    choices = choices_va_cb
                  )
                  ),
                  column(3,selectizeInput(
                    inputId="vv_cb",
                    label="Child booster vaccine",
                    choices = choices_vv_cb,
                    selected = "Td"
                  )),
                  column(3,sliderInputReg(
                    inputId = "del_v_hf_cb",
                    label = "Delivery: Health facility (%)",
                    value = 10,
                    min = 0,
                    max = 100,
                    post = "%"
                  )),
                  column(3, verbatimTextOutput(outputId='del_o_cb'))
                  
                ),
                fluidRow(
                  column(3,selectizeInput(
                    inputId="va_ab",
                    label = "Adolescent booster target age",
                    choices = choices_va_ab
                  )
                  ),
                  column(3,selectizeInput(
                    inputId="vv_ab",
                    label="Adolescent booster vaccine",
                    choices = choices_vv_ab,
                    selected = "Td"
                  )),
                  column(3,sliderInputReg(
                    inputId = "del_v_hf_ab",
                    label = "Delivery: Health facility (%)",
                    value = 10,
                    min = 0,
                    max = 100,
                    post = "%"
                  )),
                  column(3, verbatimTextOutput(outputId='del_o_ab'))
                  
                ),
                fluidRow(
                  column(6,
                   selectizeInput(
                     inputId="vv_m",
                     label="Maternal vaccination (via ANC)",
                     choices = choices_vv_m,
                     selected = "Td"
                   )),
                  column(3,sliderInputReg(
                    inputId = "del_v_hf_m",
                    label = "Delivery: Health facility (%)",
                    value = 69,
                    min = 0,
                    max = 100,
                    post = "%"
                  )),
                  column(3, verbatimTextOutput(outputId='del_o_m')) #, p("*Outreach vaccination sites include schools"))
                  
                )
              ),
              box(
                width = 12, title = "Coverage",
                p("Please review coverage data in the table below. Either proceed with the default values", actionButton("covDefaults", label = "", icon = icon("info-circle"), class = "btn-info"), "or use the EDIT COVERAGE DATA button to replace the values in the table."),
                column(
                  width = 12,
                  reactableOutput(outputId = "vaccineCoverageTable"),
                  br(),
                  p("* If maternal vaccination is currently implemented, default vaccination coverage for the first dose will be ‘Antenatal care coverage - at least 1 visit’ based on WHO Global Health Observatory data. For dose 2 and above, TTCV2 data will be used.")
                )
              ),
              column(
                width = 8,
                dropdownButton(inputId = 'cov_temp',label="EDIT COVERAGE DATA",circle = FALSE, status = "primary", size = "default",
                               icon = icon("glyphicon-edit"), tooltip = FALSE, right = FALSE, up = TRUE,
                               fluidRow(
                                 column(12,
                                        h4("Edit coverage data"),
                                        br(),
                                        p("To replace the default data, please download the template by clicking the",tags$b("DOWNLOAD TEMPLATE"), "button below,
                                            make your changes on the downloaded template, and upload this file using the", tags$b("BROWSE"), "option.
                                            Add notes on the source of your data or any other relevant details. "),
                                        br(),
                                        h5("1. Download template"),
                                        shiny::checkboxInput(
                                          inputId = "templatevaxcov_israw",
                                          label = tags$span('Download Raw Data',
                                                            actionButton("rawCoverageDataInfo", label = "", icon = icon("info-circle"), class = "btn-info")),
                                          value = F
                                        ),
                                        shiny::downloadButton(
                                          outputId = "templatevaxcov",
                                          label = 'Download'
                                        ),
                                        br(),
                                        br(),
                                        h5("2. Upload changes"),
                                        shiny::fileInput(
                                          inputId = "vaxInput",
                                          label = NULL,
                                          buttonLabel = list(icon("fas fa-upload"),"Browse"),
                                          accept='.xlsx'
                                        ),
                                        h5("3. Add notes"),
                                        textAreaInput(
                                          inputId = 'notes_cov_vacc_custom',
                                          label = NULL,
                                          width = "100%"
                                        )
                                 )
                               )
                ),
                br()
              ),
              box(
                width = 12,
                title = "Notes",
                h5("Please add any notes or comments here (e.g. details of the source of data used)"),
                textAreaInput(
                  inputId = 'notes_cov_vacc_default',
                  label = NULL,
                  width = "100%",
                  height = "100px"
                )
              )
            )
          ),
          ### 1.3 Health System ----
          tabPanel(title = '1.3. Health System', value = 3,
                   fluidRow(
                     column(
                       width = 2, 
                       offset = 10,
                       actionButton("next_hs", "Next", class = "btn-block")
                     )
                   ),
                   fluidRow(
                     box(width = 12, title = "DTP Treatment",
                         fluidRow(
                           column(12, "Use the options below to select how likely it is that if cases are present, a mild/severe case will be treated."),
                           column(4,"Diphtheria",
                                  sliderInput(post='%', inputId="p_mt_D", label="Probability of a mild case being treated (outpatient)", min=0, max=100, value=40),
                           ),
                           column(4,"Pertussis",
                                  sliderInput(post='%', inputId="p_mt_P", label="Probability of a mild case being treated (outpatient)", min=0, max=100, value=30)),
                           column(4,"Tetanus",
                                  sliderInput(post='%', inputId="p_mt_T", label="Probability of a mild case being treated (outpatient)", min=0, max=100, value=50)
                           )
                         ),
                         fluidRow(
                           column(4,
                                  sliderInput(post='%', inputId="p_st_D", label="Probability of a severe case being treated (inpatient)", min=0, max=100, value=90)
                           ),
                           column(4,
                             sliderInput(post='%', inputId="p_st_P", label="Probability of a severe case being treated (inpatient)", min=0, max=100, value=90)
                           ),
                           column(4,
                                  sliderInput(post='%', inputId="p_st_T", label="Probability of a severe case being treated (inpatient)", min=0, max=100, value=98)
                           )
                         )
                     )
                   ),
                   br(),
                   fluidRow(
                     box(width = 12, title = "DTP Reporting",
                         fluidRow(
                           column(12, "Use the options below to select (1) how likely it is that if symptomatic cases are present, they will be diagnosed, and (2) how likely it is that if deaths occur,  they will be reported and classifed as caused by the disease."),
                           column(4, "Diphtheria",
                                  autonumericInput(suffixText='%', inputId="p_rep_clin_D", label="Probability of symptomatic cases being diagnosed", minimumValue=0, maximumValue =100, value=10, decimalPlaces = 2),
                                  sliderInput(post='%', inputId="p_rep_death_D", label="Probability of deaths being reported", min=0, max=100, value=85)),
                           column(4, "Pertussis",
                                  autonumericInput(suffixText='%', inputId="p_rep_clin_P", label="Probability of symptomatic cases being diagnosed", minimumValue=0, maximumValue=100, value=0.1, decimalPlaces = 2),
                                  sliderInput(post='%', inputId="p_rep_death_P", label="Probability of deaths being reported", min=0, max=100, value=60)),
                           column(4, "Tetanus",
                                  autonumericInput(suffixText='%', inputId="p_rep_clin_T", label="Probability of symptomatic cases being diagnosed", minimumValue=0, maximumValue=100, value=90, decimalPlaces = 2),
                                  sliderInput(post='%', inputId="p_rep_death_T", label="Probability of deaths being reported", min=0, max=100, value=100))
                         )
                     )
                   ),
                   br(),
                   fluidRow(
                     box(
                       width = 12,
                       title = "Notes",
                       h5("Please add any notes or comments here (e.g. details of the source of data used)"),
                       textAreaInput(
                         inputId = 'notes_health_system',
                         label = NULL,
                         width = "100%",
                         height = "100px"
                       )
                     )
                   )
          ),
          ### 1.4 Costs ----
          tabPanel(
            title = "1.4 Costs",
            value = 4,
            fluidRow(
              column(
                width = 2,
                offset = 10,
                actionButton("next_costs", "Next", class = "btn-block")
              ),
              column(
                width = 12,
                h3("Instructions"),
                p("Please use the options below to indicate relevant cost data for DTP immunisation and cost of illness estimates.
                    You can use the defaults or replace with your own cost estimates using the slider and input boxes below."),
                p(),
                p("For more details, please click the information button.", actionButton("costs_info", label = "", icon = icon("info-circle"), class = "btn-info")),
                br()
              ),
              column(
                #offset = 9,
                width = 2,
                autonumericInput(
                  inputId = "discount",
                  label = "Discount rate (%)",
                  align = "left",
                  value = 3.5,
                  minimumValue = 0
                )
              )
            ),
            fluidRow(
              column(
                width = 12,
                h3("Cost of vaccination (delivery + vaccine)"),
                br()
              ),
              column(
                width = 12,
                fluidRow(
                  box(
                    width = 12,
                    title = "Cost of routine delivery (per dose)",
                    p("Average cost per dose of routine vaccine delivery (delivery only, excluding vaccine cost)"),
                    div(id = "del_ps", costsDelRowUI("Primary series", "ps", pfin = 22)),
                    div(id = "del_b", costsDelRowUI("Early childhood booster", "b", pfin = 22)) %>% shinyjs::hidden(),
                    div(id = "del_cb", costsDelRowUI("Child booster", "cb", pfin = 22)) %>% shinyjs::hidden(),
                    div(id = "del_ab", costsDelRowUI("Adolescent booster", "ab", pfin = 22)) %>% shinyjs::hidden(),
                    div(id = "del_m", costsDelRowUI("Maternal vaccination (via ANC)", "m", pfin = 22)) %>% shinyjs::hidden()
                  )
                )
              )
            ),
            br(),
            fluidRow(
              box(
                width = 12,
                title = "Cost of vaccines (per dose)",
                p("Cost of vaccine per dose (unit cost of vaccine only, no delivery cost)"),
                div(id = "vax_ps", costsVaxRowUI("Primary series", "ps", pfin = 25)),
                div(id = "vax_b", costsVaxRowUI("Early childhood booster", "b", pfin = 25)) %>% shinyjs::hidden(),
                div(id = "vax_cb", costsVaxRowUI("Child booster", "cb", pfin = 100)) %>% shinyjs::hidden(),
                div(id = "vax_ab", costsVaxRowUI("Adolescent booster", "ab", pfin = 100)) %>% shinyjs::hidden(),
                div(id = "vax_m", costsVaxRowUI("Maternal vaccination", "m", pfin = 100)) %>% shinyjs::hidden()
              )
            ),
            fluidRow(
              column(
                width = 12,
                h3("Cost of illness"),
                br()
              ),
              column(
                width = 4,
                fluidRow(
                  box(
                    width = 12,
                    title = "Diphtheria",
                    autonumericInput(
                      inputId = "outp_cost_D",
                      label = "Cost per outpatient case (USD)",
                      value = 50,
                      decimalCharacter = ".",
                      digitGroupSeparator = ",",
                      decimalPlaces = 3,
                      align = "left",
                      minimumValue = 0
                    ),
                    autonumericInput(
                      inputId = "inp_cost_D",
                      label = "Cost per inpatient case (USD)",
                      value = 111,
                      decimalCharacter = ".",
                      digitGroupSeparator = ",",
                      decimalPlaces = 3,
                      align = "left",
                      minimumValue = 0
                    )
                  )
                )
              ),
              column(
                width = 4,
                fluidRow(
                  box(
                    width = 12,
                    title = "Pertussis",
                    autonumericInput(
                      inputId = "outp_cost_P",
                      label = "Cost per outpatient case (USD)",
                      value = 50,
                      decimalCharacter = ".",
                      digitGroupSeparator = ",",
                      decimalPlaces = 3,
                      align = "left",
                      minimumValue = 0
                    ),
                    autonumericInput(
                      inputId = "inp_cost_P",
                      label = "Cost per inpatient case (USD)",
                      value = 111,
                      decimalCharacter = ".",
                      digitGroupSeparator = ",",
                      decimalPlaces = 3,
                      align = "left",
                      minimumValue = 0
                    )
                  )
                )
              ),
              column(
                width = 4,
                fluidRow(
                  box(
                    width = 12,
                    title = "Tetanus",
                    autonumericInput(
                      inputId = "outp_cost_T",
                      label = "Cost per outpatient case (USD)",
                      value = 50,
                      decimalCharacter = ".",
                      digitGroupSeparator = ",",
                      decimalPlaces = 3,
                      align = "left",
                      minimumValue = 0
                    ),
                    autonumericInput(
                      inputId = "inp_cost_T",
                      label = "Cost per inpatient case (USD)",
                      value = 125,
                      decimalCharacter = ".",
                      digitGroupSeparator = ",",
                      decimalPlaces = 3,
                      align = "left",
                      minimumValue = 0
                    )
                  )
                )
              )
            ),
            br(),
            fluidRow(
              box(
                width = 12,
                title = "Notes",
                h5("Please add any notes or comments here (e.g. details of the source of data used)"),
                textAreaInput(
                  inputId = 'notes_costs',
                  label = NULL,
                  width = "100%",
                  height = "100px"
                )
              )
            )
          )
        )
      )
    ),
    ## Step 2. Calibrate Model ----
    tabItem(
      tabName = "model2",
      fluidRow(
        tabBox(
          id = "calibration_tab",
          width = 12, title = NULL,
          ### Model Calibration ----
          tabPanel(
            title = "2.1 Select data",
            value = 1,
            fluidRow(
              column(
                width = 12,
                h3("Instructions"),
                p("Please use the options below to select the incidence dataset to be used for model fitting. Model fitting is conducted to validate that the transmission model produces estimates that are similar to observed data. A fitted model adds validity to predictions of the impact of vaccination. The reporting rate specified in the Health System section will be used to correct for under-reporting of diagnosed cases."),
                p("Two data options are available: 1) WHO observed incidence data and 2) Global Burden of Disease incidence estimates. ", actionButton("calData", label = "", icon = icon("info-circle"), class = "btn-info"), " If you prefer to upload your own incidence data, you may do so in the ‘Upload your data’ box below. "),
                p("Given that unpredictable external drivers often seed diphtheria outbreaks, calibration is applied to pertussis and tetanus only. It is important to interrogate the datasets to determine if data are valid given local experience" ,tags$i("for both pertussis and tetanus"), "to fit the model. ")
              )
            ),
            
            br(),
            fluidRow(
              box(
                title = strong("Choose a dataset for calibration"),
                p("Use the buttons to select a dataset for calibration that best fits your country's profile."),
                width = 12,
                fluidRow(
                  column(
                    offset = 0,
                    width = 3,
                    radioButtons(
                      inputId = "data_source_calibration",
                      #label = "Select your data for calibration",
                      label = NULL,
                      choices = c("WHO", "GBD", "OWN DATA"),
                      selected = "GBD"
                      #        status = "calibration_btns"
                    )
                  ),
                  column(
                    offset = 0,
                    width = 3,
                    actionButton(
                      inputId = "go_to_calibration",
                      label = "Go to calibration"
                    )
                  )
                ),
                fluidRow(
                  column(
                    width = 12,
                    p("The plots below display the ",
                       strong("WHO data"), "and", strong('GBD estimates'), " to help you select the best approach. The option to upload your own data is shown under the heading", strong('Upload your data.'))
                  )
                )
              )
            ),
            br(),
            fluidRow(
              column(
                width = 12,
                fluidRow(
                  box(
                    title = "WHO data: Global Health Observatory annual reported incidence ",
                    width = 12,
                    #column(12, reactableOutput("clinical_burden_table"))
                    plotlyOutput("who_clinical_burden_plot")
                  )
                )
              ),
              column(
                width = 12,
                fluidRow(
                  box(
                    title = "GBD estimates: Global Burden of Disease estimates of annual clinical incidence",
                    width = 12,
                    plotlyOutput("gbd_clinical_burden_plot")
                  )
                )
              ),
              column(
                width = 12,
                fluidRow(
                  box(
                    title = "Upload your data",
                    width = 12,
                    fluidRow(
                      column(
                        width = 8,
                        #h4("Upload own data"),
                        p(""),
                        p("To replace the default data, please download the template by clicking the",tags$b("DOWNLOAD TEMPLATE"), "button on the right,
                          make your changes on the downloaded template, and upload this file using the", tags$b("BROWSE"), "option.
                          Add notes on the source of your data or any other relevant details. "),
                        p("Once you have uploaded your own data, return to the top, make sure ", tags$b("OWN DATA"), "is selected and click ",tags$b("GO TO CALIBRATION"), "to continue."),
                        br(),
                        h5("Add notes"),
                        textAreaInput(
                          inputId = 'notes_calibration',
                          label = NULL,
                          width = "100%"
                        )
                      ),
                      column(
                        width = 4,
                        h5("1. Download template"),
                        shiny::downloadButton(
                          outputId = "template_burden",
                          label = 'Download'
                        ),
                        br(),
                        br(),
                        h5("2. Upload changes"),
                        shiny::fileInput(
                          inputId = "burden_input",
                          label = NULL,
                          buttonLabel = list(icon("fas fa-upload"),"Browse"),
                          accept='.xlsx'
                        )
                      )
                    )
                  )
                )
              )
            )
          ),
          tabPanel(
            title = "2.2 Calibration",
            value = 2,
            fluidRow(
              column(
                12,
                h3("Instructions"),
                p("The aim of the manual calibration is to adjust the slider values for both pertussis and tetanus until the model output resembles the data. You will need to press the", tags$strong("RUN THE MODEL"), "button after each adjustment of the sliders."),
                p("This manual calibration is a simple form of face validation, which qualitatively assesses if the model is an adequate representation of the data. "),
                p("If you are happy with your calibration, press the",tags$strong("ACCEPT THIS CALIBRATION AND MOVE ON"), "button to move to Step 3: Booster Strategy Design."),
                br()
              )
            ),
            fluidRow(
              column(
                width = 3,
                tags$span(class="hideThis",
                          sliderInput(
                            inputId = "ptrans_D",  # This should be hidden (in www/style.css) since we don't view diphtheria in calibration currently
                            label = "Diphtheria transmission tuning parameter",
                            min = 1,
                            max = 1000,
                            value = 275
                          )
                ),
                sliderInput(
                  inputId = "ptrans_T",
                  label = "Tetanus transmission tuning parameter",
                  min = 1,
                  max = 50,
                  value = 12, 
                  step = 0.1
                ),
                sliderInput(
                  inputId = "ptrans_P",
                  label = "Pertussis transmission tuning parameter",
                  min = 1,
                  max = 100,
                  value = 25, 
                  step = 0.1
                ),
                br(),
                actionButton(
                  inputId = "calibrate_manual",
                  label = "Run the model",
                  width = "100%"
                )
              ),
              column(
                width = 9,
                uiOutput("manual_calibration")
              )
            ),
            br(),
            fluidRow(
              column(
                offset = 8,
                width = 4,
                actionButton(
                  inputId = "accept_manual_calibration",
                  label = "ACCEPT THIS CALIBRATION AND MOVE ON"
                )
              )
            )
          ),
          tabPanel(
            title = "2.3  Assessing uncertainty",
            value = 3,
            fluidRow(
              column(
                width = 2,
                offset = 10,
                actionButton("next_uncertainty", "Next", class = "btn-block")
              ),
              column(
                12,
                h3("Instructions"),
                p("As models are simplifications of the real world, it is important to assess the variability of the model predictions in relation to the decision you wish to inform from model evidence. Guidance has been provided below on how to use the DTP Boost tool to test the robustness of model predictions."),
                br(),
                h4("Many sources of uncertainty"),
                p("Uncertainty exists in many forms in modelling including:"),
                tags$ul(
                  tags$li("Uncertainty in the model inputs such as the those entered into the sliders and information boxes."),
                  tags$li("Uncertainty in the case incidence data that is used to tune the model in the calibration process to create the baseline scenario."),
                  tags$li("Uncertainty in the reduction of effectiveness of the vaccine booster strategies due to operational implementation."),
                  tags$li("Uncertainty in the estimates of cost and cost effectiveness of the vaccine booster strategies.")
                ),
                br(),
                h4("Using the DTP Boost tool"),
                p("Consider the example of using the DTP Boost tool to determine if the country should introduce a new adolescent booster dose."),
                p("The baseline scenario is important to establish correctly as it sets the foundation against which new strategies are explored and measured."),
                br(),
                p("To develop a robust baseline scenario:"),
                tags$ul(
                  tags$li("Vary the input values such as annual vaccine coverage and health system characteristics to see how the baseline changes."),
                  tags$li("Place emphasis on varying those input values you are most uncertain about. See how the baseline changes for a range of plausible values.")
                ),
                br(),
                p("The value of the calibration is to position the baseline scenario in line with observed/estimated cases. It is important to choose the dataset for calibration carefully as this dataset should best describe the incidence in the population for both pertussis and tetanus."),
                tags$ul(
                  tags$li("Where data does not exist, provide insight by uploading plausible expert-informed pseudo-data using the 'Upload your data' feature. Try to capture different trends and patterns in incidence."),
                  tags$li("Fit the model to multiple datasets to define several baselines. You can use the 'Save your session' feature at the top right of the page to save your different baseline scenarios. You can then upload each session separately and test out the intended adolescent booster strategy.")
                ),
                br(),
                p("To develop a robust booster scenario:"),
                tags$ul(
                  tags$li("Because there can be a decrease in vaccine impact due to operational challenges during implementation, you should test the booster scenario for a range of coverage values."),
                  tags$li("Consider different target age groups if the intended age group may be difficult to reach or characterise."),
                  tags$li("As potential implementation and vaccine costs are not always known in advance, run the scenario several times varying the input cost values. Note the impact that changing input costs has on the total cost and cost effectiveness.")
                ),
                br(),
                p("In this manner, assessing the uncertainty will allow you to:"),
                tags$ul(
                  tags$li("Gain insight into which health system and cost inputs are most important to estimate correctly. "),
                  tags$li("Understand what sources of uncertainty have a large impact on the decision to introduce the new adolescent booster dose. ")
                ),
                br()
              )
            )
          )
        )
      )
    ),
    ## Step 3. Booster Strategy ----
    tabItem(
      tabName = "model3",
      fluidRow(
        column(
          width = 12,
          tabBox(
            width = NULL, 
            title = NULL,
            id = "booster_strategy_tab",
            ### Instructions ----
            tabPanel(
              title = "3.1 Instructions",
              value = 1, 
              fluidRow(
                column(
                  width = 2,
                  offset = 10,
                  actionButton("next_dbs", "Next", class = "btn-block")
                )
              ),
              fluidRow(
                box(
                  width = 12, title = "Designing a booster vaccination strategy",
                  # h4("Designing a booster vaccination strategy"),
                  # p("",tags$b("You may design, save and simulate as many strategies as you would like to compare and explore in the results section (Step 3).")," "),
                  br(),
                  p("You may design, save and simulate as many strategies as you would like to compare and explore in the results section (Step 4). "),
                  br(),
                  p("",tags$b("Build package:")," To design a vaccination strategy, toggle on the options you would like to include. Once in the 'on' position, a settings button will become visible. This allows you to adjust the details of each option (see below). Defaults are provided but should be updated to reflect your selected strategy and context."),
                  br(),
                  p("",tags$b("Vaccination options:")," You may build a vaccination strategy using any number of the following options. Click ", icon('info-circle'), " in the next section for more information on each."),
                  p("",tags$li("Early childhood booster: recommended dose given between 12 - 23 months old"),""),
                  p("",tags$li("Child booster: recommended dose given between 4 - 7 years old"),""),
                  p("",tags$li("Adolescent booster: recommended dose given between 9 - 15 years old"),""),
                  p("",tags$li("Maternal vaccinaton: dose(s) given to women during antenatal care (ANC) visits"),""),
                  # p("",tags$li("Mass vaccination: campaign aimed at reaching large groups in the population over a short duration"),""),
                  br(),
                  p("",tags$b("Vaccination settings:")," You may define the following settings for each vaccination option you include in your strategy. Click ", icon('info-circle'), " in the next section for more information on each."),
                  p("",tags$li("Target age"),""),
                  p("",tags$li("Vaccine "),""),
                  p("",tags$li("Year of introduction"),""),
                  p("",tags$li("Coverage in year of introduction"),""),
                  p("",tags$li("Target operational coverage"),""),
                  p("",tags$li("Years to reach target coverage"),""),
                  p("",tags$li("Average cost per dose"),""),
                  p("",tags$li("Proportion paid directly by government"),""),
                  p("",tags$li("Delivery platform (Health facility or Outreach site)"),""),
                  br(),
                  p("",tags$b("Save & simulate:")," Once you have completed designing a strategy, name it in the box (bottom right) and click ",tags$b("Run.")," This strategy will be visible in the results section. If you ",tags$b("SAVE CURRENT SESSION")," any time after setting vaccination strategies to simulate, these will be stored as part of your saved session."),
                  br()
                )
              )
            ), #/tabPanel
            ### Build a vaccination strategy ----
            tabPanel(
              title = "3.2 Build a vaccination strategy",
              value = 2,
              br(),
              p("To design a vaccination strategy, toggle on the options you would like to include. Once in the 'on' position, a settings button will become visible. This allows you to adjust the details of each option (see below). Defaults are provided but should be updated to reflect your selected strategy and context."),
              fluidRow(
                if (shouldIncludeDebugWidgets) {
                  column(12,
                         selectInput(inputId='dbgSelectMat', label='DEBUG:Show Coverage',
                                     choices = c('cov','cov1yr','cov2yr','cov3yr','covbyr','covcbyr','covabyr','covm', 'covm2',
                                                 'mov_D','mov_T','mov_P','movm_D','movm_T','movm_P'),
                                     selected = 'cov'
                         ),
                         checkboxInput(inputId='dbgIsBaseline', label='DEBUG: Show baseline', value = FALSE),
                         imageOutput(outputId = 'dbgMat')
                  )
                } else {
                  column(12)  # empty column
                },
                column(
                  width = 12,
                  h4("DTP Booster doses", actionButton("dtpBoosterInfo", label = "", icon = icon("info-circle"), class = "btn-info")),
                  source("./www/R/ui/infant_booster.R", local = TRUE)$value,
                  source("./www/R/ui/child_booster.R", local = TRUE)$value,
                  source("./www/R/ui/adolescent_booster.R", local = TRUE)$value
                ),
                column(
                  width = 12,
                  h4("Other DTP Vaccinations", actionButton("dtpVaccinationsInfo", label = "", icon = icon("info-circle"), class = "btn-info")),
                  source("./www/R/ui/maternal_booster.R", local = TRUE)$value
                  #source("./www/R/ui/catchup_campaign.R", local = TRUE)$value,
                  #source("./www/R/ui/mass_vaccination.R", local = TRUE)$value
                )
              ),
              br(),
              p("Once you have built your vaccination package, please indicate the anticipated cost of introducing new dose(s). You only need to complete this information once. The introduction cost is the average total cost of introducing new vaccine dose(s) in a given year. This is a fixed cost regardless of the number of doses introduced in the introduction year. Please indicate whether this cost will only be incurred once, even if additional booster doses are introduced in future years."),
              fluidRow(
                column(
                  width = 12,
                  h4("Cost of introduction (total)", actionButton("dtpCostofIntroInfo", label = "", icon = icon("info-circle"), class = "btn-info")),
                  fluidRow(
                    box(
                      id = "intro_cost_box",
                      width = 6,
                      title = NULL,
                      p(" Average total once-off cost of introducing new vaccine dose(s) in a given year (fixed cost regardless of number of doses introduced in the introduction year)."),
                      fluidRow(
                        column(
                          width = 6,
                          autonumericInput(
                            inputId = "strat_intro_cost",
                            label = "Fixed cost (USD)",
                            value = 1880744,
                            decimalCharacter = ".",
                            digitGroupSeparator = ",",
                            decimalPlaces = 3,
                            align = "left",
                            minimumValue = 0
                          )
                        )
                      ),
                      fluidRow(
                        column(
                          width = 10,
                          sliderInputReg(
                            inputId = "strat_intro_cost_pfin",
                            label = "Proportion of fixed introduction costs paid directly by government",
                            min = 0,
                            max = 100,
                            value = 30,
                            post = "%"
                          )
                        )
                      ),
                      fluidRow(
                        column(
                          width = 12,
                          checkboxInput(
                            inputId = "strat_intro_applyonce", 
                            label = "Select if the introduction cost will only be incurred once", 
                            value = TRUE, 
                            width = NULL
                          )
                        )
                      )
                    )
                  )
                )
              ),
              div(
                class = "float-go-interv",
                div(
                  class = "panel panel-success",
                  div(class = "panel-heading", "Save and simulate strategy"),
                  div(
                    class = "panel-body",
                    fluidRow(
                      column(
                        width = 9,
                        span(
                          "Strategy name:",
                          textInput("package_name", label = NULL,
                                    placeholder = "e.g. Early childhood + Child", width = "250px")
                        )
                      ),
                      column(
                        width = 3,
                        br(), br(),
                        actionButton("go_interventions", "Run", class="btn btn-success")
                      )
                      
                    )
                  )
                )
              )
            )
          )
        )
      )
    ),
    ## Step 4. Results ----
    tabItem(
      tabName = "model4",
      fluidRow(
        column(
          width = 12,
          tabBox(
            width = NULL, title = NULL, id = 'panel_costs',
            ### 4.1 Epi Outputs ----
            tabPanel(
              "4.1 Epidemiological Output",
              fluidRow(
                column(
                  width = 4,
                  fluidRow(
                    box(
                      id = "packages",
                      width = 12,
                      title = "Baseline & Strategies (Display/Hide)",
                      uiOutput("select_packages"),
                      uiOutput("delete_packages")
                    )
                  ),
                  fluidRow(
                    box(
                      width = 12,
                      title = "Model horizon",
                      selectInput(
                        inputId = "model_horizon",
                        label = "Select model horizon",
                        choices = c("5 years", "10 years", "15 years"),
                        selected = "15 years"
                      ),
                      p(
                        id = "model_horizon_text", 
                        icon("info-circle"), 
                        "The selection you make here will also apply to both the", 
                        tags$strong("Budget Impact"),
                        "and", 
                        tags$strong("Cost Effectiveness"), 
                        "pages."
                      )
                    )
                  ),
                  fluidRow(
                    box(
                      width = 12,
                      title = "Output options",
                      selectInput(
                        inputId = "epi_output_variable",
                        label = "Select model output",
                        # TODO: Get choices from parametersDTP.xlsx outputs sheet?
                        # this is where they are linked to variables in the models
                        choices = c(
                          "Population protected",
                          "Reported cases",
                          "Reported deaths",
                          "Clinical cases",
                          "Deaths",
                          "All treated cases (inpatient & outpatient)",
                          "Inpatient cases",
                          "Outpatient cases"
                        )
                      ),
                      checkboxInput(
                        inputId = "epi_switch_y_axis",
                        label = "Fix vertical axis range ",
                        value = FALSE
                      ),
                      
                      radioGroupButtons(
                        inputId = "epi_output_count_method",
                        label = "View output as",
                        choices = c("Count"="Total", "Per 100,000"="Rate")
                      ),
                      selectizeInput(
                        label="Age groups",
                        inputId="epi_output_age_group",
                        choices=c("All", ageGroups)
                      )
                      # sliderInput(
                      #   label="Plot time window",
                      #   inputId="epi_output_sim_window",
                      #   min=2023, max=2039,
                      #   value=c(2025,2039), sep="")
                    )
                  )
                ),
                column(8, tabsetPanel(
                  id = "results_output",
                  title = NULL,
                  tabPanel(
                    "All",
                    fluidRow(
                      column(
                        width = 12,
                        br(),
                        p("This section allows the user to explore key model outputs such as clinical cases, deaths, reported cases and deaths, treatment and population protected. Please use the menu on the left to select the model output you would like to view and to change how the results are displayed in the plot below."),
                        p(strong("NOTE: As diphtheria is a disease prone to outbreaks, it often has very low (often zero) cases in periods outside of outbreaks. The model is not designed to predict the future occurrence of diphtheria outbreaks. Rather it is intended to model the level of vaccine-derived protection in the population. Therefore, model output should be interpreted in this context."),  style = "font-size:15px !important; color:#F5793A")
                      )
                    ),
                    
                    fluidRow(column(12,  br(),plotlyOutput("model_plot", height = "600px") %>% withSpinner(color = "#237D80"))),
                    #br(),
                    fluidRow(
                      column(
                        width = 12,
                        br(),
                        p("The table below shows the total number of clinical cases and total number of deaths for diphtheria, tetanus and pertussis combined for the full model time frame",tags$span(id = "epi_table_text", "(15 years starting in 2025).") , "The baseline (existing schedule with no new doses added) and user-defined strategies are presented. Cases and deaths averted compare each strategy to the baseline.")
                      )
                    ),
                    br(),
                    h4("Model outputs (diphtheria, tetanus and pertussis)*"),
                    fluidRow(column(12, reactableOutput("model_tbl"))),
                    br(),
                    p(
                      id = "epi_out_all",
                      "* Outputs are shown as the total for the full model timeframe (2025 - 2039)"
                    ),
                    p("** Compared to Baseline")
                  ),
                  tabPanel(
                    "Diphtheria",
                    fluidRow(
                      column(
                        width = 12,
                        br(),
                        p("This section allows the user to explore key model outputs such as clinical cases, deaths, reported cases and deaths, treatment and population protected. Please use the menu on the left to select the model output you would like to view and to change how the results are displayed in the plot below."),
                        p(strong("NOTE: As diphtheria is a disease prone to outbreaks, it often has very low (often zero) cases in periods outside of outbreaks. The model is not designed to predict the future occurrence of diphtheria outbreaks. Rather it is intended to model the level of vaccine-derived protection in the population. Therefore, model output should be interpreted in this context."),  style = "font-size:15px !important; color:#F5793A")
                      )
                    ),
                    
                    fluidRow(column(12, br(),plotlyOutput("dip_model_plot") %>% withSpinner(color = "#237D80"))),
                    br(),
                    fluidRow(
                      column(
                        width = 12,
                        br(),
                        p("The table below shows the total number of clinical cases and total number of deaths for diphtheria only, for the full model time frame (15 years starting in 2025). The baseline (existing schedule with no new doses added) and user-defined strategies are presented. Cases and deaths averted compare each strategy to the baseline.")
                      )
                    ),
                    br(),
                    h4("Model outputs (diphtheria)*"),
                    fluidRow(column(12, reactableOutput("dip_model_tbl"))),
                    br(),
                    p(
                      id = "epi_out_diphtheria",
                      "* Outputs are shown as the total for the full model timeframe (2025 - 2039)"
                    ),
                    p("** Compared to Baseline")
                  ),
                  tabPanel(
                    "Pertussis",
                    fluidRow(
                      column(
                        width = 12,
                        br(),
                        p("This section allows the user to explore key model outputs such as clinical cases, deaths, reported cases and deaths, treatment and population protected. Please use the menu on the left to select the model output you would like to view and to change how the results are displayed in the plot below.")
                      )
                    ),
                    
                    fluidRow(column(12, br(),plotlyOutput("pert_model_plot") %>% withSpinner(color = "#237D80"))),
                    br(),
                    fluidRow(
                      column(
                        width = 12,
                        br(),
                        p("The table below shows the total number of clinical cases and total number of deaths for pertussis only, combined for the full model time frame (15 years starting in 2025). The baseline (existing schedule with no new doses added) and user-defined strategies are presented. Cases and deaths averted compare each strategy to the baseline.")
                      )
                    ),
                    br(),                   
                    h4("Model outputs (pertussis)*"),
                    fluidRow(column(12, reactableOutput("pert_model_tbl"))),
                    br(),
                    p(
                      id = "epi_out_pertussis",
                      "* Outputs are shown as the total for the full model timeframe (2025 - 2039)"
                    ),
                    p("** Compared to Baseline")
                  ),
                  tabPanel(
                    "Tetanus",
                    fluidRow(
                      column(
                        width = 12,
                        br(),
                        p("This section allows the user to explore key model outputs such as clinical cases, deaths, reported cases and deaths, treatment and population protected. Please use the menu on the left to select the model output you would like to view and to change how the results are displayed in the plot below.")
                      )
                    ),
                    
                    fluidRow(column(12, br(),plotlyOutput("tet_model_plot") %>% withSpinner(color = "#237D80"))),
                    br(),
                    fluidRow(
                      column(
                        width = 12,
                        br(),
                        p("The table below shows the total number of clinical cases and total number of deaths for tetanus only, combined for the full model time frame (15 years starting in 2025). The baseline (existing schedule with no new doses added) and user-defined strategies are presented. Cases and deaths averted compare each strategy to the baseline.")
                      )
                    ),
                    br(),
                    h4("Model outputs (tetanus)*"),
                    fluidRow(column(12, reactableOutput("tet_model_tbl"))),
                    br(),
                    p(
                      id = "epi_out_tetanus",
                      "* Outputs are shown as the total for the full model timeframe (2025 - 2039)"
                    ),
                    p("** Compared to Baseline")
                  )
                )
                )
                
              )
            ),
            ### 4.2 Budget Impact ----
            tabPanel(
              "4.2 Budget Impact",
              fluidRow(
                column(
                  width = 4,
                  fluidRow(
                    box(
                      id = "eo_packages",
                      width = 12,
                      title = "Baseline & Strategies (Display/Hide)",
                      uiOutput("eo_select_packages")
                    )
                  ),
                  #tags$head(tags$style('#packages .box-header{ display: none}')),  # target the box header
                  fluidRow(
                    box(
                      width = 12,
                      title = "Output options",
                      selectInput(
                        inputId = "eco_bi_cost_type",
                        label = "Costing approach",
                        choices = c(
                          "Proportion paid by government", 
                          "Full cost (donor and government)"
                        ),
                        selected = "Proportion paid by government"
                      ),
                      selectInput(
                        inputId = "eco_bi_discount",
                        label = "Discounting",
                        choices = c("Discounted costs and benefits", 
                                    "Undiscounted costs and benefits"),
                        selected = "Undiscounted costs and benefits"
                      ),
                      selectInput(
                        inputId = "eco_bi_scenario",
                        label = "Scenario for net cost comparison",
                        choices = NULL,
                        selected = NULL
                      )
                    )
                  )
                ),
                column(8,
                       tabsetPanel(
                         id = "eo_results_output",
                         title = NULL,
                         tabPanel(
                           "All",
                          
                           fluidRow(
                             column(
                               width = 12,  
                               br(),
                               p("The annual net total cost shows the yearly cost of each user-defined scenario compared to the baseline (existing schedule with no new doses added). These costs include both the cost of the vaccination programme (cost of vaccines, routine delivery costs and cost of introducing new doses if relevant) and the cost of illness (treating inpatient and outpatient cases of diphtheria, tetanus and pertussis). As introducing booster doses is anticipated to reduce cases, this leads to a lower cost of illness due to averted treatment costs. This is shown as the green bar extending below zero. The black line shows the annual net total (the difference between higher vaccination cost and lower treatment cost, compared to the baseline)."),
                               p("This plot shows the net total cost for one scenario at a time. Please use the dropdown menu on the left to select the scenario you would like to view.")
                             )
                           ),
                           fluidRow(
                             column(
                               width = 12,  
                               br(),
                               plotlyOutput("eo_model_plot") %>% withSpinner(color = "#237D80"),
                               br()
                             )
                           ),
                           fluidRow(
                             column(
                               width = 12,  
                               br(),
                               p("The net total costs plot below shows the yearly cost of each user-defined strategy compared to the baseline. Total costs include both treatment costs averted and the cost of the vaccination programme (cost of vaccines, routine delivery costs and cost of introducing new doses if relevant). The black 'Net total cost' line in the plot above, for each scenario, are shown here for easy comparison.")
                               #br()
                             )
                           ),
                           fluidRow(
                             column(
                               width = 12,
                               br(),
                               plotlyOutput("bi_net_total_cost_plot"),
                               br()
                             )
                           ),
                           fluidRow(
                             column(
                               width = 12,  
                               br(),
                               p("The total annual cost of vaccination shows the yearly cost of the vaccination programme (cost of vaccines, routine delivery costs and cost of introducing new doses if relevant). The baseline (existing schedule with no new doses added) and user-defined scenarios are presented for comparison. Note that this plot shows the cost of vaccination only, not the cost of illness."),
                               p("Please use the dropdown menu on the left to select whether you would like to view the entire cost or only the proportion of the cost paid by government (based on the inputs given by the user in Step 1 and Step 3).")
                               #br()
                             )
                           ),
                           fluidRow(
                             column(
                               width = 12,  
                               br(),
                               plotlyOutput("eo_model_plot2") %>% withSpinner(color = "#237D80")
                             )
                           ),
                           br(),
                           br(),
                           h4(id = "eo_model_tbl_title", "Projected costs by component (total cost for 2023 - 2038)"),
                           fluidRow(column(12, reactableOutput("eo_model_tbl")))
                         )
                       )
                )
              )
            ),
            ### 4.3 Cost Effectiveness ----
            tabPanel(
              "4.3 Cost Effectiveness",
              fluidRow(
                column(
                  width = 4,
                  fluidRow(
                    box(
                      id = "eo2_packages",
                      width = 12,
                      title = "Baseline & Strategies (Display/Hide)",
                      uiOutput("eo2_select_packages")
                    )
                  ),
                  #tags$head(tags$style('#packages .box-header{ display: none}')),  # target the box header
                  fluidRow(
                    box(
                      width = 12,
                      title = "Output options",
                      selectInput(
                        inputId = "eco_ce_output_variable",
                        label = "Select model output (cost effectiveness)",
                        choices = c(
                          'Clinical cases' = 'avert_clin',
                          'Deaths' = 'avert_death'
                        )
                      ),
                      selectInput(
                        inputId = "eco_ce_cost_type",
                        label = "Costing approach",
                        choices = c(
                          "Proportion paid by government", 
                          "Full cost (donor and government)"
                        ),
                        selected = "Full cost (donor and government)"
                      ),
                      selectInput(
                        inputId = "eco_ce_discount",
                        label = "Discounting",
                        choices = c("Discounted costs and benefits", 
                                    "Undiscounted costs and benefits"),
                        selected = "Discounted costs and benefits"
                      )
                    )
                  )
                ),
                column(8, tabsetPanel(
                  id = "eo2_results_output",
                  title = NULL,
                  tabPanel(
                    "All",
                    fluidRow(
                      column(
                        width = 12,  
                        br(),
                        p("These results show the cost-effectiveness of the vaccination programme in terms of cost per clinical case averted and cost per death averted, for each user-defined strategy compared to baseline (existing schedule with no new doses added). The total cost includes both the cost of the vaccination programme (cost of vaccines, routine delivery costs and cost of introducing new doses if relevant) and the cost of illness (treating inpatient and outpatient cases of diphtheria, tetanus and pertussis). Clinical cases and deaths are counted for diphtheria, tetanus and pertussis combined."),
                        p("Please use the dropdown menu on the left to select whether you would like to view the cost-effectiveness based on the entire cost or only the proportion of the cost paid by government (based on the inputs given by the user in Step 1 and Step 3).")
                      )
                    ),
                    fluidRow(column(12,  br(),plotlyOutput("eo2_model_plot") %>% withSpinner(color = "#237D80"))),
                    br(),
                    fluidRow(
                      column(
                        width = 12,  
                        br(),
                        p("Note: If the net total cost (plot above) or the cost per clinical case averted/cost per death averted (table below) is negative, the strategy is cost-saving. This happens when the amount saved through averted treatment costs is greater than the amount spent implementing the new vaccination strategy.")
                      )
                    ),
                    br(),
                    h4("Effectiveness outputs (diphtheria, tetanus and pertussis)*"),
                    fluidRow(column(12, reactableOutput("eo2_model_tbl"))),
                    br(),
                    p(
                      id = "ce_table_all",
                      "* Outputs are shown as the total for the full model timeframe"
                    ),
                    p("** Compared to Baseline")
                  ),
                  tabPanel(
                    "Diphtheria",
                   fluidRow(
                     column(
                       width = 12,
                       br(),
                       p("These results show the cost-effectiveness of the vaccination programme in terms of cost per clinical case averted and cost per death averted, for each user-defined strategy compared to baseline (existing schedule with no new doses added). The total cost includes both the cost of the vaccination programme (cost of vaccines, routine delivery costs and cost of introducing new doses if relevant) and the cost of illness for diphtheria only."),
                       p(strong("NOTE: This analysis shows the cost-effectiveness for diphtheria only (i.e. based on counting clinical cases and deaths from diphtheria). As diphtheria is a disease prone to outbreaks, it often has very low (often zero) cases in periods outside of outbreaks. This will distort the economic analysis. The model is not designed to predict the future occurrence of diphtheria outbreaks. Rather it is intended to model the level of vaccine-derived protection in the population. Therefore these epidemiological and economic results should be interpreted in this context."),  style = "font-size:15px !important; color:#F5793A")
                     )
                   ),
                    fluidRow(column(12, br(),plotlyOutput("eo2_dip_model_plot") %>% withSpinner(color = "#237D80"))),
                    br(),
                   fluidRow(
                     column(
                       width = 12,  
                       br(),
                       p("Note: If the net total cost (plot above) or the cost per clinical case averted/cost per death averted (table below) is negative, the strategy is cost-saving. This happens when the amount saved through averted treatment costs is greater than the amount spent implementing the new vaccination strategy.")
                     )
                   ),
                   br(),
                    h4("Effectiveness outputs (diphtheria)*"),
                    fluidRow(column(12, reactableOutput("eo2_dip_model_tbl"))),
                    br(),
                    p(
                      id = "ce_table_diphtheria",
                      "* Outputs are shown as the total for the full model timeframe"
                    ),
                    p("** Compared to Baseline")
                  ),
                  tabPanel(
                    "Pertussis",
                    fluidRow(
                      column(
                        width = 12,  
                        br(),
                        p("These results show the cost-effectiveness of the vaccination programme in terms of cost per clinical case of pertussis averted and cost per pertussis-related death averted, for each user-defined strategy compared to baseline (existing schedule with no new doses added). The total cost includes both the cost of the vaccination programme (cost of vaccines, routine delivery costs and cost of introducing new doses if relevant) and the cost of illness for pertussis only. Measured benefits (cases and deaths averted) are for pertussis only.")
                      )
                    ),
                    fluidRow(column(12, br(),plotlyOutput("eo2_pert_model_plot") %>% withSpinner(color = "#237D80"))),
                    br(),
                    fluidRow(
                      column(
                        width = 12,  
                        br(),
                        p("Note: If the net total cost (plot above) or the cost per clinical case averted/cost per death averted (table below) is negative, the strategy is cost-saving. This happens when the amount saved through averted treatment costs is greater than the amount spent implementing the new vaccination strategy.")
                      )
                    ),
                    br(),
                    h4("Effectiveness outputs (pertussis)*"),
                    fluidRow(column(12, reactableOutput("eo2_pert_model_tbl"))),
                    br(),
                    p(
                      id = "ce_table_pertussis",
                      "* Outputs are shown as the total for the full model timeframe"
                    ),
                    p("** Compared to Baseline")
                  ),
                  tabPanel(
                    "Tetanus",
                    fluidRow(
                      column(
                        width = 12,  
                        br(),
                        p("These results show the cost-effectiveness of the vaccination programme in terms of cost per clinical case of tetanus averted and cost per tetanus-related death averted, for each user-defined strategy compared to baseline (existing schedule with no new doses added). The total cost includes both the cost of the vaccination programme (cost of vaccines, routine delivery costs and cost of introducing new doses if relevant) and the cost of illness for tetanus only. Measured benefits (cases and deaths averted) are for tetanus only.")
                      )
                    ),
                    fluidRow(column(12, br(),plotlyOutput("eo2_tet_model_plot") %>% withSpinner(color = "#237D80"))),
                    br(),
                    fluidRow(
                      column(
                        width = 12,  
                        br(),
                        p("Note: If the net total cost (plot above) or the cost per clinical case averted/cost per death averted (table below) is negative, the strategy is cost-saving. This happens when the amount saved through averted treatment costs is greater than the amount spent implementing the new vaccination strategy.")
                      )
                    ),
                    br(),
                    h4("Effectiveness outputs (tetanus)*"),
                    fluidRow(column(12, reactableOutput("eo2_tet_model_tbl"))),
                    br(),
                    p(
                      id = "ce_table_tetanus",
                      "* Outputs are shown as the total for the full model timeframe"
                    ),
                    p("** Compared to Baseline")
                  )
                ) # end tabset
                ) # end col
              )
            ),
            ### 4.4 Downloads ----
            tabPanel(
              "4.4 Downloads",
              h4("Download results"),
              #br(),
              p("Results can be downloaded to view offline in two formats: 1) download a full set of results in csv format; or 2) generate a PDF report of your current session. The csv file contains the full set of epidemiological and economic model outputs (all years, by age group) for the baseline and all user-defined strategies. All available selections (e.g., output type, count vs per 100,000, costing approach, discounting) as well as details of the baseline and intervention strategy input selections are included. The PDF report includes a selection of results (plots and tables) with the relevent text to provide context."),
              p("If you would like to save additional figures using alternate selections (e.g., to view a specific age category, output type, costing approach), you can hover over any plot in the tool to download it as a png image."),
              br(),
              p("Results for all strategies will be downloaded to a .csv file."),
              fluidRow(
                column(
                  width = 4,
                  downloadButton(
                    outputId = "downloadoutput", 
                    label="Download results", 
                    class = "buttM"
                  )
                )
              ),
              br(),
              p("Download a PDF report of your current session."),
              fluidRow(
                column(
                  width = 4,
                  downloadButton(
                    outputId = 'report', 
                    label = "Download report"
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)

selectInputOriginal = selectInput
selectInput = function(inputId, label, multiple=TRUE) {
  selectInputOriginal(inputId, label, multiple)
}

# UI ----
ui <- dashboardPage(
  title = "DTP Booster Strategy Tool",
  skin = "green", # "green",
  md = TRUE,  # Material Design,
  header = header,
  sidebar = sidebar,
  body = body
)

# Server ----
server <- function(input, output, session) {
  
  LOG("mem_used: {pryr::mem_used()}")
  options(shiny.maxRequestSize = 60 * 1024^2) #increase upload max size to 60 MB
  
  session$userData$selectedCurrency = "USD"
  
  # disable other tabs on load
  shinyjs::disable(selector = '.nav-tabs-custom>.nav-tabs>li>a[data-value="2"]')
  shinyjs::disable(selector = '.nav-tabs-custom>.nav-tabs>li>a[data-value="3"]')
  shinyjs::disable(selector = '.nav-tabs-custom>.nav-tabs>li>a[data-value="4"]')
  shinyjs::disable(selector = '.sidebar-menu>li>a[data-value="model2"]')
  shinyjs::disable(selector = '.sidebar-menu>li>a[data-value="model3"]')
  shinyjs::disable(selector = '.sidebar-menu>li>a[data-value="model4"]')
  
  observeEvent(input$start, {
    updateTabsetPanel(session = session, inputId = "menu", selected = "model1")
  })
  
  observeEvent(input$restoreSpecialSession, {
    selectionsVerified(TRUE)
    restoreSession(fnameSession = 'AFENET_Custom_Session.xlsx',
                   cost_data = cost_data,
                   simul_baseline = simul_baseline,
                   simul_packages = simul_packages,
                   calibration_fit = calibration_fit,
                   session = session)
  })
  
  calibration_training_sheet_id = reactiveVal()
  # calibration_training_sheet = reactiveVal()
  
  observeEvent(input$verify_setup, ignoreInit = T, ignoreNULL = T, {
    # disable the verify selection button once user has confirmed selections
    # also triggers the disabled tabs to become enabled
    selectionsVerified(TRUE)
    
    if (shouldCacheCalibrationTibble) {
      LOG("Fetching iso3={input$selectedCountry} detail from AppData directory")
      lsDribble = googledrive::drive_get(path=paste0('DTP - internal/Calibration/AppData/',input$selectedCountry))
      LOG("Reading calibration sheet for {input$selectedCountry}")
      calibration_training_sheet_id(lsDribble$id)
      # tbCali = googlesheets4::read_sheet(lsDribble$id)
      # LOG("Updating calibration_training_sheet reactive with {nrow(tbCali)} values")
      # calibration_training_sheet(tbCali)
    }
    
    # move the user to section 1.2
    updateTabsetPanel(session,"country_profile", selected = "2")
  })
  
  # Upload parameters pop-up modal ----
  observeEvent(input$parameter_upload, {
    
    showModal(modalDialog(
      title = NULL,
      easyClose = TRUE,
      "You can upload an excel file with the parameters that were previously saved.",
      br(),
      br(),
      fluidRow(
        column(
          width = 12,
          fileInput(
            inputId = "file_parameters",
            label = "Upload template",
            buttonLabel = "Browse"
          )
        )
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("session_upload", "Proceed")
      )
    ))
  })
  
  # Save the input parameters to an excel file ----
  template <- reactive({
    
    # get the default template
    parameters <- read_excel(here("data/DTPApp-Template.xlsx"))
    
    # Update all sliders with one value
    if(!is_empty(parameters$Parameter[parameters$Type == 'slider_input'])) {
      for (input_excel in parameters$Parameter[parameters$Type == 'slider_input']){
        if(!is.null(input[[input_excel]])){
          parameters$Value1[parameters$Parameter == input_excel] = input[[input_excel]]
        }
      }}
    
    # Update all sliders with two values
    if(!is_empty(parameters$Parameter[parameters$Type == 'slider_input_double'])) {
      for (input_excel in parameters$Parameter[parameters$Type == 'slider_input_double']){
        if(!is.null(input[[input_excel]])){
          parameters$Value1[parameters$Parameter == input_excel] = input[[input_excel]][1]
          parameters$Value2[parameters$Parameter == input_excel] = input[[input_excel]][2]
        }
      }}
    
    # Update all select input values
    if(!is_empty(parameters$Parameter[parameters$Type == 'select_input'])) {
      for (input_excel in parameters$Parameter[parameters$Type == 'select_input']){
        if(!is.null(input[[input_excel]])){
          parameters$Value1[parameters$Parameter == input_excel] = input[[input_excel]]
        }
      }}
    
    # update all numeric input values
    if(!is_empty(parameters$Parameter[parameters$Type == 'numeric_input'])) {
      for (input_excel in parameters$Parameter[parameters$Type == 'numeric_input']){
        # input[[input_excel]] returns NULL when a booster is not selected
        if(!is.null(input[[input_excel]])){
          parameters$Value1[parameters$Parameter == input_excel] = input[[input_excel]]
        }
      }}
    
    # update all text area inputs
    if(!is_empty(parameters$Parameter[parameters$Type == 'text_area_input'])) {
      for (input_excel in parameters$Parameter[parameters$Type == 'text_area_input']){
        if(!is.null(input[[input_excel]])){
          parameters$Value1[parameters$Parameter == input_excel] = input[[input_excel]]
        }
      }}
    
    # update all radio buttons
    if(!is_empty(parameters$Parameter[parameters$Type == 'radio_button'])) {
      for (input_excel in parameters$Parameter[parameters$Type == 'radio_button']){
        if(!is.null(input[[input_excel]])){
          parameters$Value1[parameters$Parameter == input_excel] = input[[input_excel]]
        }
      }}
    
    return(parameters)
    
  })
  
  booster_tbl <- reactive({
    get_strategies(simulations = simul_packages$simul)
  })
  
  # Reactive values to store the data sets when the session is saved
  worksheet <- reactiveValues()
  
  selectionsVerified <- reactiveVal(FALSE)
  observeEvent(selectionsVerified(), ignoreInit = T, {
    if(!isTRUE(selectionsVerified())) return()
    # Verify Selections button should be disabled
    shinyjs::disable(id = "verify_setup")
    
    # enable all disabled sections once the session has been uploaded
    shinyjs::enable(selector = '.nav-tabs-custom>.nav-tabs>li>a[data-value="2"]')
    shinyjs::enable(selector = '.nav-tabs-custom>.nav-tabs>li>a[data-value="3"]')
    shinyjs::enable(selector = '.nav-tabs-custom>.nav-tabs>li>a[data-value="4"]')
    shinyjs::enable(selector = '.sidebar-menu>li>a[data-value="model2"]')
    shinyjs::enable(selector = '.sidebar-menu>li>a[data-value="model3"]')
    shinyjs::enable(selector = '.sidebar-menu>li>a[data-value="model4"]')
  })
  
  # Update inputs when an Excel file is uploaded ----
  observeEvent(eventExpr = input$session_upload, ignoreNULL = TRUE,  {
    LOG("A session_upload was triggered...")
    LOG("mem_used: {pryr::mem_used()}")
    # Session Upload ####
    if (!is.null(input$file_parameters)) {
      selectionsVerified(TRUE)
      restoreSession(input$file_parameters$datapath,
                     cost_data = cost_data,
                     simul_baseline=simul_baseline,
                     simul_packages=simul_packages,
                     calibration_fit = calibration_fit,
                     session=session)
    }
    LOG("Session upload complete.")
    LOG("mem_used: {pryr::mem_used()}")
  })
  
  # Step 1. Set-up Country Profile ----
  
  ## 1.1 Country ----
  
  observeEvent(input$currencyInfo, {
    showModal(modalDialog(
      title = "Currency selection",
      "This is the currency that will be used throughout the app. You will not be able to change this once you click verify selections, unless you restart the app. If you want to change the currency, you will need to restart the app and make a new currency selection.",
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$verify_setup, {
    shinyjs::disable("selectedCurrency")
    shinyjs::disable("selectedCountry")
  })
  
  # Country & Data demography data popup
  observeEvent(input$datasetDemogInfo, {
    showModal(modalDialog(
      title = "Demography data",
      HTML(
        "<b>United Nations World Population Prospects (2020)</b>

        <p>This is informtation that you need to give you the necessary context.</p>

        <b>World Bank Population data (2020)</b>

        <p>This is informtation that you need to give you the necessary context.</p>"
      )
    ))
  })
  
  ### Country & Data Health data popup ###
  observeEvent(input$datasetHealthInfo, {
    showModal(modalDialog(
      title = "Health data",
      HTML(
        "<b>Global Burden of Disease (2019)</b>

        <p>This is informtation that you need to give you the necessary context.</p>

        <b>World Health Organization (2020)</b>

        <p>This is informtation that you need to give you the necessary context.</p>"
      )
    ))
  })
  
  # Demographics info popup
  
  observeEvent(input$demographicsInfo, {
    showModal(modalDialog(
      title = "Data Source",
      HTML(
        "<b>UN World Population Prospects 2019</b>
        <p>
        <p>The 2019 Revision of World Population Prospects is the twenty-sixth round of official United Nations population estimates and projections that have been prepared by the Population Division of the Department of Economic and Social Affairs of the United Nations Secretariat.</p>
        <p>
        <p> For more information, visit: https://population.un.org/wpp/
        "
      ),
      easyClose = TRUE
    ))
  })
  
  # Demographics verify popup
  observeEvent(input$verifyDemog, {
    showModal(modalDialog(
      p("The tool will run with the default data. Please make sure you are happy with the default data set."),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("verifyDemogInfo", "Proceed")
      )
    ))
  })
  
  observeEvent(input$verifyDemogInfo, {
    showModal(modalDialog(
      p("Data has been verified"),
      footer = NULL
    ))
    
    Sys.sleep(1)
    
    shinyjs::disable("verifyDemog")
    
    removeModal()
  })
  
  # Demographics population download
  output$templateDemog <- downloadHandler(
    filename = function() {
      paste0("template.xlsx")
    },
    content = function(file) {
      
      df <- data.frame(x = seq(1:10), y = seq(1:10))
      write_xlsx(df, file)
    }
  )
  
  # Birth Death Verify popup
  observeEvent(input$verifyBirthDeathDemog, {
    showModal(modalDialog(
      p("The tool will run with the default data. Please make sure you are happy with the default data set."),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("verifyBirthDeathDemogInfo", "Proceed")
      )
    ))
  })
  
  observeEvent(input$verifyBirthDeathDemogInfo, {
    showModal(modalDialog(
      p("Data has been verified"),
      footer = NULL
    ))
    
    Sys.sleep(1)
    
    shinyjs::disable("verifyBirthDeathDemog")
    
    removeModal()
  })
  
  # Demographics birth and death download
  output$templateBirthDeathDemog <- downloadHandler(
    filename = function() {
      paste0("template.xlsx")
    },
    content = function(file) {
      
      df <- data.frame(x = seq(1:10), y = seq(1:10))
      
      write_xlsx(df, file)
    }
  )
  
  ## 1.2 Current Vaccine Schedule ----
  
  coverage_type <- "default"
  
  # move on to next page
  observeEvent(input$next_cvs, {
    updateTabsetPanel(session = session, inputId = "country_profile", selected = "3")
  })
  
  # update primary series health facility value
  output$del_o_ps <- renderText({
    glue("Delivery: Outreach site (including schools) \n{100-input$del_hf_ps}%") 
  })
  output$del_o_b <- renderText({
    glue("Delivery: Outreach site (including schools) \n{100-input$del_v_hf_b}%") 
  })
  output$del_o_cb <- renderText({
    glue("Delivery: Outreach site (including schools) \n{100-input$del_v_hf_cb}%") 
  })
  output$del_o_ab <- renderText({
    glue("Delivery: Outreach site (including schools) \n{100-input$del_v_hf_ab}%") 
  })
  output$del_o_m <- renderText({
    glue("Delivery: Outreach site (including schools) \n{100-input$del_v_hf_m}%") 
  })
  
  observeEvent(input$covDefaults, {
    showModal(modalDialog(
      title = "Data Sources",
      easyClose = TRUE,
      HTML(
        "
        <b>Coverage data</b>
        <p>
        <p>Primary series: Coverage data for dose 1 and 3 are drawn from the WHO Global Health Observatory. Coverage data for dose 2 is interpolated as the average between values for dose 1 and 3.
        <p>Boosters: Coverage data for booster doses, if applicable, is drawn from the WHO Global Health Observatory.
        <p>Maternal vaccination: Coverage data for the first dose of maternal vaccination is drawn from reported ‘Antenatal care coverage - at least 1 visit’ based on WHO Global Health Observatory data. For dose 2 and above, the WHO GHO TT2 data is used.
        <p>
        <b>WHO Global Health Observatory</b>
        <p>
        <p>The GHO data repository is WHO's gateway to health-related statistics for its 194 Member States. It provides access to over 1000 indicators on priority health topics including mortality and burden of diseases, the Millennium Development Goals (child nutrition, child health, maternal and reproductive health, immunization, HIV/AIDS, tuberculosis, malaria, neglected diseases, water and sanitation), non communicable diseases and risk factors, epidemic-prone diseases, health systems, environmental health, violence and injuries, equity among others.

        <p>Many of these datasets represent the best estimates of WHO using methodologies for specific indicators that aim for comparability across countries and time; they are updated as more recent or revised data become available, or when there are changes to the methodology being used. Therefore, they are not always the same as official national estimates, although WHO whenever possible will provide Member States the opportunity review and comment on data and estimates as part of country consultations.</p>
        <p>
        <p> For more information, visit: https://www.who.int/data/gho
        <p>
        
        "
      )
    ))
  })
  
  ### Current Vaccine Schedule Verify ###
  observeEvent(input$verifyvaxcov, {
    showModal(modalDialog(
      p("The tool will run with the default data. Please make sure you are happy with the default data set."),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("verifyvaxcovInfo", "Proceed")
      )
    ))
  })
  
  observeEvent(input$verifyvaxcovInfo, {
    showModal(modalDialog(
      p("Data has been verified"),
      footer = NULL
    ))
    
    Sys.sleep(1)
    
    shinyjs::disable("verifyvaxcov")
    
    removeModal()
  })
  
  # Current Vaccine Schedule download
  output$templatevaxcov <- downloadHandler(
    filename = function() {
      paste0("DTP Coverage template.xlsx")
    },
    content = function(file) {
      df <- getVaccineSchedule(input$selectedCountry,
                               fillBlanks = !input$templatevaxcov_israw)
      write_xlsx(df, file)
    }
  )
  
  observeEvent(input$rawCoverageDataInfo, {
    showModal(modalDialog(
      title = "Coverage Data Preprocessing",
      HTML(
        "<b>Coverage Data</b>
        <p>
        <p>The Coverage Data for first and third primary series doses as well as ANC 1+ visits comes from <a href='https://data.unicef.org/resources/resource-type/datasets/' target='_blank'>UNICEF</a></p>
        <p>The Coverage Data for TTCV2+ comes from the <a href='https://www.who.int/data/gho' target='_blank'>WHO Global Health Observatory (GHO)</a></p>
        <p>Raw data can be downloaded by ticking the appropriate checkbox here, however for the model to run the table needs to be complete. As such, the table has been constructed by first filling any empty values with their most recent known value. Where no recent value is known the first known value proceeding the empty value is filled in. Any remaining empty cells are filled with zeros. Lastly, DTP dose 2 is filled with the average coverage of DTP dose 1 and DTP dose 3.</p>
        "
      ),
      easyClose = TRUE
    ))
  })
  
  # custom vaccine coverage data
  custom_coverage <- reactiveVal()
  
  # replace coverage table with uploaded template
  observeEvent(input$vaxInput, {
    if (!is.null(input$vaxInput)) {
      withProgress(message = "Uploading data", {
        uploaded_coverage_table <- read_excel(input$vaxInput$datapath)
        
        # update the names to match the ones used in the original table
        names(uploaded_coverage_table) <- c("Year", "DTPCV1", "DTPCV2", "DTPCV3",
                                            "InfantBooster", "ChildBooster",
                                            "AdolescentBooster", "ANC", "TTCV2")
        
        if (is.logical(validateCustomCoverage(uploaded_coverage_table))) { # this will only be a logical when it returns true
          custom_coverage(uploaded_coverage_table)
          shinyjs::runjs("window.scrollTo(0, document.body.scrollHeight)")
        } else {
          error_cols <- validateCustomCoverage(uploaded_coverage_table)
          sendSweetAlert(
            title = "Validation error",
            text = glue::glue("You have invalid data in the following columns: {glue::glue_collapse(error_cols, sep = ', ')}"),
            type = "error"
          )
        }
      })
    }
    
    updateReactable(
      outputId = "vaccineCoverageTable",
      data = uploaded_coverage_table,
      session = session
    )
    
    toggleDropdownButton(inputId = "cov_temp")
  })
  
  output$vaccineCoverageTable <- renderReactable({
    req(coverage_data())
    reactable(
      pagination = FALSE, height = 250,
      coverage_data(),
      columns = list(
        Year = colDef(align = "left"),
        DTPCV1 = colDef(
          name = "Dose 1",
          format = colFormat(percent = TRUE, digits = 1),
          align = "center"
        ),
        DTPCV2 = colDef(
          name = "Dose 2",
          format = colFormat(percent = TRUE, digits = 1),
          align = "center"
        ),
        DTPCV3 = colDef(
          name = "Dose 3",
          format = colFormat(percent = TRUE, digits = 1),
          align = "center"
        ),
        InfantBooster = colDef(
          name = "Early childhood booster",
          format = colFormat(percent = TRUE, digits = 1),
          width = 160,
          align = "center"
        ),
        ChildBooster = colDef(
          name = "Child booster",
          format = colFormat(percent = TRUE, digits = 1),
          width = 160,
          align = "center"
        ),
        AdolescentBooster = colDef(
          name = "Adolescent booster",
          format = colFormat(percent = TRUE, digits = 1),
          width = 160,
          align = "center"
        ),
        ANC = colDef(
          name = "ANC*",
          format = colFormat(percent = TRUE, digits = 1),
          width = 175,
          align = "center"
        ),
        TTCV2 = colDef(
          name = "TTCV2",
          format = colFormat(percent = TRUE, digits = 1),
          width = 175,
          align = "center"
        )
      )
    )
    
  })
  
  ## 1.3 Health system ---- 
  
  # Remove slider for “Probability of a mild case being treated (outpatient)” for Tetanus
  shinyjs::hide("p_mt_T")
  
  # move on to next page
  observeEvent(input$next_hs, {
    updateTabsetPanel(
      session = session, 
      inputId = "country_profile", 
      selected = "4"
    )
  })
  
  
  # Step 2. Calibrate Model ----
  
  caliTimeRange <- reactiveVal(c(2010, 2022))
  
  calibration_fit <- reactiveVal()
  
  observeEvent(input$calData, {
    showModal(modalDialog(
      title = "Data Sources",
      easyClose = TRUE,
      HTML(
        "<b>WHO Global Health Observatory</b>
        <p>
        <p>The GHO data repository is WHO's gateway to health-related statistics for its 194 Member States. It provides access to over 1000 indicators on priority health topics including mortality and burden of diseases, the Millennium Development Goals (child nutrition, child health, maternal and reproductive health, immunization, HIV/AIDS, tuberculosis, malaria, neglected diseases, water and sanitation), non communicable diseases and risk factors, epidemic-prone diseases, health systems, environmental health, violence and injuries, equity among others.

        <p>Many of these datasets represent the best estimates of WHO using methodologies for specific indicators that aim for comparability across countries and time; they are updated as more recent or revised data become available, or when there are changes to the methodology being used. Therefore, they are not always the same as official national estimates, although WHO whenever possible will provide Member States the opportunity review and comment on data and estimates as part of country consultations.</p>
        <p>
        <p> For more information, visit: https://www.who.int/data/gho
        <p>
        <b>IHME Global Burden of Diseases, Injuries, and Risk Factors Study (GBD)</b>
        <p>
        <p>The Global Burden of Diseases, Injuries, and Risk Factors Study (GBD) is led by the Institute for Health Metrics and Evaluation (IHME) at the University of Washington. Examining trends from 1990 to the present, the latest GBD study includes data on mortality and morbidity in 204 countries and territories, 369 diseases and injuries, and 87 risk factors. The incidence and mortality figures are modelled estimates, not reported data.</p>
        <p>
        <p> For more information, visit: https://www.healthdata.org/gbd/2019 and https://ghdx.healthdata.org/gbd-2019
        "
      )
    ))
  })
  
  # scroll down to the upload your data section when the user selects own data
  observeEvent(input$data_source_calibration, {
    
    if (input$data_source_calibration == "OWN DATA") {
      shinyjs::runjs("window.scrollTo(0, document.body.scrollHeight)") 
    }
  })
  
  observeEvent(input$calibrate_manual, {
    saveDebugRDS(reactiveValuesToList(input), 'manual_calibration_input.rds')
    saveDebugRDS(coverage_data(), 'manual_calibration_coverage_data.rds')
    setLockedState(isLocked=TRUE)
    showNotification(
      tagList(
        span(h4(img(src="CustomLoader2.gif"), "Simulation Running...", style = "color:#FFFFFF !important;")),
        tags$div(id="myProgress", tags$div(id="myBar")),
        span(p("Typically runs in 5 to 8 minutes", style = "color:#FFFFFF !important;"))
      ),
      duration = NULL,
      type = "message",
      id = "manual_calibration_model",
      session = session,
      closeButton = FALSE
    )
    # update transmission probabilities with user inputs
    
    # run model
    parameters = c(getModelParameters(),
                   getShinyInputs(input=input,
                                  coverage_table=coverage_data(),
                                  baseline = T,
                                  startyear = startyear,
                                  tyears = tyears))
    saveDebugRDS(parameters, 'shinyInputs_baseline.rds')
    
    # Create variables for adding results of makeSummaryTibble() to google sheets 
    input_list <- reactiveValuesToList(input)
    saveDebugRDS(input_list, glue::glue('shinyInputList_baseline.rds'))
    cov_table <- coverage_data()
    if (shouldCacheCalibrationTibble) {
      LOG("Saving simplified outputs to Google Sheet...")
      tbSummaryForSheets <- makeSummaryTibble(input_list, cov_table)
    }
    
    time_start <- Sys.time()
    run_model(parameters=parameters, timesteps=timesteps, coverage_table=coverage_data(), iso3=input$selectedCountry,
              cacheAs='mo_calibrated.rds', inParallel=TRUE,
              DEBUG = DEBUG) %...>%
      with({
        tbResult <- mutate(., time=year)
        saveDebugRDS(tbResult, 'eo_mo_baseline.rds')
        #bind_rows(p.D$mo, p.T$mo, p.P$mo)
        simul_baseline(tbResult)

        if (shouldCacheCalibrationTibble) {
          LOG("Including model results in summary tibble for Google Sheet...")
          pat = 'clin_Inc_'
          tbSummaryForSheetsWithModel = tbResult %>%
            filter(str_detect(variable,pattern=pat),
                   year %between% c(2010, 2020)) %>%
            group_by(disease, year) %>%
            # sum over the age groups
            summarise(value=sum(value), .groups = 'drop_last') %>%
            pivot_wider(values_from = value, names_prefix = 'modelValue', names_from = year) %>%
            inner_join(tbSummaryForSheets,
                       .,
                       by = join_by(disease))
          
          addedToCache = addToSheetsIfUnique(tbSummaryForSheetsWithModel, ss=calibration_training_sheet_id())
          if(addedToCache) {LOG("...Google Sheet Updated")} else {LOG("...Google Sheet NOT Updated")}
        }
        
        # Unlock the app and proceed
        setLockedState(isLocked=FALSE)
        removeNotification(id = "manual_calibration_model", session = session)
        dtSecs <- ceiling(as.numeric(Sys.time() - time_start, units = "secs"))
        showNotification(span(h4(icon("check"), "Done"), paste0("Simulation ran in ", dtSecs, " seconds.")),
                         duration = 4, type = "default")
      })
    LOG("Leaving observeEvent(input$calibrate_manual, ...)", level = LEVEL$TRACE)
  })
  
  ## 1.4. DTP Clinical Burden
  
  ### DTP Clinical Burden:Pertussis ###
  
  # Clinical  Burden info popup
  
  observeEvent(input$clinicalInfo, {
    showModal(modalDialog(
      title = "Data Sources",
      easyClose = TRUE,
      HTML(
        "<b>WHO Global Health Observatory</b>
        <p>
        <p>The GHO data repository is WHO's gateway to health-related statistics for its 194 Member States. It provides access to over 1000 indicators on priority health topics including mortality and burden of diseases, the Millennium Development Goals (child nutrition, child health, maternal and reproductive health, immunization, HIV/AIDS, tuberculosis, malaria, neglected diseases, water and sanitation), non communicable diseases and risk factors, epidemic-prone diseases, health systems, environmental health, violence and injuries, equity among others.

        <p>Many of these datasets represent the best estimates of WHO using methodologies for specific indicators that aim for comparability across countries and time; they are updated as more recent or revised data become available, or when there are changes to the methodology being used. Therefore, they are not always the same as official national estimates, although WHO whenever possible will provide Member States the opportunity review and comment on data and estimates as part of country consultations.</p>
        <p>
        <p> For more information, visit: https://www.who.int/data/gho
        <p>
        <b>IHME Global Burden of Diseases, Injuries, and Risk Factors Study (GBD)</b>
        <p>
        <p>The Global Burden of Diseases, Injuries, and Risk Factors Study (GBD) is led by the Institute for Health Metrics and Evaluation (IHME) at the University of Washington. Examining trends from 1990 to the present, the latest GBD study includes data on mortality and morbidity in 204 countries and territories, 369 diseases and injuries, and 87 risk factors. The incidence and mortality figures are modelled estimates, not reported data.</p>
        <p>
        <p> For more information, visit: https://www.healthdata.org/gbd/2019 and https://ghdx.healthdata.org/gbd-2019
        "
      )
    ))
  })
  
  output$manual_calibration <- renderUI({
    # box title
    dataset = input$data_source_calibration
    if (dataset == "WHO") {
      box_title <- "DTP Reported Incidence"
    } else if (dataset == "GBD") {
      box_title <- "DTP Estimated Incidence"
    } else {
      dataset = "OWN"
      box_title <- "DTP Incidence"
    }
    
    mean_burden_data <- function() {
      if (dataset == "OWN DATA") {
        data <- req(custom_burden_data())
      } else {
        data <- get_clinical_burden(dataset, input$selectedCountry)
      }
      result = data %>%
        filter(Year %between% c(2010,2020)) %>%
        # Replace NA with 0 and get the mean burden values over the period:
        summarise(across(.fns = \(x)mean(if_else(is.na(x),0,x))), .groups = 'drop_last') %>%
        select(!Year)
      if (dataset != "GBD") {
        # Reported clinical incidence should be scaled to it's implied true clinicial incidence
        reportingRates = c(input$p_rep_clin_D, input$p_rep_clin_T, input$p_rep_clin_P) / 100
        result = as.numeric(result) / reportingRates
      } else {
        result = as.numeric(result)
      }
      names(result)=c('D','T','P')
      result
    }

    # Notes:
    # We need a surrogate model for each disease
    # Each model should take in average
    # I did this manually for now (linear model defined using known inputs)
    # We'll want valueX from mean_burden_data()
    if(shouldPredictPtrans) {
      # Update ptrans values according to surrogate model
      input_list = isolate(reactiveValuesToList(input))
      tbCaliSummary = makeSummaryTibble(input_list, coverage_data())
      cases = mean_burden_data()
      with(as.list(tbCaliSummary %>% mutate(disease=factor(disease, levels=c("Diphtheria", "Tetanus", "Pertussis"))) %>% arrange(disease)),{
        valueDiphtheria = cases[['D']]
        valueTetanus = cases[['T']]
        valuePertussis = cases[['P']]
        # cov_scalar_D = (vps_weighted+vb_weighted+vm_weighted)/(2.73+0.9733)
        cov_scalar_T = (vps_weighted+vb_weighted+vm_weighted)[2]/(1.5965+0.457451)
        cov_scalar_P = (vps_weighted+vb_weighted+vm_weighted)[3]/(2.346754545)
        # ptrans_D = 24.3355979482316 + 1.22137855382332e+106*(valueDiphtheria) + -4.30710679379424*(cov_scalar_D)
        ptrans_T = -5.10274264083287 + 0.0217690482311246*(valueTetanus) + 5.11575631267282*(cov_scalar_T)
        ptrans_P = -70.1862768755404 + 6.75565817249389e-05*(valuePertussis) + 71.3773947663338*(cov_scalar_P)
        # updateNumericInput(inputId='ptrans_D', value = ptrans_D)
        LOG("ptrans_T={ptrans_T} for meanCases={valueTetanus} and cov_scalar_T={cov_scalar_T}")
        LOG("ptrans_P={ptrans_P} for meanCases={valuePertussis} and cov_scalar_P={cov_scalar_P}")
        value_T = unname(as.numeric(round(ptrans_T, digits = 1)))
        value_P = unname(as.numeric(round(ptrans_P, digits = 1))) + 5  # I needed to add 5 to improve the fit for now.
        LOG("updateNumericInput(session, inputId='ptrans_T', value = {value_T}, min=1, max={ceiling(2*value_T)}, step=0.1)")
        LOG("updateNumericInput(session, inputId='ptrans_P', value = {value_P})")
        
        updateSliderInput(session, inputId='ptrans_T',
                          value = value_T,
                          min=1, max=ceiling(2*value_T), step=0.1)
        updateSliderInput(session, inputId='ptrans_P', value = value_P)
      })
    }
    
    fluidRow(
      box(
        width = 12, title = box_title, status = "success",
        
        if (dataset == "WHO") {
          fluidRow(
            # column(5, reactableOutput("clinical_burden_table")),
            column(12, plotlyOutput("who_calibration"))
          )
        } else if (dataset == "GBD") {
          fluidRow(
            # column(5, reactableOutput("gbd_clinical_burden_table")),
            column(12, plotlyOutput("gbd_calibration"))
          )
        } else {
          fluidRow(
            column(12,plotlyOutput("own_data_calibration"))
          )
        }
      )
    )
  })
  
  observeEvent(input$go_to_calibration, {
    updateTabsetPanel(session, "calibration_tab", selected = "2")
  })
  
  observeEvent(input$accept_manual_calibration, {
    updateTabsetPanel(session, "calibration_tab", selected = "3")
  })
  
  observe({
    updateRadioGroupButtons(
      session = getDefaultReactiveDomain(),
      inputId = "data_source_calibration",
      selected = input$data_source_calibration
    )
  })
  
  # download template
  output$template_burden <- downloadHandler(
    filename = function() {
      paste0("DTP Incidence Data template.xlsx")
    },
    content = function(file) {
      file.copy(here('data/DTP Incidence Data template.xlsx'), file)
    }
  )
  
  # custom clinical burden table with uploaded template
  custom_burden_data <- reactiveVal()
  
  observeEvent(input$burden_input, {
    if (!is.null(input$burden_input)) {
      withProgress(message = "Uploading data", {
        uploaded_burden_data <- read_excel(
          input$burden_input$datapath, 
          sheet = "Data",
          col_types = "numeric"
        )

        # validation 
        rules <- validator(
          Diphtheria >= 0 | is.na(Diphtheria), # both (NA | TRUE) and (TRUE | NA) give TRUE
          Tetanus >= 0 | is.na(Tetanus),
          Pertussis >= 0 | is.na(Pertussis)  
        )
        
        out <- confront(uploaded_burden_data, rules)

        if (sum(summary(out)$fails) > 0) {
          show_alert(
            title = "Validation error",
            text = tags$div(
              p("You have entered invalid data:"),
              reactableOutput("incidence_validation")
            ),
            type = "error",
            html = TRUE
          )
          
          output$incidence_validation <- renderReactable({
            violating(uploaded_burden_data, out) %>% 
              reactable(
                bordered = TRUE
              )
          })
        } else {
          custom_burden_data(uploaded_burden_data)
          updateTabsetPanel(session, "calibration_tab", selected = "2")
        }
      })
    }
  })
  
  # table to show custom burden data
  output$custom_burden <- renderReactable({
    req(custom_burden_data())
    reactable(
      custom_burden_data(),
      pagination = FALSE, height = 350,
      defaultColDef = colDef(
        format = colFormat(separators = TRUE, digits = 0),
        headerStyle = list(background = "#f7f7f8")
      ),
      columns = list(
        Year = colDef(
          align = "left",
          format = colFormat(separators = FALSE),
          style = list(position = "sticky", left = 0, background = "#f7f7f8", zIndex = 1 ,borderRight = "1px solid #eee"),
          headerStyle = list(position = "sticky", left = 0, background = "#f7f7f8", zIndex = 1, borderRight = "1px solid #eee")
        )
      )
    )
  })
  
  output$clinical_burden_table <- renderReactable({
    who_data <- get_clinical_burden("WHO", input$selectedCountry)
    
    reactable(
      who_data,
      pagination = FALSE,height = 350,
      defaultColDef = colDef(
        format = colFormat(separators = TRUE),
        headerStyle = list(background = "#f7f7f8")
      ),
      columns = list(
        Year = colDef(
          align = "left",
          format = colFormat(separators = FALSE),
          style = list(position = "sticky", left = 0, background = "#f7f7f8", zIndex = 1 ,borderRight = "1px solid #eee"),
          headerStyle = list(position = "sticky", left = 0, background = "#f7f7f8", zIndex = 1, borderRight = "1px solid #eee")
        )
      )
    )
  })
  
  
  output$who_clinical_burden_plot <- renderPlotly({
    mo <- simul_baseline()
    if(is.null(mo)) {mo=tibble(year=integer(0),
                               value=numeric(0),
                               unit=character(0),
                               disease=character(0),
                               age_group=character(0),
                               variable=character(0))}
    
    moNeeded <- mo %>%
      filter(age_group=="All", variable %>% str_starts('rep_Clin_')) %>% 
      select(Year=year, modelValue=value, Disease=disease)
    
    plot_data <- getClinicalBurden(input$selectedCountry) %>%
      mutate(type="Data") %>% 
      select(Year, value, Disease) %>% 
      left_join(moNeeded, by = c('Year', 'Disease')) %>%
      filter(Year>=startyear) %>% 
      mutate(Disease = as_factor(Disease))
    
    p_dip <- plot_data %>% 
      filter(Disease == "Diphtheria") %>%
      plot_ly(x = ~Year, y = ~value) %>% 
      add_markers(
        marker = list(color = '#000000'),
        hovertemplate = paste(
          "<b>Value: %{y:,.0f}</b><br><b>Year: %{x}",  
          "<extra></extra>"
        )
      )  %>%
      layout(
        annotations = list(x = 0.5 , y = 1.02, text = "Diphtheria", showarrow = FALSE, 
                           xref='paper', yref='paper', font = list(size = 15)),
        yaxis = list(rangemode = "tozero"),
        showlegend = FALSE
      )
    
    p_tet <- plot_data %>% 
      filter(Disease == "Tetanus") %>%
      plot_ly(x = ~Year, y = ~value) %>% 
      add_markers(
        marker = list(color = '#000000'),
        hovertemplate = paste(
          "<b>Value: %{y:,.0f}</b><br><b>Year: %{x}",  
          "<extra></extra>"
        )
      )  %>%
      layout(
        annotations = list(x = 0.5 , y = 1.02, text = "Tetanus", showarrow = FALSE, 
                           xref='paper', yref='paper', font = list(size = 15)),
        yaxis = list(rangemode = "tozero"),
        showlegend = FALSE
      )
    
    
    p_pert <- plot_data %>% 
      filter(Disease == "Pertussis") %>%
      plot_ly(x = ~Year, y = ~value) %>% 
      add_markers(
        marker = list(color = '#000000'),
        hovertemplate = paste(
          "<b>Value: %{y:,.0f}</b><br><b>Year: %{x}",  
          "<extra></extra>"
        )
      ) %>%
      layout(
        annotations = list(x = 0.5 , y = 1.02, text = "Pertussis", showarrow = FALSE, 
                           xref='paper', yref='paper', font = list(size = 15)),
        yaxis = list(rangemode = "tozero"),
        xaxis = list(tickvals = list(2000, 2005, 2010, 2015, 2020)), # xaxis only has one value
        showlegend = FALSE
      )
    
    subplot(p_dip, p_tet, p_pert, nrows = 1) %>% 
      plotly::config(
        toImageButtonOptions = list(
          filename = "WHO-observed-incidence-data-plot"
        )
      )
    
  })
  
  output$who_calibration <- renderPlotly({
    
    mo <- simul_baseline()
    
    if(is.null(mo)) {mo=tibble(year=integer(0),
                               value=numeric(0),
                               unit=character(0),
                               disease=character(0),
                               age_group=character(0),
                               variable=character(0))}
    
    moNeeded <- mo %>%
      filter(age_group=="All", variable %>% str_starts('rep_Clin_')) %>% 
      select(Year=year, modelValue=value, Disease=disease)
    
    plot_data <- getClinicalBurden(input$selectedCountry) %>%
      mutate(type="Data") %>% 
      select(Year, value, Disease) %>% 
      left_join(moNeeded, by = c('Year', 'Disease')) %>%
      filter(Year%between%caliTimeRange()) %>% 
      mutate(Disease = as_factor(Disease)) %>% 
      mutate(across(where(is.numeric), round))

    p <- who_calibration_fit(plot_data = plot_data)
    
    calibration_fit(p)
    
    p_dip <- plot_data %>%
      filter(Disease == "Diphtheria") %>% 
      plot_ly(x = ~Year, y = ~value, color = ~Disease) %>% 
      add_markers(
        marker = list(color = '#000000'),
        showlegend = FALSE,
        hovertemplate = paste(
          "<b>Value: %{y:,.0f}</b><br><b>Year: %{x}",  
          "<extra></extra>"
        )
      ) %>% 
      add_lines(
        y = ~modelValue,
        line = list(color = "#4477AA"),
        showlegend = TRUE,
        name = "Diphtheria model value",
        hovertemplate = paste(
          "<b>Model Value: %{y:,.0f}</b><br><b>Year: %{x}",  
          "<extra></extra>"
        )
      ) %>% 
      layout(
        annotations = list(x = 0.5 , y = 1.02, text = "Diphtheria", showarrow = FALSE, 
                           xref='paper', yref='paper', font = list(size = 15)),
        yaxis = list(rangemode = "tozero"),
        showlegend = TRUE,
        hovermode = "x"
      )
    
    p_tet <- plot_data %>%
      filter(Disease == "Tetanus") %>% 
      plot_ly(x = ~Year, y = ~value, color = ~Disease) %>% 
      add_markers(
        marker = list(color = '#000000'),
        showlegend = FALSE,
        hovertemplate = paste(
          "<b>Value: %{y:,.0f}</b><br><b>Year: %{x}",  
          "<extra></extra>"
        )
      ) %>% 
      add_lines(
        y = ~modelValue,
        line = list(color = "#44AA99"),
        showlegend = TRUE,
        name = "Tetanus model value",
        hovertemplate = paste(
          "<b>Model Value: %{y:,.0f}</b><br><b>Year: %{x}",  
          "<extra></extra>"
        )
      ) %>% 
      layout(
        annotations = list(x = 0.5 , y = 1.02, text = "Tetanus", showarrow = FALSE, 
                           xref='paper', yref='paper', font = list(size = 15)),
        yaxis = list(rangemode = "tozero"),
        hovermode = "x",
        showlegend = TRUE
      )
    
    p_pert <- plot_data %>%
      filter(Disease == "Pertussis") %>% 
      plot_ly(x = ~Year, y = ~value, color = ~Disease) %>% 
      add_markers(
        marker = list(color = '#000000'),
        showlegend = FALSE,
        hovertemplate = paste(
          "<b>Value: %{y:,.0f}</b><br><b>Year: %{x}",  
          "<extra></extra>"
        )
      ) %>% 
      add_lines(
        y = ~modelValue,
        line = list(color = "#CC6677"),
        showlegend = TRUE,
        name = "Pertussis model value",
        hovertemplate = paste(
          "<b>Model Value: %{y:,.0f}</b><br><b>Year: %{x}",  
          "<extra></extra>"
        )
      ) %>% 
      layout(
        annotations = list(x = 0.5 , y = 1.02, text = "Pertussis", showarrow = FALSE, 
                           xref='paper', yref='paper', font = list(size = 15)),
        yaxis = list(rangemode = "tozero"),
        showlegend = TRUE,
        hovermode = "x"
      )
    
    # for now we will only show the fit for Tetanus
    #subplot(p_dip, p_tet, p_pert, nrows = 1)
    p_tet %>% 
      layout(
        xaxis = list(title = ""),
        yaxis = list(title = ""),
        legend = list(orientation = 'h')
      ) %>% 
      plotly::config(
        toImageButtonOptions = list(filename = "WHO-calibration-plot")
      )
  })
  
  output$gbd_clinical_burden_table <- renderReactable({
    
    gbd_data <- get_clinical_burden("GBD", input$selectedCountry)
    
    gbd_data <- gbd_data %>% 
      arrange(desc(Year))
    
    reactable(
      gbd_data,
      pagination = FALSE, height = 350,
      defaultColDef = colDef(
        format = colFormat(separators = TRUE, digits = 0),
        headerStyle = list(background = "#f7f7f8")
      ),
      columns = list(
        Year = colDef(
          align = "left",
          format = colFormat(separators = FALSE),
          style = list(position = "sticky", left = 0, background = "#f7f7f8", zIndex = 1 ,borderRight = "1px solid #eee"),
          headerStyle = list(position = "sticky", left = 0, background = "#f7f7f8", zIndex = 1, borderRight = "1px solid #eee")
        )
      )
    )
  })
  
  output$gbd_clinical_burden_plot <- renderPlotly({
    
    mo <- simul_baseline()
    if(is.null(mo)) {mo=tibble(year=integer(0),
                               value=numeric(0),
                               unit=character(0),
                               disease=character(0),
                               age_group=character(0),
                               variable=character(0))}
    moNeeded <- mo %>%
      filter(age_group=="All", variable %>% str_starts('clin_Inc_')) %>% 
      select(Year=year, modelValue=value, Disease=disease)
    
    plot_data <- tbGBDIncidence %>%
      filter(year >= startyear, iso3 == input$selectedCountry) %>%
      rename(Year=year, value=val, Disease=disease) %>% 
      left_join(moNeeded, by = c('Year', 'Disease')) %>% 
      mutate(across(where(is.numeric), round))

    p_dip <- plot_data %>% 
      filter(Disease == "Diphtheria") %>%
      plot_ly(x = ~Year, y = ~value, color = ~Disease) %>% 
      add_markers(error_y = list(
        color='#7E7E7E',
        type = "data",
        symmetric = FALSE,
        array = ~upper-value,
        arrayminus = ~value-lower),
        marker = list(color='#000000'),
        hovertemplate = paste("<b>Value: %{y:,.0f}</b><br><b>Year: %{x}")
      ) %>% 
      layout(
        annotations = list(x = 0.5 , y = 1.02, text = "Diphtheria", showarrow = FALSE, 
                           xref='paper', yref='paper', font = list(size = 15)),
        yaxis = list(rangemode = "tozero"),
        showlegend = FALSE
      )
    
    p_tet <- plot_data %>% 
      filter(Disease == "Tetanus") %>%
      plot_ly(x = ~Year, y = ~value, color = ~Disease) %>% 
      add_markers( error_y = list(
        color='#7E7E7E',
        type = "data",
        symmetric = FALSE,
        array = ~upper-value,
        arrayminus = ~value-lower),
        marker = list(color='#000000'),
        hovertemplate = paste("<b>Value: %{y:,.0f}</b><br><b>Year: %{x}")
      ) %>% 
      layout(
        annotations = list(x = 0.5 , y = 1.02, text = "Tetanus", showarrow = FALSE, 
                           xref='paper', yref='paper', font = list(size = 15)),
        yaxis = list(rangemode = "tozero"),
        showlegend = FALSE
      )
    
    p_pert <- plot_data %>% 
      filter(Disease == "Pertussis") %>%
      plot_ly(x = ~Year, y = ~value, color = ~Disease) %>% 
      add_markers( error_y = list(
        color='#7E7E7E',
        type = "data",
        symmetric = FALSE,
        array = ~upper-value,
        arrayminus = ~value-lower),
        marker = list(color='#000000'),
        hovertemplate = paste("<b>Value: %{y:,.0f}</b><br><b>Year: %{x}")
      ) %>% 
      layout(
        annotations = list(x = 0.5 , y = 1.02, text = "Pertussis", showarrow = FALSE, 
                           xref='paper', yref='paper', font = list(size = 15)),
        yaxis = list(rangemode = "tozero"),
        showlegend = FALSE
      )
    
    subplot(p_dip, p_tet, p_pert, nrows = 1) %>% 
      plotly::config(
        toImageButtonOptions = list(
          filename = "GBD-incidence-estimates-plot"
        )
      )
    
  })
  
  output$gbd_calibration <- renderPlotly({
    mo <- simul_baseline()
    if(is.null(mo)) {mo=tibble(year=integer(0),
                               value=numeric(0),
                               unit=character(0),
                               disease=character(0),
                               age_group=character(0),
                               variable=character(0))}
    
    moNeeded <- mo %>%
      filter(age_group=="All", variable %>% str_starts('clin_Inc_')) %>% 
      select(Year=year, modelValue=value, Disease=disease)
    
    plot_data <- tbGBDIncidence %>%
      filter(year%between%caliTimeRange(), iso3 == input$selectedCountry) %>%
      rename(Year=year, value=val, Disease=disease) %>% 
      left_join(moNeeded, by = c('Year', 'Disease')) %>% 
      mutate(across(where(is.numeric), round)) %>% 
      mutate(
        Disease = factor(Disease, levels = c("Diphtheria", "Tetanus", "Pertussis" ))
      )
    
    p <- gbd_calibration_fit(plot_data)
    
    calibration_fit(p)
    
    p_dip <- plot_data %>% 
      filter(Disease == "Diphtheria") %>%
      plot_ly(x = ~Year, y = ~value, color = ~Disease) %>% 
      add_markers(error_y = list(
        color='#7E7E7E',
        type = "data",
        symmetric = FALSE,
        array = ~upper-value,
        arrayminus = ~value-lower),
        marker = list(color='#000000'),
        showlegend = FALSE,
        hovertemplate = paste(
          "<b>Value: %{y:,.0f}</b><br><b>Year: %{x}",
          "<extra></extra>"
        )
      ) %>% 
      add_lines(
        y = ~modelValue,
        line = list(color = "#4477AA"),
        showlegend = TRUE,
        name = "Diphtheria model value",
        hovertemplate = paste(
          "<b>Model Value: %{y:,.0f}</b><br><b>Year: %{x}",  
          "<extra></extra>"
        )
      ) %>% 
      layout(
        annotations = list(x = 0.5 , y = 1.02, text = "Diphtheria", showarrow = FALSE, 
                           xref='paper', yref='paper', font = list(size = 15)),
        yaxis = list(rangemode = "tozero"),
        hovermode = "x"
      )
    
    p_tet <- plot_data %>% 
      filter(Disease == "Tetanus") %>%
      plot_ly(x = ~Year, y = ~value, color = ~Disease) %>% 
      add_markers( error_y = list(
        color='#7E7E7E',
        type = "data",
        symmetric = FALSE,
        array = ~upper-value,
        arrayminus = ~value-lower),
        marker = list(color='#000000'),
        showlegend = FALSE,
        hovertemplate = paste(
          "<b>Value: %{y:,.0f}</b><br><b>Year: %{x}",
          "<extra></extra>"
        )
      ) %>%
      add_lines(
        y = ~modelValue,
        line = list(color = "#44AA99"),
        showlegend = TRUE,
        name = "Tetanus model value",
        hovertemplate = paste(
          "<b>Model Value: %{y:,.0f}</b><br><b>Year: %{x}",  
          "<extra></extra>"
        )
      ) %>% 
      layout(
        annotations = list(x = 0.5 , y = 1.02, text = "Tetanus", showarrow = FALSE, 
                           xref='paper', yref='paper', font = list(size = 15)),
        yaxis = list(rangemode = "tozero"),
        hovermode = "x"
      )
    
    p_pert <- plot_data %>% 
      filter(Disease == "Pertussis") %>%
      plot_ly(x = ~Year, y = ~value, color = ~Disease) %>% 
      add_markers( error_y = list(
        color='#7E7E7E',
        type = "data",
        symmetric = FALSE,
        array = ~upper-value,
        arrayminus = ~value-lower),
        marker = list(color='#000000'),
        showlegend = FALSE,
        hovertemplate = paste(
          "<b>Value: %{y:,.0f}</b><br><b>Year: %{x}",
          "<extra></extra>"
        )
      ) %>% 
      add_lines(
        y = ~modelValue,
        line = list(color = "#CC6677"),
        showlegend = TRUE,
        name = "Pertussis model value",
        hovertemplate = paste(
          "<b>Model Value: %{y:,.0f}</b><br><b>Year: %{x}",  
          "<extra></extra>"
        )
      ) %>%
      layout(
        annotations = list(x = 0.5 , y = 1.02, text = "Pertussis", showarrow = FALSE, 
                           xref='paper', yref='paper', font = list(size = 15)),
        yaxis = list(rangemode = "tozero"),
        hovermode = "x"
      )
    
    subplot(p_dip, p_tet, p_pert, nrows = 1) %>% 
      layout(legend = list(orientation = 'h')) %>% 
      plotly::config(
        toImageButtonOptions = list(filename = "GBD-calibration-plot")
      )
    
  })
  
  output$own_data_calibration <- renderPlotly({
    
    mo <- simul_baseline()
    
    if(is.null(mo)) {mo=tibble(year=integer(0),
                               value=numeric(0),
                               unit=character(0),
                               disease=character(0),
                               age_group=character(0),
                               variable=character(0))}
    
    moNeeded <- mo %>%
      filter(age_group=="All", variable %>% str_starts('rep_Clin_')) %>% 
      select(Year=year, modelValue=value, Disease=disease)

    plot_data <- req(custom_burden_data()) %>% 
      pivot_longer(!Year, names_to="Disease", values_to = "value") %>% 
      mutate(type="Data") %>% 
      select(Year, value, Disease) %>% 
      left_join(moNeeded, by = c('Year', 'Disease')) %>%
      filter(Year%between%caliTimeRange())
    
    p <- ggplot(
      data = plot_data,
      mapping = aes(
        x = Year, y = value, colour = Disease, group = 1,
        text = sprintf(
          "Value: %s<br>Year: %s<br>", 
          format(value, big.mark = ",", scientific = FALSE), 
          Year
        )
      )
    ) +
      geom_point(size = 3, color='Black') +
      geom_line(
        aes(
          y=modelValue, 
          text = sprintf(
            "Model Value:%s<br>Year: %s<br>",  
            format(modelValue, big.mark = ",", scientific = FALSE), 
            Year
          )
        ),
        size = 0.5
      ) +
      expand_limits(y = 0) +
      scale_y_continuous(labels = scales::comma) +
      scale_color_hue(direction = 1) +
      theme_minimal() +
      labs(
        shape=''
      ) +
      theme(axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            legend.position="bottom") +
      scale_x_continuous(breaks=scales::pretty_breaks())+
      facet_wrap(vars(Disease), scales='free_y', nrow = 1)
    
    calibration_fit(p)
    
    ggplotly(p, tooltip = c("text")) %>% 
      layout(
        legend = list(orientation = 'h'),
        hovermode = 'x'
      )
  })
  
  ### DTP Clinical Burden:Diphtheria ###
  
  # Verify button
  observeEvent(input$verifyDip, {
    showModal(modalDialog(
      p("The tool will run with the default data. Please make sure you are happy with the default data set."),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("verifyDipInfo", "Proceed")
      )
    ))
  })
  
  observeEvent(input$verifyDipInfo, {
    showModal(modalDialog(
      p("Data has been verified"),
      footer = NULL
    ))
    
    Sys.sleep(1)
    
    shinyjs::disable("verifyDip")
    
    removeModal()
  })
  
  # download template
  output$templateDip <- downloadHandler(
    filename = function() {
      paste0("template.xlsx")
    },
    content = function(file) {
      
      df <- data.frame(x = seq(1:10), y = seq(1:10))
      write_xlsx(df, file)
    }
  )
  
  ### DTP Clinical Burden:Tetanus ###
  
  # Verify button
  observeEvent(input$verifyTet, {
    showModal(modalDialog(
      p("The tool will run with the default data. Please make sure you are happy with the default data set."),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("verifyTetInfo", "Proceed")
      )
    ))
  })
  
  observeEvent(input$verifyTetInfo, {
    showModal(modalDialog(
      p("Data has been verified"),
      footer = NULL
    ))
    
    Sys.sleep(1)
    
    shinyjs::disable("verifyTet")
    
    removeModal()
  })
  
  # download template
  output$templateTet <- downloadHandler(
    filename = function() {
      paste0("template.xlsx")
    },
    content = function(file) {
      
      df <- data.frame(x = seq(1:10), y = seq(1:10))
      write_xlsx(df, file)
    }
  )
  
  ### Diseases Tables ###
  
  # switch between tables and plots
  viewType <- reactiveVal("table")
  observeEvent(input$asPlt, {
    viewType("plot")
  })
  observeEvent(input$asTbl, {
    viewType("table")
  })
  # Pertussis
  output$PertCase <- renderUI({
    if(viewType() == "plot") {
      plotlyOutput("plPertCase")
    } else {
      reactableOutput("tbPertCase")
    }
  })
  
  output$tbPertCase <- renderReactable({
    tbCountryData() %>%
      mutate(across(!contains("Year"), function(x){as.integer(0.014*x)})) %>%
      reactable_format()
  })
  
  output$plPertCase <- renderPlotly({
    tbCountryData() %>%
      mutate(across(!contains("Year"), function(x){as.integer(0.014*x)})) %>%
      pivot_longer(cols = -c(Year), names_to = "Age_grp", values_to = "Cases") %>%
      mutate(Age_grp = as_factor(Age_grp)) %>%
      plot_ly(x = ~Year, y = ~Cases) %>%
      add_lines(linetype = ~Age_grp)
  })
  
  output$PertDeath <- renderUI({
    if(viewType() == "plot") {
      plotlyOutput("plPertDeath")
    } else {
      reactableOutput("tbPertDeath")
    }
  })
  output$tbPertDeath <- renderReactable({
    tbCountryData() %>%
      mutate(across(!contains("Year"), function(x){as.integer(0.01*x)})) %>%
      reactable_format()
  })
  
  output$plPertDeath <- renderPlotly({
    tbCountryData() %>%
      mutate(across(!contains("Year"), function(x){as.integer(0.01*x)})) %>%
      pivot_longer(cols = -c(Year), names_to = "Age_grp", values_to = "Deaths") %>%
      mutate(Age_grp = as_factor(Age_grp)) %>%
      plot_ly(x = ~Year, y = ~Deaths) %>%
      add_lines(linetype = ~Age_grp)
  })
  
  # Diphtheria
  output$DipCase <- renderUI({
    if(viewType() == "plot") {
      plotlyOutput("plDipCase")
    } else {
      reactableOutput("tbDipCase")
    }
  })
  
  output$tbDipCase <- renderReactable({
    tbCountryData() %>%
      mutate(across(!contains("Year"), function(x){as.integer(0.008*x)})) %>%
      reactable_format()
  })
  
  output$plDipCase <- renderPlotly({
    tbCountryData() %>%
      mutate(across(!contains("Year"), function(x){as.integer(0.008*x)})) %>%
      pivot_longer(cols = -c(Year), names_to = "Age_grp", values_to = "Cases") %>%
      mutate(Age_grp = as_factor(Age_grp)) %>%
      plot_ly(x = ~Year, y = ~Cases) %>%
      add_lines(linetype = ~Age_grp)
  })
  
  output$DipDeath <- renderUI({
    if(viewType() == "plot") {
      plotlyOutput("plDipDeath")
    } else {
      reactableOutput("tbDipDeath")
    }
  })
  
  output$tbDipDeath <- renderReactable({
    tbCountryData() %>%
      mutate(across(!contains("Year"), function(x){as.integer(0.001*x)})) %>%
      reactable_format()
  })
  
  output$plDipDeath <- renderPlotly({
    tbCountryData() %>%
      mutate(across(!contains("Year"), function(x){as.integer(0.001*x)})) %>%
      pivot_longer(cols = -c(Year), names_to = "Age_grp", values_to = "Deaths") %>%
      mutate(Age_grp = as_factor(Age_grp)) %>%
      plot_ly(x = ~Year, y = ~Deaths) %>%
      add_lines(linetype = ~Age_grp)
  })
  
  # Tetanus
  output$TetCase <- renderUI({
    if(viewType() == "plot") {
      plotlyOutput("plTetCase")
    } else {
      reactableOutput("tbTetCase")
    }
  })
  
  output$tbTetCase <- renderReactable({
    tbCountryData() %>%
      mutate(across(!contains("Year"), function(x){as.integer(0.024*x)})) %>%
      reactable_format()
  })
  
  output$plTetCase <- renderPlotly({
    tbCountryData() %>%
      mutate(across(!contains("Year"), function(x){as.integer(0.024*x)})) %>%
      pivot_longer(cols = -c(Year), names_to = "Age_grp", values_to = "Cases") %>%
      mutate(Age_grp = as_factor(Age_grp)) %>%
      plot_ly(x = ~Year, y = ~Cases) %>%
      add_lines(linetype = ~Age_grp)
  })
  
  output$TetDeath <- renderUI({
    if(viewType() == "plot") {
      plotlyOutput("plTetDeath")
    } else {
      reactableOutput("tbTetDeath")
    }
  })
  
  output$tbTetDeath <- renderReactable({
    tbCountryData() %>%
      mutate(across(!contains("Year"), function(x){as.integer(0.001*x)})) %>%
      reactable_format()
  })
  
  output$plTetDeath <- renderPlotly({
    tbCountryData() %>%
      mutate(across(!contains("Year"), function(x){as.integer(0.001*x)})) %>%
      pivot_longer(cols = -c(Year), names_to = "Age_grp", values_to = "Deaths") %>%
      mutate(Age_grp = as_factor(Age_grp)) %>%
      plot_ly(x = ~Year, y = ~Deaths) %>%
      add_lines(linetype = ~Age_grp)
  })
  
  ## 1.4. Costs ----
  
  # move on to the next page
  observeEvent(input$next_costs, {
    updateTabItems(
      session = session,
      inputId = "menu",
      selected = "model2"
    )
  })
  
  # load the initial costs
  init_costs <- read_excel(here("data/DTP Costs initial values.xlsx")) %>% 
    select(inputId, value = usd_value)
  # a reactiveVal object to hold all costs used in the app
  cost_data <- reactiveVal(init_costs)

  # pop up modal costs
  observeEvent(input$costs_info, {
    showModal(
      modalDialog(title = "Cost Details", HTML("
        <p><b>Currency: </b> You can add or view the costs in either local currency or United States dollars (USD) by selecting your preferred currency at the start of Step 1. Once you have made a selection, this is the currency that will be used throughout the app. If you want to change the currency, you will need to restart the app and make a new currency selection. If you change your currency selection, the default costs will automatically be calculated using a constant exchange rate for the average 2021 value. For Uganda, the exchange rate used is USD 1 = UGX 3544. This rate is from 8th October 2021 as accessed from https://www1.oanda.com/currency/converter/. This is the exchange rate used in the Ugandan Booster Dose Costing Study and aligns with the base year (2021) for costs.</p>
        
        <p><b>Cost of vaccination:</b> Cost of vaccination is separated into 1) the cost of routine vaccination delivery (by dose); and 2) the unit cost of the vaccine (by dose). The cost of routine vaccination delivery is further split by delivery location, either at a health facility or outreach site (including schools). We assume the unit cost of vaccines will be constant given the history of UNICEF prices for these vaccines. Costs are incremental.<p> 
 
        <p><b>Cost of illness: </b> This is captured as the average cost per inpatient case and average cost per outpatient case. The cost is shown from the provider perspective (the government) and is direct cost only. 
        
        <p><b>Default values: </b>  The default values are drawn from a costing study for DTPCV booster doses in Uganda and published literature for the cost of illness estimates. For more details on these, please refer to the Resources and Model information pages.

        <p><b>Financial and economic costs: </b>  We allow the user to indicate the proportion of each cost that will be paid directly by government (as opposed to paid by donors or provided in-kind). This is used as an approximation of financial versus economic costs. However, throughout the DTP Boost tool, we refer to the costs as either 'Full cost (donor and government)' or 'Proportion paid by government'. By default we use the proportion paid by government for the budget impact analysis and the full costs for the cost-effectiveness analyses. However, users can switch between the two by using the dropdown menu on the results pages (Step 4). economic costs. In the case where 100% of the costs are paid by the government with no donor support, the two are equivalent. 

        <p><b>Discount rate: </b> The default discount rate used is 3.5%. This value can be changed by the user in Step 1.

        <p><b>Inflation: </b> All default costs have been adjusted for inflation using the consumer price index (CPI) for Uganda, except the cost of vaccines as we assume these will remain constant for the time horizon of the study. CPI values were sourced from the International Monetary Fund’s World Economic Outlook database (April 2022). The values were accessed on 23 August 2022. The average inflation rate projected for 2022 - 2027 is 5%.

        <p><b>Perspective: </b> The perspective of the study is the provider, which we assume is equivalent to the government. No societal costs are included.

        <p><b>Study time horizon: </b> The budget impact analysis and cost-effectiveness analysis have a 15 year horizon, which starts in the first year in which a new booster dose can be added (currently 2024). If additional booster doses are added in future years, the full impact of these may not be captured."
      ),
      easyClose = TRUE,
      )
    )
  })
  
  # local currency based on the selected country
  local_currency <- eventReactive(input$selectedCountry, {
    get_exchange_rates() %>%
      filter(ISO3_Code == input$selectedCountry) %>%
      pull(Code)
  })
  
  # update currency with user selected currency
  observeEvent(input$selectedCountry, {
    # do not run when a session is being uploaded
    if (is.null(input$file_parameters)) {
      updateRadioGroupButtons(
        session = session,
        inputId = "selectedCurrency",
        choices = c("USD",local_currency())
      )
    }
  })
  
  # Cost of vaccination per dose UI ----
  observe({
    
    if(input$va_b != "None" && input$vv_b != "None") {
      shinyjs::show("del_b")
      shinyjs::show("vax_b")
    } else {
      shinyjs::hide("del_b")
      shinyjs::hide ("vax_b")
    } 
    
    if(input$va_cb != "None" && input$vv_cb != "None") {
      shinyjs::show("del_cb")
      shinyjs::show("vax_cb")
    } else {
      shinyjs::hide("del_cb")
      shinyjs::hide("vax_cb")
    }
    
    if(input$va_ab != "None" && input$vv_ab != "None") {
      shinyjs::show("del_ab")
      shinyjs::show("vax_ab")
    } else {
      shinyjs::hide("del_ab")
      shinyjs::hide("vax_ab")
    }
    
    if(input$vv_m != "None") {
      shinyjs::show("del_m")
      shinyjs::show("vax_m")
    } else {
      shinyjs::hide("del_m")
      shinyjs::hide("vax_m")
    }
    
  })
  
  # Update costs ----
  observe({
    LOG("A currency change was triggered...")
    if (isTRUE(selectionsVerified())) {return()}
    updateCosts(
      cost_data,
      tbCostParmsCurrency = 'USD',
      selectedCurrency = input$selectedCurrency,
      input$vv_ps,
      input$vv_b,
      input$vv_cb,
      input$vv_ab,
      input$vv_m,
      session = session
    )
    
  })
  
  ## 1.6 Save Country Profile ----
  
  # coverage data
  coverage_data <- reactive({
    if(!is.null(input$vaxInput)) {                   # user uploaded coverage data on the coverage page (1.2)
      data <- req(custom_coverage())
    } else if(!is.null(input$file_parameters)) {     # user uploaded a session with a coverage table included
      unzip(input$file_parameters$datapath, files = "model_workbook.xlsx", exdir = tempdir())
      file_path <- file.path(tempdir(), "model_workbook.xlsx")
      data <- read_excel(file_path, sheet = "coverage_table")
    } else if (input$restoreSpecialSession != 0) {   # user uploaded a special session (Load AFENET Session)
      data <- read_excel('AFENET_Custom_Session.xlsx', sheet = "coverage_table")
    } else {
      data <- getVaccineSchedule(input$selectedCountry, fillBlanks = T)
    }
    data
  })
  
  # save parameters
  output$save_session <- downloadHandler(
    filename = function() {
      paste0("DTP_session-", Sys.Date(), ".zip")
    },
    content = function(file) {
      
      withProgress(
        message = "Saving session, please wait...", {
            # burden data
            burden_data <- function() {
              if (input$data_source_calibration == "OWN DATA") {
                data <- req(custom_burden_data())
              } else {
                data <- get_clinical_burden(input$data_source_calibration, input$selectedCountry)
              }
              return(data)
            }
            
            scenarios_exist = FALSE # start by assuming there are no scenarios
            
            if (is.null(simul_baseline())) { # pre calibration
              worksheet$parameters <- req(template())
              worksheet$coverage_table <- coverage_data()
              
            } else if (is.null(simul_packages$names)) { # post calibration, pre interventions
              worksheet$parameters <- req(template())
              worksheet$coverage_table <- coverage_data()
              worksheet$clinical_burden <- burden_data()
              worksheet$baseline <- simul_baseline()
              
            } else { # post interventions
              scenarios_exist = TRUE
              worksheet$econ_output_summary <- req(costs()$resultSummary)
              worksheet$econ_output_annual <- req(costs()$resultAnnual)
              worksheet$baseline <- simul_baseline()
              worksheet$strategies <- req(booster_tbl())
              worksheet$coverage_table <- coverage_data()
              worksheet$clinical_burden <- burden_data()
              worksheet$parameters <- req(template())
            }

            excel_worksheet <- reactiveValuesToList(worksheet)

            if (scenarios_exist) {
              # create a workbook for the epi output with each scenario having its own sheet
              # we do this to avoid breaching the excel row limit 
              available_scenarios <- model_outputs() %>% 
                pull(scenario) %>% 
                unique()
              
              epi_scenario_outout <- map(available_scenarios, \(x) {
                model_outputs() %>% 
                  filter(scenario == x)
              })
              
              names(epi_scenario_outout) <- available_scenarios
              
              write_xlsx(excel_worksheet, paste0(tempdir(), "/model_workbook.xlsx"))
              
              write_xlsx(epi_scenario_outout, paste0(tempdir(), "/epi_scenario_outout.xlsx"))
              
              zip(
                file,
                files = c(
                  paste0(tempdir(), "/model_workbook.xlsx"), 
                  paste0(tempdir(), "/epi_scenario_outout.xlsx")
                ),
                flags = '-j'
              )
            } else {
              write_xlsx(excel_worksheet, paste0(tempdir(), "/model_workbook.xlsx"))
              
              zip(
                file,
                files = c(paste0(tempdir(), "/model_workbook.xlsx")),
                flags = '-j'
              )
            }
        }
      )
      
    },
    contentType = "application/zip"
  )
  
  # Step 2. Calibrate Model ----
  
  # 2.3 Assessing uncertainty
  observeEvent(input$next_uncertainty,{
    updateTabsetPanel(session, "menu", selected = "model3")
  })
  
  # Step 3. Booster Strategy ----
  maxRelCovEC = reactive({
    changeWhen <- c(input$va_ib, input$vsy_ib, input$vytrt_ib, input$vtc_ib,
                    input$va_icb, input$vsy_icb,
                    input$boost_infant_switch, input$boost_child_switch)
    mat = isolate(getShinyInputCoverage(input, coverage_table=coverage_data(), baseline=F, startyear=startyear, tyears=tyears)[['cov']])
    value <- mat[ageId(input$va_ib), as.character(input$vsy_ib)]
    result <- max(min(1/value, 100), 1)
    LOG("maxRelCovEC was recalculated as {result} from {value} using {paste0(changeWhen,collapse=',')}")
    return(result)
  })
  observeEvent(maxRelCovEC(), {
    newRelCov <- floor(maxRelCovEC() / 0.05) * 0.05
    LOG("maxRelCovEC changed to {maxRelCovEC()}")
    updateSliderInput(
      session = session,
      'vps_ib', 
      max = newRelCov
    )
  })
  maxRelCovCB = reactive({
    changeWhen <- c(input$va_icb, input$vsy_icb, input$vytrt_icb, input$vtc_icb,
                    input$va_iab, input$vsy_iab,
                    input$boost_child_switch, input$boost_adolescent_switch)
    mat = isolate(getShinyInputCoverage(input, coverage_table=coverage_data(), baseline=F, startyear=startyear, tyears=tyears)[['cov']])
    value <- mat[ageId(input$va_icb), as.character(input$vsy_icb)]
    result <-  max(min(1/value, 100), 1)
    LOG("maxRelCovCB was recalculated as {result} from {value} using {paste0(changeWhen,collapse=',')}")
    return(result)
  })
  observeEvent(maxRelCovCB(), {
    newRelCov <- floor(maxRelCovCB() / 0.05) * 0.05
    LOG("maxRelCovCB changed to {maxRelCovCB()}")
    updateSliderInput(
      session = session,
      'vps_icb', 
      max = newRelCov
    )
  })
  
  output$dbgMat <- renderPlot({
    matId = input$dbgSelectMat
    baseline = input$dbgIsBaseline
    if (str_starts(matId, 'cov')) {
      mat = getShinyInputCoverage(input, coverage_table=coverage_data(), baseline=baseline, startyear=startyear, tyears=tyears)[[matId]]
    } else if (str_starts(matId, 'mov')) {
      mat = getShinyInputVaccineMovementVectors(input, baseline=baseline, startyear=startyear, tyears=tyears)[[matId]]
    } else {
      mat = matrix()
    }
    if (length(mat)<=1) {
      LOG("Unexpected selection in dbgSelectMat element")
    }
    reshape2::melt(mat) %>%
      select(ageId=Var1, year=Var2, value) %>%
      ggplot() +
      aes(x = year, y = ageId, fill = value) +
      geom_tile() +
      scale_fill_distiller(palette = "Spectral", direction = -1,
                           limits = c(0,1)) +
      labs(x = "Year", y = "AgeID", fill = "Coverage") +
      theme_minimal() +
      theme(legend.position = "bottom")
  })
  # move on to the next page
  observeEvent(input$next_dbs, {
    updateTabItems(
      session = session,
      inputId = "booster_strategy_tab",
      selected = "2"
    )
  })
  
  # DTP Booster Doses info popup
  observeEvent(input$dtpBoosterInfo, {
    showModal(modalDialog(
      title = "DTP Booster Doses",
      easyClose = TRUE,
      HTML(
        "<b>Options</b>
        <p>
        <p>You may build a vaccination strategy using any number of the following options.</p>

        <li>Early childhood booster: recommended dose given between 12 - 23 months old</li>
        <li>Child booster: recommended dose given between 4 - 7 years old</li>
        <li>Adolescent booster: recommended dose given between 9 - 15 years old</li>
         <p>
         <p>
         <p>
        <b>Settings</b>
        <p>
        <p>You may define the following settings for each vaccination option you include in your strategy.</p>

        <li>Target age</li>
        <li>Vaccine</li>
        <li>Year of introduction</li>
        <li>Coverage in year of introduction</li>
        <li>Target operational coverage</li>
        <li>Years to reach target coverage</li>
        <li>Average cost per dose</li>
        <li>Proportion paid directly by government</li>
        <li>Delivery platform (Health facility or Outreach site)</li>

        "
      )
    ))
  })
  
  # Pulsing "Relative coverage" info popup
  pulse_popup <- reactive({
    input$pulseRelativeCoverageInfoChild
    input$pulseRelativeCoverageInfoInfant
  })
  observeEvent(pulse_popup(), ignoreNULL = TRUE, ignoreInit = TRUE, {
    showModal(modalDialog(
      title = "Age Targeted Supplementary Vaccination",
      easyClose = TRUE,
      HTML(
        "<b>Offer to expanded age cohorts in the year of introduction</b>
        <p></p>
        <p>This check box allows you to offer the booster dose to an expanded range of age cohorts in the year of introduction. This provides protection to cohorts that are in the eligible age range for the respective dose, but would otherwise miss receiving the dose because they are past the chosen target age in the routine schedule. The coverage will be the input value for ‘Coverage in the year of introduction’. The same vaccine type will be used.</p>

        <p>For example, if you select 'Early childhood (EC) booster' offered at 18mths starting in 2026 and 'Child booster' offered at age 4yr starting in 2026, you will be able to select ‘Expanded cohort’ to offer the EC booster dose to all ages between the two target age groups (i.e. >18 months and <4 years). </p>
        "
      )
    ))
  })
  
  # DTP Vaccinations popup info
  observeEvent(input$dtpVaccinationsInfo, {
    showModal(modalDialog(
      title = "Other DTP Vaccinations ",
      easyClose = TRUE,
      HTML(
        "<b>Options</b>
        <p>
        <p>You may build a vaccination strategy on the following option.</p>

        <li>Maternal vaccination: dose(s) given to pregnant women during antenatal care (ANC) visits</li>
         <p>
         <p>
         <p>
        <b>Settings</b>
        <p>
        <p>You may define the following settings for each vaccination option you include in your strategy.</p>

        <li>Target age</li>
        <li>Vaccine</li>
        <li>Year of introduction</li>
        <li>Coverage in year of introduction</li>
        <li>Target operational coverage</li>
        <li>Years to reach target coverage</li>
        <li>Average cost per dose</li>
        <li>Proportion paid directly by government</li>
        <li>Delivery platform (Health facility or Outreach site)</li>
        "
      )
    ))
  })
  
  # DTP Costs of Introduction info
  observeEvent(input$dtpCostofIntroInfo, {
    showModal(modalDialog(
      title = "Introduction Costs ",
      easyClose = TRUE,
      HTML(
        " <p>The introduction cost is the average total once-off cost of introducing new vaccine dose(s) in a given year. This is a fixed cost regardless of the number of doses introduced in the introduction year. </p>

          <p>The button at the bottom allows users to indicate whether this cost will only be incurred once, even if additional booster doses are introduced in future years (e.g. in a phased, multi-year roll out strategy of different booster doses). If this option is selected, the introduction cost will only be incurred once regardless of the number of doses or year(s) of introduction. If this option is not selected, the total introduction cost will be incurred again for each booster dose introduced in a different year.</p>
           
         "
      )
    ))
  })
  
  # When we toggle on a booster open the settings page for that booster:
  observeEvent(input$boost_infant_switch, ignoreInit = T,ignoreNULL = T,{ if(input$boost_infant_switch==TRUE) toggleDropdownButton('boost_infant_settings') })
  observeEvent(input$boost_child_switch, ignoreInit = T,ignoreNULL = T,{ if(input$boost_child_switch==TRUE) toggleDropdownButton('boost_child_settings') })
  observeEvent(input$boost_adolescent_switch, ignoreInit = T,ignoreNULL = T,{ if(input$boost_adolescent_switch==TRUE) toggleDropdownButton('boost_adolescent_settings') })
  observeEvent(input$boost_maternal_switch, ignoreInit = T,ignoreNULL = T,{ if(input$boost_maternal_switch==TRUE) toggleDropdownButton('boost_maternal_settings') })
  cov_upd <- reactive({
    getShinyInputCoverage(input,
                          coverage_table=coverage_data(),
                          baseline=FALSE,
                          startyear=startyear,
                          tyears=tyears)
  })
  output$coverage_widget_ib_settings <- renderPlot({
    req(cov_upd())
    xy <- cov_upd()$covbyr[ageId(input$va_ib),]
    xx <- as.integer(names(xy))
    yy <- as.numeric(xy)
    y_start <- input$vsy_ib
    y_end <- y_start + input$vytrt_ib 
    coverage_plot(xx = xx, yy = yy, y_start = y_start, y_end = y_end, startyear = startyear)
  })
  output$coverage_widget_cb_settings <- renderPlot({
    req(cov_upd())
    xy <- cov_upd()$covcbyr[ageId(input$va_icb),]
    xx <- as.integer(names(xy))
    yy <- as.numeric(xy)
    y_start <- input$vsy_icb
    y_end <- y_start + input$vytrt_icb 
    coverage_plot(xx = xx, yy = yy, y_start = y_start, y_end = y_end, startyear = startyear)
  })
  output$coverage_widget_ab_settings <- renderPlot({
    req(cov_upd())
    xy <- cov_upd()$covabyr[ageId(input$va_iab),]
    xx <- as.integer(names(xy))
    yy <- as.numeric(xy)
    y_start <- input$vsy_iab
    y_end <- y_start + input$vytrt_iab 
    coverage_plot(xx = xx, yy = yy, y_start = y_start, y_end = y_end, startyear = startyear)
  })
  
  # Provide a default name for created package ----
  observe({
    label_default <- paste0(
      # DTP Booster Doses
      ifelse(input$boost_infant_switch, "+ EC", ""),
      ifelse(input$boost_child_switch, "+C", ""),
      ifelse(input$boost_adolescent_switch, "+A", ""),
      
      # Other DTP Vaccinations
      ifelse(input$boost_maternal_switch, "+M", ""),
      ifelse(input$boost_catchup_switch, "+ CatchupCampaign", ""),
      ifelse(input$boost_mass_switch, "+ MassVaccine", "")) %>%
      substring(first = 3)
    
    updateTextInput(session, "package_name", label = label_default, value = label_default)
  })
  
  # Provide a limit to the length of name
  shinyjs::runjs("$('#package_name').attr('maxlength',30)")
  
  # infant booster delivery sliders
  observe({
    updateSliderInput(
      session = session,
      inputId = "del_o_ib",
      value = 100 - input$del_hf_ib
    )
  })
  
  observe({
    updateSliderInput(
      session = session,
      inputId = "del_hf_ib",
      value = 100 - input$del_o_ib
    )
  })
  
  # child booster delivery sliders
  observe({
    updateSliderInput(
      session = session,
      inputId = "del_o_icb",
      value = 100 - input$del_hf_icb,
    )
  })
  
  observe({
    updateSliderInput(
      session = session,
      inputId = "del_hf_icb",
      value = 100 - input$del_o_icb
    )
  })
  
  # adolescent booster delivery sliders
  observe({
    updateSliderInput(
      session = session,
      inputId = "del_o_iab",
      value = 100 - input$del_hf_iab,
    )
  })
  
  observe({
    updateSliderInput(
      session = session,
      inputId = "del_hf_iab",
      value = 100 - input$del_o_iab
    )
  })
  
  # maternal booster delivery sliders
  observe({
    updateSliderInput(
      session = session,
      inputId = "del_o_im",
      value = 100 - input$del_hf_im
    )
  })
  
  observe({
    updateSliderInput(
      session = session,
      inputId = "del_hf_im",
      value = 100 - input$del_o_im
    )
  })
  
  simul_baseline <- reactiveVal()
  
  # Simulation packages
  simul_packages <- reactiveValues(simul = list(), nb = 0, names = NULL)
  
  model_inputs <- reactiveVal()
  
  model_outputs <- reactive({
    
    LOG("Combining models")
    tbSimulPackages <- seq_along(simul_packages$simul) %>%
      map(~mutate(simul_packages$simul[[.x]]$results_epi,
                  scenario=simul_packages$names[[.x]])) %>%
      do.call(bind_rows, .)
    
    LOG("Including baseline")
    
    validate(
      need(simul_baseline(), HTML('Please design at least one booster strategy to enable results.')),
    )
    
    bind_rows(simul_baseline() %>% mutate(scenario="Baseline"),
              tbSimulPackages) -> result
    saveDebugRDS(result, "result.rds")
    epi_result <- result
    LOG("Calculating epi_result")
    epi_result <- epi_result %>% 
      mutate(
        value = if_else(
          str_detect(variable, "protected_prop"),
          round(value, digits = 2),
          round(value)
        )
      )
    
    epi_result
    
  })
  
  # Run model
  observeEvent(input$go_interventions, {
    
    # Check the user has completed calibration")
    if (simul_packages$nb <= 0) {
      showModal(modalDialog(
        title = "No Baseline",
        "Please run the calibration step first to create a baseline model.",
        easyClose = TRUE
      ))
      return(NULL)
    }
    
    if (is.null(input$strat_intro_cost) || input$strat_intro_cost < 0) {
      show_toast(
        title = "Fixed cost",
        text = "Please provide a valid value for the fixed costs",
        type = "error",
        timer = 5000
      )
      return(NULL)
    }
    
    if(input$package_name == ""){
      show_toast(
        title = "Provide Name",
        text = "Please provide a name for your strategy and run again",
        type = "error"
      )
      return(NULL)
    }
    
    if(input$package_name %in% c("Baseline", simul_packages$names)){
      show_toast(
        title = "Provide Another Name",
        text = "This name is already in use, please provide a different name for your strategy",
        type = "error"
      )
      return(NULL)
    }
    
    showNotification(
      tagList(
        span(h4(img(src="CustomLoader2.gif"), "Simulation Running...", style = "color:#FFFFFF !important;")),
        tags$div(id="myProgress", tags$div(id="myBar")),
        span(p("Typically runs in 5 to 8 minutes", style = "color:#FFFFFF !important;"))
      ),
      duration = NULL,
      type = "message",
      id = "model_interventions",
      session = session,
      closeButton = FALSE
    )
    
    
    # Define Scenarios
    LOG("TODO: Check the user has completed calibration")
    # if (simul_packages$nb <= 0) {
    #   mo_baseline <- getOrCreate('mo_baseline.rds', function() {
    #     parameters = c(getModelParameters(),
    #                    getShinyInputs(input=input, coverage_table=coverage_data(), baseline = T, startyear = startyear, tyears = tyears))
    #     saveDebugRDS(parameters, 'shinyInputParameters_Baseline.rds')
    #     mo <- run_model(parameters=parameters, coverage_table=coverage_data(), iso3=input$selectedCountry, timesteps=timesteps)
    #     mo <- mo %>%
    #       lapply(last) %>%
    #       do.call(bind_rows, args=.) %>% ungroup() %>% 
    #       mutate(time=lubridate::date_decimal(year))
    #     mo
    #   })
    #   simul_baseline(mo_baseline %>% mutate(scenario="Baseline"))
    # }
    LOG("Running Intervention")
    
    setLockedState(isLocked=TRUE)
    
    boostSwitchNames <- names(input)[str_detect(names(input),'^boost_.+_switch$')]
    active <- sapply(boostSwitchNames, function(name){input[[name]]}) %>% unname()
    interventionNames <- boostSwitchNames[active] %>% str_replace_all('boost_|_switch','') %>% paste0(collapse='_')
    LOG("interventionNames = {interventionNames}")
    time_start <- Sys.time()
    parameters = c(getModelParameters(),
                   getShinyInputs(input=input, coverage_table=coverage_data(), baseline = F, startyear = startyear, tyears = tyears))

    input_list <- reactiveValuesToList(input)
    saveDebugRDS(input_list, glue::glue('shinyInputList_{interventionNames}.rds'))
    saveDebugRDS(parameters, glue::glue('shinyInputParameters_{interventionNames}.rds'))
    
    model_inputs(input_list)
    
    run_model(parameters=parameters,
              timesteps = timesteps,
              coverage_table = coverage_data(),
              iso3 = input$selectedCountry,
              cacheAs=glue::glue('mo_{interventionNames}.rds'),
              inParallel = TRUE,
              DEBUG=DEBUG) %...>%
      with({
        tbResult <- mutate(., time=year)
        input_list <- model_inputs()
        
        saveDebugRDS(tbResult, glue::glue('eo_mo_{interventionNames}.rds'))
        
        dtSecs <- ceiling(as.numeric(Sys.time() - time_start, units = "secs"))
        
        switches_all <- names(input_list)[names(input_list) %>% str_ends('_switch')] %>% sapply(function(id){input_list[[id]]})
        
        switches_selected <- names(switches_all[switches_all])
        
        switch_simple_names <- switches_selected %>%
          setNames(str_replace_all(.,'boost_|_switch',''),.)
        
        booster_strategies_mapping <- read_excel(here("data/DTPApp-BoosterStrategies.xlsx"))
        # lsInterventions is a list of lists
        # the top level lists each booster and the inner level is the inputid->value mapping.
        lsInterventions <- setNames(switches_selected,
                                    switches_selected) %>%
          # str_replace_all(switches_selected,'boost_|_switch','')) %>%
          lapply(function(.x) {  # .x is, eg, boost_maternal_switch=T
            booster_strategies_mapping %>%
              filter(SwitchId==.x) %>%
              pull(ParameterId, name = ParameterId) %>%  # find all parameter ids in this intervention
              lapply(function(id) {
                input_list[[id]]  # get their value
              })
          })
        
        # NB: If ecoVals changes then costsScenarios needs to account for that change
        ecoVals <- list(
          intro_cost=input_list$strat_intro_cost,
          intro_cost_pfin=input_list$strat_intro_cost_pfin,
          intro_once=input_list$strat_intro_applyonce
        )
        LOG("Adding intervention package to list")
        add_intervention_package(package_name=input_list$package_name, mo=tbResult, lsInterventions=lsInterventions,
                                 ecoVals=ecoVals, simul_packages=simul_packages)
        
        saveDebugRDS(reactiveValuesToList(simul_packages),
                     glue::glue('r_simul_packages_{length(simul_packages$names)}.rds'))
        
        removeNotification(id = "model_interventions", session = session)
        showNotification(span(h4(icon("check"), "Done"), paste0("Simulation ran in ", dtSecs, " seconds.")),
                         duration = 4, type = "default")
        
        setLockedState(isLocked=FALSE)
        updateTabsetPanel(session, "menu", selected = "model4")
      })
  })
  
  # Step 4. Explore Results ----
  
  ## 4.1 Epi Outputs ----
  
  shinyhelper::observe_helpers()
  
  packagesAsHTMLRankList <- reactive({
    LOG("packagesAsHTMLRankList()")
    lapply(seq_along(simul_packages$names), function(i){
      pkgName <- simul_packages$names[[i]]
      LOG("packagesAsHTMLRankList - {pkgName}")
      strategies <- get_strategies(simul_packages$simul)
      strategies <- strategies %>%
        filter(strategy_name == pkgName)
      
      tooltipTxt <- strategies %>%
        mutate(Value1 = if_else(str_ends(Label, "coverage"), paste0(Value1, "%"), Value1)) %>%
        mutate(Value1 = if_else(
          str_detect(Label, regex("cost of", ignore_case = TRUE)), 
          paste0(Value1," ",input$selectedCurrency), 
          Value1
        )) %>%
        mutate(Value1 = if_else(str_starts(Label, "Proportion"), paste0(Value1, "%"), Value1)) %>%
        mutate(Value=paste0(Value1,ifelse(Value2=="","",paste0("-",Value2)))) %>%
        group_by(strategy_name) %>%
        group_map(function(tbStrat,tbGrp){
          tbStrat %>%
            group_by(Intervention) %>%
            group_map(~.x %>%
                        transmute(HTML=paste("<li>",Label,": ",Value,"</li>")) %>%
                        pull(HTML) %>%
                        paste0(collapse = '') %>%
                        paste0("<h4>",.y$Intervention,"</h4>",.)) %>%
            paste0(collapse="\n") %>%
            paste0("<h3>",tbGrp$strategy_name,"</h3>", .)
        })
      result <- tags$div(pkgName)%>%shinyhelper::helper(
        type='inline',
        content=tooltipTxt,
        color = "#237D80"
      )
      
      result
    }) %>% as.list()
  })
  
  output$select_packages <- renderUI({
    LOG("makeBucketList()")
    makeBucketList(packagesAsHTMLRankList(),
                   inputIdShowList = "rank_list_show",
                   inputIdHideList = "rank_list_hide")
  })
  
  output$delete_packages <- renderUI({
    LOG("delete_packages")
    if("Baseline" %in% input$rank_list_hide) {
      return(HTML("To delete packages, remove the baseline from the hidden section."))
    }
    
    if(! is_empty(input$rank_list_hide)) {
      actionLink("confirm_delete_packages", label = span(icon("trash-alt"), " Delete packages that are hidden."))
    }
  })
  
  ### Process on "Delete package" ----
  observeEvent(input$confirm_delete_packages, {
    
    packages <- setdiff(input$rank_list_hide, "Baseline")
    
    simul_packages$simul <- simul_packages$simul[-which(simul_packages$names %in% packages)]
    simul_packages$nb <- simul_packages$nb - length(packages)
    simul_packages$names <- setdiff(simul_packages$names, packages)
    
  })
  
  # plot output for all the diseases
  output$model_plot <- renderPlotly({
    req(input$rank_list_show)  
    
    # saveDebugRDS(shiny::reactiveValuesToList(input), 'inp_for_plots.rds')
    if (input$epi_switch_y_axis==T) scale_option = "fixed" else scale_option = "free_y"
    count_type = input$epi_output_count_method
    output_variable = input$epi_output_variable
    if(count_type=="Rate" && output_variable == "Population protected") {
      showNotification("Population protected not available for PER 100,000. Plotting the COUNT value.", duration = 5)
      count_type = "Total"
    }
    
    
    
    pltModelOutput(
      model_outputs = isolate(model_outputs()),
      age_group = input$epi_output_age_group,
      scenarios = input$rank_list_show,
      #sim_window = input$epi_output_sim_window,
      sim_window = econAnalysisRange(),
      disease = "All",
      count_type = count_type,
      output_variable = output_variable,
      scale_option = scale_option
    ) 
      
  })
  # table output for all the diseases
  output$model_tbl <- renderReactable({
    req(input$rank_list_show)
    
    makeEpiTable(epiEconResultSummary(),
                 DIS = "DTP",
                 scenarios = input$rank_list_show)
  })
  
  # plot output for Diphtheria
  output$dip_model_plot <- renderPlotly({
    req(input$rank_list_show, model_outputs())
    count_type = input$epi_output_count_method
    output_variable = input$epi_output_variable
    if(count_type=="Rate" && output_variable == "Population protected") {
      showNotification("Population protected not available for PER 100,000. Plotting the COUNT value.", duration = 5)
      count_type = "Total"
    }
    pltModelOutput(
      model_outputs = isolate(model_outputs()),
      age_group = input$epi_output_age_group,
      scenarios = input$rank_list_show,
      #sim_window = input$epi_output_sim_window,
      sim_window = econAnalysisRange(),
      disease = "Diphtheria",
      count_type = count_type,
      output_variable = output_variable
    )
  })
  
  output$dip_model_tbl <- renderReactable({
    
    makeEpiTable(epiEconResultSummary(),
                 DIS = "Diphtheria",
                 scenarios = input$rank_list_show)
  })
  
  output$pert_model_plot <- renderPlotly({
    req(input$rank_list_show, model_outputs())
    count_type = input$epi_output_count_method
    output_variable = input$epi_output_variable
    if(count_type=="Rate" && output_variable == "Population protected") {
      showNotification("Population protected not available for PER 100,000. Plotting the COUNT value.", duration = 5)
      count_type = "Total"
    }
    pltModelOutput(
      model_outputs = isolate(model_outputs()),
      age_group = input$epi_output_age_group,
      scenarios = input$rank_list_show,
      #sim_window = input$epi_output_sim_window,
      sim_window = econAnalysisRange(),
      disease = "Pertussis",
      count_type = count_type,
      output_variable = output_variable
    )
  })
  
  output$pert_model_tbl <- renderReactable({
    
    makeEpiTable(epiEconResultSummary(),
                 DIS = "Pertussis",
                 scenarios = input$rank_list_show)
  })
  
  output$tet_model_plot <- renderPlotly({
    req(input$rank_list_show, model_outputs())
    count_type = input$epi_output_count_method
    output_variable = input$epi_output_variable
    if(count_type=="Rate" && output_variable == "Population protected") {
      showNotification("Population protected not available for PER 100,000. Plotting the COUNT value.", duration = 5)
      count_type = "Total"
    }
    pltModelOutput(
      model_outputs = isolate(model_outputs()),
      age_group = input$epi_output_age_group,
      scenarios = input$rank_list_show,
      #sim_window = input$epi_output_sim_window,
      sim_window = econAnalysisRange(),
      disease = "Tetanus",
      count_type = count_type,
      output_variable = output_variable
    ) 
  })
  
  output$tet_model_tbl <- renderReactable({
    makeEpiTable(epiEconResultSummary(),
                 DIS = "Tetanus",
                 scenarios = input$rank_list_show)
  })
  
  ### Costs reactives for Economics ####
  econStartYear <- reactiveVal(2025)
  econNumYears <- reactiveVal(15)  # model horizon
  
  econAnalysisRange <- reactive({
    c(
      econStartYear(),
      econStartYear() + econNumYears() - 1
    )
  })
  
  observeEvent({input$model_horizon},
               {econNumYears(parse_number(input$model_horizon))},
               ignoreNULL = T)
  
  create_costsBaseline <- function(econAnalysisRange) {
    costInputs = getShinyInputCosts(input)  # includes interventions values
    costInputs$intro_cost = 0
    result <- calculateCostsScenario(moAnnual = simul_baseline(),
                                     costInputs = costInputs,
                                     analysisRange = econAnalysisRange,
                                     discountRate = input$discount/100)
    result
  }
  
  costsBaseline <- reactive({
    validate(
      need(simul_baseline(), HTML('Please design at least one booster strategy to enable results.')),
    )
    create_costsBaseline(econAnalysisRange = econAnalysisRange())
  })
  
  create_costsScenarios <- function(costsBaseline, econAnalysisRange) {
    # force update for scenarios when baseline updates
    # this is so if you change costs page after model runs you can get
    # the latest costs pulled through (since these trigger the baseline to recalculate)
    costs_baseline_dependency = costsBaseline
    input_list_default = isolate(reactiveValuesToList(input))
    # Ensure that all booster switches are false by default
    input_list_default[str_detect(names(input_list_default), 'boost_.+_switch')] = F
    costInputs = getShinyInputCosts(input_list_default)
    lapply(seq_along(simul_packages$simul), function(i) {
      # this strategy contains the inputs list where the booster switches are all FALSE
      # we will update each one as needed
      input_list = input_list_default
      simul <- simul_packages$simul[[i]]
      
      interventions <- simul$interventions
      for (j in seq_along(interventions)) {
        intervention <- interventions[[j]]
        interventionName = names(interventions)[[j]]
        # Update the booster_switch for this intervention
        input_list[interventionName] = T  # set the booster slider to true for this intervention
        common_input_names <- intersect(names(intervention), names(input_list))
        input_list[common_input_names] <- intervention[common_input_names]
      }
      # ecoVals was list(intro_cost=input$strat_intro_cost, intro_cost_pfin = input$strat_intro_cost_pfin, intro_once=input$strat_intro_applyonce)
      ecoVals <- as.list(simul$ecoVals)
      input_list$strat_intro_cost = ecoVals$intro_cost
      input_list$strat_intro_cost_pfin = ecoVals$intro_cost_pfin
      input_list$strat_intro_applyonce = ecoVals$intro_once
      costInputs = getShinyInputCosts(input_list)
      result = calculateCostsScenario(moAnnual=simul$results_epi,
                                      costInputs=costInputs,
                                      analysisRange = econAnalysisRange,
                                      discountRate=input_list_default$discount/100)
      result
    }) -> results
    names(results) <- simul_packages$names

    results
  }
  
  costsScenarios <- reactive({
    LOG("Calculating costsScenarios()")
    create_costsScenarios(
      costsBaseline = costsBaseline(),
      econAnalysisRange = econAnalysisRange()
    )
  })
  
  create_costsComparisons <- function(costsBaseline, costsScenarios) {
    costInputs = getShinyInputCosts(input)  # includes interventions values
    cScen <- costsScenarios
    lapply(seq_along(cScen), function(i) {
      scenarioName <- names(cScen)[[i]]
      costsScenario <- cScen[[i]]
      calculateCostsComparison(costsBaseline, costsScenario)
    }) -> results
    names(results) <- names(cScen)
    
    results
  }
  
  costsComparisons <- reactive({
    LOG("Calculating costsComparisons()")
    create_costsComparisons(
      costsBaseline = costsBaseline(),
      costsScenarios = costsScenarios()
    )
  })
  
  scenarioCostsToTibble <- function(costsScenario, cType="Baseline", cScenario="Baseline") {
    # If we're calculating summed values (net/averted/icer) then note the year as the last year in the analysis:
    if (cType=='Comparison') costsScenario$year=last(costsScenario$year)
    # Now bind together the discounted and undiscounted rows and pivot variables longer
    bind_rows(as_tibble(costsScenario$Discounted)%>%mutate(discounting="Discounted",year=costsScenario$year),
              as_tibble(costsScenario$Undiscounted)%>%mutate(discounting="Undiscounted",year=costsScenario$year)) %>% 
      select(year, discounting, everything()) %>%
      pivot_longer(!c(year, discounting)) %>%
      mutate(type=cType, scenario=cScenario)
  }
  
  create_econResultAnnual <- function(costsBaseline, costsScenarios) {
    tbEconBaseline <- scenarioCostsToTibble(costsBaseline, cType="Baseline", cScenario="Baseline")
    tbEconScenarios <- lapply(seq_along(costsScenarios), function(i) {
      costsScenario <- costsScenarios[[i]]
      scenarioName <- names(costsScenarios)[[i]]
      scenarioCostsToTibble(costsScenario, cType="Scenario", cScenario=scenarioName)
    }) %>%
      do.call(what=bind_rows, args=.)
    tbEconComparison <- lapply(seq_along(costsComparisons()), function(i) {
      costsComparison <- costsComparisons()[[i]]
      scenarioName <- names(costsComparisons())[[i]]
      scenarioCostsToTibble(costsComparison, cType="Comparison", cScenario=scenarioName)
    }) %>%
      do.call(what=bind_rows, args=.)
    result <- bind_rows(tbEconBaseline,tbEconScenarios,tbEconComparison) %>% 
      select(year, discounting, type, scenario, variable=name, value) %>% 
      mutate(across(where(is.character),as_factor))
    
    result
  }
  
  econResultAnnual <- reactive({
    LOG("Calculating econResultAnnual()")
    create_econResultAnnual(
      costsBaseline = costsBaseline(), 
      costsScenarios = costsScenarios()
    )
  })
  
  create_econResultSummary <- function(econResultAnnual) {
    result <- econResultAnnual() %>%
      group_by(variable, discounting, type, scenario) %>%
      summarise(value=sum(value), year="All", .groups = 'drop') %>%
      select(year, discounting, type, scenario, variable, value)

    result
  }
  
  econResultSummary <- reactive({
    LOG("Calculating econResultSummary() reactive")
    create_econResultSummary(econResultAnnual())
  })
  
  epiEconResultSummary <- reactive({
    LOG("Calculating epiEconResultSummary() reactive")
    result <- econResultAnnual() %>%
      # Full timeline
      group_by(variable, discounting, type, scenario) %>%
      summarise(value=sum(value), year="All", .groups = 'drop') %>%
      select(year, discounting, type, scenario, variable, value)
    saveDebugRDS(result, 'r_epiEconResultSummary.rds')
    
    result <- result %>% 
      mutate(value = round(value))
  })
  
  costs <- reactive({
    LOG("Calculating costs() reactive")
    result <- list(
      resultSummary=econResultSummary() %>% 
        mutate(value = case_when(
          value < 10 ^ 6 ~ signif(value, digits = 3),
          .default = round(value / 1000) * 1000
        )),
      resultAnnual=econResultAnnual() %>% 
        mutate(value = case_when(
          value < 10 ^ 6 ~ signif(value, digits = 3),
          .default = round(value / 1000) * 1000
        )) 
    )
    saveDebugRDS(result, 'r_costs.rds')
    result
  })
  
  epiwide <- reactive({
    model_outputs() %>% 
      select(!c(disease, unit)) %>% 
      group_by(age_group, year, scenario) %>% 
      pivot_wider(names_from = variable, values_from = value) %>% 
      mutate(across(contains("protected_prop"), ~ round(.x,digits = 2))) %>% 
      mutate(across(contains("protected_prop"), as.character)) %>% 
      mutate(across(where(is.double), round))
  })
  
  econsummarywide <- reactive({
    costanalysis<-costs() 
    costanalysis$resultSummary %>% 
      group_by(year, discounting, type, scenario) %>% 
      pivot_wider(names_from = variable, values_from = value) %>% 
      unnest() %>% 
      mutate(across(where(is.double), ~ if_else(
        .x < 10 ^ 6, 
        format(signif(.x, digits = 3), scientific = FALSE),
        round(.x / 1000) * 1000
      )))
  })
  
  econcostwide <- reactive({
    costanalysis<-costs() 
    costanalysis$resultAnnual %>% 
      group_by(year, discounting, type, scenario) %>% 
      mutate(across(where(is.double), round)) %>%
      pivot_wider(names_from = variable, values_from = value) %>% 
      unnest() %>% 
      mutate(across(where(is.double), ~ if_else(
        .x < 10 ^ 6, 
        format(signif(.x, digits = 3), scientific = FALSE),
        round(.x / 1000) * 1000
      )))
  })
  
  ## 4.2 Budget Impact ----
  
  isFinancialBI <- reactive({
    input$eco_bi_cost_type=="Proportion paid by government"
  })
  isFinancialCE <- reactive({
    input$eco_ce_cost_type=="Proportion paid by government"
  })
  isDiscountedBI <- reactive({
    input$eco_bi_discount=="Discounted costs and benefits"
  })
  isDiscountedCE <- reactive({
    input$eco_ce_discount=="Discounted costs and benefits"
  })
  # economic output select/hide packages
  output$eo_select_packages <- renderUI({
    makeBucketList(packagesAsHTMLRankList(),
                   inputIdShowList="eo_rank_list_show",
                   inputIdHideList="eo_rank_list_hide")
  })
  
  # update the table headings with model horizon
  observeEvent(input$model_horizon, {
    
    yearText = glue::glue("{econAnalysisRange()[[1]]} - {econAnalysisRange()[[2]]}")

    shinyjs::html(
      id = "epi_table_text",
      glue::glue("({yearText}).")
    )
    
    update_text <- function(tag_id, yearText) {
      shinyjs::html(
        id = tag_id,
        glue::glue("* Outputs are shown as the total for the full model timeframe ({yearText})")
      )
    }
    
    update_text("epi_out_all", yearText)
    
    update_text("epi_out_diphtheria", yearText)
    
    update_text("epi_out_pertussis", yearText)
    
    update_text("epi_out_tetanus", yearText)
    
    update_text("ce_table_all", yearText)
    
    update_text("ce_table_diphtheria", yearText)
    
    update_text("ce_table_pertussis", yearText)
    
    update_text("ce_table_tetanus", yearText)
    
    shinyjs::html(
      id = "eo_model_tbl_title",
      glue::glue("Projected costs by component (total cost for {yearText})")
    )
  })
  
  # update the net cost comparison options
  observe({
    if (!is.null(input$eo_rank_list_show)) {
      updateSelectInput(
        session = session,
        inputId = "eco_bi_scenario",
        choices = setdiff(input$eo_rank_list_show, "Baseline"),
        selected = first(setdiff(input$eo_rank_list_show, "Baseline"))
      )
    }
  })
  
  # economic output all plots 1
  output$eo_model_plot <- renderPlotly({
    
    req(input$eo_rank_list_show, input$eco_bi_scenario)
    
    fig <- makeCostsPlot(
      costsComparisons = costsComparisons(),
      costs = costs(),
      scenario = input$eco_bi_scenario,
      DIS = "DTP",
      currency = input$selectedCurrency,
      isFinancial = isFinancialBI(), 
      isDiscounted = isDiscountedBI()
    ) %>% 
      ggplotly(tooltip = c("text")) %>% 
        layout(
          legend = list(orientation = 'h')
        ) %>% 
      plotly::config(
        toImageButtonOptions = list(filename = "net-total-costs-compared-to-baseline-plot")
      )
    # source: https://stackoverflow.com/questions/66528066/remove-parenthesis-from-ggplotly-legend
    for (i in 1:length(fig$x$data)){
      if (!is.null(fig$x$data[[i]]$name)){
        fig$x$data[[i]]$name = gsub('^\\(|,\\d+\\)$', '', fig$x$data[[i]]$name)
      }
    }
    
    fig
  })
  
  # economic output all plots 2
  output$eo_model_plot2 <- renderPlotly({
    req(input$eo_rank_list_show)
    fig <- makeCostsPlot2(costs(),
                          scenarios=input$eo_rank_list_show,
                          startYear=2023,
                          currency = input$selectedCurrency,
                          isDiscounted = isDiscountedBI(),
                          isFinancial=isFinancialBI())
    
    fig
  })
  
  # net total cost plot
  output$bi_net_total_cost_plot <- renderPlotly({
    makeCostsPlot4(
      costsComparisons = costsComparisons(),
      DIS = "DTP",
      currency = input$selectedCurrency,
      isFinancial = isFinancialBI(), 
      isDiscounted = isDiscountedBI()
    )
  })
  
  # eo table output for all the diseases
  output$eo_model_tbl <- renderReactable({
    req(input$eo_rank_list_show)
    tbMapping = tibble::tribble(
      ~variableSimple,          ~pretty,
      "costTot",                "Total cost",
      "costVacc_wIntro",        "Cost of vaccination",
      "tot_coi",                "Cost of illness",
      "cost_ib",  "Cost of early childhood booster",
      "cost_icb", "Cost of child booster",
      "cost_iab", "Cost of adolescent booster",
      "cost_im",  "Cost of maternal vaccination"#,
      # "clin_Inc",               "Clinical cases",
      # "deaths",                 "Deaths",
      # "icer_clin", "Cost per clinical case averted",
      # "icer_death",         "Cost per death averted"
    )
    makeCostsTable(costs=costs(),
                   scenarios=input$eo_rank_list_show,
                   currency=input$selectedCurrency,
                   DIS=c("All", "DTP"),
                   tbMapping=tbMapping,
                   isDiscounted = isDiscountedBI(),
                   isFinancial=isFinancialBI())
  })
  
  ## 4.3 Cost Effectiveness render ----
  
  # economic output select/hide packages
  output$eo2_select_packages <- renderUI({
    makeBucketList(packagesAsHTMLRankList(),
                   inputIdShowList="eo2_rank_list_show",
                   inputIdHideList="eo2_rank_list_hide")
  })
  
  # economic output all plots 1
  output$eo2_model_plot <- renderPlotly({
    req(input$eo2_rank_list_show)
    fig <- makeCostsPlot3(costs = costs(),
                          scenarios=input$eo2_rank_list_show,
                          DIS = "DTP",
                          currency = input$selectedCurrency,
                          x=input$eco_ce_output_variable,
                          isDiscounted = isDiscountedCE(),
                          isFinancial=isFinancialCE()) %>%
      ggplotly(tooltip = c("text")) %>% 
      plotly::config(
        toImageButtonOptions = list(filename = "incremental-cost-effectiveness-plot")
      )
    fig
  })
  
  # eo table output for all the diseases
  output$eo2_model_tbl <- renderReactable({
    req(input$eo2_rank_list_show)
    tbMapping = tibble::tribble(
      ~variableSimple,          ~pretty,
      "costTot",                "Total cost",
      "costVacc_wIntro",        "Cost of vaccination",
      "tot_coi",                "Cost of illness",
      # "cost_ib",  "Cost of infant booster",
      # "cost_icb", "Cost of child booster",
      # "cost_iab", "Cost of adolescent booster",
      # "cost_im",  "Cost of maternal",
      "clin_Inc",               "Clinical cases (Total)",
      "deaths",                 "Deaths (Total)",
      "icer_clin", "Cost per clinical case averted**",
      "icer_death",         "Cost per death averted**"
    )
    makeCostsTable(costs=costs(),
                   scenarios=input$eo2_rank_list_show,
                   currency=input$selectedCurrency,
                   DIS=c("All", "DTP"),
                   tbMapping=tbMapping,
                   isDiscounted = isDiscountedCE(),
                   isFinancial=isFinancialCE())
  })
  
  # economic output diph plots 1
  output$eo2_dip_model_plot <- renderPlotly({
    req(input$eo2_rank_list_show)
    
    fig <- makeCostsPlot3(costs = costs(),
                          scenarios=input$eo2_rank_list_show,
                          DIS = "Diphtheria",
                          currency = input$selectedCurrency,
                          x=input$eco_ce_output_variable,
                          isDiscounted = isDiscountedCE(),
                          isFinancial=isFinancialCE()) %>%
      ggplotly(tooltip = c("text"))
    
    fig 
  })
  
  # eo table output for dip the diseases
  output$eo2_dip_model_tbl <- renderReactable({
    req(input$eo2_rank_list_show)
    tbMapping = tibble::tribble(
      ~variableSimple,          ~pretty,
      "costTot",                "Total cost",
      "costVacc_wIntro",        "Cost of vaccination",
      "tot_coi",                "Cost of illness",
      # "cost_ib",  "Cost of infant booster",
      # "cost_icb", "Cost of child booster",
      # "cost_iab", "Cost of adolescent booster",
      # "cost_im",  "Cost of maternal",
      "clin_Inc",               "Clinical cases (Total)",
      "deaths",                 "Deaths (Total)",
      "icer_clin", "Cost per clinical case averted**",
      "icer_death",         "Cost per death averted**"
    )
    makeCostsTable(costs = costs(),
                   scenarios=input$eo2_rank_list_show,
                   DIS=c("DTP","Diphtheria"),
                   currency=input$selectedCurrency,
                   tbMapping=tbMapping,
                   isDiscounted = isDiscountedCE(),
                   isFinancial=isFinancialCE())
  })
  
  # economic output pert plots 1
  output$eo2_pert_model_plot <- renderPlotly({
    req(input$eo2_rank_list_show)
    
    fig <- makeCostsPlot3(costs = costs(),
                          scenarios=input$eo2_rank_list_show,
                          DIS = "Pertussis",
                          currency = input$selectedCurrency,
                          x=input$eco_ce_output_variable,
                          isDiscounted = isDiscountedCE(),
                          isFinancial=isFinancialCE()) %>%
      ggplotly(tooltip = c("text"))
    
    fig
  })
  
  # eo table output for pert the diseases
  output$eo2_pert_model_tbl <- renderReactable({
    req(input$eo2_rank_list_show)
    tbMapping = tibble::tribble(
      ~variableSimple,          ~pretty,
      "costTot",                "Total cost",
      "costVacc_wIntro",        "Cost of vaccination",
      "tot_coi",                "Cost of illness",
      # "cost_ib",  "Cost of infant booster",
      # "cost_icb", "Cost of child booster",
      # "cost_iab", "Cost of adolescent booster",
      # "cost_im",  "Cost of maternal",
      "clin_Inc",               "Clinical cases (Total)",
      "deaths",                 "Deaths (Total)",
      "icer_clin", "Cost per clinical case averted**",
      "icer_death",         "Cost per death averted**"
    )
    
    makeCostsTable(costs = costs(),
                   scenarios=input$eo2_rank_list_show,
                   DIS=c("DTP","Pertussis"),
                   currency=input$selectedCurrency,
                   tbMapping=tbMapping,
                   isDiscounted = isDiscountedCE(),
                   isFinancial=isFinancialCE())
  })
  
  # economic output tet plots 1
  output$eo2_tet_model_plot <- renderPlotly({
    req(input$eo2_rank_list_show)
    fig <- makeCostsPlot3(costs = costs(),
                          scenarios=input$eo2_rank_list_show,
                          DIS = "Tetanus",
                          currency = input$selectedCurrency,
                          x=input$eco_ce_output_variable,
                          isDiscounted = isDiscountedCE(),
                          isFinancial=isFinancialCE()) %>%
      ggplotly(tooltip = c("text"))
    
    fig
  })
  
  # eo table output for tet the diseases
  output$eo2_tet_model_tbl <- renderReactable({
    req(input$eo2_rank_list_show)
    tbMapping = tibble::tribble(
      ~variableSimple,          ~pretty,
      "costTot",                "Total cost",
      "costVacc_wIntro",        "Cost of vaccination",
      "tot_coi",                "Cost of illness",
      # "cost_ib",  "Cost of infant booster",
      # "cost_icb", "Cost of child booster",
      # "cost_iab", "Cost of adolescent booster",
      # "cost_im",  "Cost of maternal",
      "clin_Inc",               "Clinical cases (Total)",
      "deaths",                 "Deaths (Total)",
      "icer_clin", "Cost per clinical case averted**",
      "icer_death",         "Cost per death averted**"
    )
    
    makeCostsTable(costs = costs(),
                   scenarios=input$eo2_rank_list_show,
                   DIS=c("DTP","Tetanus"),
                   currency=input$selectedCurrency,
                   tbMapping=tbMapping,
                   isDiscounted = isDiscountedCE(),
                   isFinancial=isFinancialCE())
  })
  
  session$allowReconnect(TRUE)
  
  ## 4.4 Downloads ----
  
  # download results for all displayed packages
  output$downloadoutput <- downloadHandler(
    
    filename = function(){
      if (shouldZipFiles) {
        paste0("DTPBoost_", Sys.time(), ".zip")
      } else {
        paste0("DTPBoost_", Sys.time(), ".xlsx")
      }
    },
    content = function(file) {
      withProgress(
        message = "Downloading, please wait ...", {
          
          # model horizons
          five_years <- c(2025, 2029)
          ten_years <- c(2025, 2034)
          fifteen_years <- c(2025, 2039)
          
          costsBaseline_5yrs <- create_costsBaseline(econAnalysisRange = five_years)
          costsBaseline_10yrs <- create_costsBaseline(econAnalysisRange = ten_years)
          costsBaseline_15yrs <- create_costsBaseline(econAnalysisRange = fifteen_years)
          
          costsScenarios_5yrs <- create_costsScenarios(costsBaseline_5yrs, five_years)
          costsScenarios_10yrs <- create_costsScenarios(costsBaseline_10yrs, ten_years)
          costsScenarios_15yrs <- create_costsScenarios(costsBaseline_15yrs, fifteen_years)
          
          costsComparisons_5yrs <- create_costsComparisons(costsBaseline_5yrs, costsScenarios_5yrs)
          costsComparisons_10yrs <- create_costsComparisons(costsBaseline_10yrs, costsScenarios_10yrs)
          costsComparisons_15yrs <- create_costsComparisons(costsBaseline_15yrs, costsScenarios_15yrs)
          
          econResultAnnual_5yrs <- create_econResultAnnual(costsBaseline_5yrs, costsScenarios_5yrs)
          econResultAnnual_10yrs <- create_econResultAnnual(costsBaseline_10yrs, costsScenarios_10yrs)
          econResultAnnual_15yrs <- create_econResultAnnual(costsBaseline_15yrs, costsScenarios_15yrs)
          
          econResultSummary_5yrs <- create_econResultSummary(econResultAnnual_5yrs)
          econResultSummary_10yrs <- create_econResultSummary(econResultAnnual_10yrs)
          econResultSummary_15yrs <- create_econResultSummary(econResultAnnual_15yrs)
          
          econResultSummary_combined <- bind_rows(
            econResultSummary_5yrs %>% mutate(year = "2025 - 2029"),
            econResultSummary_10yrs %>% mutate(year = "2025 - 2034"),
            econResultSummary_15yrs %>% mutate(year = "2025 - 2039")
          )
          
          sep <- ","  # also possible sep = "\t" or sep = ";"
          download_sheet <- list(
            baseline_inputs = template() %>% 
              select(Section, Label, Value = Value1) %>% 
              filter(!Section %in% "Strategies") %>% 
              filter(!str_detect(Section, "Explore Results")) %>%
              filter(!str_detect(Label, "Notes")),
            interventions = get_strategies(simul_packages$simul) %>% 
              select(`Strategy name` = strategy_name, Intervention, Label, Value = Value1) %>% 
              mutate(Value = if_else(str_ends(Label, "coverage"), paste0(Value, "%"), Value)) %>%
              mutate(Value = if_else(
                str_detect(Label, regex("cost of", ignore_case = TRUE)), 
                paste0(Value," ",input$selectedCurrency), 
                Value
              )) %>%
              mutate(Value = if_else(str_starts(Label, "Proportion"), paste0(Value, "%"), Value)),
            epi_output = req(model_outputs()) %>% 
              filter(year >= 2025) %>% 
              filter(!str_detect(variable, "treat_")) %>% 
              filter(!str_detect(variable, "prev_prop_")) %>% 
              filter(!variable %in% c("protected_T", "protected100k_T", "protected_P", "protected100k_P")) %>% 
              select(!c(time)) %>% 
              arrange(desc(age_group)), 
            econ_output_summary = econResultSummary_combined,
            econ_output_annual = econResultAnnual_15yrs %>% 
              mutate(value = round(value)) %>% 
              arrange(year)
          )
          if (shouldZipFiles) {
            saveRDS(download_sheet, "dtp_output.rds")
            
            write_xlsx(download_sheet, paste0("dtp_output-", Sys.Date(),".xlsx"))
            
            zip(file, c(paste0("dtp_output-", Sys.Date(),".xlsx"), "dtp_output.rds", "www/README.txt"), extras = "-j")
          } else {
            write_xlsx(download_sheet, file)
          }
        }
      )
      
    })
  
  # download pdf report
  output$report <- downloadHandler(
    filename = paste0("DTPBoost_report_",Sys.Date(), ".pdf"),
    
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      withProgress(
        message = "Downloading, please wait...", {
          tempReport <- file.path(tempdir(), "DTP_Boost_report.Rmd")
          tempImage <- file.path(tempdir(), "logos_band.png")
          file.copy("DTP_Boost_report.Rmd", tempReport, overwrite = TRUE)
          file.copy("www/logos_band.png", tempImage, overwrite = TRUE)
          
          tbOutputs <- readxl::read_excel('models/parameters/parametersDTP.xlsx',sheet='outputs') %>% 
            mutate(variable=Name) %>%
            select(variable, Output, everything())

          # Set up parameters to pass to Rmd document
          params <- list(
            selected_country = getLocMap() %>% filter(ISO3_Code == input$selectedCountry) %>% pull(Location),
            currency = input$selectedCurrency,
            output_variable = input$epi_output_variable,
            plot_y_scale = input$epi_switch_y_axis,
            count_type = input$epi_output_count_method,
            resAgeGrp = input$epi_output_age_group,
            #resSimWindow = input$epi_output_sim_window,
            resSimWindow = econAnalysisRange(),
            scenarios = input$rank_list_show,
            tbOutputs = tbOutputs,
            model_outputs = model_outputs(),
            epiEconResultSummary = epiEconResultSummary(),
            eo_currency = input$selectedCurrency,
            costs = costs(),
            eo_scenarios = input$eo_rank_list_show,
            eo2_model_output = input$eco_ce_output_variable,
            intervention_strategies = get_strategies(simul_packages$simul),
            parameters =  req(template()),
            strategies = req(booster_tbl()),
            coverage_table = req(coverage_data()),
            coverage_type = coverage_type,
            notes_vacc_cov = input$notes_cov_vacc_default,
            notes_vacc_cov_custom = input$notes_cov_vacc_custom,
            notes_health_system = input$notes_health_system,
            notes_costs = input$notes_costs,
            notes_calibration = input$notes_calibration,
            calibration_fit = calibration_fit(),
            isFinancialBI = isFinancialBI(),
            isFinancialCE = isFinancialCE(),
            isDiscountedBI = isDiscountedBI(),
            isDiscountedCE = isDiscountedCE(),
            p_rep_clin_D = input$p_rep_clin_D,
            p_rep_death_D = input$p_rep_death_D,
            p_rep_clin_P = input$p_rep_clin_P,
            p_rep_death_P = input$p_rep_death_P,
            p_rep_clin_T = input$p_rep_clin_T,
            p_rep_death_T = input$p_rep_death_T,
            econAnalysisRange = econAnalysisRange(),
            costsComparisons = costsComparisons()
          )
          
          # Knit the document, passing in the `params` list, and eval it in a
          # child of the global environment (this isolates the code in the document
          # from the code in this app).
          rmarkdown::render(tempReport, output_file = file,
                            params = params,
                            envir = new.env(parent = globalenv())
          )
        }
      )
    }
  )
  
}

LOG("mem_used: {pryr::mem_used()}")
shinyApp(ui=ui, server=server, options=list(host='0.0.0.0',port=8123, launch.browser=F))
