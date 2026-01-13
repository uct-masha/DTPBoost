source(here::here("www/R/mod_explore_disease_metric.R")) # Step 2.1 Explore Data - module for each box (disease&metric tile)

ISO_country <- "UGA"  # TODO: Change this

exploreDataUI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    column(
      width = 12,
      h3("Explore country data"),
      p("This page shows the known disease burden data for Diphtheria, Tetanus, and Pertussis in the selected country. You can explore both reported cases and estimated disease incidence from different data sources. Additionally, you can upload your own custom data for comparison."),
      tabsetPanel(
        id = "explore_data",
        tabPanel(
          "Diphtheria",
          br(),
          diseaseMetricUI(
            id = ns("diphtheria_cases"),
            disease = "Diphtheria",
            metric = "Cases"
          ),
          br(),
          diseaseMetricUI(
            id = ns("diphtheria_incidence"),
            disease = "Diphtheria",
            metric = "Incidence"
          )
        ),
        tabPanel(
          title = "Tetanus",
          br(),
          diseaseMetricUI(
            id = ns("tetanus_cases"),
            disease = "Tetanus",
            metric = "Cases"
          ),
          br(),
          diseaseMetricUI(
            id = ns("tetanus_incidence"),
            disease = "Tetanus",
            metric = "Incidence"
          )
        ),
        tabPanel(
          title = "Pertussis",
          br(),
          diseaseMetricUI(
            id = ns("pertussis_cases"),
            disease = "Pertussis",
            metric = "Cases"
          ),
          br(),
          diseaseMetricUI(
            id = ns("pertussis_incidence"),
            disease = "Pertussis",
            metric = "Incidence"
          )
        )
      ) # /tabsetPanel
    ) # /column
  ) # /fluidRow
}

getData <- function(Disease, Metric, ISO=ISO_country) {
  result <- list()
  if (is.null(ISO)) stop("ISO code is required to get data.")
  print(glue::glue("Getting data for {Disease} {Metric} with ISO {ISO}"))
  if (Metric=="Cases") result[["WHO"]] <- get_clinical_burden("WHO", ISO)|>select(Year, Value={{Disease}})
  if (Metric=="Incidence") result[["GBD"]] <- get_clinical_burden("GBD", ISO)|>select(Year, Value={{Disease}})
  return(result)
}

getDescription <- function(Disease, Metric) {
  desc <- glue::glue("There is no description available for {Disease} {Metric}.")
  if (Metric=="Cases") desc <- glue::glue("The WHO clinical burden data for {Disease} represents the reported number of cases per year as collected by the WHO.\n\nFor more information find \"The Model\" section in the \"Welcome\" tab to the left of the app.")
  if (Metric=="Incidence") desc <- glue::glue("The GBD clinical burden data for {Disease} represents the estimated incidence as given by the Global Burden of Disease study.\n\nFor more information find \"The Model\" section in the \"Welcome\" tab to the left of the app.")
  return(desc)
}

newTile <- function(tile_id) {
  parts <- strsplit(tile_id, "_")[[1]]
  Disease <- parts[1] |> stringr::str_to_title()
  Metric <- parts[2] |> stringr::str_to_title()
  nsid <- paste(stringr::str_to_lower(Disease), stringr::str_to_lower(Metric), sep = "_")
  print(nsid)
  diseaseMetricServer(nsid, disease=Disease, metric=Metric,
                      info = getDescription(Disease, Metric),
                      data_list = getData(Disease, Metric))
}

exploreDataServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      newTile("diphtheria_cases")
      newTile("diphtheria_incidence")
      newTile("tetanus_cases")
      newTile("tetanus_incidence")
      newTile("pertussis_cases")
      newTile("pertussis_incidence")
    }
  )
}
