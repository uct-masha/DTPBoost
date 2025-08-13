mockDataTableUI <- function(tableOutputId = "populationDemog", 
                            verifyInputId = "verifyDemog",
                            downloadTemplateInputId = "templateDemog", 
                            uploadTemplateInputId = "demogInput",
                            templateLabel = "demographics.csv",
                            colSizes=c(9,1,2),
                            includeButtons=TRUE,
                            includeViewAs=FALSE) {
  fluidRow(
    column(12,
           DT::DTOutput(outputId=tableOutputId)
    ),
    column(colSizes[[1]],HTML("&nbsp;")),
    column(colSizes[[2]],
           if(includeButtons==TRUE) {
             actionBttn(inputId = verifyInputId, label = "Verify", 
                        color="success",size='sm', style="pill")
           } else {
               HTML("&nbsp;")
           }
    ),
    column(colSizes[[3]],
           if(includeButtons==TRUE) {
                  dropdownButton(label="Replace",circle = FALSE, status = "success", size = "sm", 
                          icon = icon("glyphicon-edit"), tooltip = FALSE, right = TRUE,
                          fluidRow(
                            column(12,
                                   p("To replace the default data, please download the template and upload the changes."),
                                   shiny::actionButton(inputId = downloadTemplateInputId, 'Download Template'),
                                   p("Then Upload the changes"),
                                   shiny::fileInput(inputId = uploadTemplateInputId, label = templateLabel, accept='.csv')
                            )
                          )
                  )
           }
           else{
                  HTML("&nbsp;")
           }
    )
  )
}