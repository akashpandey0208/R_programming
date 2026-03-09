data_upload_ui <- function(id){
  
  ns <- NS(id)
  
  fluidRow(
    
    column(
      width = 12,
      
      div(
        class = "dm-panel-box",
        div(class = "dm-panel-title", "DATA UPLOAD"),
        
        div(
          class = "dm-center-upload",
          
          fileInput(
            inputId = ns("files"),
            label = NULL,
            multiple = TRUE,
            accept = c(".xpt",".csv",".sas7bdat",".xlsx")
          )
        )
      )
    )
  )
}