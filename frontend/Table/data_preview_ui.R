data_preview_ui <- function(id){
  
  ns <- NS(id)
  
  fluidRow(
    
    column(
      width = 12,
      
      div(
        class = "dm-panel-box dm-table-box",
        div(class = "dm-panel-title","Data Preview"),
        DT::DTOutput(ns("preview_table"))
      )
      
    )
    
  )
}