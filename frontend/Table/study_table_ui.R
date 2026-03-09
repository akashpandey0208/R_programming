# ==============================================================================
# STUDY TABLE UI MODULE
# ==============================================================================
# UI definition for the Study data table.
# Includes Study data uploads,
# export, and a DT table rendered by server module.
# ==============================================================================

study_table_ui <- function(id){
  
  ns <- NS(id)
  
  tagList(
    
    div(class = "admin-table-wrapper",
        
        fluidRow(
          
          column(
            width = 2,
            
            
          div(
              id = ns("explorer_panel"),
              class = "dm-panel-box dm-explorer-box",
              
              div(class = "dm-panel-title","EXPLORER"),
            #   actionButton(
            #   ns("toggle_explorer"),
            #   label = "☰",
            #   class = "explorer-toggle"
            # ),
              uiOutput(ns("explorer_files"))
            )
          ),

          column(
            width = 10,
            
            tabsetPanel(
              
              tabPanel(
                "Upload",
                data_upload_ui(ns("upload"))
              ),
              
              tabPanel(
                "Data",
                data_preview_ui(ns("preview"))
              ),
              
              tabPanel(
                "Table",
                data_table_ui(ns("table"))
              ),
              
              tabPanel(
                "Programs",
                "Coming Soon"
              ),
              
              tabPanel(
                "Outputs",
                "Coming Soon"
              )
              
            )
            
          )
          
        )
        
    )
  )
}