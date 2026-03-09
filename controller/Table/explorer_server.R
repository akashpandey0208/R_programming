explorer_server <- function(id, datasets){
  
  moduleServer(id, function(input, output, session){
    
    output$explorer_list <- renderUI({
      
      req(datasets())
      
      radioButtons(
        "selected_dataset",
        label = NULL,
        choices = datasets()
      )
      
    })
    
  })
}