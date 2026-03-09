data_preview_server <- function(id, selected_dataset){
  
  moduleServer(id, function(input, output, session){
    
    message("Data preview server started")
    
    # Table preview
    output$preview_table <- DT::renderDT({
  
      req(selected_dataset())
      
      DT::datatable(
        selected_dataset(),
        options = list(
          pageLength = 10,
          scrollX = TRUE
        )
      )
      
    })
    
    # Debug print
    observe({
      
      req(selected_dataset())
      
      print("Preview module received dataset:")
      print(dim(selected_dataset()))
      
    })
    
  })
}