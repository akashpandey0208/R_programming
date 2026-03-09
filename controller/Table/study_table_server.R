study_table_server <- function(id){
  
  moduleServer(id, function(input, output, session){
    
    message("Study table server started")
    
    datasets <- reactiveValues(
      files = NULL,
      data = list(),
      selected = NULL
    )
    
    
    uploaded_data <- data_upload_server("upload")
    
    
    observeEvent(uploaded_data(), {

      data <- uploaded_data()

      datasets$data  <- data
      datasets$files <- names(data)

      print("Uploaded datasets:")
      print(datasets$files)

    })
    
    
    output$explorer_files <- renderUI({
      
      req(datasets$files)
      
      radioButtons(
        inputId = session$ns("explorer_dataset"),
        label = NULL,
        choices = datasets$files
      )
      
    })
    
    
    observeEvent(input$explorer_dataset,{
  
      print(paste("Selected dataset:", input$explorer_dataset))
      
      datasets$selected <- datasets$data[[ input$explorer_dataset ]]
      
      print(dim(datasets$selected))
      
    })
    
    data_preview_server(
      "preview",
      selected_dataset = reactive(datasets$selected)
    )

    data_table_server(
      "table",
      selected_dataset = reactive(datasets$selected)
    )

    explorer_visible <- reactiveVal(TRUE)

    observeEvent(input$toggle_explorer, {

      explorer_visible(!explorer_visible())

    })

    observe({

      if(explorer_visible()){
        
        shinyjs::show(session$ns("explorer_panel"))
        
      } else {
        
        shinyjs::hide(session$ns("explorer_panel"))
        
      }

    })
    
  })
}