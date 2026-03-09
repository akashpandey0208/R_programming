data_upload_server <- function(id){
  
  moduleServer(id, function(input, output, session){
    
    message("upload server started")
    
    datasets <- reactive({
      
      req(input$files)
      
      paths <- input$files$datapath
      names <- tools::file_path_sans_ext(input$files$name)
      
      data_list <- list()
      
      for(i in seq_along(paths)){
        
        domain <- toupper(names[i])
        
        data_list[[domain]] <- read_file(paths[i])
        
      }
      
      data_list
      
    })
    
    return(datasets)
    
  })
}