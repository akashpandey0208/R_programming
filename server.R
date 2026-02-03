# ============================================================================
# Server Logic
# ============================================================================

server <- function(input, output, session) {
  
  # ============================================================================
  # Reactive Values and State Management
  # ============================================================================
  
  # Track current active tab
  current_tab <- reactiveVal("data")
  
  # Store the main dataset
  dataset <- reactiveVal()
  
  # ============================================================================
  # Tab Navigation Observers
  # ============================================================================
  
  observeEvent(input$menu_data, current_tab("data"))
  observeEvent(input$menu_table, current_tab("table"))
  observeEvent(input$menu_graphs, current_tab("graphs"))
  observeEvent(input$menu_reports, current_tab("reports"))
  observeEvent(input$menu_help, current_tab("help"))
  
  # ============================================================================
  # Sidebar Rendering
  # ============================================================================
  
  output$sidebar_ui <- renderUI({
    tab <- current_tab()
    
    menu_item <- function(id, icon_class, label, key) {
      cls <- paste("menu-item", if (tab == key) "active" else "")
      actionLink(id, label = label, class = cls)
    }
    
    div(
      class = "sidebar",
      div(
        class = "brand",
        img(src = "actalent_1.png", alt = "logo", style = "width:120px; height:auto;")
      ),
      div(
        class = "menu",
        menu_item("menu_data", "fa-solid fa-database", "Data", "data"),
        menu_item("menu_table", "fa-solid fa-table", "Table", "table"),
        menu_item("menu_graphs", "fa-solid fa-chart-line", "Graphs", "graphs"),
        menu_item("menu_reports", "fa-solid fa-file-lines", "Reports", "reports"),
        menu_item("menu_help", "fa-solid fa-circle-question", "Help", "help")
      )
    )
  })
  
  # ============================================================================
  # Data Management
  # ============================================================================
  
  # Load sample dataset by default if available
  sample_path <- file.path("www", "sample.csv")
  if (file.exists(sample_path)) {
    try({
      df0 <- read.csv(sample_path, stringsAsFactors = FALSE)
      df0 <- sanitize_df(df0)
      dataset(df0)
    }, silent = TRUE)
  }
  
  # Column information reactive
  col_info <- reactive({
    req(dataset())
    get_col_info(dataset())
  })
  
  # Filtered dataset based on filters
  filtered_df <- reactive({
    req(dataset())
    df <- dataset()
    if (isTruthy(input$filter_region) && input$filter_region != "All") {
      df <- df %>% filter(Region == input$filter_region)
    }
    if (isTruthy(input$filter_income) && input$filter_income != "All") {
      df <- df %>% filter(IncomeGroup == input$filter_income)
    }
    df
  })
  
  # ============================================================================
  # Page Routing
  # ============================================================================
  
  output$page_ui <- renderUI({
    tab <- current_tab()
    if (tab == "data") return(dataModuleUI("data_module", dataset))
    if (tab == "table") return(tableModuleUI("table_module", dataset, filtered_df))
    if (tab == "graphs") return(graphsModuleUI("graphs_module", dataset, filtered_df, col_info))
    if (tab == "reports") return(reportsModuleUI("reports_module"))
    if (tab == "help") return(helpModuleUI("help_module"))
    div()
  })
  
  # ============================================================================
  # Module Servers
  # ============================================================================
  
  # Data module
  dataModuleServer("data_module", dataset)
  
  # Table module
  tableModuleServer("table_module", dataset, filtered_df, col_info)
  
  # Graphs module
  graphsModuleServer("graphs_module", dataset, filtered_df, col_info)
  
  # Reports module (placeholder)
  reportsModuleServer("reports_module")
  
  # Help module (placeholder)
  helpModuleServer("help_module")
}
