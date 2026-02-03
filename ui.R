# ============================================================================
# UI Definition
# ============================================================================

ui <- fluidPage(
  shinyjs::useShinyjs(),
  tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$link(rel = "stylesheet", type = "text/css", href = "css/style_main.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "css/style_sidebar.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "css/style_topbar.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "css/style_buttons.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "css/style_graphs.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "css/style_data_explorer.css"),
    tags$link(
      rel = "stylesheet",
      href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css"
    )
  ),
  
  div(
    class = "app-container",
    
    # Sidebar
    uiOutput("sidebar_ui"),
    
    # Main content area
    div(
      class = "main",
      
      # Topbar
      div(
        class = "topbar",
        div(class = "top-left", "Clinical Webapp"),
        div(
          class = "top-right",
          div(
            class = "top-search",
            textInput("search", NULL, placeholder = "Search", width = NULL)
          ),
          div(
            class = "top-lang",
            selectInput("lang", NULL, choices = c("English"), selected = "English")
          ),
          actionButton("notif", label = NULL, icon = icon("bell"), class = "icon-btn"),
          actionButton("refresh", label = NULL, icon = icon("rotate-right"), class = "icon-btn"),
          div(class = "avatar", "P")
        )
      ),
      
      # Content area (dynamic based on selected tab)
      div(
        class = "content",
        uiOutput("page_ui")
      )
    )
  )
)
