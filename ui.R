<<<<<<< HEAD
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
=======
# ==============================================================================
# CLINICAL WEBAPP - USER INTERFACE
# ==============================================================================
# This file defines the complete user interface structure including the
# sidebar, header, and main content area.
# ==============================================================================

# Source Global Configuration ================================================
source("global.R")

# Source UI Components =======================================================
source("modules/ui_components.R")

# Main UI Definition =========================================================
ui <- tagList(
  
  # HTML Head - CSS and Fonts ================================================
  tags$head(
    # Custom CSS Stylesheet
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    
    # Google Fonts - Inter
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "preconnect", href = "https://fonts.gstatic.com", crossorigin = NA),
    tags$link(
      href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;600;700&display=swap", 
      rel = "stylesheet"
    ),
    
    # Prevent body overflow
    tags$style(HTML("
      html, body { 
        margin: 0; 
        padding: 0; 
        height: 100%; 
        width: 100%; 
        overflow: hidden; 
      }
    "))
  ),
  
  # Main Application Container ===============================================
  div(class = "main-container",
    
    # Left Sidebar Navigation ================================================
    create_sidebar(),
    
    # Top Header Bar =========================================================
    create_header(),
    
    # Main Content Area ======================================================
    div(class = "main-content",
      uiOutput("main_content")
    ),
    
    # JavaScript for Interactive Menu Highlighting ==========================
    tags$script(HTML("
      $(document).ready(function() {
        // Remove active class from all menu items and add to clicked item
        $('.menu-item').click(function() {
          $('.menu-item').removeClass('active');
          $(this).addClass('active');
        });
      });
    "))
  )
)
>>>>>>> b7020ab (Inital check-in of main page clinical)
