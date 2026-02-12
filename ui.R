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
