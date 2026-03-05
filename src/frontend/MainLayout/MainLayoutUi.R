# ==============================================================================
# MAIN LAYOUT UI
# ==============================================================================
# Defines the complete application UI structure.
# ==============================================================================

main_layout_ui <- function() {
  bootstrapPage(
    
    # HTML Head - CSS and Fonts ==============================================
    tags$head(
      tags$meta(name = "viewport", content = "width=device-width, initial-scale=1.0, viewport-fit=cover"),
      tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
      tags$script(src = "auth.js"),
      tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
      tags$link(rel = "preconnect", href = "https://fonts.gstatic.com", crossorigin = NA),
      tags$link(
        href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;600;700&display=swap", 
        rel = "stylesheet"
      ),
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
    
    shinyjs::useShinyjs(),
    
    # LOGIN PAGE - Hidden (show main app directly)
    div(id = "login_page", style = "display: none;", login_ui()),
    
    # FORGOT PASSWORD PAGE - Hidden by default
    forgot_ui(),
    
    # RESET PASSWORD PAGE - Hidden by default
    reset_ui(),
    
    # REGISTRATION PAGE - Hidden by default
    registration_ui(),
    
    # SUMMARY PAGE - Hidden by default
    summary_ui(),
    
    # MAIN APPLICATION - Shown by default (skip auth page)
    div(id = "app_page", style = "display: block;",
        div(class = "main-container",
            
            # Left Sidebar Navigation ==========================================
            create_sidebar(),
            
            # Top Header Bar ===================================================
            uiOutput("header_ui"),
            
            # Main Content Area ================================================
            div(class = "main-content", 
                uiOutput("main_content")
                ),
            
            # JavaScript for Interactive Menu Highlighting ====================
            tags$script(HTML("
          $(document).ready(function() {
            $('.menu-item').click(function() {
              $('.menu-item').removeClass('active');
              $(this).addClass('active');
            });
          });
        "))
        )
    ),
    
    # Overlay element (hidden by default)
    div(
      id = "session_check_overlay",
      style = "display:none; position:fixed; top:0; left:0; width:100%; height:100%;
               background-color:rgba(0,0,0,0.5); z-index:9999; text-align:center; color:white;
               padding-top:200px;",
      h3("Checking session...")
    )
  )
}
