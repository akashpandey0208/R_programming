# ==============================================================================
# CLINICAL WEBAPP - SERVER LOGIC
# ==============================================================================
# This file contains the server-side logic including event handlers and
# reactive content rendering.
# ==============================================================================

# Source Global Configuration ================================================
source("global.R")

# Source Content Modules =====================================================
source("modules/content_modules.R")

# Server Function ============================================================
server <- function(input, output, session) {
  
  # Reactive Values ==========================================================
  # Track the currently selected menu item
  selected_menu <- reactiveVal(DEFAULT_MODULE)
  
  # Event Observers ==========================================================
  
  # Menu Selection Handler ---------------------------------------------------
  observeEvent(input$selected_menu, {
    selected_menu(input$selected_menu)
  })
  
  # Notification Bell Click Handler ------------------------------------------
  observeEvent(input$notification_clicked, {
    showNotification(
      "Notifications clicked!", 
      type = "message", 
      duration = 3,
      closeButton = TRUE
    )
  })
  
  # User Profile Click Handler -----------------------------------------------
  observeEvent(input$user_clicked, {
    showNotification(
      "User profile clicked!", 
      type = "message", 
      duration = 3,
      closeButton = TRUE
    )
  })
  
  # Render Main Content ======================================================
  output$main_content <- renderUI({
    menu <- selected_menu()
    
    # Route to appropriate content module based on selected menu
    content <- switch(
      menu,
      "clinical" = render_clinical_module(),
      "data_management" = render_data_management_module(),
      "blinded_biometrics" = render_blinded_biometrics_module(),
      "medical_monitor" = render_medical_monitor_module(),
      "rwe" = render_rwe_module(),
      "dmc" = render_dmc_module(),
      "client" = render_client_module(),
      "admin" = render_admin_module(),
      render_welcome_module()  # Default fallback
    )
    
    content
  })
}
