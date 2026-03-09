# ==============================================================================
# MAIN LAYOUT CONTROLLER
# ==============================================================================
# Handles navigation and content rendering for the main application.
# ==============================================================================

#' @description Main navigation and content controller
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
main_layout_controller <- function(input, output, session) {
  
  # Reactive Values ==========================================================
  selected_menu <- reactiveVal(DEFAULT_MODULE)
  dm_sub_menu   <- reactiveVal("data_upload")   # default sub-page for DM
  
  # Event Observers ==========================================================
  
  # Menu Selection Handler
  observeEvent(input$selected_menu, {
    selected_menu(input$selected_menu)
  })

  # DM Sub-menu Selection Handler
  observeEvent(input$dm_sub_menu, {
    dm_sub_menu(input$dm_sub_menu)
  })
  
  # Notification Bell Click Handler
  observeEvent(input$notification_clicked, {
    showNotification(
      "Notifications clicked!", 
      type = "message", 
      duration = 3,
      closeButton = TRUE
    )
  })
  
  # User Profile Click Handler
  observeEvent(input$user_clicked, {
    showNotification(
      "User profile clicked!", 
      type = "message", 
      duration = 3,
      closeButton = TRUE
    )
  })
  
  # Display profile icon (user-initials)
  output$header_ui <- renderUI({
    req(session$userData$full_name())   
    full_name <- session$userData$full_name() 
    
    if (is.null(full_name) || full_name == "") {
      initials <- "U"
    } else {
      initials <- get_initials(full_name)
    }
    create_header(initials = initials)
  })
  
  # Render Main Content ======================================================
  output$main_content <- renderUI({
    menu <- selected_menu()
    
    content <- switch(
      menu,
      "clinical"           = render_clinical_module(),
      "data_management"    = render_data_management_module(dm_sub_menu()),
      "blinded_biometrics" = render_blinded_biometrics_module(),
      "medical_monitor"    = render_medical_monitor_module(),
      "rwe"                = render_rwe_module(),
      "dmc"                = render_dmc_module(),
      "client"             = render_client_module(),
      "admin"              = render_admin_module(),
      render_welcome_module()
    )
    
    content
  })
}
