# ==============================================================================
# RESET PASSWORD CONTROLLER
# ==============================================================================
# Server-side logic for password reset flow. Calls /reset_password API.
# ==============================================================================

reset_controller <- function(input, output, session) {
  
  # --- Back to login ---
  observeEvent(input$reset_back_to_login, {
    
    updateTextInput(session, "email", value = "")
    updateTextInput(session, "password", value = "")
    shinyjs::hide("reset_page")
    shinyjs::show("login_page")
  })
  
  # --- Handle Save & Login ---
  observeEvent(input$save_login, {
    
    if (input$new_pwd_value != input$confirm_pwd_value) {
      output$reset_status <- renderUI({
        toast_message("error", "New password and confirm password do not match.")
      })
      return()
    }
    
    req(input$new_pwd_value, input$confirm_pwd_value)
    
    body <- list(
      email            = session$userData$reset_email,   # carried from login controller
      temp_password    = input$current_pwd,
      new_password     = input$new_pwd_value
    )
    
    res <- httr::POST(
      paste0(Sys.getenv("API_URL"), "/reset_password"),
      body = jsonlite::toJSON(body, auto_unbox = TRUE),
      encode = "json"
    )
    
    if (httr::status_code(res) == 200) {
      output$reset_status <- renderUI({
        toast_message("success", "Password reset successful")
      })
      
      shinyjs::delay(1500, {
        shinyjs::hide("reset_page")
        shinyjs::show("login_page")
        
        updateTextInput(session, "email", value = "")
        updateTextInput(session, "password", value = "")
        # Clear any login status/error messages
        output$login_status <- renderUI({ NULL })
        output$reset_status <- renderUI({ NULL })
      })
      
    } else {
      msg <- httr::content(res)$message %||% "Unknown error"
      output$reset_status <- renderUI({
        toast_message("error", paste("Reset failed:", msg))
      })
    }
  })
}
