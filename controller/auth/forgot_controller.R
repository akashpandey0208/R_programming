# ==============================================================================
# FORGOT PASSWORD CONTROLLER
# ==============================================================================
forgot_controller <- function(input, output, session) {
  
  # Go back to login
  observeEvent(input$back_to_login, {
    
    # Clear inputs when returning
    updateTextInput(session, "forgot_email", value = "")
    updateTextInput(session, "email", value = "")
    updateTextInput(session, "password", value = "")
    # Clear any login status/error messages
    output$login_status <- renderUI({ NULL })
    output$reset_status <- renderUI({ NULL })
  })
  
  # Handle reset password request
  observeEvent(input$reset_password, {
    req(input$forgot_email)
    
    res <- POST(
      "http://localhost:8000/forgot_password",
      body = list(email = input$forgot_email),
      encode = "json"
    )
    
    if (status_code(res) == 200) {
      output$forgot_status <- renderUI({
        toast_message("success", "Email Sent Successfully")
      })
      
      shinyjs::delay(1000, {
        # Clear inputs before redirect
        updateTextInput(session, "forgot_email", value = "")
        updateTextInput(session, "email", value = "")
        updateTextInput(session, "password", value = "")
        # Clear any login status/error messages
        output$login_status <- renderUI({ NULL })
        output$reset_status <- renderUI({ NULL })
        
        shinyjs::hide("forgot_page")
        shinyjs::show("login_page")
      })
    } else {
      resp <- content(res)
      output$forgot_status <- renderUI({
        toast_message("error", resp$message)
      })
    }
  })
}
