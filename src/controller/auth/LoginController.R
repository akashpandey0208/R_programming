library(httr)
library(jsonlite)

login_controller <- function(input, output, session) {
  
  # ===============================
  # Store tokens in session
  # ===============================
  session$userData$access_token  <- reactiveVal(NULL)

  session$userData$refresh_token <- reactiveVal(NULL)
  session$userData$full_name     <- reactiveVal(NULL)

  # ===============================
  # RESTORE LOGIN ON RELOAD
  # ===============================
  observeEvent(input$stored_access_token, {
    
    access  <- input$stored_access_token
    refresh <- input$stored_refresh_token
    
    cat("DEBUG RESTORE: access =", substr(access, 1, 20), "\n")
    cat("DEBUG RESTORE: refresh =", substr(refresh, 1, 20), "\n")
    
    show_overlay()

    # Try existing access token
    if (!is.null(access) && nzchar(access)) {
      res <- GET(
        paste0(Sys.getenv("API_URL"), "/secure_data"),
        add_headers(Authorization = paste("Bearer", access))
      )
      
      if (status_code(res) == 200) {
        session$userData$access_token(access)
        session$userData$refresh_token(refresh)

      # Fetch full name from API using restore token
      name_res <- GET(
        paste0(Sys.getenv("API_URL"), "/user_fullname"),
        add_headers(Authorization = paste("Bearer", access))
      )
      if (status_code(name_res) == 200) {
        name_data <- content(name_res)
        session$userData$full_name(name_data$full_name)
            }

        hide_overlay()
        switch_page("app_page")
        return()
      }
    }
    
    # Try refresh token if access token failed/expired
    if (!is.null(refresh) && nzchar(refresh)) {
      refresh_res <- POST(
        paste0(Sys.getenv("API_URL"), "/refresh"),
        body = list(refresh_token = refresh),
        encode = "json"
      )
      
      if (status_code(refresh_res) == 200) {
        new_tokens <- content(refresh_res)
        
        session$userData$access_token(new_tokens$access_token)
        session$userData$refresh_token(new_tokens$refresh_token)
        
        # Save rotated tokens in cookies
        session$sendCustomMessage("setCookie", list(
          name = "access_token",
          value = new_tokens$access_token,
          max_age = 3600
        ))
        session$sendCustomMessage("setCookie", list(
          name = "refresh_token",
          value = new_tokens$refresh_token,
          max_age = 3*24*3600
        ))

        hide_overlay()
        switch_page("app_page")
        return()
      }
    }
    
    # If neither works, go back to login
    hide_overlay()
    switch_page("login_page")
    
  }, ignoreNULL = TRUE)
  
  # ===============================
  # LOGIN HANDLER
  # ===============================
  observeEvent(input$login_btn, {
    
    req(input$email_value, input$password_value)
    
    cat("====================================\n")
cat("LOGIN ATTEMPT\n")
cat("API_URL:", Sys.getenv("API_URL"), "\n")
cat("Email:", input$email_value, "\n")

    res <- POST(
      paste0(Sys.getenv("API_URL"), "/login"),
      body = list(email = input$email_value,
                  password = input$password_value),
      encode = "json"
    )
    
    cat("Status Code:", httr::status_code(res), "\n")
cat("Raw Response:\n")
print(httr::content(res, as = "text"))
cat("====================================\n")
    if (status_code(res) == 200) {
      tokens <- content(res, as = "parsed")
print(tokens)
str(tokens)
access_token  <- tokens$access_token
  refresh_token <- tokens$refresh_token

# Store in session
session$userData$access_token(access_token)
session$userData$refresh_token(refresh_token)

# Fetch full name
name_res <- GET(
  paste0(Sys.getenv("API_URL"), "/user_fullname"),
  add_headers(Authorization = paste("Bearer", access_token))
)

# Save token in cookie
session$sendCustomMessage("setCookie", list(
  name = "access_token",
  value = access_token,
  max_age = 3600
))
      session$sendCustomMessage("setCookie", list(
        name = "refresh_token",
        value = tokens$refresh_token,
        max_age = 3*24*3600
      ))
      
      output$login_status <- renderUI({
        toast_message("success", "Login Successful")
      })
      
      shinyjs::delay(300, {
        shinyjs::hide("login_page")
        shinyjs::show("app_page")
      })
      
    } else {
      resp <- content(res)
      
      if (!is.null(resp$status) && resp$status == "reset_required") {
        session$userData$reset_email <- input$email_value
        output$login_status <- renderUI({
          toast_message("warning", "Password reset required")
        })
        
        shinyjs::hide("login_page")
        shinyjs::show("reset_page")   # redirect to reset password UI
        updateTextInput(session, "reset_email", value = input$email_value)
      } else {
        output$login_status <- renderUI({
          toast_message("error", "Login Failed")
        })
      }
    }
  })
  
  # ===============================
  # FORGOT PASSWORD LINK
  # ===============================
  observeEvent(input$goto_reset, {
    shinyjs::hide("login_page")
    shinyjs::show("forgot_page")
  })
  
  # ===============================
  # LOGOUT HANDLER
  # ===============================
  observeEvent(input$logout_btn, {
    
    POST(
      paste0(Sys.getenv("API_URL"), "/logout"),
      body = list(email = input$email_value),
      encode = "json"
    )
    
    session$userData$access_token(NULL)
    session$userData$refresh_token(NULL)
    
    # Clear cookies
    session$sendCustomMessage("setCookie", list(
      name = "access_token",
      value = "",
      max_age = 0
    ))
    session$sendCustomMessage("setCookie", list(
      name = "refresh_token",
      value = "",
      max_age = 0
    ))
    
    switch_page("login_page")
    updateTextInput(session, "email", value = "")
    updateTextInput(session, "password", value = "")
  })
}
