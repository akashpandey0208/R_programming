# ==============================================================================
# AUTH UI MODULE
# ==============================================================================
# Login and Reset Password user interface definitions.
# Styled to match Clinical Webapp conventions (Inter font, #0d1f36 theme).
# ==============================================================================

# Login Page UI ==============================================================
#' @description Full-screen login page with brand panel and login form
#' @return HTML div containing the complete login page
login_ui <- function() {
  div(id = "login_page", class = "login-wrapper",
    
    # Left Brand Panel (65%) ------------------------------------------------
    div(class = "login-bg",
      tags$img(
        src = "images/actalent_login_logo.png",
        alt = "Actalent Logo",
        class = "login-logo"
      )
    ),
    
    # Right Login Form Panel (35%) ------------------------------------------
    div(class = "login-right",
      div(class = "login-card",
        
        # Card Header
        h2("Login", style = "color: #0d1f36; font-family: 'Inter', sans-serif; margin-bottom: 16px; font-size: 28px; text-align: center; font-weight: bold;"),
        
        # Email Field
        div(class = "form-group",
          tags$label("Email", `for` = "email", 
            style = "display: block; font-size: 13px; font-weight: 600; color: #0d1f36; margin-bottom: 6px; font-family: 'Inter', sans-serif;"
          ),
          tags$input(
            type = "text", id = "email", class = "form-control auth-input",
            placeholder = "Enter your email"
          ),
          div(id = "email_feedback", class = "invalid-feedback", style = "display: none;")
        ),
        
        # Password Field with Eye Toggle
        div(class = "form-group", style = "margin-top: 18px;",
          tags$label("Password", `for` = "password", 
            style = "display: block; font-size: 13px; font-weight: 600; color: #0d1f36; margin-bottom: 6px; font-family: 'Inter', sans-serif;"
          ),
          div(class = "password-wrapper",
            tags$input(
              type = "password", id = "password", class = "form-control auth-input",
              placeholder = "Enter your password"
            ),
            tags$span(class = "toggle-password", `data-target` = "password",
              HTML("&#128065;")
            )
          ),
          div(id = "password_feedback", class = "invalid-feedback", style = "display: none;")
        ),
        
        # Forgot Password Link
        div(style = "text-align: right; margin-top: 10px;",
          tags$a(
            id = "forgot_pwd_link", href = "#", class = "forgot-link",
            "Forgot password?",
            onclick = "Shiny.setInputValue('goto_reset', Math.random(), {priority: 'event'}); return false;"
          )
        ),
        
        # Login Button
        div(style = "margin-top: 24px;",
          tags$button(
            id = "login_btn", class = "login-btn",
            disabled = "disabled",
            style = "opacity: 0.5; cursor: not-allowed;",
            "Login",
            onclick = "Shiny.setInputValue('login_btn', Math.random(), {priority: 'event'})"
          )
        ),
        
        # Login Status Message
        uiOutput("login_status"),
        
        # Request Access Link
        div(style = "text-align: center; margin-top: 20px;",
          tags$span(style = "color: #64748b; font-size: 13px;", "Don't have an account? "),
          tags$a(
            id = "goto_register", href = "#",
            style = "color: #0d1f36; font-weight: 600; font-size: 13px; text-decoration: none;",
            "Request Access",
            onclick = "Shiny.setInputValue('goto_register', Math.random(), {priority: 'event'}); return false;"
          )
        )
      )
    )
  )
}


