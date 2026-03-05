# ==============================================================================
# RESET PASSWORD UI MODULE
# ==============================================================================
# Reset Password user interface definition.
# ==============================================================================

# Reset Password Page UI ====================================================
#' @description Centered reset password form
#' @return HTML div containing the reset password page
reset_ui <- function() {
  div(id = "reset_page", style = "display: none;", class = "login-wrapper",
    
    # LEFT PANEL (same as login) --------------------------------------
    div(class = "login-bg",
      tags$img(
        src = "images/actalent_login_logo.png",
        alt = "Actalent Logo",
        class = "login-logo"
      )
    ),
    
    # RIGHT PANEL ------------------------------------------------
    div(class = "login-right",
      div(class = "login-card reset-card",
        
        # Back to Login
        div(
          class = "login-links reset-back",
          actionLink(
            "reset_back_to_login",
            tagList(
              tags$i(class = "fa-solid fa-arrow-left"),
              " Back to login"
            )
          )
        ),

        # Card Header
        h2("Reset Password", style = "color: #0d1f36; font-family: 'Inter', sans-serif; margin-bottom: 6px; font-size: 28px;"),
        p("Please create a new secure password", style = "color: #64748b; font-size: 14px; margin-bottom: 30px;"),
        
        # Current Password
        password_field(
          input_id = "current_pwd",
          label_text = "Current Password",
          autocomplete = "current-password",
          show_toggle = FALSE
        ),
        div(id = "current_pwd_feedback", class = "invalid-feedback", style = "display:none;"),

        # New Password
        password_field(
          input_id = "new_pwd",
          label_text = "New Password",
          autocomplete = "new-password",
          show_toggle = FALSE
        ),
        div(id = "new_pwd_feedback", class = "invalid-feedback", style = "display:none;"),

        # Confirm Password
        password_field(
          input_id = "confirm_pwd",
          label_text = "Confirm Password",
          autocomplete = "new-password",
          show_toggle = FALSE
        ),
        div(id = "confirm_pwd_feedback", class = "invalid-feedback", style = "display:none;"),

        # Password Strength Requirements
        div(
          class = "reset-reqs",
          div(class = "reset-reqs-title", "Password requirements"),
          tags$ul(
            tags$li(
              class = "req-item is-invalid",
              `data-rule` = "length",
              tags$span(class = "req-icon"),
              tags$span(class = "req-text", "At least 8 characters")
            ),
            tags$li(
              class = "req-item is-invalid",
              `data-rule` = "uppercase",
              tags$span(class = "req-icon"),
              tags$span(class = "req-text", "At least one uppercase letter")
            ),
            tags$li(
              class = "req-item is-invalid",
              `data-rule` = "special",
              tags$span(class = "req-icon"),
              tags$span(class = "req-text", "At least one special character")
            ),
            tags$li(
              class = "req-item is-invalid",
              `data-rule` = "number",
              tags$span(class = "req-icon"),
              tags$span(class = "req-text", "At least one number")
            )
          )
        ),
        
        # Reset Password Button
        div(style = "margin-top: 24px;",
          tags$button(
            id = "save_login", class = "login-btn reset-btn",
            disabled = "disabled",
            "Save and Login",
            onclick = "Shiny.setInputValue('save_login', Math.random(), {priority: 'event'})"
          )
        ),
        
        # Reset Status Message
        uiOutput("reset_status")
      )
    )
  )
}


