# ==============================================================================
# FORGOT PASSWORD UI MODULE
# ==============================================================================
# Ported from Project Folder.
# ==============================================================================

forgot_ui <- function() {
  div(
    id = "forgot_page",
    class = "login-wrapper",
    style = "display: none;", # Hidden by default

    # LEFT PANEL (background/logo section)
    div(
      class = "login-bg",
      tags$img(
        src   = "images/actalent_login_logo.png",
        class = "login-logo",
        alt   = "Logo"
      )
    ),

    # RIGHT PANEL (forgot password form)
    div(
      class = "login-right",
      div(
        class = "login-card forgot-card",
        div(
          class = "login-links forgot-back",
          actionLink(
            "back_to_login",
            tagList(
              tags$i(class = "fa-solid fa-arrow-left"),
              " Back to login"
            )
          )
        ),
        div(class = "login-title forgot-title", "Forgot Password?"),
        p(class = "forgot-subtitle", "Temporary password will be sent to your registered mail address"),

        textInput("forgot_email", "Email address"),
        div(id = "forgot_email_feedback", class = "invalid-feedback", style = "display:none;"),
        p(class = "forgot-hint", "We will send reset password instructions to this email."),

        actionButton("reset_password", "Reset Password", class = "login-btn forgot-btn", disabled = TRUE),
        
        # Forgot Status Message
        uiOutput("forgot_status")
      )
    )
  )
}
