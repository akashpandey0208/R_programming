# ==============================================================================
# AUTH HELPERS MODULE
# ==============================================================================
# Password strength checking, toast notifications, and UI helpers.
# ==============================================================================

# Check Password Strength ====================================================
check_password_strength <- function(password) {
  errors <- c()
  
  if (nchar(password) < 8) {
    errors <- c(errors, "Password must be at least 8 characters long")
  }
  if (!grepl("[A-Z]", password)) {
    errors <- c(errors, "Password must contain at least one uppercase letter")
  }
  if (!grepl("[0-9]", password)) {
    errors <- c(errors, "Password must contain at least one number")
  }
  if (!grepl("[!@#$%^&*(),.?\":{}|<>]", password)) {
    errors <- c(errors, "Password must contain at least one special character")
  }
  
  if (length(errors) == 0) {
    return(list(valid = TRUE, message = "Password meets all requirements"))
  } else {
    return(list(valid = FALSE, message = paste(errors, collapse = "; ")))
  }
}

# ==============================================================================
# TOAST MESSAGE HELPER
# ==============================================================================
toast_message <- function(type = c("success", "error"), text, icon = NULL) {
  type <- match.arg(type)
  
  if (is.null(icon)) {
    icon <- if (type == "success") HTML("&#10003;") else HTML("&#10007;")
  }
  
  div(
    class = paste0("toast-", type),
    span(class = paste0(type, "-icon"), icon),
    span(class = paste0(type, "-text"), text),
    tags$button(
      class = "close-btn",
      HTML("&times;"),
      onclick = "this.parentElement.style.display='none';"
    )
  )
}

# ==============================================================================
# PASSWORD FIELD UI HELPER
# ==============================================================================
password_field <- function(
    input_id = "password",
    label_text = "Password",
    autocomplete = "current-password",
    show_toggle = TRUE
) {
  div(
    class = "form-group",
    tags$label(label_text, `for` = input_id,
               style = "display: block; font-size: 13px; font-weight: 600; color: #0d1f36; margin-bottom: 6px; font-family: 'Inter', sans-serif;"
    ),
    div(
      class = "password-wrapper",
      tags$input(
        id = input_id,
        type = "password",
        class = "form-control auth-input password-field",
        name = input_id,
        autocomplete = autocomplete,
        placeholder = paste("Enter", tolower(label_text))
      ),
      tags$span(
        class = "toggle-password",
        `data-target` = input_id,
        role = "button",
        tabindex = "0",
        `aria-label` = "Toggle password visibility",
        `aria-controls` = input_id,
        HTML("&#128065;")
      )
    )
  )
}

# ==============================================================================
# OVERLAY HELPERS
# ==============================================================================
show_overlay <- function() {
  shinyjs::show("session_check_overlay")
}

hide_overlay <- function() {
  shinyjs::hide("session_check_overlay")
}

switch_page <- function(page_id) {
  shinyjs::hide("login_page")
  shinyjs::hide("forgot_page")
  shinyjs::hide("code_verification_page")
  shinyjs::hide("reset_page")
  shinyjs::hide("dashboard_page")
  shinyjs::show(page_id)
}