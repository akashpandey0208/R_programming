# ==============================================================================
# CONTENT MODULES
# ==============================================================================
# This module contains rendering functions for each menu section's content.
# Each function returns the HTML content to be displayed in the main area.
# ==============================================================================

# Clinical Module ============================================================
#' @description Renders the Clinical module content
#' @return HTML div with clinical module content
render_clinical_module <- function() {
  div(
    h2("Clinical Module"),
    p("Welcome to the Clinical module. This is where clinical data and workflows are managed."),
    br(),
    p("Features include:"),
    tags$ul(
      tags$li("Clinical trial management"),
      tags$li("Patient enrollment tracking"),
      tags$li("Protocol adherence monitoring"),
      tags$li("Adverse event reporting")
    )
  )
}

# Data Management Module =====================================================
#' @description Renders the Data Management module content
#' @return HTML div with data management module content
render_data_management_module <- function() {
  div(
    h2("Data Management"),
    p("Data management tools and utilities for clinical trials."),
    br(),
    p("Manage and monitor:"),
    tags$ul(
      tags$li("Electronic Data Capture (EDC)"),
      tags$li("Data quality checks"),
      tags$li("Query management"),
      tags$li("Database locks")
    )
  )
}

# Blinded Biometrics Module ==================================================
#' @description Renders the Blinded Biometrics module content
#' @return HTML div with blinded biometrics module content
render_blinded_biometrics_module <- function() {
  div(
    h2("Blinded Biometrics"),
    p("Blinded statistical analysis and biometric evaluation tools."),
    br(),
    p("Services include:"),
    tags$ul(
      tags$li("Blinded data review"),
      tags$li("Sample size re-estimation"),
      tags$li("Interim analysis support"),
      tags$li("Statistical programming")
    )
  )
}

# Medical Monitor Module =====================================================
#' @description Renders the Medical Monitor dashboard with key metrics
#' @return HTML div with medical monitor dashboard content
render_medical_monitor_module <- function() {
  div(
    h2("Medical Monitor Dashboard"),
    p("Real-time monitoring of clinical trial data and safety signals."),
    br(),
    
    # Dashboard Cards Grid
    div(
      style = "display: grid; grid-template-columns: repeat(auto-fit, minmax(280px, 1fr)); gap: 20px; margin-top: 30px;",
      
      # Active Trials Card
      div(
        style = "background: white; padding: 24px; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
        h4("Active Trials"),
        p(style = "font-size: 36px; font-weight: bold; color: #0d1f36; margin: 10px 0;", "24"),
        p(style = "color: #64748b; font-size: 14px;", "Currently ongoing")
      ),
      
      # Enrolled Patients Card
      div(
        style = "background: white; padding: 24px; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
        h4("Enrolled Patients"),
        p(style = "font-size: 36px; font-weight: bold; color: #0d1f36; margin: 10px 0;", "1,247"),
        p(style = "color: #64748b; font-size: 14px;", "Across all trials")
      ),
      
      # Safety Alerts Card
      div(
        style = "background: white; padding: 24px; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
        h4("Safety Alerts"),
        p(style = "font-size: 36px; font-weight: bold; color: #ef4444; margin: 10px 0;", "3"),
        p(style = "color: #64748b; font-size: 14px;", "Requiring attention")
      )
    )
  )
}

# Real World Evidence Module =================================================
#' @description Renders the Real World Evidence module content
#' @return HTML div with RWE module content
render_rwe_module <- function() {
  div(
    h2("Real World Evidence"),
    p("Real-world evidence collection and analysis platform."),
    br(),
    p("Capabilities:"),
    tags$ul(
      tags$li("Observational study design"),
      tags$li("Claims data analysis"),
      tags$li("Patient registry management"),
      tags$li("Comparative effectiveness research")
    )
  )
}

# Data Monitoring Committee Module ===========================================
#' @description Renders the DMC module content
#' @return HTML div with DMC module content
render_dmc_module <- function() {
  div(
    h2("Data Monitoring Committee"),
    p("DMC review materials and safety data monitoring."),
    br(),
    p("Functions:"),
    tags$ul(
      tags$li("Safety data preparation"),
      tags$li("Efficacy endpoint reports"),
      tags$li("Meeting materials generation"),
      tags$li("Recommendation tracking")
    )
  )
}

# Client Portal Module =======================================================
#' @description Renders the Client Portal module content
#' @return HTML div with client portal module content
render_client_module <- function() {
  div(
    h2("Client Portal"),
    p("Client-facing reports and data access."),
    br(),
    p("Available resources:"),
    tags$ul(
      tags$li("Trial status reports"),
      tags$li("Enrollment dashboards"),
      tags$li("Milestone tracking"),
      tags$li("Document repository")
    )
  )
}

# Administration Module ======================================================
#' @description Renders the Administration module content
#' @return HTML div with admin module content
render_admin_module <- function() {
  div(
    h2("Administration"),
    p("System administration and user management."),
    br(),
    p("Administrative functions:"),
    tags$ul(
      tags$li("User account management"),
      tags$li("Role and permission settings"),
      tags$li("Audit log review"),
      tags$li("System configuration")
    )
  )
}

# Welcome/Default Module =====================================================
#' @description Renders the default welcome screen
#' @return HTML div with welcome content
render_welcome_module <- function() {
  div(
    h2("Welcome to Clinical Webapp"),
    p("Please select a module from the sidebar to begin."),
    br(),
    p("This application provides comprehensive tools for clinical trial management and analysis.")
  )
}
