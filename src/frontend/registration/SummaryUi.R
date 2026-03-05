# ==============================================================================
# SUMMARY UI MODULE
# ==============================================================================
# This module defines the Summary / Review page UI.
# ==============================================================================

#' @description Summary review page with dashboard-style sidebar and header
#' @return HTML div containing the complete summary page
summary_ui <- function() {
  div(id = "summary_page", class = "main-container", style = "display: none;",
    
    # Sidebar ------------------------------------------------------------------
    create_sidebar(list(list(id = "register", label = "Register", active = TRUE))),
    
    # Header -------------------------------------------------------------------
    create_header(show_icons = FALSE),
    
    # Main Content Area --------------------------------------------------------
    div(class = "main-content",
      div(class = "page-wrap page-wrap--summary",
        
        # Review Header
        div(class = "section-header review-header", h3("Review")),
        
        # Requestor Information
        div(class = "section-header", h3("Requestor Information")),
        uiOutput("summary_requestor"),
        
        div(class = "black-sep", style = "margin: 30px 0;"),
        
        # Web Application Panels
        div(class = "section-header", h3("Web Application Panels Requested")),
        uiOutput("summary_panels"),
        
        div(class = "black-sep", style = "margin: 30px 0;"),
        
        # Study Information
        div(class = "section-header", h3("Study Information")),
        uiOutput("summary_study"),
        
        # Consent
        div(class = "consent-box",
          checkboxInput(
            "consent_review",
            label = "This information is accurate. I agree to submit this form with my consent.",
            value = FALSE, width = "100%"
          )
        ),
        
        # Action Buttons
        div(class = "review-actions", style = "margin-top: 40px; display: flex; justify-content: space-between; align-items: center;",
          actionButton("back_to_edit", "Back to Edit", class = "btn-secondary"),
          shinyjs::hidden(
            actionButton("submit_request", "Submit Request", class = "btn-continue submit-right")
          )
        )
      )
    )
  )
}
