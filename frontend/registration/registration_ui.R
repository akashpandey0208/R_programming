# ==============================================================================
# REGISTRATION UI MODULE
# ==============================================================================
# Registration and Summary page UI definitions.
# ==============================================================================

# Registration Page UI =======================================================
#' @description Full registration form with dashboard-style sidebar and header
#' @return HTML div containing the complete registration page
registration_ui <- function() {
  div(id = "registration_page", class = "main-container", style = "display: none;",
    
    # Sidebar ------------------------------------------------------------------
    create_sidebar(list(list(id = "register", label = "Register", active = TRUE))),
    
    # Header -------------------------------------------------------------------
    create_header(show_icons = FALSE),
    
    # Main Content Area --------------------------------------------------------
    div(class = "main-content",
      div(class = "page-wrap",
        
        # Requester Information
        div(class = "section-header", h3("Requester Information")),
        
        fluidRow(
        column(6,
          textInput("reg_full_name",
            tagList("Full Name", span("*", class = "required")),
            placeholder = "Enter Full Name", width = "100%"),
          uiOutput("reg_full_name_error", class = "field-error")
        ),
        column(6,
          textInput("reg_job_title",
            tagList("Job Title", span("*", class = "required")),
            placeholder = "Enter Job Title", width = "100%"),
          uiOutput("reg_job_title_error", class = "field-error")
        )
      ),
      
      fluidRow(
        column(6,
          textInput("reg_dept",
            tagList("Department / Functional Area", span("*", class = "required")),
            placeholder = "Enter Department / Functional Area", width = "100%"),
          uiOutput("reg_dept_error", class = "field-error")
        ),
        column(6,
          textInput("reg_org",
            tagList("Organization / Company", span("*", class = "required")),
            placeholder = "Enter Organization / Company", width = "100%"),
          uiOutput("reg_org_error", class = "field-error")
        )
      ),
      
      fluidRow(
        column(6,
          textInput("reg_email",
            tagList("Work Email Address", span("*", class = "required")),
            placeholder = "Email@sample.com", width = "100%"),
          uiOutput("reg_email_error", class = "field-error")
        ),
        column(6,
          textInput("reg_phone",
            tagList("Phone Number", span("*", class = "required")),
            placeholder = "1234567890", width = "100%"),
          uiOutput("reg_phone_error", class = "field-error")
        )
      ),
      
      fluidRow(
        column(6,
          textInput("reg_office",
            tagList("Office Location", span("*", class = "required")),
            placeholder = "Enter Office Location", width = "100%"),
          uiOutput("reg_office_error", class = "field-error")
        ),
        column(6,
          selectInput("reg_employment_type",
            tagList("Employment Type", span("*", class = "required")),
            choices = c("", "Sponsor Employee", "CRO Employee", "Vendor",
              "Site Staff/Investigator / Study Coordinator", "Other"),
            selected = "", width = "100%", selectize = FALSE),
          uiOutput("reg_employment_type_error", class = "field-error")
        )
      ),
      
      div(style = "height: 20px;"),
      
      div(class = "form-block",
        tags$label("Web Application Panel Requested (Check all that apply)", span("*", class = "required"),style = "font-weight: 600; color: #1f2937;"),
        fluidRow(
          column(6,
            checkboxInput("chk_clinical", "Clinical", FALSE),
            checkboxInput("chk_dm", "Data Management", FALSE),
            checkboxInput("chk_blinded", "Blinded Biometrics", FALSE),
            checkboxInput("chk_unblinded", "Unblinded Biometrics/Confirm Access", FALSE)
          ),
          column(6,
            checkboxInput("chk_medmon", "Medical Monitor", FALSE),
            checkboxInput("chk_rwe", "RWE", FALSE),
            checkboxInput("chk_dmc", "DMC", FALSE),
            checkboxInput("chk_client", "Client", FALSE)
            )
          ),
        uiOutput("reg_panels_error", class = "field-error")
        ),
        
        div(class = "divider"),
        div(class = "section-header center study-info-header", h3("Study Information")),
        fluidRow(
          column(12,
            textInput("reg_study_title",
              tagList("Study Title / Protocol Number", span("*", class = "required")),
              placeholder = "Enter Study Title or Protocol Number", width = "100%"),
            uiOutput("reg_study_title_error", class = "field-error")
          )
        ),
        fluidRow(
          column(6, 
            textInput("reg_sponsor", 
              tagList("Sponsor Name", span("*", class = "required")), 
              "", placeholder = "Enter sponsor name", width = "100%"),
            uiOutput("reg_sponsor_error", class = "field-error")
          ),
          column(6, 
            textInput("reg_ta", 
              tagList("Therapeutic Area", span("*", class = "required")), 
              "", placeholder = "Enter therapeutic area", width = "100%"),
            uiOutput("reg_ta_error", class = "field-error")
          )
        ),
        fluidRow(
          column(12, textAreaInput("reg_comments", "Comments", width = "100%",
            placeholder = "Enter Comments", rows = 4))
        ),
        
        # Continue Button
        div(class = "action-bar",
          actionButton("btn_continue", "Continue", class = "btn-continue")
        )
      )
    )
  )
}

