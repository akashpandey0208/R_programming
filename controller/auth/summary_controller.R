# ==============================================================================
# SUMMARY CONTROLLER
# ==============================================================================
# This module defines the Controller logic for the Summary / Review page.
# ==============================================================================

#' @description Handles summary page actions (back to edit, submit, modal)
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
summary_controller <- function(input, output, session) {
  
  # Toggle Submit Request Button based on checkbox ---------------------------
  observeEvent(input$consent_review, {
    if (isTRUE(input$consent_review)) {
      shinyjs::show("submit_request")
    } else {
      shinyjs::hide("submit_request")
    }
  })
  
  # Back to Edit Button -------------------------------------------------------
  observeEvent(input$back_to_edit, {
    shinyjs::hide("summary_page")
    shinyjs::show("registration_page")
    
    # Clear errors after UI re-renders
    session$onFlushed(function(...) {
      for (id in c(
        "reg_full_name", "reg_dept", "reg_email", "reg_office", "reg_job_title",
        "reg_org", "reg_phone", "reg_employment_type", "reg_study_title"
      )) {
        session$sendCustomMessage(type = "remove_error", message = list(id = id))
        output[[paste0(id, "_error")]] <- renderUI({ NULL })
      }
    }, once = TRUE)
  })
  
  # Submit Request Button -----------------------------------------------------
  observeEvent(input$submit_request, {
    req(isTRUE(input$consent_review))
    
    # Collect Form Data
    panels <- c(
      if (isTRUE(input$chk_clinical))  "Clinical",
      if (isTRUE(input$chk_dm))        "Data Management",
      if (isTRUE(input$chk_blinded))   "Blinded Biometrics",
      if (isTRUE(input$chk_unblinded)) "Unblinded Biometrics/Confirm Access",
      if (isTRUE(input$chk_medmon))    "Medical Monitor",
      if (isTRUE(input$chk_rwe))       "RWE",
      if (isTRUE(input$chk_dmc))       "DMC",
      if (isTRUE(input$chk_client))    "Client"
    )
    
    request_payload <- list(
      full_name = input$reg_full_name,
      email = input$reg_email,
      job_title = input$reg_job_title,
      dept = input$reg_dept,
      org = input$reg_org,
      phone = input$reg_phone,
      office = input$reg_office,
      employment_type = input$reg_employment_type,
      roles_requested = panels,
      study_title = input$reg_study_title,
      sponsor = input$reg_sponsor,
      ta = input$reg_ta,
      request_comments = input$reg_request_comments
    )
    
    # Call Plumber API
    response <- tryCatch({
      httr::POST(
        url = paste0(Sys.getenv("API_URL"), "/submit_request"),
        body = request_payload,
        encode = "json",
        httr::add_headers(`Content-Type` = "application/json")
      )
    }, error = function(e) {
      NULL
    })
    
    # Handle Response
    if (!is.null(response)) {
      
      content <- httr::content(response)
      
      if (response$status_code == 200 && content$status == "success") {
        
        showModal(
          modalDialog(
            title = "Submission Successful",
            "Your access request has been submitted successfully.",
            footer = tagList(
              actionButton("ok_modal", "OK")
            ),
            easyClose = FALSE
          )
        )
        
      } else {
        
        showModal(
          modalDialog(
            title = "Submission Failed",
            paste("Error:", content$message),
            easyClose = TRUE
          )
        )
      }
      
    } else {
      
      showModal(
        modalDialog(
          title = "Submission Failed",
          "API not reachable",
          easyClose = TRUE
        )
      )
    }
    
  })
  
  
  # OK Button in Modal: Return to Login --------------------------------------
  observeEvent(input$ok_modal, {
    removeModal()
    shinyjs::hide("summary_page")
    shinyjs::show("login_page")
    
    # Reset form state if needed
    updateCheckboxInput(session, "consent_review", value = FALSE)
  })
}

