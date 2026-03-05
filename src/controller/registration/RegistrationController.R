# ==============================================================================
# REGISTRATION CONTROLLER
# ==============================================================================

registration_controller <- function(input, output, session) {
  
  
  # ---- Email validator ----
  valid_email <- function(email) {
    email <- trimws(email %||% "")
    pattern <- "^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$"
    grepl(pattern, email, perl = TRUE)
  }
  
  # ---- Full Name validator (Unicode aware) ----
  valid_full_name <- function(x) {
    x <- trimws(x %||% "")
    grepl("^(?=.*\\p{L}.*\\p{L})[\\p{L} .'-]+$", x, perl = TRUE)
  }
  
  # ---- Phone validator (digits only) ----
  valid_phone <- function(x) {
    x <- gsub("\\s", "", x %||% "")
    nzchar(x) && grepl("^[0-9]+$", x)
  }
  
  # ---------------------------------------------------------------------------
  # Navigate to Registration Page
  # ---------------------------------------------------------------------------
  observeEvent(input$goto_register, {
    shinyjs::hide("login_page")
    shinyjs::show("registration_page")
  })
  
  
  # ---------------------------------------------------------------------------
  # Continue Button: Validate Required Fields
  # ---------------------------------------------------------------------------
  observeEvent(input$btn_continue, {
    
    # -----------------------------
    # Required Fields Mapping
    # -----------------------------
    required_fields <- list(
      reg_full_name = "Full Name",
      reg_dept = "Department / Functional Area",
      reg_email = "Work Email Address",
      reg_office = "Office Location",
      reg_job_title = "Job Title",
      reg_org = "Organization / Company",
      reg_phone = "Phone Number",
      reg_employment_type = "Employment Type",
      reg_study_title = "Study Title / Protocol Number",
      reg_sponsor = "Sponsor Name",
      reg_ta = "Therapeutic Area"
    )
    
    all_ok <- TRUE
    
    
    # -----------------------------
    # Validate Each Required Field
    # -----------------------------
    for (id in names(required_fields)) {
      
      label_text <- required_fields[[id]]   # capture safely
      value <- input[[id]]
      ok <- !is.null(value) && nzchar(trimws(value))
      
      if (!ok) {
        all_ok <- FALSE
        
        # Add red border
        session$sendCustomMessage(
          type = "add_error",
          message = list(id = id)
        )
        
        # Show specific error
        local({
          output_id <- paste0(id, "_error")
          label_copy <- label_text
          
          output[[output_id]] <- renderUI({
            div(
              class = "invalid-feedback visible",
              paste0(label_copy, " is required")
            )
          })
        })
        
      } else {
        
        session$sendCustomMessage(
          type = "remove_error",
          message = list(id = id)
        )
        
        local({
          output_id <- paste0(id, "_error")
          output[[output_id]] <- renderUI({ NULL })
        })
      }
    }
    
    
    # -------------------------------------------------------------------------
    # ADDITIONAL FORMAT VALIDATIONS (Only if required fields passed)
    # -------------------------------------------------------------------------
    
    # ---- Full Name ----
    if (all_ok && !valid_full_name(input$reg_full_name)) {
      all_ok <- FALSE
      session$sendCustomMessage("add_error", list(id = "reg_full_name"))
      output$reg_full_name_error <- renderUI({
        div(class = "invalid-feedback visible",
            "Use letters, spaces, apostrophes, hyphens, or dots (no digits).")
      })
    }
    
    # ---- Email ----
    if (all_ok && !valid_email(input$reg_email)) {
      all_ok <- FALSE
      session$sendCustomMessage("add_error", list(id = "reg_email"))
      output$reg_email_error <- renderUI({
        div(class="invalid-feedback visible",
            "Enter a valid email (e.g., name@email.com)")
      })
    }
    
    # ---- Phone ----
    if (all_ok && !valid_phone(input$reg_phone)) {
      all_ok <- FALSE
      session$sendCustomMessage("add_error", list(id = "reg_phone"))
      output$reg_phone_error <- renderUI({
        div(class="invalid-feedback visible",
            "Enter a valid phone number (digits only)")
      })
    }
    
    # --- Live-clear for Sponsor (as soon as user types something non-empty) ---
    observeEvent(input$reg_sponsor, {
      val <- trimws(input$reg_sponsor %||% "")
      if (nzchar(val)) {
        session$sendCustomMessage(type = "remove_error", message = list(id = "reg_sponsor"))
        output$sponsor_error <- renderUI({ NULL })
      }
    }, ignoreInit = TRUE)
    
    # --- Live-clear for Therapeutic Area (TA) ---
    observeEvent(input$reg_ta, {
      val <- trimws(input$reg_ta %||% "")
      if (nzchar(val)) {
        session$sendCustomMessage(type = "remove_error", message = list(id = "reg_ta"))
        output$ta_error <- renderUI({ NULL })
      }
    }, ignoreInit = TRUE)
    
    
    
    # -------------------------------------------------------------------------
    # Validate Panels (at least one checkbox)
    # -------------------------------------------------------------------------
    panel_ids <- c(
      "chk_clinical", "chk_dm", "chk_blinded", "chk_unblinded",
      "chk_medmon", "chk_rwe", "chk_dmc", "chk_client"
    )
    
    any_panel_selected <- any(sapply(panel_ids, function(x) isTRUE(input[[x]])))
    
    if (!any_panel_selected) {
      all_ok <- FALSE
      output$reg_panels_error <- renderUI({
        div(
          class = "invalid-feedback visible",
          "Please select at least one application panel."
        )
      })
    } else {
      output$reg_panels_error <- renderUI({ NULL })
    }
    
    
    # -------------------------------------------------------------------------
    # If Everything Valid → Show Summary
    # -------------------------------------------------------------------------
    if (all_ok) {
      
      g <- function(x) if (is.null(x) || !nzchar(x)) "\u2014" else x
      
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
      
      panel_text <- if (length(panels) == 0) "\u2014" else paste(panels, collapse = ", ")
      
      
      # -----------------------------
      # Render Requestor Info
      # -----------------------------
      output$summary_requestor <- renderUI({
        div(class = "kv",
            div(class = "kv-item", div(class = "k", "Full Name:"), div(class = "v", g(input$reg_full_name))),
            div(class = "kv-item", div(class = "k", "Job Title:"), div(class = "v", g(input$reg_job_title))),
            div(class = "kv-item", div(class = "k", "Department / Functional Area:"), div(class = "v", g(input$reg_dept))),
            div(class = "kv-item", div(class = "k", "Organization / Company:"), div(class = "v", g(input$reg_org))),
            div(class = "kv-item", div(class = "k", "Work Email Address:"), div(class = "v", g(input$reg_email))),
            div(class = "kv-item", div(class = "k", "Phone Number:"), div(class = "v", g(input$reg_phone))),
            div(class = "kv-item", div(class = "k", "Office Location:"), div(class = "v", g(input$reg_office))),
            div(class = "kv-item", div(class = "k", "Employment Type:"), div(class = "v", g(input$reg_employment_type)))
        )
      })
      
      
      # -----------------------------
      # Render Panels
      # -----------------------------
      output$summary_panels <- renderUI({
        div(class = "v-panel-list", panel_text)
      })
      
      
      # -----------------------------
      # Render Study Info
      # -----------------------------
      output$summary_study <- renderUI({
        div(class = "kv",
            div(class = "kv-item", div(class = "k", "Study Title / Protocol Number:"), div(class = "v", g(input$reg_study_title))),
            div(class = "kv-item", div(class = "k", "Sponsor / Program Name:"), div(class = "v", g(input$reg_sponsor))),
            div(class = "kv-item", div(class = "k", "Therapeutic Area:"), div(class = "v", g(input$reg_ta)))
        )
      })
      
      
      # Reset consent
      updateCheckboxInput(session, "consent_review", value = FALSE)
      shinyjs::hide("submit_request")
      
      # Navigate
      shinyjs::hide("registration_page")
      shinyjs::show("summary_page")
    }
  })
  
  observeEvent(input$submit_request, {
    body <- list(
      full_name       = input$reg_full_name,
      email           = input$reg_email,
      job_title       = input$reg_job_title,
      department      = input$reg_dept,
      organization    = input$reg_org,
      phone_number    = input$reg_phone,
      office_location = input$reg_office,
      employment_type = input$reg_employment_type,
      roles_requested = c(
        if (isTRUE(input$chk_clinical))  "Clinical",
        if (isTRUE(input$chk_dm))        "Data Management",
        if (isTRUE(input$chk_blinded))   "Blinded Biometrics",
        if (isTRUE(input$chk_unblinded)) "Unblinded Biometrics/Confirm Access",
        if (isTRUE(input$chk_medmon))    "Medical Monitor",
        if (isTRUE(input$chk_rwe))       "RWE",
        if (isTRUE(input$chk_dmc))       "DMC",
        if (isTRUE(input$chk_client))    "Client"
      ),
      study_name_protocol_number = input$reg_study_title,
      sponsor         = input$reg_sponsor,
      therapeutic_areas = input$reg_ta,
      request_comments = input$request_comments
    )
    
    res <- httr::POST(
      "http://localhost:8000/submit_request",   # match your API port
      body = jsonlite::toJSON(body, auto_unbox = TRUE),
      encode = "json",
      httr::add_headers(Authorization = paste("Bearer", session$userData$access_token()))
    )
    
    if (httr::status_code(res) == 200) {
      output$summary_submit_status <- renderUI({
        toast_message("success", "Access request submitted successfully")
      })
      shinyjs::hide("summary_page")
      shinyjs::show("login_page")
    } else {
      msg <- httr::content(res)$message %||% "Unknown error"
      output$summary_submit_status <- renderUI({
        toast_message("error", paste("Submission failed:", msg))
      })
    }
  })
  
}