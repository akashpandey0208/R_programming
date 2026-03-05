# ==============================================================================
# ADMIN TABLE SERVER MODULE
# ==============================================================================
# Server logic for the Admin user management table.
# - Accepts a reactive `data` parameter (data frame from DB)
# - Status + Request Type filter with AND logic
# - Badge counts = count of Pending rows per request type (from full data)
# - Status column rendered as colored pills
# - Request Type rendered as colored text labels
# - Pending with column rendered as pill tags
# - Action column: Review (Pending) / View (Approved/Rejected) buttons
# - Full column selector, clear filters, export, custom pagination from Final_Filter1
# ==============================================================================

admin_table_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    admin_data <- reactive({
      input$dt_clear_all 
      tryCatch({
        base_url <- Sys.getenv("APIURL")
        endpoint <- paste0(base_url, "/requests")
        raw_data <- jsonlite::fromJSON(endpoint, flatten = TRUE)
        if (is.null(raw_data) || length(raw_data) == 0) {
          return(NULL)
        }
        df <- as.data.frame(raw_data, stringsAsFactors = FALSE)
        # Just rename columns instead of rebuilding
        colnames(df) <- c(
          "Request Type",
          "Request Id",
          "Full Name",
          "Email",
          "Study Name",
          "Role",
          "Status",
          "Requested At",
          "Pending with",
          "Modified By",
          "Modified On"
        )
        return(df)
      }, error = function(e) {
        warning("API Fetch Error: ", e$message)
        return(NULL)
      })
    })
    ns <- session$ns
    # ================= POPUP STATE =================
    current_request_id   <- reactiveVal(NULL)
    current_request_type <- reactiveVal(NULL)
    current_request_data <- reactiveVal(NULL)
    # -------------------------------------------------------------------------
    # Filter state reactives
    # -------------------------------------------------------------------------
    status_filter <- reactiveVal("All")
    type_filter   <- reactiveVal("All")
    observeEvent(input$status_filter, { status_filter(input$status_filter) }, ignoreNULL = FALSE)
    observeEvent(input$type_filter,   { type_filter(input$type_filter) },     ignoreNULL = FALSE)
    # -------------------------------------------------------------------------
    # Badge counts: Pending rows per Request Type from the FULL unfiltered data
    # -------------------------------------------------------------------------
    pending_counts <- reactive({
      df <- admin_data()
      if (is.null(df) || !"Request Type" %in% colnames(df) || !"Status" %in% colnames(df)) {
        return(list(new_reg = 0, role_change = 0))
      }
      pending_rows <- df[df$Status == "Pending", ]
      list(
        new_reg     = sum(pending_rows[["Request Type"]] == "New Registration", na.rm = TRUE),
        role_change = sum(pending_rows[["Request Type"]] == "Role Change",      na.rm = TRUE)
      )
    })
    # -------------------------------------------------------------------------
    # Dynamic Request Type filter buttons with live badge counts
    # -------------------------------------------------------------------------
    output$btn_new_reg <- renderUI({
      cnt        <- pending_counts()$new_reg
      badge_html <- if (cnt > 0) paste0('<span class="filter-badge">', cnt, '</span>') else ""
      is_active  <- !is.null(type_filter()) && type_filter() == "New Registration"
      tags$button(
        id    = ns("type_new_reg"),
        class = paste("filter-btn type-btn", if (is_active) "active" else ""),
        HTML(paste0("New Registration", badge_html)),
        onclick = paste0(
          "adminFilterClick(this,'", ns("type_filter"), "','New Registration','type-btn')"
        )
      )
    })
    output$btn_role_change <- renderUI({
      cnt        <- pending_counts()$role_change
      badge_html <- if (cnt > 0) paste0('<span class="filter-badge">', cnt, '</span>') else ""
      is_active  <- !is.null(type_filter()) && type_filter() == "Role Change"
      tags$button(
        id    = ns("type_role_change"),
        class = paste("filter-btn type-btn", if (is_active) "active" else ""),
        HTML(paste0("Role Change", badge_html)),
        onclick = paste0(
          "adminFilterClick(this,'", ns("type_filter"), "','Role Change','type-btn')"
        )
      )
    })
    # -------------------------------------------------------------------------
    # Filtered data — AND logic: Status filter AND Request Type filter
    # -------------------------------------------------------------------------
    filtered_data <- reactive({
      df <- admin_data()
      if (is.null(df)) return(NULL)
      sf <- status_filter()
      tf <- type_filter()
      if (!is.null(sf) && sf != "All" && "Status" %in% colnames(df)) {
        df <- df[df$Status == sf, , drop = FALSE]
      }
      if (!is.null(tf) && tf != "All" && "Request Type" %in% colnames(df)) {
        df <- df[df[["Request Type"]] == tf, , drop = FALSE]
      }
      df
    })
    # -------------------------------------------------------------------------
    # Placeholder vs. Table
    # -------------------------------------------------------------------------
    output$table_or_placeholder <- renderUI({
      if (is.null(admin_data())) {
        return(div(class = "table-placeholder", "No data available."))
      }
      tagList(
        DTOutput(ns("table")),
        uiOutput(ns("hidden_downloads"))
      )
    })
    # -------------------------------------------------------------------------
    # Render DT — apply column renderers on a display copy
    # -------------------------------------------------------------------------
    output$table <- DT::renderDT({
      df <- filtered_data()
      req(df)

      display <- df[, !colnames(df) %in% c("Request Id"), drop = FALSE]

      # Request Type: colored text labels
      if ("Request Type" %in% colnames(display)) {
        display[["Request Type"]] <- sapply(display[["Request Type"]], function(v) {
          if (is.na(v)) return("")
          if (v == "New Registration") {
            paste0('<span class="req-new-reg">', v, '</span>')
          } else if (v == "Role Change") {
            paste0('<span class="req-role-change">', v, '</span>')
          } else {
            v
          }
        })
      }
      # Status: colored pill badges
      if ("Status" %in% colnames(display)) {
        display$Status <- sapply(display$Status, function(s) {
          if (is.na(s)) return("")
          cls <- switch(s,
                        "pending"  = "status-pill status-pending",
                        "approved" = "status-pill status-approved",
                        "rejected" = "status-pill status-rejected",
                        "status-pill"
          )
          paste0('<span class="', cls, '">', s, '</span>')
        })
      }
      # Pending with: pill tag or em-dash
      if ("Pending with" %in% colnames(display)) {
        display[["Pending with"]] <- sapply(display[["Pending with"]], function(v) {
          if (is.na(v) || trimws(v) == "" || v == "\u2014") {
            return('<span style="color:#aaa;">\u2014</span>')
          }
          paste0('<span class="pending-with-tag">', v, '</span>')
        })
      }
      # Action column
      raw_status <- if ("Status" %in% colnames(df)) df$Status else rep("", nrow(df))
      display$Action <- sapply(seq_len(nrow(df)), function(i) {
        rid <- df$`Request Id`[i]
        rtype <- df$`Request Type`[i]
        status <- tolower(df$Status[i])
        if (!is.na(status) && status == "pending") {
          paste0(
            '<button class="action-review-btn status-pending-btn" ',
            'onclick="Shiny.setInputValue(\'', ns("review_clicked"),
            '\', {request_id:\'', rid, '\', request_type:\'', rtype, '\'}, {priority:\'event\'})">Review</button>'
          )
        } else {
          paste0(
            '<button class="action-view-btn status-view-btn" ',
            'onclick="Shiny.setInputValue(\'', ns("view_clicked"),
            '\', {request_id:\'', rid, '\', request_type:\'', rtype, '\'}, {priority:\'event\'})">View</button>'
          )
        }
      })
      # compute 0-based indices for the columns
      email_idx <- which(colnames(display) == "Email")
      role_idx  <- which(colnames(display) %in% c("Role/Panels"))

      # Convert to 0-based for DataTables; handle not-found gracefully
      email_targets <- if (length(email_idx)) list(targets = email_idx - 1L, className = "col-email") else NULL
      role_targets  <- if (length(role_idx))  list(targets = role_idx  - 1L, className = "col-role")  else NULL
      
      DT::datatable(
        display,
        callback = JS(sprintf("
          var ns = '%s';

          table.on('init.dt', function() {
            setTimeout(function(){
              if (typeof renderCustomPager === 'function') {
                renderCustomPager(ns);
              }
            }, 50);
          });

          table.on('draw.dt', function() {
            setTimeout(function(){
              if (typeof renderCustomPager === 'function') {
                renderCustomPager(ns);
              }
            }, 10);
          });
        ", ns(""))),
        rownames  = FALSE,
        escape    = FALSE,
        selection = "none",
        class     = "compact",
        options   = list(
          autoWidth    = FALSE,
          scrollX      = TRUE,
          paging       = TRUE,
          pageLength   = 15,
          lengthChange = FALSE,
          info         = FALSE,
          searching    = TRUE,
          dom          = "t",
          order        = list(),
          ordering     = TRUE,
          orderMulti   = FALSE,
          orderClasses = FALSE,
          columnDefs   = Filter(Negate(is.null), list(
            list(orderable = FALSE, targets = "_all"),
            list(defaultContent = "\u2014", targets = "_all"),
            email_targets,
            role_targets
          ))
        )
      )
    }, server = FALSE, filter = "none")
    # -------------------------------------------------------------------------
    # Initialise column selector overlay after data loads
    # -------------------------------------------------------------------------
    observeEvent(filtered_data(), {
      df <- filtered_data()
      req(df)
      
      display <- df[, !colnames(df) %in% c("Request Id"), drop = FALSE]
      display$Action <- ""

      session$onFlushed(function() {
        session$sendCustomMessage(
          "initColumns",
          list(
            ns = session$ns(""),
            names = as.character(colnames(display))
      )
        )
      }, once = FALSE)
    }, ignoreInit = FALSE)
    
    session$onFlushed(function() {
      session$sendCustomMessage("wirePagerOnce", list(ns = session$ns("")))
    }, once = TRUE)


    # -------------------------------------------------------------------------
    # Clear Filters (DT side + JS side)
    # -------------------------------------------------------------------------
    observeEvent(input$dt_clear_all, {
      proxy <- DT::dataTableProxy("table", session = session)
      DT::updateSearch(proxy, keywords = list(global = "", columns = NULL))
      DT::selectRows(proxy, NULL)
      session$sendCustomMessage("dt_clear_all", list(ns = session$ns("")))
    })
    # -------------------------------------------------------------------------
    # Export handlers
    # -------------------------------------------------------------------------
    export_df <- reactive({ req(filtered_data()); filtered_data() })
    output$dl_csv <- downloadHandler(
      filename = function() paste0("admin_export_", Sys.Date(), ".csv"),
      content  = function(file) readr::write_csv(export_df(), file)
    )
    output$dl_xlsx <- downloadHandler(
      filename = function() paste0("admin_export_", Sys.Date(), ".xlsx"),
      content  = function(file) openxlsx::write.xlsx(export_df(), file)
    )
    output$hidden_downloads <- renderUI({
      tagList(
        downloadButton(ns("dl_csv"),  label = NULL, class = "hidden-download"),
        downloadButton(ns("dl_xlsx"), label = NULL, class = "hidden-download")
      )
    })
    
    # Unified Review/View handler for main layout
    observeEvent(list(input$review_clicked, input$view_clicked), {
      evt <- NULL
      if (!is.null(input$review_clicked)) evt <- input$review_clicked
      if (!is.null(input$view_clicked))   evt <- input$view_clicked
      req(evt)
      request_id   <- evt$request_id
      request_type <- evt$request_type
      
      current_request_id(request_id)
      current_request_type(request_type)
      
      url <- paste0(
        "http://127.0.0.1:8000/requests/",
        request_id,
        "?request_type=",
        URLencode(request_type)
      )
      request_type_global<- session$request_type
      res <- httr::GET(url)
      if(res$status_code == 200){
        data_list <- httr::content(res, as="parsed")
        current_request_data(data_list[[1]])   # STORE DATA
      } else {
        showNotification("Failed to load request data", type="error")
        return()
      }
      show_review_popup(
        request_type,
        "main",
        session,
        current_request_data()   
      )
    })
    # -------------------------------------------------------------------------
    # Approve button clicked -> Open Approve Confirmation Popup
    # -------------------------------------------------------------------------
    observeEvent(input$approve_btn, {
      print("approve")
      req(current_request_type())
      req(current_request_data())
      removeModal()
      show_review_popup(
        current_request_type(),
        "approve",
        session,
        current_request_data()
      )
    })
    observeEvent(input$back_btn, {
      req(current_request_type())
      req(current_request_data())
      removeModal()
      show_review_popup(
        current_request_type(),
        "main",
        session,
        current_request_data()
      )
    })
  })
}
