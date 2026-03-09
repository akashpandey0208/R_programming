# ==============================================================================
# DATA UPLOAD FOLDER SERVER
# ==============================================================================
# Module server for the Data Upload explorer.
# Uses local filesystem (src/data/studies/) and _fileinfo.json for metadata.
# ==============================================================================

data_upload_folder_server <- function(id) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # ── Reactive state ───────────────────────────────────────────────────────
    selected_study  <- reactiveVal(NULL)
    selected_node   <- reactiveVal(NULL)   # relative path within study e.g. "DM/Raw Data"
    expanded_nodes  <- reactiveVal(c(""))  # set of relative paths that are expanded
    refresh_trigger <- reactiveVal(0)      # increment to refresh tree / inventory

    # Initialise studies on startup
    observe({
      studies <- list_studies()
      if (length(studies) == 0) {
        initialize_dm_structure("Study-001")
        studies <- list_studies()
      }
      if (!is.null(studies) && length(studies) > 0) {
        selected_study(studies[1])
      }
    })

    # ── Study selector UI ────────────────────────────────────────────────────
    output$study_selector_ui <- renderUI({
      studies <- list_studies()
      if (length(studies) == 0) studies <- character(0)
      selectInput(
        inputId  = ns("chosen_study"),
        label    = NULL,
        choices  = studies,
        selected = selected_study(),
        width    = "100%"
      )
    })

    observeEvent(input$chosen_study, {
      selected_study(input$chosen_study)
      selected_node(NULL)
      expanded_nodes(c(""))
    })

    # ── Build tree HTML recursively ──────────────────────────────────────────
    build_tree_nodes <- function(study, rel_path, depth = 0) {
      full_path  <- study_folder_path(study, rel_path)
      children   <- get_folder_children(full_path)

      # Apply search filter
      sq <- if (!is.null(input$search_query) && nchar(trimws(input$search_query)) > 0)
              tolower(trimws(input$search_query)) else NULL
      if (!is.null(sq)) {
        children <- children[grepl(sq, tolower(children), fixed = TRUE)]
      }

      expanded <- expanded_nodes()
      sel_node <- selected_node()

      lapply(children, function(child_name) {
        child_rel  <- if (rel_path == "") child_name else paste0(rel_path, "/", child_name)
        child_full <- file.path(full_path, child_name)
        is_expanded <- child_rel %in% expanded
        is_selected <- !is.null(sel_node) && sel_node == child_rel
        node_css    <- paste0("du-tree-node", if (is_selected) " selected" else "")
        toggle_css  <- paste0("du-toggle", if (is_expanded) " open" else "")
        sub_children <- get_folder_children(child_full)
        has_children <- length(sub_children) > 0
        indent_style <- paste0("padding-left:", 12 + depth * 14, "px")

        tagList(
          div(
            class = node_css,
            style = indent_style,
            # Toggle chevron
            tags$span(
              class   = toggle_css,
              style   = if (!has_children) "visibility:hidden" else "",
              tags$img(src="images/icon_chevron.svg", alt=">", style="width:10px;height:10px;display:block;"),
              onclick = sprintf(
                "event.stopPropagation(); Shiny.setInputValue('%s','%s',{priority:'event'})",
                ns("toggle_node"), child_rel
              )
            ),
            # Folder icon
            tags$img(src="images/folder_icon.png", class="du-folder-icon", alt="folder", style="width:16px;height:16px;"),
            # Label — clicking selects the node
            tags$span(
              class   = "du-node-label",
              title   = child_name,
              child_name,
              onclick = sprintf(
                "Shiny.setInputValue('%s','%s',{priority:'event'})",
                ns("select_node"), child_rel
              )
            )
          ),
          # Children container (hidden or visible based on expanded state)
          div(
            class = "du-tree-children",
            style = if (!is_expanded) "display:none" else "",
            if (is_expanded && has_children)
              build_tree_nodes(study, child_rel, depth + 1)
            else
              NULL
          )
        )
      })
    }

    # ── Render folder tree ───────────────────────────────────────────────────
    output$folder_tree <- renderUI({
      refresh_trigger()  # take dependency
      req(selected_study())
      study <- selected_study()
      tagList(build_tree_nodes(study, ""))
    })

    # ── Toggle expand/collapse ───────────────────────────────────────────────
    observeEvent(input$toggle_node, {
      node <- input$toggle_node
      exp  <- expanded_nodes()
      if (node %in% exp) {
        expanded_nodes(exp[exp != node])
      } else {
        expanded_nodes(c(exp, node))
      }
    })

    # ── Select a node ────────────────────────────────────────────────────────
    observeEvent(input$select_node, {
      selected_node(input$select_node)
      # Auto-expand the clicked node
      node <- input$select_node
      exp  <- expanded_nodes()
      if (!node %in% exp) expanded_nodes(c(exp, node))
    })

    # ── Breadcrumb ───────────────────────────────────────────────────────────
    output$breadcrumb <- renderUI({
      study <- selected_study()
      node  <- selected_node()

      if (is.null(node) || is.null(study)) {
        return(div(class = "du-breadcrumb"))
      }

      parts <- c(study, strsplit(node, "/", fixed = TRUE)[[1]])
      items <- tagList()
      for (i in seq_along(parts)) {
        is_last <- (i == length(parts))
        items <- tagList(
          items,
          if (i > 1) tags$span(class = "du-bc-sep", ">") else NULL,
          tags$span(
            class = paste0("du-bc-item", if (is_last) " last" else ""),
            parts[i]
          )
        )
      }
      div(class = "du-breadcrumb", items)
    })

    # ── Main panel (empty state or folder content) ───────────────────────────
    output$main_panel <- renderUI({
      node <- selected_node()
      if (is.null(node)) {
        # Empty / placeholder state
        div(class = "du-empty-state",
          tags$img(src="images/folder_icon.png", class="du-empty-icon", alt="folder", style="width:64px;height:64px;opacity:0.35;"),
          div(class = "du-empty-title", "Select a folder from the explorer"),
          div(class = "du-empty-subtitle", "Choose a folder to view its contents or upload files")
        )
      } else {
        # Folder selected — show upload + inventory
        folder_name <- basename(node)
        study       <- selected_study()
        full_path   <- study_folder_path(study, node)
        files       <- get_folder_files(full_path)
        nfiles      <- length(files)

        div(class = "du-folder-content",
          # Title row
          div(class = "du-folder-title-row",
            div(class = "du-folder-name", folder_name),
            div(class = "du-file-count-badge", paste0(nfiles, " file", if (nfiles != 1) "s" else ""))
          ),

          # Upload zone
          div(class = "du-upload-zone",
            tags$img(src="images/CloudUpload.png", class="du-upload-cloud-icon", alt="Upload", style="width:40px;height:40px;opacity:0.7;"),
            div(class = "du-upload-label", "Drop files here"),
            div(class = "du-upload-types",
              "SAS \u00b7 XPT \u00b7 SAS7BDAT \u00b7 PDF \u00b7 DOCX \u00b7 RTF \u00b7 XLS \u00b7 CSV"
            ),
            fileInput(
              inputId  = ns("upload_files"),
              label    = NULL,
              multiple = TRUE,
              accept   = c(".sas", ".xpt", ".sas7bdat", ".pdf",
                           ".docx", ".doc", ".rtf", ".xls", ".xlsx", ".csv"),
              buttonLabel = "Upload Files",
              width    = "auto"
            )
          ),

          # File inventory
          div(class = "du-inventory-wrapper",
            div(class = "du-section-title", "File Inventory"),
            uiOutput(ns("file_inventory_table")),
            div(class = "du-inventory-footer",
              uiOutput(ns("inventory_footer"))
            )
          )
        )
      }
    })

    # ── File inventory table ─────────────────────────────────────────────────
    output$file_inventory_table <- renderUI({
      refresh_trigger()  # dependency
      node  <- selected_node()
      study <- selected_study()
      req(node, study)

      full_path <- study_folder_path(study, node)
      files     <- get_folder_files(full_path)

      if (length(files) == 0) {
        return(div(
          style = "padding:24px 0; text-align:center; color:#94a3b8; font-size:13px;",
          "No files uploaded yet."
        ))
      }

      meta <- read_file_metadata(full_path)

      rows <- lapply(seq_along(files), function(i) {
        fname    <- files[i]
        fpath    <- file.path(full_path, fname)
        ext      <- file_type_badge(fname)
        bg_col   <- badge_color(ext)
        info     <- tryCatch(file.info(fpath), error = function(e) NULL)
        fsize    <- if (!is.null(info)) format_filesize(info$size) else "—"
        m        <- meta[[fname]]
        uploader <- if (!is.null(m$uploader)) m$uploader else "—"
        uptime   <- if (!is.null(m$upload_time))
                      format(as.POSIXct(m$upload_time), "%m-%d-%Y")
                    else "—"

        safe_id  <- gsub("[^A-Za-z0-9]", "_", fname)

        tags$tr(
          tags$td(
            tags$span(
              class = "du-type-badge",
              style = paste0("background:", bg_col),
              ext
            )
          ),
          tags$td(class = "col-filename", fname),
          tags$td(fsize),
          tags$td(uptime),
          tags$td(uploader),
          tags$td(class = "col-actions",
            # Download button
            tags$button(
              class   = "du-action-btn download",
              title   = "Download",
              id      = ns(paste0("dl_", safe_id)),
              onclick = sprintf(
                "Shiny.setInputValue('%s','%s',{priority:'event'})",
                ns("download_file"), fname
              ),
              tags$img(src="images/dowload_file.png", alt="Download", style="width:15px;height:15px;")
            ),
            # Preview button
            tags$button(
              class   = "du-action-btn preview",
              title   = "Preview",
              id      = ns(paste0("pv_", safe_id)),
              onclick = sprintf(
                "Shiny.setInputValue('%s','%s',{priority:'event'})",
                ns("preview_file"), fname
              ),
              tags$img(src="images/view_button.png", alt="Preview", style="width:15px;height:15px;")
            ),
            # Delete button
            tags$button(
              class   = "du-action-btn delete",
              title   = "Delete",
              id      = ns(paste0("del_", safe_id)),
              onclick = sprintf(
                "Shiny.setInputValue('%s','%s',{priority:'event'})",
                ns("delete_file"), fname
              ),
              tags$img(src="images/Trash2.png", alt="Delete", style="width:15px;height:15px;")
            )
          )
        )
      })

      tagList(
        tags$table(class = "du-inventory-table",
          tags$thead(
            tags$tr(
              tags$th("TYPE"),
              tags$th("FILE NAME"),
              tags$th("SIZE"),
              tags$th("UPLOADED DATE"),
              tags$th("UPLOADED BY"),
              tags$th("ACTIONS")
            )
          ),
          tags$tbody(rows)
        )
      )
    })

    output$inventory_footer <- renderUI({
      refresh_trigger()
      node  <- selected_node()
      study <- selected_study()
      req(node, study)
      files <- get_folder_files(study_folder_path(study, node))
      n     <- length(files)
      if (n == 0) return(NULL)
      div(paste0("Showing 1\u20136 of ", n, " file", if (n != 1) "s" else ""))
    })

    # ── Upload handler ───────────────────────────────────────────────────────
    observeEvent(input$upload_files, {
      req(input$upload_files)
      node  <- selected_node()
      study <- selected_study()
      req(node, study)

      dest_folder <- study_folder_path(study, node)
      df          <- input$upload_files
      uploader    <- tryCatch(session$userData$full_name(), error = function(e) "Unknown")

      errors <- character(0)
      for (i in seq_len(nrow(df))) {
        res <- save_uploaded_file(df$datapath[i], df$name[i], dest_folder, uploader)
        if (!res$ok) errors <- c(errors, paste0(df$name[i], ": ", res$msg))
      }

      refresh_trigger(refresh_trigger() + 1)

      if (length(errors) > 0) {
        showNotification(
          paste("Upload errors:", paste(errors, collapse = "; ")),
          type = "error", duration = 6
        )
      } else {
        showNotification(
          paste0(nrow(df), " file(s) uploaded successfully."),
          type = "message", duration = 3
        )
      }
    })

    # ── Delete file ──────────────────────────────────────────────────────────
    observeEvent(input$delete_file, {
      fname <- input$delete_file
      node  <- selected_node()
      study <- selected_study()
      req(fname, node, study)

      showModal(modalDialog(
        title  = "Delete File",
        div(style = "text-align:center;",
          div(style = "margin-bottom:14px;",
              tags$img(src="images/icon_alert.svg", alt="Warning", style="width:56px;height:56px;")),
          p(HTML(paste0(
            "Are you sure you want to delete <strong>", htmltools::htmlEscape(fname), "</strong>?",
            "<br/><span style='color:#64748b;font-size:12px;'>This action cannot be undone.</span>"
          )))
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("confirm_delete_file"), "Delete",
                       class = "btn btn-danger",
                       style = "background:#dc2626;border-color:#dc2626;color:#fff")
        ),
        size = "s"
      ))
    })

    observeEvent(input$confirm_delete_file, {
      fname <- isolate(input$delete_file)
      node  <- isolate(selected_node())
      study <- isolate(selected_study())
      req(fname, node, study)

      res <- delete_file_from_folder(study_folder_path(study, node), fname)
      removeModal()
      refresh_trigger(refresh_trigger() + 1)

      if (!res$ok) {
        showNotification(paste("Error:", res$msg), type = "error", duration = 4)
      }
    })

    # ── Create folder ────────────────────────────────────────────────────────
    observeEvent(input$btn_create_folder, {
      node  <- selected_node()
      study <- selected_study()
      req(study)

      parent_label <- if (is.null(node) || node == "") {
        study
      } else {
        paste(c(study, strsplit(node, "/", fixed = TRUE)[[1]]), collapse = " > ")
      }

      showModal(modalDialog(
        title = "Create New Folder",
        div(class = "du-modal-label", "Folder Name"),
        tags$input(
          type        = "text",
          id          = ns("new_folder_name"),
          class       = "du-modal-input",
          placeholder = "Enter folder name"
        ),
        div(class = "du-modal-hint",
          HTML(paste0("Creating in: <strong>", htmltools::htmlEscape(parent_label), "</strong>"))
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("confirm_create_folder"), "Create",
                       class = "btn btn-primary",
                       style = "background:#0d1f36;border-color:#0d1f36;color:#fff")
        ),
        size = "s"
      ))
    })

    observeEvent(input$confirm_create_folder, {
      name  <- isolate(input$new_folder_name)
      node  <- isolate(selected_node())
      study <- isolate(selected_study())
      req(study)

      parent_path <- study_folder_path(study, if (is.null(node)) "" else node)
      res         <- create_subfolder(parent_path, name)

      removeModal()

      if (!res$ok) {
        showNotification(paste("Error:", res$msg), type = "error", duration = 4)
      } else {
        # Auto-expand the parent so the new folder becomes visible
        if (!is.null(node) && !node %in% expanded_nodes()) {
          expanded_nodes(c(expanded_nodes(), node))
        }
        refresh_trigger(refresh_trigger() + 1)
      }
    })

    # ── Delete folder ────────────────────────────────────────────────────────
    observeEvent(input$btn_delete_folder, {
      node <- selected_node()
      req(node)

      folder_name <- basename(node)
      showModal(modalDialog(
        title = "Delete Folder",
        div(style = "text-align:center;",
          div(style = "margin-bottom:14px;",
              tags$img(src="images/icon_alert.svg", alt="Warning", style="width:56px;height:56px;")),
          p(HTML(paste0(
            "Are you sure you want to delete <strong>", htmltools::htmlEscape(folder_name), "</strong>",
            " and all its contents?<br/>",
            "<span style='color:#64748b;font-size:12px;'>This action cannot be undone.</span>"
          )))
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("confirm_delete_folder"), "Delete",
                       class = "btn btn-danger",
                       style = "background:#dc2626;border-color:#dc2626;color:#fff")
        ),
        size = "s"
      ))
    })

    observeEvent(input$confirm_delete_folder, {
      node  <- isolate(selected_node())
      study <- isolate(selected_study())
      req(node, study)

      full_path <- study_folder_path(study, node)
      res       <- delete_folder_recursive(full_path)

      removeModal()

      if (!res$ok) {
        showNotification(paste("Error:", res$msg), type = "error", duration = 4)
      } else {
        # Clear selection and remove from expanded list
        exp <- expanded_nodes()
        expanded_nodes(exp[!startsWith(exp, node)])
        selected_node(NULL)
        refresh_trigger(refresh_trigger() + 1)
      }
    })

    # ── Collapse all ─────────────────────────────────────────────────────────
    observeEvent(input$btn_collapse_all, {
      expanded_nodes(c(""))
    })

    # ── Download file ────────────────────────────────────────────────────────
    observeEvent(input$download_file, {
      fname <- input$download_file
      node  <- selected_node()
      study <- selected_study()
      req(fname, node, study)

      full_path <- study_folder_path(study, node)
      file_path <- file.path(full_path, fname)

      if (!file.exists(file_path)) {
        showNotification("File not found.", type = "error", duration = 3)
        return()
      }

      session$sendCustomMessage("triggerDownload", list(
        url      = paste0("data/studies/", study, "/", node, "/", fname),
        filename = fname
      ))
    })

  })
}
