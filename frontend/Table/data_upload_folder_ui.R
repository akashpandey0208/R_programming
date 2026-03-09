# ==============================================================================
# DATA UPLOAD FOLDER UI
# ==============================================================================
# Full-page explorer + file inventory for the Data Upload sub-section of
# Data Management.  Local filesystem backed (src/data/studies/).
# ==============================================================================

data_upload_folder_ui <- function(id) {
  ns <- NS(id)

  div(class = "du-page-wrapper",

    # ── LEFT: Explorer panel ─────────────────────────────────────────────────
    div(class = "du-explorer-panel",

      # Header row: title + action icon-buttons
      div(class = "du-explorer-header",
        span(class = "du-explorer-title", "EXPLORER"),
        div(class = "du-explorer-actions",
          # Create folder
          tags$button(
            class   = "du-icon-btn",
            id      = ns("btn_create_folder"),
            title   = "Create new folder",
            onclick = sprintf(
              "Shiny.setInputValue('%s', Math.random(), {priority:'event'})",
              ns("btn_create_folder")
            ),
            tags$img(src="images/Add_folder.png", alt="Create folder", style="width:36px;height:36px;")
          ),
          # Delete selected folder
          tags$button(
            class   = "du-icon-btn",
            id      = ns("btn_delete_folder"),
            title   = "Delete selected folder",
            onclick = sprintf(
              "Shiny.setInputValue('%s', Math.random(), {priority:'event'})",
              ns("btn_delete_folder")
            ),
            tags$img(src="images/delete.png", alt="Delete folder", style="width:36px;height:36px;")
          ),
          # Collapse all
          tags$button(
            class   = "du-icon-btn",
            id      = ns("btn_collapse_all"),
            title   = "Collapse all",
            onclick = sprintf(
              "Shiny.setInputValue('%s', Math.random(), {priority:'event'})",
              ns("btn_collapse_all")
            ),
            tags$img(src="images/collapse.png", alt="Collapse all", style="width:30px;height:30px;")
          )
        )
      ),

      # Study selector dropdown
      div(class = "du-study-selector",
        uiOutput(ns("study_selector_ui"))
      ),

      # Search box
      div(class = "du-search-box",
        tags$img(src="images/Search.png", class="du-search-icon", alt="Search", style="width:16px;height:16px;"),
        tags$input(
          type        = "text",
          id          = ns("search_folders"),
          placeholder = "Search folders...",
          oninput     = sprintf(
            "Shiny.setInputValue('%s', this.value, {priority:'event'})",
            ns("search_query")
          )
        )
      ),

      # Collapsible folder tree
      div(class = "du-tree-container",
        uiOutput(ns("folder_tree"))
      )
    ),

    # ── RIGHT: Content panel ─────────────────────────────────────────────────
    div(class = "du-content-panel",

      # Breadcrumb
      uiOutput(ns("breadcrumb")),

      # Conditional: empty state OR folder content
      uiOutput(ns("main_panel"))
    )
  )
}
