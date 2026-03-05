# ==============================================================================
# ADMIN TABLE UI MODULE
# ==============================================================================
# UI definition for the Admin user management table.
# Includes status + request-type filter buttons, search, column selector,
# export, and a DT table rendered by the companion server module.
# ==============================================================================

admin_table_ui <- function(id) {
  ns <- NS(id)

  tagList(
    div(class = "admin-table-wrapper",

      # -----------------------------------------------------------------------
      # TOP BAR: Filter buttons (left) + Controls (right)
      # -----------------------------------------------------------------------
      div(class = "admin-topbar",

        # Left: Status filter + Request Type filter
        div(class = "admin-filters",

          # STATUS GROUP: All / Pending / Approved / Rejected
          div(class = "filter-group",
            tags$button(
              id = ns("status_all"), class = "filter-btn status-btn active",
              "All",
              onclick = paste0(
                "adminFilterClick(this,'", ns("status_filter"), "','All','status-btn')"
              )
            ),
            tags$button(
              id = ns("status_pending"), class = "filter-btn status-btn",
              "Pending",
              onclick = paste0(
                "adminFilterClick(this,'", ns("status_filter"), "','Pending','status-btn')"
              )
            ),
            tags$button(
              id = ns("status_approved"), class = "filter-btn status-btn",
              "Approved",
              onclick = paste0(
                "adminFilterClick(this,'", ns("status_filter"), "','Approved','status-btn')"
              )
            ),
            tags$button(
              id = ns("status_rejected"), class = "filter-btn status-btn",
              "Rejected",
              onclick = paste0(
                "adminFilterClick(this,'", ns("status_filter"), "','Rejected','status-btn')"
              )
            )
          ),

          # REQUEST TYPE GROUP: All / New Registration (badge) / Role Change (badge)
          div(class = "filter-group",
            tags$button(
              id = ns("type_all"), class = "filter-btn type-btn active",
              "All",
              onclick = paste0(
                "adminFilterClick(this,'", ns("type_filter"), "','All','type-btn')"
              )
            ),
            uiOutput(ns("btn_new_reg")),
            uiOutput(ns("btn_role_change"))
          )
        ),

        # Right: Column Selector + Clear Filters + Search + Export
        div(class = "topbar-right",

          # COLUMN SELECTOR
          div(class = "table-toolbar",
            tags$button(
              id = ns("colCounterSelect"), class = "dt-top-select",
              HTML("<span>0 columns selected</span><span>&#9660;</span>")
            ),
            div(id = ns("colOverlay"), class = "col-overlay hidden",
              div(id = ns("colCountText"),
                  style = "font-weight:bold; font-size:11px; margin-bottom:6px;"),
              tags$input(id = ns("colSearch"), class = "col-search",
                         placeholder = "Search columns"),
              div(class = "col-actions",
                tags$label(
                  tags$input(type = "checkbox", id = ns("selectAllCols"),
                             checked = TRUE),
                  " Select All"
                ),
                tags$button(id = ns("resetCols"), class = "reset-btn", "Reset")
              ),
              div(id = ns("colList"), class = "col-list")
            )
          ),

          # CLEAR FILTERS
          actionButton(ns("dt_clear_all"), label = "Clear Filters",
                       class = "btn-clear action-button"),

          # SEARCH
          tags$input(
            id = ns("global_search"), class = "global-search admin-search",
            type = "text",
            placeholder = "Search by name, email, or ID..."
          ),

          # EXPORT
          div(class = "export-wrap",
            tags$button(
              id = ns("exportBtn"), class = "export-btn", type = "button",
              "Export"
            ),
            div(id = ns("exportMenu"), class = "export-menu hidden",
              actionButton(ns("export_csv"),  label = "CSV",   class = "export-item"),
              actionButton(ns("export_xlsx"), label = "Excel", class = "export-item")
            )
          )
        )
      ),

      # -----------------------------------------------------------------------
      # TABLE AREA
      # -----------------------------------------------------------------------
      uiOutput(ns("table_or_placeholder"))
    ),

    # Inline JS: toggle active class on filter buttons + send value to Shiny
    tags$script(HTML("
      function adminFilterClick(btn, inputId, value, groupClass) {
        $(btn).closest('.filter-group').find('.' + groupClass).removeClass('active');
        $(btn).addClass('active');
        Shiny.setInputValue(inputId, value, {priority: 'event'});
      }
    "))
  )
}
