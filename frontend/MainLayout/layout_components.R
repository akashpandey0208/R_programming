# ==============================================================================
# UI COMPONENTS MODULE
# ==============================================================================
# This module contains reusable UI component functions for building the
# application interface (sidebar, header, etc.)
# ==============================================================================

# Create Sidebar Navigation ==================================================
#' @description Generates the left sidebar with logo and menu items
#' @param items List of menu items to display (defaults to MENU_ITEMS)
#' @return HTML div containing the complete sidebar structure
create_sidebar <- function(items = MENU_ITEMS) {
  div(class = "left-sidebar",
    
        # Logo Section -----------------------------------------------------------
    div(class = "logo-container", 
      tags$img(
        src = "images/actalent.png", 
        alt = "Actalent Logo", 
        style = "max-width: 100%; max-height: 100%; object-fit: contain;"
      )
    ),
    
    # Navigation Menu --------------------------------------------------------
    div(class = "sidebar-menu",
      lapply(items, function(item) {
        create_menu_item(
          id = item$id, 
          label = item$label, 
          active = item$active
        )
      })
    )
  )
}

# Create Individual Menu Item ================================================
#' @description Creates a single menu button with proper styling and onclick.
#'   For items with submenus (data_management), wraps in a hover wrapper and
#'   appends the submenu card.
#' @param id Unique identifier for the menu item
#' @param label Text label (can be vector for multi-line)
#' @param active Boolean indicating if this is the default active item
#' @return HTML button or wrapper div element
create_menu_item <- function(id, label, active = FALSE) {
  class_name <- if(active) "menu-item active" else "menu-item"

  # Handle multi-line labels
  label_content <- if(length(label) > 1) {
    div(class = "menu-item-text",
      lapply(label, tags$span)
    )
  } else {
    label
  }

  btn <- tags$button(
    class = class_name,
    id = paste0("menu-", id),
    onclick = sprintf(
      "Shiny.setInputValue('selected_menu', '%s', {priority: 'event'})",
      id
    ),
    label_content
  )

  # Data Management gets a hover submenu card
  if (id == "data_management") {
    div(class = "menu-item-wrapper has-submenu",
      btn,
      div(class = "dm-submenu-card",
        tags$button(
          class = "dm-submenu-item",
          id    = "dm-sub-data-upload",
          onclick = paste0(
            "Shiny.setInputValue('selected_menu','data_management',{priority:'event'});",
            "Shiny.setInputValue('dm_sub_menu','data_upload',{priority:'event'});",
            "document.querySelectorAll('.dm-submenu-item').forEach(function(el){el.classList.remove('dm-sub-active');});",
            "this.classList.add('dm-sub-active');"
          ),
          tags$img(src="images/CloudUpload.png", class="dm-sub-icon", alt="Upload"),
          "Data Upload"
        ),
        tags$button(
          class = "dm-submenu-item",
          id    = "dm-sub-data-operations",
          onclick = paste0(
            "Shiny.setInputValue('selected_menu','data_management',{priority:'event'});",
            "Shiny.setInputValue('dm_sub_menu','data_operations',{priority:'event'});",
            "document.querySelectorAll('.dm-submenu-item').forEach(function(el){el.classList.remove('dm-sub-active');});",
            "this.classList.add('dm-sub-active');"
          ),
          tags$img(src="images/operations.png", class="dm-sub-icon", alt="Operations"),
          "Data Operations"
        )
      )
    )
  } else {
    btn
  }
}

# Create Top Header ==========================================================
#' @description Generates the top header bar with title and action icons
#' @param show_icons Boolean to show/hide right-side icons (notifications, profile)
#' @return HTML div containing the complete header structure
create_header <- function(initials = "U", show_icons = TRUE) {
  div(class = "top-header",
    
    # Application Title ------------------------------------------------------
    div(class = "header-title", APP_TITLE),
    
    # Action Icons (Right Side) ----------------------------------------------
    if(show_icons) {
      div(class = "header-icons",
        
        # Notification Bell Icon
        create_bell_icon(),
        
        tags$button(
          id = "logout_btn",
          class = "logout-btn",
          "Logout",
          onclick = "Shiny.setInputValue('logout_btn', Math.random(), {priority: 'event'})"
        ),
        
        # User Profile Avatar
        create_user_avatar(initials)
        
      )
    } else {
      NULL
    }
  )
}

# Create Bell Notification Icon ==============================================
#' @description Creates clickable notification bell icon
#' @return HTML div containing bell image
create_bell_icon <- function() {
  div(
    class = "bell-icon",
    onclick = "Shiny.setInputValue('notification_clicked', Math.random(), {priority: 'event'})",
    tags$img(src = "images/Bell.png", alt = "Notifications")
  )
}

# Create User Avatar =========================================================
#' @description Creates clickable user profile avatar
#' @return HTML div containing profile image
create_user_avatar <- function(initials = "U") {
  div(
    class = "user-avatar",
    onclick = "Shiny.setInputValue('user_clicked', Math.random(), {priority: 'event'})",
    span(class = "avatar-text", initials)
  )
}
