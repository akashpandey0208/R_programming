# ==============================================================================
# UI COMPONENTS MODULE
# ==============================================================================
# This module contains reusable UI component functions for building the
# application interface (sidebar, header, etc.)
# ==============================================================================

# Create Sidebar Navigation ==================================================
#' @description Generates the left sidebar with logo and menu items
#' @return HTML div containing the complete sidebar structure
create_sidebar <- function() {
  div(class = "left-sidebar",
    
    # Logo Section -----------------------------------------------------------
    div(class = "logo-container", 
      tags$img(
        src = "actalent.png", 
        alt = "actalent Logo", 
        style = "max-width: 100%; max-height: 100%; object-fit: contain;"
      )
    ),
    
    # Navigation Menu --------------------------------------------------------
    div(class = "sidebar-menu",
      lapply(MENU_ITEMS, function(item) {
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
#' @description Creates a single menu button with proper styling and onclick
#' @param id Unique identifier for the menu item
#' @param label Text label (can be vector for multi-line)
#' @param active Boolean indicating if this is the default active item
#' @return HTML button element
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
  
  tags$button(
    class = class_name,
    id = paste0("menu-", id),
    onclick = sprintf(
      "Shiny.setInputValue('selected_menu', '%s', {priority: 'event'})", 
      id
    ),
    label_content
  )
}

# Create Top Header ==========================================================
#' @description Generates the top header bar with title and action icons
#' @return HTML div containing the complete header structure
create_header <- function() {
  div(class = "top-header",
    
    # Application Title ------------------------------------------------------
    div(class = "header-title", APP_TITLE),
    
    # Action Icons (Right Side) ----------------------------------------------
    div(class = "header-icons",
      
      # Notification Bell Icon
      create_bell_icon(),
      
      # User Profile Avatar
      create_user_avatar()
    )
  )
}

# Create Bell Notification Icon ==============================================
#' @description Creates clickable notification bell icon
#' @return HTML div containing bell image
create_bell_icon <- function() {
  div(
    class = "bell-icon",
    onclick = "Shiny.setInputValue('notification_clicked', Math.random(), {priority: 'event'})",
    tags$img(src = "Bell.png", alt = "Notifications")
  )
}

# Create User Avatar =========================================================
#' @description Creates clickable user profile avatar
#' @return HTML div containing profile image
create_user_avatar <- function() {
  div(
    class = "user-avatar",
    onclick = "Shiny.setInputValue('user_clicked', Math.random(), {priority: 'event'})",
    tags$img(src = "Profile_img.png", alt = "User Profile")
  )
}
