<<<<<<< HEAD
# ============================================================================
# Clinical Webapp - Main Application Entry Point
# ============================================================================
# 
# This is the main entry point for the Shiny application.
# The application is organized into separate modules for maintainability.
#
# File Structure:
# - global.R: Libraries, helper functions, and global variables
# - ui.R: User interface definition
# - server.R: Server logic and module coordination
# - modules/: Individual module files for each feature
#   - data_module.R: Data upload and management
#   - table_module.R: Data table display and export
#   - graphs_module.R: Visualization creation
#   - reports_module.R: Report generation (placeholder)
#   - help_module.R: Documentation and help
# - www/css/: Organized CSS files
#   - style_main.css: Main layout and general styles
#   - style_sidebar.css: Sidebar navigation styles
#   - style_topbar.css: Top navigation bar styles
#   - style_buttons.css: Button and control styles
#   - style_graphs.css: Graph and visualization styles
#
# ============================================================================

# Source all module files
source("modules/data_module.R")
source("modules/table_module.R")
source("modules/graphs_module.R")
source("modules/reports_module.R")
source("modules/help_module.R")

# Run the application
shinyApp(ui = ui, server = server)
=======
# ==============================================================================
# CLINICAL WEBAPP - MAIN APPLICATION FILE
# ==============================================================================
# Application entry point. This file loads the UI and server components
# and starts the Shiny application.
#
# To run this application:
#   1. Make sure all required packages are installed
#   2. Set working directory to this folder
#   3. Run: shiny::runApp()
# ==============================================================================

# Source UI and Server Components ============================================
source("ui.R")
source("server.R")

# Launch Application =========================================================
shinyApp(ui = ui, server = server)
>>>>>>> b7020ab (Inital check-in of main page clinical)
