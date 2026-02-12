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
