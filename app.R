# ==============================================================================
# CLINICAL WEBAPP - MAIN ENTRY POINT
# ==============================================================================
# This is the main entry point for the Clinical Webapp Shiny application.
# It sources all required modules and initializes the application.
# ==============================================================================

# Set working directory to src folder for relative paths
# Working directory should be set to src folder when running

# 1. Load Libraries ==========================================================
source("libraries/load_libraries.R")

# 2. Load Constants ==========================================================
source("constants/app_constants.R")

# 3. Load Components =========================================================
source("components/auth_helpers.R")
source("components/UserHelpers.R")
source("components/folder_helpers.R")

# 4. Load Frontend (UI) ======================================================
source("frontend/auth/login_ui.R")
source("frontend/auth/forgot_ui.R")
source("frontend/auth/reset_ui.R")
source("frontend/registration/registration_ui.R")
source("frontend/registration/summary_ui.R")
source("frontend/MainLayout/layout_components.R")
source("frontend/MainLayout/main_layout_ui.R")
source("frontend/Table/study_table_ui.R")
source("frontend/Table/data_upload_ui.R")
source("frontend/Table/data_preview_ui.R")
source("frontend/Table/data_table_ui.R")
source("frontend/Table/data_upload_folder_ui.R")

# 5. Load Controllers (Server Logic) =========================================
source("controller/auth/login_controller.R")
source("controller/auth/forgot_controller.R")
source("controller/auth/reset_controller.R")
source("controller/auth/registration_controller.R")
source("controller/auth/summary_controller.R")
source("controller/MainLayout/content_router.R")
source("controller/MainLayout/main_layout_controller.R")
source("controller/Table/study_table_server.R")
source("controller/Table/data_upload_server.R")
source("controller/Table/data_preview_server.R")
source("controller/Table/data_table_server.R")
source("controller/Table/explorer_server.R")
source("controller/Table/ReadFileMiddleware.R")
source("controller/Table/data_upload_folder_server.R")


readRenviron("constants/.Renviron")

# 6. Assemble UI =============================================================
ui <- main_layout_ui()
  
# 7. Assemble Server =========================================================
server <- function(input, output, session) {
  # Auth Controllers
  login_controller(input, output, session)
  forgot_controller(input, output, session)
  reset_controller(input, output, session)
  registration_controller(input, output, session)
  summary_controller(input, output, session)
  
  # Main Layout Controller
  main_layout_controller(input, output, session)
  study_table_server("study_table")
  data_upload_folder_server("data_upload_folder")

  # Seed study structure on first boot
  observe({
    initialize_dm_structure("Study-001")
  })
}

# 8. Launch Application ======================================================
shinyApp(ui = ui, server = server)

