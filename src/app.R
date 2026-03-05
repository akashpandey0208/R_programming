# ==============================================================================
# CLINICAL WEBAPP - MAIN ENTRY POINT
# ==============================================================================
# This is the main entry point for the Clinical Webapp Shiny application.
# It sources all required modules and initializes the application.
# ==============================================================================

# Set working directory to src folder for relative paths
# Working directory should be set to src folder when running

# 1. Load Libraries ==========================================================
source("libraries/LoadLibraries.R")

# 2. Load Constants ==========================================================
source("constants/AppConstants.R")
source("constants/EmailConstants.R")
readRenviron("constants/.Renviron")

# 3. Load Components =========================================================
source("components/AuthHelpers.R")
source("components/EmailModule.R")
source("components/EmailTemplates.R")
source("components/TempPasswordService.R")
source("components/UserHelpers.R")

# 4. Load Frontend (UI) ======================================================
source("frontend/admin/AdminTableUi.R")
source("frontend/auth/ForgotPasswordUi.R")
source("frontend/auth/LoginUi.R")
source("frontend/auth/ResetUi.R")
source("frontend/registration/RegistrationUi.R")
source("frontend/registration/SummaryUi.R")
source("frontend/mainLayout/MainLayoutUi.R")
source("frontend/mainLayout/LayoutComponents.R")

# 5. Load Controllers (Server Logic) =========================================
source("controller/admin/AdminTableServer.R")
source("controller/auth/ForgotPasswordController.R")
source("controller/auth/LoginController.R")
source("controller/auth/ResetController.R")
source("controller/registration/RegistrationController.R")
source("controller/registration/SummaryController.R")
source("controller/mainLayout/MainLayoutController.R")
source("controller/mainLayout/ContentRouter.R")


# 7. Integrations =============================================================
source("integrations/SendEmailService.R")



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
}

# 8. Launch Application ======================================================
shinyApp(ui = ui, server = server)

