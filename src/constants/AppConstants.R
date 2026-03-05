# ==============================================================================
# APPLICATION CONSTANTS
# ==============================================================================
# Global configuration values for the Clinical Webapp.
# ==============================================================================

# Application Metadata =======================================================
APP_TITLE <- "Clinical Webapp"
APP_VERSION <- "1.0.0"

# Sidebar Menu Configuration =================================================
MENU_ITEMS <- list(
  list(id = "clinical", label = "Clinical", active = FALSE),
  list(id = "data_management", label = c("Data", "Management"), active = FALSE),
  list(id = "blinded_biometrics", label = c("Blinded", "Biometrics"), active = FALSE),
  list(id = "medical_monitor", label = "Medical Monitor", active = FALSE),
  list(id = "rwe", label = "RWE", active = FALSE),
  list(id = "dmc", label = "DMC", active = FALSE),
  list(id = "client", label = "Client", active = FALSE),
  list(id = "admin", label = "Admin", active = TRUE)
)

# Default Landing Page =======================================================
DEFAULT_MODULE <- "admin"
