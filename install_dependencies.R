# Installation script for Clinical Webapp dependencies
# Run this script before launching the app

# Check and install required packages
packages <- c("shiny", "shinyjs", "DT", "ggplot2", "plotly", "dplyr", 
              "readxl", "haven")  # Added readxl for Excel, haven for XPT

install_if_missing <- function(pkg) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    cat(sprintf("Installing %s...\n", pkg))
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
    cat(sprintf("✓ %s installed successfully\n", pkg))
  } else {
    cat(sprintf("✓ %s already installed\n", pkg))
  }
}

cat("Checking and installing dependencies for Clinical Webapp...\n\n")

for (pkg in packages) {
  install_if_missing(pkg)
}

cat("\n✓ All dependencies are installed!\n")
cat("\nYou can now run the app with:\n")
cat("  shiny::runApp()\n")
