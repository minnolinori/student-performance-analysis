# =============================================================================
# Student Performance Analysis - Package Requirements
# =============================================================================

# List of required packages
required_packages <- c(
  "tidyverse",    # Data manipulation and ggplot2
  "corrplot",     # Correlation plots  
  "plotly",       # Interactive plots
  "car",          # ANOVA and regression diagnostics
  "broom",        # Tidy statistical outputs
  "DT"            # Interactive data tables
)

# Function to install packages if not already installed
install_if_missing <- function(packages) {
  for (pkg in packages) {
    if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
      cat("Installing package:", pkg, "\n")
      install.packages(pkg, dependencies = TRUE)
      library(pkg, character.only = TRUE)
    } else {
      cat("✓ Package", pkg, "already installed\n")
    }
  }
}

# Install missing packages
cat("Checking and installing required packages...\n")
cat("==========================================\n")
install_if_missing(required_packages)

# Test loading all packages
cat("\nTesting package loading...\n")
cat("==========================\n")
success <- TRUE
for (pkg in required_packages) {
  if (require(pkg, character.only = TRUE, quietly = TRUE)) {
    cat("✓", pkg, "loaded successfully\n")
  } else {
    cat("✗", pkg, "failed to load\n")
    success <- FALSE
  }
}

if (success) {
  cat("\n🎉 All packages installed and working correctly!\n")
  cat("You can now run the main analysis script.\n")
} else {
  cat("\n⚠️  Some packages failed to load. Please check the errors above.\n")
}

# Display R version info
cat("\nR Version Information:\n")
cat("======================\n")
print(R.version.string)
cat("Platform:", R.version$platform, "\n")