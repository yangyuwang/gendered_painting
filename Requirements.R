# List of required packages
required_packages <- c(
  "broom.mixed",   # For tidying mixed-effect model outputs.
  "jsonlite",      # For reading JSON data.
  "tidyverse",     # For data manipulation and visualization.
  "lme4",          # For fitting mixed-effect models.
  "stargazer",     # For formatting regression outputs.
  "ggplot2",       # For plotting.
  "cowplot"        # For combining multiple ggplot figures.
)

# Function to check and install missing packages
install_if_missing <- function(pkg){
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}

# Apply function for each required package
invisible(sapply(required_packages, install_if_missing))

# Optional: Print a message
message("All required packages are installed and loaded.")
