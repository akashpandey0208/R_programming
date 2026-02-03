# ============================================================================
# Global Variables, Libraries, and Helper Functions
# ============================================================================

# Load required libraries
library(shiny)
library(shinyjs)
library(DT)
library(ggplot2)
library(plotly)
library(dplyr)

# ============================================================================
# Source Module Files
# ============================================================================

source("modules/cdisc_converter.R")
source("modules/data_module.R")
source("modules/table_module.R")
source("modules/graphs_module.R")
source("modules/reports_module.R")
source("modules/help_module.R")

# ============================================================================
# Helper Functions
# ============================================================================

# Sanitize incoming dataframes: normalize names and drop empty/auto-generated columns
sanitize_df <- function(df) {
  if (is.null(df)) return(df)
  # ensure valid, unique names
  names(df) <- make.names(names(df), unique = TRUE)
  # drop columns with empty names or R-auto-generated '...1' style names
  bad <- (names(df) == "") | grepl("^\\.\\.\\.", names(df))
  if (any(bad)) df <- df[, !bad, drop = FALSE]
  df
}

# Get column information (roles) for a dataset
get_col_info <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    return(list(all = character(0), numeric = character(0), categorical = character(0), date = character(0)))
  }
  
  all_cols <- names(df)
  nums <- names(df)[vapply(df, is.numeric, logical(1))]
  cats <- names(df)[vapply(df, function(x) is.factor(x) || is.character(x), logical(1))]
  
  # simple date-like detection using common formats on a sample
  detect_date_like <- function(x) {
    if (!is.character(x) && !is.factor(x)) x <- as.character(x)
    vals <- head(na.omit(x), 50)
    if (length(vals) < 3) return(FALSE)
    fmts <- c("%Y-%m-%d", "%d-%m-%Y", "%m/%d/%Y", "%d/%m/%Y", "%Y/%m/%d")
    best_ok <- 0
    for (f in fmts) {
      parsed <- suppressWarnings(as.Date(vals, format = f))
      ok <- sum(!is.na(parsed))
      if (ok > best_ok) best_ok <- ok
    }
    return((best_ok / max(1, length(vals))) > 0.6)
  }
  
  date_like <- vapply(df, detect_date_like, logical(1))
  date_cols <- names(df)[date_like]
  
  if (length(nums) == 0) nums <- all_cols
  # treat date columns as not categorical for selection convenience
  cats <- setdiff(cats, date_cols)
  
  list(all = all_cols, numeric = nums, categorical = cats, date = date_cols)
}

# Safe numeric coercion
safe_num <- function(x) {
  if (is.numeric(x)) return(x)
  conv <- suppressWarnings(as.numeric(as.character(x)))
  if (all(is.na(conv))) return(NULL)
  na_frac <- sum(is.na(conv)) / max(1, length(conv))
  if (na_frac > 0.5) return(NULL) else return(conv)
}

# Safe date coercion
safe_date <- function(x) {
  if (inherits(x, "Date") || inherits(x, "POSIXt")) return(as.Date(x))
  s <- as.character(x)
  fmts <- c("%Y-%m-%d", "%d-%m-%Y", "%m/%d/%Y", "%d/%m/%Y", "%Y/%m/%d")
  for (f in fmts) {
    conv <- suppressWarnings(as.Date(s, format = f))
    if (sum(!is.na(conv)) / max(1, length(conv)) > 0.6) return(conv)
  }
  return(NULL)
}
