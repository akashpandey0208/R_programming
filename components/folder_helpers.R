# ==============================================================================
# FOLDER HELPERS
# ==============================================================================
# Local filesystem utilities for the Data Upload explorer.
# All paths are relative to the app working directory (src/).
# Metadata (uploader, upload_time) is stored per-folder in _fileinfo.json.
# ==============================================================================

# Base path -------------------------------------------------------------------
get_studies_base_path <- function() {
  file.path("data", "studies")
}

# List all study names --------------------------------------------------------
list_studies <- function() {
  base <- get_studies_base_path()
  if (!dir.exists(base)) return(character(0))
  dirs <- list.dirs(base, recursive = FALSE, full.names = FALSE)
  dirs[nchar(dirs) > 0]
}

# Build the full path for a relative folder path inside a study ---------------
study_folder_path <- function(study, rel_path = "") {
  base <- get_studies_base_path()
  if (rel_path == "" || is.null(rel_path)) {
    file.path(base, study)
  } else {
    file.path(base, study, rel_path)
  }
}

# Initialize DM folder structure for a study ----------------------------------
initialize_dm_structure <- function(study_name) {
  base <- get_studies_base_path()
  if (!dir.exists(base)) dir.create(base, recursive = TRUE, showWarnings = FALSE)

  paths <- c(
    file.path(base, study_name),
    file.path(base, study_name, "DM"),
    file.path(base, study_name, "DM", "Documents"),
    file.path(base, study_name, "DM", "Documents", "Protocol"),
    file.path(base, study_name, "DM", "Documents", "DMP"),
    file.path(base, study_name, "DM", "Documents", "eCRFs"),
    file.path(base, study_name, "DM", "Raw Data"),
    file.path(base, study_name, "DM", "Raw Data", "EDC Data"),
    file.path(base, study_name, "DM", "Raw Data", "Vendor Data"),
    file.path(base, study_name, "DM", "Data"),
    file.path(base, study_name, "DM", "Programs"),
    file.path(base, study_name, "DM", "Programs", "Tables"),
    file.path(base, study_name, "DM", "Programs", "Listings"),
    file.path(base, study_name, "DM", "Programs", "Figures"),
    file.path(base, study_name, "DM", "Programs", "Edit Checks"),
    file.path(base, study_name, "DM", "Outputs"),
    file.path(base, study_name, "DM", "Outputs", "Tables"),
    file.path(base, study_name, "DM", "Outputs", "Listings"),
    file.path(base, study_name, "DM", "Outputs", "Figures"),
    file.path(base, study_name, "DM", "Outputs", "Edit Checks")
  )

  for (p in paths) {
    if (!dir.exists(p)) dir.create(p, recursive = TRUE, showWarnings = FALSE)
  }

  invisible(TRUE)
}

# List immediate child folders of a path -------------------------------------
get_folder_children <- function(full_path) {
  if (!dir.exists(full_path)) return(character(0))
  dirs <- list.dirs(full_path, recursive = FALSE, full.names = FALSE)
  dirs[nchar(dirs) > 0]
}

# List files in a folder (excluding _fileinfo.json) --------------------------
get_folder_files <- function(full_path) {
  if (!dir.exists(full_path)) return(character(0))
  files <- list.files(full_path, full.names = FALSE, recursive = FALSE)
  files <- files[files != "_fileinfo.json"]
  files[!dir.exists(file.path(full_path, files))]
}

# Read metadata from _fileinfo.json ------------------------------------------
read_file_metadata <- function(folder_path) {
  meta_path <- file.path(folder_path, "_fileinfo.json")
  if (!file.exists(meta_path)) return(list())
  tryCatch(
    jsonlite::fromJSON(meta_path, simplifyVector = FALSE),
    error = function(e) list()
  )
}

# Write/update metadata to _fileinfo.json ------------------------------------
write_file_metadata <- function(folder_path, metadata) {
  meta_path <- file.path(folder_path, "_fileinfo.json")
  jsonlite::write_json(metadata, meta_path, auto_unbox = TRUE, pretty = TRUE)
}

# Create a named subfolder (validates name) -----------------------------------
create_subfolder <- function(parent_path, name) {
  name <- trimws(name)
  if (nchar(name) == 0) return(list(ok = FALSE, msg = "Folder name cannot be empty."))
  if (grepl("[<>:\"/\\|?*]", name)) return(list(ok = FALSE, msg = "Folder name contains invalid characters."))
  new_path <- file.path(parent_path, name)
  if (dir.exists(new_path)) return(list(ok = FALSE, msg = paste0("A folder named '", name, "' already exists.")))
  dir.create(new_path, recursive = FALSE, showWarnings = FALSE)
  list(ok = TRUE, msg = "")
}

# Delete a folder and everything inside --------------------------------------
delete_folder_recursive <- function(full_path) {
  if (!dir.exists(full_path)) return(list(ok = FALSE, msg = "Folder not found."))
  unlink(full_path, recursive = TRUE)
  list(ok = TRUE, msg = "")
}

# Save an uploaded file to a destination folder with metadata ----------------
save_uploaded_file <- function(tmp_path, filename, dest_folder, uploader = "Unknown") {
  if (!dir.exists(dest_folder)) return(list(ok = FALSE, msg = "Destination folder does not exist."))
  dest <- file.path(dest_folder, filename)
  ok <- file.copy(tmp_path, dest, overwrite = TRUE)
  if (!ok) return(list(ok = FALSE, msg = "File copy failed."))
  meta <- read_file_metadata(dest_folder)
  meta[[filename]] <- list(
    uploader    = uploader,
    upload_time = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  )
  write_file_metadata(dest_folder, meta)
  list(ok = TRUE, msg = "")
}

# Delete a single file from a folder -----------------------------------------
delete_file_from_folder <- function(folder_path, filename) {
  file_path <- file.path(folder_path, filename)
  if (!file.exists(file_path)) return(list(ok = FALSE, msg = "File not found."))
  file.remove(file_path)
  meta <- read_file_metadata(folder_path)
  meta[[filename]] <- NULL
  write_file_metadata(folder_path, meta)
  list(ok = TRUE, msg = "")
}

# Format bytes to human-readable size ----------------------------------------
format_filesize <- function(bytes) {
  if (is.na(bytes) || bytes < 0) return("—")
  if (bytes < 1024)       return(paste0(bytes, " B"))
  if (bytes < 1024^2)     return(paste0(round(bytes / 1024, 1), " KB"))
  if (bytes < 1024^3)     return(paste0(round(bytes / 1024^2, 1), " MB"))
  paste0(round(bytes / 1024^3, 2), " GB")
}

# Detect file type badge label from filename extension -----------------------
file_type_badge <- function(filename) {
  ext <- toupper(tools::file_ext(filename))
  if (ext == "") return("FILE")
  ext
}

# Badge colour per file type -------------------------------------------------
badge_color <- function(ext) {
  switch(toupper(ext),
    "XPT"     = "#7c3aed",
    "SAS7BDAT" = "#0284c7",
    "SAS"     = "#0284c7",
    "CSV"     = "#16a34a",
    "XLSX"    = "#16a34a",
    "XLS"     = "#16a34a",
    "PDF"     = "#dc2626",
    "DOCX"    = "#2563eb",
    "DOC"     = "#2563eb",
    "RTF"     = "#d97706",
    "#6b7280"  # default gray
  )
}

# Convert relative folder path to a safe node ID for Shiny inputs ------------
path_to_node_id <- function(rel_path) {
  gsub("[^A-Za-z0-9]", "_", rel_path)
}
