get_initials <- function(full_name) {
  full_name <- trimws(full_name)
  parts <- unlist(strsplit(full_name, "\\s+"))
  
  if(length(parts) == 1) {
    initials <- substr(parts[1], 1, 1)
  } 
  else {
    first <- substr(parts[1], 1, 1)
    last  <- substr(parts[length(parts)], 1, 1)
    initials <- paste0(first, last)
  }
  
  toupper(initials)
}