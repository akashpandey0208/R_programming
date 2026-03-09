generate_temp_password <- function() {
  special_chars <- c("@", "#", "$", "!", "%", "&")
  alphanum <- c(LETTERS, letters, 0:9)
  
  special_part <- sample(special_chars, 1)
  
  random_part <- paste0(sample(alphanum, 7, replace = TRUE), collapse = "")
  paste0(random_part, special_part)
}
