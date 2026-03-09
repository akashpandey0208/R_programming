send_email <- function(to, subject, body) {
  
  tryCatch({
    
    email_msg <- blastula::compose_email(
      body = htmltools::HTML(as.character(body))
    )
    
    blastula::smtp_send(
      email_msg,
      to = to,
      from = Sys.getenv("SMTP_FROM"),
      subject = subject,
      credentials = blastula::creds_envvar(
        user = Sys.getenv("SMTP_FROM"),
        pass_envvar = "SMTP_PASS",
        host = SMTP_HOST,
        port = SMTP_PORT,
        use_ssl = SMTP_USE_SSL
      )
    )
    
    return(TRUE)
    
  }, error = function(e) {
    message("Email sending failed: ", e$message)
    return(FALSE)
  })
}