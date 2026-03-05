send_email <- function(to, subject, body) {
  
  tryCatch({
    
    # If body is already blastula object → send directly
    if (inherits(body, "blastula_message")) {
      
      blastula::smtp_send(
        body,
        to = to,
        from = Sys.getenv("SMTP_FROM"),
        subject = subject,
        credentials = blastula::creds_envvar(
          user = Sys.getenv("SMTP_FROM"),
          pass_envvar = "SMTP_PASS",
        host = Sys.getenv("SMTP_HOST"),
        port = as.integer(Sys.getenv("SMTP_PORT")),
        use_ssl = as.logical(Sys.getenv("SMTP_USE_SSL"))
        )
      )
      
    } else {
      
      # If body is raw HTML string
    email_msg <- blastula::compose_email(
        body = htmltools::HTML(body)
    )
    
    blastula::smtp_send(
      email_msg,
      to = to,
      from = Sys.getenv("SMTP_FROM"),
      subject = subject,
      credentials = blastula::creds_envvar(
        user = Sys.getenv("SMTP_FROM"),
        pass_envvar = "SMTP_PASS",
        host = Sys.getenv("SMTP_HOST"),
        port = as.integer(Sys.getenv("SMTP_PORT")),
        use_ssl = as.logical(Sys.getenv("SMTP_USE_SSL"))
      )
    )
    
    return(TRUE)

    }
    
  }, error = function(e) {
    message("Email sending failed: ", e$message)
    return(FALSE)
  })
}