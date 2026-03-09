library(glue)
library(blastula)
library(htmltools)
source("../operations/SendEmailService.R")

send_admin_approval_mail <- function(user_name, email, department, organization, panel_requested, study_title, admin_email, admin_name) {
  
  base_url <- Sys.getenv("APP_BASE_URL")
  dashboard_link <- glue::glue(
    "{base_url}/admin-dashboard"
  )
  html_body <- glue::glue("
<!DOCTYPE html>
<html>
<body style='margin:0;padding:0;background-color:#f3f4f6;font-family:Arial,Helvetica,sans-serif;'>

  <div style='max-width:700px;margin:20px auto;'>

    <!-- Header -->
    <div style='background-color:#0f172a;color:white;
                padding:10px;text-align:center;
                font-weight:bold;font-size:16px;'>
      New User Registration
    </div>

    <!-- Container -->
    <div style='background:#ffffff;
                border:1px solid #d1d5db;
                padding:20px;'>

      <p>Hello <b>{admin_name}</b>,</p>

      <p>
        A new user has registered on the website and is awaiting your approval.
      </p>

      <!-- Info Box -->
      <div style='background:#f9fafb;
                  border:1px solid #d1d5db;
                  padding:15px;
                  margin:15px 0;'>

        <b>Full Name:</b> {user_name}<br>
        <b>Work Email:</b> {email}<br>
        <b>Department</b> {department}<br>
        <b>Organization:</b> {organization}<br>
        <b>Panel Requested:</b> {panel_requested}<br>
        <b>Study Title:</b> {study_title}

      </div>

      <p>
        Please review the registration from your 
        <a href='{dashboard_link}' style='color:#2563eb; text-decoration:underline;'>
          admin dashboard
        </a>.
      </p>

    </div>
  </div>
</body>
</html>
")
  
  
  send_email(
    to = admin_email,
    subject = "New user approval request",
    body = html_body
  )
}

  
send_reset_password_mail <- function(user_email, user_name, temp_password) {
  
  base_url <- Sys.getenv("APP_BASE_URL")
  
  encoded_email <- utils::URLencode(user_email, reserved = TRUE)
  
  reset_link <- glue::glue(
    "{base_url}/reset-password?email={encoded_email}"
  )
  
  html_body <- glue::glue("
<!DOCTYPE html>
<html>
<body style='margin:0;padding:0;background-color:#f3f4f6;font-family:Arial,Helvetica,sans-serif;'>

  <div style='max-width:700px;margin:20px auto;'>

    <!-- Header -->
    <div style='background-color:#0f172a;color:white;
                padding:10px;text-align:center;
                font-weight:bold;font-size:16px;'>
      Reset Password
    </div>

    <!-- Container -->
    <div style='background:#ffffff;
                border:1px solid #d1d5db;
                padding:20px;'>

      <p>Hello <b>{user_name}</b>,</p>

      <p>
        A temporary password has been generated for your account.
        Please use it to reset your password.
      </p>

      <!-- Info Box -->
      <div style='background:#f9fafb;
                  border:1px solid #d1d5db;
                  padding:15px;
                  margin:15px 0;'>

        <b>Temporary Password:</b> {temp_password}

      </div>

      <div style='margin:20px 0;'>
        <a href='{reset_link}'
           style='padding:8px 16px;
                  background:#2563eb;
                  color:white;
                  text-decoration:none;
                  border-radius:3px;
                  display:inline-block;'>
          Reset Password
        </a>
      </div>
    </div>
  </div>
</body>
</html>
")
  
  
  
  send_email(
    to = user_email,
    subject = "Reset Password | Clinical Webapp",
    body = html_body
  )
  
}


