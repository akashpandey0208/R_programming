library(blastula)

source("SendEmailService.R")
source("EmailTemplates.R")
source("TempPasswordService.R")
source("EmailService.R")

# ==========================================================
# (A) USER SUBMITS REQUEST
# -> send 2 mails: User + Approver
# ==========================================================
user_submit_request_service <- function(
    requester_name,
    requester_email,
    approver1_email,
    web_panels,
    justification
) {
  
  requester_name  <- trimws(requester_name)
  requester_email <- trimws(requester_email)
  approver1_email <- trimws(approver1_email)
  justification   <- trimws(justification)
  
  # ---- Mail 1: To User
  send_email(
    to = requester_email,
    subject = paste0("Role Change Request Created - ", requester_name),
    body = email_user_request_created(
      requester_name  = requester_name,
      requester_email = requester_email,
      web_panels      = web_panels,
      justification   = justification
    )
  )
  
  # ---- Mail 2: To Approver
  send_email(
    to = approver1_email,
    subject = paste0("Approval Needed (Level 1) - ", requester_name),
    body = email_approver1_approval_needed(
      requester_name  = requester_name,
      requester_email = requester_email,
      web_panels      = web_panels,
      justification   = justification
    )
  )
  
  return(invisible(TRUE))
}

# ==========================================================
# (B) APPROVER DECISION (LEVEL 1)
#
# Approver Accepts -> 2 mails: User + Admin
# Approver Rejects -> 1 mail: User (with comments)
# ==========================================================
approver1_decision_service <- function(
    requester_name,
    requester_email,
    approver1_email,
    admin_email,
    web_panels,
    justification,
    decision,
    comments = ""
) {
  
  requester_name  <- trimws(requester_name)
  requester_email <- trimws(requester_email)
  approver1_email <- trimws(approver1_email)
  admin_email     <- trimws(admin_email)
  
  decision        <- trimws(decision)
  comments        <- trimws(comments)
  justification   <- trimws(justification)
  
  # ==========================================================
  # CASE 1: REJECTED (Level 1)
  # ==========================================================
  if (tolower(decision) == "rejected") {
    
    # ---- Mail to User only
    send_email(
      to = requester_email,
      subject = paste0("Role Change Request Rejected (Level 1) - ", requester_name),
      body = email_user_rejected_level1(
        requester_name = requester_name,
        web_panels     = web_panels,
        comments       = comments
      )
    )
    
    return(invisible(TRUE))
  }
  
  # ==========================================================
  # CASE 2: ACCEPTED (Level 1)
  # -> send 2 mails: User + Admin
  # ==========================================================
  
  # ---- Mail to User
  send_email(
    to = requester_email,
    subject = paste0("Role Change Request Approved (Level 1) - ", requester_name),
    body = email_user_approved_level1(
      requester_name = requester_name,
      web_panels     = web_panels
    )
  )
  
  # ---- Mail to Admin
  send_email(
    to = admin_email,
    subject = paste0("Final Approval Needed (Level 2) - ", requester_name),
    body = email_admin_final_approval_needed(
      requester_name  = requester_name,
      requester_email = requester_email,
      approver1_email = approver1_email,
      web_panels      = web_panels,
      justification   = justification,
      comments        = comments
    )
  )
  
  return(invisible(TRUE))
}

# ==========================================================
# (C) ADMIN FINAL DECISION (LEVEL 2)
#
# Admin Accepts -> 3 mails:
#    1) Approved mail to User
#    2) Reset Password mail to User
#    3) Final decision mail to Approver
#
# Admin Rejects  -> 2 mails:
#    1) Rejected mail to User (with comments)
#    2) Final decision mail to Approver
# ==========================================================
admin_decision_service <- function(
    requester_name,
    requester_email,
    approver1_email,
    admin_email,
    web_panels,
    justification,
    decision,
    comments = ""
) {
  
  requester_name  <- trimws(requester_name)
  requester_email <- trimws(requester_email)
  approver1_email <- trimws(approver1_email)
  admin_email     <- trimws(admin_email)
  
  decision        <- trimws(decision)
  comments        <- trimws(comments)
  justification   <- trimws(justification)
  
  # ==========================================================
  # CASE 1: REJECTED (Final)
  # ==========================================================
  if (tolower(decision) == "rejected") {
    
    # ---- Mail 1: To User
    send_email(
      to = requester_email,
      subject = paste0("Role Change Request Rejected (Final) - ", requester_name),
      body = email_user_rejected_final(
        requester_name = requester_name,
        web_panels     = web_panels,
        justification  = justification,
        comments       = comments
      )
    )
    
    # ---- Mail 2: To Approver
    send_email(
      to = approver1_email,
      subject = paste0("Final Decision: Rejected - ", requester_name),
      body = email_approver_final_decision(
        requester_name  = requester_name,
        requester_email = requester_email,
        web_panels      = web_panels,
        justification   = justification,
        decision        = "Rejected",
        comments        = comments
      )
    )
    
    return(invisible(TRUE))
  }
  
  # ==========================================================
  # CASE 2: ACCEPTED (Final)
  # ==========================================================
  
  # ---- Mail 1: Approved to User
  send_email(
    to = requester_email,
    subject = paste0("Role Change Request Approved (Final) - ", requester_name),
    body = email_user_approved_final(
      requester_name = requester_name,
      web_panels     = web_panels,
      justification  = justification
    )
  )
  
  # ---- Mail 2: Reset Password to User
  temp_pwd =  generate_temp_password()
  send_reset_password_mail(requester_email, requester_name, temp_pwd)
  
  # ---- Mail 3: Final Decision to Approver
  send_email(
    to = approver1_email,
    subject = paste0("Final Decision: Approved - ", requester_name),
    body = email_approver_final_decision(
      requester_name  = requester_name,
      requester_email = requester_email,
      web_panels      = web_panels,
      justification   = justification,
      decision        = "Approved",
      comments        = comments
    )
  )
  
  return(invisible(TRUE))
}
