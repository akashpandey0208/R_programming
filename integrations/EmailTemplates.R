library(blastula)

fmt_panels <- function(web_panels) {
  if (is.null(web_panels) || length(web_panels) == 0) return("")
  paste(web_panels, collapse = ", ")
}

email_user_request_created <- function(requester_name, requester_email, web_panels, justification) {
  
  panels_txt <- fmt_panels(web_panels)
  
  compose_email(
    body = md(paste0(
      "## Role Change Request Created\n\n",
      "Hi ", requester_name, ",\n\n",
      "Your role change request is generated successfully with the following details:\n\n",
      "- **Name:** ", requester_name, "\n",
      "- **Email:** ", requester_email, "\n",
      "- **Web App Panel Requested:** ", panels_txt, "\n\n",
      "### Justification\n\n",
      justification, "\n\n",
      "**Status:** Under Level 1 Review\n\n",
      "Sincerely,  \n",
      "Clinical project group"
    ))
  )
}

email_approver1_approval_needed <- function(requester_name, requester_email, web_panels, justification) {
  
  panels_txt <- fmt_panels(web_panels)
  
  compose_email(
    body = md(paste0(
      "## Role Change Request - Approval Needed (Level 1)\n\n",
      "Hello Approver,\n\n",
      "A role change request has been submitted and requires your review.\n\n",
      "- **Name:** ", requester_name, "\n",
      "- **Requester Email:** ", requester_email, "\n",
      "- **Web App Panel Requested:** ", panels_txt, "\n\n",
      "### Justification\n\n",
      justification, "\n\n",
      "Please review and provide your decision.\n\n",
      "Sincerely,  \n",
      "Clinical project group"
    ))
  )
}

email_user_rejected_level1 <- function(requester_name, web_panels, comments) {
  
  panels_txt <- fmt_panels(web_panels)
  
  compose_email(
    body = md(paste0(
      "## Role Change Request - Rejected (Level 1)\n\n",
      "Hi ", requester_name, ",\n\n",
      "Your role change request has been **Rejected** at Level 1.\n\n",
      "- **Web App Panel Requested:** ", panels_txt, "\n\n",
      if (comments != "") paste0("### Comments\n\n", comments, "\n\n") else "",
      "Sincerely,  \n",
      "Clinical project group"
    ))
  )
}

email_user_approved_level1 <- function(requester_name, web_panels) {
  
  panels_txt <- fmt_panels(web_panels)
  
  compose_email(
    body = md(paste0(
      "## Role Change Request - Approved (Level 1)\n\n",
      "Hi ", requester_name, ",\n\n",
      "Your role change request has been **Approved** at Level 1.\n\n",
      "- **Web App Panel Requested:** ", panels_txt, "\n\n",
      "**Status:** Under Level 2 Review\n\n",
      "Sincerely,  \n",
      "Clinical project group"
    ))
  )
}

email_admin_final_approval_needed <- function(
    requester_name,
    requester_email,
    approver1_email,
    web_panels,
    justification,
    comments
)
{
  
  panels_txt <- fmt_panels(web_panels)
  
  compose_email(
    body = md(paste0(
      "## Role Change Request - Final Approval Needed (Level 2)\n\n",
      "Hello,\n\n",
      "A role change request has been approved at Level 1 and requires your final decision.\n\n",
      "- **Name:** ", requester_name, "\n",
      "- **Requester Email:** ", requester_email, "\n",
      "- **Web App Panel Requested:** ", panels_txt, "\n\n",
      "### Justification\n\n",
      justification, "\n\n",
      "- **Level 1 Decision:** Approved\n\n",
      if (comments != "") paste0("### Level 1 Comments\n\n", comments, "\n\n") else "",
      "Please review and provide final approval.\n\n",
      "Sincerely,  \n",
      "Clinical project group"
    ))
  )
}

email_user_rejected_final <- function(requester_name, web_panels, justification, comments) {
  
  panels_txt <- fmt_panels(web_panels)
  
  compose_email(
    body = md(paste0(
      "## Role Change Request - Rejected (Final)\n\n",
      "Hi ", requester_name, ",\n\n",
      "Your role change request has been **Rejected** (Final Decision).\n\n",
      "- **Web App Panel Requested:** ", panels_txt, "\n\n",
      "### Justification\n\n",
      justification, "\n\n",
      if (comments != "") paste0("### Comments\n\n", comments, "\n\n") else "",
      "**Status:** Rejected (Final)\n\n",
      "Sincerely,  \n",
      "Clinical project group"
    ))
  )
}

email_user_approved_final <- function(requester_name, web_panels, justification) {
  
  panels_txt <- fmt_panels(web_panels)
  
  compose_email(
    body = md(paste0(
      "## Role Change Request - Approved (Final)\n\n",
      "Hi ", requester_name, ",\n\n",
      "Your role change request has been **Approved** (Final Decision).\n\n",
      "- **Web App Panel Requested:** ", panels_txt, "\n\n",
      "### Justification\n\n",
      justification, "\n\n",
      "**Status:** Completed\n\n",
      "**Note:** You will receive another email for password reset.\n\n",
      "Sincerely,  \n",
      "Clinical project group"
    ))
  )
}

email_user_reset_password <- function(requester_name) {
  
  compose_email(
    body = md(paste0(
      "## Password Reset Required\n\n",
      "Hi ", requester_name, ",\n\n",
      "Your role has been updated successfully.\n\n",
      "**Please reset your password before your next login.**\n\n",
      "Sincerely,  \n",
      "Clinical project group"
    ))
  )
}

email_approver_final_decision <- function(requester_name, requester_email, web_panels, justification, decision, comments) {
  
  panels_txt <- fmt_panels(web_panels)
  
  compose_email(
    body = md(paste0(
      "## Role Change Request - Final Decision\n\n",
      "Hello Approver,\n\n",
      "The role change request has been completed with the following final decision:\n\n",
      "- **Requester Name:** ", requester_name, "\n",
      "- **Requester Email:** ", requester_email, "\n",
      "- **Web App Panel Requested:** ", panels_txt, "\n\n",
      "### Justification\n\n",
      justification, "\n\n",
      "- **Final Decision:** **", decision, "**\n\n",
      if (comments != "") paste0("### Final Comments\n\n", comments, "\n\n") else "",
      "Sincerely,  \n",
      "Clinical project group"
    ))
  )
}
