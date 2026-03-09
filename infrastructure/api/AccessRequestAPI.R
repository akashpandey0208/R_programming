library(plumber)
library(DBI)
library(RPostgres)
library(jsonlite)
library(jose)
library(bcrypt)
source("../../components/temp_passwordhelper.R")
source("../../components/EmailModule.R")

Sys.getenv()
# ---- DB Connection ----
con <- dbConnect(
  RPostgres::Postgres(),
  dbname = "clinical_db",
  host = "localhost",
  port = 5432,
  user = "postgres",
  password = "Vishwa@123"
)

# Secret key must be raw
secret <- charToRaw(Sys.getenv("JWT_SECRET"))

# ==============================
# FILTER: JWT authentication
# ==============================
# Skip filter for endpoints that don't require access token
#* @filter auth
function(req, res){
  if (req$PATH_INFO %in% c("/login", "/refresh", "/logout","/submit_request","/reset_password","/forgot_password")) {
    forward()
    return()
  }
  
  auth_header <- req$HTTP_AUTHORIZATION
  if (is.null(auth_header)){
    res$status <- 401
    return(list(error="Missing token"))
  }
  
  token <- sub("Bearer ", "", auth_header)
  
  claims <- tryCatch({
    jwt_decode_hmac(token, secret)
  }, error=function(e){
    res$status <- 401
    return(list(error="Invalid token"))
  })
  
  # Explicit expiry check
  cat("DEBUG FILTER: email =", claims$email, " exp =", claims$exp, " Sys.time() =", as.numeric(Sys.time()), "\n")
  if (!is.null(claims$exp) && as.numeric(Sys.time()) > claims$exp) {
    res$status <- 401
    return(list(error="Access token expired"))
  }
  
  req$user <- claims
  forward()
}
# ==============================
# LOGIN endpoint
# ==============================
#* @post /login
function(req, res){
  body <- fromJSON(req$postBody)
  email <- body$email
  password <- body$password
  
  user <- dbGetQuery(con, "SELECT * FROM user_login WHERE email=$1", params=list(email))
  
  # Invalid credentials check
  if (nrow(user) == 0 || !checkpw(password, user$password[1])) {
    res$status <- 401
    return(list(error="Invalid credentials"))
  }
  
  # If account is flagged for reset, force redirect
  if (user$is_reset[1]) {
    res$status <- 403
    return(list(
      status="reset_required",
      message="Password reset required. Redirecting to reset page..."
    ))
  }
  
  # Access token (short-lived)
  access_claims <- jwt_claim(
    email = email,
    exp = as.numeric(Sys.time()) + 3600
  )
  access_token <- jwt_encode_hmac(access_claims, secret)
  
  # Refresh token (long-lived)
  refresh_claims <- jwt_claim(
    email = email,
    type = "refresh",
    exp = as.numeric(Sys.time()) + (3*24*3600)
  )
  refresh_token <- jwt_encode_hmac(refresh_claims, secret)
  
  # Save refresh token in user_login table (force POSIXct)
  dbExecute(con,
            "UPDATE user_login SET refresh_token=$1, refresh_exp=$2 WHERE email=$3",
            params = list(refresh_token, as.POSIXct(Sys.time() + (7*24*3600)), email)
  )
  
  cat("DEBUG LOGIN: email =", email, "\n")
  cat("DEBUG LOGIN: refresh_token =", substr(refresh_token, 1, 40), "... \n")
  cat("DEBUG LOGIN: refresh_exp =", as.POSIXct(Sys.time() + (7*24*3600)), "\n")
  
  list(
    access_token = access_token,
    refresh_token = refresh_token
  )
}

# ==============================
# REFRESH endpoint
# ==============================
#* @post /refresh
function(req, res){
  body <- fromJSON(req$postBody)
  refresh_token <- body$refresh_token
  
  claims <- tryCatch({
    jwt_decode_hmac(refresh_token, secret)
  }, error=function(e){
    res$status <- 401
    return(list(error="Invalid refresh token"))
  })
  
  if (is.null(claims$type) || claims$type != "refresh") {
    res$status <- 401
    return(list(error="Not a refresh token"))
  }
  
  user <- dbGetQuery(con, "SELECT * FROM user_login WHERE email=$1", params=list(claims$email))
  
  # Handle both numeric and timestamp cases
  val <- user$refresh_exp[1]
  refresh_exp <- tryCatch({
    if (is.numeric(val)) {
      as.POSIXct(val, origin="1970-01-01", tz="UTC")
    } else {
      as.POSIXct(val, tz="UTC")
    }
  }, error=function(e) NA)
  
  # Debug logging
  cat("DEBUG REFRESH: email =", claims$email, "\n")
  cat("DEBUG REFRESH: incoming refresh_token =", substr(refresh_token, 1, 40), "... \n")
  cat("DEBUG REFRESH: stored refresh_token =", substr(user$refresh_token[1], 1, 40), "... \n")
  cat("DEBUG REFRESH: Sys.time() =", Sys.time(), "\n")
  cat("DEBUG REFRESH: refresh_exp =", refresh_exp, "\n")
  
  if (nrow(user) == 0 || user$refresh_token[1] != refresh_token || is.na(refresh_exp) || Sys.time() > refresh_exp) {
    res$status <- 401
    return(list(error="Refresh token expired or invalid"))
  }
  
  # Issue new access token
  new_claims <- jwt_claim(
    email = claims$email,
    exp = as.numeric(Sys.time()) + 3600
  )
  new_access_token <- jwt_encode_hmac(new_claims, secret)
  
  # Rotate refresh token
  new_refresh_claims <- jwt_claim(
    email = claims$email,
    type = "refresh",
    exp = as.numeric(Sys.time()) + (7*24*3600)
  )
  new_refresh_token <- jwt_encode_hmac(new_refresh_claims, secret)
  
  dbExecute(con,
            "UPDATE user_login SET refresh_token=$1, refresh_exp=$2 WHERE email=$3",
            params = list(new_refresh_token, as.POSIXct(Sys.time() + (7*24*3600)), claims$email)
  )
  
  # Debug after update
  test <- dbGetQuery(con, "SELECT refresh_token, refresh_exp FROM user_login WHERE email=$1", params=list(claims$email))
  cat("DEBUG REFRESH: updated refresh_token =", substr(test$refresh_token[1], 1, 40), "... \n")
  cat("DEBUG REFRESH: updated refresh_exp =", test$refresh_exp[1], "\n")
  
  list(
    access_token = new_access_token,
    refresh_token = new_refresh_token
  )
}

# ==============================
# SECURE endpoint
# ==============================
#* @get /secure_data
function(req){
  list(message="You accessed secure data!", user=req$user$email)
}

# ==============================
# LOGOUT endpoint
# ==============================
#* @post /logout
function(req, res){
  body <- fromJSON(req$postBody)
  email <- body$email
  
  dbExecute(con,
            "UPDATE user_login SET refresh_token=NULL, refresh_exp=NULL WHERE email=$1",
            params = list(email)
  )
  
  cat("DEBUG LOGOUT: email =", email, " tokens cleared\n")
  
  list(message="Logged out successfully")
}

# ==========================================================
# SUBMIT ACCESS REQUEST API
# ==========================================================
#* Submit Access Request
#* @post /submit_request
#* @serializer json
function(req, res) {
  
  # Manually parse JSON body (this ALWAYS works)
  body <- jsonlite::fromJSON(req$postBody)
  
  print("REQUEST BODY:")
  print(body)
  
  tryCatch({
    
    dbExecute(con, "
      INSERT INTO access_requests (
        full_name,
        email,
        job_title,
        department,
        organization,
        phone_number,
        office_location,
        employment_type,
        roles_requested,
        study_name_protocol_number,
        sponsor,
        therapeutic_areas,
        request_comments
      )
      VALUES ($1,$2,$3,$4,$5,$6,$7,$8,$9::jsonb,$10,$11,$12,$13)
    ",
              params = list(
                as.character(body$full_name),
                as.character(body$email),
                as.character(body$job_title),
                as.character(body$department),
                as.character(body$organization),
                as.character(body$phone_number),
                as.character(body$office_location),
                as.character(body$employment_type),
                if (is.null(body$roles_requested) || length(body$roles_requested) == 0)
                  "[]"
                else
                  jsonlite::toJSON(body$roles_requested, auto_unbox = FALSE),
                as.character(body$study_name_protocol_number),
                if (is.null(body$sponsor)) NA_character_ else as.character(body$sponsor),
                if (is.null(body$therapeutic_areas)) NA_character_ else as.character(body$therapeutic_areas),
                if (is.null(body$request_comments)) NA_character_ else as.character(body$request_comments[1])
              ))
    
    list(status = "success")
    
  }, error = function(e) {
    
    print("DATABASE ERROR:")
    print(e$message)
    
    res$status <- 500
    list(status = "error", message = e$message)
  })
}

# ==========================================================
# PASSWORD RESET API
# ==========================================================
#* Reset password using temporary password
#* @post /reset_password
#* @serializer json
function(req, res){
  
  # ---- Parse JSON body ----
  body <- jsonlite::fromJSON(req$postBody)
  
  email            <- as.character(body$email)
  temp_password    <- as.character(body$temp_password)
  new_password     <- as.character(body$new_password)
  confirm_password <- as.character(body$confirm_password)
  
  cat("DEBUG: email =", email, "\n")
  
  # ---- Safe validation helper ----
  is_invalid <- function(x) {
    if (is.null(x)) return(TRUE)
    if (is.na(x)) return(TRUE)
    if (!nzchar(x)) return(TRUE)
    return(FALSE)
  }
  
  # ---- Basic validation ----
  if (is_invalid(email)) {
    res$status <- 400
    return(list(status="error", message="Email missing"))
  }
  if (is_invalid(temp_password)) {
    res$status <- 400
    return(list(status="error", message="Temporary password missing"))
  }
  if (is_invalid(new_password)) {
    res$status <- 400
    return(list(status="error", message="New password missing"))
  }
  if (new_password != confirm_password) {
    res$status <- 400
    return(list(status="error", message="Passwords do not match"))
  }
  
  # ---- Fetch user ----
  user <- dbGetQuery(con, "
      SELECT password, is_active, is_reset_required, updated_on
      FROM user_login
      WHERE email = $1
  ", params = list(email))
  
  if (nrow(user) == 0){
    res$status <- 404
    return(list(status="error", message="User not found"))
  }
  
  db_password <- as.character(user$password[1])
  
  if (!user$is_active[1]){
    res$status <- 403
    return(list(status="error", message="Account inactive"))
  }
  
  if (!user$is_reset_required[1]){
    res$status <- 400
    return(list(status="error", message="No reset request pending"))
  }
  
  if (is_invalid(db_password)) {
    res$status <- 400
    return(list(status="error", message="Temporary password not generated"))
  }
  
  # ---- Expiry check ----
  expiry_time <- as.POSIXct(user$updated_on[1]) + 24*60*60
  if (Sys.time() > expiry_time){
    res$status <- 400
    return(list(status="error", message="Temporary password expired"))
  }
  
  # ---- Temp password validation ----
  if (!bcrypt::checkpw(temp_password, db_password)) {
    res$status <- 401
    return(list(status="error", message="Invalid temporary password"))
  }
  
  # ---- Update password ----
  new_hash <- bcrypt::hashpw(as.character(new_password))
  
  dbExecute(con, "
      UPDATE user_login
      SET password = $1,
          is_reset_required = FALSE,
          updated_on = NOW()
      WHERE email = $2
  ", params=list(new_hash, email))
  
  return(list(status="success", message="Password reset successful"))
}


# ==========================================================
# FORGOT PASSWORD API
# ==========================================================
#* Forgot Password
#* @post /forgot_password
#* @serializer json
function(req, res) {
  body <- jsonlite::fromJSON(req$postBody)
  email <- body$email
  
  # Check if user exists
  user <- dbGetQuery(con, "SELECT * FROM user_login WHERE email=$1", params=list(email))
  if (nrow(user) == 0) {
    res$status <- 404
    return(list(status="error", message="Email not found"))
  }
  
  if (!user$is_active[1]) {
    res$status <- 403
    return(list(status="error", message="Account inactive"))
  }
  
  # Use your helper function to generate temp password
  temp_password <- generate_temp_password()
  print(temp_password)
  temp_hash <- bcrypt::hashpw(temp_password)
  
  # Update DB with temp password and mark reset flag
  dbExecute(con, "
    UPDATE user_login
    SET password=$1, is_reset_required=TRUE, updated_on=NOW()
    WHERE email=$2
  ", params=list(temp_hash, email))
  
  # Send email with temp password
  send_reset_password_mail(email,temp_password)
  
  return(list(status="success", message="Temporary password sent to your email"))
}


# ==========================================================
#  Display Login Initials API
# ==========================================================
#* @get /user_fullname
function(req) {
  email <- req$user$email
  result <- dbGetQuery(con,
                       "SELECT full_name FROM users WHERE email = $1",
                       params = list(email)
  )
  if (nrow(result) == 1) {
    return(list(full_name = result$full_name[1]))
  } else {
    return(list(full_name = NULL))
  }
}