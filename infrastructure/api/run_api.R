library(plumber)
pr <- plumb("./infrastructure/api/AccessRequestAPI.R")
pr$run(host="0.0.0.0", port=8000)
