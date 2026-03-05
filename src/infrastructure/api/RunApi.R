setwd("C:/Users/kusharani/clinical_poc/sf_xxxxxxx_poc_clinicalsas_rshinywebapp") 
library(plumber) 
pr <- plumb("src/infrastructure/api/ApiLogin.R") 
pr$run(host="0.0.0.0", port=8000)