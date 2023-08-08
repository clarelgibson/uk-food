# SUMMARY ######################################################################
# Reads in source data from Food Standards Agency using an API call.

# SETUP ########################################################################
library(here)
library(httr)

# FHR API ######################################################################
url <- "www.food.gov.uk/ratings"

res <- VERB(
  "GET",
  url = url
)

content <- content(res, "text")
