# SUMMARY ######################################################################
# Reads in source data from Food Standards Agency using an API call.

# SETUP ########################################################################
library(here)
library(xml2)
library(tidyr)
library(janitor)
library(readr)
library(dplyr)
library(glue)
library(sf)

# UK LOCAL AUTHORITY DISTRICTS #################################################
# > FHR LADs ===================================================================
fhrlad_url <- "https://ratings.food.gov.uk/authorities/xml"
fhrlad_xml <- as_list(read_xml(fhrlad_url))

# Convert to df
fhrlad_src <- 
  as_tibble(fhrlad_xml) %>% 
  # split data into columns
  unnest_wider(ArrayOfWebLocalAuthorityAPI) %>% 
  # tidy column headings
  clean_names() %>% 
  # unnest twice to get the data out of list format
  unnest(cols = names(.)) %>% 
  unnest(cols = names(.)) %>% 
  # remove country level records
  filter(!is.na(friendly_name)) %>% 
  # clean data types
  type_convert()

# > UK Geography LADs ==========================================================
ukglad_path <- here("data/src/shape-lad")
ukglad_src <- read_sf(ukglad_path)

# FHR API ######################################################################
# List LA codes to loop through and extract data
lad_codes <- 
  fhrlad_src %>% 
  # keep only records for England and Wales
  filter(!(region_name %in% c("Scotland", "Northern Ireland"))) %>% 
  select(file = file_name,
         code = local_authority_id_code,
         name)

# Use smaller number of values as test data
#lad_codes <- lad_codes[1:10, ]

# Create an empty df to store the results
fhr_src <- tibble()

# Loop through each LAD code, extract data and store in df
if (!file.exists(here("data/src/fhr_src.rds"))) {
  
  for (i in seq_along(1:nrow(lad_codes))) {
    print(glue("Starting iteration {i} of {nrow(lad_codes)} \n"))
    print(glue("..Local authority {lad_codes$code[i]}: {lad_codes$name[i]} \n"))
    
    # Read data for this iteration
    url <- lad_codes$file[i]
    xml <- as_list(read_xml(url))
    
    # Get iteration data in to df
    df <- 
      as_tibble(xml) %>% 
      unnest_longer(FHRSEstablishment) %>% 
      filter(FHRSEstablishment_id == "EstablishmentDetail") %>% 
      # unnest all levels with children
      unnest_wider(FHRSEstablishment) %>% 
      unnest_wider(Scores) %>% 
      unnest_wider(Geocode) %>% 
      # unnest twice to extract values from list
      unnest(cols = names(.)) %>% 
      unnest(cols = names(.))
    
    # Bind data to df
    fhr_src <- fhr_src %>% 
      bind_rows(df)
    
    print(glue("Finished iteration {i} of {nrow(lad_codes)} \n"))
  } 
  
  saveRDS(fhr_src, file = here("data/src/fhr_src.rds"))
} else {
  fhr_src <- readRDS(here("data/src/fhr_src.rds"))
}
