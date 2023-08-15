# SUMMARY ######################################################################
# This script cleans the data ready for further analysis in Tableau

# SETUP ########################################################################
# > Packages ===================================================================
library(here)

# > Scripts ====================================================================
source(here("R/1-get-data.R"))

# CLEAN DATA ###################################################################
# > LADs =======================================================================
# Which LADs in the UKG data have no match in the FHR data?
ukglad_anti <- ukglad_src %>% 
  clean_names() %>% 
  select(lad23nm) %>% 
  anti_join(
    select(
      fhrlad_src,
      lad23nm = name
    )
  )

# Add clean names to the FHR LAD data so that we can join UKG data
fhrlad_cln <- 
  fhrlad_src %>% 
  mutate(
    lad23nm = case_when(
      name == "Stockton On Tees" ~ "Stockton-on-Tees",
      name == "Blackburn" ~ "Blackburn with Darwen",
      name == "Hull City" ~ "Kingston upon Hull, City of",
      name == "Hull and Goole Port" ~ "Kingston upon Hull, City of",
      name == "Derby City" ~ "Derby",
      name == "Leicester City" ~ "Leicester",
      name == "Nottingham City" ~ "Nottingham",
      name == "Herefordshire" ~ "Herefordshire, County of",
      name == "Telford and Wrekin Council" ~ "Telford and Wrekin",
      name == "Stoke-On-Trent" ~ "Stoke-on-Trent",
      name == "Bristol" ~ "Bristol, City of",
      name == "Plymouth City" ~ "Plymouth",
      name == "Peterborough City" ~ "Peterborough",
      name == "Southend-On-Sea" ~ "Southend-on-Sea",
      name == "Durham" ~ "County Durham",
      name == "Cambridge City" ~ "Cambridge",
      name == "Exeter City" ~ "Exeter",
      name == "Gloucester City" ~ "Gloucester",
      name == "Winchester City" ~ "Winchester",
      name == "Canterbury City" ~ "Canterbury",
      name == "Lancaster City" ~ "Lancaster",
      name == "Lincoln City" ~ "Lincoln",
      name == "Norwich City" ~ "Norwich",
      name == "Oxford City" ~ "Oxford",
      name == "Newcastle-Under-Lyme" ~ "Newcastle-under-Lyme",
      name == "Worcester City" ~ "Worcester",
      name == "St Albans City" ~ "St Albans",
      name == "St Helens" ~ "St. Helens",
      name == "Newcastle Upon Tyne" ~ "Newcastle upon Tyne",
      name == "City of London Corporation" ~ "City of London",
      name == "Kingston-Upon-Thames" ~ "Kingston upon Thames",
      name == "Richmond-Upon-Thames" ~ "Richmond upon Thames",
      name == "Belfast City" ~ "Belfast",
      name == "Lisburn and Castlereagh City" ~ "Lisburn and Castlereagh",
      name == "Comhairle nan Eilean Siar (Western Isles)" ~ "Na h-Eileanan Siar",
      name == "Edinburgh (City of)" ~ "City of Edinburgh",
      name == "Anglesey" ~ "Isle of Anglesey",
      name == "River Tees" ~ "Redcar and Cleveland",
      TRUE ~ name
    )
  )

# Which LADs in the FHR data have no match in the UKG data?
fhrlad_anti <- fhrlad_cln %>% 
  clean_names() %>% 
  select(lad23nm) %>% 
  anti_join(
    select(
      ukglad_src,
      lad23nm = LAD23NM
    )
  )

# Make full dataframe for Local Authorities
lad <- ukglad_src %>% 
  clean_names() %>% 
  full_join(fhrlad_cln,
            by = "lad23nm") %>% 
  rename(local_authority_code = local_authority_id_code)

# > FHR ========================================================================
fhr <- fhr_src %>% 
  # Clean the column headers
  clean_names() %>% 
  # Remove records with missing location data
  filter(!is.na(longitude)) %>% 
  # clean rating value
  mutate(rating_value = as.numeric(rating_value)) %>% 
  # Remove records with missing rating values
  filter(!is.na(rating_value)) %>% 
  # add count column
  mutate(business_count = 1) %>% 
  # recode rating key
  mutate(
    rating_key = case_when(
      grepl("0", rating_key) ~ "Urgent improvement is required",
      grepl("1", rating_key) ~ "Major improvement is required",
      grepl("2", rating_key) ~ "Some improvement is required",
      grepl("3", rating_key) ~ "Hygiene standards are generally satisfactory",
      grepl("4", rating_key) ~ "Hygiene standards are good",
      grepl("5", rating_key) ~ "Hygiene standards are very good",
      TRUE ~ rating_key
    )
  ) %>% 
  # add rating indicators
  mutate(
    rating_indicator_inc_zero = case_when(
      rating_value >= 3 ~ "Satisfactory",
      rating_value >= 1 ~ "Unsatisfactory",
      rating_value == 0 ~ "Rating 0"
    ),
    rating_indicator_binary = case_when(
      rating_value >= 3 ~ "Satisfactory",
      rating_value >= 0 ~ "Unsatisfactory"
    )
  ) %>% 
  # append score to score columns
  rename(hygiene_score = hygiene,
         structural_score = structural,
         management_score = confidence_in_management) %>% 
  # join local authority data
  left_join(
    select(
      fhrlad_cln,
      local_authority_code = local_authority_id_code,
      lad23nm,
      region_name
    )
  ) %>% 
  # select and rename columns to keep
  select(
    fhrsid,
    business_id = local_authority_business_id,
    business_name,
    business_type,
    business_count,
    business_longitude = longitude,
    business_latitude = latitude,
    rating_value,
    rating_key,
    rating_date,
    rating_indicator_inc_zero,
    rating_indicator_binary,
    local_authority_code,
    local_authority_name,
    local_authority_web_site,
    lad23nm,
    region_name
  ) %>% 
  # correct data types
  type_convert()

# > FHR Waffle =================================================================
# >> Scaffold ------------------------------------------------------------------
scaffold_types <- fhr %>% 
  select(business_type) %>% 
  distinct() %>% 
  arrange(business_type)

scaffold_lads <- fhr %>% 
  select(local_authority_name) %>% 
  distinct() %>% 
  arrange(local_authority_name)

scaffold_rating <- fhr %>% 
  select(rating_indicator_binary) %>% 
  distinct() %>% 
  arrange(rating_indicator_binary)

scaffold <- 
  scaffold_lads %>% 
  cross_join(scaffold_types) %>% 
  cross_join(scaffold_rating) %>% 
  mutate(scaffold_count = 0)

# >> Waffle --------------------------------------------------------------------
fhr_waffle <- scaffold %>% 
  left_join(
    select(
      fhr,
      local_authority_name,
      business_type,
      rating_indicator_binary,
      business_count
    )
  ) %>% 
  arrange(local_authority_name,
          business_type,
          rating_indicator_binary) %>% 
  # fix business count
  mutate(
    business_count = if_else(
      is.na(business_count),
      scaffold_count,
      business_count
    )
  ) %>% 
  select(-scaffold_count) %>% 
  # add count by business type
  group_by(local_authority_name,
           business_type) %>% 
  mutate(count_business_type = sum(business_count)) %>% 
  ungroup() %>% 
  # add count of rating by business type and local authority
  group_by(local_authority_name,
           business_type,
           rating_indicator_binary) %>% 
  mutate(count_rating = sum(business_count),
         pct_rating = count_rating / count_business_type) %>% 
  ungroup() %>% 
  distinct() %>% 
  # keep only the satisfactory rows
  filter(rating_indicator_binary == "Satisfactory") %>% 
  select(local_authority_name,
         business_type,
         total_businesses = count_business_type,
         count_satisfactory = count_rating,
         pct_satisfactory = pct_rating)
