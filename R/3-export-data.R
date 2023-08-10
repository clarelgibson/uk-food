# SUMMARY ######################################################################
# This script exports the data to CSV

# SETUP ########################################################################
# > Packages ===================================================================
library(here)
library(data.table)

# > Scripts ====================================================================
source(here("R/2-prep-data.R"))

# EXPORT #######################################################################
saveRDS(fhr, file = here("data/cln/fhr.rda"))
saveRDS(fhr_waffle, file = here("data/cln/fhr-waffle.rda"))

fwrite(fhr, file = here("data/cln/fhr.csv"))
fwrite(fhr_waffle, file = here("data/cln/fhr_waffle.csv"))