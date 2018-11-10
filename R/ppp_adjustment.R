# ------------------------------------------------------------------------
#
# PPP R-Script
# Autoren: Engelen & Kuschnig
# Datum: 2018-11-08
#
# -------------------------------------------------------------------------

library(eurostat)
library(dplyr)

country <- "CZ"
year <- 2013

# Source the Setup script to provide merged household and personal data
source("R/_setup.R")



# Eurostat ----------------------------------------------------------------

ppp <- get_eurostat(id = "prc_ppp_ind")

# Filter relevant values
ppp <- ppp %>% filter(aggreg == "A01" &
                        na_item == "PPP_EU28" & 
                        time == "2013-01-01")

# Rename the country variable according to the EU-SILC convention
colnames(ppp)[3] <- "pb020"
colnames(ppp)[3] <- "hb020"

# Merge the data
ppp.pd <- left_join(silc.pd, ppp, by = "pb020")
ppp.hd <- left_join(silc.hd, ppp, by = "hb020")

# Calculate the income corrected for purchasing power parity
# 1. multiply with the exchange rate (px010, hx010), then divide by ppp
ppp.pd <- ppp.pd %>%
  mutate(py010ppp = py010g / px010 * values)
ppp.hd <- ppp.hd %>%
  mutate(hy010ppp = hy010 / hx010 * values)
