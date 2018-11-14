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

# Source the Setup scripts to provide merged household and personal data
source("R/_connection.R")
source("R/_setup.R")


# Eurostat ----------------------------------------------------------------

ppp <- get_eurostat(id = "prc_ppp_ind")

# Filter relevant values
ppp <- ppp %>% filter(aggreg == "A01" &
                        na_item == "PPP_EU28" & 
                        time == "2013-01-01")

# Merge the data
ppp.pd <- left_join(silc.pd, ppp, by = c("pb020" = "geo"))
ppp.hd <- left_join(silc.hd, ppp, by = c("hb020" = "geo"))

# Calculate the income corrected for purchasing power parity
# 1. multiply with the exchange rate (px010, hx010), then divide by ppp
ppp.pd <- ppp.pd %>%
  mutate(total.inc.ppp = (total.inc) * px010 / values)
ppp.hd <- ppp.hd %>%
  mutate(hy010ppp = hy010 * hx010 / values)
