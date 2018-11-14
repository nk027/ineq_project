# ------------------------------------------------------------------------
#
# Laeken Indicators
# Autoren: Engelen & Kuschnig
# Datum: 2018-11-10
#
# -------------------------------------------------------------------------

library(laeken)
library(dplyr)

country <- "CZ"
year <- 2013

# Source the Setup scripts to provide merged household and personal data
source("R/_connection.R")
source("R/_setup.R")


# Subsetting --------------------------------------------------------------

# To get useful results we may want to subset to only positive income
silc.pd.inc <- silc.pd %>% filter(py010g > 0)
silc.hd.inc <- silc.hd %>% filter(hy010 > 0)

# For hourly wages we replace NAs in working hours with 0
silc.pd.wage <- silc.pd
silc.pd.wage$pl060[is.na(silc.pd.wage$pl060)] <- 0
silc.pd.wage$pl100[is.na(silc.pd.wage$pl100)] <- 0
# Filter out observations with no income and/or hours worked
silc.pd.wage <- silc.pd.wage %>% 
  filter(py010g > 0 & pl060 > 0 & (pl073 + pl074) > 0)


# Indicators --------------------------------------------------------------

# Share of population at risk of poverty
#
arpr(inc = silc.pd.inc$py010g, weights = silc.pd.inc$pb040, 
     breakdown = silc.pd.inc$pb020)
arpr(inc = silc.hd.inc$hy010, silc.hd.inc$db090)

# Gender Pay Gap
#
silc.pd.wage <- silc.pd.wage %>% 
  mutate(hwages = py010g / ((pl060 + pl100) * (pl073 + pl074) * 52 / 12),
         gender = factor(pb150, labels = c("Male", "Female")))

silc.pd.wage <- silc.pd.wage %>% # Make sure female is the first level
  mutate(gender = relevel(gender, "Female"))
gpg(silc.pd.wage$hwages, gender = silc.pd.wage$gender)
