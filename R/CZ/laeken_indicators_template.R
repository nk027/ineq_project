# ------------------------------------------------------------------------
#
# Laeken Indicators
# Autoren: Engelen & Kuschnig
# Datum: 2018-11-10
#
# -------------------------------------------------------------------------
library(laeken)
library(dplyr)
library(survey)
library(convey)
library(readr)
library(eurostat)

# create db object
# Use the code from class to connect to the database. 

# Set country
country <- c("CZ")


##################################################################################################################
###                                  Downloading and merging the Household data                                ###
##################################################################################################################
## Downloading the household data.
# Household Register Data of 2013, containing the weights.
silc.d <- tbl(pg, 'dd') %>%
  filter(db010 == 2013 & db020 %in% country) %>%
  select(db020, db030, db090) %>%
  collect()

# Household Data of 2013
silc.h <- tbl(pg, 'hh') %>%
  filter(hb010 == 2013 & hb020 %in% country) %>%
  select(hb020, hb030, hy010, hx010) %>%
  collect()

## In order to merge the two dataframe we need to create a complete ID. Containing Country and Household ID#
# In the register data
silc.d <- silc.d %>%
  mutate(id_h = paste0(db020, db030))

# In the household data.
silc.h <- silc.h %>%
  mutate(id_h = paste0(hb020, hb030))

# merge hh data with hh register to add hh weights
silc.hd <- left_join(silc.h, silc.d)

##################################################################################################################
###                                   Downloading and merging the Personal data                                ###
##################################################################################################################
# download personal data
silc.p <- tbl(pg, 'c13p') %>%
  filter(pb010 == 2013 & pb020 %in% country) %>%
  select(pb020, pb030, pb040, pb150, py010g, px010, px030, pl060, pl073, pl074, pl100, rx060) %>%
  collect(n = Inf)

# generate id_h (merge with country code)
silc.p <- silc.p %>%
  mutate(id_h = paste0(pb020, px030))

# merge personal data with household register
silc.pd <- left_join(silc.p, silc.d %>% select(id_h, db020, db090))


##################################################################################################################
###                                         Subsetting the data                                                ###
##################################################################################################################
# In order to get useful results we calculate mean/Decile/QSR/Top 10%/Gini/Theil using only positive income (in personal income and household income respectively)
# First for the personal incomes
silc.pd <- silc.pd %>% filter(py010g>0)
# Now for the household incomes
silc.hd <- silc.hd %>% filter(hy010>0)


##################################################################################################################
###                                            At risk of poverty 60% Median                                   ###
##################################################################################################################

# Share of population at risk of poverty
# Personal
arpr(inc = silc.pd$py010g, weights = silc.pd$pb040, breakdown = silc.pd$pb020)

# Household level
arpr(inc = silc.hd$hy010, silc.hd$db090)


##################################################################################################################
###                                            Gender pay gap                                                  ###
##################################################################################################################
# replace NAs in working hours by 0
silc.pd$pl060[is.na(silc.pd$pl060)] <- 0
silc.pd$pl100[is.na(silc.pd$pl100)] <- 0
# filter only observations with income and working time
silc.pd <- silc.pd %>% filter(py010g>0 & pl060>0 & (pl073+pl074)>0)

# calculate hourly wage
silc.pd <- silc.pd %>% mutate(hwages = py010g / ((pl060+pl100)*(pl073+pl074)*52/12) )
# Rewrite gender variable as factor
silc.pd <- silc.pd %>% mutate( 
  gender = factor(pb150,labels = c('Male','Female')))
# Making sure female is first level
silc.pd <- silc.pd %>% mutate(
  gender = relevel(gender, "Female")
)
# Calculate the gender pay gap
gpg(silc.pd$hwages, gender = silc.pd$gender)
