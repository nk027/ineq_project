# ------------------------------------------------------------------------
#
# Template R-Script
# Autoren: Engelen & Kuschnig
# Datum: 2018-11-08
#
# -------------------------------------------------------------------------

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
silc.p <- tbl(pg, 'pp') %>%
  filter(pb010 == 2013 & pb020 %in% country) %>%
  select(pb020, pb030, pb040, py010g, px010, px030) %>%
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
###                                       Purchasing Power Parity   (PPP)                                      ###
##################################################################################################################
# Getting the PPP-Data
ppp<-get_eurostat(id="prc_ppp_ind")

# filter 2013 values
ppp <- ppp %>% filter(aggreg=="A01" & na_item == "PPP_EU28" & time=="2013-01-01")

# Set colname of country equal to EU-SILC dataframe
colnames(ppp)[3] <- "pb020"

# Merge the data
ppp.pd <- left_join(silc.pd, ppp, by = "pb020")

# Calculate the income corrected for purchasing power parity. First we multiply with the change rate (px010, hx010), then divide by ppp correction.
ppp.pd <- ppp.pd %>% mutate(corrected.income = py010g/px010*values)

# Set colnames equal to EU-SILC Household dataframe
colnames(ppp)[3] <- "hb020"

# Merge the household data
ppp.hd <- left_join(silc.hd, ppp, by = "hb020")
# Calculate the income corrected for purchasing power parity. First we multiply with the change rate (px010, hx010), then divide by ppp correction.
ppp.hd <- ppp.hd %>% mutate(corrected.income = hy010/hx010*values)

# Create Survey design for Household and Personal-data
# Survey design personal data
ppp.pd.svy <- svydesign(ids=~id_h,
                        strata=~pb020,
                        weights=~pb040,
                        data=ppp.pd) %>% convey_prep()

# Survey design household data
ppp.hd.svy <- svydesign(ids=~id_h,
                        strata=~hb020,
                        weights=~db090,
                        data=ppp.hd) %>% convey_prep()

##################################################################################################################
###                                               Mean                                                         ###
##################################################################################################################
# Personal income mean:
svymean(~corrected.income, ppp.pd.svy)
# For country comparison
# svyby(~corrected.income, ~as.factor(db020), subset(ppp.pd.svy, corrected.income > 0), svymean)

# Household income mean:
svymean(~corrected.income, ppp.hd.svy)
# For country comparison
#svyby(~corrected.income, ~as.factor(db020), subset(ppp.hd.svy, corrected.income > 0), svymean)
##################################################################################################################
###                                               Median                                                       ###
##################################################################################################################
# Median Personal Income
svyquantile(~corrected.income, ppp.pd.svy, quantiles= c(0.5))
# For country comparison
# svyby(~corrected.income, ~as.factor(db020), subset(ppp.pd.svy, corrected.income > 0), svyquantile, ~corrected.income, quantiles = c(0.5), keep.var = FALSE)

# Median Household Income
svyquantile(~corrected.income, ppp.hd.svy, quantiles= c(0.5))
# For country comparison
# svyby(~corrected.income, ~as.factor(db020), subset(ppp.hd.svy, corrected.income > 0), svyquantile, ~corrected.income, quantiles = c(0.5), keep.var = FALSE)

##################################################################################################################
###                                            Decile points                                                   ###
##################################################################################################################
# Personal Decile points
svyquantile(~corrected.income, ppp.pd.svy, quantiles = seq(0, 1, 0.1))

# For country comparison
#svyby(~corrected.income, ~as.factor(db020), subset(ppp.pd.svy, corrected.income > 0), svyquantile, ~corrected.income,
#     quantiles = seq(0, 1, 0.1), keep.var = FALSE)

# Household Decile Points
svyquantile(~corrected.income, ppp.hd.svy, quantiles = seq(0, 1, 0.1))
# For country comparison
#svyby(~corrected.income, ~as.factor(hb020), subset(ppp.pd.svy, corrected.income > 0), svyquantile, ~corrected.income,
#     quantiles = seq(0, 1, 0.1), keep.var = FALSE)


##################################################################################################################
###                                       Quantile Share Ratios  QSR                                           ###
##################################################################################################################
#### S80/20 ####
# Personal Income
svyqsr(~corrected.income, ppp.pd.svy, 0.2, 0.8)

# For country comparison
# svyby(~corrected.income, ~as.factor(db020), subset(ppp.pd.svy, corrected.income > 0),
#      svyqsr, 0.2, 0.8)

# Household Income
svyqsr(~corrected.income, ppp.hd.svy, 0.2, 0.8)

# For country comparison
# svyby(~corrected.income, ~as.factor(db020), subset(ppp.hd.svy, corrected.income> 0),
#      svyqsr, 0.2, 0.8)

#### S90/10 ####
# Personal Income
svyqsr(~corrected.income, ppp.pd.svy, 0.1, 0.9)

# For country comparison
# svyby(~corrected.income, ~as.factor(db020), subset(ppp.pd.svy, corrected.income > 0),
#      svyqsr, 0.1, 0.9)

# Household Income
svyqsr(~corrected.income, ppp.hd.svy, 0.1, 0.9)

# For country comparison
# svyby(~corrected.income, ~as.factor(db020), subset(ppp.hd.svy, corrected.income> 0),
#      svyqsr, 0.1, 0.9)
##################################################################################################################
###                                             Top 10% Share                                                  ###
##################################################################################################################
# Personal Income top 10%
svytotal(~corrected.income,subset(ppp.pd.svy, pb020 == country &
                                    corrected.income>=as.numeric(svyquantile(~corrected.income,ppp.pd.svy,quantile=c(0.9))))) /
  svytotal(~corrected.income,subset(ppp.pd.svy, pb020 == country))

# Household Income top 10%
svytotal(~corrected.income,subset(ppp.hd.svy, db020 == country &
                                    corrected.income>=as.numeric(svyquantile(~corrected.income,ppp.hd.svy,quantile=c(0.9))))) /
  svytotal(~corrected.income,subset(ppp.hd.svy, db020 == country))

##################################################################################################################
###                                           Gini Coefficient                                                 ###
##################################################################################################################
# Personal Income Gini
svygini(~corrected.income, ppp.pd.svy)

# COuntry comparison
#svyby(~corrected.income, ~as.factor(db020), ppp.pd.svy, svygini)

# Household Income Gini
svygini(~corrected.income, ppp.hd.svy)

# Country Comparison
# svyby(~corrected.income, ~as.factor(db020), ppp.hd.svy, svygini)
##################################################################################################################
###                                             Theil Index                                                    ###
##################################################################################################################
# Personal Income Theil-Index
svygei(~corrected.income , ppp.pd.svy, epsilon = 1)

# For country comparison
# svyby(~corrected.income, ~as.factor(db020), subset(ppp.pd.svy, corrected.income > 0),
#      svygei, epsilon = 1)

# Household Income
svygei(~corrected.income , ppp.hd.svy, epsilon = 1)
# For country comparison
#svyby(~corrected.income, ~as.factor(db020), subset(ppp.hd.svy, corrected.income > 0),
#      svygei, epsilon = 1)
