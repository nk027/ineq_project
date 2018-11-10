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
  filter(pb010 == 2013 & pb020 %in% c('NL')) %>%
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
###                                         Creating survey data                                               ###
##################################################################################################################
# init survey design Personal
silc.pd.svy <- svydesign(ids =  ~ id_h,
                         strata = ~db020,
                         weights = ~pb040,
                         data = silc.pd) %>%
  convey_prep()


# init survey design Household
silc.hd.svy <- svydesign(ids = ~id_h,
                         strata = ~db020,
                         weights = ~db090,
                         data = silc.hd) %>%
  convey_prep()

##################################################################################################################
###                                               Mean                                                         ###
##################################################################################################################
# Personal income mean:
svymean(~py010g, silc.pd.svy)
# For country comparison
# svyby(~py010g, ~as.factor(db020), subset(silc.pd.svy, py010g > 0), svymean)

# Household income mean:
svymean(~hy010, silc.hd.svy)
# For country comparison
#svyby(~hy010, ~as.factor(db020), subset(silc.hd.svy, hy010 > 0), svymean)
##################################################################################################################
###                                               Median                                                       ###
##################################################################################################################
# Median Personal Income
svyquantile(~py010g, silc.pd.svy, quantiles= c(0.5))
# For country comparison
# svyby(~py010g, ~as.factor(db020), subset(silc.pd.svy, py010g > 0), svyquantile, ~py010g, quantiles = c(0.5), keep.var = FALSE)

# Median Household Income
svyquantile(~hy010, silc.hd.svy, quantiles= c(0.5))
# For country comparison
# svyby(~hy010, ~as.factor(db020), subset(silc.hd.svy, hy010 > 0), svyquantile, ~hy010, quantiles = c(0.5), keep.var = FALSE)

##################################################################################################################
###                                            Decile points                                                   ###
##################################################################################################################
# Personal Decile points
svyquantile(~py010g, silc.pd.svy, quantiles = seq(0, 1, 0.1))

# For country comparison
#svyby(~py010g, ~as.factor(db020), subset(silc.pd.svy, py010g > 0), svyquantile, ~py010g,
#     quantiles = seq(0, 1, 0.1), keep.var = FALSE)

# Household Decile Points
svyquantile(~hy010, silc.hd.svy, quantiles = seq(0, 1, 0.1))
# For country comparison
#svyby(~hy010, ~as.factor(hb020), subset(silc.pd.svy, py010g > 0), svyquantile, ~py010g,
#     quantiles = seq(0, 1, 0.1), keep.var = FALSE)


##################################################################################################################
###                                       Quantile Share Ratios  QSR                                           ###
##################################################################################################################
#### S80/20 ####
# Personal Income
svyqsr(~py010g, silc.pd.svy, 0.2, 0.8)

# For country comparison
# svyby(~py010g, ~as.factor(db020), subset(silc.pd.svy, py010g > 0),
#      svyqsr, 0.2, 0.8)

# Household Income
svyqsr(~hy010, silc.hd.svy, 0.2, 0.8)

# For country comparison
# svyby(~hy010, ~as.factor(db020), subset(silc.hd.svy, hy010> 0),
#      svyqsr, 0.2, 0.8)

#### S90/10 ####
# Personal Income
svyqsr(~py010g, silc.pd.svy, 0.1, 0.9)

# For country comparison
# svyby(~py010g, ~as.factor(db020), subset(silc.pd.svy, py010g > 0),
#      svyqsr, 0.1, 0.9)

# Household Income
svyqsr(~hy010, silc.hd.svy, 0.1, 0.9)

# For country comparison
# svyby(~hy010, ~as.factor(db020), subset(silc.hd.svy, hy010> 0),
#      svyqsr, 0.1, 0.9)
##################################################################################################################
###                                             Top 10% Share                                                  ###
##################################################################################################################
# Personal Income top 10%
svytotal(~py010g,subset(silc.pd.svy, pb020 == country &
                          py010g>=as.numeric(svyquantile(~py010g,silc.pd.svy,quantile=c(0.9))))) /
  svytotal(~py010g,subset(silc.pd.svy, pb020 == country))

# Household Income top 10%
svytotal(~hy010,subset(silc.hd.svy, db020 == country &
                         hy010>=as.numeric(svyquantile(~hy010,silc.hd.svy,quantile=c(0.9))))) /
  svytotal(~hy010,subset(silc.hd.svy, db020 == country))

##################################################################################################################
###                                           Gini Coefficient                                                 ###
##################################################################################################################
# Personal Income Gini
svygini(~py010g, silc.pd.svy)

# COuntry comparison
#svyby(~py010g, ~as.factor(db020), silc.pd.svy, svygini)

# Household Income Gini
svygini(~hy010, silc.hd.svy)

# Country Comparison
# svyby(~hy010, ~as.factor(db020), silc.hd.svy, svygini)
##################################################################################################################
###                                             Theil Index                                                    ###
##################################################################################################################
# Personal Income Theil-Index
svygei(~py010g , silc.pd.svy, epsilon = 1)

# For country comparison
# svyby(~py010g, ~as.factor(db020), subset(silc.pd.svy, py010g > 0),
#      svygei, epsilon = 1)

# Household Income
svygei(~hy010 , silc.hd.svy, epsilon = 1)
# For country comparison
#svyby(~hy010, ~as.factor(db020), subset(silc.hd.svy, hy010 > 0),
#      svygei, epsilon = 1)



