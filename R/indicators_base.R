# ------------------------------------------------------------------------
#
# Indicators R-Script
# Autoren: Engelen & Kuschnig
# Datum: 2018-11-08
#
# -------------------------------------------------------------------------

library(dplyr)
library(survey)
library(convey)

country <- "CZ"
year <- 2013

# Source the Setup scripts to provide merged household and personal data
source("R/_connection.R")
source("R/_setup.R")


# Subsetting --------------------------------------------------------------

# To get useful results we may want to subset to only positive income
silc.pd.inc <- silc.pd %>% filter(py010g > 0)
silc.hd.inc <- silc.hd %>% filter(hy010 > 0)

# Creating Survey Objects -------------------------------------------------

silc.pd.svy <- svydesign(ids =  ~ id_h,
                         strata = ~db020,
                         weights = ~pb040,
                         data = silc.pd) %>% convey_prep()

silc.hd.svy <- svydesign(ids = ~id_h,
                         strata = ~db020,
                         weights = ~db090,
                         data = silc.hd) %>% convey_prep()


# Indicators --------------------------------------------------------------

# Mean Income
#
svymean(~total.inc, silc.pd.svy)
svymean(~hy010, silc.hd.svy)
# For comparing countries
# svyby(~total.inc, ~as.factor(db020), silc.pd.svy, svymean)
# svyby(~hy010, ~as.factor(db020), silc.hd.svy, svymean)

# Median Income
#
svyquantile(~total.inc, silc.pd.svy, quantiles = c(0.5))
svyquantile(~hy010, silc.hd.svy, quantiles = c(0.5))

# For comparing countries
# svyby(~total.inc, ~as.factor(db020), silc.pd.svy,
#       svyquantile, ~total.inc, quantiles = c(0.5), keep.var = FALSE)
# svyby(~hy010, ~as.factor(db020), silc.hd.svy,
#       svyquantile, ~hy010, quantiles = c(0.5), keep.var = FALSE)

# Decile Points
#
svyquantile(~total.inc, silc.pd.svy, quantiles = seq(0, 1, 0.1))
svyquantile(~hy010, silc.hd.svy, quantiles = seq(0, 1, 0.1))
# For comparing countries
# svyby(~total.inc, ~as.factor(db020), silc.pd.svy, 
#       svyquantile, ~total.inc, quantiles = seq(0, 1, 0.1), keep.var = FALSE)
# svyby(~hy010, ~as.factor(hb020), silc.pd.svy, 
#       svyquantile, ~total.inc, quantiles = seq(0, 1, 0.1), keep.var = FALSE)

# Quantile Share Ratio
#
svyqsr(~total.inc, silc.pd.svy, 0.2, 0.8)
svyqsr(~hy010, silc.hd.svy, 0.2, 0.8)
# For comparing countries
# svyby(~total.inc, ~as.factor(db020), silc.pd.svy, svyqsr, 0.2, 0.8)
# svyby(~hy010, ~as.factor(db020), silc.hd.svy, svyqsr, 0.2, 0.8)

# Top 10% Income Share
#
svytotal(~total.inc, subset(silc.pd.svy, pb020 == country & total.inc >= 
                           as.numeric(svyquantile(~total.inc, silc.pd.svy, quantile = 0.9)))) / 
  svytotal(~total.inc, subset(silc.pd.svy, pb020 == country))
svytotal(~hy010, subset(silc.hd.svy, db020 == country & hy010 >= 
                          as.numeric(svyquantile(~hy010, silc.hd.svy, quantile = 0.9)))) /
  svytotal(~hy010,subset(silc.hd.svy, db020 == country))

# Gini Coefficient
#
svygini(~total.inc, silc.pd.svy)
svygini(~hy010, silc.hd.svy)
# For comparing countries
# svyby(~total.inc, ~as.factor(db020), silc.pd.svy, svygini)
# svyby(~hy010, ~as.factor(db020), silc.hd.svy, svygini)

# Theil Index
#
svygei(~total.inc, silc.pd.svy, epsilon = 1)
svygei(~hy010, silc.hd.svy, epsilon = 1)
# For comparing countries
# svyby(~total.inc, ~as.factor(db020), silc.pd.svy,
#      svygei, epsilon = 1)
# svyby(~hy010, ~as.factor(db020), silc.hd.svy,
#      svygei, epsilon = 1)
